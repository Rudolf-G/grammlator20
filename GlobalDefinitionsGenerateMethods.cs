using IndexSetNamespace;

using System;
using System.CodeDom.Compiler;
using System.Collections;
using System.Collections.Generic;
using System.Configuration;
using System.Diagnostics;
using System.Globalization;
using System.Text;


namespace grammlator
{
   internal abstract partial class ParserAction
   {

      /// <summary>
      /// Generate the code implementing this action. 
      /// Return the next action to generate or null.
      /// </summary>
      /// <param name="codegen">The class that implements the generation of the code</param>
      /// <param name="accept"></param>
      /// <returns>the next action to generate or null</returns>
      internal virtual ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
         => throw new NotImplementedException($"Codegeneration is not implemented for {ParserActionType}");
   }

   internal sealed partial class BranchAction : ParserAction
   {

      public struct ActionAndCounter
      {
         public Int32 Counter;
         public Int32 MaxCondition, MinCondition;
         public ParserAction Action;

         public ActionAndCounter(Int32 counter, ParserAction action, Int32 minMaxCondition)
         {
            this.Counter = counter;
            this.Action = action;
            this.MinCondition = minMaxCondition;
            this.MaxCondition = minMaxCondition;
         }
      }

      /// <summary>
      /// In der BranchToGenerate.ListOfCases kann als Folge der Optimierungen die gleiche Aktion mit verschiedenen Kennungen vorkommen.
      /// In der ActionCounterList kommt jede Aktion daraus genau einmal vor. Der Zähler gibt an, wie oft sie in der ListOfCases vorkommt.
      /// </summary>
      public sealed class ActionCounterList : List<ActionAndCounter>
      {
         private ActionCounterList(Int32 capacity) : base(capacity) { }

         private ActionCounterList()
         {
         }

         /// <summary>
         /// Constructs an ActionCounterList with the same length as the ListOfCases of the BranchToGenerate,
         /// adds all different branchcases counting duplicates, finally trims the list
         /// </summary>
         /// <param name="BranchToGenerate"></param>
         public ActionCounterList(BranchAction BranchToGenerate) : this(BranchToGenerate.ListOfCases.Count)
         {
            foreach (BranchcaseStruct branchcase in BranchToGenerate.ListOfCases)
            {
               // Die Aktion in der Liste suchen:
               Int32 FoundIndex = FindIndex(x => x.Action == branchcase.BranchcaseAction);

               if (FoundIndex == -1)
               { // not found
                  Add(
                     new ActionAndCounter(counter: 1, action: branchcase.BranchcaseAction,
                     minMaxCondition: branchcase.BranchcaseCondition)
                     );
               }
               else
               { // found:  increment counter and update MaxCondition
                  ActionAndCounter actionAndCounter = this[FoundIndex];
                  actionAndCounter.Counter++;
                  if (actionAndCounter.MinCondition > branchcase.BranchcaseCondition)
                     actionAndCounter.MinCondition = branchcase.BranchcaseCondition;
                  if (actionAndCounter.MaxCondition < branchcase.BranchcaseCondition)
                     actionAndCounter.MaxCondition = branchcase.BranchcaseCondition;
                  this[FoundIndex] = actionAndCounter;
               }
            }
            TrimExcess();
         }
      }

      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         return GenerateCondionalActionsOfBranch(codegen, out accept);
      }
      private ParserAction GenerateCondionalActionsOfBranch(P5CodegenCS codegen, out Boolean accept)
      {
         accept = false;
         // Create a CounterList which is ready to use preset with one entry and counter for each different action in BranchtoGenerate  
         var CounterList = new ActionCounterList(this);

         // if there is only one action: return it to be generated without condition
         if (CounterList.Count == 1)
         {
            return CounterList[0].Action;
         }

         // Select a default action
         // There are different criteria to select the default action:
         //   select the most frequent action to reduce the conditions;
         //   avoid goto actions to move them into the conditional part;
         //   avoid other simple actions to put them in the conditional part

         Int32 MinIndex = 0, PriorityIndex = 0, MaxIndex = 0;

         Int32 MinCondition = Int32.MaxValue;
         Int32 MaxCondition = Int32.MinValue;

         Int32 MinPriority = Int32.MinValue;
         Int32 Priority = Int32.MinValue;

         for (Int32 i = 0; i < CounterList.Count; i++)
         {
            var actionAndCounter = CounterList[i];
            // Select a default action to be generated as last:
            // select the most frequent action and if equal frequency
            // prefer actions with larger branch condition (to reduce the span of values to be tested in the generated switch)

            Int32 thisPriority = actionAndCounter.Counter * 1000 + actionAndCounter.MaxCondition;

            // but reduce the priority of actions which will be generated as goto
            if (P5GenerateCode.GeneratesGoto(generateAccept: false, actionAndCounter.Action, codegen.IndentationLevel)
                // or of actions which will loop back 
                || (actionAndCounter.Action as ReduceAction)?.NextAction == this
                // or of indirect or direct HaltActions
                || (actionAndCounter.Action as ReduceAction)?.NextAction is HaltAction
                || actionAndCounter.Action is HaltAction
                )
            {
               thisPriority -= (Int32.MaxValue >> 1);
            }

            if (actionAndCounter.MinCondition < MinCondition)
            {
               MinCondition = actionAndCounter.MinCondition;
               MinIndex = i;
               MinPriority = thisPriority;
            }
            if (actionAndCounter.MaxCondition > MaxCondition)
            {
               MaxCondition = actionAndCounter.MaxCondition;
               MaxIndex = i;
            }

            if (thisPriority > Priority)
            {
               PriorityIndex = i;
               Priority = thisPriority;
            }
         }

         // If the action with the MaxIndex didn't get priority 
         // then check if the action with the MinIndex is qualified as default action
         if (PriorityIndex != MaxIndex && MinPriority > 0)
            PriorityIndex = MinIndex;

         var defaultAction = CounterList[PriorityIndex].Action;

         // If there are only 2 different actions one of which has only one condition (Counter==1) to check for
         // then it is possible to generate an IF condition with one test for equality (or unequality)
         if (CounterList.Count == 2 && (CounterList[0].Counter == 1 || CounterList[1].Counter == 1))
         {
            GenerateBranchAsIFInstruction(codegen, CounterList, defaultAction);
            return defaultAction;
         }

         return GenerateBranchAsSwitch(codegen, CounterList, defaultAction);
      }

      /// <summary>
      /// generate an if instruction so that not the default action will be executed conditionally 
      /// and the default action will remain to be generated in the codesequence
      /// </summary>
      /// <param name="branchToGenerate"></param>
      /// <param name="counterList"></param>
      /// <param name="defaultAction"></param>
      /// 
      private void GenerateBranchAsIFInstruction(P5CodegenCS codegen, ActionCounterList counterList, ParserAction defaultAction)
      {
         // The simple condition case is the case whose BranchcaseCondition occurs only once in the branchToGenerate.ListOfCases
         // Its BranchcaseCondition (int value) will be used as condition in the generated if instruction.
         Int32 simpleConditionCaseIndex = 0, complexConditionCaseIndex = 1; // initial assignment

         if (counterList[simpleConditionCaseIndex].Counter != 1
            // if each of both action occurs only once use the default-Action as complexCase
            || (counterList[complexConditionCaseIndex].Counter == 1 && counterList[simpleConditionCaseIndex].Action == defaultAction))
         {
            // change the initial assignment
            simpleConditionCaseIndex = 1;
            complexConditionCaseIndex = 0;
         }

         ParserAction simpleConditionCase = counterList[simpleConditionCaseIndex].Action;
         ParserAction complexConditionCase = counterList[complexConditionCaseIndex].Action;
         Int32 complexCaseCounter = counterList[complexConditionCaseIndex].Counter;

         // simpleCaseCounter == 1   !

         // Find simpleCase in the ListOfCases - where it occurs only once - to get its condition
         Int32 simpleCaseCondition = this.ListOfCases.Find(x => x.BranchcaseAction == simpleConditionCase).BranchcaseCondition;

         if (complexConditionCase == defaultAction)
         {
            codegen.GenerateIfSPeek(false, simpleCaseCondition);
            // generate simpleCase
            codegen.IncrementIndentationLevel();
            P5GenerateCode.GenerateCodeSequence(codegen, simpleConditionCase, false, false);
            codegen.DecrementIndentationLevel();
            // Adjust the number of calls because some actions are handled together 
            if (complexConditionCase.Calls > 0)
               complexConditionCase.Calls -= complexCaseCounter - 1;
         }
         else
         {  // simpleCase == defaultAction
            // generate the complement of the condition of simpleCase-condition 
            codegen.GenerateIfSPeek(true, simpleCaseCondition);
            // generate complexCase
            // Adjust the number of calls because some actions are handled together
            if (complexConditionCase.Calls > 0)
               complexConditionCase.Calls -= complexCaseCounter - 1;
            codegen.IncrementIndentationLevel();
            P5GenerateCode.GenerateCodeSequence(codegen, complexConditionCase, false, false);
            codegen.DecrementIndentationLevel();
         }
      }


      private ParserAction GenerateBranchAsSwitch(P5CodegenCS codegen, ActionCounterList counterList, ParserAction defaultAction)
      {
         // Generate a switch statement
         // Für jede Aktion ungleich Defaultaktion in der Zählliste alle gleichen Aktionen in der Falliste der Verzweigung suchen
         // und eine oder mehrere Case-Anweisungen erzeugen sowie einmal die Codefolge für die Aktion

         codegen.IndentExactly()
            .Append("switch (")
            .Append(GlobalSettings.StateStack.Value)
            .Append(".Peek())")
            .IndentExactly()
            .Append("{");

         foreach (ActionAndCounter ElementOFCounterList in counterList)
         {
            if (ElementOFCounterList.Action != defaultAction)
            {
               // Combine all cases with the same action
               foreach (BranchcaseStruct f in this.ListOfCases)
               {
                  if (f.BranchcaseAction == ElementOFCounterList.Action)
                     codegen.IndentExactly()
                        .Append("case ")
                        .Append(f.BranchcaseCondition.ToString(CultureInfo.InvariantCulture))
                        .Append(": ");
               }

               // Generate the code sequence
               codegen.IncrementIndentationLevel();
               P5GenerateCode.GenerateCodeSequence(codegen, ElementOFCounterList.Action, false, false);
               codegen.DecrementIndentationLevel();
               // The sequence always ends with a goto
            }
         }

         // Generate the cases of the default action as comment
         // If more than 1 cases are combined to the default the Calls statistic of the default actionhas to be reduced
         codegen.Indent().Append("/*");
         Int32 countOfDefaultCases = 0;
         foreach (BranchcaseStruct f in this.ListOfCases)
         {
            if (f.BranchcaseAction == defaultAction)
            {
               countOfDefaultCases++;
               codegen.AppendWithOptionalLinebreak("case ", f.BranchcaseCondition.ToString(CultureInfo.InvariantCulture), ": ");
            }
         }

         defaultAction.Calls -= countOfDefaultCases - 1;

         // Generate the end of the comment and return the defaultAction as next action to be generated
         codegen.IndentAndAppendLine("default: break; */")
         .IndentExactly()
         .AppendLine("}");

         return defaultAction;
      }

   }

   internal sealed partial class ReduceAction : ParserActionWithNextAction
   {
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         // generate description
         if (GlobalSettings.GenerateComments.Value)
         {
            codegen.IndentExactly();
            codegen.Append("/* ");
            if (StateStackAdjustment != 0)
            {
               codegen.Append("sAdjust: ");
               codegen.Append(-StateStackAdjustment);
               if (AttributeStackAdjustment != 0)
                  codegen.Append(", ");
            }
            if (AttributeStackAdjustment != 0)
            {
               codegen.Append("aAdjust: ");
               codegen.Append(AttributeStackAdjustment);
            }
            if (StateStackAdjustment != 0 || AttributeStackAdjustment != 0)
               codegen.IndentExactly().Append(" * ");

            codegen.IndentAndAppendLines(Description, " * ");
            codegen.AppendLine(" */");
         }


         // Generate instructions to handle the state stack
         if (this.StateStackAdjustment != 0)
         {
            codegen.Indent().AppendFormat(
               GlobalSettings.StateStackRemoveInstructionFormat.Value,
               GlobalSettings.StateStack.Value,
               this.StateStackAdjustment);
         }

         // generate instructions to handle the attribute stack and to call the method

         if (this.AttributeStackAdjustment != 0 && this.FirstAdjustAttributeStackThenCallMethod)
            codegen.GenerateAttributeStackAdjustment(this.AttributeStackAdjustment);

         if (this.SemanticMethod != null)
         {
            codegen.IndentExactly()
               .AppendLine() // empty line preceding method call
               .GenerateSemanticMethodCall(this.SemanticMethod)
               .AppendLine(";")
               .AppendLine(); // empty line following method call
         }

         if (this.AttributeStackAdjustment != 0 && !this.FirstAdjustAttributeStackThenCallMethod)
         {
            codegen.Indent();
            codegen.GenerateAttributeStackAdjustment(this.AttributeStackAdjustment);
         }

         // generate end of line
         if (codegen.LineLength > 0)
            codegen.AppendLine();

         // return next action
         accept = false;
         Debug.Assert(
            this.NextAction != null,
            $"Error in Phase 5: {nameof(this.NextAction)} == null"
            );

         return this.NextAction;
      }
   }

   internal sealed partial class PrioritySelectAction : ConditionalAction
   {
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         accept = false;
         return NextAction;
      }
   }

   internal sealed partial class PriorityBranchAction : ParserAction
   {
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         // TOCHECK The sequence of the actions is relevant if there are some with same priority: Is it reproducible?
         // If there is a constant condition, this is always first. Only this may be a shift operation
         // which should have higher priority than LookAhead operations.
         codegen.IndentExactly();
         codegen.AppendLine("/* Dynamic priority controlled actions */");
         Int32 PrioritiesCount = DynamicPriorityActions.Count + ((ConstantPriorityAction == null) ? 0 : 1);

         if (PrioritiesCount == 2)
            return GeneratePriorityBranchAsIfThen(codegen, out accept);

         return GeneratePriorityBranchAsSwitch(codegen, out accept);
      }

      private ParserAction GeneratePriorityBranchAsIfThen(P5CodegenCS codegen, out Boolean accept)
      {
         codegen.Append(" if (");
         codegen.IncrementIndentationLevel();

         // First argument of comparision
         ParserActionWithNextAction IfDependentAction;
         if (ConstantPriorityAction != null)
         {
            codegen.Append(ConstantPriority);
            IfDependentAction = ConstantPriorityAction;
         }
         else
         {
            codegen
              .AppendLine() // empty line preceding method call
              .GenerateSemanticMethodCall(PriorityFunctions[0]);
            IfDependentAction = (ConditionalAction)DynamicPriorityActions[0];
         }

         // ">=" and second argument of comparision and end of condition
         codegen.AppendLine()
            .DecrementIndentationLevel().IndentExactly().Append(">= ").IncrementIndentationLevel()
            .GenerateSemanticMethodCall(PriorityFunctions[^1])
            .AppendLine().DecrementIndentationLevel().IndentExactly().AppendLine(")");

         // Conditional action
         codegen.IncrementIndentationLevel();
         P5GenerateCode.GenerateCodeSequence
            (codegen,
            IfDependentAction.NextAction,
            accept: IfDependentAction is TerminalTransition,
            labelMustBeGenerated: false);
         codegen.DecrementIndentationLevel();

         accept = DynamicPriorityActions[^1] is TerminalTransition; // TOCHECK must be adapted if another action is the default
         return ((ConditionalAction)DynamicPriorityActions[^1]).NextAction;
      }

      private ParserAction GeneratePriorityBranchAsSwitch(P5CodegenCS codegen, out Boolean accept)
      {

         codegen
            .IndentExactly()
            .Append(GlobalSettings.PriorityDynamicSwitchAndStartOfMathExpression.Value); // "switch(Methods.IndexOfMaximum("

         // IndexOfMaximum has to return the index of the 1st occurence of the greatest argument. Then:
         // TerminalTransition (priority 0) has priority over dynamic priority value 0 because it is generated 1st !! 

         /* 1st argument: */
         bool Is1stArgument = true;
         if (ConstantPriorityAction != null)
         {
            codegen.Append(ConstantPriority).Append(", ");
            Is1stArgument = false;
         }

         codegen.IncrementIndentationLevel();

         for (Int32 i = 0; i < DynamicPriorityActions.Count; i++)
         {
            if (!Is1stArgument)
               codegen.Append(",");
            Is1stArgument = false;

            codegen.IndentExactly()
               .AppendLine() // empty line preceding method call
               .GenerateSemanticMethodCall(PriorityFunctions[i])
               .AppendLine();
         }
         codegen
            .AppendLine() // empty line following method calls
            .Indent().Append(GlobalSettings.PriorityDynamicSwitchEndOfMatchExpression.Value); //"))"

         codegen.DecrementIndentationLevel();
         codegen.IndentExactly().Append("{");
         // end of generating switch condition

         // generate switch cases
         Int32 CaseCount = 0;
         if (ConstantPriorityAction != null)
         {
            ConditionalAction ca = (ConditionalAction)(ConstantPriorityAction);

            codegen.IndentExactly()
               .AppendFormat(
                  GlobalSettings.PriorityDynamicSwitchCaseLabelFormat.Value,
                  CaseCount++
                  )
               .IncrementIndentationLevel();
            P5GenerateCode.GenerateCodeSequence(
               codegen,
               ca.NextAction, // ca may be TerminalTransition or LookaheadAction: condition is included in condition of PrioritySelect 
               accept: ca is TerminalTransition,
               labelMustBeGenerated: false);
            codegen.DecrementIndentationLevel();
         }
         // do not generate a case for the last dynamic priority: this
         // is returned to be generated after the switch instruction (default)
         for (Int32 i = 0; i < DynamicPriorityActions.Count - 1; i++)
         {
            codegen.IndentExactly().Append("case ").Append(CaseCount++).Append(": ");
            ConditionalAction ca = (ConditionalAction)DynamicPriorityActions[i];
            codegen.IncrementIndentationLevel();
            P5GenerateCode.GenerateCodeSequence
               (codegen,
               ca.NextAction,
               accept: false,  // dynamic priority actions always look ahead: can not be accepting actions
               labelMustBeGenerated: false);
            codegen.DecrementIndentationLevel();
         }
         codegen.IndentExactly().Append("}");

         accept = DynamicPriorityActions[^1] is TerminalTransition;
         return ((ConditionalAction)DynamicPriorityActions[^1]).NextAction;
      }


   }

   internal sealed partial class ErrorhandlingAction : ConditionalAction
   {
      /// <summary>
      /// Generates the assignment to StateNumber and the call of ErrorHandler dependent on the global variables or
      /// if there is no error handling method defined, returns only the next action (error halt)
      /// </summary>
      /// <param name="accept"></param>
      /// <returns>next action: <see cref="ErrorHaltAction>"/> </returns>
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         if (!String.IsNullOrEmpty(GlobalSettings.NameOfErrorHandlerMethod.Value))
         {
            // generate ErrorHandlerCall   ErrorHandler(ErrorStateNumber, StateDescription, ParserInput);
            codegen
               .Indent()
               .Append("if (")
               .Append(GlobalSettings.NameOfErrorHandlerMethod.Value)
               .Append('(')
               .Append(this.IdNumber + 1)
               .Append(", ");
            if (String.IsNullOrEmpty(GlobalSettings.PrefixOfStateDescriptionConstant.Value))
               codegen.Append(@""""""); // empty error description
            else
            {
               codegen
               .Append(GlobalSettings.PrefixOfStateDescriptionConstant.Value)
               .Append(this.State.IdNumber + 1);
            }
            codegen
               .Append(", ")
               .Append(GlobalSettings.InputExpression.Value)
               .AppendLine("))");

            if (this.State.StateStackNumber >= 0)
            { // generate {_s.Remove(..); goto state...;}
               codegen
                  .IncrementIndentationLevel()
                  .Indent()
                  .AppendLine("{")
                  .Indent().AppendFormat(
                     GlobalSettings.StateStackRemoveInstructionFormat.Value,
                     GlobalSettings.StateStack.Value,
                     1)
                  .GenerateGoto(this.State, accept: false)
                  .IndentAndAppendLine("};")
                  .DecrementIndentationLevel();
            }
            else
            { // generate goto state...;
               codegen
                  .IncrementIndentationLevel()
                  .Indent()
                  .GenerateGoto(this.State, accept: false)
                  .DecrementIndentationLevel();
            }

         }

         accept = false;
         return this.NextAction; // errorhandlingAction.NextAction == ErrorHalt  
      }
   }

   internal sealed partial class HaltAction : ParserActionWithNextAction
   {
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         codegen.IndentExactly();
         Int32 numberOfAttributesToStore = this.AttributestackAdjustment;
         Debug.Assert(numberOfAttributesToStore >= 0);

         if (GlobalSettings.GenerateComments.Value)
            codegen.IndentAndAppendLine(
               "// Halt: a definition of the startsymbol with " 
               + numberOfAttributesToStore.ToString(CultureInfo.InvariantCulture) + " attributes has been recognized.");

         if (GlobalVariables.ListOfAllStates[0].StateStackNumber >= 0)
            codegen.IndentExactly().AppendFormat(
                     GlobalSettings.StateStackRemoveInstructionFormat.Value,
                     GlobalSettings.StateStack.Value,
                     1);
         if (numberOfAttributesToStore != 0)
            codegen.IndentExactly().AppendFormat(
               GlobalSettings.AttributesCopyAndRemoveInstructionFormat.Value,
               GlobalSettings.AttributesOfSymbolStack,
               GlobalSettings.AttributeStack.Value,
               numberOfAttributesToStore
               );

         accept = false;
         return this.NextAction;
      }
   }

   internal sealed partial class ErrorHaltAction : ParserActionWithNextAction
   {
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         if (GlobalSettings.GenerateComments.Value)
            codegen.IndentAndAppendLine(
              "// This point is reached after an input error has been found");

         // generate _s.Pop(x)
         if (GlobalVariables.NumberOfStatesWithStateStackNumber > 0)
            codegen.IndentExactly()
                   .AppendFormat(
                     GlobalSettings.StateStackResetInstructionFormat.Value,
                     GlobalSettings.StateStack.Value,
                     GlobalSettings.StateStackNameOfInitialCountVariable
                     );

         // generate _a.Remove(x)
         if (GlobalVariables.reductionsModifyAttributStack)
            codegen.IndentExactly()
               .AppendFormat(
               GlobalSettings.AttributeStackResetInstructionFormat.Value,
                  GlobalSettings.AttributeStack.Value,
                  GlobalSettings.AttributeStackNameOfInitialCountVariable.Value
               )
               .AppendLine();

         // generate additional instruction
         if (!String.IsNullOrEmpty(GlobalSettings.ErrorHaltInstruction.Value))
            codegen.IndentAndAppendLine(GlobalSettings.ErrorHaltInstruction.Value);

         accept = false;
         return GlobalVariables.ErrorHaltInstance.NextAction;
      }
   }

   internal sealed partial class EndOfGeneratedCodeAction : ParserAction
   {
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         accept = false;
         codegen.Indent().AppendLine(';');
         return null;
      }
   }

   internal sealed partial class PushStateAction : ParserActionWithNextAction
   {
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         accept = false;
         codegen.GenerateStateStackPush(this.StateStackNumber);
         return this.NextAction;
      }
   }

   internal sealed partial class ParserState : ParserAction, IELementOfPartition
   {
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         /// <summary>
         /// generates all actions of the state including nested actions of some other states
         /// </summary>
         /// <param name="Accept"></param>
         /// <param name="State"></param>
         /// <param name="sbTemp"></param>
         /// 
         /// <returns>action which has to be generated as next action</returns>
         // private ParserAction? GenerateState(P5CodegenCS codegen, out Boolean Accept, ParserState State, StringBuilder sbTemp)

         // Generate description
         var sbTemp = GlobalVariables.stringBuilderPool.Get();
         sbTemp.Capacity = 2000;
         sbTemp.Clear();

         CoreItems.AppendToSB(sbTemp);

         if (ContainsErrorHandlerCall
            && !String.IsNullOrEmpty(GlobalSettings.PrefixOfStateDescriptionConstant.Value)
            && !String.IsNullOrEmpty(GlobalSettings.NameOfErrorHandlerMethod.Value)
            )
         {
            // Generate assignment to VariableNameStateDescription (if defined)
            codegen.Indent();
            codegen.Append("const String ");
            codegen.Append(GlobalSettings.PrefixOfStateDescriptionConstant.Value);
            codegen.Append(IdNumber + 1);
            codegen.Append(" =");
            codegen.AppendLine();

            // Generate the item descriptions to be assigned to the variable
            sbTemp.Replace("\\", "\\\\").Replace("\"", "\\\""); // escape the symbols which are not allowed in strings

            codegen.IndentAndAppendLinesWithSeparator(
                linesToAppend: sbTemp.ToString() // the strings describing the items of the state
                , stringPrecedingFirstLine: "     \""  // indentation and character """" in front of each string
                , separatorAtEndofLine: GlobalSettings.StringNewLineWithEscapes + "\""  // at end of each string except the last
                , separatorAtNewLine: "   + \"" // before each string except the first
                , stringAtEndOfLastLine: "\";" // after the last string
                );
            codegen.AppendLine();
         }
         else if (GlobalSettings.GenerateComments.Value)
         {
            sbTemp.Replace("*/", "* /"); // escape the symbols which are not allowed in comment

            codegen.IndentExactly().Append("/*");

            codegen.IndentAndAppendLinesWithSeparator(
                linesToAppend: sbTemp.ToString() // the strings describing the items of the state
                , stringPrecedingFirstLine: " "  // indentation and character """" in front of each string
                , separatorAtEndofLine: ""  // at end of each string except the last
                , separatorAtNewLine: " * " // before each string except the first
                , stringAtEndOfLastLine: "" // after the last string
                );
            codegen.Append(" */");
         }

         GlobalVariables.stringBuilderPool.Return(sbTemp);

         codegen.Indent();

         // generate push to the state stack if necessary, generate a comment if push has been moved to actions
         if (StateStackNumber >= 0)
            codegen.GenerateStateStackPush(StateStackNumber);
         else if (StateStackNumber <= -2)
         {
            codegen.IndentExactly();
            codegen.AppendLine("// *Push(" + (-StateStackNumber - 2).ToString(CultureInfo.InvariantCulture) + ')');
         }

         // The call of "FetchSymbol();" must be generated only if the state contains actions, which check the input symbol.
         // This is prepared  by shortening chains in phase 4 und implemented by the following.

         if (!(PossibleInputTerminals!.IsComplete))
         {
            // there has been look ahead: no InstructionAssignSymbol needed
            // Show restricted set of possible symbols
            GenerateConditionAsComment(codegen, PossibleInputTerminals!, checkingForbiddenTerminals: false);

         }
         else if (Actions.Count >= 1)
         {
            // If the state contains more than one action or one action, which is a terminal transition (all symbols allowed),
            // "PeekSymbol()" must be generated but not if it is only one unconditional action
            if (Actions.Count > 1
                || Actions[0] is LookaheadAction
                || Actions[0] is TerminalTransition
                || Actions[0] is ErrorhandlingAction
                )
            {
               codegen.IndentExactly()
                     .AppendWithOptionalLinebreak(GlobalSettings.InputAssignInstruction.Value);
            }
            else
            {
               accept = false; // returned action is not a terminal transition
               return Actions[0];
            }
         }
         else // State.Actions.Count = 0
         {
            // This shouldn't happen 'cause each state should at least contain one action (may be error action)
            GlobalVariables.OutputMessage(
                MessageTypeOrDestinationEnum.Error, "Check your grammar: no actions in state " 
                + (IdNumber + 1).ToString(CultureInfo.InvariantCulture));
            throw new ErrorInGrammlatorProgramException("Can not generate state without actions");
         }

         return SortAndGenerateConditionalActionsOfState(codegen, out accept); // => next action to generate or null
      }

      /// <summary>
      /// Sort actions depending on the weight of the terminals symbols and an estimation of the complexity
      /// of the if conditions to be generated and then generate "if (...){...}" code
      /// </summary>
      /// <param name="state">the state whose conditional actions are to be generated</param>
      /// <param name="accept">if the result is not null but an action then accept indicates that a call of accept has to be generated before the action will be generated</param>
      /// 
      /// <returns>Null or an unconditional action which has to be generated next</returns>
      /// <exception cref="ErrorInGrammlatorProgramException"></exception>
      private ParserAction? SortAndGenerateConditionalActionsOfState(P5CodegenCS codegen, out Boolean accept)
      {
         /* only one action in state => return it 
            only few actions         => generate if instructions
            else                     => generate switch instruction
         */

         if (IfComplexity > GlobalSettings.GenerateSwitchStartingLevel.Value)
            return GenerateSwitchWithActionsOfState(codegen, out accept); // <--- generate switch

         // Sort the actions dependent on their terminal inp.
         // Sort criteria and order see CompareWeightandConditionComplexity
         if (GlobalVariables.NumberOfTerminalSymbols > 0)
            Actions.Sort(P5GenerateCode.CompareWeightandConditionComplexity);

         // Exchange an ErrorhandlingAction at the end with the preceding action,
         // because an ErrorhandlingAction has no NextAction which can be generated next without goto
         if (Actions.Count > 1 &&
            (Actions[^1] is ErrorhandlingAction
            || (Actions[^1] is LookaheadAction la && la.NextAction is ErrorHaltAction)))
         {
            (Actions[^1], Actions[^2]) = (Actions[^2], Actions[^1]);
         }

         return GenerateConditionalActionsOfState(codegen, out accept);// <--- generate if(....)
      }

      /// <summary>
      /// The state contains no unconditional action.
      /// Generate the conditional actions of the state and return an action (if any) to generate next.
      /// </summary>
      /// <param name="State"></param>
      /// <param name="Accept"></param>
      /// 
      /// <returns>next <see cref="ParserAction"/> to generate</returns>
      private ParserAction? GenerateConditionalActionsOfState(P5CodegenCS codegen, out Boolean Accept)
      {
         // all terminal symbols which are condition of one action can be ignored
         // in the conditions of all following actions (are not relevant)
         var relevantSymbols = new IndexSet(PossibleInputTerminals);

         for (Int32 i = 0; i < Actions.Count - 1; i++)
         {
            var a = (ConditionalAction)Actions[i];
            GenerateOneConditionalAction(codegen, a, relevantSymbols,
               a.NextAction is ErrorhandlingAction || a.NextAction is ErrorHaltAction); // Modifies relevantSymbols
         }

         var LastAction = (ConditionalAction)Actions[^1];

         ParserAction? nextAction = LastAction; //CHECK may nextAction be null?
         if (LastAction is TerminalTransition || LastAction is LookaheadAction)
            nextAction = LastAction.NextAction; // TOCHECK must nextAction.Calls be reduced by 1 ??

         // Generate condition as comment, but only if not trivial and if more than 1 action 
         // (else it would duplicate the condition generated at the start of the state)
         IndexSet suppressedCondition = LastAction.TerminalSymbols;
         if (Actions.Count > 1 && nextAction != null && !suppressedCondition.IsComplete)
         {
            GenerateConditionAsComment(codegen, suppressedCondition,
               nextAction is ErrorhandlingAction || nextAction is ErrorHaltAction);
         }

         Accept = LastAction is TerminalTransition;
         return nextAction;
      }

      /// <summary>
      /// Sort actions depending on the terminal symbols
      /// then generate "switch(...)case ...:" code
      /// </summary>
      /// <param name="this"></param>
      /// <param name="accept"></param>
      /// 
      /// <returns>Unconditional action which has to be generated</returns>
      /// <exception cref="ErrorInGrammlatorProgramException"></exception>
      private ParserAction GenerateSwitchWithActionsOfState(P5CodegenCS codegen, out Boolean accept)
      {
         // Sort the actions by their first symbol to improve readability of the generated code
         if (GlobalVariables.NumberOfTerminalSymbols > 0)
            Actions.Sort(P5GenerateCode.CompareIndexOfFirstTrueElement);

         codegen.IndentExactly();
         codegen.AppendWithOptionalLinebreak("switch (");
         codegen.Append(GlobalSettings.InputExpression.Value);
         codegen.Append(")");
         codegen.GenerateBeginOfBlock();

         Int32 IndexOfLastPossibleTerminal = PossibleInputTerminals!.IndexOfLastBit(true);
         Int32 IndexOf1stPossibleTerminal = PossibleInputTerminals!.IndexOfFirstBit(true);

         Int32 LeadingCount = 0, TrailingCount = 0;
         ConditionalAction? LeadingAction = null, TrailingAction = null;

         // for each action generate 0 or more "case ...:"-clauses and then the action
         // or for special cases a comment "..... // see end of switch"
         // if 
         for (Int32 i = 0; i < Actions.Count; i++)
         {
            var ThisAction = (ConditionalAction)Actions[i];

            IndexSet Terminals = ThisAction.TerminalSymbols;
            Int32 TerminalIndex = Terminals.IndexOfFirstBit(true);
            Boolean IsDefaultAction = false;

            // Special case: default action if Terminals contains the first possible terminal
            if (TerminalIndex == IndexOf1stPossibleTerminal)
            {
               // remember this action and the count of leading terminals
               Debug.Assert(LeadingCount == 0, "Phase5: LeadingCount already != 0");
               LeadingAction = ThisAction;
               LeadingCount = Terminals.IndexOfNextBit(IndexOf1stPossibleTerminal, false) - IndexOf1stPossibleTerminal;
               TerminalIndex = Terminals.IndexOfNextBit(IndexOf1stPossibleTerminal + LeadingCount);
               IsDefaultAction = true;

               // generate first part of comment "// <= xxx"
               codegen.IndentExactly()
                 .Append("// <= ")
                 .Append(GlobalVariables.TerminalSymbols[IndexOf1stPossibleTerminal + LeadingCount - 1].NameToGenerate);
            }

            // Special case: default action if Terminals contains the last possible terminal
            if (Terminals.Get(IndexOfLastPossibleTerminal))
            {
               // remember this action and the count of trailing terminals
               Debug.Assert(TrailingCount == 0, "Phase5: TrailingCount already != 0");
               TrailingAction = ThisAction;
               TrailingCount = IndexOfLastPossibleTerminal - Terminals.IndexOfPrecedingBit(IndexOfLastPossibleTerminal, false);
               IsDefaultAction = true;

               // generate first part of comment "// >= yyy"
               codegen.IndentExactly()
                  .Append("// >= ")
                  .Append(GlobalVariables.TerminalSymbols[IndexOfLastPossibleTerminal - TrailingCount + 1].NameToGenerate);
            }

            if (IsDefaultAction)
            {
               // generate second part of comment ": goto aaa; // see end of switch"
               codegen.Append(": ")
                  .Append("goto ")
                  .Append(P5CodegenCS.GotoLabel(ThisAction))
                  .Append("; // see end of switch");
            }

            // if remaining terminal symbols, for each generate "case xxx:"
            Boolean CreatedCase = false;
            while (TerminalIndex < IndexOfLastPossibleTerminal - TrailingCount + 1)
            {
               CreatedCase = true;
               // generate "case TerminalSymbol: " 
               codegen.IndentExactly()
                  .Append("case ")
                  .Append(GlobalVariables.TerminalSymbols[TerminalIndex].NameToGenerate)
                  .Append(":");

               TerminalIndex = Terminals.IndexOfNextBit(TerminalIndex);
            }

            if (CreatedCase)
            {
               codegen.IncrementIndentationLevel();

               if (IsDefaultAction)
               {
                  // adjust the statistics because here a "goto label" will be generated
                  // in addition to the code generated in the default action.
                  // If the statistics are not adjusted those code might be generated without label.

                  // if the code has been generated already (counters are 0)
                  //     the statistics must not be changed

                  // Todo If AcceptCalls or Calls is 0, labels may have been not generated
                  // Todo If AcceptCalls or Calls is <0 teh value should be decremented

                  if (ThisAction is TerminalTransition && ThisAction.NextAction.AcceptCalls > 0)
                     ThisAction.NextAction.AcceptCalls++;
                  else if (ThisAction is ErrorhandlingAction && ThisAction.Calls > 0)
                     ThisAction.Calls++;
                  else if (ThisAction.NextAction.Calls > 0)
                     ThisAction.NextAction.Calls++;

                  // generate goto
                  codegen.GenerateGoto(
                     action: ThisAction is ErrorhandlingAction ? ThisAction : ThisAction.NextAction,
                     accept: ThisAction is TerminalTransition);
               }
               else
               {
                  P5GenerateCode.GenerateCodeSequence(codegen, ThisAction, labelMustBeGenerated: false);
               }

               codegen.DecrementIndentationLevel();
            }
         }

         Debug.Assert(LeadingAction != null && TrailingAction != null, "Leading or Trailing Action is null");

         // generate end of switch statement
         codegen.GenerateEndOfBlock("end of switch");

         // implement "default:" as fall through

         ConditionalAction Action1 = LeadingAction, Action2 = TrailingAction;
         ParserAction Action1Generate = Action1 is ErrorhandlingAction ? Action1 : Action1.NextAction;
         ParserAction Action2Generate = Action2 is ErrorhandlingAction ? Action2 : Action2.NextAction;

         Boolean ActionsSwapped = false;

         // If there are two different default actions
         if (Action1Generate != Action2Generate)
         {
            // yes: generate if
            codegen.IndentExactly();
            codegen.Append("if (");
            codegen.Append(GlobalSettings.InputExpression.Value);

            // prefer action which generates goto as first action
            // TODO else prefer action which does not generate a label
            if (P5GenerateCode.GeneratesGoto(generateAccept: Action2 is TerminalTransition, Action2Generate, codegen.IndentationLevel) &&
                !P5GenerateCode.GeneratesGoto(generateAccept: Action1 is TerminalTransition, Action1Generate, codegen.IndentationLevel))
            {
               static void Swap<T>(ref T a, ref T b)
               {
                  (b, a) = (a, b);
               }

               Swap(ref Action1, ref Action2);
               Swap(ref Action1Generate, ref Action2Generate);
               ActionsSwapped = true;

               codegen.Append(" >= ")
                  .Append(GlobalVariables.TerminalSymbols[IndexOfLastPossibleTerminal - TrailingCount + 1].NameToGenerate);
            }
            else
            {
               codegen.Append(" <= ")
                  .Append(GlobalVariables.TerminalSymbols[IndexOf1stPossibleTerminal + LeadingCount - 1].NameToGenerate);
            }

            codegen.Append(") ");

            codegen.IncrementIndentationLevel();
            P5GenerateCode.GenerateCodeSequence(codegen, Action1, labelMustBeGenerated: false);
            codegen.DecrementIndentationLevel();
         }

         // generate comment as Debug.Assert
         GenerateCommentAtEndOfSwitch(codegen, IndexOf1stPossibleTerminal, IndexOfLastPossibleTerminal, LeadingCount, TrailingCount, Action1Generate, Action2Generate, ActionsSwapped);

         // generate Action2
         accept = Action2 is TerminalTransition; // TOCHECK try to avoid the out parameter "accept"?
         return Action2Generate;
      }

      private static void GenerateCommentAtEndOfSwitch(P5CodegenCS codegen, Int32 IndexOf1stPossibleTerminal, Int32 IndexOfLastPossibleTerminal,
         Int32 LeadingCount, Int32 TrailingCount, ParserAction Action1Generate, ParserAction Action2Generate, Boolean ActionsSwapped)
      {
         if (GlobalSettings.NameOfAssertMethod.Value == "")
            return;
         codegen.IndentExactly();
         codegen
            .Append(GlobalSettings.NameOfAssertMethod.Value).
            Append('(');
         if (Action1Generate == Action2Generate)
         {
            codegen.Append(GlobalSettings.InputExpression.Value)
               .Append(" <= ")
               .Append(GlobalVariables.TerminalSymbols[IndexOf1stPossibleTerminal + LeadingCount - 1].NameToGenerate)
               .Append(" || ")
               .Append(GlobalSettings.InputExpression.Value)
               .Append(" >= ")
               .Append(GlobalVariables.TerminalSymbols[IndexOfLastPossibleTerminal - TrailingCount + 1].NameToGenerate);
         }
         else if (ActionsSwapped)
         {
            codegen.Append(GlobalSettings.InputExpression.Value)
               .Append(" <= ")
               .Append(GlobalVariables.TerminalSymbols[IndexOf1stPossibleTerminal + LeadingCount - 1].NameToGenerate);
         }
         else
         {
            codegen.Append(GlobalSettings.InputExpression.Value)
               .Append(" >= ")
               .Append(GlobalVariables.TerminalSymbols[IndexOfLastPossibleTerminal - TrailingCount + 1].NameToGenerate);
         }
         codegen.AppendLine(");")
            .AppendLine("");
      }

      /// <summary>
      /// 
      /// </summary>
      /// <param name="codegen"></param>
      /// <param name="a"></param>
      /// <param name="relevantSymbols"></param>
      /// <param name="checkForbiddenTerminals">true if it is the condition of an error action which checks forbidden Symbols</param>
      private static void GenerateOneConditionalAction(P5CodegenCS codegen, ConditionalAction a, IndexSet relevantSymbols, Boolean checkForbiddenTerminals)
      {
         codegen.IndentExactly();
         codegen.AppendWithOptionalLinebreak("if (");

         codegen.IncrementIndentationLevel();

         GenerateCondition(codegen, a.TerminalSymbols, relevantSymbols, checkForbiddenTerminals);

         codegen.AppendWithOptionalLinebreak(") ");

         P5GenerateCode.GenerateCodeSequence(codegen, a, labelMustBeGenerated: false);

         // TODO if !GlobalSettings.InputElementsAreTerminals: is context tested (not longer relevant)?
         relevantSymbols.ExceptWith(a.TerminalSymbols);

         codegen.DecrementIndentationLevel();
      }

      /// <summary>
      /// A block of equal bits starting with a relevant bit and ending with a relevant bit.
      /// </summary>
      internal struct BlockOfEqualBits
      {
         /// <summary>
         /// false if the relevant bits of the block are false, true if the relevant bits are true
         /// </summary>
         internal Boolean BlockType;

         /// <summary>
         /// blockStart is the index of the first relevant bit of the block
         /// </summary>
         internal Int32 BlockStart;

         /// <summary>
         /// blockLength is the number of relevant bits
         /// </summary>
         internal Int32 RelevantBitsCount;

         /// <summary>
         /// blockEnd is the indexc of the last relevant bit of the block
         /// </summary>
         internal Int32 BlockEnd;
      }

      private static void GenerateCondition(P5CodegenCS codegen, IndexSet condition, IndexSet relevant, Boolean checkingForbiddenTerminals)
      {
         // Special case if error action and no relevant symbols are true
         // TODO generate a condition that only checks the bounds  "xxx<1stTerminals || xxx>lastTrminal" or 

         // In the worst case all symbols are relevant and the condition changes at each index
         // so that each symbol needs an own block

         // Reuse the blockList            blockList.Clear();
         /// <summary>
         /// list of blocks of symbols inside TerminalSymbols to be reused in <see cref="P5GenerateCode"/>
         /// </summary>
         List<BlockOfEqualBits> blockList = new(GlobalVariables.NumberOfTerminalSymbols);

         // determine consecutive blocks (indexes) of equal values of Condition ignoring not relevant symbols (indexes)
         ComputeBlocklist(condition, relevant, blockList);

         // TODO if !GlobalSettings.InputElementsAreTerminals: take account of context testing
         Boolean test1sRecommended = AnalyseBlockList(blockList, out Int32 Complexity);

         if (Complexity > GlobalSettings.GenerateFlagTestStartingLevel.Value * 100 - 50
            // 350 allows e.g. 2 comparisions and 1 logical operator
            && (GlobalSettings.NameOfFlagTestMethod.Value != ""
                || GlobalVariables.TerminalSymbolsAreFlags)
            )
         {
            GenerateIsIn(codegen, condition, relevant, checkingForbiddenTerminals);

            // The generated code contains 1 "<<"-operator, 1 "&"-bitoperator, 1 "==0" comparision, optional a "!"-operator,
            // 1 method call (may be inlined by JIT-Compiler), some additions of constants which are evaluated at compile time
         }
         else if (test1sRecommended)
         {
            GenerateTest1s(codegen, blockList, checkingForbiddenTerminals);
         }
         else
         {
            GenerateTest0s(codegen, blockList, checkingForbiddenTerminals);
         }

         blockList.Clear(); // keep capacity for reuse
         return;
      }

      private static void GenerateConditionAsComment(P5CodegenCS codegen, IndexSet condition, Boolean checkingForbiddenTerminals)
      {
         if (GlobalSettings.NameOfAssertMethod.Value == "")
            return;
         codegen
            .IndentExactly()
            .AppendWithOptionalLinebreak(GlobalSettings.NameOfAssertMethod.Value)
            .Append('(')
            .IncrementIndentationLevel();
         // codegen.Indent(nestingLevel + 1);
         // force complete check, e.g. /* MyCharacterInput.Symbol >= CharGroupEnum.Digit */
         /* MyCharacterInput.Symbol == CharGroupEnum.Digit || MyCharacterInput.Symbol == CharGroupEnum.Letter */
         GenerateCondition(codegen, condition, GlobalVariables.AllTerminalSymbols, checkingForbiddenTerminals);

         // The restriction on relevant symbols would produce an optimized condition resulting on preceding checks, e.g.
         /* MyCharacterInput.Symbol >= CharGroupEnum.Digit */
         // GenerateCondition(Condition, RelevantSymbols); 

         codegen.AppendWithOptionalLinebreak(");");
         codegen.DecrementIndentationLevel();
      }


      /// <summary>
      /// Creates a list of blocks each of which desribes a sequence of contiguous 0s resp. 1s of the given condition.
      /// Bits which are not relevant are interpreted as 0s or as 1s arbitrarily to get large blocks.
      /// </summary>
      /// <param name="Condition">A <see cref="IndexSet"/> with the terminal symbols to be checked</param>
      /// <param name="Relevant">A <see cref="IndexSet"/> with the terminal symbols which are not yet checked</param>
      /// <param name="BlockList">The blocklist to be filled with information</param>
      private static void ComputeBlocklist(
          IndexSet Condition,
          IndexSet Relevant,
          List<BlockOfEqualBits> BlockList)
      {
         Int32 firstRelevant = Relevant.IndexOfFirstBit(true);
         Int32 lastRelevant = Relevant.IndexOfLastBit(true);

         if (firstRelevant == -1)
         {
            if (GlobalSettings.InputPeekChecksBounds.Value)
               return; // no relevant elements, blocklist remains empty
            else
               Debug.Assert(false, "! InputPeekChecksBounds and no relevant elements: checking bounds is not yet implemented");
            // this may occur if all terminals are allowed and an error action must be generated only to check the bounds
         }

         Boolean BlockType;
         Int32 BlockStart;
         Int32 RelevantBitsCount;
         Int32 BlockEnd;

         Int32 NextRelevant = firstRelevant; // start with first relevant

         while (true)
         { // loop over all blocks
            Debug.Assert(NextRelevant <= lastRelevant);

            BlockStart = NextRelevant;
            BlockType = Condition.Get(BlockStart);
            RelevantBitsCount = 1;
            BlockEnd = BlockStart;

            // increment BlockEnd and RelevantBitsCount until end of block or end of relevant part of condition
            while (++NextRelevant <= lastRelevant)
            {
               // skip all elements which are not relevant
               NextRelevant = Relevant.IndexOfNextBit(NextRelevant - 1);
               // Note: Relevant.FindNextTrue(LastRelevant - 1) == LastRelevant

               Debug.Assert(NextRelevant <= lastRelevant && Relevant.Get(NextRelevant));

               // beyond end of block?
               if (Condition.Get(NextRelevant) != BlockType)
                  break; // yes

               // this is until now the last relevant bit 
               BlockEnd = NextRelevant;
               // count the relevant bits
               RelevantBitsCount++;
            }

            // found a block
            BlockList.Add(new BlockOfEqualBits
            {
               BlockType = BlockType,
               BlockStart = BlockStart,
               RelevantBitsCount = RelevantBitsCount,
               BlockEnd = BlockEnd
            });

            if (NextRelevant > lastRelevant)
               break; // no more blocks

            Debug.Assert(NextRelevant <= lastRelevant && Relevant.Get(NextRelevant)
               && Condition.Get(NextRelevant) != BlockType); // = blockStart of next block
         }
      }

      /// <summary>
      /// returns true if GenerateTest1s will create less or equal number of comparisions than GenerateTest0s
      /// </summary>
      /// <param name="BlockList">the <paramref name="BlockList"/> will be cleared and filled with information for code generation</param>
      /// <returns>returns true if testing 1 s will create less comparisions</returns>
      private static Boolean AnalyseBlockList(List<BlockOfEqualBits> BlockList, out Int32 complexity)
      {

         if (BlockList.Count <= 1)
         {
            complexity = 1;
            return true; // all true elements (if any) must be testet for true (may occur in conditions generated as comment)
         }

         var Complexity = new Int32[2] { 0, 0 };

         Int32 firstRelevant = BlockList[0].BlockStart;
         Int32 lastRelevant = BlockList[^1].BlockEnd;

         // default: start the loop below with the first block
         Int32 blockIndex = 0;
         BlockOfEqualBits block = BlockList[0];

         // The 1st block may need special handling
         // TODO bear in mind modifications and new GlobalSettings.InputElementsAreTerminals
         if (blockIndex == 0 && block.RelevantBitsCount >= 2)
         {
            // special handlings of first block if it is type true and contains more than 1 element
            Debug.Assert(blockIndex == 0 && block.BlockStart == firstRelevant);

            if (block.BlockEnd == lastRelevant)
            {
               // special case: all elements are equal, condition returns true
               // This should not happen if conflicts are solved properly.
               // This will have the effect that parts of the generated code can never be reached
               ////               codegen.AppendWithOptionalLinebreak("true");

               // complexity[block.blockType ? 1 : 0] += 0; // contains no operator
               complexity = 1;
               return Complexity[0] >= Complexity[1]; // returns true if testing 1 s will create less comparisions
            }

            // special case: at beginning of relevant symbols test of start may be ommitted
            ////var endTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.blockEnd);
            ////codegen.AppendWithOptionalLinebreakAndPrefix(endTerminalSymbol.InputClass, "Symbol <= ");
            ////codegen.AppendWithPrefix(endTerminalSymbol.SymbolEnum, endTerminalSymbol.Bezeichner);
            Complexity[block.BlockType ? 1 : 0] += 100;

            blockIndex++; // special case has been handled here, start the loop below with next block
         }

         // handle all (remaining) blocks with type==true
         for (; blockIndex < BlockList.Count; blockIndex++)
         {
            block = BlockList[blockIndex];

            if (blockIndex >= 2)
            {
               // concatenate code for not the first block etc. with " || " 
               ////codegen.AppendLineAndIndent();
               ////codegen.Append(" || ");
               Complexity[block.BlockType ? 1 : 0] += 100;
            }

            switch (block.RelevantBitsCount)
            {
               // prefer == and != (by using complexity 99) to <=, <, > and >= (with complexity 100) 
               // The codegen examples are taken from GenerateTest1s.
               // The results are also applicable for GenerateTest0s where only the comparision operators differ.
               case 1: // generate: check of equality of one allowed symbol
                  {
                     ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "Symbol == ");
                     ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
                     Complexity[block.BlockType ? 1 : 0] += 99; // == 
                     break;
                  }

               case 2: // generate: check of two allowed symbols or special case
                  {
                     if (block.BlockEnd == lastRelevant)
                     {
                        // special case: at the end of relevant symbols test of end may be ommitted
                        ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "Symbol >= ");
                        ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
                        Complexity[block.BlockType ? 1 : 0] += 100; // >=
                     }
                     else
                     { // compare two symbols: same complexity as test of interval but better readability
                       ////codegen.AppendWithOptionalLinebreakAndPrefix(endTerminalSymbol.InputClass, "Symbol == ");
                       ////codegen.AppendWithPrefix(endTerminalSymbol.SymbolEnum, endTerminalSymbol.Bezeichner);
                       ////codegen.Append(" || ");
                       ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "Symbol == ");
                       ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
                        Complexity[block.BlockType ? 1 : 0] += 100 + 99 + 99; // == || ==
                     }
                     break;
                  }

               default:// generate: check a sequence of three or more allowed symbols
                  {
                     if (block.BlockEnd == lastRelevant)
                     {
                        // special case: at end the of relevant symbols test of end may be ommitted
                        ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "Symbol >= ");
                        ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
                        Complexity[block.BlockType ? 1 : 0] += 100; // >=
                     }
                     else
                     { // generate: test closed interval of three or more symbols
                       ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "(Symbol >= ");
                       ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
                       ////codegen.Append(" && ");
                       ////codegen.AppendWithOptionalLinebreakAndPrefix(endTerminalSymbol.InputClass, "Symbol <= ");
                       ////codegen.AppendWithPrefix(endTerminalSymbol.SymbolEnum, endTerminalSymbol.Bezeichner);
                        Complexity[block.BlockType ? 1 : 0] += 300; // >= && <=
                     }
                     break;
                  }
            }
         }

         if (Complexity[0] >= Complexity[1])
         {
            complexity = Complexity[1];
            return true; // testing 1 s will create less comparisions;
         }
         complexity = Complexity[0];
         return false; // testing 0 s will create less comparisions;
      }

      /// <summary>
      /// Code is generated for all blocks with BlockType == false
      /// </summary>
      /// <param name="blockList"></param>
      private static void GenerateTest0s(P5CodegenCS codegen, List<BlockOfEqualBits> blockList, Boolean checkingForbiddenTerminals)
      {
         /* Code is generated for all blocks  of type == false, typically  "(symbol < 'StartIndexOfBlock' || symbol > EndIndexOfBlock) && ..."  
          * Code for different blocks is concatenated by " && "
          * Cases:
          *    if IsSpecial1stBlock() : "Symbol > EndOfBlock"
          *    blockLength==1         : "Symbol != StartOfBlock"
          *    blockLength==2         : "Symbol != StartOfBlock && Symbol != EndOfBlock"
          *    blocklength >2         : "(Symbol < StartOfBlock || Symbol > EndOfBlock)" with paranthesis
          *    if IsSpecialLastBlock(): "Symbol < StartOfBlock"
          */

         if (blockList.Count <= 1)
         {
            Debug.Assert(false, "all true or false condition should not occur");
            if (blockList.Count == 0 || blockList[0].BlockType)
               codegen.AppendWithOptionalLinebreak("true");
            else
               codegen.AppendWithOptionalLinebreak("false");
            return;
         }

         Int32 firstRelevant = blockList[0].BlockStart;
         Int32 lastRelevant = blockList[^1].BlockEnd;

         // Start the loop below with the first block of type==false
         Int32 blockIndex = blockList[0].BlockType ? 1 : 0;
         BlockOfEqualBits block = blockList[blockIndex];
         Boolean is1stCondition = true;

         // Enclose condition in parentheses if there is one more block of the same type
         Boolean useParentheses = blockList.Count > blockIndex + 2; // example: Count==3, blockIndex=1 => no parentheses

         // The 1st and last block may be handled special: check only BlockEnd (resp. Blockstart) instead of interval
         Boolean IsSpecial1stBlockOf0s() => // generate "Symbol > EndOfBlock && ..." 
              blockIndex == 0 // the 1st block of 0s must be block 0 (no preceding 1s)
              &&
              (GlobalSettings.InputPeekChecksBounds.Value
                ? // the condition will test only defined terminals, simplification allowed (check only BlockEnd) 
                block.RelevantBitsCount >= 2
                :
                //  the condition has to handle values lower than the defined terminals
                //  namely as 1s in case of error actions (complete interval check necessary to allow lower values)
                //  else as 0s (simplification extending interval of 0s) 
                !checkingForbiddenTerminals
              );

         Boolean IsSpecialLastBlockOf0s() => // generate &&"Symbol < StartOfBlock"
              blockIndex == blockList.Count - 1 // the last block of 1s must be the last block (no trailing 0s)
              &&
              (GlobalSettings.InputPeekChecksBounds.Value
                ? // the condition will test only defined terminals, simplification allowed (check only BlockStart)
                block.RelevantBitsCount >= 2
                :
                //  the condition has to handle values higher than the defined terminals
                //  namely as 1s in case of error actions (complete interval check necessary to allow higher values)
                // else as 0s (simplification extending interval of 0s)
                !checkingForbiddenTerminals
              );

         if (blockIndex == 1 && !checkingForbiddenTerminals && !GlobalSettings.InputPeekChecksBounds.Value)
         {
            // Simulate block -1 and generate exclusion of preceding values   "Symbol > EndOfBlock && ..." 
            TerminalSymbol firstTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(0);
            codegen.AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " > ")
               .Append(firstTerminalSymbol.NameToGenerate);
            is1stCondition = false;
         }
         else // blockIndex == 0 !
         if (IsSpecial1stBlockOf0s())
         {
            //  generate "Symbol > EndOfBlock && ..."
            Debug.Assert(blockIndex == 0 && block.BlockStart == firstRelevant);

            if (block.BlockEnd == lastRelevant)
            {
               // Only 1 block; should not occur here, because blockList.Count<=1 has been excluded in If(...
               // so that conditions generated as comment are not only the text "true" or "false"
               codegen.AppendWithOptionalLinebreak("false");
               return;
            }

            TerminalSymbol endTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.BlockEnd);
            codegen.AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " >= ")
               .Append(endTerminalSymbol.NameToGenerate);

            blockIndex += 2; // special case has been handled here, start the loop below with next block of same type 
            is1stCondition = false;
         }

         // handle all (remaining) blocks with type==false
         for (; blockIndex < blockList.Count; blockIndex += 2)
         {
            // all blocks of blockType==true are skipped by blockIndex += 2
            block = blockList[blockIndex];
            Debug.Assert(!block.BlockType);

            TerminalSymbol startTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.BlockStart);
            TerminalSymbol endTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.BlockEnd);

            if (!is1stCondition)
            {
               // concatenate code for not the first block etc. with " && " 
               codegen.AppendLineAndIndent()
                  .Append("&& ");
            }
            is1stCondition = false;

            if (IsSpecialLastBlockOf0s())
            {
               // generate "Symbol > EndOfBlock" 
               codegen.AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " < ")
                  .Append(startTerminalSymbol.NameToGenerate);
            }
            else
               switch (block.RelevantBitsCount)
               {
                  case 1: // generate: check of unequality of one allowed symbol
                     {
                        codegen.AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " != ")
                           .Append(startTerminalSymbol.NameToGenerate);
                        break;
                     }
                  case 2: // generate: check of two forbidden symbols
                     {
                        // compare two symbols: same complexity as test of interval but better readability
                        // no parantheses necessary
                        codegen.AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " != ")
                           .Append(startTerminalSymbol.NameToGenerate)
                           .Append(" && ")
                           .AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " != ")
                           .Append(endTerminalSymbol.NameToGenerate);
                        break;
                     }
                  default:// generate: check a sequence of three or more allowed symbols
                     {
                        // generate: test closed interval of three or more symbols
                        if (useParentheses)
                           codegen.Append('(');
                        codegen.AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " < ")
                           .Append(startTerminalSymbol.NameToGenerate)
                           .Append(" || ")
                           .AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " > ")
                           .Append(endTerminalSymbol.NameToGenerate);
                        if (useParentheses)
                           codegen.Append(')');
                        break;
                     }
               }
         }

         if (blockList[^1].BlockType && !checkingForbiddenTerminals && !GlobalSettings.InputPeekChecksBounds.Value)
         {
            // Append condition to check additional values 
            Debug.Assert(blockIndex == blockList.Count);

            if (!is1stCondition)
            {
               // concatenate code for not the first block etc. with " || " 
               codegen.AppendLineAndIndent();
               codegen.Append("&& ");
            }
            codegen.AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " <= ")
               .Append(GlobalVariables.GetTerminalSymbolByIndex(blockList[^1].BlockEnd).NameToGenerate);
         }
      }

      /// <summary>
      /// Code is generated for all blocks with BlockType == true
      /// </summary>
      /// <param name="blockList"></param>
      private static void GenerateTest1s(P5CodegenCS codegen, List<BlockOfEqualBits> blockList, Boolean checkingForbiddenTerminals)
      {
         /* Code is generated for all blocks of type == true, typically  "(symbol >= 'StartOfBlock' && symbol <= 'EndOfBlock") || ..." 
          * Code for different blocks is concatenated by " || "
          * Cases:
          *    if IsSpecial1stBlock() : "Symbol <= EndOfBlock":   see below
          *    blockLength==1         :  "Symbol == StartOfBlock"
          *    blockLength==2         : "Symbol == StartOfBlock || Symbol == EndOfBlock"
          *    blocklength >2         : "(Symbol >= StartOfBlock && Symbol <= EndOfBlock)" with paranthesis
          *    if IsSpecialLastBlock(): "Symbol >= StartOfBlock": only if ... (see below)
          */

         if (blockList.Count <= 1)
         {
            Debug.Assert(false, "all true or false condition should not occur");
            if (blockList.Count == 0 || blockList[0].BlockType)
               codegen.AppendWithOptionalLinebreak("true");
            else
               codegen.AppendWithOptionalLinebreak("false");
            return;
         }

         // Int32 firstRelevant = blockList[0].BlockStart;
         Int32 lastRelevant = blockList[^1].BlockEnd;

         // Start the loop below with the first block of type==true
         Int32 blockIndex = blockList[0].BlockType ? 0 : 1;
         BlockOfEqualBits block = blockList[blockIndex];
         Boolean is1stCondition = true;

         // Enclose condition in parentheses if there is one more block of the same type
         Boolean useParentheses = blockList.Count > blockIndex + 2; // example: Count==3, blockIndex=1 => no parentheses

         // The 1st and last block may be handled special: check only BlockEnd (resp. Blockstart) instead of interval
         Boolean IsSpecial1stBlockOf1s() =>
              blockIndex == 0 // the 1st block of 1s must be block 0 (no preceding 0s)
              &&
              (GlobalSettings.InputPeekChecksBounds.Value
                ? // the condition will test only defined terminals, simplification allowed (check only BlockEnd) 
                block.RelevantBitsCount >= 2
                :
                //  the condition has to handle values lower than the defined terminals
                //  namely as 1s in case of error actions (check of lower values necessary)
                //  else as 0s (no simplification, complete interval check necessary) 
                checkingForbiddenTerminals
              );

         Boolean IsSpecialLastBlockOf1s(int xxxindex)
         {
            Boolean xxxresult;
            //if (xxxindex != blockList.Count - 1)
            //   return false;
            xxxresult =
               (xxxindex == blockList.Count - 1) // the last block of 1s must be the last block (no trailing 0s)
               &&
               (GlobalSettings.InputPeekChecksBounds.Value
                 ? // the condition will test only defined terminals, simplification allowed (check only BlockStart)
                 block.RelevantBitsCount >= 2
                 :
                 //  the condition has to handle values gigher than the defined terminals
                 //  namely as 1s in case of error actions (check of higher values necessary)
                 // else as 0s (no simplification, complete interval check needed) 
                 checkingForbiddenTerminals
               );
            Debug.Assert(!xxxresult || (xxxindex + 1 == blockList.Count));
            return xxxresult;
         }

         // TOCHECK warning message if gaps between terminals symbols and not GlobalSettings.InputElementsAreTerminals.Value

         if (blockIndex == 1 && checkingForbiddenTerminals && !GlobalSettings.InputPeekChecksBounds.Value)
         {
            // Simulate block -1 and generate test of preceding values  "Symbol < 1st terminal symbol"
            TerminalSymbol firstTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(0);
            codegen.AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " < ")
               .Append(firstTerminalSymbol.NameToGenerate);
            is1stCondition = false;
         }
         else // blockIndex == 0 !
         if (IsSpecial1stBlockOf1s())
         {
            // generate "symbol <= EndOfBlock"
            if (block.BlockEnd == lastRelevant)
            {
               // Only 1 block; should not occur here, because blockList.Count<=1 has been excluded in If(...
               // so that conditions generated as comment are not only the text "true"
               codegen.AppendWithOptionalLinebreak("true");
               return;
            }

            TerminalSymbol endTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.BlockEnd);
            codegen.AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " <= ")
               .Append(endTerminalSymbol.NameToGenerate);

            is1stCondition = false;
            blockIndex += 2; // special case has been handled here, start the loop below with next block of same type 
         }

         // handle all (remaining) blocks with type==true
         for (; blockIndex < blockList.Count; blockIndex += 2)
         {
            // all blocks of blockType==false are skipped by blockIndex += 2
            block = blockList[blockIndex];
            Debug.Assert(block.BlockType);

            TerminalSymbol startTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.BlockStart);
            TerminalSymbol endTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.BlockEnd);

            if (!is1stCondition)
            {
               // concatenate code for not the first block etc. with " || " 
               codegen.AppendLineAndIndent();
               codegen.Append("|| ");
            }
            is1stCondition = false;

            if (IsSpecialLastBlockOf1s(blockIndex))
            {
               // generate "Symbol >= StartOfBlock"
               codegen.AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " >= ")
                  .Append(startTerminalSymbol.NameToGenerate);
            }
            else
               switch (block.RelevantBitsCount)
               {
                  case 1: // generate: check of equality of one allowed symbol
                     {
                        codegen.AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " == ").
                           Append(startTerminalSymbol.NameToGenerate);
                        break;
                     }
                  case 2: // generate: check of two allowed symbols
                     {
                        // compare two symbols: same complexity as test of interval but better readability
                        // no parantheses necessary
                        codegen.AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " == ")
                           .Append(startTerminalSymbol.NameToGenerate)
                           .Append(" || ")
                           .AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " == ")
                           .Append(endTerminalSymbol.NameToGenerate);
                        break;
                     }

                  default:// generate: check a sequence of three or more allowed symbols
                     {
                        // generate: test closed interval of three or more symbols
                        if (useParentheses)
                           codegen.Append('(');
                        codegen.AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " >= ")
                           .Append(startTerminalSymbol.NameToGenerate)
                           .Append(" && ")
                           .AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " <= ")
                           .Append(endTerminalSymbol.NameToGenerate);
                        if (useParentheses)
                           codegen.Append(')');
                        break;
                     }
               }
         }

         if (!blockList[^1].BlockType && checkingForbiddenTerminals && !GlobalSettings.InputPeekChecksBounds.Value)
         {
            // Append condition to check additional values 
            Debug.Assert(blockIndex == blockList.Count);

            if (!is1stCondition)
            {
               // concatenate code for not the first block etc. with " || " 
               codegen.AppendLineAndIndent();
               codegen.Append("|| ");
            }
            codegen.AppendWithOptionalLinebreak(GlobalSettings.InputExpression.Value, " > ")
               .Append(GlobalVariables.GetTerminalSymbolByIndex(blockList[^1].BlockEnd).NameToGenerate);
         }
      }

      private static void GenerateIsIn(P5CodegenCS codegen, IndexSet condition, IndexSet relevant, Boolean checkingForbiddenTerminals)
      {
         IndexSet InverseCondition = new IndexSet(condition).Complement().IntersectWith(relevant);
         if (condition.Count < InverseCondition.Count) // TOCHECK 1st and last condition?
            GenerateIsInArguments(codegen, condition, false, relevant, checkingForbiddenTerminals);
         else
            GenerateIsInArguments(codegen, InverseCondition, true, relevant, checkingForbiddenTerminals);
      }

      private static void GenerateIsInArguments(P5CodegenCS codegen, IndexSet condition, Boolean inverse, IndexSet relevant, Boolean checkingForbiddenTerminals)
      {
         codegen.IncrementIndentationLevel();

         if (condition.IsComplete)
         {
            codegen.AppendWithOptionalLinebreak("true")
               .DecrementIndentationLevel();
            return;
         }
         else if (condition.IsEmpty)
         {
            codegen.AppendWithOptionalLinebreak("false")
               .DecrementIndentationLevel();
            return;
         }

         int first = relevant.IndexOfFirstBit(true);
         int last = relevant.IndexOfLastBit(true);

         /* consider GlobalSettings.InputElementsAreTerminals
          * There are 2 cases
          * a) independent of checkingForbiddenTerminals
          * a1) !inverse  =>    IsIn( a | c | e ...) with xxx == 1
          * a2) inverse   =>    !IsIn( b | d ...) with xxx == 0 implemented by inverting the bits
          *
          * consider !GlobalSettings.InputElementsAreTerminals: there are extra values
          * There are 4 cases
          * a) standard "if (...) accept..." !checkingForbiddenTerminals (extra values return false)
          * a1) !inverse  =>    IsIn( a | c | e ...)
          * a2) inverse   =>    symbol >= first && symbol <= last && !IsIn( b | d ...) 
          * b) special "if (...) error.." checkingForbiddenTerminals (extra values return true)
          * b1) !inverse  =>    symbol < first || symbol > last || IsIn( a | c | e ...)
          * b2) inverse  = >    !IsIn( b | d ...) 
          */

         Boolean CheckExtraValues =
            !GlobalSettings.InputPeekChecksBounds.Value // special case if there may be extra values
            && (checkingForbiddenTerminals ^ inverse)       // and if checkingForbiddenTerminals and inverse are different
            ;

         string op2 = ""; // will be finally "" or " || " or " && "

         if (CheckExtraValues) // cases a2) and b1)
         {
            // generate code to check lower and higher values
            string op1, op3;

            if (checkingForbiddenTerminals) // && ! inverse : case b1)
            {  // include lower and higher values: "symbol < first || Symbol > last || (!)IsIn") 
               op1 = " < ";
               op2 = " || ";
               op3 = " > ";

               // optimize: include test of first and/or last sequence if condition[first / last]
               //   "symbol < first || Symbol > last || IsIn(first|...|last")
               //        ==>  "symbol <= first+x || Symbol >= last-y || IsIn(first+x+1...last-y-1")
               if (condition.Get(first))
               {
                  while (first < last && (condition.Get(first + 1) || !relevant.Get(first + 1)))
                     first++;
                  op1 = " <= ";
               }
               if (condition.Get(last))
               {
                  while (last > first && (condition.Get(last - 1) || !relevant.Get(last - 1)))
                     last--; // Note:  first==last is possible
                  op3 = " >= ";
               }
            }
            else // !checkingForbiddenTerminals && inverse: case a2)
            {  // exclude lower and higher values: "symbol >= first && Symbol <= last && (!)IsIn") 
               op1 = " >= ";
               op2 = " && ";
               op3 = " <= ";
               // optimize: include test of first and/or last if condition[] is true
               //   "symbol >= first && Symbol <= last && IsIn(first|...|last")
               //        ==>  "symbol > first && Symbol < last && IsIn(...")
               if (condition.Get(first))
               {
                  while (first < last && condition.Get(first + 1))
                     first++;
                  op1 = " > ";
               }
               if (condition.Get(last))
               {
                  while (first < last && condition.Get(last - 1))
                     last--;
                  op3 = " < "; // Note:  first==last is possible
               }
            }

            codegen
               .Append(GlobalSettings.InputExpression.Value)
               .Append(op1)
               .Append(GlobalVariables.GetTerminalSymbolByIndex(first).NameToGenerate)
               .Append(op2)
               .Append((GlobalSettings.InputExpression.Value))
               .Append(op3)
               .Append(GlobalVariables.GetTerminalSymbolByIndex(last).NameToGenerate);
            // op2 will be appended if at least one Flag is tested
            first++;
            last--;
         }

         Boolean Is1stTrueFlag = true;
         // foreach terminal symbol (maybe except [0] and [^1], maybe empty depending on above code generation):
         for (Int32 i = condition.IndexOfNextBit(first - 1); i <= last; i = condition.IndexOfNextBit(i))
         {
            if (!relevant.Get(i))
               continue;

            TerminalSymbol t = GlobalVariables.GetTerminalSymbolByIndex(i);

            // the int constant "_(identifier of terminal symbol)" has to be declared (if not terminals are flags)
            t.IsUsedInIsIn = true;

            if (Is1stTrueFlag)
            {
               GenerateFlagTestPart1(codegen, inverse, op2);
            }
            else
               codegen.Append(' ').AppendOptionalLinebreak().Append("| ");

            Is1stTrueFlag = false;

            if (GlobalVariables.TerminalSymbolsAreFlags)
               codegen.Append(t.NameToGenerate);
            else
               codegen
                  .Append(GlobalSettings.PrefixOfFlagConstants.Value)
                  .Append(t.FlagName);

            codegen.AppendOptionalLinebreak();
         }

         codegen.DecrementIndentationLevel();

         if (!Is1stTrueFlag)
            GenerateFlagTestPart2(codegen);
      }

      private static void GenerateFlagTestPart1(P5CodegenCS codegen, Boolean inverse, String op2)
      {
         codegen.Append(op2); // "" or " || " or " && "

         if (GlobalVariables.TerminalSymbolsAreFlags)
         {
            if (inverse)
               codegen.Append("0 == (");
            else
               codegen.Append("0 != (");
            codegen.Append(GlobalSettings.InputExpression.Value)
               .Append(" & (");
         }
         else
         {
            // generate optional "!" and then "_is("
            if (inverse)
               codegen.Append("!");
            codegen
               .Append(GlobalSettings.NameOfFlagTestMethod.Value)
               .Append('(');
         }
      }

      private static void GenerateFlagTestPart2(P5CodegenCS codegen)
      {
         if (GlobalVariables.TerminalSymbolsAreFlags)
            codegen.Append("))");
         else
            codegen.Append(")");
      }
   }

}
