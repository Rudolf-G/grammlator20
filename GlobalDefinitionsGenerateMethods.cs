using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;


namespace grammlator {
   internal abstract partial class ParserAction {

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

   internal sealed partial class BranchAction : ParserAction {

      public struct ActionAndCounter {
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
      public class ActionCounterList : List<ActionAndCounter> {
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
            .Append(GlobalVariables.StateStack.Value)
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
                        .Append(f.BranchcaseCondition.ToString())
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
               codegen.AppendWithOptionalLinebreak("case ", f.BranchcaseCondition.ToString(), ": ");
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

   internal sealed partial class ReduceAction : ParserActionWithNextAction {
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         // Generate description
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

         // Generate instructions to handle the state stack
         if (this.StateStackAdjustment != 0)
         {
            if (this.StateStackAdjustment == 1)
            {
               codegen.IndentAndAppend(GlobalVariables.StateStack.Value)
                  .AppendLine(".Pop(); ");
            }
            else
            {
               codegen.IndentAndAppend(GlobalVariables.StateStack.Value)
                  .Append(".Discard(")
                  .Append(this.StateStackAdjustment)
                  .Append("); ");
            }
         }

         // Generate instructions to handle the attribute stack and to call the method

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

         // Gegebenenfalls die Zeile beenden
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

   internal sealed partial class PrioritySelectAction : ConditionalAction  {
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         accept = false;
         return NextAction;
      }
   }

   internal sealed partial class PriorityBranchAction : ParserAction {
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

         codegen.AppendInstruction($"switch({GlobalVariables.MethodIndexOfMaximum}(");

         // IndexOfMaximum has to return the index of the 1st occurence of the greatest argument. Then:
         // TerminalTransition (priority 0) has priority over dynamic priority value 0 because it is generated 1st !! 
         if (ConstantPriorityAction != null)
            codegen.Append(ConstantPriority).Append(", ");

         codegen.IncrementIndentationLevel();

         for (Int32 i = 0; i < DynamicPriorityActions.Count; i++)
         {
            codegen.IndentExactly()
               .AppendLine() // empty line preceding method call
               .GenerateSemanticMethodCall(PriorityFunctions[i]);
            if (i < DynamicPriorityActions.Count - 1)
               codegen.Append(",");
            codegen.AppendLine(); // end of line

            if (i == DynamicPriorityActions.Count - 1)
               codegen.AppendLine(); // empty line following method call
         }
         codegen.Indent().Append("))");

         codegen.DecrementIndentationLevel();
         codegen.IndentExactly().Append("{");
         // end of generating switch condition


         // generate switch cases
         Int32 CaseCount = 0;
         if (ConstantPriorityAction != null)
         {
            ConditionalAction ca = (ConditionalAction)(ConstantPriorityAction);

            codegen.IndentExactly().Append("case ").Append(CaseCount++).Append(": ");
            codegen.IncrementIndentationLevel();
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

   internal sealed partial class ErrorhandlingAction : ConditionalAction {
      /// <summary>
      /// Generates the assignment to StateNumber and the call of ErrorHandler dependent on the global variables or
      /// if there is no error handling method defined, returns only the next action (error halt)
      /// </summary>
      /// <param name="accept"></param>
      /// <returns>next action: <see cref="ErrorHaltAction>"/> </returns>
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         if (!String.IsNullOrEmpty(GlobalVariables.ErrorHandlerMethod.Value))
         {
            // generate ErrorHandlerCall   ErrorHandler(ErrorStateNumber, StateDescription, ParserInput);
            codegen
               .Indent()
               .Append("if (")
               .Append(GlobalVariables.ErrorHandlerMethod.Value)
               .Append('(')
               .Append(this.IdNumber + 1)
               .Append(", ");
            if (String.IsNullOrEmpty(GlobalVariables.StateDescriptionPrefix.Value))
               codegen.Append("\"\""); // empty error description
            else
            {
               codegen
               .Append(GlobalVariables.StateDescriptionPrefix.Value)
               .Append(this.State.IdNumber + 1);
            }
            codegen
               .Append(", ")
               .Append(GlobalVariables.SymbolNameOrFunctionCall.Value)
               .AppendLine("))");

            if (this.State.StateStackNumber >= 0)
            { // generate {_s.POP(..); goto state...;}
               codegen
                  .IncrementIndentationLevel()
                  .Indent()
                  .AppendLine("{")
                  .IndentAndAppend(GlobalVariables.StateStack.Value)
                  .AppendLine(".Pop(); ")
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

   internal sealed partial class HaltAction : ParserActionWithNextAction {
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         codegen.IndentExactly();
         Int32 numberOfAttributesToStore = this.AttributestackAdjustment;
         Debug.Assert(numberOfAttributesToStore >= 0);

         codegen.IndentAndAppendLine("// Halt: a definition of the startsymbol with " + numberOfAttributesToStore.ToString() + " attributes has been recognized.");

         if (GlobalVariables.ListOfAllStates[0].StateStackNumber >= 0)
            codegen.IndentExactly()
               .Append(GlobalVariables.StateStack.Value)
               .Append(".Pop();");

         if (numberOfAttributesToStore != 0)
         {
            codegen.Append("AttributesOfSymbol.CopyAndRemoveFrom(")
               .Append(GlobalVariables.AttributeStack.Value)
               .Append(", ")
               .Append(numberOfAttributesToStore)
               .AppendLine(");");
         }

         accept = false;
         return this.NextAction;
      }
   }

   internal sealed partial class ErrorHaltAction : ParserActionWithNextAction {
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         codegen.IndentAndAppendLine(
           "// This point is reached after an input error has been found");

         // generate _s.Pop(x)
         if (GlobalVariables.CountOfStatesWithStateStackNumber > 0)
            codegen.IndentExactly()
               .Append(GlobalVariables.StateStack.Value)
               .Append(".Discard(")
               .Append(GlobalVariables.StateStack.Value)
               .Append(".Count - ")
               .Append(GlobalVariables.StateStackInitialCountVariable.Value)
               .AppendLine(");");
         // codegen.IndentAndAppendLine("_s.Discard(_s.Count - StateStackInitialCount);");  // TODO als Ressource definieren

         // generate _a.Remove(x)
         if (GlobalVariables.reductionsModifyAttributStack)
            codegen.IndentExactly()
               .Append(GlobalVariables.AttributeStack.Value)
               .Append(".Remove(")
               .Append(GlobalVariables.AttributeStack.Value)
               .Append(".Count - ")
               .Append(GlobalVariables.AttributeStackInitialCountVariable.Value)
               .AppendLine(");");

         // generate additional instruction
         if (!String.IsNullOrEmpty(GlobalVariables.ErrorHaltInstruction.Value))
            codegen.IndentAndAppendLine(GlobalVariables.ErrorHaltInstruction.Value);

         accept = false;
         return GlobalVariables.TheOnlyOneErrorHaltAction.NextAction;
      }
   }

   internal sealed partial class EndOfGeneratedCodeAction : ParserAction {
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         accept = false;
         return codegen.GenerateEndOfCodeAction();
      }
   }

   internal sealed partial class PushStateAction : ParserActionWithNextAction {
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         accept = false;
         codegen.GenerateStateStackPushWithOptionalLinebreak(this.StateStackNumber);
         return this.NextAction;
      }
   }

   internal sealed partial class ParserState : ParserAction, IELementOfPartition {
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
            && !String.IsNullOrEmpty(GlobalVariables.StateDescriptionPrefix.Value)
            && !String.IsNullOrEmpty(GlobalVariables.ErrorHandlerMethod.Value)
            )
         {
            // Generate assignment to VariableNameStateDescription (if defined)
            codegen.Indent();
            codegen.Append("const String ");
            codegen.Append(GlobalVariables.StateDescriptionPrefix.Value);
            codegen.Append(IdNumber + 1);
            codegen.Append(" =");
            codegen.AppendLine();

            // Generate the item descriptions to be assigned to the variable
            sbTemp.Replace("\\", "\\\\").Replace("\"", "\\\""); // escape the symbols which are not allowed in strings

            codegen.IndentAndAppendLinesWithSeparator(
                linesToAppend: sbTemp.ToString() // the strings describing the items of the state
                , stringPrecedingFirstLine: "     \""  // indentation and character """" in front of each string
                , separatorAtEndofLine: GlobalVariables.NewLineWithEscapes + "\""  // at end of each string except the last
                , separatorAtNewLine: "   + \"" // before each string except the first
                , stringAtEndOfLastLine: "\";" // after the last string
                );
            codegen.AppendLine();
         }
         else
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

         // generate push to the state stack if necessary, generate comment im moved to actions
         if (StateStackNumber >= 0)
            codegen.GenerateStateStackPushWithOptionalLinebreak(StateStackNumber);
         else if (StateStackNumber <= -2)
            codegen.AppendLine("// *Push(" + (-StateStackNumber - 2).ToString() + ')');


         // The call of "FetchSymbol();" must be generated only if the state contains actions, which check the input symbol.
         // This is prepared  by shortening chains in phase 4 und implemented by the following.

         if (!(PossibleInputTerminals!.IsEqualTo(GlobalVariables.AllTerminalSymbols)))
         {
            // there has been look ahead: no InstructionAssignSymbol needed
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
                     .AppendWithOptionalLinebreak(GlobalVariables.SymbolAssignInstruction.Value);
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
                MessageTypeOrDestinationEnum.Error, "Check your grammar: no actions in state " + (IdNumber + 1).ToString());
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

         if (IfComplexity > GlobalVariables.IfToSwitchBorder.Value)
            return GenerateSwitchWithActionsOfState(codegen, out accept);

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
            ParserAction temp = Actions[^2];
            Actions[^2] = Actions[^1];
            Actions[^1] = temp;
         }

         return GenerateConditionalActionsOfState(codegen, out accept);
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
         var relevantSymbols = new BitArray(PossibleInputTerminals!);

         for (Int32 i = 0; i < Actions.Count - 1; i++)
         {
            var a = (ConditionalAction)Actions[i];
            GenerateOneConditionalAction(codegen, a, relevantSymbols); // Modifies relevantSymbols
         }

         var LastAction = (ConditionalAction)Actions[^1];

         ParserAction? nextAction = LastAction; //CHECK may nextAction be null?
         if (LastAction is TerminalTransition || LastAction is LookaheadAction)
            nextAction = LastAction.NextAction; // TOCHECK must nextAction.Calls be reduced by 1 ??

         BitArray suppressedCondition = LastAction.TerminalSymbols;
         if (nextAction != null && suppressedCondition != null
            && !suppressedCondition.All())
         {
            GenerateConditionAsComment(codegen, suppressedCondition);
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
         codegen.Append(GlobalVariables.SymbolNameOrFunctionCall.Value);
         codegen.Append(")");
         codegen.GenerateBeginOfBlock();

         Int32 IndexOfLastUsedTerminal = PossibleInputTerminals!.IndexOfLastTrueElement();
         Int32 IndexOfFirstUsedTerminal = PossibleInputTerminals!.IndexOfFirstTrueElement();

         Int32 LeadingCount = 0, TrailingCount = 0;
         ConditionalAction? LeadingAction = null, TrailingAction = null;

         for (Int32 i = 0; i < Actions.Count; i++)
         {
            var ThisAction = (ConditionalAction)Actions[i];
            BitArray Terminals = ThisAction.TerminalSymbols;
            Int32 TerminalIndex = Terminals.FindNextTrue(-1);
            Boolean IsDefaultAction = false;

            // Special case if Terminals contains the first used terminal
            if (TerminalIndex == IndexOfFirstUsedTerminal)
            {
               // remember this action and the count of leading terminals
               Debug.Assert(LeadingCount == 0, "Phase5: LeadingCount already != 0");
               LeadingAction = ThisAction;
               LeadingCount = Terminals.FindNextFalse(IndexOfFirstUsedTerminal) - IndexOfFirstUsedTerminal;
               TerminalIndex = Terminals.FindNextTrue(IndexOfFirstUsedTerminal + LeadingCount);
               IsDefaultAction = true;

               // generate first part of comment
               codegen.IndentExactly();
               codegen.Append("// <= ");
               codegen.AppendWithPrefix(
                   GlobalVariables.TerminalSymbolEnum.Value,
                   GlobalVariables.TerminalSymbols[LeadingCount - 1].Identifier
                   );
            }

            // Special case if Terminals contains the last used terminal
            if (Terminals[IndexOfLastUsedTerminal])
            {
               // remember this action and the count of trailing terminals
               Debug.Assert(TrailingCount == 0, "Phase5: TrailingCount already != 0");
               TrailingAction = ThisAction;
               TrailingCount = IndexOfLastUsedTerminal - Terminals.FindPrecedingFalse(IndexOfLastUsedTerminal);
               IsDefaultAction = true;

               // generate first part of comment
               codegen.IndentExactly();
               codegen.Append("// >= ");
               codegen.AppendWithPrefix(
                   GlobalVariables.TerminalSymbolEnum.Value,
                   GlobalVariables.TerminalSymbols[IndexOfLastUsedTerminal - TrailingCount + 1].Identifier
                   );
            }

            if (IsDefaultAction)
            {
               // generate second part of comment
               codegen.Append(": ");
               codegen.Append("goto ");
               codegen.Append(P5CodegenCS.GotoLabel(ThisAction));
               codegen.Append(" // see end of switch");
            }

            // if remaining terminal symbols, for each 
            Boolean RemainingSymbols = false;
            while (TerminalIndex < IndexOfLastUsedTerminal - TrailingCount + 1)
            {
               RemainingSymbols = true;
               // generate "case TerminalSymbol: " 
               codegen.IndentExactly();
               codegen.Append("case ");
               codegen.AppendWithPrefix(
                   GlobalVariables.TerminalSymbolEnum.Value,
                   GlobalVariables.TerminalSymbols[TerminalIndex].Identifier
                   );
               codegen.Append(":");

               TerminalIndex = Terminals.FindNextTrue(TerminalIndex);
            }

            if (RemainingSymbols)
            {
               codegen.IncrementIndentationLevel();

               if (IsDefaultAction)
               {
                  // adjust the statistics because here a "goto label" will be generated
                  // in addition to the code generated in the default action.
                  // If the statistics are not adjusted those code may be generated without label.
                  if (ThisAction is TerminalTransition)
                     ThisAction.NextAction.AcceptCalls++;
                  else if (ThisAction is ErrorhandlingAction)
                     ThisAction.Calls++;
                  else
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
            codegen.Append(GlobalVariables.SymbolNameOrFunctionCall.Value);

            // prefer action which generates goto as first action
            // TODO else prefer action which does not generate a label
            if (P5GenerateCode.GeneratesGoto(generateAccept: Action2 is TerminalTransition, Action2Generate, codegen.IndentationLevel) &&
                !P5GenerateCode.GeneratesGoto(generateAccept: Action1 is TerminalTransition, Action1Generate, codegen.IndentationLevel))
            {
               static void Swap<T>(ref T a, ref T b)
               {
                  T temp = a;
                  a = b;
                  b = temp;
               }

               Swap(ref Action1, ref Action2);
               Swap(ref Action1Generate, ref Action2Generate);
               ActionsSwapped = true;

               codegen.Append(" >= ")
               .AppendWithPrefix(
                   GlobalVariables.TerminalSymbolEnum.Value,
                   GlobalVariables.TerminalSymbols[IndexOfLastUsedTerminal - TrailingCount + 1].Identifier
                   );
            }
            else
            {
               codegen.Append(" <= ");
               codegen.AppendWithPrefix(
                   GlobalVariables.TerminalSymbolEnum.Value,
                   GlobalVariables.TerminalSymbols[LeadingCount - 1].Identifier
                   );
            }

            codegen.Append(") ");

            codegen.IncrementIndentationLevel();
            P5GenerateCode.GenerateCodeSequence(codegen, Action1, labelMustBeGenerated: false);
            codegen.DecrementIndentationLevel();
         }

         // generate comment as Debug.Assert
         codegen.IndentExactly();
         codegen.Append("Debug.Assert(");
         if (Action1Generate == Action2Generate)
         {
            codegen.Append(GlobalVariables.SymbolNameOrFunctionCall.Value);
            codegen.Append(" <= ");
            codegen.AppendWithPrefix(
                GlobalVariables.TerminalSymbolEnum.Value,
                GlobalVariables.TerminalSymbols[LeadingCount - 1].Identifier
                );
            codegen.Append(" || ");
            codegen.Append(GlobalVariables.SymbolNameOrFunctionCall.Value);
            codegen.Append(" >= ");
            codegen.AppendWithPrefix(
                GlobalVariables.TerminalSymbolEnum.Value,
                GlobalVariables.TerminalSymbols[IndexOfLastUsedTerminal - TrailingCount + 1].Identifier
                );
         }
         else if (ActionsSwapped)
         {
            codegen.Append(GlobalVariables.SymbolNameOrFunctionCall.Value);
            codegen.Append(" <= ");
            codegen.AppendWithPrefix(
                GlobalVariables.TerminalSymbolEnum.Value,
                GlobalVariables.TerminalSymbols[LeadingCount - 1].Identifier
                );
         }
         else
         {
            codegen.Append(GlobalVariables.SymbolNameOrFunctionCall.Value);
            codegen.Append(" >= ");
            codegen.AppendWithPrefix(
                GlobalVariables.TerminalSymbolEnum.Value,
                GlobalVariables.TerminalSymbols[IndexOfLastUsedTerminal - TrailingCount + 1].Identifier
                );
         }
         codegen.AppendLine(");");
         codegen.AppendLine("");

         // generate Action2
         accept = Action2 is TerminalTransition; // TOCHECK try to avoid the out parameter "accept"?
         return Action2Generate;
      }

      private static void GenerateOneConditionalAction(P5CodegenCS codegen, ConditionalAction a, BitArray relevantSymbols)
      {
         codegen.IndentExactly();
         codegen.AppendWithOptionalLinebreak("if (");

         codegen.IncrementIndentationLevel();
         // codegen.Indent(nestingLevel + 1);
         GenerateCondition(codegen, a.TerminalSymbols, relevantSymbols);
         codegen.AppendWithOptionalLinebreak(") ");

         P5GenerateCode.GenerateCodeSequence(codegen, a, labelMustBeGenerated: false);

         // All elements (value==true) in conditions of remaining actions remain relevant.
         // Only bits representing not allowed elements may become irrelevant.
         relevantSymbols.ExceptWith(a.TerminalSymbols);

         codegen.DecrementIndentationLevel();
      }

      internal struct BlockOfEqualBits {
         /// <summary>
         /// false if the relevant bits of the block are false, true if the relevant bits are true
         /// </summary>
         internal Boolean blockType;

         /// <summary>
         /// blockStart is the index of the first bit of the block
         /// </summary>
         internal Int32 blockStart;

         /// <summary>
         /// blockLength is the number of relevant bits
         /// </summary>
         internal Int32 blockLength;

         /// <summary>
         /// blockEnd is the indexc of the last bit of the block
         /// </summary>
         internal Int32 blockEnd;
      }

      private static void GenerateCondition(P5CodegenCS codegen, BitArray Condition, BitArray Relevant)
      {
         // In the worst case all symbols are relevant and the condition changes at each index
         // so that each symbol needs an own block

         // Reuse the blockList            blockList.Clear();
         /// <summary>
         /// list of blocks of symbols inside TerminalSymbols to be reused in <see cref="P5GenerateCode"/>
         /// </summary>
         List<BlockOfEqualBits> blockList = new List<BlockOfEqualBits>(GlobalVariables.NumberOfTerminalSymbols);

         if (blockList.Capacity != Condition.Count)
            blockList.Capacity = Condition.Count;

         // determine consecutive blocks (indexes) of equal values of Condition ignoring not relevant symbols (indexes)
         ComputeBlocklist(Condition, Relevant, blockList);

         Boolean test1sRecommended = AnalyseBlockList(blockList, out Int32 Complexity);

         if (Complexity > GlobalVariables.CompareToFlagTestBorder.Value * 100 - 50
            // 350 allows e.g. 2 comparisions and 1 logical operator
            && GlobalVariables.IsInMethod.Value != ""
            && GlobalVariables.NumberOfTerminalSymbols <= 63
            // TODO correct condition: the value of each enum element must be >= 0  and  <=63
            )
         {
            GenerateIsIn(codegen, Condition, Relevant);

            // The generated code contains 1 "<<"-operator, 1 "&"-bitoperator, 1 "==0" comparision, optional a "!"-operator,
            // 1 method call (may be inlined by JIT-Compiler), some additions of constants which are evaluated at compile time
         }
         else if (test1sRecommended)
         {
            GenerateTest1s(codegen, blockList);
         }
         else
         {
            GenerateTest0s(codegen, blockList);
         }

         blockList.Clear(); // keep capacity for reuse
         return;
      }

      private static void GenerateConditionAsComment(P5CodegenCS codegen, BitArray condition)
      {
         codegen.IndentExactly();
         codegen.AppendWithOptionalLinebreak("Debug.Assert(");
         codegen.IncrementIndentationLevel();
         // codegen.Indent(nestingLevel + 1);
         // force complete check, e.g. /* MyCharacterInput.Symbol >= CharGroupEnum.Digit */
         /* MyCharacterInput.Symbol == CharGroupEnum.Digit || MyCharacterInput.Symbol == CharGroupEnum.Letter */
         GenerateCondition(codegen, condition, GlobalVariables.AllTerminalSymbols);

         // The restriction on relevant symbols would produce a optimized condition resulting on preceding checks, e.g.
         /* MyCharacterInput.Symbol >= CharGroupEnum.Digit */
         // GenerateCondition(Condition, RelevantSymbols); 

         codegen.AppendWithOptionalLinebreak(");");
         codegen.DecrementIndentationLevel();
      }


      /// <summary>
      /// Creates a list of blocks each of which desribes a sequence of contiguous 0s resp. 1s of the given condition.
      /// Bits which are not relevant are interpreted as 0s or as 1s arbitrarily to get large blocks.
      /// </summary>
      /// <param name="Condition">A <see cref="BitArray"/> with the terminal symbols to be checked</param>
      /// <param name="Relevant">A <see cref="BitArray"/> with the terminal symbols which are not yet checked</param>
      /// <param name="BlockList">The blocklist to be filled with information</param>
      private static void ComputeBlocklist(
          BitArray Condition,
          BitArray Relevant,
          List<BlockOfEqualBits> BlockList)
      {
         (Int32 firstRelevant, Int32 lastRelevant) = Relevant.IndexOfFirstAndLastTrueElement();

         if (firstRelevant == -1)
         {
            return; // no relevant elements, blocklist remains empty
         }

         Boolean blockType;
         Int32 blockStart;
         Int32 blockLength; // counts the relevant bits
         Int32 blockEnd;

         Int32 nextRelevant = firstRelevant; // first relevant

         while (true)
         { // loop over all blocks
            Debug.Assert(nextRelevant <= lastRelevant);

            blockStart = nextRelevant;
            blockType = Condition[blockStart];
            blockLength = 1;
            blockEnd = blockStart;

            // increment blockEnd and blockLength until end of block or end of relevant part of condition
            while (++nextRelevant <= lastRelevant)
            {
               // ignore all elements which are not relevant
               nextRelevant = Relevant.FindNextTrue(nextRelevant - 1);
               // Note: Relevant.FindNextTrue(LastRelevant) == LastRelevant

               Debug.Assert(nextRelevant <= lastRelevant && Relevant[nextRelevant]);

               // beyond end of block?
               if (Condition[nextRelevant] != blockType)
                  break; // yes

               // assume this is the last relevant bit 
               blockEnd = nextRelevant;
               // count the relevant bits
               blockLength++;
            }

            // found a block
            BlockList.Add(new BlockOfEqualBits {
               blockType = blockType,
               blockStart = blockStart,
               blockLength = blockLength,
               blockEnd = blockEnd
            });

            if (nextRelevant > lastRelevant)
               break; // no more blocks

            Debug.Assert(nextRelevant <= lastRelevant && Relevant[nextRelevant] && Condition[nextRelevant] != blockType); // = blockStart of next block
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

         Int32 first = BlockList[0].blockStart;
         Int32 last = BlockList[^1].blockEnd;

         BlockOfEqualBits block;

         // default: start the loop below with the first block
         Int32 blockIndex = 0;

         //  1st block may need special handling
         block = BlockList[0];

         if (blockIndex == 0 && block.blockLength >= 2)
         {
            // special handlings of first block if it is type true and contains more than 1 element
            Debug.Assert(blockIndex == 0 && block.blockStart == first);

            if (block.blockEnd == last)
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
            Complexity[block.blockType ? 1 : 0] += 100;

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
               Complexity[block.blockType ? 1 : 0] += 100;
            }

            switch (block.blockLength)
            {
            // prefer == and != (by using complexity 99) to <=, <, > and >= (with complexity 100) 
            // The codegen examples are taken from GenerateTest1s.
            // The results are also applicable for GenerateTest0s where only the comparision operators differ.
            case 1: // generate: check of equality of one allowed symbol
            {
               ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "Symbol == ");
               ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
               Complexity[block.blockType ? 1 : 0] += 99; // == 
               break;
            }

            case 2: // generate: check of two allowed symbols or special case
            {
               if (block.blockEnd == last)
               {
                  // special case: at the end of relevant symbols test of end may be ommitted
                  ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "Symbol >= ");
                  ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
                  Complexity[block.blockType ? 1 : 0] += 100; // >=
               }
               else
               { // compare two symbols: same complexity as test of interval but better readability
                 ////codegen.AppendWithOptionalLinebreakAndPrefix(endTerminalSymbol.InputClass, "Symbol == ");
                 ////codegen.AppendWithPrefix(endTerminalSymbol.SymbolEnum, endTerminalSymbol.Bezeichner);
                 ////codegen.Append(" || ");
                 ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "Symbol == ");
                 ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
                  Complexity[block.blockType ? 1 : 0] += 100 + 99 + 99; // == || ==
               }
               break;
            }

            default:// generate: check a sequence of three or more allowed symbols
            {
               if (block.blockEnd == last)
               {
                  // special case: at end the of relevant symbols test of end may be ommitted
                  ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "Symbol >= ");
                  ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
                  Complexity[block.blockType ? 1 : 0] += 100; // >=
               }
               else
               { // generate: test closed interval of three or more symbols
                 ////codegen.AppendWithOptionalLinebreakAndPrefix(startTerminalSymbol.InputClass, "(Symbol >= ");
                 ////codegen.AppendWithPrefix(startTerminalSymbol.SymbolEnum, startTerminalSymbol.Bezeichner);
                 ////codegen.Append(" && ");
                 ////codegen.AppendWithOptionalLinebreakAndPrefix(endTerminalSymbol.InputClass, "Symbol <= ");
                 ////codegen.AppendWithPrefix(endTerminalSymbol.SymbolEnum, endTerminalSymbol.Bezeichner);
                  Complexity[block.blockType ? 1 : 0] += 300; // >= && <=
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
      private static void GenerateTest0s(P5CodegenCS codegen, List<BlockOfEqualBits> blockList)
      {
         /* Code is generated for all blocks  of type == false, typically  "(symbol < 'StartIndexOfBlock' || symbol > EndIndexOfBlock) && ..."  
          * Code for different blocks is concatenated by " && "
          * special cases:
          *    blockstart == blockend:  "Symbol != StartOfBlock"
          *    blockstart == 1st relevant elements index: "symbol > EndOfBlock"
          *    blockend   == last relevant elements index: "Symbol < StartOfBlock"
          *    blockLength==2: "Symbol != StartOfBlock && Symbol != EndOfBlock"
          *    blocklength >2: "(Symbol < StartOfBlock || Symbol > EndOfBlock)" with paranthesis

          *  some more special cases see below
          */

         if (blockList.Count == 0)
         {
            // no relevant symbol
            codegen.AppendWithOptionalLinebreak("true"); // "false" would also be ok
            return;
         }

         Int32 first = blockList[0].blockStart;
         Int32 last = blockList[^1].blockEnd;

         BlockOfEqualBits block;

         // default: start the loop below with the first block of type==false
         Int32 blockIndex = blockList[0].blockType ? 1 : 0;
         // Enclose condition in parentheses if there is one more block of the same type
         Boolean useParentheses = blockList.Count > blockIndex + 2; // example: Count==3, blockIndex=1 => no parentheses

         // 1st block may need special handling
         block = blockList[0];

         if (blockIndex == 0) // && block.blockLength >= 2 && blockList.Count > 1)
         {
            // special handlings of first block if its type is false and contains more than 1 element
            Debug.Assert(blockIndex == 0 && block.blockStart == first);

            if (block.blockEnd == last)
            {
               // special case: all elements are false, condition returns false
               // This should not happen if conflicts are solved properly.
               // This will have the effect that parts of the generated code can never be reached
               codegen.AppendWithOptionalLinebreak("false");

               return;
            }

            // special case: at beginning of relevant symbols test of start may be ommitted
            TerminalSymbol endTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.blockEnd);
            codegen.AppendWithOptionalLinebreak(GlobalVariables.SymbolNameOrFunctionCall.Value, " > ");
            codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, endTerminalSymbol.Identifier);

            blockIndex += 2; // special case has been handled here, start the loop below with next block of same type 
         }

         // handle all (remaining) blocks with type==true
         for (; blockIndex < blockList.Count; blockIndex += 2)
         {
            // by increment 2 all blocks of blockType==false are skipped
            block = blockList[blockIndex];
            Debug.Assert(!block.blockType);

            TerminalSymbol startTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.blockStart);
            TerminalSymbol endTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.blockEnd);

            if (blockIndex >= 2)
            {
               // concatenate code for not the first block etc. with " && " 
               codegen.AppendLineAndIndent();
               codegen.Append("&& ");
            }

            switch (block.blockLength)
            {
            case 1: // generate: check of equality of one allowed symbol
            {
               if (block.blockEnd == last)
                  goto default;
               codegen.AppendWithOptionalLinebreak(GlobalVariables.SymbolNameOrFunctionCall.Value, " != ");
               codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, startTerminalSymbol.Identifier);
               break;
            }
            case 2: // generate: check of two allowed symbols or special case
            {
               if (block.blockEnd == last)
                  goto default;
               // compare two symbols: same complexity as test of interval but better readability
               // no parantheses necessary
               codegen.AppendWithOptionalLinebreak(GlobalVariables.SymbolNameOrFunctionCall.Value, " != ");
               codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, startTerminalSymbol.Identifier);
               codegen.Append(" && ");
               codegen.AppendWithOptionalLinebreak(GlobalVariables.SymbolNameOrFunctionCall.Value, " != ");
               codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, endTerminalSymbol.Identifier);
               break;
            }
            default:// generate: check a sequence of three or more allowed symbols
            {
               if (block.blockEnd == last)
               {
                  // special case: at end the of relevant symbols test of end must be ommitted
                  // because the last terminal symbol represents al fllowing symbols
                  codegen.AppendWithOptionalLinebreak(GlobalVariables.SymbolNameOrFunctionCall.Value, " < ");
                  codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, startTerminalSymbol.Identifier);
               }
               else
               { // generate: test closed interval of three or more symbols
                  if (useParentheses)
                     codegen.Append('(');
                  codegen.AppendWithOptionalLinebreak(GlobalVariables.SymbolNameOrFunctionCall.Value, " < ");
                  codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, startTerminalSymbol.Identifier);
                  codegen.Append(" || ");
                  codegen.AppendWithOptionalLinebreak(GlobalVariables.SymbolNameOrFunctionCall.Value, " > ");
                  codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, endTerminalSymbol.Identifier);
                  if (useParentheses)
                     codegen.Append(')');
               }
               break;
            }
            }
         }
      }

      /// <summary>
      /// Code is generated for all blocks with BlockType == true
      /// </summary>
      /// <param name="blockList"></param>
      private static void GenerateTest1s(P5CodegenCS codegen, List<BlockOfEqualBits> blockList)
      {
         /* Code is generated for all blocks of type == true, typically  "(symbol >= 'StartOfBlock' && symbol <= 'EndOfBlock") || ..." 
          * Code for different blocks is concatenated by " || "
          * special cases:
          *    blockstart == blockend:  "Symbol == StartOfBlock"
          *    blockstart == 1st relevant elements index: "symbol <= EndOfBlock"
          *    blockend   == last relevant elements index: "Symbol >= StartOfBlock"
          *    blockLength==2: "Symbol == StartOfBlock || Symbol == EndOfBlock"
          *    blocklength >2: "(Symbol >= StartOfBlock && Symbol <= EndOfBlock)" with paranthesis
          *  some more special cases see below
          */

         if (blockList.Count == 0)
         {
            // no relevant symbol, should not occur
            codegen.AppendWithOptionalLinebreak("true"); // "false" would also be ok
            return;
         }

         Int32 first = blockList[0].blockStart;
         Int32 last = blockList[^1].blockEnd;

         /// blockType==0 if the block contains 0s else 1
         BlockOfEqualBits block;

         // default: start the loop below with the first block of type==true
         Int32 blockIndex = blockList[0].blockType ? 0 : 1;
         // Enclose condition in parentheses if there is one more block of the same type
         Boolean useParentheses = blockList.Count > blockIndex + 2; // example: Count==3, blockIndex=1 => no parentheses

         // 1st block may need special handling
         block = blockList[0];
         if (blockIndex == 0) //  && block.blockLength >= 2 && blockList.Count > 1)
         {
            // special handlings of first block if its type is true and contains more than 1 element
            Debug.Assert(blockIndex == 0 && block.blockStart == first);

            if (block.blockEnd == last)
            {
               // Must not occur here, because blockList.Count<=1 has been excluded in If(...
               // so that conditions generated as comment are not only the text "true"
               codegen.AppendWithOptionalLinebreak("true");
               return;
            }

            // special case: at beginning of relevant symbols test of start may be ommitted;
            // resp. must be omitted if the first terminal is intended to include all lower values
            TerminalSymbol endTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.blockEnd);
            codegen.AppendWithOptionalLinebreak(GlobalVariables.SymbolNameOrFunctionCall.Value, " <= ");
            codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, endTerminalSymbol.Identifier);

            blockIndex += 2; // special case has been handled here, start the loop below with next block of same type 
         }

         // handle all (remaining) blocks with type==true
         for (; blockIndex < blockList.Count; blockIndex += 2)
         {
            // by increment 2 all blocks of blockType==false are skipped
            block = blockList[blockIndex];
            Debug.Assert(block.blockType);

            TerminalSymbol startTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.blockStart);
            TerminalSymbol endTerminalSymbol = GlobalVariables.GetTerminalSymbolByIndex(block.blockEnd);

            if (blockIndex >= 2)
            {
               // concatenate code for not the first block etc. with " || " 
               codegen.AppendLineAndIndent();
               codegen.Append("|| ");
            }

            switch (block.blockLength)
            {
            case 1: // generate: check of equality of one allowed symbol (except block with last terminal symbol)
            {
               if (block.blockEnd == last)
                  goto default;
               codegen.AppendWithOptionalLinebreak(GlobalVariables.SymbolNameOrFunctionCall.Value, " == ");
               codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, startTerminalSymbol.Identifier);
               break;
            }
            case 2: // generate: check of two allowed symbols or special case
            {
               if (block.blockEnd == last)
                  goto default;
               // compare two symbols: same complexity as test of interval but better readability
               // no parantheses necessary
               codegen.AppendWithOptionalLinebreak(GlobalVariables.SymbolNameOrFunctionCall.Value, " == ");
               codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, startTerminalSymbol.Identifier);
               codegen.Append(" || ");
               codegen.AppendWithOptionalLinebreak(GlobalVariables.SymbolNameOrFunctionCall.Value, " == ");
               codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, endTerminalSymbol.Identifier);
               break;
            }

            default:// generate: check a sequence of three or more allowed symbols
            {
               if (block.blockEnd == last)
               {
                  // special case: at end the of relevant symbols test of end may be ommitted
                  codegen.AppendWithOptionalLinebreak(GlobalVariables.SymbolNameOrFunctionCall.Value, " >= ");
                  codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, startTerminalSymbol.Identifier);
               }
               else
               { // generate: test closed interval of three or more symbols
                  if (useParentheses)
                     codegen.Append('(');
                  codegen.AppendWithOptionalLinebreak(GlobalVariables.SymbolNameOrFunctionCall.Value, " >= ");
                  codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, startTerminalSymbol.Identifier);
                  codegen.Append(" && ");
                  codegen.AppendWithOptionalLinebreak(GlobalVariables.SymbolNameOrFunctionCall.Value, " <= ");
                  codegen.AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, endTerminalSymbol.Identifier);
                  if (useParentheses)
                     codegen.Append(')');
               }
               break;
            }
            }
         }
      }

      private static void GenerateIsIn(P5CodegenCS codegen, BitArray Condition, BitArray Relevant)
      {
         codegen.IncrementIndentationLevel();
         BitArray InverseCondition = new BitArray(Condition).Not().And(Relevant);
         if (Condition.PopulationCount() < InverseCondition.PopulationCount())
         {
            codegen
               .Append(GlobalVariables.IsInMethod.Value)
               .Append('(');
            GenerateIsInArguments(codegen, Condition);
         }
         else
         {
            codegen.Append('!')
               .Append(GlobalVariables.IsInMethod.Value)
               .Append('(');
            GenerateIsInArguments(codegen, InverseCondition);
         }
         codegen.DecrementIndentationLevel().Append(')');
      }

      private static void GenerateIsInArguments(P5CodegenCS codegen, BitArray Condition)
      {
         Boolean Is1st = true;
         // foreach terminal symbol:
         for (Int32 i = Condition.FindNextTrue(); i < Condition.Count; i = Condition.FindNextTrue(i))
         {
            TerminalSymbol t = GlobalVariables.GetTerminalSymbolByIndex(i);
            if (!Is1st)
               codegen.AppendWithOptionalLinebreak(" | ");
            Is1st = false;

            t.IsUsedInIsIn = true; // the int constant "_(identifier of terminal symbol)" has to be declared

            codegen
               .AppendWithOptionalLinebreak(GlobalVariables.FlagsPrefix.Value)
               .Append(t.Identifier);
         }

         if (Is1st)
         { // no argument (all false) - usually will not happen
            codegen.Append('0');
         }
      }

   }

}
