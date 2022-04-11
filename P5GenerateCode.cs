﻿using System;
using System.Diagnostics;

namespace grammlator {
   internal partial class P5GenerateCode {
      public static void MakeInstanceAndExecute(P5CodegenCS Codegen)
         => new P5GenerateCode(Codegen).Execute();

      /// <summary>
      /// Generate code
      /// </summary>
      /// <param name="codeGen"></param>
      private P5GenerateCode(P5CodegenCS codeGen)
      {
         this.Codegen = codeGen;
      }

      private readonly P5CodegenCS Codegen;

      /* TODO Not yet implemented:
       *    only an idea: allow different sets of terminal symbols generated by different classes
       *       each with its own peek and accept instructions
       *       
       *    CHECK is there garbadge in the program from some older prefix attempts
       *    
       *    If states are reached only via look ahead actions actions (no accept)
       *      then they do not need a peek instruction;
       *      then only those terminal symbols can occur as input symbols,
       *      which are allowed symbols for those reduce actions.
       *      In some states this might avoid error actions!
       *    
       *    Error handling: for each state try to find at least 1 terminal symbol which will lead
       *    (via other states and the respective symbols) to halt
       *    
       *    Recognize states which will never lead to halt (recognize endless loop example).
       *    
       *    */

      private void Execute()
      {
         // Determine whether the generated code will modify the attribute stack
         GlobalVariables.reductionsModifyAttributStack = false;
         foreach (ReduceAction reduction in GlobalVariables.ListOfAllReductions)
         {
            if (reduction.Calls > 0 && reduction.AttributeStackAdjustment != 0)
            {
               GlobalVariables.reductionsModifyAttributStack = true;
               break;
            }
         }

         // Check if the AttributeStack has to be reset here, because all xxx.Calls will be reset to 0
         Boolean CodeContainsErrorHalt = GlobalVariables.ErrorHaltInstance.Calls > 0;

         // Prevent special actions from being generated somewhere inside the generated code.
         // They will be generated at the end of the generated code (see below).
         GlobalVariables.EndOfGeneratedCodeInstance.Calls = -GlobalVariables.EndOfGeneratedCodeInstance.Calls;
         GlobalVariables.ErrorHaltInstance.Calls = -GlobalVariables.ErrorHaltInstance.Calls;

         // Check if the enum values of all terminal symbols can be used as flags
         GlobalVariables.TerminalSymbolsAreFlags = true;
         Int64 Flags = 0;
         foreach (TerminalSymbol terminal in GlobalVariables.TerminalSymbols)
         {
            if (terminal.EnumValue == 0 || (terminal.EnumValue & Flags) != 0)
            {
               // the terminal value is 0 or has at least one bit in common with one preceding terminal
               GlobalVariables.TerminalSymbolsAreFlags = false;
               break; // 
            }
            Flags = terminal.EnumValue | Flags;
         }

         if (GlobalVariables.TerminalSymbolsAreFlags)
            // the flags tests are generated as conditions without method call
            GlobalSettings.NameOfFlagTestMethod.Value = "";
         else
         {
            // Clear GlobalSettings.NameOfFlagTestMethod.Value if Flag-Tests can not be used in generated code
            Int64 MinValue = GlobalVariables.TerminalSymbols[0].EnumValue;
            Int64 MaxValue = GlobalVariables.TerminalSymbols[^1].EnumValue;
            if (MaxValue - MinValue > 63)
               GlobalSettings.NameOfFlagTestMethod.Value = "";
         }

         // The start of the code will be generated last (see below!)

         // Generate the code for the parsers first action and the sequence of actions reached from this action without goto
         GenerateCodeSequence(Codegen,
            GlobalVariables.Startaction,
            accept: false,
            labelMustBeGenerated: false);

         // Generate all the actions which are reached by goto and which are not yet generated
         // 1. generate code for all actions which are called more than once and therefore need a label
         //    and for all actions which can be reached from those actions without goto
         GenerateCodeWithLabels(2);
         // 2. generate code for all other actions
         GenerateCodeWithLabels(1);

         // Allow the special actions (see above) to be generated and generate them
         GlobalVariables.ErrorHaltInstance.Calls = -GlobalVariables.ErrorHaltInstance.Calls;
         GlobalVariables.EndOfGeneratedCodeInstance.Calls = -GlobalVariables.EndOfGeneratedCodeInstance.Calls;
         if (GlobalVariables.ErrorHaltInstance.Calls > 0)
         {
            GenerateCodeSequence(Codegen,
               GlobalVariables.ErrorHaltInstance, GlobalVariables.ErrorHaltInstance.AcceptCalls > 0,
               labelMustBeGenerated: true);
         }
         if (GlobalVariables.EndOfGeneratedCodeInstance.Calls > 0)
         {
            GenerateCodeSequence(Codegen,
               GlobalVariables.EndOfGeneratedCodeInstance, GlobalVariables.EndOfGeneratedCodeInstance.AcceptCalls > 0,
               labelMustBeGenerated: true);
         }

         Codegen.GenerateEndOfRegion(); // and output code which may be buffered in codegen

         // Now the declarations can be generated preceding the already generated instructions
         Codegen.GenerateStartOfCodeAndCopyCodeToResultBuilder(
            GlobalVariables.TerminalSymbolsAreFlags,
            GenerateStateStackInitialCountVariable: GlobalVariables.NumberOfStatesWithStateStackNumber > 0,
            GenerateAttributeStackInitialCountVariable:
            GlobalVariables.reductionsModifyAttributStack && CodeContainsErrorHalt
            // The variable AttributeStackInitialCount is used by ErrorHaltActions, if the attributestack is used
            );

         return;
      }

      internal static Boolean GeneratesGoto(Boolean generateAccept, ParserAction action, Int32 indentationLevel)
      {
         if (generateAccept)
         {
            if (action.AcceptCalls <= 0 // has already been generated
                || (action.AcceptCalls >= 2 && indentationLevel > 0) // labels can not be generated in code blocks
                )
            {
               return true;
            }
         }

         return action.Calls <= 0
             || (action.Calls >= 2 && indentationLevel > 0)
             || indentationLevel >= GlobalSettings.OutputNestingLevelLimit.Value;
      }

      /// <summary>
      /// If the referenced (accept) parser action can be generated at the actual place of code
      /// <para>then
      /// if forced by <paramref name="forceLabel"/> or if the (accept) parser action is referenced more than
      /// once a label is generated.
      /// If <paramref name="generateAccept"/> the accept instruction is generated and as just described a label.
      /// </para>
      /// Else a goto is generated and true is returned.
      /// </summary>
      /// <param name="forceLabel">Forces the generation of a label. The IndentationLevelLevel must be 0.</param>
      /// <param name="generateAccept">Selects whether the action will be generated with accept</param>
      /// <param name="parserAction">The action to which a goto or for which a label (if needed) shall be generated</param>
      /// <param name="beginOfBlockHasBeenGenerated">Gibt an, ob eine öffnende Klammer erzeugt wurde</param>
      /// 
      /// <returns>true, if goto has been generated</returns>
      private static Boolean GenerateOptionalLabelOptionalAcceptOrGoto(
         P5CodegenCS codegen,
         Boolean forceLabel, // because goto has been generated on IndentationLevel==0
         Boolean generateAccept,
         ParserAction parserAction,
         ref Boolean beginOfBlockHasBeenGenerated)
      {
         Boolean InsideBlock = codegen.IndentationLevel > 0;

         if (generateAccept)
         {
            // generate accept part of action
            if (GenerateLabelOrGotoOrNothing
                  (codegen,
                   forceLabel,
                   commentIfNoLabel: false, // suppress accept comment
                   P5CodegenCS.GotoLabel(parserAction, accept: true),
                   parserAction.AcceptCalls,
                   InsideBlock))
               return true; // generated "goto accept..."

            // generated label "accept:..." or nothing;
            // now generate begin of cascaded block if inside of a new block
            if (InsideBlock && !beginOfBlockHasBeenGenerated)
            {
               codegen.GenerateBeginOfBlock();
               beginOfBlockHasBeenGenerated = true;
            }

            // generate accept instruction
            codegen.IndentExactly();
            codegen.GenerateAcceptInstruction();
            parserAction.AcceptCalls = 0; // mark that the accept part of the action has been generated
            forceLabel = false; // forceLabel has already been evaluated
         }

         //generate action part of action
         if (GenerateLabelOrGotoOrNothing
               (codegen,
                forceLabel,
                commentIfNoLabel:
                       !(parserAction is ErrorhandlingAction
                        || parserAction is ErrorHaltAction
                        || parserAction is HaltAction
                        || !GlobalSettings.GenerateComments.Value),
                P5CodegenCS.GotoLabel(parserAction, accept: false),
                parserAction.Calls,
                insideBlock: codegen.IndentationLevel > 0))
            return true; // generated goto

         parserAction.Calls = 0;  // mark that the begin of the action part of the parser code has been generated
                                  // the parser code will be generated by the calling method immediately
         return false; // generated begin of the action part
      } // GenerateOptionalLabelOptionalAcceptOrGoto

      private static Boolean GenerateLabelOrGotoOrNothing(
         P5CodegenCS codegen,
         Boolean forceLabel, // because goto has been generated on IndentationLevel==0
         Boolean commentIfNoLabel,
         String label, // codegen.GenerateLabel(parserAction, generateAccept)
         Int32 calls, // = generateAccept ? parserAction.AcceptCalls : parserAction.Calls;
         Boolean insideBlock // IndentationLevel > 0
         )
      {
         Debug.Assert(!(forceLabel && insideBlock));

         Boolean GenerateGoto =
            calls <= 0 // code has been already generated or must not be generated
            || (calls > 1 && insideBlock)  // code with more than 1 reference needs label but label not allowed in block
            || codegen.IndentationLevel >= GlobalSettings.OutputNestingLevelLimit.Value; // code would be indented to much

         Boolean GenerateLabel =
            !GenerateGoto &&
            (forceLabel || calls > 1);

         if (GenerateGoto)
         {
            codegen.GenerateGoto(label);
            return true; // goto has been generated
         }

         if (GenerateLabel)
         {
            codegen.GenerateLabel(label);
         }
         else if (commentIfNoLabel)
            codegen.IndentExactly().Append("// ").Append(label).AppendLine(":");

         return false;

      } // GenerateLabelOrGotoOrNothing (...)

      static internal Int32 CompareWeightandConditionComplexity(ParserAction? a, ParserAction? b)
      {
         // result < 0  sort order will be a then b
         // result == 0  a will not happen if a != b
         // result > 0  sort order will be b then a

         if (a is not ConditionalAction ActionA)
            return (b == null) ? 0 : 1;
         if (b is not ConditionalAction ActionB)
            return -1;

         Single PositionA = Position(ActionA);

         Single PositionB = Position(ActionB);

         if (PositionA != PositionB)
            return (Int32)(PositionA - PositionB);

         return (ActionA).IdNumber - ActionB.IdNumber;
      }

      private static Single Position(ConditionalAction action)
      {
         Single PositionA = action.Complexity * 100_000
            + ((action is ErrorhandlingAction
               || (action is LookaheadAction la && la.NextAction is ErrorHaltAction)
               )
              ? -0
              : -10 * action.SumOfWeights);

         // if NextAction is called only once ( ...calls==1) it can be generated inline and
         // perhaps the other actions NextAction might be generated at the end (lower indentation level)
         // if NextAction has been generated already (... calls<=0) it will generate a goto
         if (action is TerminalTransition)
            PositionA += ((action.NextAction.AcceptCalls <= 1) ? -5.0f : 0.0f);
         else
            PositionA += ((action.NextAction.Calls <= 1) ? -5.0f : 0.0f);
         return PositionA;
      }

      /// <summary>
      /// Compares ActionA.TerminalSymbols.IndexOfFirstTrueElement() with ActionB...
      /// </summary>
      /// <param name="a">Action A</param>
      /// <param name="b">Action B</param>
      /// <returns></returns>
      internal static Int32 CompareIndexOfFirstTrueElement(ParserAction a, ParserAction b)
      {
         if (a is not ConditionalAction ActionA)
            return (b == null) ? 0 : 1;
         if (b is not ConditionalAction ActionB)
            return -1;

         Int32 First1 = ActionA.TerminalSymbols.First();
         Int32 First2 = ActionB.TerminalSymbols.First();

         if (First1 != First2)
            return First1 - First2;

         return ActionA.IdNumber - ActionB.IdNumber;
      }

      /// <summary>
      /// Generates nothing if .Calls (resp. .AccepCalls) &lt;=0.
      /// Generates goto xxx, if the code has been generated or can not be generated at the actual nesting level.
      /// Generates accept if the action to be generated is a <see cref="TerminalTransition"/>.
      /// Sets .Calls (resp. .AcceptCalls) to 0.
      /// </summary>
      /// <param name="actionToGenerate"></param>
      /// <param name="labelMustBeGenerated"></param>
      /// 
      static internal void GenerateCodeSequence(P5CodegenCS codegen, ParserActionWithNextAction actionToGenerate, Boolean labelMustBeGenerated)
      {
         GenerateCodeSequence(codegen,
             actionToGenerate:
                actionToGenerate is ErrorhandlingAction || actionToGenerate is PrioritySelectAction
                   ? actionToGenerate // ErrorhandlingAction | PrioritySelectAction
                   : actionToGenerate.NextAction, // LookaheadAction | TerminalTransition: condition has been generated already
            accept: actionToGenerate is TerminalTransition,
            labelMustBeGenerated: labelMustBeGenerated);
      }

      /// <summary>
      /// Generates nothing if .Calls (resp. .AccepCalls) &lt;=0.
      /// Generates goto xxx, if the code has been generated or can not be generated at the actual nesting leve.
      /// Sets .Calls (resp. .AcceptCalls) to 0.
      /// </summary>
      /// <param name="actionToGenerate"></param>
      /// <param name="accept"></param>
      /// <param name="labelMustBeGenerated"></param>
      /// 
      static internal void GenerateCodeSequence(P5CodegenCS codegen, ParserAction actionToGenerate, Boolean accept, Boolean labelMustBeGenerated)
      {
         ParserAction? ActionToGenerate = actionToGenerate;
         Boolean Accept = accept;
         Boolean GotoHasBeenGenerated = labelMustBeGenerated;

         Boolean BeginOfBlockHasBeenGenerated = false;

         while (ActionToGenerate != null)
         {
            GotoHasBeenGenerated = GenerateOptionalLabelOptionalAcceptOrGoto(
               codegen,
               forceLabel: GotoHasBeenGenerated,
               generateAccept: Accept,
               parserAction: ActionToGenerate,
               beginOfBlockHasBeenGenerated: ref BeginOfBlockHasBeenGenerated);

            if (GotoHasBeenGenerated)
               break;

            // if need generate begin of block
            if (!BeginOfBlockHasBeenGenerated && codegen.IndentationLevel > 0)
            {
               codegen.GenerateBeginOfBlock();
               BeginOfBlockHasBeenGenerated = true;
            }

            /**** Generate the action ****/

            ActionToGenerate = ActionToGenerate.Generate(codegen, out Accept);

         }

         if (BeginOfBlockHasBeenGenerated)
         {
            codegen.IndentExactly();
            codegen.GenerateEndOfBlock("");
         }

         // No action to generate => if topmost level the next code can only be reached by a goto.
         // Generate an empty line preceding the label that will be generated.
         if (codegen.IndentationLevel == 0)
            codegen.AppendLine(' ');

      } //  ... GenerateCodeSequence(...)

      /// <summary>
      /// Generates code for all not yet generated actions with Calls &gt; <paramref name="MinimumOfCalls"/>"/>
      /// </summary>
      /// <param name="MinimumOfCalls">Mindestzahl von Aufrufen</param>
      private void GenerateCodeWithLabels(Int32 MinimumOfCalls)
      {
         /* Each action a with a.xxxCalls == 1 has a good chance to be generated as part of another action
          * (if not prohibited by the nesting level limit). The generation of these actions is delayed.
          * 
          * Each action a with a.xxxCalls > 1 needs a label and can not be generated
          * in nested instructions (but following another action as next action).
          * 
          * The following code is a simple heuristic (!) to get a sequence where 
          * actions are generated earlier than actions they contain.
          * 
          * Another perhaps better to optimize solution would be to sort the actions in advance.
          */

         /* Generate ReduceActions early if
          * * their next action has not yet been generated
          * * their next action has more than one call
          * Typically the next action of ReduceActions are States.
          */

         foreach (ReduceAction r in GlobalVariables.ListOfAllReductions)
         {
            if (r.NextAction.Calls > 1)
            {
               if (r.AcceptCalls >= MinimumOfCalls)
                  GenerateCodeSequence(Codegen, r, accept: true, labelMustBeGenerated: true);
               if (r.Calls >= MinimumOfCalls)
                  GenerateCodeSequence(Codegen, r, accept: false, labelMustBeGenerated: true);
            }
         }

         /* Generate ParserStates early because they often include other actions or have a next action 
          */
         foreach (ParserState state in GlobalVariables.ListOfAllStates)
         {
            if (state.AcceptCalls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, state, accept: true, labelMustBeGenerated: true);
            if (state.Calls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, state, accept: false, labelMustBeGenerated: true);
         }

         /* Typically PrioritySelectActions will have PriorityBranchActions as NextAction
          * and PriorityBranchActions will have ReduceActions as NextActions
          */
         foreach (PrioritySelectAction ps in GlobalVariables.ListOfAllPrioritySelectActions)
         {
            if (ps.AcceptCalls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, ps, accept: true, labelMustBeGenerated: true);
            if (ps.Calls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, ps, accept: false, labelMustBeGenerated: true);
         }

         foreach (PriorityBranchAction pb in GlobalVariables.ListOfAllPriorityBranchActions)
         {
            if (pb.AcceptCalls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, pb, accept: true, labelMustBeGenerated: true);
            if (pb.Calls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, pb, accept: false, labelMustBeGenerated: true);
         }


         /* Generate the other ReduceActions 
          */
         foreach (ReduceAction r in GlobalVariables.ListOfAllReductions)
         {
            if (r.AcceptCalls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, r, accept: true, labelMustBeGenerated: true);
            if (r.Calls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, r, accept: false, labelMustBeGenerated: true);
         }

         /* Typically branches are next actions of ReduceActions. So generate them after those 
          */
         foreach (BranchAction b in GlobalVariables.ListOfAllBranchActions)
         {
            if (b.AcceptCalls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, b, accept: true, labelMustBeGenerated: true);
            if (b.Calls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, b, accept: false, labelMustBeGenerated: true);
         }

         foreach (ErrorhandlingAction e in GlobalVariables.ListOfAllErrorhandlingActions)
         {
            if (e.AcceptCalls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, e, accept: true, labelMustBeGenerated: true);
            if (e.Calls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, e, accept: false, labelMustBeGenerated: true);
         }

         foreach (HaltAction h in GlobalVariables.ListOfAllHaltActions)
         {
            if (h.AcceptCalls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, h, accept: true, labelMustBeGenerated: true);
            if (h.Calls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, h, accept: false, labelMustBeGenerated: true);
         }

         // PushStateActions are next actions of conditional actions.
         foreach (PushStateAction p in GlobalVariables.ListOfAllPushStateActions)
         {
            // Because AcceptCalls<=1, Calls==1 PushStateActions are generated without labels
            // and the following should only be executed
            // if this assumption is changed by some  modifications of grammlator 
            Debug.Assert(p.AcceptCalls <= 0 && p.Calls <= 0);
            if (p.AcceptCalls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, p, accept: true, labelMustBeGenerated: true);
            if (p.Calls >= MinimumOfCalls)
               GenerateCodeSequence(Codegen, p, accept: false, labelMustBeGenerated: true);
         }
      } // void FehlendenCodeErzeugen
   } // class Phase5
} // namespace ...