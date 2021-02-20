using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace grammlator {
   /// <summary>
   /// IdNumbers of <see cref="ParserState"/>s are assigned at start of phase 2 and reset at end of phase 2
   /// </summary>
   internal sealed partial class ParserState : ParserAction, IELementOfPartition {
      internal override ParserActionEnum ParserActionType => ParserActionEnum.isParserState;

      internal ItemList CoreItems = new ItemList();

      private static readonly ListOfParserActions emptyListOfParserActions = new ListOfParserActions(0);

      /// <summary>
      /// <list type="table">
      /// <term>P1 adds the following <see cref="ParserAction"/>s to <see cref="Actions"/>:</term>
      /// <item> <see cref="NonterminalTransition"/>, NextAction is <see cref="HaltAction"/> (nonterminal shift - halt)</item>
      /// <item> <see cref="NonterminalTransition"/>, NextAction is <see cref="Definition"/> (nonterminal shift - reduce)</item>
      /// <item> <see cref="NonterminalTransition"/>, NextAction is <see cref="ParserState"/> (nonterminal shift to state)</item>
      /// <item> <see cref="LookaheadAction"/>, NextAction is <see cref="Definition"/> (lookahead - reduce) </item>
      /// <item> <see cref="TerminalTransition"/>, NextAction is <see cref="Definition"/> (shift-reduce)</item>
      /// <item> <see cref="TerminalTransition"/>, NextAction is <see cref="ParserState"/> (shift to state)</item>
      /// </list>
      /// <list type="table">
      /// <term>P3.Solve..conflicts adds the following <see cref="ParserAction"/>s to <see cref="Actions"/>:</term>
      /// <item> <see cref="PrioritySelectAction"/>, NextAction is <see cref="PriorityBranchAction"/></item>
      /// </list>
      /// </summary>
      internal ListOfParserActions Actions = emptyListOfParserActions;

      private static readonly List<ParserState> emptyPredecessorList = new List<ParserState>(0);

      /// <summary>
      /// Is computed in phase 2 and used in phase 3 and 4
      /// </summary>
      internal List<ParserState> PredecessorList = emptyPredecessorList;

      internal BitArray? PossibleInputTerminals = null;

      /// <summary>
      /// The set of all terminal symbols accepted by terminal transitions of the FollowState
      /// It is the Or of all of the terminalTransition.TerminalSymbols with terminalTransition is in this.Actions.
      /// <para>Is set and used (to avoid recomputation) in P3 AssignTerminalSymbolsAndComputeDirectRead()
      /// for all states which are the NextAction of a nonterminal transition.</para>
      /// </summary>
      internal BitArray? DirectRead = null;

      internal Boolean ContainsErrorHandlerCall {
         get;
         set;
      }

      /// <summary>
      /// Estimated complexity of generated code (conditions)
      /// for the case of generationg if instructions. This complexity
      /// may be used by grammlator to decide if it should generate
      /// if instructions (for all but the last conditional action)
      /// or a switch instruction.
      /// </summary>
      internal Int32 IfComplexity {
         get;
         set;
      }

      /// <summary>
      /// Constructor which assigns the number (&gt;=1)to the states IdNumber and <see cref="StateStackNumber"/>
      /// and copies the items to the <see cref="CoreItems"/>
      /// </summary>
      /// <param name="Number">Assigned to <see cref="StateStackNumber"/> and to IdNumber</param>
      /// <param name="Items">Core items which will be copied to <see cref="CoreItems"/></param>
      internal ParserState(Int32 Number, ItemList Items)
      {
         this.CoreItems = new ItemList(Items);
         this.StateStackNumber = Number; // StateStackNumber >= 0
         this.IdNumber = Number; // IDNumber >= 0
      }

      internal override void CountUsage(Boolean Accept) // ParserState
      {
         if (Calls > 0)
         {
            base.CountUsage(Accept);
         }
         else
         {
            // Because this ParserState is used all its actions are used
            base.CountUsage(Accept);
            foreach (ParserAction actionOfParserstate in Actions)
               actionOfParserstate.CountUsage(false);
         }
      }

      /// <summary>
      /// Bestimmt die Folgeaktion bei Eingabe des einzugebenden Symbols in den Zustand
      /// Achtung: statt einer Haltaktion wird null zurück gegeben
      /// </summary>
      /// <param name="InputSymbol">Symbol, zu dessen Eingabe die Folgeaktion bestimmt werden soll</param>
      /// <returns>the action or null (halt)</returns>
      /// <exception cref="ErrorInGrammlatorProgramException"></exception>
      internal NonterminalTransition ActionCausedBy(NonterminalSymbol InputSymbol)
      {
         var result = Actions.Find((a) => (a as NonterminalTransition)?.InputSymbol == InputSymbol);

         // There must exist an action also for the Startsymbol: a HaltAction
         if (result == null)
            throw new ErrorInGrammlatorProgramException(
               $"Missing action in state {this.IdNumber + 1} for InpuSymbol {InputSymbol.Identifier}"
               );

         return (NonterminalTransition)result;
      }

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         sb.Append(P5CodegenCS.GotoLabel(this, false));
         if (StateStackNumber >= 0)
         {
            sb.Append(" (")
              .Append(StateStackNumber)
              .Append(") ");
         }
         else if (StateStackNumber <= -2)
         {
            sb.Append(" (*")
              .Append(-StateStackNumber - 2)
              .Append(") ");
         }
         sb.AppendLine(": ")
           .AppendLine("Items:");
         CoreItems.ToStringbuilderExtended(sb)
           .Append("Actions: ");
         Actions?.AppendToSB(sb)
           .AppendLine();
         return sb;
      }

      internal override StringBuilder AppendShortToSB(StringBuilder sb)
          => sb.Append("goto ").Append(P5CodegenCS.GotoLabel(this, false));

      /// <summary>
      /// Checks the states actions whether there is exactly one action of type
      /// <see cref="LookaheadAction"/> 
      /// with all terminal symbols allowed
      /// or of type <see cref="PrioritySelectAction"/>.
      /// If yes returns the index of this action else returns n-1. 
      /// </summary>
      /// <returns>the index of the only one terminal action (or of the 1st of eqivalent actions) or -1</returns>
      internal Int32 IndexOfRedundantLookaheadOrSelectAction()
      {

         Int32 FirstFoundIndex = -1;

         for (Int32 i = 0; i < Actions.Count; i++)
         {
            Int32 FoundIndex;

            switch (Actions[i])
            {
            case TerminalTransition transition:
               if (!transition.TerminalSymbols.Empty())
               {
                  // a TerminalTransition, even if caused by all symbols,
                  // can not be skipped because the accept must be generated
                  return -1;
               }
               // ...TerminalSymbols.Empty(): this transition will never be executed, can be ignored
               // This may happen if there is also a IF ( ...All()) ...
               continue;
            case LookaheadAction lookAction:
               if (lookAction.TerminalSymbols.All())
                  FoundIndex = i;
               // FittingAction = lookAction; // action with all terminal symbols allowed (or no terminal symbols defined)
               else if (lookAction.TerminalSymbols.Empty()) // only correct after preceding "if( ...ALL())"
                  continue; // this action will never be executed
               else
               {
                  FoundIndex = i;
               }

               break;
            case PrioritySelectAction selectAction:
               if (selectAction.NextAction is TerminalTransition)
                  return -1; // see above ????

               // The selectAction contains only Definitions -> LookAheadActions
               if (selectAction.TerminalSymbols.All())
                  FoundIndex = i;
               // FittingAction = selectAction; // action with all terminal symbols allowed (or no terminal symbols defined)
               else if (selectAction.TerminalSymbols.Empty()) // must not occur because some terminals must cause the conflict
                  continue; // this action will never be executed
               else
               {
                  FoundIndex = i;
               }

               break;
            case ReduceAction _:
               FoundIndex = i;
               break;
            case DeletedParserAction _:
            case NonterminalTransition _:
               continue;
            default: // all other types of actions
               Debug.Assert(false);
               throw new ArgumentException();
            }

            if (FirstFoundIndex == -1)
               FirstFoundIndex = FoundIndex;
            else if (!(Actions[FirstFoundIndex].Equals(Actions[FoundIndex])))
               return -1; // more than one action
         } // foreach

         return FirstFoundIndex;
      }

      /// <summary>
      /// Finds and resolves the conflicts of the state, returns 1, if conflicts have been found, else 0
      /// </summary>
      /// <param name="allowedTerminalsUpToThisAction">A Bitarray to be used by the method</param>
      /// <param name="sb">the log of conflicts will be written to <paramref name="sb"/></param>
      /// 
      /// <returns>1 if conflict have been found, else 0</returns>
      public Int32 FindAndSolveConflictsOfState(
         BitArray allowedTerminalsUpToThisAction,
         out int numberOfConflictsNotSolvedByExplicitPriority,
         StringBuilder sb
         )
      {
         // The union of the TerminalSymbols of all the actions up to the action with IndexOfAction-1
         allowedTerminalsUpToThisAction.SetAll(false);
         // Allocate a BitArray to be used for the intersection of the actual actions terminal symbols
         // and the union of the terminal symbols of all preceding actions
         var conflictSymbols = new BitArray(allowedTerminalsUpToThisAction); // value doesn't matter

         Boolean writeStateHeader = true;  // false after the header "Conflicts in state ..." has been written
         numberOfConflictsNotSolvedByExplicitPriority = 0;


         // Find all actions which have at least one terminal symbol in common
         // with one of the preceding actions.
         for (Int32 IndexOfAction = 0; IndexOfAction < Actions!.Count; IndexOfAction++)
         {

            ParserAction thisAction = Actions[IndexOfAction];
            if (!(
                  thisAction is TerminalTransition
                  || thisAction is LookaheadAction
                  ))
               continue;

            BitArray terminalsOfThisAction = ((ConditionalAction)thisAction).TerminalSymbols;


            if (!
               conflictSymbols
                 .Assign(allowedTerminalsUpToThisAction)  // Assigns to conflictSymbols
                 .And(terminalsOfThisAction) // modifies conflictSymbols
                 .Empty()  // no new conflict
               )
            {
               // Solve the conflict between thisAction and one or more of the preceding actions 
               writeStateHeader =
                  SolveAndLogConflictsOfAction(IndexOfAction, terminalsOfThisAction, conflictSymbols,
                                         allowedTerminalsUpToThisAction, sb,
                                         writeStateHeader, out numberOfConflictsNotSolvedByExplicitPriority);
            }

            // allowedSymbolsUpToThisAction and terminalsOfThisAction might be modified by SolveConflicts
            allowedTerminalsUpToThisAction.Or(terminalsOfThisAction);
         } // for (int IndexOfAction

         // Remove all actions that have been replaced by DeletedParserAction
         RemoveDeletedActions();

         return writeStateHeader ? 0 : 1;
      }

      /// <summary>
      /// Tests the state for conflicts regarding the given action, solves potential conflicts by modifying
      /// the terminal symbols of the states actions, writes a protocol to sb and may modify 
      /// <paramref name="allowedTerminalsUpToConflictAction"/>
      /// </summary>
      /// <param name="indexOfConflictAction">Index of action which may cause a conflict</param>
      /// <param name="terminalsOfThisAction">The terminal symbols which are the condition of this action</param>
      /// <param name="allowedTerminalsUpToConflictAction">The union of the terminal symbols of all preceding actions
      /// of the state: may be modified by conflict 
      /// <returns>false, if there have been conflicts and state information has been written to sb</returns>
      /// solution</param>
      /// <param name="sb">The description of the conflicts will be written to sb</param>
      /// <param name="writeHeadline">If true when there are conflicts a description of the state has to be be written to sb above the conflict description(s)</param>
      private Boolean SolveAndLogConflictsOfAction(Int32 indexOfConflictAction, BitArray terminalsOfThisAction,
            BitArray conflictSymbols, BitArray allowedTerminalsUpToConflictAction, StringBuilder sb,
            Boolean writeHeadline, out int  numberOfConflictsNotSolvedByExplicitPriority)
      {

         numberOfConflictsNotSolvedByExplicitPriority = 0;
         // find all groups of actions which conflict with the ConflictAction and solve the conflict for each group
         do
         {
            // There is at least one conflict between the action and one of the preceding actions 

            // conflictSymbols is a subset of AllowedSymbolsUpToConflictAction and 
            // conflictSymbols is a subset of SymbolsOfThisAction

            // log state if 1st conflict in this state
            if (writeHeadline)
            {
               writeHeadline = false;
               sb.AppendLine()
                 .Append("Conflicts in state ")
                 .Append(IdNumber + 1)
                 .AppendLine();
               CoreItems.ToStringbuilderExtended(sb);
            }

            // find one group of actions which conflict with the ConflictAction and solve the conflict for this group
            SolveAndLogSubsetOfConflict(indexOfConflictAction,
                                         conflictSymbols,
                                         allowedTerminalsUpToConflictAction, // will be modified if the terminal symbols of one of the prededing actions is modified
                                         out int numberOfActionsWithHighestPriority,
                                         sb);
            numberOfConflictsNotSolvedByExplicitPriority += (numberOfActionsWithHighestPriority-1);
            // One conflict is solved, symbolsOfThisAction may be modified
            // There may be more conflicts between the action and the preceding actions

         }
         while
            (!conflictSymbols
                .Assign(allowedTerminalsUpToConflictAction)  // Assigns to conflictSymbols
                .And(terminalsOfThisAction) // modifies conflictSymbols
                .Empty()  // break if no conflict or all conflicts are solved
            );

         return writeHeadline;
      }

      /// <summary>
      /// Determine one set of terminal symbols common to a set of conflicting actions and solve
      /// the conflict between these actions by modifying the x.TerminalSymbols of some of the actions.
      /// This method must be called again until no more conflicting actions remain!
      /// </summary>
      /// <param name="indexOfFirstConflictAction">Index of action which causes a conflict</param>
      /// <param name="subsetOfConflictSymbols">Terminal symbols causing the conflict(s),
      /// a subset of <paramref name="allowedSymbolsUpToFirstConflictAction"/></param>
      /// <param name="allowedSymbolsUpToFirstConflictAction">The union of the terminal symbols of all preceding actions
      /// of the state: may be modified by conflict solution</param>
      /// <param name="sb">The protocol will be writte to sb</param>
      private void SolveAndLogSubsetOfConflict(
            Int32 indexOfFirstConflictAction,
            BitArray subsetOfConflictSymbols,
            BitArray allowedSymbolsUpToFirstConflictAction,
            out int numberOfActionsWithHighestPriority,
            StringBuilder sb)
      {
         var State = this; // makes method easier to understand

         // TOCHECK Might there be states - after solving conflicts -  without path to the halt action,
         //         might solving conflicts lead to endless loops?

         // Reduce the subsetOfConflictSymbols to the intersection of conflicting actions,
         // find the highest constant priority action of those actions and all  dynamic priority conflicting actions
         var dynamicPriorityActions
            = new ListOfParserActions(10); // usually will be empty or very short

         Int32 indexOfActionWithPriority
            = FindHighestPriorityActionOfConflictingActions(
               subsetOfConflictSymbols, dynamicPriorityActions, out numberOfActionsWithHighestPriority);

         // The  indexOfActionWithPriority may be -1 if only actions with dynamic priority are in conflict
         // The subsetOfConflictSymbols causes the conflict.
         // These symbols have to be removed from all participating actions exept 
         // the one with the highest priority and those with dynamic priority


         // If there are conflicting actions with dynamic priority a new ConditionalAction has to be added
         if (dynamicPriorityActions.Count > 0)
         {
            var prioritySelectAction
               = new PrioritySelectAction(
                        inputSymbols: subsetOfConflictSymbols,
                        constantPriorityAction:
                           indexOfActionWithPriority < 0
                           ? null
                           : this.Actions![indexOfActionWithPriority] as ConditionalAction,
                        dynamicPriorityActions: dynamicPriorityActions
                );

            // indexOfActionWithPriority = this.Actions!.Count;
            this.Actions.Add(prioritySelectAction);
            prioritySelectAction.IdNumber = GlobalVariables.ListOfAllPrioritySelectActions.Count;
            GlobalVariables.ListOfAllPrioritySelectActions.Add(prioritySelectAction);
            prioritySelectAction.NextAction.IdNumber = GlobalVariables.ListOfAllPriorityBranchActions.Count;
            GlobalVariables.ListOfAllPriorityBranchActions.Add((PriorityBranchAction)prioritySelectAction.NextAction);
         }

         // Log the conflict
         sb.AppendLine().Append("caused by == ");
         subsetOfConflictSymbols.BitsToStringbuilder(
             sb,
             GlobalVariables.TerminalSymbols,
             " | ",
             "all terminal symbols",
             "no terminal symbols")
            .AppendLine("; ");

         BitArray thisActionsConflictSymbols = new BitArray(subsetOfConflictSymbols.Length);

         // action might be removed from State.Actions inside the following loop.
         // "foreach (cAktion Aktion in Zustand.Aktionen)" would not allow this.
         // Hence a loop with an eplicit index is used.

         // For each action in the parser state 
         // log the actions priority and determine the action with the highest priority
         // remove the conflict symbols from lower priority actions
         for (Int32 IndexOfAction = 0; IndexOfAction < State!.Actions!.Count; IndexOfAction++)
         {
            ParserAction action = State.Actions[IndexOfAction];
            if (action is NonterminalTransition || !(action is ConditionalAction conditionalAction))
               continue;

            BitArray symbolsOfThisAction = conditionalAction.TerminalSymbols;
            if (symbolsOfThisAction == null)
               continue;

            if (thisActionsConflictSymbols.Assign(subsetOfConflictSymbols).And(symbolsOfThisAction).Empty())
               continue; // no conflict

            if (IndexOfAction == indexOfActionWithPriority)
            {
               if (dynamicPriorityActions.Count > 0)
                  symbolsOfThisAction.ExceptWith(subsetOfConflictSymbols); // moved to PrioritySelectAction
               else
               {
                  sb.Append("  priority ");
                  conditionalAction.AppendPriorityTo(sb);
                  sb.Append(" => highest priority: ");
                  conditionalAction.NextAction.AppendShortToSB(sb).AppendLine();
               }
            }
            else if (action is PrioritySelectAction)
            {
               conditionalAction.NextAction.AppendShortToSB(sb).AppendLine();
            }
            else
            {
               symbolsOfThisAction.ExceptWith(subsetOfConflictSymbols);

               // Write log:
               if (!(conditionalAction.HasPriorityFunction()))
               {
                  sb.Append("  priority ");
                  conditionalAction.AppendPriorityTo(sb);
                  sb.Append(" => is overruled: ");
                  conditionalAction.NextAction.AppendShortToSB(sb).AppendLine();

                  if (symbolsOfThisAction.Empty())
                     sb.AppendLine("    This action has been deleted because no input symbols remained.");
               }

            }

            if (symbolsOfThisAction.Empty())
            {
               // Delete actions without remaining terminal symbols.
               State.Actions[IndexOfAction] = new DeletedParserAction(conditionalAction.NextAction);

               // This can not be replaced by State.Actions.RemoveAt(IndexOfAction);
               // because then the indexes (which are used in loops) would skip one element
            }
         } // for (int IndexOfAction ...

         if (indexOfActionWithPriority >= indexOfFirstConflictAction)
         {
            // Remove the subsetOfConflictSymbols from the superset AllowedSymbolsUpToFirstConflictAction
            allowedSymbolsUpToFirstConflictAction.Xor(subsetOfConflictSymbols);
         }
      }

      /// <summary>
      /// Tests all actions with one or more of the conflict symbols and returns the action with the highest priority.
      /// Reduces the subsetOfConflictSymbols to the intersection of the conflicting actions
      /// </summary>
      /// <param name="subsetOfConflictSymbols"></param>
      /// <returns>Index of action with priority or -1 if only dynamic priorities</returns>
      private Int32 FindHighestPriorityActionOfConflictingActions(
            BitArray subsetOfConflictSymbols,
            ListOfParserActions dynamicPriorityActions,
            out int numberOfActionsWithHighestPriority)
      {
         // Test each action of the state if it causes a conflict with any preceding action 

         dynamicPriorityActions.Clear();
         numberOfActionsWithHighestPriority = 1;
         var thisActionsConflictSymbols = new BitArray(subsetOfConflictSymbols.Count);

         Int32 indexOfActionWithPriority = -1;
         Int64 highestPriority = Int32.MinValue;

         // For each action in the parser state (ignoring actions with dynamic priority)
         // determine action symbols and priority and determine the action with the highest priority
         for (Int32 IndexOfAction = 0; IndexOfAction < this.Actions!.Count; IndexOfAction++)
         {
            if (!(this.Actions[IndexOfAction] is ConditionalAction action)
               || (this.Actions[IndexOfAction] is NonterminalTransition))
               continue;

            // Has this action at least one symbol in common with the subsetOfConflictSymbols
            var symbolsOfThisAction = action.TerminalSymbols;
            if (symbolsOfThisAction == null)
               continue; // it hasn't, no conflict

            if (thisActionsConflictSymbols.Assign(subsetOfConflictSymbols).And(symbolsOfThisAction).Empty())
               continue; // no, it hasn't

            // Yes, it has
            // Reduce the set of symbols, handled in this call of the method,
            // to the symbols this action and the preceding conflicting actions have in common
            subsetOfConflictSymbols.Assign(thisActionsConflictSymbols);


            Int64 priority = 0; // priority of terminal transitions is always 0
            if (action is LookaheadAction laAction)
            {
               priority = laAction.ConstantPriority; // use assigned priority if no dynamic priority
               if (laAction.PriorityFunction != null)
               {
                  Debug.Assert(laAction.NextAction is Definition);
                  dynamicPriorityActions.Add(laAction);
                  continue; // an action with dynamic priority can not have highest static priority
               }
            }

            // Bookmark the action with the highest priority.
            // If two actions have the same priority, give TerminalTransition higher priority than LookAhead.
            if (priority >= highestPriority)
            {
               if (priority == highestPriority)
                  numberOfActionsWithHighestPriority++;
               else
                  numberOfActionsWithHighestPriority = 1;
               if (priority > highestPriority
                   || (priority == highestPriority
                       && action is TerminalTransition))
               {
                  highestPriority = priority;
                  indexOfActionWithPriority = IndexOfAction;
               }
            }
         }
         return indexOfActionWithPriority; // may be -1
      }

      /// <summary>
      /// Check if there are terminal symbols not causing any action of the state
      /// and if this is the case add an error action.
      /// </summary>
      /// <returns>The added action or null if none added</returns>
      public ConditionalAction? CheckAndAddErrorAction()
      {
         BitArray allowedSymbols;
         if (PossibleInputTerminals == null)
            allowedSymbols = new BitArray(GlobalVariables.NumberOfTerminalSymbols); // symbols causing actions
         else
            allowedSymbols = new BitArray(PossibleInputTerminals!).Not();

         Int32 counter = 0;

         foreach (ConditionalAction conditionalAction in Actions.OfType<ConditionalAction>())
         {
            Debug.Assert(!(conditionalAction is NonterminalTransition));
            allowedSymbols.Or(conditionalAction.TerminalSymbols);
            counter++;
         }

         if (counter == 0)
            return null; // there is an unconditional action

         if (allowedSymbols.All() && GlobalSettings.InputPeekChecksBounds.Value)
            return null; // in this state all terminal symbols are allowed

         ConditionalAction e;
         if (GlobalSettings.NameOfErrorHandlerMethod.Value != "")
         {
            // Add ErrorhandlingAction
            e = new ErrorhandlingAction(
             lookAhead: new BitArray(allowedSymbols).Not(),
             idNumber: this.IdNumber, // use the IdNumber of the ParserState as IdNumber of the ErrorHandlingAction
             state: this
             );
         }
         else
         { // Add LookaheadAction with nextAction ErrorHaltAction
            e = new LookaheadAction(
               number: GlobalVariables.NumberOfActions++,
               lookAheadSet: new BitArray(allowedSymbols).Not(),
               nextAction: GlobalVariables.ErrorHaltInstance
               );
         }

         Actions.Add(e);
         e.CountUsage(false);
         e.ComputeTerminalcountSumOfWeightsComplexity(GlobalVariables.TerminalSymbols);
         ContainsErrorHandlerCall = GlobalSettings.NameOfErrorHandlerMethod.Value != "";
         return e;
      }

      public void RemoveDeletedActions()
      {
         Int32 DeletedActionsCount = 0;

         for (Int32 ActionIndex = 0; ActionIndex < this.Actions!.Count; ActionIndex++)
         {
            if (Actions[ActionIndex] is DeletedParserAction)
               DeletedActionsCount++;
            else if (DeletedActionsCount > 0)
            {
               // Move the action to
               // replace first unused action in State.Actions with this action
               Actions[ActionIndex - DeletedActionsCount] = Actions[ActionIndex];
            }
         }

         Actions.RemoveFromEnd(DeletedActionsCount);
      }

      private Int32 SimplifyRecursionCount = 0;
      internal override ParserAction Simplify() // ParserState
      {
         /*
          * In general a ParserState contains only conditional actions of type:
          *   TerminalTransition
          *   NonterminalTransition
          *   LookaheadAction
          * These actions must not be simplified, but the respective NextAction may be.
          * 
          * Equivalent actions may be combined to one action.
          * 
          * A link to a ParserState which contains only one LookaheadAction
          * (and perhaps NonterminalTransitions) may be replaced by a link to
          * the LookaheadAction.
          * 
          * The LookaheadActions will be removed after all Definitions of the grammar have been
          * replaced.
          */


         //Prohibit an endless loop caused e.g. by "B=B??1??"
         if (SimplifyRecursionCount > 2)
            return this;

         /* if the parser state does not push on the state stack
          * and if it has no shift and no shift-reduce action
          * and only one look ahead action (perhaps after solving conflicts)
          * which is not "apply definition",
          * then the "goto state action" MUST be replaced 
          * by the next action of the single look ahead action
          * to avoid unnecessary look ahead
          *
          * The Problem "the definition "B=B??1??" causes an endless loop"
          * is prevented by SimplifyRecursionCount
          */

         Int32 IndexOfSingleAction = IndexOfRedundantLookaheadOrSelectAction();
         if (IndexOfSingleAction >= 0
            && Actions[IndexOfSingleAction] is LookaheadAction laAction
            // exactly one lookahead action
            && !(laAction.NextAction is Definition)
            )
         {
            Debug.Assert(!(laAction.NextAction is NonterminalTransition));



            if (StateStackNumber == -1)
            {
               // the state doesn't contain other actions except NonterminalTransition 
               SimplifyRecursionCount++;
               ParserAction result = laAction.NextAction.Simplify();
               SimplifyRecursionCount--;

               return result;
            }
            else
            {
               // the state can not be skipped but the laAction.NextAction can be made an unconditional action
               Actions[IndexOfSingleAction] = laAction.NextAction.Simplify();
            }
         }

         return this;
      }
   } // ParserState

   /// <summary>
   /// The ItemStruct includes an additional field InputSymbol to make the special handling of trivial definitions possible.
   /// First the InputSymbol is the marked symbol. Then the item is copied for all trivial definitions of the marked symbol
   /// and each trivial definition (symbol) is used as InputSymbol
   /// </summary>
   public struct ItemStruct {
      internal readonly Definition SymbolDefinition;
      internal readonly Int32 ElementNr;
      internal Symbol InputSymbol; // == EmptySymbol if enditem

      static readonly Symbol EmptySymbol = new TerminalSymbol(new UnifiedString(0), 0, 0, 0);
      /// <summary>
      /// Constructor returns a new item with definition, elementNr
      /// and InputSymbol==definition.Elements[elementNr] or InputSymbol==null for enditem
      /// </summary>
      /// <param name="definition"></param>
      /// <param name="elementNr"></param>
      internal ItemStruct(Definition definition, Int32 elementNr)
      {
         this.SymbolDefinition = definition;
         this.ElementNr = elementNr;
         this.InputSymbol =
            elementNr < definition.Elements.Length
             ? definition.Elements[elementNr] : EmptySymbol;
      }

      internal ItemStruct NewItemWithAdvancedMarker()
          => new ItemStruct(this.SymbolDefinition, this.ElementNr + 1);

      /// <summary>
      /// Constructor returns a new start item with definiton, elementNr==0 and InputSymbol==Alternative.Elements[0] or InputSymbol==null for empty definition
      /// </summary>
      /// <param name="definition"></param>
      internal ItemStruct(Definition definition) : this(definition, 0) { }

      internal Boolean IsEnditem => this.ElementNr == this.SymbolDefinition.Elements.Length;

      // GetHashcode see https://msdn.microsoft.com/de-de/library/system.object.gethashcode%28v=vs.110%29.aspx
      /// <summary>
      /// gets xor of SymbolDefinition.GetHashCode and ElementNr
      /// </summary>
      /// <returns>HashCode</returns>
      public override Int32 GetHashCode()
         => SymbolDefinition.GetHashCode() ^ ElementNr ^ InputSymbol?.GetHashCode() ?? 0;

      // Überladen von Equals und der Operatoren   == und != für sItem
      /// <summary>
      ///  compares xxx.SymbolDefinition and then xxx.ElementNr
      /// </summary>
      /// <param name="obj"></param>
      /// <returns>true if equal</returns>
      public override Boolean Equals(Object? obj) =>
          // schneller als der Default
          // siehe https://msdn.microsoft.com/de-de/library/2dts52z7%28v=vs.110%29.aspx
          obj is ItemStruct other
              && other.SymbolDefinition == this.SymbolDefinition
              && other.ElementNr == this.ElementNr
              && other.InputSymbol == this.InputSymbol;

      /// <summary>
      /// compares xxx.SymbolDefinition and then xxx.ElementNr
      /// </summary>
      /// <param name="leftArgument"></param>
      /// <param name="rightArgument"></param>
      /// <returns>true if equal</returns>
      public static Boolean operator ==(ItemStruct leftArgument, ItemStruct rightArgument)
      {
         return leftArgument.SymbolDefinition == rightArgument.SymbolDefinition
             && leftArgument.ElementNr == rightArgument.ElementNr
             && leftArgument.InputSymbol == rightArgument.InputSymbol;
      }

      /// <summary>
      ///  compares xxx.SymbolDefinition and then xxx.ElementNr
      /// </summary>
      /// <param name="leftArgument"></param>
      /// <param name="rightArgument"></param>
      /// <returns>true if not equal</returns>
      public static Boolean operator !=(ItemStruct leftArgument, ItemStruct rightArgument)
      {
         return leftArgument.SymbolDefinition != rightArgument.SymbolDefinition
             || leftArgument.ElementNr != rightArgument.ElementNr;
      }

      // Mit IComparable ist die Default-Sortierreihenfolge für Sort() etc. implementiert
      // siehe https://msdn.microsoft.com/en-us/library/system.icomparable.compareto%28v=vs.110%29.aspx

      /// <summary>
      /// Compares SymbolDefinition.DefinedSymbol.SymbolNumber, then SymbolDefinition.IdNumber, 
      /// then ElementNr, then IdNumber
      /// </summary>
      /// <param name="other"></param>
      /// <returns>0 if equal, -1 if less than other, +1 if greater than other</returns>
      public Int32 CompareTo(ItemStruct other)
      {
         /* Is used in P2 to sort the items of each new state.
          * The resulting order influences the order in which new states are computed 
          * and hence the numbers assigned to states.
          * The resulting order of actions is essential for comparing states.
          */
         if (this.SymbolDefinition.DefinedSymbol!.SymbolNumber < other.SymbolDefinition.DefinedSymbol!.SymbolNumber)
            return -1;
         if (this.SymbolDefinition.DefinedSymbol!.SymbolNumber > other.SymbolDefinition.DefinedSymbol!.SymbolNumber)
            return +1;
         if (this.SymbolDefinition.IdNumber < other.SymbolDefinition.IdNumber)
            return -1;
         if (this.SymbolDefinition.IdNumber > other.SymbolDefinition.IdNumber)
            return +1;
         if (this.ElementNr < other.ElementNr)
            return -1;
         if (this.ElementNr > other.ElementNr)
            return +1;
         Debug.Assert(this.SymbolDefinition.DefinedSymbol!.SymbolNumber == other.SymbolDefinition.DefinedSymbol!.SymbolNumber);
         if (this.SymbolDefinition.IdNumber < other.SymbolDefinition.IdNumber)
            return -1;
         if (this.SymbolDefinition.IdNumber > other.SymbolDefinition.IdNumber)
            return +1;
         return 0;
      }

      /// <summary>
      /// Used in P2 to sort items by their input symbols when computing LR0 states. 
      /// Orders items with terminal input symbols ahead of items with nonterminal input symbols,
      /// then by the defined symbols, 
      /// then the definitions id-nr, then the items element-nr
      /// </summary>
      /// <param name="item"></param>
      /// <returns>0 if equal, -1 if this is less, +1 if this is greater than other</returns>
      internal Int32 CompareInputSymbols(ItemStruct item)
      {
         // This comparision is used in phase 2.
         // Items have to be sorted by their input symbols.
         //
         // Items with equal input symbols are sorted by additional criteria to avoid random sort order,
         // which may influence the generated code.
         // These items are sorted by the symbol defined by the items alternative,
         // then by the alternatives IdNumber,
         // then by the items element number.

         // terminal symbols ahead of nonterminal symbols
         if (this.InputSymbol.IsTerminalSymbol && !item.InputSymbol.IsTerminalSymbol)
            return -1;
         if (!this.InputSymbol.IsTerminalSymbol && item.InputSymbol.IsTerminalSymbol)
            return +1;

         // terminal resp. nonterminal inputsymbols with increasing numbers
         if (this.InputSymbol.SymbolNumber < item.InputSymbol.SymbolNumber)
            return -1;
         if (this.InputSymbol.SymbolNumber > item.InputSymbol.SymbolNumber)
            return +1;

         if (this.SymbolDefinition.DefinedSymbol!.SymbolNumber < item.SymbolDefinition.DefinedSymbol!.SymbolNumber)
            return -1;
         if (this.SymbolDefinition.DefinedSymbol!.SymbolNumber > item.SymbolDefinition.DefinedSymbol!.SymbolNumber)
            return +1;

         if (this.SymbolDefinition.IdNumber < item.SymbolDefinition.IdNumber)
            return -1;
         if (this.SymbolDefinition.IdNumber > item.SymbolDefinition.IdNumber)
            return +1;

         if (this.ElementNr < item.ElementNr)
            return -1;
         if (this.ElementNr > item.ElementNr)
            return +11;

         return 0;
      }
   }

   internal class ItemList : List<ItemStruct> {
      /// <summary>
      /// Constructor
      /// </summary>
      internal ItemList()
      {
      }

      internal ItemList(ItemList CopyFrom) : base(CopyFrom) { }

      internal Boolean IsEqualTo(ItemList other)
      {
         if (this.Count != other.Count)
            return false;
         for (Int32 i = 0; i < this.Count; i++)
         {
            if ((this[i].SymbolDefinition != other[i].SymbolDefinition)
                || (this[i].ElementNr != other[i].ElementNr))
            {
               return false;
            }
         }
         return true;
      }

      internal void AppendToSB(StringBuilder sb)
      {
         Boolean isfirst = true;
         foreach (ItemStruct Item in this)
         {
            if (!isfirst)
               sb.AppendLine();
            isfirst = false;

            Definition d = Item.SymbolDefinition;
            d.DefinedSymbol!.IdentifierAndAttributesToSB(sb).Append("= ");
            d.ElementsToStringbuilder(sb, Item.ElementNr);
         }
      }

      internal StringBuilder ToStringbuilderExtended(StringBuilder sb)
      {
         foreach (ItemStruct Item in this)
         {
            Definition d = Item.SymbolDefinition;
            d.DefinedSymbol!.IdentifierAndAttributesToSB(sb).Append("= ");
            d.AppendToSB(sb, Item.ElementNr);
            sb.AppendLine();
         }
         return sb;
      }
   } // class ItemList
} // namespace
