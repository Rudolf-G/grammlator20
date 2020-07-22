using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Grammlator {
   /// <summary>
   /// IdNumbers of ParserStates are assigned at start of phase 2 and reset at end of phase 2
   /// </summary>
   internal sealed class ParserState: ParserAction, IELementOfPartition {
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
      internal ListOfParserActions Actions=emptyListOfParserActions;

      private static readonly List<ParserState> emptyPredecessorList = new List<ParserState>(0);

      /// <summary>
      /// Is computed in phase 2 and used in phase 3 and 4
      /// </summary>
      internal List<ParserState> PredecessorList = emptyPredecessorList;

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

      internal override void CountUsage(Boolean Accept)
         {
         if (Calls > 0)
            {
            base.CountUsage(Accept);
            }
         else
            {
            // Because this ParserState is used all its actions are used
            base.CountUsage(Accept);
            foreach (ParserAction actionOfParserstate in Actions!)
               actionOfParserstate.CountUsage(false);

            // Generating the error handler will generate a call of the state
            if (ContainsErrorHandlerCall)
               base.CountUsage(false);
            }
         }

      /// <summary>
      /// Bestimmt die Folgeaktion bei Eingabe des einzugebenden Symbols in den Zustand
      /// Achtung: statt einer Haltaktion wird null zurück gegeben
      /// </summary>
      /// <param name="InputSymbol">Symbol, zu dessen Eingabe die Folgeaktion bestimmt werden soll</param>
      /// <returns>the action or null (halt)</returns>
      /// <exception cref="ErrorInGrammlatorProgramException"></exception>
      internal ParserAction ActionCausedBy(NonterminalSymbol InputSymbol)
         {
         foreach (ParserAction parserAction in Actions!)
            {
            if ((parserAction as NonterminalTransition)?.InputSymbol == InputSymbol)
               return parserAction;
            }
         // There must exist an action also for the Startsymbol: a HaltAction
         throw new ErrorInGrammlatorProgramException(
            $"Missing action in state {this.IdNumber + 1} for InpuSymbol {InputSymbol.Identifier}"
            );
         }

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
         {
         sb.Append("State ")
           .Append(IdNumber + 1);
         if (StateStackNumber >= 0)
            {
            sb.Append(" (")
              .Append(StateStackNumber)
              .Append(") ");
            }
         sb.AppendLine(": ")
           .AppendLine("Items:");
         CoreItems.ToStringbuilderMitZusatzinfo(sb)
           .Append("Actions: ");
         Actions?.ToStringbuilder(sb)
           .AppendLine();
         return sb;
         }

      internal override void NameToSb(StringBuilder sb)
          => sb.Append("State ")
            .Append(IdNumber + 1);

      /// <summary>
      /// Checks the states actions whether there is exactly one action of type
      /// <see cref="TerminalTransition"/>
      /// with all terminal symbols allowed
      /// or of type <see cref="LookaheadAction"/> or 
      /// <see cref="PrioritySelectAction"/>.
      /// If yes returns this action else returns null. 
      /// </summary>
      /// <returns>the only one terminal action or null</returns>
      internal ConditionalAction? RedundantLookaheadOrSelectActionOrNull()
         {
         ConditionalAction? theOnlyOneAction = null;

         foreach (ParserAction action in Actions!)
            {
            ConditionalAction FittingAction;

            if (action is TerminalTransition transition)
               {
               if (!transition.TerminalSymbols.Empty())
                  {
                  // a TerminalTransition, even if caused by all symbols,
                  // can not be skipped because the accept must be generated
                  return null; // there must be one more action in the state, for example error handling
                  }
               // ...TerminalSymbols.Empty(): this transition will never be executed, can be ignored
               // This may happen if there is also a IF ( ...All()) ...
               continue;
               }
            else if (action is LookaheadAction lookAction)
               {
               if (lookAction.TerminalSymbols.All())
                  FittingAction = lookAction; // action with all terminal symbols allowed (or no terminal symbols defined)
               else if (lookAction.TerminalSymbols.Empty()) // only correct after preceding "if( ...ALL())"
                  continue; // this action will never be executed
               else
                  {
                  FittingAction = lookAction;
                  // TOCHECK this may cause deferred error recognition because the error handling action may not be generated
                  // return null; // this variant allows early error recognition
                  }
               }
            else if (action is PrioritySelectAction selectAction)
               {
               if (selectAction.NextAction is TerminalTransition)
                  return null; // see above

               // The selectAction contains only Definitions -> LookAheadActions
               if (selectAction.TerminalSymbols.All())
                  FittingAction = selectAction; // action with all terminal symbols allowed (or no terminal symbols defined)
               else if (selectAction.TerminalSymbols.Empty()) // must not occur
                  continue; // this action will never be executed
               else
                  {
                  FittingAction = selectAction;
                  // TOCHECK this may cause deferred error recognition because the error handling action may not be generated
                  // return null; // this variant allows early error recognition
                  }
               }
            else
               {
               continue; // ignore all other types of actions
               }

            if (theOnlyOneAction == null || theOnlyOneAction.Equals(FittingAction))
               theOnlyOneAction = FittingAction;
            else if (theOnlyOneAction != FittingAction)
               return null; // more than one action
            } // foreach

         return theOnlyOneAction;
         }

      /// <summary>
      /// Finds and resolves the conflicts of the state, returns 1, if conflicts have been found, else 0
      /// </summary>
      /// <param name="allowedTerminalsUpToThisAction">A Bitarray to be used by the method</param>
      /// <param name="sb">the log of conflicts will be written to <paramref name="sb"/></param>
      /// 
      /// <returns>1 if conflict have been found, else 0</returns>
      public Int32 FindAndSolveConflictsOfState(BitArray allowedTerminalsUpToThisAction, StringBuilder sb)
         {
         // The union of the TerminalSymbols of all the actions up to the action with IndexOfAction-1
         allowedTerminalsUpToThisAction.SetAll(false);
         // Allocate a BitArray to be used for the intersection of the actual actions terminal symbols
         // and the union of the terminal symbols of all preceding actions
         var conflictSymbols = new BitArray(allowedTerminalsUpToThisAction); // value doesn't matter

         Boolean writeStateHeader = true;  // false after the header "Conflicts in state ..." has been written


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
                  SolveConflictsOfAction(IndexOfAction, terminalsOfThisAction, conflictSymbols,
                                         allowedTerminalsUpToThisAction, sb, writeStateHeader);
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
      private Boolean SolveConflictsOfAction(Int32 indexOfConflictAction, BitArray terminalsOfThisAction,
            BitArray conflictSymbols, BitArray allowedTerminalsUpToConflictAction, StringBuilder sb,
            Boolean writeHeadline)
         {

         // find all groups of actions which conflict with the ConflictAction and solve the conflict for each group
         do
            {
            // There is at least one conflict between the action and one of the preceding actions 

            // conflictSymbols is a subset of AllowedSymbolsUpToConflictAction and 
            // conflictSymbols is a subset of SymbolsOfThisAction

            // protocol state
            if (writeHeadline)
               {
               writeHeadline = false;
               sb.AppendLine()
                 .Append("Conflicts in state ")
                 .Append(IdNumber + 1)
                 .AppendLine();
               CoreItems.ToStringbuilderMitZusatzinfo(sb);
               }

            // find one group of actions which conflict with the ConflictAction and solve the conflict for this group
            SolveAndLogSubsetOfConflict(indexOfConflictAction,
                                         conflictSymbols,
                                         allowedTerminalsUpToConflictAction, // will be modified if the terminal symbols of one of the prededing actions is modified
                                         sb);
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
            StringBuilder sb)
         {
         var State = this; // makes method easier to understand

         // TOCHECK Might there be states - after solving conflicts -  without path to the halt action,
         //         might solving conflicts lead to endless loops?

         // Reduce the subsetOfConflictSymbols to the intersection of conflicting actions,
         // find the highest constant priority action of those actions and all  dynamic priotity conflicting actions
         var dynamicPriorityActions = new ListOfParserActions(10); // usually will be empty or very short
         int indexOfActionWithPriority
            = FindHighestPriorityActionOfConflictingActions(subsetOfConflictSymbols, dynamicPriorityActions);

         // The  indexOfActionWithPriority may be -1 if only actions with dynamic priority are in conflict
         // The subsetOfConflictSymbols causes the conflict.
         // These symbols have to be removed from all participating actions exept 
         // the one with the highest priority and those with dynamic priority


         // If there are conflicting actions with dynamic priority a new ConditionalAction has to be added
         // Actions[indexOfActionWithPriority] is a ConditionalAction. As part of a ConditionalAction
         // the condition is not needed and hence .NextAction is used. 
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

            indexOfActionWithPriority = this.Actions!.Count;
            this.Actions.Add(prioritySelectAction);
            prioritySelectAction.IdNumber = GlobalVariables.ListOfAllPrioritySelectActions.Count;
            GlobalVariables.ListOfAllPrioritySelectActions.Add(prioritySelectAction);
            prioritySelectAction.NextAction.IdNumber = GlobalVariables.ListOfAllPriorityBranchActions.Count;
            GlobalVariables.ListOfAllPriorityBranchActions.Add((PriorityBranchAction)prioritySelectAction.NextAction);
            }

         // Log the conflict
         sb.AppendLine()
            .Append(" caused by == ");
         subsetOfConflictSymbols.BitsToStringbuilder(
             sb,
             GlobalVariables.TerminalSymbolByIndex,
             " | ",
             "all terminal symbols",
             "no terminal symbols")
            .AppendLine("; ");

         // action might be removed from State.Actions inside the following loop.
         // "foreach (cAktion Aktion in Zustand.Aktionen)" would not allow this.
         // Hence a loop with an eplicit index is used.

         // For each action in the parser state 
         // determine action symbols and priority and determine the action with the highest priority
         for (Int32 IndexOfAction = 0; IndexOfAction < State!.Actions!.Count; IndexOfAction++)
            {
            ParserAction action = State.Actions[IndexOfAction];
            if (!(action is ConditionalAction conditionalAction))
               continue;

            if (action is NonterminalTransition)
               continue;

            BitArray symbolsOfThisAction = conditionalAction.TerminalSymbols;
            if (symbolsOfThisAction == null)
               continue;

            if (IndexOfAction == indexOfActionWithPriority)
               {
               sb.Append("  priority ")
                 .Append(conditionalAction.Priority)
                 .Append(" => not modified: ");

               conditionalAction.ToStringbuilder(sb)
                 .AppendLine();
               }
            else
               {
               symbolsOfThisAction.ExceptWith(subsetOfConflictSymbols);
               // Write log:
               sb.Append("  priority ")
                 .Append(conditionalAction.Priority)
                 .Append(" => modified to: ");

               conditionalAction.ToStringbuilder(sb)
                 .AppendLine();

               if (symbolsOfThisAction.Empty())
                  {
                  // Delete actions without remaining terminal symbols.
                  // This may cause that other actions can not be reached.
                  // If not deleted there might result code with If(true)... and code that can be never be reached

                  sb.AppendLine("    This action has been deleted because no input symbols remained.");

                  State.Actions[IndexOfAction] = new DeletedParserAction(conditionalAction.NextAction);
                  // This can not be replaced by State.Actions.RemoveAt(IndexOfAction);
                  // because then the indexes (which are used in loops) would skip one element
                  }
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
            ListOfParserActions dynamicPriorityActions)
         {
         // Test each action of the state if it causes a conflict with any preceding action 

         dynamicPriorityActions.Clear();
         var thisActionsConflictSymbols = new BitArray(subsetOfConflictSymbols.Count); // TODO allocate only once

         Int32 indexOfActionWithPriority = -1;
         Int32 highestPriority = Int32.MinValue;

         // For each action in the parser state (ignoring actions with dynamic priority)
         // determine action symbols and priority and determine the action with the highest priority
         for (Int32 IndexOfAction = 0; IndexOfAction < this.Actions!.Count; IndexOfAction++)
            {
            if (!(this.Actions[IndexOfAction] is ConditionalAction action))
               continue;

            // Has this action at least one symbol in common with the subsetOfConflictSymbols
            var symbolsOfThisAction = action.TerminalSymbols;
            if (symbolsOfThisAction == null)
               continue; // it hasn't, no conflict

            thisActionsConflictSymbols.Assign(subsetOfConflictSymbols).And(symbolsOfThisAction);
            if (thisActionsConflictSymbols.Empty())
               continue; // no, it hasn't

            // Yes, it has
            // Reduce the set of symbols, handled in this call of the method,
            // to the symbols this action and the preceding conflicting actions have in common
            subsetOfConflictSymbols.Assign(thisActionsConflictSymbols);


            Int32 priority = 0; // priority of terminal transitions is always 0
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
            if (priority > highestPriority
                || (priority == highestPriority
                    && action is TerminalTransition))
               {
               highestPriority = priority;
               indexOfActionWithPriority = IndexOfAction;
               }
            }
         return indexOfActionWithPriority; // may be -1
         }

      /// <summary>
      /// Check if there are terminal symbols not causing any action of the state
      /// and if this is the case add an error action.
      /// </summary>
      /// <returns>The added action or null if none added</returns>
      public ErrorhandlingAction? CheckAndAddErrorAction(Boolean ErrorHandlerIsDefined)
         {
         // Fehleraktion ergänzen, falls im Zustand nicht alle terminalen Symbole erlaubt sind und 
         // es mehrere Aktionen gibt (Lookahead nötig) oder mindestens einen terminalenÜbergang
         // oder keine Aktion bei Eingabe terminaler Symbole (Beispiel S:S,a.).
         var allowedSymbols = new BitArray(GlobalVariables.NumberOfTerminalSymbols); // symbols causing actions with constant priority
         ErrorhandlingAction e;
         Int32 counter = 0;
         Boolean containsLookaheadAction = false;

         foreach (ConditionalAction conditionalAction in Actions.OfType<ConditionalAction>())
            {
            Debug.Assert(!(conditionalAction is NonterminalTransition));
            allowedSymbols.Or(conditionalAction.TerminalSymbols);
            counter++;
            if (conditionalAction is LookaheadAction)
               containsLookaheadAction = true;
            }

         if (counter == 0)
            return null; // there is an unconditional action

         var notAllowedSymbols = (BitArray)(new BitArray(allowedSymbols)).Not();
         if (notAllowedSymbols.Empty())
            return null;

         if (counter == 1 // one action
             &&
             containsLookaheadAction // not a terminal transition => lookahead action)
             )
            {
            return null; // execute lookahead action without condition
            }
         // TODO check this might be used earlier by optimizations ? This causes late error recognition

         // Add ErrorhandlingAction
         e = new ErrorhandlingAction(
             lookAhead: notAllowedSymbols,
             idNumber: this.IdNumber, // use the IdNumber of the ParserState as IdNumber of the ErrorHandlingAction
             state: this
             );
         Actions!.Add(e);

         ContainsErrorHandlerCall = ErrorHandlerIsDefined;
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

      /// <summary>
      /// Yields all actions of the state whereby instead of a PrioritySelectActions its
      /// NextAction is used and if it is a PriorityBranchAction
      /// </summary>
      public IEnumerable<ParserAction> FlatSetOfActions {
         get {
            Debug.Assert(Actions != null);
            for (Int32 i = 0; i < Actions.Count; i++)
               {
               ParserAction? a = Actions[i];
               while (a != null)
                  {
                  if (a is PrioritySelectAction psa)
                     a = psa.NextAction;
                  else if (a is PriorityBranchAction pba)
                     {
                     if (pba.ConstantPriorityAction != null)
                        yield return pba.ConstantPriorityAction;
                     for (Int32 k = 0; k < pba.DynamicPriorityActions.Count; k++)
                        {
                        yield return pba.DynamicPriorityActions[k];
                        }
                        ;
                     }
                  else
                     {
                     yield return a;
                     a = null;
                     }
                  }
               }
            }
         } //
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

      static readonly Symbol EmptySymbol = new TerminalSymbol("", 0);
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

      // zu GetHashcode siehe https://msdn.microsoft.com/de-de/library/system.object.gethashcode%28v=vs.110%29.aspx
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
         // TODO Check nachdem CompareErzeugtesSymbolFirst getrennt implementiert ist, könnte hier wieder die Elementnummer als erstes Kriterium genommen werden
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

   internal class ItemList: List<ItemStruct> {
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

      internal void ToStringbuilder(StringBuilder sb)
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

      internal StringBuilder ToStringbuilderMitZusatzinfo(StringBuilder sb)
         {
         foreach (ItemStruct Item in this)
            {
            Definition d = Item.SymbolDefinition;
            d.DefinedSymbol!.IdentifierAndAttributesToSB(sb).Append("= ");
            d.ToStringbuilder(sb, Item.ElementNr);
            sb.AppendLine();
            }
         return sb;
         }
      } // class ItemList
   } // namespace
