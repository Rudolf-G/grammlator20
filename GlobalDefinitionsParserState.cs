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
      internal ListOfParserActions Actions;

      /// <summary>
      /// Is computed in phase 2 and used in phase 3 and 4
      /// </summary>
      internal List<ParserState> PredecessorList;

      /// <summary>
      ///  The number, the parser state pushes on the state stack.
      ///  -1 if the state doesn't push its number on the stack.
      /// </summary>
      internal Int32 StateStackNumber {
         get; set;
         }

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
      /// Constructor, which copies the items to the <see cref="CoreItems"/>
      /// </summary>
      /// <param name="Items">The items to be copied to <see cref="CoreItems"/></param>
      private ParserState(ItemList Items) => this.CoreItems = new ItemList(Items);

      /// <summary>
      /// Constructor which assigns the number (&gt;=1)to the states IdNumber and <see cref="StateStackNumber"/>
      /// and copies the items to the <see cref="CoreItems"/>
      /// </summary>
      /// <param name="Number">Assigned to <see cref="StateStackNumber"/> and to IdNumber</param>
      /// <param name="Items">Core items which will be copied to <see cref="CoreItems"/></param>
      internal ParserState(Int32 Number, ItemList Items)
          : this(Items)
         {
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
            foreach (ParserAction actionOfParserstate in Actions)
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
         foreach (ParserAction parserAction in Actions)
            {
            if ((parserAction as NonterminalTransition)?.InputSymbol == InputSymbol)
               return parserAction;
            }
         // There must exist an action also for the Startsymbol: a HaltAction
         throw new ErrorInGrammlatorProgramException("Found no action");
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
         Actions.ToStringbuilder(sb)
           .AppendLine();
         return sb;
         }

      internal override void NameToSb(StringBuilder sb)
          => sb.Append("State ")
            .Append(IdNumber + 1);

      /// <summary>
      /// Checks the states actions whether there is exactly one action of type
      /// <see cref="TerminalTransition"/> with all terminal symbols allowed
      /// or of type <see cref="LookaheadAction"/>.
      /// If yes returns this action else returns null. 
      /// </summary>
      /// <returns>the only one terminal action or null</returns>
      internal LookaheadAction RedundantLookaheadActionOrNull()
         {
         LookaheadAction theOnlyOneAction = null;

         foreach (ParserAction action in Actions)
            {
            LookaheadAction FittingAction;

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
      /// Finds and resolves the static conflicts of the state, returns 1, if conflicts have been found
      /// </summary>
      /// <param name="sb"></param>
      /// <param name="allowedSymbolsUpToThisAction"></param>
      /// <param name="dynamicSymbols"></param>
      /// <returns>1 if a conflict has been found, else 0</returns>
      public Int32 FindAndResolveStaticConflictsOfState(StringBuilder sb, BitArray allowedSymbolsUpToThisAction, BitArray dynamicSymbols)
         {
         // Find and resolve conflicts for this state and add ErrorAction as appropriate
         allowedSymbolsUpToThisAction.SetAll(false); // Die im Zustand in den bisher bearbeiten Aktionen erlaubten Symbole
         Int32 numberOfConditionalTransitions = 0;

         Boolean writeStateHeader = true;  // false after the header "Conflicts in state ..." has been written

         // Find all actions which have at least one terminal symbol in common
         // with one preceding action. Ignore actions with dynamic priority.
         for (Int32 IndexOfAction = 0; IndexOfAction < Actions.Count; IndexOfAction++)
            {
            BitArray symbolsOfThisAction;

            switch (Actions[IndexOfAction])
               {
            case TerminalTransition TerminalTransition:
                  {
                  symbolsOfThisAction = TerminalTransition.TerminalSymbols;
                  numberOfConditionalTransitions++;
                  break;
                  }

            case LookaheadAction LookAheadAction:
                  {
                  symbolsOfThisAction = LookAheadAction.TerminalSymbols;
                  numberOfConditionalTransitions++;
                  if (LookAheadAction.NextAction is Definition d
                      && d.PriorityFunction != null)
                     {
                     if (symbolsOfThisAction != null)
                        dynamicSymbols.Or(symbolsOfThisAction);
                     continue; // ignore conflicts caused by actions with dynamic priority at the moment
                     }
                  break;
                  }

            default:
               continue; // no symbolsOfThisAction 
               }

            // found action with symbolsOfThisAction
            if (symbolsOfThisAction?.Empty() == false)
               {
               writeStateHeader =
                  SolveConflicts(sb, writeStateHeader, IndexOfAction, symbolsOfThisAction, allowedSymbolsUpToThisAction);

               // allowedSymbols* and symbolsOfThisAction* may be modified by conflict resolution
               allowedSymbolsUpToThisAction.Or(symbolsOfThisAction);
               }
            } // for (int IndexOfAction

         return writeStateHeader ? 0 : 1;
         }

      /// <summary>
      /// Tests the state for conflicts regarding the given action, solves the conflicts by modifying
      /// the terminal symbols of the states actions, writes a protocol to sb and modifies allowedSymbols
      /// </summary>
      /// <param name="sb">The description of the conflicts will be written to sb</param>
      /// <param name="WriteHeadline">If true when there are conflicts a description of the state has to be be written to sb above the conflict description(s)</param>
      /// <param name="IndexOfConflictAction">Index of action which may cause a conflict</param>
      /// <param name="SymbolsOfThisAction">The terminal symbols which are the condition of this action</param>
      /// <param name="AllowedSymbolsUpToConflictAction">The union of the terminal symbols of all preceding actions of the state: may be modified by conflict 
      /// <returns>false, if there have been conflicts and state information has been writte to sb</returns>
      /// solution</param>
      public Boolean SolveConflicts(StringBuilder sb, Boolean WriteHeadline,
          Int32 IndexOfConflictAction, BitArray SymbolsOfThisAction,
          BitArray AllowedSymbolsUpToConflictAction)
         {
         var conflictSymbols = new BitArray(SymbolsOfThisAction.Count);

         // repeat test and partial solution of conflicts until no more conflicts regarding this action
         while (!conflictSymbols.Assign(AllowedSymbolsUpToConflictAction).And(SymbolsOfThisAction).Empty())
            {
            // There is at least one conflict between the action and one of the preceding actions 

            // protocol state
            if (WriteHeadline)
               {
               WriteHeadline = false;
               sb.AppendLine()
                 .Append("Conflicts in state ")
                 .Append(IdNumber + 1)
                 .AppendLine();
               CoreItems.ToStringbuilderMitZusatzinfo(sb);
               }

            SolveAndProtocolSubsetsOfConflict(sb, IndexOfConflictAction, conflictSymbols, AllowedSymbolsUpToConflictAction);
            // One conflict is solved, allowedSymbols is modified
            // There may be more conflicts between the action and the preceding actions
            }
         return WriteHeadline;
         }

      /// <summary>
      /// Determine one set of terminal symbols common to a set of conflicting actions and solve
      /// the conflict between these actions by modifying the x.TerminalSymbols of some of the actions.
      /// This method must be called again until no more conflicting actions remain!
      /// </summary>
      /// <param name="sb">The protocol will be writte to sb</param>
      /// <param name="IndexOfFirstConflictAction">Index of action which causes a conflict</param>
      /// <param name="ConflictSymbols">Terminal symbols causing the conflict(s)</param>
      /// <param name="AllowedSymbolsUpToFirstConflictAction">The union of the terminal symbols of all preceding actions of the state: may be modified by conflict 
      /// solution</param>
      private void SolveAndProtocolSubsetsOfConflict(StringBuilder sb, Int32 IndexOfFirstConflictAction,
          BitArray ConflictSymbols, BitArray AllowedSymbolsUpToFirstConflictAction)
         {
         var State = this;

         // TOCHECK whether there should be some more optimizations before solving conflicts            

         var subsetOfConflictSymbols = new BitArray(ConflictSymbols);

         // TOCHECK Should more information about the conflicts be written into the log?
         // TOCHECK Might there be states - after solving conflicts -  without path to the halt action
         //         and might solving conflicts lead to endless loops?

         int indexOfActionWithPriority = GetHighestPriorityActionOfConflictingActions(subsetOfConflictSymbols);

         // The subsetOfConflictSymbols cause the conflict
         // These symbols have to be removed from all participating actions exept 
         // the one with the highest priority and those with dynamic priority

         // Log the conflict
         sb.AppendLine()
            .Append(" when Symbol == ");
         subsetOfConflictSymbols.BitsToStringbuilder(
             sb,
             GlobalVariables.TerminalSymbolByIndex,
             " | ",
             "all terminal symbols",
             "no terminal symbols")
            .AppendLine("; ");

         // Action will be modified inside the following loop.
         // "foreach (cAktion Aktion in Zustand.Aktionen)" would not allow this.
         // Hence a loop with an eplicit index is used.

         // For each action in the parser state (ignoring actions with dynamic priority)
         // determine action symbols and priority and determine the action with the highest priority
         for (Int32 IndexOfAction = 0; IndexOfAction < State.Actions.Count; IndexOfAction++)
            {
            ParserAction action = State.Actions[IndexOfAction];
            BitArray symbolsOfThisAction = null;

            Int32 priority = 0;

            if (!(action is ConditionalAction conditionalAction))
               continue;

            switch (conditionalAction)
               {
            case TerminalTransition ActionAsTerminalTransition:
                  {
                  symbolsOfThisAction = ActionAsTerminalTransition.TerminalSymbols;
                  break;
                  }

            case LookaheadAction laAktion:
                  {
                  symbolsOfThisAction = laAktion.TerminalSymbols;
                  if (laAktion.PriorityFunction != null)
                     continue;
                  priority = laAktion.ConstantPriority;
                  break;
                  }
               }

            if (symbolsOfThisAction == null)
               continue;

            if (IndexOfAction == indexOfActionWithPriority)
               {
               sb.Append("  priority ")
                 .Append(priority)
                 .Append(" => not modified: ");
               // sb.Append(maximalePriorität);

               action.ToStringbuilder(sb)
                 .AppendLine();
               }
            else
               {
               var weiterEingeengteKonfliktsymbole =
                  (BitArray)new BitArray(subsetOfConflictSymbols).And(symbolsOfThisAction);

               if (!weiterEingeengteKonfliktsymbole.Empty())
                  {
                  symbolsOfThisAction.ExceptWith(subsetOfConflictSymbols);
                  // Protokollieren:
                  sb.Append("  priority ")
                    .Append(priority)
                    .Append(" => modified to: ");
                  //.Append(maximalePriorität);

                  conditionalAction.ToStringbuilder(sb)
                    .AppendLine();

                  // TOCHECK What is the right place in the program to solve conflicts (first optimization  or first solve conflicts?)


                  // Delete actions with the empty set of terminal input symbols.
                  // This may cause that other actions can not be reached

                  // TODO Check   How is DeletedAction handled in further steps ???
                  if (symbolsOfThisAction.Empty())
                     {
                     // Delete actions without remaining terminal symbols.
                     // Otherwise there might result code with If(true)... and code that can be never be reached

                     sb.AppendLine("    This action has been deleted because no input symbols remained.");

                     State.Actions.RemoveAt(IndexOfAction);
                     IndexOfAction--;

                     //State.Actions[IndexOfAction] = new DeletedParserAction(conditionalAction.NextAction);
                     }
                  }
               }
            } // for (int IndexOfAction ...

         if (indexOfActionWithPriority < IndexOfFirstConflictAction)
            {
            return;
            }
         // die Konfliktsymbole von den erlaubten Symbolen abziehen
         AllowedSymbolsUpToFirstConflictAction.And(subsetOfConflictSymbols.Not());
         }

      /// <summary>
      /// Test all actions with one or more of the conflict symbols and return the action with the highest priority
      /// </summary>
      /// <param name="subsetOfConflictSymbols"></param>
      /// <returns>Index of action with priority</returns>
      private Int32 GetHighestPriorityActionOfConflictingActions(BitArray subsetOfConflictSymbols)
         {
         // Test each action of the state if it causes a conflict with any preceding action 

         var thisActionsConflictSymbols = new BitArray(subsetOfConflictSymbols.Count);

         Int32 indexOfActionWithPriority = 0;
         Int32 highestPriority = Int32.MinValue;

         for (Int32 IndexOfAction = 0; IndexOfAction < this.Actions.Count; IndexOfAction++)
            {
            ParserAction action = this.Actions[IndexOfAction];

            // For each action in the parser state (ignoring actions with dynamic priority)
            // determine action symbols and priority and determine the action with the highest priority
            Int32 priority = 0;
            BitArray symbolsOfThisAction;

            switch (action)
               {
            case TerminalTransition ActionAsTerminalTransition:
                  {
                  symbolsOfThisAction = ActionAsTerminalTransition.TerminalSymbols;
                  break;
                  }

            case LookaheadAction LookAheadAction:
                  {
                  symbolsOfThisAction = LookAheadAction.TerminalSymbols;
                  if (LookAheadAction.PriorityFunction != null)
                     continue;
                  priority = LookAheadAction.ConstantPriority;
                  break;
                  }

            default: // 
               continue; // no symbolsOfThisAction 
               }

            // found action with symbolsOfThisAction
            if (symbolsOfThisAction == null)
               continue;

            // Does this action have at least one symbol in common with the conflicting action?
            thisActionsConflictSymbols.Assign(subsetOfConflictSymbols).And(symbolsOfThisAction);

            if (thisActionsConflictSymbols.Empty())
               continue; // no, it hasn't

            // Yes, it does
            // Reduce the set of symbols, handled in this call of the method,
            // to the symbols this action and the preceding conflicting actions have in common
            subsetOfConflictSymbols.Assign(thisActionsConflictSymbols);

            // Bookmark the action with the highest priority.
            // If two actions have the same priortiy, give TerminalTransition higher priority than LookAhead.
            if (priority > highestPriority
                || (priority == highestPriority
                    && action is TerminalTransition))
               {
               highestPriority = priority;
               indexOfActionWithPriority = IndexOfAction;
               }
            }
         return indexOfActionWithPriority;
         }

      // TODO Redesign the copies of static conflict resolution to handle dynamic conflicts

      /// <summary>
      /// Tests the state for conflicts regarding the given action, solves the conflicts by modifying
      /// the terminal symbols of the states actions, writes a protocol to sb and modifies allowedSymbols
      /// </summary>
      /// <param name="sb">The protocol will be written to sb</param>
      /// <param name="IndexOfConflictAction">Index of action which may cause a conflict</param>
      /// <param name="SymbolsOfThisAction">The terminal symbols which are the condition of this action</param>
      /// <param name="AllowedSymbolsUpToConflictAction">The union of the terminal symbols of all preceding actions of the state: may be modified by conflict 
      /// solution</param>
      public void SolveDynamicConflicts(StringBuilder sb,
          Int32 IndexOfConflictAction, BitArray SymbolsOfThisAction,
          BitArray AllowedSymbolsUpToConflictAction)
         {
         var conflictSymbols = new BitArray(SymbolsOfThisAction.Count);
         // repeat test and partial solution of conflicts until no more conflicts regarding this action
         while (!conflictSymbols.Assign(AllowedSymbolsUpToConflictAction).And(SymbolsOfThisAction).Empty())
            {
            // There is at least one conflict between the action and one of the preceding actions  
            SolveAndProtocolSubsetOfDynamicConflict(sb, IndexOfConflictAction, conflictSymbols, AllowedSymbolsUpToConflictAction);
            // One conflict is solved, allowedSymbols is modified
            // There may be more conflicts between the action and the preceding actions
            }
         }

      /// <summary>
      /// Determine one set of terminal symbols common to a set of conflicting actions and solve
      /// the conflict between these actions by modifying the x.TerminalSymbols of some of the actions.
      /// This method must be called again until no more conflicting actions remain!
      /// </summary>
      /// <param name="sb">The protocol will be writte to sb</param>
      /// <param name="IndexOfFirstConflictAction">Index of action which causes a conflict</param>
      /// <param name="ConflictSymbols">Terminal symbols causing the conflict(s)</param>
      /// <param name="AllowedSymbolsUpToFirstConflictAction">The union of the terminal symbols of all preceding actions of the state: may be modified by conflict 
      /// solution</param>
      private void SolveAndProtocolSubsetOfDynamicConflict(StringBuilder sb, Int32 IndexOfFirstConflictAction,
          BitArray ConflictSymbols, BitArray AllowedSymbolsUpToFirstConflictAction)
         {
         var State = this;

         // TOCHECK Should more information to the conflicts be written into the log?
         // TOCHECK Might there be states - after solving conflicts -  without path to the halt action
         //         and might solving conflicts lead to endless loops?

         var subsetOfConflictSymbols = new BitArray(ConflictSymbols);

         // TOCHECK Should more information about the conflicts be written into the log?
         // TOCHECK Might there be states - after solving conflicts -  without path to the halt action
         //         and might solving conflicts lead to endless loops?

         /* -------------- */
         int indexOfActionWithPriority = SolveoOneSubsetOfDynamicConflicts(subsetOfConflictSymbols);
         /* -------------- */

         // The subsetOfConflictSymbols cause the conflict
         // These symbols have to be removed from all participating actions exept 
         // the one with the highest priority and those with dynamic priority

         sb.AppendLine()
           .Append("Conflict in state ")
           .Append(State.IdNumber + 1)
           .Append(" when Symbol == ");
         subsetOfConflictSymbols.BitsToStringbuilder(
             sb, GlobalVariables.TerminalSymbolByIndex, " | ", "all terminal symbols", "no terminal symbols")
           .AppendLine("; ");
         State.CoreItems.ToStringbuilderMitZusatzinfo(sb)
           .AppendLine();

         // da Aktion geändert wird, kann hier (und in den umgebenden Schleifen) nicht 
         // "foreach (cAktion Aktion in Zustand.Aktionen)" verwendet werden.
         // In der expliziten Schleife steht der Index zur Verfügung.

         // For each action in the parser state (ignoring actions with dynamic priority)
         // determine action symbols and priority and determine the action with the highest priority
         for (Int32 IndexOfAction = 0; IndexOfAction < State.Actions.Count; IndexOfAction++)
            {
            ParserAction action = State.Actions[IndexOfAction];
            BitArray symbolsOfThisAction;

            if (!(action is ConditionalAction conditionalAction))
               continue;

            symbolsOfThisAction = conditionalAction.TerminalSymbols;

            if (symbolsOfThisAction == null)
               continue;

            sb.AppendLine(" solved at runtime by dynamic priority");
            if (IndexOfAction == indexOfActionWithPriority)
               {
               sb.Append("  priority ")
                 //.Append(priority);// sb.Append(maximalePriorität)
                 .Append(" => not modified: ");
               action.ToStringbuilder(sb)
                 .AppendLine();
               }
            else
               {
               var weiterEingeengteKonfliktsymbole =
               (BitArray)(new BitArray(subsetOfConflictSymbols)).And(symbolsOfThisAction);

               if (!weiterEingeengteKonfliktsymbole.Empty())
                  {
                  symbolsOfThisAction.ExceptWith(subsetOfConflictSymbols);
                  // Protokollieren:
                  sb.Append("  priority ")
                    //.Append(priority) .Append(maximalePriorität);
                    .Append(" => modified to: ");
                  action.ToStringbuilder(sb)
                    .AppendLine();

                  // Prüfen: was ist die richtige Stelle, um Konflikte zu erkennen und / bzw. zu lösen ???

                  // Die folgenden Zeilen waren zeitweise auskommentiert, damit die Aktion  in der Zustandsliste angezeigt wird.
                  // Das führt zu nicht erreichbaren Anweisungen im generierten code und zu "IF (true)..."

                  // Aktionen mit leerer Menge terminaler Symbole löschen (nicht mehr erreichbar)
                  // Das kann dazu führen, dass weitere Aktionen nicht mehr erreichbar sind! ????
                  // TODO Check   no longer generated DeletedAction  - any consequence? How is DeletedAction handled in further steps ???
                  if (symbolsOfThisAction.Empty())
                     {
                     sb.AppendLine("This action has been deleted because no input symbols remained after conflict resolution.");

                     State.Actions.RemoveAt(IndexOfAction);
                     IndexOfAction--;

                     //State.Actions[IndexOfAction] = new DeletedParserAction(conditionalAction.NextAction);
                     }
                  }
               }
            } // for (int IndexOfAction ...

         if (indexOfActionWithPriority < IndexOfFirstConflictAction)
            {
            return;
            }
         // die Konfliktsymbole von den erlaubten Symbolen abziehen
         AllowedSymbolsUpToFirstConflictAction.And(subsetOfConflictSymbols.Not());
         }

      /// <summary>
      /// has to bee implemented for dynamic prioritites !!!
      /// </summary>
      /// <param name="subsetOfConflictSymbols"></param>
      /// <returns>Index of action with priority</returns>
      private Int32 SolveoOneSubsetOfDynamicConflicts(BitArray subsetOfConflictSymbols)
         { // TODO implement this for dynamic priorities
           // Alle Aktionen auf Konflikte mit den vorhergehenden Aktionen überprüfen
         var State = this;
         var thisActionsConflictSymbols = new BitArray(subsetOfConflictSymbols.Count);
         Int32 indexOfActionWithPriority = 0;

         // ------------------ Test ---------------------
#pragma warning disable IDE0059 // Der Wert, der dem Symbol zugeordnet ist, wird niemals verwendet.
         var prioritySelectAction = new PrioritySelectAction(thisActionsConflictSymbols, new ListOfParserActions(10));
#pragma warning restore IDE0059 // Der Wert, der dem Symbol zugeordnet ist, wird niemals verwendet.
         Debug.Fail("");

         Int32 highestPriority = Int32.MinValue;

         for (Int32 IndexOfAction = 0; IndexOfAction < State.Actions.Count; IndexOfAction++)
            {
            ParserAction action = State.Actions[IndexOfAction];

            // For each action in the parser state (ignoring actions with dynamic priority)
            // determine action symbols and priority and determine the action with the highest priority
            Int32 priority = 0;
            BitArray symbolsOfThisAction;

            switch (action)
               {
            case TerminalTransition ActionAsTerminalTransition:
                  {
                  symbolsOfThisAction = ActionAsTerminalTransition.TerminalSymbols;
                  break;
                  }

            case LookaheadAction LookAheadAction:
                  {
                  symbolsOfThisAction = LookAheadAction.TerminalSymbols;
                  if (LookAheadAction.PriorityFunction != null)
                     continue;
                  priority = LookAheadAction.ConstantPriority;
                  break;
                  }

            default: // 
               continue; // no symbolsOfThisAction 
               }

            // found action with symbolsOfThisAction
            if (symbolsOfThisAction == null)
               continue;

            // Does this action have at least one symbol in common with the conflicting action?
            thisActionsConflictSymbols.Assign(subsetOfConflictSymbols).And(symbolsOfThisAction);

            if (thisActionsConflictSymbols.Empty())
               continue; // no, it hasn't

            // Yes, it does
            // Reduce the set of symbols, handled in this call of the method,
            // to the symbols this action and the preceding conflicting actions have in common
            subsetOfConflictSymbols.Assign(thisActionsConflictSymbols);

            // Bookmark the action with the highest priority.
            // If two actions have the same priortiy, give TerminalTransition higher priority than LookAhead.
            if (priority > highestPriority
                || (priority == highestPriority
                    && action is TerminalTransition))
               {
               highestPriority = priority;
               indexOfActionWithPriority = IndexOfAction;
               }
            }
         return indexOfActionWithPriority;
         }

      /// <summary>
      /// Check if there are terminal symbols not causing any action of the state
      /// and if this is the case add an error action.
      /// </summary>
      /// <returns>The added action or null if none added</returns>
      public ErrorhandlingAction CheckAndAddErrorAction(Boolean ErrorHandlerIsDefined)
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
             Folgesymbole: notAllowedSymbols,
             idNumber: this.IdNumber, // use the IdNumber of the ParserState as IdNumber of the ErrorHandlingAction
             state: this
             );
         Actions.Add(e);

         ContainsErrorHandlerCall = ErrorHandlerIsDefined;
         return e;
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
      internal Symbol InputSymbol;

      /// <summary>
      /// Constructor returns an new item with definition, elementNr
      /// and InputSymbol==definition.Elements[elementNr] or InputSymbol==null for enditem
      /// </summary>
      /// <param name="definition"></param>
      /// <param name="elementNr"></param>
      internal ItemStruct(Definition definition, Int32 elementNr)
         {
         this.SymbolDefinition = definition;
         this.ElementNr = elementNr;
         this.InputSymbol = elementNr < definition.Elements.Length
             ? definition.Elements[elementNr] : null;
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
      public override Int32 GetHashCode() => SymbolDefinition.GetHashCode() ^ ElementNr ^ InputSymbol.GetHashCode();

      // Überladen von Equals und der Operatoren   == und != für sItem
      /// <summary>
      ///  compares xxx.SymbolDefinition and then xxx.ElementNr
      /// </summary>
      /// <param name="obj"></param>
      /// <returns>true if equal</returns>
      public override Boolean Equals(Object obj) =>
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
         if (this.SymbolDefinition.DefinedSymbol.SymbolNumber < other.SymbolDefinition.DefinedSymbol.SymbolNumber)
            return -1;
         if (this.SymbolDefinition.DefinedSymbol.SymbolNumber > other.SymbolDefinition.DefinedSymbol.SymbolNumber)
            return +1;
         if (this.SymbolDefinition.IdNumber < other.SymbolDefinition.IdNumber)
            return -1;
         if (this.SymbolDefinition.IdNumber > other.SymbolDefinition.IdNumber)
            return +1;
         if (this.ElementNr < other.ElementNr)
            return -1;
         if (this.ElementNr > other.ElementNr)
            return +1;
         Debug.Assert(this.SymbolDefinition.DefinedSymbol.SymbolNumber == other.SymbolDefinition.DefinedSymbol.SymbolNumber);
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

         if (this.SymbolDefinition.DefinedSymbol.SymbolNumber < item.SymbolDefinition.DefinedSymbol.SymbolNumber)
            return -1;
         if (this.SymbolDefinition.DefinedSymbol.SymbolNumber > item.SymbolDefinition.DefinedSymbol.SymbolNumber)
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
            d.DefinedSymbol.IdentifierAndAttributesToSB(sb).Append("= ");
            d.ElementsToStringbuilder(sb, Item.ElementNr);
            }
         }

      internal StringBuilder ToStringbuilderMitZusatzinfo(StringBuilder sb)
         {
         foreach (ItemStruct Item in this)
            {
            Definition d = Item.SymbolDefinition;
            d.DefinedSymbol.IdentifierAndAttributesToSB(sb).Append("= ");
            d.ToStringbuilder(sb, Item.ElementNr);
            sb.AppendLine();
            }
         return sb;
         }
      }
   } // namespace
