﻿using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Grammlator {
   /// <summary>
   /// see <see cref="MakeInstanceAndExecute"/>
   /// </summary>
   public sealed class P2ComputeLR0States {
      /// <summary>
      /// Phase2 computes the LR(0) states g.ListOfAllStates and their predecessor relation
      /// <para>The LR(0) states have associated lists of actions:
      /// terminal transitions (shift-reduce), nonterminal transitions (shift-reduce) and
      /// look ahead actions (reduce). </para>
      /// <para>State 0 has one or more extra associated actions:
      ///   "if startsymbol: NonTerminalTransition "</para>
      /// <para> Look ahead terminal symbols are computed in a later phase. </para>
      /// </summary>
      internal static void MakeInstanceAndExecute()
         {
         var p2 = new P2ComputeLR0States();
         p2.ComputeLR0StatesAndActions();
         ComputePredecessorRelation();
         }

      // private readonly Action<MessageTypeOrDestinationEnum, string> OutputMessage;

      /// <summary>
      /// Private constructor (external calls via Phase2.Execute(...))
      /// </summary>
      private P2ComputeLR0States()
         {
         // this.OutputMessage = outputMessage;
         }

      private readonly ListOfParserActions ActionsOfActualState = new ListOfParserActions(100);

      /// <summary>
      /// Sum of the number of actions of all parser states.
      /// </summary>
      private Int32 NumberOfActions = 0;

      private readonly BitArray EmptyLookAheadSet = new BitArray(GlobalVariables.NumberOfTerminalSymbols);

      private void ComputeLR0StatesAndActions()
         {
         var ListOfNotAnEnditemLOfActualState = new ItemList();

         var DoNotProcessNonterminalSymbolAgain = new Boolean[GlobalVariables.NumberOfNonterminalSymbols];

         // Construct the first state (the core items) in G.ListOfAllStates, using temporary storage ListOfNotAnEnditemLOfActualState
         ConstructFirstState(ListOfNotAnEnditemLOfActualState);

         // For all states of the growing ListOfAllStates compute
         // * terminal and nonterminal transitions with follow states
         // * look ahead actions

         for (Int32 IndexOfActualState = 0; IndexOfActualState < GlobalVariables.ListOfAllStates.Count; IndexOfActualState++)
            {
            ParserState ActualState = GlobalVariables.ListOfAllStates[IndexOfActualState];

            ListOfNotAnEnditemLOfActualState.Clear();
            ActionsOfActualState.Clear();

            /* Special Case: In the first state insert a nonterminal transitions caused by the startsymbol with null next state  
             * This is will be transformed to a halt action
             */

            if (IndexOfActualState == 0)
               {
               ActionsOfActualState.Add(
                  new NonterminalTransition(
                      NumberOfActions++, GlobalVariables.Startsymbol, GlobalVariables.ListOfAllHaltActions[0], EmptyLookAheadSet)
                      );
               }

            // Process each of the core items of the actual state:
            //    if it is an enditem then add a reduce action to the actual states actions
            //    esl copy it to ListOfNotAnEnditemOfActualState
            foreach (ItemStruct CoreItem in ActualState.CoreItems)
               {
               if (CoreItem.ElementNr >= CoreItem.SymbolDefinition.Elements.Length) // enditem: add reduce action to ActionsOfActualState
                  {
                  ActionsOfActualState.Add(
                     new LookaheadAction(number: NumberOfActions++, definition: CoreItem.SymbolDefinition,
                     EmptyLookAheadSet, GlobalVariables.ListOfAllHaltActions[0]
                     )
                     );
                  }
               else
                  {   // not an enditem: copy item and add marked symbol as input symbol of this item
                  ListOfNotAnEnditemLOfActualState.Add(CoreItem);
                  }
               }

            // CoreItems (and for now AllItemsOfActualState) either contains the startitems of the definitions of the startsymbol
            //           which can never occur as input symbol
            // or no startitems but only items which result by shift

            // for all items where the marked symbol is nonterminal
            //    for all not empty not trivial definitions of this symbol
            //       add the startitem to AllItemsOfActualState
            //    for all empty definitions of this symbol
            //       add (the definition interpreted as) a reduce action to ActionsOfActualState
            //    for all trivial definitions
            //       handle their definitions (=symbols) alike marked symbols recursively

            Array.Clear(DoNotProcessNonterminalSymbolAgain,
                 0, DoNotProcessNonterminalSymbolAgain.Length);

            for (Int32 IndexOfItem = 0; IndexOfItem < ListOfNotAnEnditemLOfActualState.Count; IndexOfItem++)
               {
               Symbol InputSymbol = ListOfNotAnEnditemLOfActualState[IndexOfItem].InputSymbol;
               if (InputSymbol is NonterminalSymbol nonterminal)
                  AddStartitemsAndLookaheadActions(nonterminal, ListOfNotAnEnditemLOfActualState, DoNotProcessNonterminalSymbolAgain); // ergänzt Arbeitsitems und ItemEingabesymbol
               }

            // If in an item a nonterminal symbol is marked this item has to be copied
            // for each trivial definition of the nonterminal symbol (recursively)
            // with item.InputSymbol set to the trivial definition (a symbol) of the nonterminal.

            Array.Clear(DoNotProcessNonterminalSymbolAgain,
                0, DoNotProcessNonterminalSymbolAgain.Length);

            for (Int32 IndexOfItem = ListOfNotAnEnditemLOfActualState.Count - 1; IndexOfItem >= 0; IndexOfItem--)
               {
               // going backwards because new entries are handled by recursion 

               ItemStruct Item = ListOfNotAnEnditemLOfActualState[IndexOfItem];
               if (Item.InputSymbol is NonterminalSymbol nonterminal && !DoNotProcessNonterminalSymbolAgain[nonterminal.SymbolNumber])
                  DuplicateItemForTrivialDefinitions(nonterminal, Item, ListOfNotAnEnditemLOfActualState, DoNotProcessNonterminalSymbolAgain);
               }

            ComputeFollowStatesAndTransitions(ListOfNotAnEnditemLOfActualState, IndexOfActualState);

            // Sort the actions of the actual state
            ActionsOfActualState.Sort((Aktion1, Aktion2) => Aktion1.CompareTo(Aktion2));

            // Copy the actions to the state
            ActualState.Actions = new ListOfParserActions(ActionsOfActualState);
            } // end of for (int IndexOfActualState= ...

         // Renumber the states according to their position in ListOfAllStates
         // This is required later.
         Int32 NewNumber = 0;
         foreach (ParserState state in GlobalVariables.ListOfAllStates)
            {
            state.IdNumber = NewNumber; // >= 0
            state.StateStackNumber = NewNumber; // >= 0
            NewNumber++;
            }
         } // end of ComputeLR0StatesAndActions()

      private void ComputeFollowStatesAndTransitions(ItemList ListOfNotAnEnditemOfActualState, Int32 IndexOfActualState)
         {
         // For all InputSymbols occuring in AllItemsOfActualState
         //    create a new follow state and
         //    for all Items in AllItemsOfActualState with this InputSymbol
         //       add the follow item to the follow state
         //    except if the follow state contains only one item which is an end item
         //       then delete the follow state
         //       and create a SHIFT-REDUCE-Action (transition action with definition as next action) 
         // A symbol with trivial definitions thereby represents itself and all its trivial definitons (recursively)

         // The list "ItemsOfNewState"  is used to compute the follow states and the transitions into those states.
         // It contains no enditems.
         var ItemsOfNewState = new ItemList();  // dient dem Aufbau einer Folgeitem-Liste pro Ausgangszustand

         // For the following steps the items must be sorted by their input symbol.
         // Later in ItemsOfNewState.Sort a different order may be used.
         ListOfNotAnEnditemOfActualState.Sort((ItemStruct x, ItemStruct y) => x.CompareInputSymbols(y));

         Int32 ItemIndex = 0;
         while (ItemIndex < ListOfNotAnEnditemOfActualState.Count)
            {
            // ItemIndex will be incremented inside an inner loop so that at each iteration of this outer loop
            // another nonterminal symbol will be assigned to InputSymbolToEvaluate

            Symbol InputSymbolToEvaluate = ListOfNotAnEnditemOfActualState[ItemIndex].InputSymbol;

            // ignore each item which has as InputSymbol a nonterminal symbol with only trivial alternatives
            if (InputSymbolToEvaluate.IsNonterminalWhichHasOnlyTrivialAlternatives)
               {
               ItemIndex++;
               continue;
               }

            // moved to end of phase 4
            //AssignPrefixToStateAndDoSomeChecks(ActualState, InputSymbolToEvaluate);

            // InputSymbolToEvaluate is a terminal symbol or
            // a nonterminals symbol which has at least one nontrivial definition

            // Because ListOfNotAnEnditemOfActualState is sorted by ... .InputSymbol
            // all items with the same input symbol can be processed in the following loop
            // which stores the respective follow items in ItemsOfNewState

            ItemsOfNewState.Clear();

            do
               {
               // process all items with the same IputSymbol
               Debug.Assert(!ListOfNotAnEnditemOfActualState[ItemIndex].IsEnditem); // because the item has an input symbol

               ItemStruct ItemWithAdvancedMarker =
                   ListOfNotAnEnditemOfActualState[ItemIndex].NewItemWithAdvancedMarker();

               ItemsOfNewState.Add(ItemWithAdvancedMarker);
               ItemIndex++;

               } while
                (ItemIndex < ListOfNotAnEnditemOfActualState.Count
                   && ListOfNotAnEnditemOfActualState[ItemIndex].InputSymbol == InputSymbolToEvaluate);

            // Sort the items of the new state to get an unambiguous representation, which can be used for searching
            ItemsOfNewState.Sort((Item1, Item2) => Item1.CompareTo(Item2));

            //  special handling of new states with a single item which is an enditem
            //  do not create a new state (clear state) but
            //  create a SHIFT-REDUCE-Action == transition with a definition as next action
            if (ItemsOfNewState.Count == 1)
               {
               ItemStruct NewStatesItem = ItemsOfNewState[0];

               if (NewStatesItem.ElementNr >= NewStatesItem.SymbolDefinition.Elements.Length)
                  {
                  // it is a enditem
                  AddTransitionToActionsOfActualState(InputSymbolToEvaluate, nextAction: NewStatesItem.SymbolDefinition);
                  ItemsOfNewState.Clear();
                  }
               }

            // else find or create the new state and add the corresponding transition to the actions of the actual state
            if (ItemsOfNewState.Count >= 1)
               {
               // In the list of all already created states search a state with the same items
               ParserState? newState = null;
               foreach (ParserState existingState in GlobalVariables.ListOfAllStates)
                  {
                  if (existingState.CoreItems.IsEqualTo(ItemsOfNewState))
                     {
                     newState = existingState;
                     break; // Ende der Suche
                     }
                  }

               // if not found create a new state with these items and 
               // add it somewhere after the actual state to the list of all states, so that it will be processed.
               // Here the new state is added after the actual state.
               if (newState == null)
                  {
                  newState =
                      new ParserState(Number: GlobalVariables.ListOfAllStates.Count, Items: new ItemList(ItemsOfNewState));
                  GlobalVariables.ListOfAllStates.Insert(IndexOfActualState + 1, newState);
                  }

               // Add the transition from the actuial state to the new or found state
               AddTransitionToActionsOfActualState(inputSymbol: InputSymbolToEvaluate, nextAction: newState);
               }
            }
         }

      private void AddTransitionToActionsOfActualState(Symbol inputSymbol, ParserAction nextAction)
         {
         if (inputSymbol is NonterminalSymbol nonterminalInputSymbol)
            {
            // (Eingabesymbol ist cNichtterminalesSymbol) => nichtterminaler Übergang
            /*   -- eine Aktion mit dem gleichen nichtterminalen Eingabesymbol
                 -- kann es im aktuellen Zustand noch nicht geben - also eintragen
                 -- sofern das Symbol mindestens eine Alternative hat}
            */
            if (nonterminalInputSymbol.NontrivialDefinitionsList.Count > 0)
               {
               ActionsOfActualState.Add(
                   new NonterminalTransition(NumberOfActions++,
                       nonterminalInputSymbol, nextAction, EmptyLookAheadSet)
                   );
               }
            }
         else
            {
            // In der Aktionenliste des aktuell bearbeiteten Zustands
            // eine cAktionTerminalerÜbergang anlegen, falls nicht vorhanden

            // Den passenden terminalen Übergang in der Aktionenliste suchen
            TerminalTransition existingTransition =
                ActionsOfActualState
                .OfType<TerminalTransition>()
                .FirstOrDefault(transition => transition.NextAction == nextAction);

            if (existingTransition == null)
               {
               // falls nicht gefunden: neu anlegen                    
               var inputSymbols = new BitArray(GlobalVariables.NumberOfTerminalSymbols);
               inputSymbols[inputSymbol.SymbolNumber] = true;
               ActionsOfActualState.Add(
                   new TerminalTransition(NumberOfActions++, inputSymbols, nextAction));
               }
            else
               {
               existingTransition.TerminalSymbols
                   [inputSymbol.SymbolNumber] = true;
               }
            }
         }

      /// <summary>
      /// Recursively copy the item for all the trivial definitions (symbols) of the marked symbol
      /// and set Item.InputSymbol of the copy to the respective symbol. 
      /// </summary>
      /// <param name="MarkedSymbol"></param>
      /// <param name="Item"></param>
      /// <param name="ListOfNotAnEnditemLOfActualState"></param>
      /// <param name="doNotProcessNonterminalSymbolAgain"></param>
      private void DuplicateItemForTrivialDefinitions(
              NonterminalSymbol MarkedSymbol, ItemStruct Item, ItemList ListOfNotAnEnditemLOfActualState,
              Boolean[] doNotProcessNonterminalSymbolAgain)
         {
         Debug.Assert(!doNotProcessNonterminalSymbolAgain[MarkedSymbol.SymbolNumber]);

         doNotProcessNonterminalSymbolAgain[MarkedSymbol.SymbolNumber] = true;

         // duplicate item, using each trivial definition of the marked symbol as InputSymbol of the duplicated item
         foreach (Symbol trivialDefinition in MarkedSymbol.TrivalDefinitionsArray)
            {
            Item.InputSymbol = trivialDefinition; // ok because item is a struct and not a class

            if (trivialDefinition is NonterminalSymbol nonterminalReplacement)
               {
               if (!doNotProcessNonterminalSymbolAgain[nonterminalReplacement.SymbolNumber])
                  {
                  ListOfNotAnEnditemLOfActualState.Add(Item);
                  DuplicateItemForTrivialDefinitions(nonterminalReplacement, Item, ListOfNotAnEnditemLOfActualState, doNotProcessNonterminalSymbolAgain);
                  }
               }
            else
               {
               ListOfNotAnEnditemLOfActualState.Add(Item);
               }
            }

         doNotProcessNonterminalSymbolAgain[MarkedSymbol.SymbolNumber] = false;
         }

      /// <summary>
      /// adds all definitions of markedSymbol with ElementNr 0 as items to  <paramref name="itemsOfActualState"/> 
      /// or (for empty definitions) adds a lookadead-reduce action to <see cref="ActionsOfActualState"/>
      /// </summary>
      /// <param name="markedSymbol"></param>
      /// <param name="itemsOfActualState"></param>
      /// <param name="doNotProcessNonterminalSymbolAgain"></param>
      private void AddStartitemsAndLookaheadActions(NonterminalSymbol markedSymbol, ItemList itemsOfActualState, Boolean[] doNotProcessNonterminalSymbolAgain)
         {
         Debug.Assert(markedSymbol != null);
         if (markedSymbol == null || doNotProcessNonterminalSymbolAgain[markedSymbol.SymbolNumber])
            return; // Das Symbol wurde bereits bearbeitet

         doNotProcessNonterminalSymbolAgain[markedSymbol.SymbolNumber] = true;

         // for all definitions of the marked symbol
         //    if empty definition: add look ahead action (apply definiton);
         //    else                 add start item "definition, 0"
         foreach (Definition Definition in markedSymbol.NontrivialDefinitionsList)
            {
            if (Definition.Elements.Length == 0)
               {
               ActionsOfActualState.Add(
                  new LookaheadAction(
                        number: NumberOfActions++,
                        definition: Definition,
                        EmptyLookAheadSet, GlobalVariables.ListOfAllHaltActions[0]
                        )
                  );
               }
            else
               {
               itemsOfActualState.Add(new ItemStruct(Definition));
               }
            }

         // process all trivial definitions of the nonterminal with a nonterminal recursively
         foreach (Symbol trivialDefinition in markedSymbol.TrivalDefinitionsArray)
            {
            if (trivialDefinition is NonterminalSymbol nonterminalDecendant)
               AddStartitemsAndLookaheadActions(nonterminalDecendant, itemsOfActualState, doNotProcessNonterminalSymbolAgain);
            }
         }


      /// <summary>
      /// Construct state 1 using all alternatives of the startsymbol as startitems 
      /// and store it as the first element of the ListOfAllStates
      /// </summary>
      /// <param name="ItemsOfActualState">temporary storage for items</param>
      private static void ConstructFirstState(ItemList ItemsOfActualState)
         {
         ItemsOfActualState.Clear();
         for (Int32 i = 0; i < GlobalVariables.Startsymbol.NontrivialDefinitionsList.Count; i++)
            {
            var definition = GlobalVariables.Startsymbol.NontrivialDefinitionsList[i];
            ItemsOfActualState.Add(new ItemStruct(definition));
            }

         // Note: the states will be renumbered later, new ParserState copies(!) the items
         GlobalVariables.ListOfAllStates.Add
             (
             new ParserState(
                 Number: GlobalVariables.ListOfAllStates.Count,
                 Items: ItemsOfActualState)
             );
         }

      private static void ComputePredecessorRelation()
         {
         var NumberOfPredecessors = new Int32[GlobalVariables.ListOfAllStates.Count];  // Indizierung mit Zustandsnummern >=0

         // The following declarations and methods are provided to mark states as processed.
         // All processed states are chained together with the anchor LastProcessed.

         var isProcessedChain = new Int32[GlobalVariables.ListOfAllStates.Count];
         Int32 LastProcessed = -1;
         Boolean IsProcessed(Int32 s) => isProcessedChain[s] >= 0;
         void MarkAsProcessed(Int32 s)
            {
            Debug.Assert(isProcessedChain[s] < 0);
            isProcessedChain[s] = LastProcessed;
            LastProcessed = s;
            }
         void ResetIsProcessed()
            { // unwind the chain and set all entries to -1
            while (LastProcessed >= 0)
               {
               Int32 s = LastProcessed;
               LastProcessed = isProcessedChain[s];
               isProcessedChain[s] = -1;
               }
            }
         void InitializeIsProcessed()
            {
            for (Int32 i = 0; i < isProcessedChain.Length; i++)
               isProcessedChain[i] = -1;
            }

         // A) Determine the number of predecessor states for each state.

         InitializeIsProcessed();
         //    For each state
         foreach (ParserState state in GlobalVariables.ListOfAllStates)
            {
            // for each successor state, which has not yet been processed for the state,
            // increment the NumberOfPredecessors and mark the successor state as processed

            foreach (Int32 numberOfSuccessorState in state.Actions.
                OfType<ParserActionWithNextAction>().Select(a => a.NextAction).
                OfType<ParserState>().Select(s => s.IdNumber).
                Where(IDNummer => !IsProcessed(IDNummer))
                )
               {
               NumberOfPredecessors[numberOfSuccessorState]++;
               MarkAsProcessed(numberOfSuccessorState);
               }

            ResetIsProcessed();
            }

         // B) Assign each state its list of predecessors
         foreach (ParserState state in GlobalVariables.ListOfAllStates)
            state.PredecessorList = new List<ParserState>(NumberOfPredecessors[state.IdNumber]);

         // C) For each state enter it into the list of predecessors of its successor
         foreach (ParserState state in GlobalVariables.ListOfAllStates)
            {
            foreach (ParserState successorState in state.Actions.
                    OfType<ParserActionWithNextAction>().Select(a => a.NextAction).
                    OfType<ParserState>()
                    )
               {
               if (!IsProcessed(successorState.IdNumber))
                  {
                  successorState.PredecessorList.Add(state);
                  MarkAsProcessed(successorState.IdNumber);
                  }
               }

            ResetIsProcessed();
            }
         } // end of ComputePredecessorRelation
      } //end of class Phase2 
   } // end of namespace
