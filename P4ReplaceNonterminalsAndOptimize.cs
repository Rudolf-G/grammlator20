using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Grammlator {
   internal class P4ReplaceNonterminalsAndOptimize {
      /* Phase 4 analyzes which actions would cause "apply definition" 
       *    (execute method and go back to a state in the state stack, input 
       *     the generated nonterminal symbol and "shift" to the follow state)
       * and replaces each one by an explicit "reduction" which adjusts the state stack
       * and may be followed by a "branch" which uses the value on top
       * of the state stack to decide which is the follow state.
       * It recognizes states which need not to push their number on
       * the state stack (assigns the number -1), 
       * tries to assign the same (small) number to different states,
       * tries to eliminate trivial branches, shorten chains of trivial actions.
       * It adds error actions.
       * When this is finished, nonterminal symbols and their definitions
       * are not longer used.
       * 
       * Not yet implemented: find states which can store their 
       * return information in a local variable
       * instead pushing it on the stack. 
       * 
       * */

      // TODO implement dynamic priorities
      // TODO some more ideas how to optimize
      /* - when shortening chains:
       * -- combine branches  b1 und b2 if b1 is contained in b2
       * -- Also shorten
       *    if x.symbol==point then goto s4;...s4: x.FetchSymbol; if x.symbol==point then ..
       * 
       * */

      public static void MakeInstanceAndExecute()
      {
         var p4 = new P4ReplaceNonterminalsAndOptimize();

         p4.P4aComputeStatesStackNumbersAndBranches();
         P4bShortenChainsOfActions();
         p4.P4cRemoveNotLongerUsedActionsFromStatesAddErrorActionsComputeActionFields();
      }

      /// <summary>
      /// Constructor
      /// </summary>
      private P4ReplaceNonterminalsAndOptimize()
      {
         SamePredecessorLevelPartitionOfStates
            = new PartitionInfoArray<ParserState>(GlobalVariables.ListOfAllStates);
         SimilarStatesRelation
             = new SymmetricRelation<ParserState>(GlobalVariables.ListOfAllStates.Count);
         DistinguishableStatesRelation
             = new SymmetricRelation<ParserState>(GlobalVariables.ListOfAllStates.Count);
      }

      private readonly SymmetricRelation<ParserState> SimilarStatesRelation;
      private readonly SymmetricRelation<ParserState> DistinguishableStatesRelation;

      /// <summary>
      /// All states belonging to the same class of <see cref="SamePredecessorLevelPartitionOfStates"/>
      /// either all do not push to the stack of states in the generated code or all push
      /// </summary>
      private readonly PartitionInfoArray<ParserState> SamePredecessorLevelPartitionOfStates;

      public enum RelationtypeEnum {
         ComputeDistinguishableStatesRelation,
         ComputeSimilarStatesRelation,
         ComputeBranches
      };

      public void P4aComputeStatesStackNumbersAndBranches()
      {
         // TODO Translate comment
         /*  Für Zustände, die keine Information kellern, wird die Kennung auf -1 gesetzt.
         Alle Zustaende der gleichen Klasse bezüglich der Relation Nachbar
         sind miteinander verkettet.
         Für jeden Zustand gibt es eine Kette zu unterscheidender Zustände und
         eine Kette aehnlicher Zustaende, das sind Zustände, welche die gleiche
         Kennung erhalten sollten. 

         Eine Relation z1 R z2 wird auch (frühere Version "nur") eingetragen, wenn z2.Nummer < z1.Nummer .
         Früher: Dies genügt wegen der Symmetrie dieser Relationen.
         Jetzt: dies ist notwendig, damit die Relationen ohne komplizierte Randbedingungen zu Enumerationen verwendet werden können
         */

         ComputeSamePredecessorLevelPartitionOfStates();
         DistinguishableStatesRelation.Clear();
         EvaluatePrecedingStates(RelationtypeEnum.ComputeDistinguishableStatesRelation);

         //// TODO FolgeaktionenPaarweise ... ändert AnzahlVerschieden!!!!
         ////// TODO allow suppression of both optimizations DetermineNotStackingStates and AssignStatesSmallStackNumbers

         GlobalVariables.CountOfStatesWithStateStackNumber =
             GlobalVariables.ListOfAllStates.Count
             - DetermineNotStackingStates(); // uses SamePredecessorLevelPartitionOfState and DistinguishableStatesRelation

         /* zuerst die Relation verschiedene_Zustaende berechnen, dann die Relation aehnliche_Zustaende !! (damit sie kleiner bleibt)} ???? */

         EvaluatePrecedingStates(RelationtypeEnum.ComputeSimilarStatesRelation);
         AssignStatesSmallStackNumbers();
         EvaluatePrecedingStates(RelationtypeEnum.ComputeBranches);

         // nicht mehr benötigt werden jetzt: FallListe, aktuelleZustände; Vorgänger

      }

      /// <summary>
      /// Visit all states, take all actions which apply a definition,
      /// and find all predecessor states up to a search depth given by the definitions length.
      /// Assign all states found at the same depth to the same class of the NeighborPartition.
      /// </summary>
      private void ComputeSamePredecessorLevelPartitionOfStates()
      {
         foreach (ParserState state in GlobalVariables.ListOfAllStates)
         {
            foreach (ParserAction action in state.Actions!.PriorityUnwindedSetOfActions)
            {
               if (action is ParserActionWithNextAction actionWithNextAction)
               {
                  if (actionWithNextAction.NextAction is Definition definition)
                  {
                     WalkBackAndCombineSamePredecessorLevelClasses(
                         StateToStartFrom: state,
                         Depth: definition.Elements.Length + ((action is LookaheadAction) ? 0 : -1)
                     );
                     // In terminal and nonterminal shifts the item is one position before the end:
                     // depth must be one less than the length of the definitions elementlist
                  }
                  else if (actionWithNextAction.NextAction is PriorityBranchAction b)
                  {
                     if (b.ConstantPriorityAction != null)
                     {
                        /* may be (look ahead - ) Definition: reduce-reduce-conflict
                         * or (terminalTransition - ) Definition (shiftreduce): shift-reduce-conflict
                         * or (terminalTransition - ) State (shift): shift-reduce-conflict)
                         * accept ... */
                        throw new NotImplementedException("Constant priority action in PrioritBranchAction not yet implemented");
                     }
                     foreach (ParserAction dynamicPriorityAction in b.DynamicPriorityActions)
                     {
                        Debug.Assert(dynamicPriorityAction is Definition);
                        if (dynamicPriorityAction is Definition d)
                           WalkBackAndCombineSamePredecessorLevelClasses(
                               StateToStartFrom: state,
                               Depth: d.Elements.Length);
                     }
                  }
               }
            }
         }
      }

      /// <summary>
      /// Start with <paramref name="StateToStartFrom"/> and walk <paramref name="Depth"/> levels
      /// back to all predecessors combining all states of the same depth into the same class
      /// of <see cref="SamePredecessorLevelPartitionOfStates"/>
      /// </summary>
      /// <param name="StateToStartFrom">state from where to walk back</param>
      /// <param name="Depth">number of levels to walk back</param>
      private void WalkBackAndCombineSamePredecessorLevelClasses(ParserState StateToStartFrom, Int32 Depth)
      {
         // start with StateWithDefinition as single element of the set of states to evaluate 
         var statesOfThisLevel =
             new List<ParserState>(GlobalVariables.ListOfAllStates.Count)
                 { StateToStartFrom };

         var precedingStates = new List<ParserState>(30); // TOCHECK size? move allocation out of the method?

         // Iterate Depth-1 levels
         for (Int32 remainingLength = Depth; remainingLength > 0; remainingLength--)
         {
            // Add all predecessors of all states of this level to the list of preceding states
            foreach (ParserState stateToEvaluate in statesOfThisLevel)
            {
               foreach (ParserState predecessor in stateToEvaluate.PredecessorList!)
                  precedingStates.Add(predecessor);
            }

            Debug.Assert(precedingStates.Count != 0, "Error in phase4: no predecessor)");

            // Choose the partition class of one of the elements
            ParserState representativeOfClass = precedingStates[0];

            // Combine the classes of all predecessors of the this level with this class
            foreach (ParserState predecessor in precedingStates)
               SamePredecessorLevelPartitionOfStates.CombineClasses(representativeOfClass, predecessor);
            //  combining the class of precedingStates[0] with itself again makes code simpler

            // The contents of statesToEvaluate is not longer needed, the lists capacity will be reused
            statesOfThisLevel.Clear();
            List<ParserState> temp = statesOfThisLevel;

            // The preceding states become the states to evaluate
            statesOfThisLevel = precedingStates;

            // As new empty list of preceding states the old cleared list of states to evaluate is reused 
            precedingStates = temp;
         }
      }

      /// <summary>
      /// Visit all states, take all actions which apply a definition,
      /// and find all predecessor states up to a search depth given by the definitions length.
      /// Apply the type of evaluation to the predecessors.
      /// </summary> 
      /// <param name="TypeOfEvaluation"></param>
      private void EvaluatePrecedingStates(RelationtypeEnum TypeOfEvaluation)
      {
         foreach (ParserState state in GlobalVariables.ListOfAllStates)
         {
            foreach (ParserAction action in state.Actions!.PriorityUnwindedSetOfActions)
            {
               if ((action is ParserActionWithNextAction actionWithNextAction)
                   && (actionWithNextAction.NextAction is Definition definition))
               {
                  actionWithNextAction.NextAction =
                      EvaluatePredecessors(
                          TypeOfEvaluation,
                          StateToStartFrom: state,
                          DefinitionToReplace: definition,
                          Depth: definition.Elements.Length + ((action is LookaheadAction) ? 0 : -1)
                      );
                  // In terminal and nonterminal shifts the item is one position before the end:
                  // depth must be one less than the length of the definitions elementlist

                  actionWithNextAction.NextAction =
                      SimplifiedNextAction(actionWithNextAction.NextAction);
               }
            }
         }
      }

      /// <summary>
      /// Start with <paramref name="StateToStartFrom"/> and walk <paramref name="Depth"/> levels
      /// back and do at each or at the last level the type of evaluation
      /// </summary>
      /// <param name="TypeOfEvaluation">defines what to do</param>
      /// <param name="StateToStartFrom">state from where to walk back</param>
      /// <param name="DefinitionToReplace">Definition to replace</param>
      /// <param name="Depth">number of levels to walk back</param>
      /// <returns>if <paramref name="TypeOfEvaluation"/> is <see cref="RelationtypeEnum.ComputeBranches"/>
      /// returns a <see cref="BranchAction"/> or <see cref="ReduceAction"/> otherwise <paramref name="DefinitionToReplace"/> 
      ///  </returns>
      private ParserAction EvaluatePredecessors(
          RelationtypeEnum TypeOfEvaluation,
          ParserState StateToStartFrom,
          Definition DefinitionToReplace,
          Int32 Depth)
      {
         ParserAction result = DefinitionToReplace; // Voreinstellung: gleiche (also keine geänderte Aktion) zurückgeben

         var statesOfThisLevel =
             new List<ParserState>(GlobalVariables.ListOfAllStates.Count)
                 { StateToStartFrom };

         var precedingStates = new List<ParserState>();

         Int32 countOfStatemarkersToPop = 0; // used only if RelationtypeEnum.ComputeBranches

         // Stufenweise alle Vorgänger des Zustands bestimmen und dabei die Nachbarrelation und die Kellertiefe berechnen            
         for (Int32 remainingDepth = Depth; remainingDepth > 0; remainingDepth--)
         {
            // Die Vorgänger aller Zustande der aktuellen Zustandsliste in die Vorgängerliste aufnehmen
            foreach (ParserState state in statesOfThisLevel)
            {
               foreach (ParserState predecessor in state.PredecessorList!)
               {
                  if (!precedingStates.Contains(predecessor))
                     precedingStates.Add(predecessor);
               }
            }

            Debug.Assert(precedingStates.Count != 0, "Error in phase4: no predecessor)");

            // Either the StateStackNumber of all predecessors is >=0 or of all is <0
            // States with <=0 won't push and are not counted 
            if (TypeOfEvaluation == RelationtypeEnum.ComputeBranches
                && statesOfThisLevel[0].StateStackNumber >= 0)
            {
               countOfStatemarkersToPop++;
            }

            // The contents of statesToEvaluate is no longer needed, the lists capacity will be reused
            statesOfThisLevel.Clear();
            List<ParserState> temp = statesOfThisLevel;

            // The preceding states become the states to evaluate
            statesOfThisLevel = precedingStates;

            // As new empty list of preceding states the old cleared list of states to evaluate is reused 
            precedingStates = temp;
         }

         /*  Die durch die gegebene Alternative aus dem gegebenen Zustand erreichbaren Zustände,
          *  die das Startitem der anzuwendenden Alternative enthalten, sind gefunden.

          *  Falls die Folgeaktion eindeutig ist, ist sie statt der Alternativen einzusetzen.
          *    Voraussetzung: Kellertiefe_Zaehler = 0
          *  Sonst kann sich die Kellertiefe noch aendern, bis die Verzweigungen berechnet werden */

         NonterminalSymbol definedSymbol = DefinitionToReplace.DefinedSymbol!;

         switch (TypeOfEvaluation)
         {
         case RelationtypeEnum.ComputeDistinguishableStatesRelation:
         case RelationtypeEnum.ComputeSimilarStatesRelation:
            FolgeaktionenPaarweiseVergleichen(TypeOfEvaluation, statesOfThisLevel, definedSymbol);
            return result; // dummy result

         case RelationtypeEnum.ComputeBranches:
            BranchcasesList ListOfBranchcases = ConstructListOfBranchcases(statesOfThisLevel, definedSymbol);
            ParserAction NextAction =
                    ListOfBranchcases.Count == 1
                    ? ListOfBranchcases[0].BranchcaseAction
                    : FindOrConstructBranch(ListOfBranchcases);

            if (DefinitionToReplace.HasNoSemantics() && (countOfStatemarkersToPop == 0)
                && DefinitionToReplace.DefinedSymbol != GlobalVariables.Startsymbol)
            {
               result = NextAction;
            }
            else
               result = MakeReduceAction(
                   countOfStatemarkersToPop,
                   DefinitionToReplace,
                   NextActionOfReduce: NextAction);
            return result;

         default:
            throw new ErrorInGrammlatorProgramException("unknown argument " + nameof(TypeOfEvaluation));
         }
      } // Ende von VorgängerSindNachbarn

      /// <summary>
      /// Feststellen, welche der aktuellen Zustände unterschieden werden müssen
      /// und dies in den Relationen mit ÄhnlichSetzenZu bzw. VerschiedenSetzenZu eintragen
      /// </summary>
      /// <param name="TypeOfEvaluation"></param>
      /// <param name="StatesWhichAcceptTheNonterminal"></param>
      /// <param name="NonterminalToInputInStates"></param>
      public void FolgeaktionenPaarweiseVergleichen(
          RelationtypeEnum TypeOfEvaluation,
          List<ParserState> StatesWhichAcceptTheNonterminal,
          NonterminalSymbol NonterminalToInputInStates)
      {
         // for all pairs of states with state1Index < state2Index
         for (Int32 state1Index = 0; state1Index < StatesWhichAcceptTheNonterminal.Count; state1Index++)
         {
            ParserAction nextAction1 = SimplifiedNextAction(
                   StatesWhichAcceptTheNonterminal[state1Index].ActionCausedBy(NonterminalToInputInStates));

            for (Int32 state2Index = state1Index + 1; state2Index < StatesWhichAcceptTheNonterminal.Count; state2Index++)
            {
               ParserAction nextAction2 = SimplifiedNextAction(
                   StatesWhichAcceptTheNonterminal[state2Index].ActionCausedBy(NonterminalToInputInStates));

               switch (TypeOfEvaluation)
               {
               case RelationtypeEnum.ComputeDistinguishableStatesRelation:
               {
                  // 2. Schritt: die ZustandsRelationVerschieden bestimmen
                  // Prüfen: wäre es sinnvoll, zuvor eine Äquivalenz-Klasseneinteilung für die Zustände 
                  // zu ermitteln und zu verwenden?
                  if (
                      !ActionsDoTheSame(
                      StatesWhichAcceptTheNonterminal[state1Index],
                      StatesWhichAcceptTheNonterminal[state2Index],
                      nextAction1,
                      nextAction2))
                  {
                     DistinguishableStatesRelation.Add(
                         StatesWhichAcceptTheNonterminal[state1Index],
                         StatesWhichAcceptTheNonterminal[state2Index]);
                  }
                  break;
               }
               case RelationtypeEnum.ComputeSimilarStatesRelation:
               {
                  // 3. Schritt: die ZustandsRelationÄhnlich bestimmen
                  if (!DistinguishableStatesRelation.Contains(
                      StatesWhichAcceptTheNonterminal[state1Index],
                      StatesWhichAcceptTheNonterminal[state2Index])
                      && ActionsDoTheSame(
                          StatesWhichAcceptTheNonterminal[state1Index],
                          StatesWhichAcceptTheNonterminal[state2Index],
                          nextAction1,
                          nextAction2))
                  {
                     SimilarStatesRelation.Add(
                         StatesWhichAcceptTheNonterminal[state1Index],
                         StatesWhichAcceptTheNonterminal[state2Index]);
                  }
                  break;
               }
               }
            }
         }
      }

      /// <summary>
      /// Without optimization each generated state pushes its number on the state stack.
      /// This method finds states that do not need to push a number on the state stack
      /// and sets their StateStackNumber to -1
      /// </summary>
      /// <returns>number of states with StateStackNumber == -1</returns>
      private Int32 DetermineNotStackingStates()
      {
         Int32 NotPushingStatesCount = 0;

         // for all classes of the SamePredecessorLevelPartitionOfStates
         foreach (ParserState classRepresentative in SamePredecessorLevelPartitionOfStates.ClassRepresentatives)
         {
            // Search a member of this class that must push a number on the states stack
            Boolean foundPushing = false;
            foreach (ParserState Neighbor in SamePredecessorLevelPartitionOfStates.ElementsOfClass(classRepresentative))
            {
               if (DistinguishableStatesRelation.ContainsKey(Neighbor.IdNumber))
               {
                  foundPushing = true;
                  break;
               }
            }

            if (foundPushing)
               continue;

            // No member of this class must push a number
            // Each member of this class does not need to push a value on the state stack
            // Their StateStackNumbers are set to 0
            foreach (ParserState Neighbor in SamePredecessorLevelPartitionOfStates.ElementsOfClass(classRepresentative))
            {
               if (Neighbor.StateStackNumber >= 0)
               {
                  NotPushingStatesCount++;
                  Neighbor.StateStackNumber = -1;
               }
            }

         }

         return NotPushingStatesCount;
      }

      /// <summary>
      /// Construct a list (action/StateStacknumber) which contains for each state the action resulting if the inputSymbol is input into this state.
      /// If all actions are equivalent remove all except the first entries from the list.
      /// </summary>
      /// <param name="StatesAcceptingThisSymbol">List of parserstates accepting the <paramref name="inputSymbol"/>"/></param>
      /// <param name="inputSymbol">The nonterminal symbol which is allowed as input in each of the states</param>
      /// <returns></returns>
      private static BranchcasesList ConstructListOfBranchcases(List<ParserState> StatesAcceptingThisSymbol, NonterminalSymbol inputSymbol)
      {
         // wird aufgerufen aus VorgängerSindNacharn,nach günstigeKennungenBerechnen()
         var ListOfCases = new BranchcasesList(StatesAcceptingThisSymbol.Count);

         ParserState State1 = StatesAcceptingThisSymbol[0];
         ParserAction Action1 = SimplifiedNextAction(State1.ActionCausedBy(inputSymbol));

         // Add the first case
         AddCase(ListOfCases, State1.StateStackNumber, Action1);

         Boolean Equivalent = true;

         // for each other state in the list of states
         for (Int32 StateIndex = 1; StateIndex < StatesAcceptingThisSymbol.Count; StateIndex++)
         {
            // Find the action caused by inputSymbol and add it to the list of cases
            ParserState StateI = StatesAcceptingThisSymbol[StateIndex];
            ParserAction ActionI = SimplifiedNextAction(StateI.ActionCausedBy(inputSymbol));
            AddCase(ListOfCases, StateI.StateStackNumber, ActionI);

            // Determine if this Action is equivalent to the first action
            Equivalent &= ActionsDoTheSame(State1, StateI, Action1, ActionI);
         }

         // if all cases are equivalent: remove all except the first one from the list
         if (Equivalent)
            ListOfCases.RemoveRange(1, ListOfCases.Count - 1);

         ListOfCases.SortByCondition();

         return ListOfCases;
      }

      private static void AddCase(BranchcasesList listOfCases, Int32 StateStackNumber, ParserAction f1)
      {
         var NewCase = new BranchcaseStruct(StateStackNumber, f1);

         if (listOfCases.Contains(NewCase))
            return;
         listOfCases.Add(NewCase);
         return;
      }

      private static Boolean ActionsDoTheSame(
          ParserState state1,
          ParserState state2,
          ParserAction action1,
          ParserAction action2)
      {
         if (action1 == null || action2 == null)
            return false; // should not occur
         if (action1 is Definition)
            return false; // the same definitions may result in different actions if in different states
         if (ReferenceEquals(action1, action2))
            return true;

         if (action1 is HaltAction a1HaltAction
             && action2 is HaltAction a2HaltAction
             && a1HaltAction.AttributestackAdjustment == a2HaltAction.AttributestackAdjustment)
         {
            return true;
         }

         return action1 is NonterminalTransition a1AsNonterminalTransition
             && action2 is NonterminalTransition a2AsNonterminalTransition
             && a1AsNonterminalTransition.InputSymbol == a2AsNonterminalTransition.InputSymbol
                 && ActionsDoTheSame(
                     state1,
                     state2,
                     a1AsNonterminalTransition.NextAction,
                     a2AsNonterminalTransition.NextAction);
      }

      private static readonly StringBuilder reduceStringBuilder = new StringBuilder(80);

      /// <summary>
      /// Constructs a <see cref="ReduceAction"/> with special handling of the Startsymbol.
      /// </summary>
      /// <param name="stateStackAdjustment">ge 0</param>
      /// <param name="Definition"></param>
      /// <param name="NextActionOfReduce"></param>
      /// <returns>Returns the reduce action or an </returns>
      private static ParserAction MakeReduceAction(
          Int32 stateStackAdjustment,
          Definition Definition,
          ParserAction NextActionOfReduce)
      {
         Debug.Assert(stateStackAdjustment >= 0);

         // Compose description
         Definition.DefinedSymbol!
            .IdentifierAndAttributesToSB(reduceStringBuilder)
            .Append("= ");
         Definition
             .ToStringbuilder(reduceStringBuilder, Definition!.Elements.Length + 1);
         String description = reduceStringBuilder.ToString();
         reduceStringBuilder.Clear();

         // construct reduceAction
         var newReduceAction = new ReduceAction(NextActionOfReduce) {
            IdNumber = GlobalVariables.ListOfAllReductions.Count,// starting with 0
            Description = description,
            StateStackAdjustment = stateStackAdjustment,
            // PriorityFunction = Definition.PriorityFunction, // has already been copied from Definition to PrioritySelectAction
            SemanticMethod = Definition.SemanticMethod,
            AttributeStackAdjustment = Definition.AttributestackAdjustment,
            // because optimizations may change AttributeStackAdjustment the 
            // value of the condition "AttributeStackAdjustment > 0" must be saved
            FirstAdjustAttributeStackThenCallMethod = Definition.AttributestackAdjustment > 0
         };

         /* Handle the special case of definitions of the startsymbol:
          *   The reduce action shall not remove the attributes of the definition from the parsers attribute stack.
          *   Those attributes shall be copied by the parsers halt action to the attributes of the result.
          */
         if (Definition.DefinedSymbol == GlobalVariables.Startsymbol)
         {
            Debug.Assert(NextActionOfReduce == GlobalVariables.ListOfAllHaltActions[0]);
            Debug.Assert(Definition.AttributestackAdjustment <= 0); // because the startsymbol has no attributes

            if (Definition.AttributestackAdjustment == 0 && GlobalVariables.ListOfAllStates[0].StateStackNumber < 0)
            {
               newReduceAction.NextAction = GlobalVariables.TheEndOfGeneratedCodeAction;
            }
            else
            {
               HaltAction? haltAction =
               GlobalVariables.ListOfAllHaltActions.Find(h => h.AttributestackAdjustment == -Definition.AttributestackAdjustment);

               if (haltAction == null)
               {
                  haltAction =
                      new HaltAction(
                          GlobalVariables.ListOfAllHaltActions.Count,
                          -Definition.AttributestackAdjustment
                          );
                  GlobalVariables.ListOfAllHaltActions.Add(haltAction);
               }

               newReduceAction.NextAction = haltAction;
            }

            newReduceAction.AttributeStackAdjustment = 0; // the halt action will remove the attributes from the stack (if any)
         }

         newReduceAction.NextAction = SimplifiedNextAction(newReduceAction.NextAction);

         SimplifyChainOfReductions(newReduceAction);

         // gleichartige Reduktion suchen - falls gefunden return
         ReduceAction? foundReduceAction = null;
         foreach (ReduceAction existingReduceAction in GlobalVariables.ListOfAllReductions)
         {
            if (
            existingReduceAction.StateStackAdjustment == newReduceAction.StateStackAdjustment
            && existingReduceAction.SemanticMethod == newReduceAction.SemanticMethod
            && existingReduceAction.AttributeStackAdjustment == newReduceAction.AttributeStackAdjustment
            && existingReduceAction.NextAction == newReduceAction.NextAction
            && !(existingReduceAction.NextAction is Definition)
            )
            {   //found 
               foundReduceAction = existingReduceAction;
               break;
            }
         }

         if (foundReduceAction != null)
         {
            // combine descriptions
            if (!foundReduceAction.Description.Contains(newReduceAction.Description)) // CHECK 
            {
               reduceStringBuilder.AppendLine(foundReduceAction.Description);
               reduceStringBuilder.Append("or: ");
               reduceStringBuilder.Append(newReduceAction.Description);
               foundReduceAction.Description = reduceStringBuilder.ToString();
               reduceStringBuilder.Clear();
            }
            return foundReduceAction;
         }

         // nicht gefunden => aufnehmen
         GlobalVariables.ListOfAllReductions.Add(newReduceAction);
         return newReduceAction;
      }

      /// <summary>
      /// If action has a chain of unambigous next actions the last of these will be returned,
      /// otherwise the action. Next actions of type <see cref="Definition"/> are not removed.
      /// This method performs different optimizations. It must be used to avoid unnecessary look ahead.
      /// </summary>
      /// <param name="action">the action to start from</param>
      /// <returns>the last action of the chain of redundant actions</returns>
      internal static ParserAction SimplifiedNextAction(ParserAction action)
      {
         switch (action)
         {
         case NonterminalTransition nonterminalTransition:
         {
            if (nonterminalTransition.NextAction is Definition)
               return action;
            // CHECK can it be guaranteed that the following recursion ends?
            return nonterminalTransition.NextAction = SimplifiedNextAction(nonterminalTransition.NextAction);
         }

         case ParserState state:
            /* if the parser state does not push on the state stack
             * and if it has no shift and no shift-reduce action
             * and only one look ahead action (perhaps after solving conflicts)
             * which is not "apply definition",
             * then the "goto state action" MUST be replaced 
             * by the next action of the single look ahead action
             * to avoid unnecessary look ahead
             */
            return (
                state.StateStackNumber == -1
                && (state.RedundantLookaheadOrSelectActionOrNull() is LookaheadAction ActionToSkip)
                && !(ActionToSkip.NextAction is Definition))
                // definitions must not be moved out of the state
                ? ActionToSkip.NextAction = SimplifiedNextAction(ActionToSkip.NextAction)
                : action;

         case ReduceAction reduceAction:
         {
            /* After replacing all Definitions by reduceActions
             * there may exist identical reduce actions in the list of all reduce actions. 
             * Replace all references by a reference to the first one
             */

            if (reduceAction.NextAction == GlobalVariables.TheEndOfGeneratedCodeAction
               && reduceAction.StateStackAdjustment == 0
               && reduceAction.AttributeStackAdjustment == 0
               && reduceAction.SemanticMethod == null)
               return GlobalVariables.TheEndOfGeneratedCodeAction;

            foreach (ReduceAction listedReduceAction in GlobalVariables.ListOfAllReductions)
            {
               if (
               listedReduceAction.StateStackAdjustment == reduceAction.StateStackAdjustment
               && listedReduceAction.SemanticMethod == reduceAction.SemanticMethod
               && listedReduceAction.AttributeStackAdjustment == reduceAction.AttributeStackAdjustment
               && listedReduceAction.NextAction == reduceAction.NextAction
               && !(listedReduceAction.NextAction is Definition)
               )
               {
                  if (listedReduceAction.Description != reduceAction.Description)
                  {
                     listedReduceAction.Description = String.Concat(
                        listedReduceAction.Description,
                        Environment.NewLine,
                        "or: ",
                        reduceAction.Description);
                  }

                  return listedReduceAction;
               }
            }
            Debug.Fail("ReduceAction not found in list");
            return reduceAction;
         }

         case BranchAction branch:
         {
            List<BranchcaseStruct> branchCaseList = branch.ListOfCases;

            // Simplify the actions of a branch 
            for (Int32 i = 0; i < branchCaseList.Count; i++)
            {
               BranchcaseStruct branchCase = branchCaseList[i];
               branchCase.BranchcaseAction = SimplifiedNextAction(branchCase.BranchcaseAction);
               // TOCHECK may this cause endless recursion ?? 
               branchCaseList[i] = branchCase;
            }

            Debug.Assert(branchCaseList.Count > 0);

            // test if all cases now (after optimization) have the same action
            BranchcaseStruct branchCase0 = branchCaseList[0];
            for (Int32 i = 1; i < branchCaseList.Count; i++)
            {
               if (branchCaseList[i].BranchcaseAction != branchCase0.BranchcaseAction)
                  return branch; // at least one is different: return the (optimized) branch
            }

            // all cases have the same action 
            return branchCase0.BranchcaseAction;
         }
         }

         return action;
      } // SimplyfiedNextAction(ParserAction action) 

      /// <summary>
      /// Test if <paramref name="Reduction"/>.NextAction is also a <see cref="ReduceAction"/> and combine them if possible
      /// </summary>
      /// <param name="Reduction"></param>
      private static void SimplifyChainOfReductions(ReduceAction Reduction)
      {
         if (!(Reduction.NextAction is ReduceAction nextReduction))
            return;

         // The NextAction of Reduction is also a ReduceAction

         // TODO add correct handling of semantic PriorityFunction

         // do not include nextReduction, if nextReduction has a semantic method (which might be duplicated)
         if (nextReduction.SemanticMethod != null)
            return;

         // do not combine, if this will cause an additional AttributeStackAdjustment
         if (Reduction.AttributeStackAdjustment == 0 && nextReduction.AttributeStackAdjustment != 0)
            return;

         if (Reduction.SemanticMethod == null && nextReduction.SemanticMethod == null)
         {
            // The reductions can be combined: both have no semantic action
            // AttributeStackAdjustment s can be added
         }
         else if (Reduction.SemanticMethod == null)
         {
            Debug.Assert(Reduction.SemanticMethod == null && nextReduction.SemanticMethod != null);

            // The semantic method of nextReduction can be copied to reduction
            // and the nextReduction be skipped. This may cause duplication of generated code!

            //TODO Test if a reference count can be introduced (if 1 try to combine)

            //if (Reduction.AttributeStackAdjustment == 0)
            //    { 
            //    // Reduction contains no SemanticMethod and no AttributeStackAdjustment
            //    Reduction.FirstAdjustAttributeStackThenCallMethod = nextReduction.FirstAdjustAttributeStackThenCallMethod;
            //    // assignments common to all combinations:
            //    // Reduction.NextAction = nextReduction.NextAction;
            //    // Reduction.AttributeStackAdjustment += nextReduction.AttributeStackAdjustment;
            //    // Reduction.StateStackAdjustment += nextReduction.StateStackAdjustment;
            //    // Reduction.Description = Reduction.Description + Environment.NewLine + "then: " + nextReduction.Description; // Prüfen: reicht diese Beschreibung der letzten Alternative der Reduktionskette  ?
            //    }
            //else if (nextReduction.AttributeStackAdjustment == 0 || 
            //     ( nextReduction.FirstAdjustAttributeStackThenCallMethod && !Reduction.FirstAdjustAttributeStackThenCallMethod)
            //    {
            //    // die nachfolgende Reduktion enthält keine Kellerkorrektur nach ihrer Aktion
            //    // Reduction.FirstAdjustAttributeStackThenCallMethod remains unchanged
            //    // assignments common to all combinations:
            //    // Reduction.NextAction = nextReduction.NextAction;
            //    // Reduction.AttributeStackAdjustment += nextReduction.AttributeStackAdjustment;
            //    // Reduction.StateStackAdjustment += nextReduction.StateStackAdjustment;
            //    // Reduction.Description = Reduction.Description + Environment.NewLine + "then: " + nextReduction.Description; // Prüfen: reicht diese Beschreibung der letzten Alternative der Reduktionskette  ?
            //    }
            //else
            //    return; // do not combine because the atribute stack has to be adjusted before and after the method

            //Reduction.SemanticMethod = nextReduction.SemanticMethod;
         }
         else
         if (nextReduction.SemanticMethod == null)
         {
            // Reduction.SemanticMethod != null && nextReduction.SemanticMethod == null
            Debug.Assert(Reduction.SemanticMethod != null);
            // combined Reduction.SemanticMethod
            if (nextReduction.AttributeStackAdjustment == 0)
            {
               // combined Reduction.AttributeStackAdjustment += 0;
               // combined Reduction.FirstAdjustAttributeStackThenCallMethod
            }
            else if (Reduction.AttributeStackAdjustment == 0 || !Reduction.FirstAdjustAttributeStackThenCallMethod)
            {
               // the combined reduction has no AttributeStackAdjustment to be done before the method call
               Reduction.FirstAdjustAttributeStackThenCallMethod = false;
            }
            else
            {
               return; // do not combine
            }
         }
         else
         { // both have semantic methods
            return; // do not combine
         }

         // Combine into Reduction

         Reduction.NextAction = nextReduction.NextAction;
         Reduction.AttributeStackAdjustment += nextReduction.AttributeStackAdjustment;
         Reduction.StateStackAdjustment += nextReduction.StateStackAdjustment;
         Reduction.Description = Reduction.Description + Environment.NewLine + "then: " + nextReduction.Description;

         SimplifyChainOfReductions(Reduction);
      }

      public static BranchAction FindOrConstructBranch(BranchcasesList newListOfCases)
      {
         /* Optimierungsmöglichkeit (derzeit teils realisiert ??):
          * es können bereits vorhandene Verzweigungen gesucht werden, die die
          * neue Verzweigung enthalten oder mit ihr zu einer komplexeren ver-
          * einigt werden koennen 
          * Achtung: einfach implementierbare Falllisten sollten nicht Teil von komplizierten werden !!!!
         */

         // Search: compare with all existing branches
         foreach (BranchAction NextExistingBranch in GlobalVariables.ListOfAllBranchActions)
         {
            BranchcasesList NextExistingBranchCases = NextExistingBranch.ListOfCases;

            // zutreffend sind auch Obermengen von FallListe
            BranchcaseStruct NextNewCase; // = newListOfCases[newListOfCases.Count - 1]; // start with last

            // test all cases of the new list whether they are contained in NextExistingBranch
            for (Int32 NewCaseindex = 0; NewCaseindex < newListOfCases.Count; NewCaseindex++)
            {
               NextNewCase = newListOfCases[NewCaseindex];

               // search the BranchcaseCondition of the new list in the existing brach
               Boolean NewCaseIsContainedInBranch = false;
               for (Int32 ExistingCaseindex = 0; ExistingCaseindex < NextExistingBranchCases.Count; ExistingCaseindex++)
               { // nächsten Vergleichsfall wählen, dabei alle Vergleichsfälle mit kleinerer Kennung ignorieren
                  BranchcaseStruct NextExistingCase = NextExistingBranchCases[ExistingCaseindex];
                  if (NextExistingCase.BranchcaseCondition == NextNewCase.BranchcaseCondition)
                  {
                     // The following simplifiing reduces the number of generated lines.
                     // Is this the right place to do it???
                     NextExistingCase.BranchcaseAction = SimplifiedNextAction(NextExistingCase.BranchcaseAction);
                     NextExistingBranchCases[ExistingCaseindex] = NextExistingCase;

                     NewCaseIsContainedInBranch =
                        NextExistingCase.BranchcaseAction == NextNewCase.BranchcaseAction;

                     break; // found the unique NextExistingCase with .BranchcaseCondition == NextNewCase.BranchcaseCondition
                  }
               }

               if (!NewCaseIsContainedInBranch)
                  goto CheckNextExistingBranch; // NextNewCase is not contained in NextExistingBranch=> check next exisitng branch 

               // continue: check next case of newListOfCases
            }

            // new list is contained in NextExistingBranch

            // not implemented: expand an existing case list if there is no contradiction

            return NextExistingBranch; // die gefundene Obermenge zurückgeben

CheckNextExistingBranch:
            ;
         }

         // all existing branches have been checked and no one contains the new list of cases

         // Construct a new branch
         var NewBranch = new BranchAction(
             ListOfCases: new BranchcasesList(newListOfCases),
             IdNumber: GlobalVariables.ListOfAllBranchActions.Count);

         GlobalVariables.ListOfAllBranchActions.Add(NewBranch);

         return NewBranch;
      } // end of FindOrConstructBranch

      public void AssignStatesSmallStackNumbers()
      {
         // Ohne Optimierung hat jeder Zustand seine eindeutige Nummer als Kennung oder
         // (wenn er als Folge von ptimierungen nicht kellernd ist) die Kennung 0.
         // Hier wird für jeden kellernden Zustand eine möglichst kleine Kennung gesucht, die kleiner gleich seiner Nummer ist.
         // Dabei werden Randbedingungen beachtet:
         // Der Zustand darf keine Kennung erhalten, die andere Zustände in seiner Relation VerschiedeneZustände haben.
         // Zum Zzustand ähnliche Zustände sollen nach Möglichkeit gleiche Kennungen erhalten.

         var AllowedStackNumbers = new Boolean[GlobalVariables.ListOfAllStates.Count];

         foreach (ParserState ParserState in GlobalVariables.ListOfAllStates
         .Where((state) => state.StateStackNumber >= 0)
         )
         {
            Int32 StateIdNumber = ParserState.IdNumber;

            // Determine which numbers must not be assigned to the state.

            // Start with:  allowed are all numbers from 0 to StateIdNumber.
            for (Int32 k = 0; k <= StateIdNumber; k++)
               AllowedStackNumbers[k] = true;
            // Numbers which are already assigned to states (having smaller StateIdNumbers)
            // and which are in relation DistinguishableStatesRelation to the actual state
            // are not allowed
            if (DistinguishableStatesRelation.ContainsKey(StateIdNumber))
            {
               foreach (ParserState distinguishableStateWithAssignedNumber
                   in DistinguishableStatesRelation[StateIdNumber].Where((s) => s.IdNumber < StateIdNumber))
               {
                  AllowedStackNumbers[distinguishableStateWithAssignedNumber.StateStackNumber] = false;
               }
            }

            // Search similar states with a lower StateStackNumber and use this if allowed
            Int32 stackNumber = StateIdNumber; // >=0

            if (SimilarStatesRelation.ContainsKey(StateIdNumber))
            {
               foreach (ParserState similarState in SimilarStatesRelation[StateIdNumber])
               {
                  if (similarState.StateStackNumber < stackNumber
                      && AllowedStackNumbers[similarState.StateStackNumber])
                  {
                     stackNumber = similarState.StateStackNumber;
                  }
               }
            }

            // If no similar state found (which would have a smaller number) use smallest allowed number
            if (stackNumber == StateIdNumber)
               stackNumber = Array.FindIndex(AllowedStackNumbers, (allowed) => allowed);

            ParserState.StateStackNumber = stackNumber;
         }
      }

      /// <summary>
      /// Kürzen von Ketten von Aktionen, g.Startaktion bestimmen und Codenummern der Zustände auf 0 setzen 
      /// </summary>
      public static void P4bShortenChainsOfActions()
      {
         for (Int32 StateIndex = 0; StateIndex < GlobalVariables.ListOfAllStates.Count; StateIndex++)
         {
            ParserState State = GlobalVariables.ListOfAllStates[StateIndex];

            State.Codenumber = 0;
            for (Int32 ActionIndex = 0; ActionIndex < State.Actions.Count; ActionIndex++)
            {
               if (!(State.Actions[ActionIndex] is ParserActionWithNextAction ActionWithNextAction))
                  continue;

               ActionWithNextAction.NextAction = SimplifiedNextAction(ActionWithNextAction.NextAction);

               // falls die Aktionenliste mehrere terminale Übergänge mit gleicher Folgeaktion enthält, diese zusammenfassen
               if (ActionWithNextAction is TerminalTransition ActionAsTerminalTransition)
               {
                  for (Int32 ActionToCompareIndex = 0; ActionToCompareIndex < ActionIndex; ActionToCompareIndex++)
                  {
                     if (!(State.Actions[ActionToCompareIndex] is TerminalTransition ActionToCompare))
                        continue;

                     if (ActionToCompare.NextAction == ActionAsTerminalTransition.NextAction)
                     {
                        ActionToCompare.TerminalSymbols.Or(ActionAsTerminalTransition.TerminalSymbols);
                        State.Actions.RemoveAt(ActionIndex); // erfordert Korrektur des Aktionenindex !!!
                        ActionIndex--; // so that the new action (if any) at this index will be checked also
                                       // ActionToCompareIndex is smaller than ActionIndex and not affected by remove
                        break;
                     }
                  }
               }
            }

            // direkt nachfolgende Zustände, die wegen Optimierung nicht erreichbar sind, aus der Liste entfernen
            // (falls sie noch über Aktionen verkettet sind, bleiben sie verfügbar)
            // Remove states which contain only on
            Int32 IndexOfNextState = StateIndex + 1;
            while (IndexOfNextState < GlobalVariables.ListOfAllStates.Count
                && GlobalVariables.ListOfAllStates[IndexOfNextState].StateStackNumber < 0)
            {
               ParserAction? singleAction = GlobalVariables.ListOfAllStates[IndexOfNextState].RedundantLookaheadOrSelectActionOrNull();
               if (singleAction == null)
                  break; // Der Zustand ist erreichbar
               if (!(singleAction is LookaheadAction))
                  break;
               GlobalVariables.ListOfAllStates.RemoveAt(IndexOfNextState);
               // nächsterZustandIndex bezeichnet jetzt den nachfolgenden Zustand, ist also nicht zu erhöhen!
            }
         } // for .. Zustand

         // Reduktionen bearbeiten: jeweils Codenummer auf 0 setzten und Folgeaktion vereinfachen
         foreach (ReduceAction r in GlobalVariables.ListOfAllReductions)
         {
            r.Codenumber = 0;
            r.NextAction = SimplifiedNextAction(r.NextAction);
            SimplifyChainOfReductions(r); // CHECK compute x.Calls and simplify using x.Calls ??
         }

         // Verzweigungen bearbeiten:
         foreach (BranchAction branch in GlobalVariables.ListOfAllBranchActions)
         {
            branch.Codenumber = 0;
            for (Int32 CaseIndex = 0; CaseIndex < branch.ListOfCases.Count; CaseIndex++)
            {
               BranchcaseStruct Branchcase = branch.ListOfCases[CaseIndex];
               Branchcase.BranchcaseAction = SimplifiedNextAction(Branchcase.BranchcaseAction);
               branch.ListOfCases[CaseIndex] = Branchcase;
            }
         }

         // Startaktion bestimmen:
         GlobalVariables.Startaction = GlobalVariables.ListOfAllStates[0];
         //do not remove the first state: not  SimplifiedNextAction(GlobalVariables.ListOfAllStates[0]);
      }

      private void P4cRemoveNotLongerUsedActionsFromStatesAddErrorActionsComputeActionFields()
      {
         // for each state
         foreach (ParserState State in GlobalVariables.ListOfAllStates)
         {
            static Int32 max(Int32 a, Int32 b)
                => a > b ? a : b;

            // Remove not longer used actions of this state
            Int32 DeletedActionsCount = 0;
            Int32 MaximumActionComplexity = 0;
            Int32 ActionComplexitySum = 0;

            for (Int32 ActionIndex = 0; ActionIndex < State.Actions.Count; ActionIndex++)
            {
               if (State.Actions[ActionIndex] is NonterminalTransition
                   || State.Actions[ActionIndex] is DeletedParserAction)
               {
                  // Count the actions to be deleted
                  DeletedActionsCount++;
               }
               else
               {
                  // Compute Terminalcount and SumOfWeights of the remaining action
                  if (State.Actions[ActionIndex] is ConditionalAction c)
                  {
                     c.ComputeTerminalcountSumOfWeightsComplexity(GlobalVariables.TerminalSymbolByIndex);
                     MaximumActionComplexity = max(MaximumActionComplexity, c.Complexity);
                     ActionComplexitySum += c.Complexity;
                  }

                  // Shift the action by DeletedActionsCount places to the front
                  // and replace the first unused action in State.Actions with this action
                  if (DeletedActionsCount > 0)
                     State.Actions[ActionIndex - DeletedActionsCount] = State.Actions[ActionIndex];
               }
            }

            // Remove all deleted actions of this state
            if (DeletedActionsCount > 0)
               State.Actions.RemoveFromEnd(DeletedActionsCount);

            // Add error handling actions
            ErrorhandlingAction? e = State.CheckAndAddErrorAction(GlobalVariables.ErrorHandlerIsDefined);
            if (e != null)
            {
               e.ComputeTerminalcountSumOfWeightsComplexity(GlobalVariables.TerminalSymbolByIndex);
               MaximumActionComplexity = max(MaximumActionComplexity, e.Complexity);
               ActionComplexitySum += e.Complexity;
            }

            /* Ignore the MaximumActionComplexity because the respective action
             * might be generated as last unconditional action
             */
            State.IfComplexity = ActionComplexitySum - MaximumActionComplexity;
         }

         // For each Parseraction compute the number of AcceptCalls and Calls
         GlobalVariables.Startaction.CountUsage(Accept: false);

         OptimizeStateStackOperations();
      }

      private void OptimizeStateStackOperations()
      {
         /* Concept:
          * If possible move the push(p) from states into reduce actions and combine with pop() actions
          * For each state which pushes onto the state stack
          *   a) for each action a of the state
          *      if a pops n from the state stack and a is called only once
          *        then a is a candidate for combining push and pop
          *           to no operation (increment removedPopCounter) or to Pop(n-1)          *           
          *        else a is a candidate to be replaced by a separate PushState(value, a) ParserAction 
          *           increment addedPushStateCounter
          *   b) evaluate the statistics (removedPopCounter-addedPushStateCounter+1>=0)
          *      and if approriate do the modifications
          *      
          * TODO later: replace the restrictions "a pops ... and a is called only once" 
          *      by some less restrictive condition, follow chains of ParserActions, consider splitting reduce actions
          */

         for (Int32 StateIndex = 0; StateIndex < GlobalVariables.ListOfAllStates.Count; StateIndex++)
         {
            ParserState State = GlobalVariables.ListOfAllStates[StateIndex];
            if (State.StateStackNumber < 0)
               continue;

            int removedPopCounter = 0, addedPushStateCounter = 0;

            for (Int32 ActionIndex = 0; ActionIndex < State.Actions.Count; ActionIndex++)
            {
               ParserAction a = State.Actions[ActionIndex];
               if (!(a is ConditionalAction c))
               {
                  removedPopCounter = 0;
                  break;
               }

               var n = c.NextAction;
               if (n.StateStackAdjustment >= 1 &&
                  n.AcceptCalls <= 1 && n.Calls == 1)
               {
                  if (n.StateStackAdjustment == 1)
                     removedPopCounter++;
               }
               else
                  addedPushStateCounter++;
            }

            if (removedPopCounter > 0 && (removedPopCounter - addedPushStateCounter + 1) >= 0)
               OptimizeStateStackOperations(State);


         }
      }

      private void OptimizeStateStackOperations(ParserState state)
      {
         for (Int32 ActionIndex = 0; ActionIndex < state.Actions.Count; ActionIndex++)
         {
            ConditionalAction c = (ConditionalAction)state.Actions[ActionIndex];

            var n = c.NextAction;
            if (n.StateStackAdjustment >= 1 &&
               n.AcceptCalls <= 1 && n.Calls == 1)
            {
               if (n.StateStackAdjustment == 1)
                  n.StateStackAdjustment--;
            }
            else
            {
               var p = new PushStateAction
                  (GlobalVariables.ListOfAllPushStateActions.Count, state.StateStackNumber, c.NextAction);
               GlobalVariables.ListOfAllPushStateActions.Add(p);

               p.Calls = 1; 

               if (c is TerminalTransition)
               { // accept call of p
                  p.AcceptCalls = 1;
                  n.AcceptCalls -= 1;
                  if (n.AcceptCalls == 0)
                     n.Calls--; // n no longer called by its own accept
               }
               n.Calls++; // n called by this

               p.StateStackNumber = state.StateStackNumber;

               c.NextAction = p;

            }

            // TODO put in list of PushStateActions
            // TODO set idNumber
            // TODO  and use the list to generate

            // CHECK PrioritySelectAction have more than one next action
         }

         state.StateStackNumber = -state.StateStackNumber - 2;
      }
   }
}