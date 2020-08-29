using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Grammlator {
   internal class P4ReplaceNonterminalsAndOptimize {
      /* Phase 4 checks all states and handles alls actions which would cause "apply definition" 
       *    (execute semantic method and go back to a state in the state stack, input 
       *     the generated nonterminal symbol and "shift" to the follow state)
       * and replaces each one by an explicit "reduction" which adjusts the state stack
       * and may be followed by a "branch" which uses the value on top
       * of the state stack to decide which is the follow state (or action after optimzation).
       * It recognizes states which need not to push their number on
       * the state stack (assigns the number -1), 
       * tries to assign the same (small) number to different states,
       * tries to eliminate trivial branches, shorten chains of trivial actions.
       * It adds error actions.
       * When this is finished, nonterminal symbols and their definitions
       * are not longer used.
       * 
       * Idea: find and generate states which can store their return information
       * in a local variable (clear variable, increment variable, decrement variable, if (variable==0)...)
       * instead of pushing it on the stack (Example: "*= aBa; aBa= b | a aBa a;" )
       * 
       * */

      // TODO test and finish implementation of dynamic priorities
      // TODO some more ideas how to optimize
      /* - when shortening chains:
       * -- combine branches  b1 und b2 if b1 is contained in b2
       * -- Also shorten
       *    if x.symbol==point then goto s4;...s4: x.FetchSymbol; if x.symbol==point then ..
       */

      public static void MakeInstanceAndExecute()
      {
         var p4 = new P4ReplaceNonterminalsAndOptimize();

         p4.P4aComputeStateStackNumbersAndBranches();
         P4bShortenChainsOfActions();

         // Now  ListOfAllStates[0] is definitely assigned and GlobalVariables.Startaction may be used
         p4.P4cRemoveNotLongerUsedActionsFromStatesAddErrorActionsComputeComplexity();

         // For each Parseraction compute the number of AcceptCalls and Calls
         GlobalVariables.Startaction.CountUsage(Accept: false);

         // Move push operations from states to actions if this reduces the number of those operations
         p4.MoveStateStackPushOperations();
      }

      /// <summary>
      /// Constructor: assigns the partition / relations <see cref="SamePredecessorLevelPartitionOfStates"/>,
      ///  and <see cref="DistinguishableStatesRelation"/>
      /// </summary>
      private P4ReplaceNonterminalsAndOptimize()
      {
         SamePredecessorLevelPartitionOfStates
            = new PartitionInfoArray<ParserState>(GlobalVariables.ListOfAllStates);
         DistinguishableStatesRelation
             = new SymmetricRelation<ParserState>(GlobalVariables.ListOfAllStates.Count);
      }

      private readonly SymmetricRelation<ParserState> DistinguishableStatesRelation;

      /// <summary>
      /// All states belonging to the same class of <see cref="SamePredecessorLevelPartitionOfStates"/>
      /// either all do not push to the stack of states in the generated code or all push
      /// </summary>
      private readonly PartitionInfoArray<ParserState> SamePredecessorLevelPartitionOfStates;

      private enum RelationtypeEnum {
         ComputeDistinguishableStatesRelation,
         ComputeBranches
      };

      /// <summary>
      /// In Phase2 the StateStackNumber of each state has been set to its idNumber (==position in ListOfallStes).
      /// Now this number is set to -1 for all states which need not to push their number on the state stack
      /// and for all other states optionally a small number is assigned, so that generated switch statements may be 
      /// translated by the jitter to more performant code.
      /// Each occurence of a "apply definition action" is replaced by a reduce action
      /// (modify state and attribute stack, call semantic method)
      /// followed by a branch action (branch to next action depending on the state stack).
      /// Optimization shortens chains of these actions.
      /// 
      /// </summary>
      public void P4aComputeStateStackNumbersAndBranches()
      {

         ComputeSamePredecessorLevelPartitionOfStates();

         // ComputeDistinguishableStatesRelation
         DistinguishableStatesRelation.Clear();
         EvaluatePrecedingStates(RelationtypeEnum.ComputeDistinguishableStatesRelation);

         // DetermineNotStackingStates using SamePredecessorLevelPartitionOfState and DistinguishableStatesRelation
         GlobalVariables.CountOfStatesWithStateStackNumber =
             GlobalVariables.ListOfAllStates.Count - DetermineNotStackingStates();

         // Optionally assign small StateStackNumbers
         if (GlobalVariables.OptimizeStateStackNumbers)
         {
            AssignStatesSmallStackNumbers();
         }

         // Compute branches
         EvaluatePrecedingStates(RelationtypeEnum.ComputeBranches);
      }

      /// <summary>
      /// Visit all states, take all actions which apply a definition,
      /// and find all predecessor states up to a search depth given by the definitions length.
      /// Assign all states found at the same depth to the same class of the NeighborPartition.
      /// Purpose: If one state within a class will have to push a state specific StateStackNumber 
      /// on to the state stack all other states of the same class will have to push to the stack.
      /// </summary>
      private void ComputeSamePredecessorLevelPartitionOfStates()
      {
         foreach (ParserState state in GlobalVariables.ListOfAllStates)
         {
            foreach (ParserAction action in state.Actions.PriorityUnwindedSetOfActions)
            {
               if (action is ParserActionWithNextAction actionWithNextAction)
               {
                  if (actionWithNextAction.NextAction is Definition definition)
                  {
                     CombineClassesOfSamePredecessorLevelPartitionOfStates(
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
                        throw new NotImplementedException("Constant priority action in Priorit<BranchAction not yet implemented");
                     }
                     foreach (ParserAction dynamicPriorityAction in b.DynamicPriorityActions)
                     {
                        Debug.Assert(dynamicPriorityAction is Definition);
                        if (dynamicPriorityAction is Definition d)
                           CombineClassesOfSamePredecessorLevelPartitionOfStates(
                               StateToStartFrom: state,
                               Depth: d.Elements.Length);
                     }
                  }
               }
            }
         }
      }

      /// <summary>
      /// used as temporary variable in <see cref="CombineClassesOfSamePredecessorLevelPartitionOfStates"/>
      /// and in <see cref="EvaluatePredecessors"/>
      /// </summary>
      List<ParserState>
         StatesOfThisLevel =
             new List<ParserState>(GlobalVariables.ListOfAllStates.Count),
         StatesOfNextLevel =
             new List<ParserState>(GlobalVariables.ListOfAllStates.Count);

      /// <summary>
      /// Start with <paramref name="StateToStartFrom"/> and walk <paramref name="Depth"/> levels
      /// back to all predecessors combining all states of the same depth into the same class
      /// of <see cref="SamePredecessorLevelPartitionOfStates"/>
      /// </summary>
      /// <param name="StateToStartFrom">state from where to walk back</param>
      /// <param name="Depth">number of levels to walk back</param>
      private void CombineClassesOfSamePredecessorLevelPartitionOfStates(ParserState StateToStartFrom, Int32 Depth)
      {
         // start with StateToStartFrom as single element of the set of states to evaluate 
         // and walk back 
         StatesOfThisLevel.Clear();
         StatesOfThisLevel.Add(StateToStartFrom);

         StatesOfNextLevel.Clear();

         // Iterate Depth-1 levels
         for (Int32 remainingLength = Depth; remainingLength > 0; remainingLength--)
         {
            // Add all predecessors of all states of this level to the list of preceding states
            foreach (ParserState stateToEvaluate in StatesOfThisLevel)
            {
               foreach (ParserState predecessor in stateToEvaluate.PredecessorList!)
                  StatesOfNextLevel.Add(predecessor);
            }

            Debug.Assert(StatesOfNextLevel.Count != 0, "Error in phase4: no predecessor)");

            // Choose the partition class of one of the elements
            ParserState representativeOfClass = StatesOfNextLevel[0];

            // Combine the classes of all predecessors of the this level with this class
            foreach (ParserState predecessor in StatesOfNextLevel)
               SamePredecessorLevelPartitionOfStates.CombineClasses(representativeOfClass, predecessor);
            //  allowing the class of precedingStates[0] to combine with itself makes code simpler

            // The contents of statesToEvaluate is not longer needed, the lists capacity will be reused
            List<ParserState> temp = StatesOfThisLevel;

            // The next level becomes he actual level
            StatesOfThisLevel = StatesOfNextLevel;

            // Reuse StatesOfThisLevel
            StatesOfNextLevel = temp;
            StatesOfNextLevel.Clear();
         }

         StatesOfThisLevel.Clear();
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
            foreach (ParserAction action in state.Actions.PriorityUnwindedSetOfActions)
            {
               if ((action is ParserActionWithNextAction actionWithNextAction)
                   && (actionWithNextAction.NextAction is Definition definition))
               {
                  switch (TypeOfEvaluation)
                  {
                  case RelationtypeEnum.ComputeDistinguishableStatesRelation:
                     EvaluatePredecessors(
                         RelationtypeEnum.ComputeDistinguishableStatesRelation,
                         StateToStartFrom: state,
                         DefinitionToReplace: definition,
                         Depth: definition.Elements.Length + ((action is LookaheadAction) ? 0 : -1
                         // In terminal and nonterminal shifts the item is one position before the end:
                         // depth must be one less than the length of the definitions elementlist
                         )
                     );
                     break;
                  case RelationtypeEnum.ComputeBranches:
                     Debug.Assert(actionWithNextAction is ConditionalAction && actionWithNextAction.NextAction is Definition);
                     actionWithNextAction.NextAction =
                         EvaluatePredecessors(
                             RelationtypeEnum.ComputeBranches,
                             StateToStartFrom: state,
                             DefinitionToReplace: definition,
                             Depth: definition.Elements.Length + ((action is LookaheadAction) ? 0 : -1)
                         )!;
                     Debug.Assert(!(actionWithNextAction.NextAction is Definition));
                     break;
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
      private ParserAction? EvaluatePredecessors(
          RelationtypeEnum TypeOfEvaluation,
          ParserState StateToStartFrom,
          Definition DefinitionToReplace,
          Int32 Depth)
      {
         // List<ParserState> StatesOfThisLevel = new List<ParserState>(GlobalVariables.ListOfAllStates.Count);
         StatesOfThisLevel.Clear();
         StatesOfThisLevel.Add(StateToStartFrom);

         StatesOfNextLevel.Clear();

         Int32 countOfStateStackNumbersToPop = 0; // used only if RelationtypeEnum.ComputeBranches

         // Iterate depth levels
         for (Int32 remainingDepth = Depth; remainingDepth > 0; remainingDepth--)
         {
            // Add the predecessors of all states of this level to the list of states of the next level
            foreach (ParserState state in StatesOfThisLevel)
            {
               foreach (ParserState predecessor in state.PredecessorList!)
               {
                  if (!StatesOfNextLevel.Contains(predecessor)) // linear search!
                     StatesOfNextLevel.Add(predecessor);
               }
            }

            Debug.Assert(StatesOfNextLevel.Count != 0, "Error in phase4: no predecessor)");

            // Either the StateStackNumber of all predecessors is >=0 or of all is <0
            // States with <=0 won't push and are not counted 
            if (StatesOfThisLevel[0].StateStackNumber >= 0)
            {
               countOfStateStackNumbersToPop++;
            }

            // The contents of statesToEvaluate is no longer needed, the lists capacity will be reused
            StatesOfThisLevel.Clear();

            List<ParserState> temp = StatesOfThisLevel;
            // go one level down: the collected preceding states become the states to evaluate
            StatesOfThisLevel = StatesOfNextLevel;
            // The cleared list StatesOfThisLevel is reused 
            StatesOfNextLevel = temp;
         }

         /*  statesOfThisLevel contains all states which are reachable from the given state
          *  by applying the given definition. Each of these states contains the startitem of the given definition.
          *  
          *  If exactly one action results by input of the nonterminal symbol (defined by the definition)
          *  the definition is replaced by this action else a branch action has to be generated.
          */

         NonterminalSymbol definedSymbol = DefinitionToReplace.DefinedSymbol!;

         switch (TypeOfEvaluation)
         {
         case RelationtypeEnum.ComputeDistinguishableStatesRelation:
            ComputeDistinguishableStatesRelation(StatesOfThisLevel, definedSymbol);
            return DefinitionToReplace; // notnull dummy

         case RelationtypeEnum.ComputeBranches:
            BranchcasesList ListOfBranchcases = ConstructListOfBranchcases(StatesOfThisLevel, definedSymbol);
            ParserAction NextAction =
                    ListOfBranchcases.Count == 1
                    ? ListOfBranchcases[0].BranchcaseAction // no branch, single action
                    : FindOrConstructBranch(ListOfBranchcases);

            StatesOfThisLevel.Clear();

            if (DefinitionToReplace.HasNoSemantics() && (countOfStateStackNumbersToPop == 0)
                && DefinitionToReplace.DefinedSymbol != GlobalVariables.Startsymbol)
            {
               return NextAction; // branch or action
            }
            else
            {  // reduce => (branch or action)
               return MakeReduceAction(
                   countOfStateStackNumbersToPop,
                   DefinitionToReplace,
                   NextActionOfReduce: NextAction);
            }

         default:
            throw new ErrorInGrammlatorProgramException("unknown argument " + nameof(TypeOfEvaluation));
         }
      } // End of EvaluatePredecessors

      /// <summary>
      /// Feststellen, welche der aktuellen Zustände unterschieden werden müssen
      /// und dies in den Relationen mit ÄhnlichSetzenZu bzw. VerschiedenSetzenZu eintragen
      /// </summary>
      /// <param name="statesWhichAcceptTheNonterminal"></param>
      /// <param name="inputSymbol"></param>
      /// 
      private void ComputeDistinguishableStatesRelation(
          List<ParserState> statesWhichAcceptTheNonterminal,
         NonterminalSymbol inputSymbol)
      {
         // for all pairs of states with state1Index < state2Index
         for (Int32 state1Index = 0; state1Index < statesWhichAcceptTheNonterminal.Count; state1Index++)
         {
            ParserState State1 = statesWhichAcceptTheNonterminal[state1Index];
            ParserAction nextAction1 = State1.ActionCausedBy(inputSymbol); //  a nonterminal transition!

            for (Int32 state2Index = state1Index + 1; state2Index < statesWhichAcceptTheNonterminal.Count; state2Index++)
            {
               ParserState State2 = statesWhichAcceptTheNonterminal[state2Index];
               ParserAction Action2 = State2.ActionCausedBy(inputSymbol); //  a nonterminal transition!

               // Step 2: compute the DistinguishableStatesRelation
               if (!ActionsDoTheSame(State1, State2, nextAction1, Action2))
               {
                  DistinguishableStatesRelation.Add(
                     statesWhichAcceptTheNonterminal[state1Index], State2);
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

         // The if condition avoids combinations of trivial halt actions resulting in lengthy descriptions
         // if(true) would generate the same code except comments (example: P1bLexer)
         ParserAction NextAction = newReduceAction.NextAction;
         if (!(NextAction is EndOfGeneratedCodeAction || NextAction is HaltAction)
            || newReduceAction.SemanticMethod != null
            || newReduceAction.StateStackAdjustment != 0
            // || newReduceAction.AttributeStackAdjustment != 0 // will always be 0, see above
            )
         {
            // search equivalent ReduceAction
            ReduceAction? foundReduceAction = SearchEquivalentReduceAction(newReduceAction);

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
         }

         // nicht gefunden => aufnehmen
         GlobalVariables.ListOfAllReductions.Add(newReduceAction);
         return newReduceAction;
      }

      private static ReduceAction? SearchEquivalentReduceAction(ReduceAction newReduceAction)
      {
         for (int ir = 0; ir < GlobalVariables.ListOfAllReductions.Count; ir++)
         {
            ReduceAction existingReduceAction = GlobalVariables.ListOfAllReductions[ir];

            // TODO Some Reductions in the list will have been constructed with a definition as NexAction.
            // Meanwhile this definition migth have been replaced by a reduction
            // The following simplify-xxx calls cause some new reductions to be found in the list of reductions.
            // But do they solve all those problems? Is it possible to reduce this multiple recalculations?
            // Concept:  1. replace all definitions by reductions (with minimal optimizations)
            //                  with NextAction == (nonterminalTransition to) state 
            //                               or    nonterminalTransition then definition
            //                               or     Branch ...
            //           2. Replace nonterminal transitions by their NextAction
            //           2. add reference counts to reductions
            //           3. combine reductions

            // remove nonterminal transitions (!) and do some more optimizations
            existingReduceAction.NextAction = SimplifiedNextAction(existingReduceAction.NextAction);
            SimplifyChainOfReductions(existingReduceAction);
            // end of simple solution

            if (
            existingReduceAction.StateStackAdjustment == newReduceAction.StateStackAdjustment
            && existingReduceAction.SemanticMethod == newReduceAction.SemanticMethod
            && existingReduceAction.AttributeStackAdjustment == newReduceAction.AttributeStackAdjustment
            && existingReduceAction.NextAction == newReduceAction.NextAction
            && !(existingReduceAction.NextAction is Definition)
            )
            {   //found 
               return existingReduceAction;
            }
         }

         return null;
      }

      static int SimplifiedNextActionRecursionCount = 0;
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
               return action; // nonterminal transitions next action has still to be resolved
            return nonterminalTransition.NextAction; // nonterminal transitions must be skipped !!
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
            //CHECK here the definition "B=B??1??" causes an endless loop
            /*
             * the state contains only one look ahead action: if (allTerminalSymbols)  goto state
             */

            if (state.StateStackNumber != -1)
               return action; // state modifies state stack and can not be skipped
            if (!(state.RedundantLookaheadOrSelectActionOrNull() is LookaheadAction ActionToSkip))
               return action; // more than one action or not a lookahead action
            if (ActionToSkip.NextAction is Definition)
               return action; // definitions must not be moved out of the state

            /*The example "//|  *= A; A= A??2??;" will cause an endless loop if recursion is not limited
             * because the second state contains only one look ahead action: if (allTerminalSymbols)  "second state"
             * */

            if (SimplifiedNextActionRecursionCount < 100) // CHECK SimplifiedNextActionRecursionCount < 100
            {
               SimplifiedNextActionRecursionCount++;
               ActionToSkip.NextAction = SimplifiedNextAction(ActionToSkip.NextAction);
               SimplifiedNextActionRecursionCount--;
               return ActionToSkip.NextAction;
            }

            // after removing nonterminal transitions a state will contain no actions 
            return action; 

            //return ( //e.g. replaces a next reduce action with the first equivalent reduce action
            //    state.StateStackNumber == -1
            //    && (state.RedundantLookaheadOrSelectActionOrNull() is LookaheadAction ActionToSkip)
            //    && !(ActionToSkip.NextAction is Definition))
            //    // definitions must not be moved out of the state
            //    ? ActionToSkip.NextAction = SimplifiedNextAction(ActionToSkip.NextAction)
            //    : action;

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

            ReduceAction? result = GlobalVariables.ListOfAllReductions.Find(
               (listedReduceAction) => listedReduceAction.StateStackAdjustment == reduceAction.StateStackAdjustment
               && listedReduceAction.SemanticMethod == reduceAction.SemanticMethod
               && listedReduceAction.AttributeStackAdjustment == reduceAction.AttributeStackAdjustment
               && listedReduceAction.NextAction == reduceAction.NextAction
               && !(listedReduceAction.NextAction is Definition));
            if (result != null)
            {
               if (!result.Description.Contains(reduceAction.Description))
               {
                  result.Description =
                     String.Concat(result.Description, Environment.NewLine, "or: ", reduceAction.Description);
               }
               return result; // is a preceding listed reduce action or the given reduce action
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
               // ????????????????????? branchCase.BranchcaseAction = SimplifiedNextAction(branchCase.BranchcaseAction);
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

         // Do not combine, if this will cause an additional AttributeStackAdjustment
         if (Reduction.AttributeStackAdjustment == 0 && nextReduction.AttributeStackAdjustment != 0)
            return;

         if (Reduction.SemanticMethod != null && nextReduction.SemanticMethod != null)
            return; // // both have semantic methods: do not combine
         else if (Reduction.SemanticMethod == null && nextReduction.SemanticMethod == null)
         {
            // The reductions may be combined: both have no semantic action
            // AttributeStackAdjustment s can be combined (added)

            // Do not combine, if this will cause an additional AttributeStackAdjustment
            if (Reduction.AttributeStackAdjustment == 0 && nextReduction.AttributeStackAdjustment != 0)
               return;
         }
         else if (Reduction.SemanticMethod == null)
         {
            Debug.Assert(Reduction.SemanticMethod == null && nextReduction.SemanticMethod != null);
            return; // avoid duplication of calls of semantic method

            //CHECK if a reference count can be introduced (if 1 try to combine)

            // The semantic method of nextReduction can be copied to reduction
            // and the nextReduction be skipped. This may cause duplication of generated code!


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
            Debug.Assert(Reduction.SemanticMethod != null && nextReduction.SemanticMethod == null);

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

            // also supersets will be accepted
            BranchcaseStruct NextNewCase;

            // test all cases of the new list whether they are contained in NextExistingBranch
            for (Int32 NewCaseindex = 0; NewCaseindex < newListOfCases.Count; NewCaseindex++)
            {
               NextNewCase = newListOfCases[NewCaseindex];

               // search the BranchcaseCondition of the new list in the existing branch
               Boolean NewCaseIsContainedInBranch = false;
               for (Int32 ExistingCaseindex = 0; ExistingCaseindex < NextExistingBranchCases.Count; ExistingCaseindex++)
               {
                  // Select the next case to compare and ignore all cases with smaller BranchcaseCondition
                  BranchcaseStruct NextExistingCase = NextExistingBranchCases[ExistingCaseindex];
                  if (NextExistingCase.BranchcaseCondition == NextNewCase.BranchcaseCondition)
                  {
                     // The following SimplifiedNextAction() may reduces the number of generated lines.
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

            return NextExistingBranch; // return the branch (equal or superset)

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

            ParserState.StateStackNumber = Array.FindIndex(AllowedStackNumbers, (allowed) => allowed);
         }
      }

      /// <summary>
      /// Shorten chains of actions and remove xxx
      /// </summary>
      public static void P4bShortenChainsOfActions()
      {

         for (Int32 StateIndex = 0; StateIndex < GlobalVariables.ListOfAllStates.Count; StateIndex++)
         {
            ParserState State = GlobalVariables.ListOfAllStates[StateIndex];

            for (Int32 ActionIndex = 0; ActionIndex < State.Actions.Count; ActionIndex++)
            {
               if (!(State.Actions[ActionIndex] is ParserActionWithNextAction ActionWithNextAction))
                  continue; // no chain

               // mandatory: 
               //   keep action -> nonterminal transition -> definition
               //   replace action -> nonterminal transition >= other action
               //        by action -> other action
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

            // Remove states which have ben skipped by optimization
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
               // IndexOfNextState must not be incremented because the next tate has been moved o the place of the deleted state
            }
         }

         // Simplify next action of reductions
         foreach (ReduceAction r in GlobalVariables.ListOfAllReductions)
         {
            r.NextAction = SimplifiedNextAction(r.NextAction);
            SimplifyChainOfReductions(r); // CHECK compute x.Calls and simplify using x.Calls ??
         }

         // Simplify naxt action of branch actions
         foreach (BranchAction branch in GlobalVariables.ListOfAllBranchActions)
         {
            for (Int32 CaseIndex = 0; CaseIndex < branch.ListOfCases.Count; CaseIndex++)
            {
               BranchcaseStruct Branchcase = branch.ListOfCases[CaseIndex];
               Branchcase.BranchcaseAction = SimplifiedNextAction(Branchcase.BranchcaseAction);
               branch.ListOfCases[CaseIndex] = Branchcase;
            }
         }

         //do not remove the first state: not  SimplifiedNextAction(GlobalVariables.ListOfAllStates[0]);
         // because it is used as Startaction
      }

      private void P4cRemoveNotLongerUsedActionsFromStatesAddErrorActionsComputeComplexity()
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
      }

      private void MoveStateStackPushOperations()
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
          * TODO may be done later: replace the restrictions "a pops ... and a is called only once" 
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
               if (!(a is ConditionalAction c) || a is PrioritySelectAction)
               {
                  // do not apply this optimization to states which contain other actions than conditional actions
                  // or which contain PrioritySelectActions (see MoveStateStackPush(...))
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
               MoveStateStackPushOperation(State);


         }
      }

      private void MoveStateStackPushOperation(ParserState state)
      {
         for (Int32 ActionIndex = 0; ActionIndex < state.Actions.Count; ActionIndex++)
         {
            ConditionalAction c = (ConditionalAction)state.Actions[ActionIndex];

            var cNext = c.NextAction;
            if (cNext.StateStackAdjustment >= 1 &&
               cNext.AcceptCalls <= 1 && cNext.Calls == 1)
            {
               // Combine with cNext
               cNext.StateStackAdjustment--;
            }
            else
            {
               // Insert PushStateAction
               var pAction = new PushStateAction
                  (GlobalVariables.ListOfAllPushStateActions.Count, state.StateStackNumber, cNext);
               GlobalVariables.ListOfAllPushStateActions.Add(pAction);

               pAction.Calls = 1;

               if (c is TerminalTransition)
               { // accept call of p
                  pAction.AcceptCalls = 1;
                  cNext.AcceptCalls -= 1;
                  if (cNext.AcceptCalls == 0)
                     cNext.Calls--; // cNext no longer called by its own accept
               }
               cNext.Calls++; // cNext called by pAction

               c.NextAction = pAction;

            }

            // TODO States with PrioritySelectAction are excluded from this optimization ('cause it's more complicated to handle their next actions)
         }

         // The state does not longer push on the state stack
         state.StateStackNumber = -state.StateStackNumber - 2;
      }
   }
}