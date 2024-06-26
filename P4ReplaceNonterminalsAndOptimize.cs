﻿using IndexSetNamespace;

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace grammlator;

internal sealed class P4ReplaceNonterminalsAndOptimize
{
   /* Phase 4 checks all states and handles all actions which would cause "apply definition" 
    *    (execute semantic method and go back to a state in the state stack, input 
    *     the generated nonterminal symbol and "shift" to the follow state)
    * and replaces each one by an explicit "reduction" which adjusts the state stack
    * and may be followed by a "branch" which uses the value on top
    * of the state stack to decide which is the follow state (or action after optimzation).
    * It recognizes states which need to push their number on
    * the state stack (assigns the states IdNumber), 
    * then tries to assign the same (small) number to different states,
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

      GlobalVariables.Startaction = GlobalVariables.ListOfAllStates[0];

      //Shorten chains of actions and remove states which have been skipped by optimization
      P4bShortenChainsOfActionsDeleteNonterminalTransitions();

      // Now  ListOfAllStates[0] is definitely assigned and GlobalVariables.Startaction may be defined and used
      P4cRemoveNotLongerUsedActionsFromStates();

      // For each Parseraction compute the number of AcceptCalls and Calls
      GlobalVariables.Startaction.CountUsage(Accept: false);

      // 
      p4.P4ePropagateLookAheadTerminals();

      P4fAddErrorhandlingActionsAndComputeComplexityOfStates();

      // Move push operations from states to actions if this reduces the number of those operations
      P4gMoveStateStackPushOperations();
   }

   /// <summary>
   /// Constructor: assigns the partition / relations <see cref="SamePredecessorLevelPartitionOfStates"/>,
   ///  and <see cref="DistinguishableStatesRelation"/>
   /// </summary>
   private P4ReplaceNonterminalsAndOptimize()
   {
      SamePredecessorLevelPartitionOfStates
         = new PartitionInfoArray<ParserState>(GlobalVariables.ListOfAllStates);
      DistinguishableStatesRelation // a symmetric relation implemented by a set of pairs
          = new SortedSet<(int SmallerIdNr, int LargerIdNr)>(new SortPairOfStatesBylargerThenBySmaller());
      // SortedSet is easy to use but allocates a node for each entry
   }

   public sealed class SortPairOfStatesBylargerThenBySmaller : Comparer<(int smaller, int larger)>
   {
      public override int Compare((int smaller, int larger) a, (int smaller, int larger) b)
      {
         return a.larger.CompareTo(b.larger) != 0
            ? a.larger.CompareTo(b.larger)
            : a.smaller.CompareTo(b.smaller);
      }
   }

   private readonly SortedSet<(int SmallerIdNr, int LargerIdNr)> DistinguishableStatesRelation;

   /// <summary>
   /// All states belonging to the same class of <see cref="SamePredecessorLevelPartitionOfStates"/>
   /// either all do not push to the stack of states in the generated code or all push
   /// </summary>
   private readonly PartitionInfoArray<ParserState> SamePredecessorLevelPartitionOfStates;

   /// <summary>
   /// Lists of states used in in <see cref="CombineClassesOfSamePredecessorLevelPartitionOfStates"/>,
   /// <see cref="ComputeDistinguishableStatesRelation"/> and  <see cref="ReplaceDefinitionsByReductionsAndBranches/>
   /// </summary>
   List<ParserState>
      StatesOfThisLevel =
          new(GlobalVariables.ListOfAllStates.Count),
      StatesOfNextLevel =
          new(GlobalVariables.ListOfAllStates.Count);


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
      ComputeDistinguishableStatesRelation();

      // DetermineNotStackingStates using SamePredecessorLevelPartitionOfState and DistinguishableStatesRelation
      GlobalVariables.NumberOfStatesWithStateStackNumber = AssignStateStackNumbersAndCountPushingStates();

      // Optionally replace StateStackNumbers by smaller numbers
      if (GlobalSettings.GenerateSmallStateStackNumbers.Value)
      {
         AssignStatesSmallerStackNumbers();
      }

      // Compute branches
      ReplaceDefinitionsByReductionsAndBranches();
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
                      Depth: definition.Elements.Length + ((action is LookaheadAction) ? 0 : -1),
                      StatesOfThisLevel,
                      StatesOfNextLevel
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
                     throw new NotImplementedException("Constant priority action in PriorityBranchAction not yet implemented");
                  }
                  foreach (ParserAction dynamicPriorityAction in b.DynamicPriorityActions)
                  {
                     Debug.Assert(dynamicPriorityAction is Definition);
                     if (dynamicPriorityAction is Definition d)
                        CombineClassesOfSamePredecessorLevelPartitionOfStates(
                            StateToStartFrom: state,
                            Depth: d.Elements.Length,
                            StatesOfThisLevel,
                            StatesOfNextLevel
                            );
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
   /// <param name="StatesOfThisLevel">temporary vairaialbe</param>
   /// <param name="StatesOfNextLevel">temporary variable</param>
   private void CombineClassesOfSamePredecessorLevelPartitionOfStates(
      ParserState StateToStartFrom, 
      Int32 Depth,
      List<ParserState> StatesOfThisLevel,
      List<ParserState> StatesOfNextLevel
)
   {
      // start with StateToStartFrom as single element of the set of states to evaluate 
      // and walk back 
      StatesOfThisLevel.Clear();
      StatesOfThisLevel.Add(StateToStartFrom);

      StatesOfNextLevel.Clear();

      // Iterate Depth levels
      for (Int32 remainingLength = Depth; remainingLength > 0; remainingLength--)
      {
         // Add all predecessors of all states of this level to the list of preceding states
         foreach (ParserState stateToEvaluate in StatesOfThisLevel)
         {
            foreach (ParserState predecessor in stateToEvaluate.PredecessorList!)
               if (!StatesOfNextLevel.Contains(predecessor))
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
         (StatesOfNextLevel, StatesOfThisLevel) = (StatesOfThisLevel, StatesOfNextLevel);
         StatesOfNextLevel.Clear();
      }

      StatesOfThisLevel.Clear();
   }

   private void ComputeDistinguishableStatesRelation()
   {
      foreach (ParserState state in GlobalVariables.ListOfAllStates)
         foreach (ParserAction action in state.Actions.PriorityUnwindedSetOfActions)
         {
            if ((action is ParserActionWithNextAction actionWithNextAction)
                && (actionWithNextAction.NextAction is Definition definition))
            {
               _ = FindPrecedingStates(
                        StateToStartFrom: state,
                        Depth: definition.Elements.Length + ((action is LookaheadAction) ? 0 : -1),
                        StatesOfThisLevel:
                        // In terminal and nonterminal shifts the item is one position before the end:
                        // depth must be one less than the length of the definitions elementlist
                        ref StatesOfThisLevel,
                        ref StatesOfNextLevel
                        );
               ComputeDistinguishableStatesRelation(
                  statesWhichAcceptTheNonterminal: StatesOfThisLevel,
                  inputSymbol: definition.DefinedSymbol!);
            }
         }
   }

   /// <summary>
   /// Visit all states, take all actions a which apply a definition,
   /// and find all predecessor states up to a search depth given by the definitions length.
   /// Replace a by a <see cref="ReduceAction"/> followed by a <see cref="BranchAction"/> or
   /// an optimized version of those
   /// </summary> 
   private void ReplaceDefinitionsByReductionsAndBranches()
   {
      foreach (ParserState state in GlobalVariables.ListOfAllStates)
         foreach (ParserAction action in state.Actions.PriorityUnwindedSetOfActions)
         {
            if ((action is ParserActionWithNextAction actionWithNextAction)
                && (actionWithNextAction.NextAction is Definition definition))
            {
               Debug.Assert(actionWithNextAction is ConditionalAction && actionWithNextAction.NextAction is Definition);

               Int32 countOfStateStackNumbersToPop =
                   FindPrecedingStates(
                       StateToStartFrom: state,
                       Depth: definition.Elements.Length + ((action is LookaheadAction) ? 0 : -1),
                       StatesOfThisLevel: ref StatesOfThisLevel,
                       StatesOfNextLevel: ref StatesOfNextLevel
                       )!;

               BranchcasesList SortedListOfBranchcases =
                  ConstructListOfBranchcases(StatesOfThisLevel, definition.DefinedSymbol!);

               StatesOfThisLevel.Clear();

               ParserAction NextActionOfReduce = // branch or action
                       SortedListOfBranchcases.Count == 1
                       ? SortedListOfBranchcases[0].BranchcaseAction // no branch, single action
                       : FindOrConstructBranch(SortedListOfBranchcases);

               // Debug.Assert(!(NextAction is NonterminalTransition));
               Debug.Assert(NextActionOfReduce is not Definition); // CHECK assertions ???????

               actionWithNextAction.NextAction =
                  (definition.HasNoSemantics() && (countOfStateStackNumbersToPop == 0)
                   && definition.DefinedSymbol != GlobalVariables.Startsymbol)
                  ? NextActionOfReduce
                  : MakeReduceAction(
                        countOfStateStackNumbersToPop,
                        definition,
                        NextActionOfReduce: NextActionOfReduce);

               Debug.Assert(actionWithNextAction.NextAction is not Definition);
               // actionWithNextAction.NextAction may be NonterminalTransition;
            }
         }
   }

   /// <summary>
   /// Starts with <paramref name="StateToStartFrom"/> and walks <paramref name="Depth"/> levels back.
   /// Return the found states in <paramref name="StatesOfThisLevel"/>
   /// </summary>
   /// <param name="StateToStartFrom">state from where to walk back</param>
   /// <param name="Depth">number of levels to walk back</param>
   /// <param name="StatesOfNextLevel">A reference to a list of states,
   /// which will be used as temporary storage and will be assigned the list of
   /// states found at the given <paramref name="Depth"/>.</param>
   /// <param name="StatesOfThisLevel"> A reference to a list of states which will be used as temporary storage.</param>
   /// <returns> The number of states x encountered on the way back with  x.StateStackNumber >= 0
   /// </returns>
   private static Int32 FindPrecedingStates(
       ParserState StateToStartFrom,
       Int32 Depth,
       ref List<ParserState> StatesOfThisLevel,
       ref List<ParserState> StatesOfNextLevel
       )
   {
      StatesOfThisLevel.Clear();
      StatesOfThisLevel.Add(StateToStartFrom);

      StatesOfNextLevel.Clear();

      Int32 countOfStateStackNumbersToPop = 0; // used only if TypeOfEvaluation==RelationtypeEnum.ComputeBranches

      // Iterate depth levels
      for (Int32 remainingDepth = Depth; remainingDepth > 0; remainingDepth--)
      {
         // Add the predecessors of all states of this level to the list of states of the next level
         foreach (ParserState state in StatesOfThisLevel)
         {
            foreach (ParserState predecessor in state.PredecessorList!)
            {  // Contains performs a linear search therefore it is an O(n) operation.
               // Because the list typically is very small BinarySearch, an O(log n) operation, will not be faster
               if (!StatesOfNextLevel.Contains(predecessor))
                  StatesOfNextLevel.Add(predecessor);
            }
         }

         Debug.Assert(StatesOfNextLevel.Count != 0, "Error in phase4: no predecessor)");

         // After StateStackNumbers have been assigned (!)
         // either the StateStackNumber of all predecessors is >=0 or of all is <0
         // States with <=0 won't push and are not counted 
         // if (TypeOfEvaluation == RelationtypeEnum.ComputeBranches && ..
         if (StatesOfThisLevel[0].StateStackNumber >= 0)
         {
            countOfStateStackNumbersToPop++;
         }

         // The contents of statesToEvaluate is no longer needed, the lists capacity will be reused
         StatesOfThisLevel.Clear();

         (StatesOfNextLevel, StatesOfThisLevel) = (StatesOfThisLevel, StatesOfNextLevel);
      }

      /*  statesOfThisLevel contains all states which are reachable from the given state
       *  by going back depth levels.
       */

      return countOfStateStackNumbersToPop;
   }

   /// <summary>
   /// Check which states have to be distinguished from each other,
   /// assign StateStackNumber and set the DistinguishableStatesRelation 
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

            if (!ActionsDoTheSame(State1, State2, nextAction1, Action2))
            {
               // Set each ones Codenumber and add pair to the the DistinguishableStatesRelation
               State1.StateStackNumber = State1.IdNumber;
               State2.StateStackNumber = State2.IdNumber;
               if (State1.IdNumber < State2.IdNumber)
                  DistinguishableStatesRelation.Add((State1.IdNumber, State2.IdNumber));
               else
                  DistinguishableStatesRelation.Add((State2.IdNumber, State1.IdNumber));
            }
         }
      }
   }

   /// <summary>
   /// Without optimization each generated state pushes its number on the state stack.
   /// This method finds all states that have to push a number on the state stack
   /// and sets their StateStackNumber to the states IdNumber
   /// </summary>
   /// <returns>number of states with StateStackNumber == -1</returns>
   private Int32 AssignStateStackNumbersAndCountPushingStates()
   {
      Int32 PushingStatesCount = 0;

      // for all classes of the SamePredecessorLevelPartitionOfStates
      foreach (ParserState classRepresentative in SamePredecessorLevelPartitionOfStates.ClassRepresentatives)
      {
         // Search a member of this class that must push a number on the states stack
         Boolean foundPushing = false;
         foreach (ParserState Neighbor in SamePredecessorLevelPartitionOfStates.ElementsOfClass(classRepresentative))
         {
            if (Neighbor.StateStackNumber >= 0)
            {
               foundPushing = true;
               break;
            }
         }

         if (!foundPushing)
            continue;

         // At least one states of the partitions class  has to push a value on the state stack =>
         // Each member of this class must push a number.
         // Each members StateStackNumber ist set to its IdNumber
         foreach (ParserState Neighbor in SamePredecessorLevelPartitionOfStates.ElementsOfClass(classRepresentative))
         {
            PushingStatesCount++; // count all elements of this class
            Neighbor.StateStackNumber = Neighbor.IdNumber;
         }

      }

      return PushingStatesCount;
   }

   /// <summary>
   /// Construct a sorted list (action/StateStacknumber) which for each state contains
   /// the action resulting if the inputSymbol is input into this state.
   /// If all actions are equivalent remove all except the first entries from the list.
   /// </summary>
   /// <param name="StatesAcceptingThisSymbol">List of parserstates accepting the <paramref name="inputSymbol"/>"/></param>
   /// <param name="inputSymbol">The nonterminal symbol which is allowed as input in each of the states</param>
   /// <returns></returns>
   private static BranchcasesList ConstructListOfBranchcases(
      List<ParserState> StatesAcceptingThisSymbol, NonterminalSymbol inputSymbol)
   {
      var ListOfCases = new BranchcasesList(StatesAcceptingThisSymbol.Count);

      ParserState State1 = StatesAcceptingThisSymbol[0];
      ParserAction Action1 = State1.ActionCausedBy(inputSymbol).Simplify();

      Debug.Assert(Action1 is not LookaheadAction l || l.NextAction is not NonterminalTransition);

      // Add the first case
      AddCase(ListOfCases, State1.StateStackNumber, Action1);

      Boolean Equivalent = true;

      // for each other state in the list of states
      for (Int32 StateIndex = 1; StateIndex < StatesAcceptingThisSymbol.Count; StateIndex++)
      {
         // Find the action caused by inputSymbol and add it to the list of cases
         ParserState StateI = StatesAcceptingThisSymbol[StateIndex];

         NonterminalTransition NT = StateI.ActionCausedBy(inputSymbol);
         ParserAction ActionI;
         if (NT.NextAction is Definition)
            ActionI = NT;
         else
            ActionI = NT.NextAction.Simplify();

         Debug.Assert(ActionI is not Definition);

         Debug.Assert(ActionI is not LookaheadAction la || la.NextAction is not NonterminalTransition);

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

   private static readonly StringBuilder reduceStringBuilder = new(80);

   /// <summary>
   /// Constructs a <see cref="ReduceAction"/> with special handling of the Startsymbol.
   /// </summary>
   /// <param name="stateStackAdjustment">ge 0</param>
   /// <param name="Definition"></param>
   /// <param name="NextActionOfReduce"></param>
   /// <returns>Returns the reduce action or an </returns>
   private static ReduceAction MakeReduceAction(
       Int32 stateStackAdjustment,
       Definition Definition,
       ParserAction NextActionOfReduce)
   {
      Debug.Assert(stateStackAdjustment >= 0);
      // Debug.Assert(!(NextActionOfReduce is NonterminalTransition)); // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


      // Compose description
      Definition.DefinedSymbol!
         .IdentifierAndAttributesToSB(reduceStringBuilder)
         .Append("= ");
      Definition
          .ElementsToStringbuilder(reduceStringBuilder, Definition!.Elements.Length + 1);
      String description = reduceStringBuilder.ToString();
      reduceStringBuilder.Clear();

      // construct reduceAction
      ReduceAction newReduceAction = new(NextActionOfReduce)
      {
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
            newReduceAction.NextAction = GlobalVariables.EndOfGeneratedCodeInstance;
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

      // Debug.Assert(!(newReduceAction.NextAction is NonterminalTransition)); // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      newReduceAction.NextAction = newReduceAction.NextAction.Simplify();

      // Debug.Assert(!(newReduceAction.NextAction is NonterminalTransition)); // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      SimplifyChainOfReductions(newReduceAction);

      // Debug.Assert(!(newReduceAction.NextAction is NonterminalTransition)); // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


      // search equivalent ReduceAction ignoring match of the descriptions
      ReduceAction? foundReduceAction = SearchEquivalentReduceAction(newReduceAction);

      if (foundReduceAction != null)
      {
         // Has descriptions to be expanded?
         Boolean containsDescription = foundReduceAction.Description.Contains(newReduceAction.Description);
         if (containsDescription)
            return foundReduceAction;

         // The if condition avoids combinations of trivial halt actions resulting in lengthy descriptions
         // if(true) should generate the same code except comments (example: P1bLexer)
         ParserAction NextAction = newReduceAction.NextAction;
         if (!(NextAction is EndOfGeneratedCodeAction || NextAction is HaltAction)
            || newReduceAction.SemanticMethod != null
            || newReduceAction.StateStackAdjustment != 0
            // || newReduceAction.AttributeStackAdjustment != 0 // will always be 0, see above
            )
         {
            reduceStringBuilder.AppendLine(foundReduceAction.Description);
            reduceStringBuilder.Append("or: ");
            reduceStringBuilder.Append(newReduceAction.Description);
            foundReduceAction.Description = reduceStringBuilder.ToString();
            reduceStringBuilder.Clear();
            return foundReduceAction;
         }
      }

      // nicht gefunden => aufnehmen
      GlobalVariables.ListOfAllReductions.Add(newReduceAction);
      return newReduceAction;
   }

   private static ReduceAction? SearchEquivalentReduceAction(ReduceAction newReduceAction)
   {
      for (Int32 ir = 0; ir < GlobalVariables.ListOfAllReductions.Count; ir++)
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
         existingReduceAction.NextAction = existingReduceAction.NextAction.Simplify();
         SimplifyChainOfReductions(existingReduceAction);
         // end of simple solution

         if (
         existingReduceAction.StateStackAdjustment == newReduceAction.StateStackAdjustment
         && existingReduceAction.SemanticMethod == newReduceAction.SemanticMethod
         && existingReduceAction.AttributeStackAdjustment == newReduceAction.AttributeStackAdjustment
         && existingReduceAction.NextAction == newReduceAction.NextAction
         && existingReduceAction.NextAction is not Definition
         )
         {   //found 
            return existingReduceAction;
         }
      }

      return null;
   }

   /// <summary>
   /// Test if <paramref name="Reduction"/>.NextAction is also a <see cref="ReduceAction"/> and combine them if possible
   /// </summary>
   /// <param name="Reduction"></param>
   private static void SimplifyChainOfReductions(ReduceAction Reduction)
   {
      if (Reduction.NextAction is not ReduceAction nextReduction)
         return;

      // The NextAction of Reduction is also a ReduceAction, both may be combined

      // TODO add correct handling of semantic PriorityFunction

      // Do not combine, if this will cause an additional AttributeStackAdjustment
      if (Reduction.AttributeStackAdjustment == 0 && nextReduction.AttributeStackAdjustment != 0)
         return;

      if (Reduction.SemanticMethod != null && nextReduction.SemanticMethod != null)
         return; // // both have semantic methods: do not combine

      if (Reduction.SemanticMethod == null && nextReduction.SemanticMethod == null)
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

   /// <summary>
   /// Finds a branch with a list equal to the new list or containing the new list or contained in the new list (and be expanded).
   /// Else a new branch will be constructed and returned.
   /// </summary>
   /// <param name="newSortedListOfCases"></param>
   /// <returns></returns>
   public static BranchAction FindOrConstructBranch(BranchcasesList newSortedListOfCases)
   {
      // Not implemented: combine branches with compatible cases

      // search in all existing branches after normalizing ("simplyfying") the actions of the existing branches
      foreach (BranchAction ExistingBranch in GlobalVariables.ListOfAllBranchActions)
      {
         BranchcasesList ExistingListOfCases = ExistingBranch.ListOfCases;

         for (Int32 i = 0; i < ExistingListOfCases.Count; i++)
         {
            BranchcaseStruct c = ExistingListOfCases[i];
            c.BranchcaseAction = c.BranchcaseAction.Simplify();
            ExistingListOfCases[i] = c;
         }


         BranchcasesList.CompareResult cr = ExistingListOfCases.Containing(newSortedListOfCases);
         if (cr == BranchcasesList.CompareResult.contains || cr == BranchcasesList.CompareResult.equal)
            return ExistingBranch;

         if (cr == BranchcasesList.CompareResult.isContained)
         {  // the existing branch is contained in the new one: modify it
            ExistingBranch.ListOfCases = newSortedListOfCases;
            return ExistingBranch;
         }

      }

      // all existing branches have been checked and no one contains the new list of cases

      // Construct a new branch
      var NewBranch = new BranchAction(
          ListOfCases: new BranchcasesList(newSortedListOfCases),
          IdNumber: GlobalVariables.ListOfAllBranchActions.Count);

      GlobalVariables.ListOfAllBranchActions.Add(NewBranch);

      return NewBranch;
   } // end of FindOrConstructBranch

   public void AssignStatesSmallerStackNumbers()
   {
      var AllowedStackNumbers = new Boolean[GlobalVariables.ListOfAllStates.Count];

      // 1st: Set all StateStackNumbers which are >-1 to 0 
      foreach (ParserState ParserState in GlobalVariables.ListOfAllStates
      .Where((state) => state.StateStackNumber > 0))
         ParserState.StateStackNumber = 0;

      // 2nd: In increasing order find a StateStackNumber for each of them
      // which is not in conflict with numbers of already assigned numbers to distinguishable states
      foreach (ParserState ParserState in GlobalVariables.ListOfAllStates
      .Where((state) => state.StateStackNumber >= 0))
      {
         SortedSet<(int SmallerIdNr, int LargerIdNr)> DistinguishableStatesRelationOfParserState =
            DistinguishableStatesRelation.GetViewBetween
            ((-1, ParserState.IdNumber), (-1, ParserState.IdNumber + 1));

         // Start with: assume all numbers from 0 to the states IdNumber are not in conflict
         for (Int32 k = 0; k <= ParserState.IdNumber; k++)
            AllowedStackNumbers[k] = true;

         // Remove StateStackNumbers assigned to related states with smaller IdNumber 
         foreach ((int SmallerIdNr, int LargerIdNr) in DistinguishableStatesRelationOfParserState)
         {
            AllowedStackNumbers[GlobalVariables.ListOfAllStates[SmallerIdNr].StateStackNumber] = false;
         }

         ParserState.StateStackNumber = Array.FindIndex(AllowedStackNumbers, (value) => value);

      }
   }

   /// <summary>
   /// Shorten chains of actions
   /// </summary>
   public static void P4bShortenChainsOfActionsDeleteNonterminalTransitions()
   {
      ParserAction DeletedAction = new DeletedParserAction();
      for (Int32 StateIndex = 0; StateIndex < GlobalVariables.ListOfAllStates.Count; StateIndex++)
      {
         ParserState State = GlobalVariables.ListOfAllStates[StateIndex];

         for (Int32 ActionIndex = 0; ActionIndex < State.Actions.Count; ActionIndex++)
         {
            if (State.Actions[ActionIndex] is not ParserActionWithNextAction ActionWithNextAction)
               continue; // no chain

            if (ActionWithNextAction is NonterminalTransition)
            {
               State.Actions[ActionIndex] = DeletedAction; // delete nonterminal transitions
               continue; // 
            }

            ActionWithNextAction.NextAction = ActionWithNextAction.NextAction.Simplify();
            // do not replace ActionWithNextAction: this would replace conditional actions

            // if State.Actions contains different TerminalTransitions with the same NextAction then combine these
            if (State.Actions[ActionIndex] is TerminalTransition ActionAsTerminalTransition)
            {
               for (Int32 ActionToCompareIndex = 0; ActionToCompareIndex < ActionIndex; ActionToCompareIndex++)
               {
                  if (State.Actions[ActionToCompareIndex] is not TerminalTransition ActionToCompare)
                     continue;

                  if (ActionToCompare.NextAction == ActionAsTerminalTransition.NextAction)
                  {
                     ActionToCompare.TerminalSymbols.UnionWith(ActionAsTerminalTransition.TerminalSymbols);
                     State.Actions[ActionIndex] = DeletedAction;
                     break;
                  }
               }
            }
         }
      }

      // Simplify next action of reductions
      foreach (ReduceAction r in GlobalVariables.ListOfAllReductions)
      {
         r.NextAction = r.NextAction.Simplify();
         SimplifyChainOfReductions(r); // CHECK compute x.Calls and simplify using x.Calls ??
      }

      // Simplify next action of branch actions
      foreach (BranchAction branch in GlobalVariables.ListOfAllBranchActions)
      {
         for (Int32 CaseIndex = 0; CaseIndex < branch.ListOfCases.Count; CaseIndex++)
         {
            BranchcaseStruct Branchcase = branch.ListOfCases[CaseIndex];
            Branchcase.BranchcaseAction = Branchcase.BranchcaseAction.Simplify();
            branch.ListOfCases[CaseIndex] = Branchcase;
         }
      }

      GlobalVariables.Startaction = GlobalVariables.Startaction.Simplify();
   }

   private static void P4cRemoveNotLongerUsedActionsFromStates()
   {
      // for each state
      foreach (ParserState State in GlobalVariables.ListOfAllStates)
      {
         // Remove not longer used actions of this state
         Int32 DeletedActionsCount = 0;

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
               // Shift this remaining action by DeletedActionsCount places to the front
               // and replace the unused action in State.Actions with this action
               if (DeletedActionsCount > 0)
                  State.Actions[ActionIndex - DeletedActionsCount] = State.Actions[ActionIndex];
            }
         }

         // Remove all deleted actions of this state
         if (DeletedActionsCount > 0)
            State.Actions.RemoveFromEnd(DeletedActionsCount);
      }
   }

   /// <summary>
   /// Propagate all look ahead terminal symbols of all actions  of all states
   /// to states with AcceptCalls==0.
   /// For all states with AcceptCalls>0
   /// set <see cref="state.PossibleInputTerminals"/> to all terminal symbols. 
   /// </summary>
   private void P4ePropagateLookAheadTerminals()
   {
      /* There may be states which are reached only via look ahead parseractions.
       * These states can only see terminals that have been tested already.
       * If such a state has actions for all of the visible terminals
       * then it does not need an error action.
       * */

      // Preset PossibleInputTerminals for each state
      foreach (ParserState state in GlobalVariables.ListOfAllStates.Where(s => s.Calls > 0))
         if (state.AcceptCalls > 0)
            state.PossibleInputTerminals = new IndexSet(GlobalVariables.AllTerminalSymbols);
         else
            state.PossibleInputTerminals = new IndexSet(GlobalVariables.NumberOfTerminalSymbols);

      // Propagate from states with PossibleInputTerminals set to AllTerminalSymbols
      foreach (ParserState state in GlobalVariables.ListOfAllStates.Where(s => s.AcceptCalls > 0))
         PropagateFromState(state);

      // Propagate from Startaction
      Propagate(GlobalVariables.Startaction!, GlobalVariables.AllTerminalSymbols);

   }

   private void PropagateFromState(ParserState state)
   {
      foreach (ParserAction action in state.Actions.PriorityUnwindedSetOfActions)
      {
         switch (action)
         {
            case TerminalTransition t:
               Propagate(t.NextAction, GlobalVariables.AllTerminalSymbols);
               break;
            case AcceptAction aa:
               Propagate(aa.NextAction, GlobalVariables.AllTerminalSymbols);
               break;
            case LookaheadAction la:
               Propagate(la, la.TerminalSymbols);
               break;
            case ReduceAction r:
               Propagate(r, state.PossibleInputTerminals!);
               break;
            case BranchAction b:
               Propagate(b, state.PossibleInputTerminals!);
               break;
            case ParserState nextState:
               Propagate(nextState, state.PossibleInputTerminals!);
               break;
            case HaltAction _:
            case ErrorHaltAction _:
            case EndOfGeneratedCodeAction _:
               return; // does not propagate to a state
            case PrioritySelectAction _: // handled by PriorityUnwindedSetOfActions and must not occur
            case PriorityBranchAction _:
            case NonterminalTransition _: // has already been removed
            case DeletedParserAction _:
            default:
               throw new ArgumentException(nameof(action));
         }
      }
   }

   Int32 PropagateRecursionDepth; // = 0; // TODO avoid recursion in a correct way
   readonly IndexSet test = new(GlobalVariables.NumberOfTerminalSymbols);

   private void Propagate(ParserAction? action, IndexSet terminals)
   {
      if (action == null)
         return;

      if (PropagateRecursionDepth > 100) // TODO make Propagate a method of the actions
         return;

      PropagateRecursionDepth++;

      switch (action)
      {
         // TerminalTransition propagates AllTerminalSymbols:  accepts input followed by Peek
         case TerminalTransition t:
            Propagate(t.NextAction, GlobalVariables.AllTerminalSymbols);
            break;
         // conditional actions propagate the terminals of their condition:
         case AcceptAction aa:
            Propagate(aa.NextAction, GlobalVariables.AllTerminalSymbols);
            break;
         case LookaheadAction la:
            IndexSet TerminalsToPropagate = la.TerminalSymbols;
            Propagate(la.NextAction, la.TerminalSymbols);
            break;
         case ReduceAction r:
            Propagate(r.NextAction, terminals);
            break;
         case BranchAction b:
            if (b.PossibleInputTerminals.Length == 0)
               b.PossibleInputTerminals = new IndexSet(terminals);
            else
            {
               if (test.CopyFrom(terminals).ExceptWith(b.PossibleInputTerminals!).IsEmpty)
                  break; // PossibleInputTerminals >= terminals
               b.PossibleInputTerminals.UnionWith(terminals);
            }
            foreach (ParserAction a in b.ListOfCases.Select(c => c.BranchcaseAction))
               Propagate(a, terminals!);
            break;
         case ParserState state:
            if (state.AcceptCalls > 0)
               break; // handled in P4ePropagateLookAheadTerminals()
            if (test.CopyFrom(terminals).ExceptWith(state.PossibleInputTerminals!).IsEmpty)
               break; // PossibleInputTerminals >= terminals

            _ = state.PossibleInputTerminals!.UnionWith(terminals);
            PropagateFromState(state);
            break;
         case HaltAction _:
         case ErrorHaltAction _:
         case EndOfGeneratedCodeAction _:
            break; // does not propagate to a state
         case PrioritySelectAction _: // handled by PriorityUnwindedSetOfActions and must not occur
         case PriorityBranchAction _:
         case NonterminalTransition _: // has already been removed
         case DeletedParserAction _:
         default:
            throw new ArgumentException("unknow ParserAction", nameof(action));
            ;
      }

      PropagateRecursionDepth--;
      return;
   }

   private static void P4fAddErrorhandlingActionsAndComputeComplexityOfStates()
   {
      foreach (ParserState State in GlobalVariables.ListOfAllStates)
      {
         if (State.Calls == 0)
            continue;

         static Int32 max(Int32 a, Int32 b)
             => a > b ? a : b;

         Int32 MaximumActionComplexity = 0;
         Int32 ActionComplexitySum = 0;

         for (Int32 ActionIndex = 0; ActionIndex < State.Actions.Count; ActionIndex++)
         {

            // Compute Terminalcount and SumOfWeights of the action
            if (State.Actions[ActionIndex] is ConditionalAction c)
            {
               c.ComputeTerminalcountSumOfWeightsComplexity(GlobalVariables.TerminalSymbols);
               MaximumActionComplexity = max(MaximumActionComplexity, c.Complexity);
               ActionComplexitySum += c.Complexity;
            }
         }

         // Add error handling action or LookAheadAction
         ConditionalAction? e = State.CheckAndAddErrorAction();

         if (e != null)
         {
            MaximumActionComplexity = max(MaximumActionComplexity, e.Complexity);
            ActionComplexitySum += e.Complexity;
         }

         /* Ignore the MaximumActionComplexity because the respective action
          * might be generated as last unconditional action
          */
         State.IfComplexity = ActionComplexitySum - MaximumActionComplexity;
      }
   }

   private static void P4gMoveStateStackPushOperations()
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
       *      and if approriate do the modifications.
       *      Modifications of ErrorHalt actions can be ommited because they
       *      will reset the stack.
       *      
       * TODO may be done later: replace the restrictions "a pops ... and a is called only once" 
       *      by some less restrictive condition, follow chains of ParserActions, consider splitting reduce actions
       */

      for (Int32 StateIndex = 0; StateIndex < GlobalVariables.ListOfAllStates.Count; StateIndex++)
      {
         ParserState State = GlobalVariables.ListOfAllStates[StateIndex];
         if (State.StateStackNumber < 0)
            continue;

         Int32 removedPopCounter = 0, addedPushStateCounter = 0;

         for (Int32 ActionIndex = 0; ActionIndex < State.Actions.Count; ActionIndex++)
         {
            ParserAction a = State.Actions[ActionIndex];
            if (a is not ConditionalAction c || a is PrioritySelectAction)
            {
               // do not apply this optimization to states which contain other actions than conditional actions
               // or which contain PrioritySelectActions (see MoveStateStackPush(...))
               removedPopCounter = 0;
               break;
            }

            var cNext = c.NextAction;
            if (cNext.StateStackAdjustment >= 1 &&
               cNext.AcceptCalls <= 1 && cNext.Calls == 1)
            {
               if (cNext.StateStackAdjustment == 1)
                  removedPopCounter++;
            }
            else if (!(cNext.ParserActionType == ParserActionEnum.isErrorhaltAction))
               addedPushStateCounter++;
         }

         if (removedPopCounter > 0 && (removedPopCounter + 1) >= addedPushStateCounter)
            MoveStateStackPushOperation(State);
      }
   }

   private static void MoveStateStackPushOperation(ParserState state)
   {
      for (Int32 ActionIndex = 0; ActionIndex < state.Actions.Count; ActionIndex++)
      {
         ConditionalAction c = (ConditionalAction)state.Actions[ActionIndex];

         var cNext = c.NextAction;
         if (cNext.StateStackAdjustment >= 1 &&
            cNext.AcceptCalls <= 1 && cNext.Calls == 1)
         {
            // Only called from c: combine with cNext
            cNext.StateStackAdjustment--;
         }
         else if (!(cNext.ParserActionType == ParserActionEnum.isErrorhaltAction))
         {
            // Insert PushStateAction
            var pAction = new PushStateAction
               (idNumber: GlobalVariables.ListOfAllPushStateActions.Count, state.StateStackNumber, cNext);
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
