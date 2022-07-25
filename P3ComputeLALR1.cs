using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

using GrammlatorRuntime;

using IndexSetNamespace;

namespace grammlator;

internal class P3ComputeLALR1
{
   /* Phase 3 computes LALR(1) Look-Ahead-Sets (see References: DeRemer and Penello) */

   /// <summary>
   /// <para>Computes look ahead sets Read and Follow of terminals symbols
   /// for the <see cref="ParserAction"/>s
   /// <see cref="LookaheadAction"/> and <see cref="NonterminalTransition"/>,</para>
   /// then solves conflicts by static priorities and removes no longer reachable states
   /// </summary>
   public static void MakeInstanceAndExecute(out Int32 sumOfConflictsNotSolvedByExplicitPriority)
   {
      var p3 = new P3ComputeLALR1();

      // compute look ahead terminal symbols
      AssignTerminalSymbolsAndComputeDirectRead();
      p3.P3a_ComputeReadSets();
      p3.P3b_ComputeFollow();

      // solve and log conflicts
      var sb = new StringBuilder(1000);
      sb.AppendLine("Protocol of conflicts and conflict resolution")
        .AppendLine();
      Int32 statesWithConflicts = P3c_FindAndResolveAllStaticConflicts(sb, out sumOfConflictsNotSolvedByExplicitPriority);

      GlobalVariables.OutputMessage(MessageTypeOrDestinationEnum.ConflictProtocol, sb.ToString());
      if (statesWithConflicts > 0)
         GlobalVariables.OutputMessage(
            MessageTypeOrDestinationEnum.Information,
            $"Found and resolved conflicts in {statesWithConflicts} state()s."
            );
      if (sumOfConflictsNotSolvedByExplicitPriority > 0)
         GlobalVariables.OutputMessage(MessageTypeOrDestinationEnum.Warning,
          $"{sumOfConflictsNotSolvedByExplicitPriority} conflict(s) have been solved by giving shift actions priority.");

   }

   /// <summary>
   /// make constructor private
   /// </summary>
   private P3ComputeLALR1() { }

   /// <summary>
   /// <para>For each <see cref="NonterminalTransition"/> compute the direct read set of terminal symbols
   /// and and assign it to its TerminalSymbols.</para>
   /// <para>For <see cref="LookaheadAction"/> assign the empty set to TerminalSymbols.
   /// For both set Codenumber to 0;</para>
   /// </summary>
   private static void AssignTerminalSymbolsAndComputeDirectRead()
   {
      foreach (ParserState state in GlobalVariables.ListOfAllStates)
      {
         foreach (LookaheadOrNonterminalTransition laOrNonterminalTansition
             in state.Actions.OfType<LookaheadOrNonterminalTransition>())
         {
            // Replace default assignment of parserAction.TerminalSymbols
            Debug.Assert(laOrNonterminalTansition.TerminalSymbols.IsEmpty);

            laOrNonterminalTansition.TerminalSymbols = new IndexSet(GlobalVariables.NumberOfTerminalSymbols);
            laOrNonterminalTansition.Codenumber = 0; // initial value for DIGRAPH algorithm

            if (laOrNonterminalTansition is not NonterminalTransition NonterminalTransition)
               continue;

            switch (NonterminalTransition.NextAction)
            {
               case ParserState FollowState:
                  {
                     // The nonterminal transition leads to a parser state.
                     // Compute its TerminalSymbols as direct read of the state,
                     // which is the set of all terminal symbols
                     // accepted by terminal transitions of the FollowState
                     if (FollowState.DirectRead.Length == 0)
                     {
                        foreach (TerminalTransition terminalTransition
                            in FollowState.Actions.OfType<TerminalTransition>())
                        {
                           NonterminalTransition.TerminalSymbols.UnionWith(terminalTransition.TerminalSymbols);
                        }
                        FollowState.DirectRead = new IndexSet(NonterminalTransition.TerminalSymbols);
                     }
                     else
                     {
                        NonterminalTransition.TerminalSymbols.UnionWith(FollowState.DirectRead);
                     }


                     break;
                  }

               case Definition _:
                  {
                     // The nonterminal transition leads to a reduction.
                     // The initial value of the (follow) terminal symbols is empty.
                     break;
                  }

               default: // case null:
                  {
                     // The nonterminal transition leads to a halt action.
                     // All terminal symbols may follow !
                     Debug.Assert(NonterminalTransition.NextAction == GlobalVariables.ListOfAllHaltActions[0]);
                     Debug.Assert((NonterminalTransition).InputSymbol.Identifier.ToString() == "*Startsymbol");

                     /* TODO   mark lookahead sets which look beyond the startsymbol
                      * e.g by adding a field to nonterminal transitions (and reductions and ...) 
                      * and use this flag in conflict analysis 
                      * */
                     NonterminalTransition.TerminalSymbols.UnionWith(GlobalVariables.AllTerminalSymbols);
                     break;
                  }
            }
         } // foreach
      } // foreach
   }

   /// <summary>
   /// Stack of nonterminal transitions used in ComputeReadSets by Traverse
   /// </summary>
   private readonly Stack<NonterminalTransition> StackOfNonterminalTransitions = new(126);

   /// <summary>
   /// For each nonterminal transition of any state compute the terminal symbols 
   /// which may follow (which the transition "reads").
   /// </summary>
   private void P3a_ComputeReadSets()
   {
      foreach (ParserState state in GlobalVariables.ListOfAllStates)
      {
         foreach (NonterminalTransition nonterminalTransition in state.Actions.OfType<NonterminalTransition>())
         {
            Traverse(nonterminalTransition);
         }
      }

      StackOfNonterminalTransitions.Clear();
   }

   /// <summary>
   /// <para>Algorithm Digraph of DeRemer and Penello (1982) used 
   /// to compute the read symbols of the <paramref name="actualTransition"/>:</para>
   /// recursively evaluate next states and their transitions 
   /// with nullable nonterminal input symbols
   /// </summary>
   /// <param name="actualTransition">the transition to evaluate</param>
   private void Traverse(NonterminalTransition actualTransition)
   {
      if (actualTransition.NextAction is not ParserState nextState)
         return;

      // A nextState is reached by the actualTransition.
      // Analyse each of its nonterminal transitions NextTransition,
      // which can produce the empty sequence (input symbol IsNullable):
      //    compute the read symbols of the nullable NextTransition (recursively)
      //    and add those to the read symbols of actualTransition

      StackOfNonterminalTransitions.Push(actualTransition);
      Int32 d = StackOfNonterminalTransitions.Count;
      actualTransition.Codenumber = d;
      /* Codenumber == 0: unmarked,
       *     >0 && < MaxValue: under active consideration, 
       *     == MaxValue: all SCCS have been found and evaluated
       */

      // Recursively add the read symbol sets
      // of all nonterminal transitions of the next state which have nullable input symbols
      foreach (NonterminalTransition NextTransition in nextState.Actions.OfType<NonterminalTransition>().
          Where(nt => nt.InputSymbol.IsNullable))
      {
         if (NextTransition.Codenumber == 0)
         {
            // The read symbols of NextTransition are not yet computed and not under consideration
            //     compute by recursive call
            Traverse(NextTransition);

            // if NextTransition.Codenumber == MaxValue then evaluation of NextTransition is complete
            // if NextTransition.Codenumber <= actualTransition.Codenumber
            //     then a SCC has ben detected and NextTransition is on the stack
         }

         // add the set of read symbols of NextTransition
         // to the set of read symbols of actualTransition
         actualTransition.TerminalSymbols.UnionWith(NextTransition.TerminalSymbols);

         if (actualTransition.Codenumber > NextTransition.Codenumber)
         {
            // found SCC: minimize Codenumber of actualTransition to prevent actual
            // transition from beeing popped  as the recursion unwinds
            actualTransition.Codenumber = NextTransition.Codenumber;
         }
      }

      if (actualTransition.Codenumber != d)
         return; // actualTransition is part of a SCC and remains in the stack

      // The actualTransition is the root of an SCC.
      // The elements of the SCC are in the stack and have all the same set of read symbols to be assigned.

      // TODO log non trivial SCC

      NonterminalTransition TopOfStackTransition;
      do
      {
         TopOfStackTransition = StackOfNonterminalTransitions.Pop();
         TopOfStackTransition.Codenumber = Int32.MaxValue;
         TopOfStackTransition.TerminalSymbols.UnionWith(actualTransition.TerminalSymbols); // equivalent to Assign(...)
      }
      while (TopOfStackTransition != actualTransition);
   }

   /*
    * ************************************************************************************************
    * ************************************************************************************************
    */

   /// <summary>
   /// <para>On the way back through states <see cref="LookaheadAction"/>s and
   /// <see cref="NonterminalTransition"/>s x are collected  in the <see cref="StackOfActionsWithFollow"/>.</para>
   /// When finally an included action y is found alle the relations x Includes y are added
   /// </summary>
   private readonly Stack<LookaheadOrNonterminalTransition> StackOfActionsWithFollow
      = new(126);

   /// <summary>
   /// Compute the sets of <see cref="TerminalSymbol"/>s which may follow
   /// <see cref="LookaheadAction"/>s or <see cref="NonterminalTransition"/>s
   /// </summary>
   private void P3b_ComputeFollow()
   {
      ClearCodenumbersOfAllActionsAndAssignEmptyIncludeSets();
      ComputeIncludeSets();
      Digraph2();
      DiscardAllIncludesSetsClearCodenumbers();
      StackOfActionsWithFollow.Clear();
   }

   private static void ClearCodenumbersOfAllActionsAndAssignEmptyIncludeSets()
   {
      foreach (ParserState State in GlobalVariables.ListOfAllStates)
      {
         foreach (ParserAction Action in State.Actions!)
         {
            Action.Codenumber = 0;
            if (Action is LookaheadOrNonterminalTransition ActionWithInclude)
               ActionWithInclude.Includes = new HashSet<NonterminalTransition>();
         }
      }
   }

   /// <summary>
   /// <para>Includes is a relation between <see cref="LookaheadOrNonterminalTransition"/>s x
   /// and <see cref="NonterminalTransition"/>s y. </para>
   /// x includes y states that the look ahead symbol sets of y have to be included
   /// in the look ahead sets of x.
   /// </summary>
   private void ComputeIncludeSets()
   {
      /* all ParserActions x of type
       *     - LookaheadAction with Definition as next action
       *     - NonterminalTransition with Definition as next action
       * are traced back to compute the relation x includes y
       *       where y is a NonterminalTransition
       *       
       * x includes y is also set for
       *       - NonterminalTransition s with (goto) ParserState as next action
       *         which are encountered via nullable symbols during backtrack
       * */

      Symbol[] elements;
      foreach (ParserState State in GlobalVariables.ListOfAllStates)
      {
         foreach (LookaheadOrNonterminalTransition Action
             in State.Actions.OfType<LookaheadOrNonterminalTransition>())
         {
            Debug.Assert(StackOfActionsWithFollow.Count == 0);

            StackOfActionsWithFollow.Push(Action);
            switch (Action)
            {
               case LookaheadAction lookAhead:
                  {
                     // Analyze reduce 
                     var nextDefinition = (Definition)lookAhead.NextAction;
                     elements = nextDefinition.Elements;
                     Lookback(State,
                         waybackAcceptsEmpty: true, // because there is not yet any symbol on the way back
                         definedSymbol: nextDefinition.DefinedSymbol!,
                         elements: elements,
                         distanceToGoBack: elements.Length);
                     break;
                  }

               case NonterminalTransition ntTransition:
                  {
                     if (ntTransition.NextAction is Definition definition)
                     {
                        // Analyze shift-reduce-action:
                        // the last element in the definition is the nonterminal symbol which causes the shift-reduce
                        elements = definition.Elements;
                        Lookback(State,
                            waybackAcceptsEmpty: // if the nonterminal is nullable
                            ((elements[^1] as NonterminalSymbol)?.IsNullable) ?? false,
                            definedSymbol: definition.DefinedSymbol!,
                            elements: elements,
                            distanceToGoBack: elements.Length - 1); // -1 because SHIFT-reduce
                     }
                     break;
                  }
            }
            StackOfActionsWithFollow.Pop();
         }
      }
   }

   private static void DiscardAllIncludesSetsClearCodenumbers()
   {
      foreach (ParserState State in GlobalVariables.ListOfAllStates)
      {
         State.Codenumber = 0;
         foreach (ParserAction a in State.Actions)
         {
            a.Codenumber = 0;
            if (a is LookaheadOrNonterminalTransition actionWithFollow)
               actionWithFollow.ClearIncludes(); // assign empty hashset to free memory (notnullable)
         }
      }
   }

   /// <summary>
   /// Find all predecessor states in the given distance of actual state 
   /// and add the action, that the defined nonterminal symbol causes there, to the includes relation.
   /// </summary>
   /// <param name="actualState"></param>
   /// <param name="waybackAcceptsEmpty"></param>
   /// <param name="definedSymbol"></param>
   /// <param name="elements"></param>
   /// <param name="distanceToGoBack"></param>
   private void Lookback(ParserState actualState, Boolean waybackAcceptsEmpty,
       NonterminalSymbol definedSymbol, Symbol[] elements, Int32 distanceToGoBack)
   {
      // WaybackAcceptsEmpty is true for all nonterminals in elements[elements.Lenght] down

      // gilt für alle Symbole in Elemente[Restlänge] und nachfolgenden (bzw. bei leerer Alternative)
      // alle nichtterminalen Aktionen, die vom aktuellen Zustand über löschare Symbole an das Ende von Elemente führen,
      // sind gekellert
      if (distanceToGoBack == 0)
      {
         /*  nichtterminalen Übergang aufgrund des erzeugten Symbols suchen (y) 
          * und in die Listen der im Stack genannten Aktionen einfügen, falls noch nicht enthalten 
          * */
         IncludesRelationenErweitern(actualState, definedSymbol); // wertet den Inhalt des aStack aus
         return;
      }

      // DistanceToGoBack > 0
      foreach (ParserState Predecessor in actualState.PredecessorList!)
      {
         if (waybackAcceptsEmpty)
         {
            /* Extend the collection of x in StackOfActionsWithFollow
             * by nonterminal transitions which lead from predecessor states
             * into the actual state
             */
            Int32 PushCount = PushNonterminalTransitions(Predecessor, actualState);

            // NonterminalSymbol nts;
            Lookback(actualState: Predecessor,
                waybackAcceptsEmpty: // is switched to false if the actual alement is not nullabel
                    ((elements[distanceToGoBack - 1] as NonterminalSymbol)?.IsNullable) ?? false,
                definedSymbol: definedSymbol,
                elements: elements,
                distanceToGoBack: distanceToGoBack - 1
                );

            // remove 
            StackOfActionsWithFollow.Remove(PushCount);
         }
         else
         {
            Lookback(Predecessor,
               false, // "WaybackAcceptsEmpty" ist und bleibt false
               definedSymbol, elements, distanceToGoBack - 1);
         }
      }
   } // Lookback

   /// <summary>
   /// Push all nonterminal transitions from fromState to toState on the StackOfActionsWithFollow and return the number of pushed parser actions.
   /// </summary>
   /// <param name="fromState"></param>
   /// <param name="toState"></param>
   /// <returns>number of pushed <see cref="NonterminalTransition"/>s</returns>
   private Int32 PushNonterminalTransitions(ParserState fromState, ParserState toState)
   {
      Int32 Counter = 0;
      foreach (NonterminalTransition NtTransition in
          fromState.Actions.OfType<NonterminalTransition>()
          .Where(transition => transition.NextAction as ParserState == toState)
          )
      {
         StackOfActionsWithFollow.Push(NtTransition);
         Counter++;
      }
      return Counter;
   }

   /// <summary>
   /// In den Aktionen des "aktuellen Zustands" den nichtterminalen Übergang mit dem gegebenen Eingabesymbol suchen
   /// und allen Aktionen des Stacks per Includes zuordnen
   /// </summary>
   /// <param name="stateReachedByReduction">Zustand, in dessen Aktionsliste die Aktion mit dem gegebenen Eingabesymbol gesucht wird</param>
   /// <param name="inputSymbol">Das Eingabesymbol, zu dem der entsprechende nichtterminale Übergang gesucht wird</param>
   private void IncludesRelationenErweitern(ParserState stateReachedByReduction, NonterminalSymbol inputSymbol)
   {
      // Im aktuellen Zustand den nichtterminalen Übergang nü mit dem gegebenen Eingabesymbol suchen
      // und für jede Aktion im Stack (=Aktion mit terminalenSymbolen) 
      //    StackAktion.Includes.Add(nü)
      // Sonderfall: im gegebenen Zustand gibt es zum gegebenen Eingabesymbol keinen nichtterminalen Übergang
      //     dann handelt es sich um *Startsymbol , hinter dem alle terminalen Symbole erlaubt sind
      //     Diese werden in alle im Stack gespeicherten Aktionen eingetragen

      // den nü mit dem gegebenen Eingabesymbol suchen
      NonterminalTransition TransitionY =
          stateReachedByReduction.Actions.OfType<NonterminalTransition>().
          FirstOrDefault(a => a.InputSymbol == inputSymbol)!;

      if (TransitionY == null)
      {
         // there is no next action, this is the case only for the Startsymbol
         Debug.Assert(inputSymbol == GlobalVariables.Startsymbol);
         // all terminal symbols are allowed, which is set in direct read

         return;
      }

      foreach (LookaheadOrNonterminalTransition ActionX in StackOfActionsWithFollow)
         ActionX.Includes.Add(TransitionY);
   }

   /*
    * 1. Berechnen der Relation x Includes y fuer nichtterminale Uebergaenge :
    *   (siehe Includes_berechnen)
    * 2. Berechnen der Follow-Symbole (Prozedur Digraph2)
    * zu 1.:
    *   die Nummer eines nichtterminalen_Uebergangs x ist
    *     0 wenn noch kein y mit  x including y   gefunden wurde
    *     sonst: Index in Includes  z
    * zu 2.:
    *   Digraph2 verwendet den in Codenummer  von ni_ bzw nr_Aktionen
    *   eingetragenen Wert>=0 zum Auffinden der Includes-Relation
    *   und ersetzt ihn durch einen Wert <0
    *   
    * Waehrend der Berechnung wird eine Liste für eine Menge jener x benoetigt, fuer
    * die gleichzeitig ein y mit x r y berechnet wird.
    * Dieser Bereich dient auch als Stack fuer Digraph2. 
    * */
   private void Digraph2()
   {
      foreach (ParserState Zustand in GlobalVariables.ListOfAllStates)
      {
         foreach (ParserAction ActionX in Zustand.Actions.
            Where(a => a.Codenumber >= 0))
         {
            if (ActionX is NonterminalTransition ActionXasNT)
            {
               Traverse2(ActionXasNT);
            }
            else if (ActionX is LookaheadAction ActionXasLA)
            {
               LookaheadDigraph2(ActionXasLA);
            }
         }
      }
   }

   private void LookaheadDigraph2(LookaheadAction x)
   {
      foreach (NonterminalTransition NtTransitionY in x.Includes)
      {
         if (NtTransitionY.Codenumber >= 0)
            Traverse2(NtTransitionY);
         x.TerminalSymbols.UnionWith(NtTransitionY.TerminalSymbols);
      }
      x.Codenumber = Int32.MinValue;
   }

   private void Traverse2(NonterminalTransition transitionX)
   {
      StackOfActionsWithFollow.Push(transitionX);
      Int32 d = -StackOfActionsWithFollow.Count;
      transitionX.Codenumber = d; // <0

      foreach (NonterminalTransition NtTransitionY in transitionX.Includes)
      {
         if (NtTransitionY.Codenumber >= 0)
         {
            // The look ahead symbols are not yet computed and not under consideration
            //     compute by recursive call
            Traverse2(NtTransitionY);
         }

         // add the look ahead symbols of NtTransitionY
         // to the look ahead symbols of transitionX
         transitionX.TerminalSymbols.UnionWith(NtTransitionY.TerminalSymbols);

         if (NtTransitionY.Codenumber > transitionX.Codenumber)
         {
            // found SCC: maximise the (negative) CodeNumber of transitionX
            // to prevent it from beeing poped as the recursion unwinds
            transitionX.Codenumber = NtTransitionY.Codenumber;
         }
      }

      if (transitionX.Codenumber != d)
         return; // transitionX is part of a SCC and remains in the stack

      // transitionX is root of a SCC
      // The elements of the SCC are in the stack and have all the same set of look ahead symbols to be assigned.

      LookaheadOrNonterminalTransition ats;
      do
      {
         ats = StackOfActionsWithFollow.Pop();
         ats.Codenumber = Int32.MinValue;
         ats.TerminalSymbols.UnionWith(transitionX.TerminalSymbols); // eqivalent to Assign(...)
      }
      while (ats != transitionX);
   }

   /*   ---------- */
   /// <summary>
   /// Checks all states and all actions of the states for conflicts which can be solved by constant priorities. 
   /// Ignores actions wih dynamic priorities.
   /// </summary>
   /// <param name="sb">The description of the conflicts will be written to <paramref name="sb"/></param>
   private static Int32 P3c_FindAndResolveAllStaticConflicts(StringBuilder sb, out int sumOfConflictsNotSolvedByExplicitPriority)
   {
      var allowedSymbolsUpToThisAction = new IndexSet(GlobalVariables.NumberOfTerminalSymbols); // allocate outside of loop

      Int32 statesWithConflict = 0;
      sumOfConflictsNotSolvedByExplicitPriority = 0;
      // Examine all parser states 
      foreach (ParserState State in GlobalVariables.ListOfAllStates)
      {
         statesWithConflict +=
            State.FindAndSolveConflictsOfState(
               allowedSymbolsUpToThisAction,
               out int numberOfConflictsNotSolvedByExplicitPriority,
               sb
               );
         sumOfConflictsNotSolvedByExplicitPriority += numberOfConflictsNotSolvedByExplicitPriority;
      }

      sb.AppendLine()
        .AppendLine($"-- Result of conflict analysis: found {statesWithConflict} states with static conflicts.");
      if (sumOfConflictsNotSolvedByExplicitPriority > 0)
         sb.AppendLine
            ($"-- Warning: {sumOfConflictsNotSolvedByExplicitPriority} conflict(s) could not be solved by semantic priorities.");

      return statesWithConflict;
   }
} // end of class 
