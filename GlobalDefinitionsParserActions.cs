using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace grammlator {
   /// <summary>
   /// <see cref="ParserActionEnum"/> is used to sort the actions of a state and to output the name of the type of actions.
   /// The order of the enum elements corresponds to the order of the elements
   /// of <see cref="GlobalVariables.LabelPrefixes"/>
   /// </summary>
   internal enum ParserActionEnum {
      isDefinition,
      isParserState,
      isPushStateAction,
      isLookaheadAction,
      isReduceAction,
      isHaltAction,
      isErrorhaltAction,
      isTerminalTransition,
      isAcceptAction,
      isNonterminalTransition,
      isBranchAction,
      isPrioritySelectAction,
      isPriorityBranchAction,
      isErrorhandlingAction,
      isDeletedParserAction,
      isEndOfGeneratedCode,
      isSomethingElse
   }

   class ParserEnumExtension {
      /// <summary>
      /// These strings are used to construct labels in the generated program.
      /// They are adressed by <see cref="ParserActionEnum"/>.
      /// Some of these will never occur in labels. They are provided for future modifications.
      /// </summary>

      internal static String LabelPrefix(ParserActionEnum e)
      {
         String result =
            e switch
            {
               ParserActionEnum.isDefinition => "ApplyDefinition???",
               ParserActionEnum.isParserState => "State",
               ParserActionEnum.isPushStateAction => "PushState",
               ParserActionEnum.isLookaheadAction => "LookAhead???",
               ParserActionEnum.isReduceAction => "Reduce",
               ParserActionEnum.isHaltAction => "ApplyStartsymbolDefinition",
               ParserActionEnum.isErrorhaltAction => "EndWithError",
               ParserActionEnum.isTerminalTransition => "TerminalTransition???",
               ParserActionEnum.isAcceptAction => "Accept",
               ParserActionEnum.isNonterminalTransition => "NonterminalTransition???",
               ParserActionEnum.isBranchAction => "Branch",
               ParserActionEnum.isPrioritySelectAction => "PrioritySelect",
               ParserActionEnum.isPriorityBranchAction => "PriorityBranch",
               ParserActionEnum.isErrorhandlingAction => "HandleError",
               ParserActionEnum.isDeletedParserAction => "Deleted???",
               ParserActionEnum.isEndOfGeneratedCode => "EndOfGeneratedCode",
               _ => "UnknownAction???"
            };
         Debug.Assert(result[^1] != '?');
         return result;
      }
   }

   internal abstract partial class ParserAction :
      EqualityComparer<ParserAction>, // implements: public Boolean Equals(ParserAction? a1, ParserAction? a2)
                                      // IComparable<ParserAction>, // implements: Int32 CompareTo([AllowNull] T other);
      IUniqueIndex  // implements: Int32 IdNumber {get;}
      {
      /// <summary>
      /// The <see cref="ParserActionType"/>  is used for AppendToSB (...) and for comparisions
      /// </summary>
      internal abstract ParserActionEnum ParserActionType {
         get;
      }

      /// <summary>
      /// compares the (Int32)ParserActionType, which is defined by <see cref="ParserActionEnum"/>, and then the IdNumbers
      /// </summary>
      /// <param name="other"></param>
      /// <returns> this... - other ...</returns>
      /// <exception cref="ArgumentNullException"><paramref name="other"/> is <c>null</c>.</exception>
      public virtual Int32 CompareTo(ParserAction? other)
      {
         if (other == null)
            throw new ArgumentNullException(nameof(other));

         Int32 result = (Int32)this.ParserActionType - (Int32)other.ParserActionType;
         if (result != 0)
            return result;

         // different instances of same type must have different IdNumbers
         result = this.IdNumber - other.IdNumber;
         Debug.Assert((result != 0) || ReferenceEquals(this, other));
         return result;
      }

      public override Boolean Equals(ParserAction? a1, ParserAction? a2)
      {
         if (ReferenceEquals(a1, a2))
            return true;

         return
            a1 is not null && a2 is not null
            && a1.ParserActionType == a2.ParserActionType && a1.IdNumber == a2.IdNumber;
      }

      public override Int32 GetHashCode(ParserAction a)
      {
         return (Int32)a.ParserActionType + a.IdNumber << 5;
      }

      /// <summary>
      /// A number &gt;= 0, unique within subclass (halts, reductions and branches are numbered separately.
      /// In protocols IdNumber+1 is used (human friendly).
      /// </summary>
      public Int32 IdNumber {
         get; set;
      }

      /// <summary>
      /// in phase3: used in digraph (0=&gt;not yet calculated); 
      /// in phase4: set to 0 (implements interface IParserAction)
      /// </summary>
      public Int32 Codenumber {
         get; set;
      }

      /// <summary>
      ///  The number, the parser state pushes on the state stack. Originally only used for ParserStates.
      ///  <para>Value -1: the state doesn't push a number on the stack (initial value).</para>
      ///  <para>Standard-Value: IdNumber; Optimized-Value: some smaller value</para>
      ///  <para> Additional Optimization may move this information to other actions.
      ///  Then the number x is replaced by (-x-2).</para>
      /// </summary>
      internal Int32 StateStackNumber {
         get; set;
      } = -1;

      /// <summary>
      /// Originally only used for ReduceActions: the number of elements that are to be removed from the
      /// state stack, is always &gt;= 0, initial value 0;
      /// <para> Optimization may move this information to other actions.</para>
      /// </summary>
      internal Int32 StateStackAdjustment {
         get; set;
      } = 0;

      /// <summary>
      /// Used in Phase5: &gt; 0: number of usages (code has not been generated),
      /// <para>0: never used or code has been generated, </para>
      /// <para>&lt;0: code has been generated or should not yet be generated (=-number of usages)</para>
      /// </summary>
      internal Int32 Calls {
         get; set;
      }
      internal Int32 AcceptCalls {
         get; set;
      }

      /// <summary>
      /// Called in Phase5 to compute the values of Calls and AcceptCalls for Startaction and recursively for all accessible actions
      /// </summary>
      /// <param name="Accept"></param>
      internal virtual void CountUsage(Boolean Accept)
      {
         if (Accept)
         {
            AcceptCalls++;
            if (AcceptCalls > 1)
               return; // The call of the non accepting part has already been counted
            Debug.Assert(AcceptCalls == 1); // Accept will call the not accepting part
         }
         Calls++;
      }

      internal virtual StringBuilder AppendToSB(StringBuilder sb)
         => sb.Append("Action ")
           .Append(IdNumber + 1)
           .Append(", ")
           .AppendLine(ParserActionType.ToString());

      /// <summary>
      /// nonrecursive short description of the <see cref="ParserAction"/>
      /// </summary>
      /// <param name="sb"></param>
      internal virtual StringBuilder AppendShortToSB(StringBuilder sb)
         => AppendToSB(sb); // some methods overwrite AppendShortToSB to avoid endless recursion

      /// <summary>
      /// Return a "simpler" <see cref="ParserAction"/> which can replace this action or return this
      /// </summary>
      /// <returns></returns>
      internal virtual ParserAction Simplify()
      {
         return this;
      }
   }

   internal class ListOfParserActions : List<ParserAction> {
      /// <summary>
      /// Initializes a new instance of the <see cref="ListOfParserActions"/>: List&lt;ParserAction&gt; class
      /// that is empty and has the specified initial <paramref name="capacity"/>.
      /// </summary>
      /// <param name="capacity">The number of elements that the new list can initially store.</param>
      internal ListOfParserActions(Int32 capacity) : base(capacity) { }

      /// <summary>
      /// Initializes a new instance of <see cref="ListOfParserActions"/>: List&lt;ParserAction&gt;  class
      /// that contains all the elements from the specified <see cref="ListOfParserActions"/> instance
      /// and has sufficient capacity to accomodate the number of elements copied.
      /// </summary>
      /// <param name="CopyFrom"></param>
      internal ListOfParserActions(ListOfParserActions CopyFrom) : base(CopyFrom) { }

      internal void RemoveFromEnd(Int32 count) => RemoveRange(this.Count - count, count);

      internal virtual StringBuilder AppendToSB(StringBuilder sb)
      {
         Int32 number = 1;
         if (this.Count == 0)
            sb.AppendLine(" - the list of actions is empty - ");
         else
            sb.AppendLine(); //  ("List of actions:");
         foreach (ParserAction a in this)
         {
            if (a == null)
            {
               sb.Append(number++)
                 .AppendLine(". null ???");
            }
            else
            {
               sb.Append(number++)
                 .Append(". ");
               a.AppendToSB(sb);
               sb.AppendLine();
            }
         }
         return sb;
      }

      /// <summary>
      /// Yields all actions of the list of actions whereby instead of a PrioritySelectActions its
      /// NextAction is used and if it is a PriorityBranchAction yields all dependend actions
      /// </summary>
      public IEnumerable<ParserAction> PriorityUnwindedSetOfActions {
         get {
            for (Int32 i = 0; i < Count; i++)
            {
               ParserAction? a = this[i];
               while (a != null)
               {
                  if (a is PrioritySelectAction psa)
                     a = psa.NextAction; // this is the reason why a while loop is used
                  else if (a is PriorityBranchAction pba)
                  {
                     if (pba.ConstantPriorityAction != null)
                        yield return pba.ConstantPriorityAction; // TOCHECK recursion needed? Example? (without terminal symbols?)
                     foreach (ParserAction dpa in pba.DynamicPriorityActions.PriorityUnwindedSetOfActions)  // recusion !
                        yield return dpa;
                     a = null;
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
   }

   /// <summary>
   /// Each nonterminal symbol has one ore more (n-1) definitions, numbered from 0 to n-1 by IdNumber.
   /// Each definition consist of the DefinedSymbol, its Elements and the AttributestackAdjustment.
   /// </summary>
   internal sealed class Definition : ParserAction {
      //   Nummer gibt an, um die wievielte Alternative des
      //     erzeugten Symbols es sich handelt
      //   TOCHECK          Kontext in dieser Version noch nicht implementiert !!!}

      public Definition(Int32 idNumber, NonterminalSymbol? definedSymbol, Symbol[] elements, Int32 attributestackAdjustment)
      {
         IdNumber = idNumber;
         DefinedSymbol = definedSymbol;
         Elements = elements;
         AttributestackAdjustment = attributestackAdjustment;
         AttributeIdentifiers = Array.Empty<UnifiedString>();
      }
      internal override ParserActionEnum ParserActionType => ParserActionEnum.isDefinition;

      /// <summary>
      /// Compares this and other by ParserActionType, DefinedSymbol.SymbolNumer and ParserAction.IdNumber
      /// </summary>
      /// <param name="other"></param>
      /// <returns>if this &lt; other: result is &lt; 0; if equal result is &gt; 0 else is  0</returns>
      public override Int32 CompareTo(ParserAction? other)
      {
         if (other == null)
            throw new ArgumentNullException(nameof(other));

         Int32 result;

         if ((result = this.ParserActionType - other.ParserActionType) != 0)
            return result;

         // other is a Definition
         var otherNumber = ((Definition)other).DefinedSymbol!.SymbolNumber;
         return (result = this.DefinedSymbol!.SymbolNumber - otherNumber) != 0
             ? result
             : this.IdNumber - other.IdNumber;
      }

      internal NonterminalSymbol? DefinedSymbol {
         get; set;
      }

      internal Int64 ConstantPriority;

      /// <summary>
      /// A Int32 method
      /// </summary>
      internal IntMethodClass? PriorityFunction;

      /// <summary>
      /// A void method
      /// </summary>
      internal VoidMethodClass? SemanticMethod;

      /// <summary>
      /// The code for <see cref="AttributestackAdjustment"/> &gt;0 has to be generated before semantic method calls,
      /// for &lt;0 after semantic method calls.
      /// </summary>
      internal Int32 AttributestackAdjustment;
      internal Symbol[] Elements;
      /// <summary>
      /// Is used to display the attribute identifiers which are used in the definition.
      /// In definitions added by grammlator this array may be null or empty.
      /// Then the attribute names of the symbols declarations are used.
      /// </summary>
      internal UnifiedString[] AttributeIdentifiers;

      /// <summary>
      /// Returns true, if no SemanticMethod, no AttributeStackCorrection. 
      /// Does not (!) check, if DefinedSymbol is the startsymbol.
      /// Does not check PriorityFunction, hence <see cref="HasNoSemantics"/> should only
      /// used in P4 or later (after solving conflicts and copying PriorityFunctions to 
      /// PrioritySelectActions in P3).
      /// </summary>
      /// <returns>true if no semantics are associated with the definition</returns>
      internal Boolean HasNoSemantics()
          => (SemanticMethod == null)
              && (AttributestackAdjustment == 0);
      // && (PriorityFunction == null);

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         sb.Append("recognized ");
         DefinedSymbol!.IdentifierAndAttributesToSB(sb).AppendLine("= ");
         sb.Append("      ");
         AppendToSB(sb, Int32.MaxValue); // do not mark any element
         return sb;
      }

      internal override StringBuilder AppendShortToSB(StringBuilder sb)
      {
         DefinedSymbol!.IdentifierAndAttributesToSB(sb).Append("= ");
         ElementsToStringbuilder(sb, Int32.MaxValue);
         return sb;
      }

      internal void ElementsToStringbuilder(StringBuilder sb, Int32 MarkiertesElement)
          => Elements.Append(sb, MarkiertesElement, this.AttributeIdentifiers);

      internal StringBuilder AppendToSB(StringBuilder sb, Int32 MarkiertesElement)
      {
         ElementsToStringbuilder(sb, MarkiertesElement);

         String Delimiter = "";

         sb.Append(' ');
         if (ConstantPriority != 0)
         {
            sb.Append("priority: ")
              .Append(ConstantPriority);
            Delimiter = ", ";
         }
         if (PriorityFunction != null)
         {
            sb.Append(Delimiter)
              .Append("dynamic priority: ")
              .Append(PriorityFunction.MethodName);
            Delimiter = ", ";
         }
         if (AttributestackAdjustment > 0)
         {
            sb.Append(Delimiter)
              .Append("aStack: ")
              .Append(AttributestackAdjustment);
            Delimiter = ", ";
         }
         if (SemanticMethod != null)
         {
            sb.Append(Delimiter)
              .Append("method: ")
              .Append(SemanticMethod.MethodName);
            Delimiter = ", ";
         }
         if (AttributestackAdjustment >= 0)
         {
            return sb;
            // Delimiter = ", ";
         }

         sb.Append(Delimiter)
           .Append("aStack: ")
           .Append(AttributestackAdjustment);
         // Delimiter = ", ";

         return sb;
      }

      internal EmptyComputationResultEnum _EmptyComputationResult;

      internal EmptyComputationResultEnum IsEmptyDefinitionComputation()
      {
         switch (_EmptyComputationResult)
         {
         // if value is known return value
         case EmptyComputationResultEnum.IsOrContainsEmptyDefinition:
         case EmptyComputationResultEnum.NotEmpty:
            return _EmptyComputationResult;
         // if recursion return NotYetComputedOrRecursion
         case EmptyComputationResultEnum.IsJustBeingComputed:
            return EmptyComputationResultEnum.NotYetComputedOrRecursion;
         }

         // Is the alternative empty (simple redundant check)
         if (Elements.Length == 0)
         {
            _EmptyComputationResult = EmptyComputationResultEnum.IsOrContainsEmptyDefinition;
            return EmptyComputationResultEnum.IsOrContainsEmptyDefinition;
         }

         // If the alternative contains a terminal symbol, the alternative is not empty
         foreach (Symbol Symbol in Elements)
         {
            if (Symbol.GetType() == typeof(TerminalSymbol))
            {
               _EmptyComputationResult = EmptyComputationResultEnum.NotEmpty;
               return EmptyComputationResultEnum.NotEmpty;
            }
         }

         // Does each (nonterminal) element contain an empty alternative? 

         _EmptyComputationResult = EmptyComputationResultEnum.IsJustBeingComputed;  // avoid endless recursion

         Boolean ResultDependsOnRecursion = false;
         foreach (Symbol Symbol in Elements)
         {
            switch (Symbol.ContainsAnEmptyDefinition())
            {
            case EmptyComputationResultEnum.NotEmpty:
            {
               _EmptyComputationResult = EmptyComputationResultEnum.NotEmpty;
               return EmptyComputationResultEnum.NotEmpty;
            }

            case EmptyComputationResultEnum.IsOrContainsEmptyDefinition:
               break; // Test if all other elements also contain an empty alternative

            case EmptyComputationResultEnum.NotYetComputedOrRecursion:
            {
               ResultDependsOnRecursion = true; // Weitersuchen, ob eLeereZeichenkette.falsch auftritt
               break; // this computation ended in recursion, check next symbols of list for DoesNotContain
            }

            default: // =  case eEmptyComputationResult.IsJustBeingComputed:
            {
               Debug.Fail("Error in Symbol.ContainsAnEmptyAlternative");
               throw new ErrorInGrammlatorProgramException("Error in Symbol.ContainsAnEmptyAlternative");
            }
            }
         }

         // there is no element that does definitely not contain an empty alternative
         _EmptyComputationResult = ResultDependsOnRecursion
             ? EmptyComputationResultEnum.NotYetComputedOrRecursion
             : EmptyComputationResultEnum.IsOrContainsEmptyDefinition;

         return _EmptyComputationResult;
      }
   }

   internal class ListOfDefinitions : List<Definition> {

      internal ListOfDefinitions(Int32 Anzahlelemente) : base(Anzahlelemente) { }

      internal ListOfDefinitions(Int32 AnzahlAlternativen, ListOfDefinitions definitions) :
          base(definitions.GetRange(definitions.Count - AnzahlAlternativen, AnzahlAlternativen))
      {
      }

      /// <summary>
      /// Search all definitions of the list for an empty one
      /// </summary>
      private EmptyComputationResultEnum _EmptyComputationResult;

      internal EmptyComputationResultEnum ListContainsEmptyDefinition()
      {
         switch (_EmptyComputationResult)
         {
         // if value is already known return value
         case EmptyComputationResultEnum.IsOrContainsEmptyDefinition:
         case EmptyComputationResultEnum.NotEmpty:
            return _EmptyComputationResult;

         // if recursion return "NotYetComputedOrRecursion"
         case EmptyComputationResultEnum.IsJustBeingComputed:
            return EmptyComputationResultEnum.NotYetComputedOrRecursion;
         }

         // Redundant check: is any of the definitions empty
         // This simple check is intended to reduce recursion
         foreach (Definition def in this)
         {
            if (def.Elements.Length == 0)
            {
               _EmptyComputationResult = EmptyComputationResultEnum.IsOrContainsEmptyDefinition;
               return _EmptyComputationResult;
            }
         }

         // Does one of the definitions define the empty string of terminal symbols ?

         _EmptyComputationResult = EmptyComputationResultEnum.IsJustBeingComputed;
         Boolean SearchLimitedByRecursion = false;

         foreach (Definition def in this)
         {
            switch (def.IsEmptyDefinitionComputation())
            {
            case EmptyComputationResultEnum.IsOrContainsEmptyDefinition: // Suche war erfolgreich
            {
               _EmptyComputationResult = EmptyComputationResultEnum.IsOrContainsEmptyDefinition;
               return EmptyComputationResultEnum.IsOrContainsEmptyDefinition;
            }

            case EmptyComputationResultEnum.NotEmpty:
               break; // continue searching

            case EmptyComputationResultEnum.NotYetComputedOrRecursion:
            {
               SearchLimitedByRecursion = true;
               break;
            }

            default:
               throw new
          ErrorInGrammlatorProgramException("illegal result of empty ComputeEmptyComputationResult()");
            }
         }

         // did not find an empty definition
         _EmptyComputationResult = SearchLimitedByRecursion ? EmptyComputationResultEnum.NotYetComputedOrRecursion : EmptyComputationResultEnum.NotEmpty;

         return _EmptyComputationResult;
      }

      internal virtual Int32 MarkAndCountAllUsedSymbols()
      {
         Int32 count = 0;
         foreach (Definition alternative in this)
         {
            count += alternative.Elements.MarkAndCountAllUsedSymbols();
         }
         return count;
      }

      internal virtual void AppendToSB(StringBuilder sb)
      {
         Int32 number = 1;
         if (this.Count == 0)
            sb.AppendLine(" - none - ");
         foreach (Definition d in this)
         {
            sb.Append("    ")
              .Append(number++)
              .Append(". ");
            d.AppendToSB(sb, Int32.MaxValue) // do not mark any element
             .AppendLine();
         }
      }

      public void RemoveFromEnd(Int32 n) => this.RemoveRange(this.Count - n, n);
   }

   internal sealed partial class BranchAction : ParserAction {
      internal BranchAction(BranchcasesList ListOfCases, Int32 IdNumber)
      {
         this.ListOfCases = ListOfCases;
         base.IdNumber = IdNumber;
      }

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isBranchAction;

      internal BranchcasesList ListOfCases;

      internal override void CountUsage(Boolean Accept)
      {
         if (Calls > 0)
         {
            base.CountUsage(Accept);
         }
         else
         {
            base.CountUsage(Accept);
            foreach (BranchcaseStruct branchcase in ListOfCases)
               branchcase.BranchcaseAction.CountUsage(false);
         }
      }

      internal override StringBuilder AppendShortToSB(StringBuilder sb)
          => sb.Append(P5CodegenCS.GotoLabel(this, false));

      internal BitArray? PossibleInputTerminals = null;

      private Int32 SimplifyRecursionCount = 0;
      internal override ParserAction Simplify()
      {
         if (SimplifyRecursionCount > 0)
            return this;

         List<BranchcaseStruct> branchCaseList = ListOfCases;

         // Simplify the actions of a branch 
         for (Int32 i = 0; i < branchCaseList.Count; i++)
         {
            SimplifyRecursionCount++;

            BranchcaseStruct branchCase = branchCaseList[i];

            if (branchCase.BranchcaseAction is LookaheadOrNonterminalTransition LaOrNt)
            {
               if (LaOrNt.NextAction is not Definition)
                  branchCase.BranchcaseAction = LaOrNt.NextAction;
            };

            branchCase.BranchcaseAction = branchCase.BranchcaseAction.Simplify();
            branchCaseList[i] = branchCase;

            SimplifyRecursionCount--;
         }

         Debug.Assert(branchCaseList.Count > 0);

         // test if all cases now (after optimization) have the same action
         BranchcaseStruct branchCase0 = branchCaseList[0];
         for (Int32 i = 1; i < branchCaseList.Count; i++)
         {
            if (branchCaseList[i].BranchcaseAction != branchCase0.BranchcaseAction)
               return this; // at least one is different: return the (optimized) branch
         }

         // all cases have the same action 
         return branchCase0.BranchcaseAction;
      }
   }

   internal class BranchcasesList : List<BranchcaseStruct> {
      internal BranchcasesList(Int32 capacity) : base(capacity) { }

      private BranchcasesList()
      {
      }

      internal BranchcasesList(IEnumerable<BranchcaseStruct> Quelle) : base(Quelle) { }

      internal void SortByCondition()
      {
         base.Sort(Comparision);
      }

      Int32 Comparision(BranchcaseStruct a, BranchcaseStruct b)
      {
         return a.BranchcaseCondition - b.BranchcaseCondition;
      }

      internal enum CompareResult { equal, contains, isContained, different };
      /// <summary>
      /// Compares this and the <see cref="BranchcasesList"/> <paramref name="other"/> 
      /// (both must be sorted) and returns
      /// equal if both are equal,
      /// contains if this contains other,
      /// isContaind if other contains this
      /// different else
      /// </summary>
      /// <param name="other"></param>
      /// <returns></returns>
      internal CompareResult Containing(BranchcasesList other)
      {
         if (this.Count >= other.Count)
            return Containing(longer: this, shorter: other);

         return Containing(longer: other, shorter: this)
            switch
         {
            CompareResult.contains => CompareResult.isContained,
            CompareResult.isContained => CompareResult.contains,
            _ => CompareResult.different // never CompareResult.equal because length differs
         };
      }

      /// <summary>
      /// <paramref name="longer"/>.Count must be greater or equal <paramref name="shorter"/>.Count, both lists must be sorted!
      /// <para>returns <see cref="CompareResult.equal"/> if both arguments are equal</para>
      /// <para>returns <see cref="CompareResult.contains"/> if <paramref name="longer"/> contains <paramref name="shorter"/></para>
      /// else returns <see cref="CompareResult.different"/>
      /// </summary>
      /// <param name="longer"></param>
      /// <param name="shorter"></param>
      /// <returns></returns>
      private static CompareResult Containing(BranchcasesList longer, BranchcasesList shorter)
      {
         Debug.Assert(longer.Count >= shorter.Count);

         // all elements of the shorter list must be contained in the longer one, else different
         Int32 skipped = 0;
         for (Int32 iShorter = 0; iShorter < shorter.Count; iShorter++)
         {
            while (shorter[iShorter].BranchcaseCondition != longer[iShorter + skipped].BranchcaseCondition
                || shorter[iShorter].BranchcaseAction != longer[iShorter + skipped].BranchcaseAction)
            {
               skipped++;
               if (skipped > longer.Count - shorter.Count)
                  return CompareResult.different;
            }
         }
         return longer.Count == shorter.Count ? CompareResult.equal : CompareResult.contains;
      }
   }

   internal struct BranchcaseStruct {
      internal Int32 BranchcaseCondition;
      internal ParserAction BranchcaseAction;

      internal BranchcaseStruct(Int32 conditionOfBranch, ParserAction actionOfBranch)
      {
         this.BranchcaseCondition = conditionOfBranch;
         this.BranchcaseAction = actionOfBranch;
      }
   }

   internal abstract class ParserActionWithNextAction : ParserAction {
      public ParserAction NextAction {
         get; set;
      }

      public ParserActionWithNextAction(ParserAction nextAction)
      {
         NextAction = nextAction;
      }

      internal override void CountUsage(Boolean Accept)
      {
         if (Accept)
         {
            AcceptCalls++;
            if (AcceptCalls > 1)
               return; // The call of the non accepting part has already been counted
            Debug.Assert(AcceptCalls == 1); // Accept will call the not accepting part
         }
         Calls++;
         if (Calls == 1) // This action calls the next action once
            NextAction?.CountUsage(false);
      }

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         sb.Append("    then: ");
         return NextAction.AppendShortToSB(sb);
      }
   }

   /// <summary>
   /// In phase4 definitions and nonterminal transitions are replaced by reduce actions with appropriate next actions
   /// (ParserState, BranchAction ...)
   /// </summary>
   internal sealed partial class ReduceAction : ParserActionWithNextAction {

      internal ReduceAction(ParserAction nextAction) : base(nextAction) { }

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isReduceAction;

      /// <summary>
      /// A comment describing this reduce action
      /// </summary>
      internal String Description = String.Empty;

      internal Int32 AttributeStackAdjustment;
      // internal IntMethodClass PriorityFunction; // has already been copied from Definition to PrioritySelectAction
      internal VoidMethodClass? SemanticMethod;

      /// <summary>
      /// If true then the code for correction of the attribute stack has to be generated before the code calling the semantic method.
      /// This field can not be replaced by a check of AttributeStackCorrection after combining chains of reductions!
      /// </summary>
      internal Boolean FirstAdjustAttributeStackThenCallMethod;

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         sb.Append(P5CodegenCS.GotoLabel(this, false));
         AppendAdjustmentsAndMethod(sb);
         return base.AppendToSB(sb);
      }

      private void AppendAdjustmentsAndMethod(StringBuilder sb)
      {
         if (StateStackAdjustment != 0 || SemanticMethod != null || AttributeStackAdjustment != 0)
         {
            sb.Append(" (");
            if (StateStackAdjustment != 0)
            {
               sb.Append("sAdjust: ").Append(-StateStackAdjustment);
               if (SemanticMethod != null || AttributeStackAdjustment != 0)
                  sb.Append(", ");
            }
            if (AttributeStackAdjustment != 0 && FirstAdjustAttributeStackThenCallMethod)
            {
               sb.Append("aAdjust: ").Append(AttributeStackAdjustment);
               if (SemanticMethod != null || AttributeStackAdjustment != 0)
                  sb.Append(", ");
            }
            if (SemanticMethod != null)
            {
               sb.Append("method: ").
                  Append(SemanticMethod.MethodName);
               if (AttributeStackAdjustment != 0 && !FirstAdjustAttributeStackThenCallMethod)
                  sb.Append(", ");
            }
            if (AttributeStackAdjustment != 0 && !FirstAdjustAttributeStackThenCallMethod)
               sb.Append("aAdjust: ").Append(AttributeStackAdjustment);
            sb.Append(") ");
         }
         sb.AppendLine();
      }

      Int32 SimplifyRecursionCount = 0;
      internal override ParserAction Simplify() // ReduceAction
      {
         if (SimplifyRecursionCount > 2)
            return this;

         /* After replacing all Definitions by reduceActions
             * there may exist identical reduce actions in the list of all reduce actions. 
             * Replace all references by a reference to the first one
             */

         while (NextAction is LookaheadOrNonterminalTransition LaOrNt)
         {
            if (LaOrNt.NextAction is not Definition)
               NextAction = LaOrNt.NextAction; // may be again a nonterminal transition
         }

         Debug.Assert(NextAction is not NonterminalTransition);

         if (NextAction == GlobalVariables.EndOfGeneratedCodeInstance
            && StateStackAdjustment == 0
            && AttributeStackAdjustment == 0
            && SemanticMethod == null)
            return GlobalVariables.EndOfGeneratedCodeInstance;

         SimplifyRecursionCount++;

         ReduceAction? foundReduceAction = GlobalVariables.ListOfAllReductions.Find(
            (listedReduceAction) =>
            listedReduceAction.StateStackAdjustment == StateStackAdjustment
            && listedReduceAction.SemanticMethod == SemanticMethod
            && listedReduceAction.AttributeStackAdjustment == AttributeStackAdjustment
            && listedReduceAction.NextAction == NextAction
            //            && (listedReduceAction.NextAction = listedReduceAction.NextAction.Simplify()) == NextAction
            );

         SimplifyRecursionCount--;

         Debug.Assert(foundReduceAction != null); // ListOfAllReductions must contain this !

         if (!foundReduceAction.Description.Contains(Description))
         {
            foundReduceAction.Description =
               String.Concat(foundReduceAction.Description, Environment.NewLine, "or: ", Description);
         }

         return foundReduceAction; // is a preceding listed reduce action or the given reduce action
      }
   }

   internal abstract class ConditionalAction : ParserActionWithNextAction {
      /// <summary>
      /// <see cref="TerminalSymbols"/> are accepted by <see cref="TerminalTransition"/>s or checked
      /// by other actions e.g. <see cref="LookaheadAction"/>s 
      /// </summary>
      public BitArray TerminalSymbols {
         get; set;
      }

      public ConditionalAction(BitArray terminalSymbols, ParserAction nextAction) : base(nextAction)
      {
         TerminalSymbols = terminalSymbols;
      }

      /// <summary>
      /// Returns Int32.MaxValue if action has dynamic priority, assigned priority of lookadead action, 0 else
      /// </summary>
      public Int64 Priority {
         get {
            if (this is LookaheadAction laAction)
            {
               if (laAction.PriorityFunction != null)
                  return 2000000001; // should never be used or shown
               return laAction.ConstantPriority; // use assigned priority if no dynamic priority
            };

            if (this is PrioritySelectAction)
               return 2000000002; // should never be used or shown

            return 0; // priority of terminal transitions is always 0
         }
      }

      public void AppendPriorityTo(StringBuilder sb)
      {
         if (this is LookaheadAction laAction)
         {
            if (laAction.PriorityFunction != null)
               sb.Append(laAction.PriorityFunction.MethodName);
            else
               sb.Append(laAction.ConstantPriority);
            return;
         }

         if (this is PrioritySelectAction)
         {
            sb.Append(" see below ");
            return;
         }

         sb.Append(0);
      }

      public Boolean HasPriorityFunction()
         => this is LookaheadAction laAction && laAction.PriorityFunction != null;

      internal StringBuilder ConditionToStringbuilder(StringBuilder sb)
      {
         sb.Append("if Symbol == ");
         TerminalSymbols.BitsToStringbuilder(
            sb, GlobalVariables.TerminalSymbols, " | ", "all terminal symbols", "no terminal symbols")
            .AppendLine("; ");
         return sb;
      }

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         ConditionToStringbuilder(sb);
         return base.AppendToSB(sb);
      }

      /// <summary>
      /// The sum of the weights of all terminal symbols in <see cref="TerminalSymbols"/>, never 0 (replaced by 1)
      /// </summary>
      public Int64 SumOfWeights {
         get; private set;
      }

      /// <summary>
      /// The number of terminal symbols in <see cref="TerminalSymbols"/>
      /// </summary>
      public Int32 Terminalcount {
         get; private set;
      }

      /// <summary>
      /// Estimates how many if conditions might be generated to test if a symbol is one of the allowed symbols.
      /// 0 &lt;= <see cref="Complexity"/> &lt;= allowed symbols. Returns 0 if all or none.
      /// </summary>
      public Int32 Complexity {
         get; private set;
      }

      /// <summary>
      /// Computes <see cref="Terminalcount"/> and <see cref="SumOfWeights"/>
      /// </summary>
      /// <param name="TerminalSymbolByIndex">For each index in <see cref="TerminalSymbols"/> defines the terminal symbol.</param>
      public void ComputeTerminalcountSumOfWeightsComplexity(TerminalSymbol[] TerminalSymbolByIndex)
      {
         Terminalcount = 0;
         SumOfWeights = 0;

         for (Int32 index = 0; index < TerminalSymbols.Count; index++)
         {
            if (!TerminalSymbols[index])
               continue;

            Terminalcount++;
            SumOfWeights += TerminalSymbolByIndex[index].Weight;
         }

         if (SumOfWeights == 0)
            SumOfWeights++;

         Complexity = ComplexityOfBitsequence();
      }

      /// <summary>
      /// Estimates how many if conditions might be generated to test if a symbol is one of the allowed symbols.
      /// 0 &lt;= <see cref="ComplexityOfBitsequence"/> &lt;= allowed symbols. Returns 0 if all or none.
      /// </summary>
      /// <returns></returns>
      private Int32 ComplexityOfBitsequence()
      {
         /* Counts the number of comparisions which would be generated 
          * if all symbols would be relevant
         */
         Int32 result = 0;
         Boolean ActualValue = TerminalSymbols[0];
         Boolean BaseValue = ActualValue;
         Boolean NextValue;

         for (Int32 i = 1; i < TerminalSymbols.Count; i++)
         {
            NextValue = TerminalSymbols[i];
            /* We have an already analyzed sequence of values == BaseValue
             * then the ActualValue
             * then the NextValue
             * If all are equal no comparision has to be generated
             */
            if (ActualValue != BaseValue)
            {
               // different values: a comparision has to be generated
               result++; 
               if (NextValue != BaseValue)
               {
                  // The next value is equal to the actual value: a >= or <= comparision is sufficient
                  BaseValue = NextValue; // new checks are needed not before the value changes
               }
               // else the BaseValue is correct (equal to the NextValue)
               // one comparision with the actual value will be sufficient
            }
            ActualValue = NextValue;
         }
         // don't forget the last value
         if (ActualValue != BaseValue)
            result++;
         return result;
      }
   }

   /// <summary>
   /// Transition with TerminalSymbols to be accepted 
   /// </summary>
   internal sealed class TerminalTransition : ConditionalAction {
      internal TerminalTransition(Int32 number, BitArray terminalSymbols, ParserAction nextAction) :
         base(terminalSymbols, nextAction)
      {
         this.IdNumber = number;
      }

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isTerminalTransition;

      internal override void CountUsage(Boolean Accept)
      {
         Boolean CountNextaction = Calls == 0;

         if (Accept)
         {
            AcceptCalls++;
            // code of accept ... will be generated only once (if any):
            //    count subsequent code of terminal transition  only once
            if (AcceptCalls == 1)
               Calls++;
         }
         else
            Calls++;

         if (CountNextaction)
            NextAction?.CountUsage(true); // terminal transition: accept == true !!!
      }

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         ConditionToStringbuilder(sb)
           .Append("    then:  accept; then: ");
         if (NextAction == null)
            return sb.Append("no action (halt)");
         else
            return NextAction.AppendShortToSB(sb);
      }
   }

   // <summary>
   /// This class is used in phase 3
   /// </summary>
   internal abstract class LookaheadOrNonterminalTransition : ConditionalAction {

      public LookaheadOrNonterminalTransition(BitArray terminalSymbols, ParserAction nextAction)
         : base(terminalSymbols, nextAction)
      {
      }

      private static readonly HashSet<NonterminalTransition>
         emptyHashSet = new(0);

      internal HashSet<NonterminalTransition> Includes {
         get; set;
      } = emptyHashSet;

      internal void ClearIncludes()
      {
         Includes = emptyHashSet;
      }
   }


   /// <summary>
   /// A nonterminal transition denotes a parser state change caused by a nonterminal.
   /// In phase 4 all nonterminal transitions are replaced by reduce actions.
   /// </summary>
   internal sealed class NonterminalTransition : LookaheadOrNonterminalTransition {
      //internal override HashSet<NonterminalTransition> Includes {
      //   get; set;
      //}

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isNonterminalTransition;
      internal NonterminalSymbol InputSymbol;

      internal NonterminalTransition(Int32 number, NonterminalSymbol inputSymbol, ParserAction nextAction, BitArray lookAheadSet)
         : base(lookAheadSet, nextAction)
      {
         this.IdNumber = number;
         this.InputSymbol = inputSymbol;
      }

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         sb.Append("if nonterminal == ")
           .AppendLine(InputSymbol.Identifier.ToString())
           .Append("    then: ");
         if (NextAction == null)
            sb.Append("no action (halt)");
         else
            NextAction.AppendShortToSB(sb);
         return sb;
      }

      internal override void CountUsage(Boolean Accept)
          => throw new ErrorInGrammlatorProgramException(
              $"Program error: NonterminalTransition.CountUsage({Accept}) must never be called.");

      internal override ParserAction Simplify() // NonterminalTransition
      {
         /* Each NonterminalTransition is a ConditionalAction of a specific ParserState.
          * The NextAction of a NonterminalTransition initially is
          * a ParserState or a Definition and my be changed by transformations.
          * 
          * All Definitions occuring as NextAction of a NonterminalTransition,
          * a TerminalTransition or a LookaheadAction
          * are replaced step by step by some other ParserAction.
          * This other ParserAction may be another NonterminalTransition.
          * 
          * As long as its NextAction is a Definition the NonterminalTransition
          * can not be replaced anywhere. 
          */
         if (NextAction is Definition)
            return this;

         /* 
          * As ConditionalAction of a ParserState it must not be replaced by its NextAction
          * except in very special cases: see ParserState.Simplify()).
          * 
          * Otherwise a NonterminalTransition occuring as NextAction can
          * be replaced by its NextAction.
          * 
          * After all Definitions of the grammar are replaced the NonterminalTransitions are not longer used.
          * 
          */
         return NextAction.Simplify();
      }
   }

   /// <summary>
   /// Look ahead actions are generated in phase 2
   /// </summary>
   internal sealed class LookaheadAction : LookaheadOrNonterminalTransition {
      //internal override HashSet<NonterminalTransition> Includes {
      //   get; set;
      //}

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isLookaheadAction;

      internal readonly Int64 ConstantPriority = 0;
      internal readonly IntMethodClass? PriorityFunction = null;

      /// <summary>
      /// constructor, assigning a definition as NextAction
      /// </summary>
      /// <param name="number">a unique number within all look ahead actions</param>
      /// <param name="definition">the <see cref="Definition"/> to be applied when the parser executes the <see cref="LookaheadAction"/></param>
      internal LookaheadAction(Int32 number, BitArray lookAheadSet, ParserAction nextAction)
         : base(lookAheadSet, nextAction)
      {
         this.IdNumber = number;
         if (NextAction is Definition definition)
         {
            this.ConstantPriority = definition.ConstantPriority;
            this.PriorityFunction = definition.PriorityFunction;
         }
      }

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         base.AppendToSB(sb);
         return sb;
      }

      internal override ParserAction Simplify() // LookaheadAction
      {
         /* Each LookaheadAction is a ConditionalAction of a specific ParserState.
          * The NextAction of a LookaheadAction initially is
          * a ParserState or a Definition and my be changed by transformations.
          * 
          * All Definitions occuring as NextAction of a NonterminalTransition,
          * a TerminalTransition or a LookaheadAction
          * are replaced step by step by some other ParserAction.
          * This other ParserAction may be another NonterminalTransition.
          * 
          * As long as its NextAction is a Definition the LookaheadAction
          * can not be replaced anywhere. */
         if (NextAction is Definition)
            return this;

         /* As ConditionalAction of a ParserState it must not be replaced by its NextAction
          * except in very special cases: see ParserState.Simplify()).
          * 
          * Otherwise a NonterminalTransition occuring as NextAction can
          * be replaced by its NextAction.
          */
         return NextAction.Simplify();
      }

   }

   internal sealed partial class PriorityBranchAction : ParserAction {
      internal PriorityBranchAction(
            ConditionalAction? constantPriorityAction,
            ListOfParserActions dynamicPriorityActions
         )
      {
         ConstantPriorityAction = constantPriorityAction;
         ConstantPriority = constantPriorityAction?.Priority ?? 0;
         DynamicPriorityActions = new ListOfParserActions(dynamicPriorityActions);

         PriorityFunctions = new IntMethodClass[dynamicPriorityActions.Count];
         for (Int32 i = 0; i < dynamicPriorityActions.Count; i++)
         {
            PriorityFunctions[i]
               = ((Definition)((ConditionalAction)dynamicPriorityActions[i]).NextAction).PriorityFunction!;
         }
      }

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isPriorityBranchAction;

      internal ParserActionWithNextAction? ConstantPriorityAction;
      // ConstantPriority is copied from ConstantPriorityAction because ConstantPriorityAction may be modified later
      internal readonly Int64 ConstantPriority;
      internal readonly ListOfParserActions DynamicPriorityActions; // the elements of the list may be modified later !
                                                                    // The PriorityFunctions are copied from PriorityActions because those may be modified later
      internal readonly IntMethodClass[] PriorityFunctions;

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         sb.AppendLine("Select by dynamic priority: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");

         if (ConstantPriorityAction != null)
         {
            sb.Append("    Priority ").Append(ConstantPriority).Append(": ")
              .Append("    ");

            if (ConstantPriorityAction is ConditionalAction ca)
               ca.NextAction.AppendShortToSB(sb); // the condition (terminal symbols) must not be used
            else
               ConstantPriorityAction.AppendShortToSB(sb);
            sb.AppendLine();
         }
         // PriorityFunctions and DynamicPriorityActions AppendToSB(sb);
         for (Int32 i = 0; i < DynamicPriorityActions.Count; i++)
         {
            sb.Append("    ")
              .Append(PriorityFunctions[i].MethodName)
              .Append(": ");
            if (DynamicPriorityActions[i] is ConditionalAction ca)
               ca.NextAction.AppendShortToSB(sb); // the condition (terminal symbols) must not be used
            else
               DynamicPriorityActions[i].AppendShortToSB(sb);
            sb.AppendLine();
         }

         return sb;
      }

      internal override void CountUsage(Boolean Accept)
      {
         base.CountUsage(Accept);

         if (Calls != 1)
            return;

         if (ConstantPriorityAction != null)
         {
            if (ConstantPriorityAction is TerminalTransition t)
               t.NextAction.CountUsage(true);
            else
               ((LookaheadAction)ConstantPriorityAction).NextAction.CountUsage(false);
         }

         foreach (ParserAction a in DynamicPriorityActions)
            ((LookaheadAction)a).NextAction.CountUsage(false);
      }
   }
   internal sealed partial class PrioritySelectAction : ConditionalAction {

      /// <summary>
      /// Constructor
      /// </summary>
      /// <param name="inputSymbols"></param>
      /// <param name="constantPriorityAction"><see cref="null"/> or <see cref="LookaheadAction"/>
      /// or <see cref="TerminalTransition"/></param>
      /// <param name="dynamicPriorityActions">List of <see cref="ConditionalAction"/>s</param>
      internal PrioritySelectAction(
            BitArray inputSymbols,
            ConditionalAction? constantPriorityAction,
            ListOfParserActions dynamicPriorityActions
         ) : base(new BitArray(inputSymbols), new PriorityBranchAction(constantPriorityAction, dynamicPriorityActions))
      {
      }

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isPrioritySelectAction;

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         sb.Append("if Symbol == ");
         TerminalSymbols.BitsToStringbuilder(
            sb, GlobalVariables.TerminalSymbols, " | ", "all terminal symbols", "no terminal symbols")
            .AppendLine("; ");

         sb.Append("    then: ");
         NextAction.AppendToSB(sb);

         return sb;
      }
   }

   internal sealed class AcceptAction : ConditionalAction {
      internal readonly String InputClass;
      internal AcceptAction(BitArray terminalSymbols, String InputClass, ParserAction nextAction)
         : base(terminalSymbols, nextAction)
         => this.InputClass = InputClass;
      internal override ParserActionEnum ParserActionType => ParserActionEnum.isAcceptAction;

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         sb.Append("accept, then :");
         return base.AppendToSB(sb);
      }
   }

   /// <summary>
   /// <see cref="ErrorhandlingAction"/>s are added to the states in Phase 4 and used for code generation in phase5
   /// </summary>
   internal sealed partial class ErrorhandlingAction : ConditionalAction {
      internal override ParserActionEnum ParserActionType => ParserActionEnum.isErrorhandlingAction;

      ///<summary>
      /// Constructs the <see cref="ErrorhandlingAction"/> and adds it to the <see cref="GlobalVariables.ListOfAllErrorhandlingActions"/>.
      /// </summary>
      /// <param name="lookAhead"></param>
      /// <param name="idNumber"></param>
      /// <param name="state"></param>
      internal ErrorhandlingAction(BitArray lookAhead, Int32 idNumber, ParserState state)
         : base(lookAhead, GlobalVariables.ErrorHaltInstance)
      {
         this.IdNumber = idNumber;
         GlobalVariables.ListOfAllErrorhandlingActions.Add(this);
         State = state;
      }

      internal override void CountUsage(Boolean Accept) // ErrorhandlingAction
      {
         Debug.Assert(this.Calls == 0 && this.AcceptCalls == 0);
         base.CountUsage(Accept);

         // A goto to State will be generated if and only if ErrorHandlerIsDefined
         if (GlobalSettings.NameOfErrorHandlerMethod.Value != "")
            State.CountUsage(false);
      }

      internal ParserState State;

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         return ConditionToStringbuilder(sb)
           .Append("    then: call error handler from state ")
           .Append(State.IdNumber + 1);
      }
   }

   internal sealed partial class HaltAction : ParserActionWithNextAction {
      /// <summary>
      /// Construct halt action with unique number>=1 and number of attributes to store
      /// </summary>
      /// <param name="IdNumber">unique number >= 0</param>
      /// <param name="AttributestackAdjustement">=0</param>
      internal HaltAction(Int32 IdNumber, Int32 AttributestackAdjustement) : base(GlobalVariables.EndOfGeneratedCodeInstance)
      {
         Debug.Assert(AttributestackAdjustement >= 0, $"{nameof(AttributestackAdjustement)} must be >= 0");
         this.AttributestackAdjustment = AttributestackAdjustement;
         this.IdNumber = IdNumber;
      }

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isHaltAction;

      internal Int32 AttributestackAdjustment {
         get; set;
      }

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         sb.Append(P5CodegenCS.GotoLabel(this, false)).
            Append(", AttributestackAdjustment ").
            Append(AttributestackAdjustment);
         base.AppendToSB(sb);
         return sb;
      }

      internal override StringBuilder AppendShortToSB(StringBuilder sb)
          => sb.Append(P5CodegenCS.GotoLabel(this, false));
   }

   /// <summary>
   /// There is only one instance of this class: <see cref="GlobalVariables.ErrorHaltInstance"/>,
   /// which is referenced by <see cref="ErrorhandlingAction"/>s.
   /// This action typically calls a user method, resets the stack pointers and jumps to the end of generated code
   /// </summary>
   internal sealed partial class ErrorHaltAction : ParserActionWithNextAction {
      internal ErrorHaltAction() : base(GlobalVariables.EndOfGeneratedCodeInstance) { }

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isErrorhaltAction;

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         sb.Append(P5CodegenCS.GotoLabel(this, false));
         base.AppendToSB(sb);
         return sb;
      }

      internal override StringBuilder AppendShortToSB(StringBuilder sb)
          => sb.Append(P5CodegenCS.GotoLabel(this, false));
   }

   internal sealed class DeletedParserAction : ParserAction {
      // : ActionWithNextAction" ??? no, NextAction should be ignored because it can not be reached
      public ParserAction? NextAction {
         get;
      }

      public DeletedParserAction(ParserAction NextAction) => this.NextAction = NextAction;
      public DeletedParserAction() => this.NextAction = null;

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isDeletedParserAction;

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         sb.AppendLine("action deleted by optimization: ")
           .Append("      ");
         NextAction?.AppendShortToSB(sb);
         return sb;
      }
   }

   /// <summary>
   /// There is only one instance of this class: <see cref="GlobalVariables.EndOfGeneratedCodeInstance"/>,
   /// which is referenced by <see cref="ErrorhandlingAction"/>s.
   /// This action typically calls a user method, resets the stack pointers and jumps to the end of gnerated code
   /// </summary>
   internal sealed partial class EndOfGeneratedCodeAction : ParserAction {

      internal EndOfGeneratedCodeAction() { }

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isEndOfGeneratedCode;

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         sb.Append(P5CodegenCS.GotoLabel(this, false));
         base.AppendToSB(sb);
         return sb;
      }

      internal override StringBuilder AppendShortToSB(StringBuilder sb)
          => sb.Append(P5CodegenCS.GotoLabel(this, false));
   }

   internal sealed partial class PushStateAction : ParserActionWithNextAction {

      internal PushStateAction(Int32 idNumber, Int32 stateStackNumber, ParserAction nextAction)
         : base(nextAction)
      {
         this.IdNumber = idNumber;
         this.StateStackNumber = stateStackNumber;
      }

      internal override ParserActionEnum ParserActionType {
         get {
            return ParserActionEnum.isPushStateAction;
         }
      }

      internal override StringBuilder AppendToSB(StringBuilder sb)
      {
         sb.Append(P5CodegenCS.GotoLabel(this, false)).
            Append("(push value: ").
            Append(StateStackNumber).
            AppendLine(") ");
         base.AppendToSB(sb);
         return sb;
      }

      internal override StringBuilder AppendShortToSB(StringBuilder sb)
      {
         sb.Append(P5CodegenCS.GotoLabel(this, false)).
            Append(" (push value: ").
            Append(StateStackNumber).
            AppendLine(")");

         return base.AppendShortToSB(sb);
      }
   }
}