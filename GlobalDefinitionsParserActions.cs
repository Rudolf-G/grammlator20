using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Text;
using System.Windows.Media;

namespace Grammlator {
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

      internal static string LabelPrefix(ParserActionEnum e)
      {
         return e switch
         {
            ParserActionEnum.isDefinition => "ApplyDefinition",
            ParserActionEnum.isParserState => "State",
            ParserActionEnum.isPushStateAction => "PushState",
            ParserActionEnum.isLookaheadAction => "LookAhead",
            ParserActionEnum.isReduceAction => "Reduce",
            ParserActionEnum.isHaltAction => "ApplyStartsymbolDefinition",
            ParserActionEnum.isErrorhaltAction => "EndWithError",
            ParserActionEnum.isTerminalTransition => "TerminalTransition",
            ParserActionEnum.isAcceptAction => "Accept",
            ParserActionEnum.isNonterminalTransition => "NonterminalTransition",
            ParserActionEnum.isBranchAction => "Branch",
            ParserActionEnum.isPrioritySelectAction => "PrioritySelect",
            ParserActionEnum.isPriorityBranchAction => "PriorityBranch",
            ParserActionEnum.isErrorhandlingAction => "HandleError",
            ParserActionEnum.isDeletedParserAction => "Deleted",
            ParserActionEnum.isEndOfGeneratedCode => "EndOfGeneratedCode",
            _ => "UnknownAction"
         };
      }
   }

   internal abstract class ParserAction :
      EqualityComparer<ParserAction>, // implements: public Boolean Equals(ParserAction? a1, ParserAction? a2)
                                      // IComparable<ParserAction>, // implements: Int32 CompareTo([AllowNull] T other);
      IUniqueIndex  // implements: Int32 IdNumber {get;}
      {
      // bei Ableitung von IComparer<>  statt von IComparable<> wäre notwendig:
      // internal override int Compare(cAktion Aktion1, cAktion Aktion2) {
      //    return Aktion1.CompareTo(Aktion2);
      //    }

      /// <summary>
      /// The <see cref="ParserActionType"/>  is used for ToStringbuilder (...) and for comparisions
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
            throw new ArgumentNullException($"CompareTo({nameof(other)}) is null");

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
            !(a1 is null) && !(a2 is null)
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
      ///  Value -1: the state doesn't push a number on the stack.
      ///  <para> Optimization may move this information to other actions. Then the number x is replaced by (-x-2).</para>
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

      /// <summary>
      /// Generate the code implementing this action. 
      /// Return the next action to generate or null.
      /// </summary>
      /// <param name="codegen">The class that implements the generation of the code</param>
      /// <param name="accept"></param>
      /// <returns>the next action to generate or null</returns>
      internal virtual ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
         => throw new NotImplementedException($"Codegeneration is not implemented for {ParserActionType}");

      internal virtual StringBuilder ToStringbuilder(StringBuilder sb)
         => sb.Append("Action ")
           .Append(IdNumber + 1)
           .Append(", ")
           .AppendLine(ParserActionType.ToString());

      /// <summary>
      /// nonrecursive short description of the <see cref="ParserAction"/>
      /// </summary>
      /// <param name="sb"></param>
      internal virtual StringBuilder NameToSb(StringBuilder sb)
         => ToStringbuilder(sb); // some methods overwrite NameToSb to avoid endless recursion
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

      internal void RemoveFromEnd(int count) => RemoveRange(this.Count - count, count);

      internal virtual StringBuilder ToStringbuilder(StringBuilder sb)
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
               a.ToStringbuilder(sb);
               sb.AppendLine();
            }
         }
         return sb;
      }

      /// <summary>
      /// Yields all actions of the state whereby instead of a PrioritySelectActions its
      /// NextAction is used and if it is a PriorityBranchAction
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
         AttributeIdentifierStringIndexArray = Array.Empty<Int32>();
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
            throw new ArgumentNullException($"CompareTo({nameof(other)})");

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

      internal Int32 ConstantPriority;

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
      internal Int32[] AttributeIdentifierStringIndexArray;
      //   function ist_geordnet (v: t_Aktion): boolean; override;

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

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         sb.Append("recognized ");
         DefinedSymbol!.IdentifierAndAttributesToSB(sb).AppendLine("= ");
         sb.Append("      ");
         ToStringbuilder(sb, Int32.MaxValue); // do not mark any element
         return sb;
      }

      internal void ElementsToStringbuilder(StringBuilder sb, Int32 MarkiertesElement)
          => Elements.Append(sb, MarkiertesElement, this.AttributeIdentifierStringIndexArray);

      internal StringBuilder ToStringbuilder(StringBuilder sb, Int32 MarkiertesElement)
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

      internal virtual void ToStringbuilder(StringBuilder sb)
      {
         Int32 number = 1;
         if (this.Count == 0)
            sb.AppendLine(" - none - ");
         foreach (Definition d in this)
         {
            sb.Append("    ")
              .Append(number++)
              .Append(". ");
            d.ToStringbuilder(sb, Int32.MaxValue) // do not mark any element
             .AppendLine();
         }
      }

      public void RemoveFromEnd(Int32 n) => this.RemoveRange(this.Count - n, n);
   }

   internal sealed class BranchAction : ParserAction {
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

      internal override StringBuilder NameToSb(StringBuilder sb)
          => sb.Append(P5CodegenCS.GotoLabel(this, false));
   }

   internal class BranchcasesList : List<BranchcaseStruct> {
      internal BranchcasesList(Int32 capacity) : base(capacity) { }

      internal BranchcasesList()
      {
      }

      internal BranchcasesList(IEnumerable<BranchcaseStruct> Quelle) : base(Quelle) { }

      internal void SortByCondition()
      {
         base.Sort(Comparision);
      }

      int Comparision(BranchcaseStruct a, BranchcaseStruct b)
      {
         return a.BranchcaseCondition - b.BranchcaseCondition;
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

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         sb.Append("    then: ");
         return NextAction.NameToSb(sb);
      }
   }

   /// <summary>
   /// In phase4 definitions and nonterminal transitions are replaced by reduce actions with appropriate next actions
   /// (ParserState, BranchAction ...)
   /// </summary>
   internal sealed class ReduceAction : ParserActionWithNextAction {

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

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         sb.Append(P5CodegenCS.GotoLabel(this, false));
         AppendAdjustmentsAndMethod(sb);
         return base.ToStringbuilder(sb);
      }

      //internal override StringBuilder NameToSb(StringBuilder sb)
      //{
      //   sb.Append(P5CodegenCS.GotoLabel(this, false));
      //   return base.ToStringbuilder(sb);
      //}

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

      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         // Generate description
         codegen.IndentExactly();
         codegen.Append("/* ");
         if (StateStackAdjustment != 0)
         {
            codegen.Append("sAdjust: ");
            codegen.Append(-StateStackAdjustment);
            if (AttributeStackAdjustment != 0)
               codegen.Append(", ");
         }
         if (AttributeStackAdjustment != 0)
         {
            codegen.Append("aAdjust: ");
            codegen.Append(AttributeStackAdjustment);
         }
         if (StateStackAdjustment != 0 || AttributeStackAdjustment != 0)
            codegen.IndentExactly().Append(" * ");

         codegen.IndentAndAppendLines(Description, " * ");
         codegen.AppendLine(" */");

         // Generate instructions to handle the state stack
         if (this.StateStackAdjustment != 0)
         {
            if (this.StateStackAdjustment == 1)
            {
               codegen.IndentAndAppend(GlobalVariables.StateStack)
                  .AppendLine(".Pop(); ");
            }
            else
            {
               codegen.IndentAndAppend(GlobalVariables.StateStack)
                  .Append(".Discard(")
                  .Append(this.StateStackAdjustment)
                  .Append("); ");
            }
         }

         // Generate instructions to handle the attribute stack and to call the method

         if (this.AttributeStackAdjustment != 0 && this.FirstAdjustAttributeStackThenCallMethod)
            codegen.GenerateAttributeStackAdjustment(this.AttributeStackAdjustment);

         if (this.SemanticMethod != null)
         {
            codegen.IndentExactly()
               .AppendLine() // empty line preceding method call
               .GenerateSemanticMethodCall(this.SemanticMethod, asInstruction: true)
               .AppendLine(";")
               .AppendLine(); // empty line following method call
         }

         if (this.AttributeStackAdjustment != 0 && !this.FirstAdjustAttributeStackThenCallMethod)
         {
            codegen.Indent();
            codegen.GenerateAttributeStackAdjustment(this.AttributeStackAdjustment);
         }

         // Gegebenenfalls die Zeile beenden
         if (codegen.LineLength > 0)
            codegen.AppendLine();

         // return next action
         accept = false;
         Debug.Assert(
            this.NextAction != null,
            $"Error in Phase 5: {nameof(this.NextAction)} == null"
            );

         return this.NextAction;
      }
   }

   internal abstract class ConditionalAction : ParserActionWithNextAction {
      /// <summary>
      /// <see cref="TerminalSymbols"/> are accepted by <see cref="TerminalTransition"/>s or checked
      /// by other actions e.g. <see cref="LookaheadAction"/>s 
      /// </summary>
      public BitArray TerminalSymbols {
         get; set;
      } // als Folgesymbole oder als Eingabesymbole 

      public ConditionalAction(BitArray terminalSymbols, ParserAction nextAction) : base(nextAction)
      {
         TerminalSymbols = terminalSymbols;
      }

      /// <summary>
      /// Returns Int32.MaxValue if action has dynamic priority, assigned priority of lookadead action, 0 else
      /// </summary>
      public Int32 Priority {
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

      public bool HasPriorityFunction()
         => this is LookaheadAction laAction && laAction.PriorityFunction != null;

      internal StringBuilder ConditionToStringbuilder(StringBuilder sb)
      {
         sb.Append("if Symbol == ");
         TerminalSymbols.BitsToStringbuilder(
            sb, GlobalVariables.TerminalSymbolByIndex, " | ", "all terminal symbols", "no terminal symbols")
            .AppendLine("; ");
         return sb;
      }

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         ConditionToStringbuilder(sb);
         return base.ToStringbuilder(sb);
      }

      /// <summary>
      /// The sum of the weights of all terminal symbols in <see cref="TerminalSymbols"/>, never 0 (replaced by 1)
      /// </summary>
      public Int32 SumOfWeights {
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

         for (int index = 0; index < TerminalSymbols.Count; index++)
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
         /* Bestimmt die Zahl der Abfragen, die Bedingung_erzeugen generieren würde,
            wenn alle Symbole relevant wären  
         */
         Int32 result = 0;
         Boolean aktuellerWert = TerminalSymbols[0];
         Boolean Basiswert = aktuellerWert;
         Boolean NächsterWert;

         for (Int32 i = 1; i < TerminalSymbols.Count; i++)
         {
            NächsterWert = TerminalSymbols[i];
            // es liegt vor: eine bereits analysierte Folge von Werten == Basiswert
            // danach der aktuelle Wert,
            // danach der nächste Wert.
            // Wenn alle Werte gleich sind, ist kein Vergleich notwendig !
            if (aktuellerWert != Basiswert)
            {
               // Der aktuelle Wert ist anders als der Basiswert, also ist ein Vergleich zu generieren
               result++; // = oder >= Abfrage notwendig bzw. <> oder <=
               if (NächsterWert != Basiswert)
               {
                  // Der nächste Wert ist gleich zum aktuellen Wert, also genügt eine >= bzw. <= Abfrage
                  Basiswert = NächsterWert; // erneute Abfragen sind erst nötig, wenn der Wert wieder wechselt
               }
               // sonst ist der Basiswert bereits richtig (gleich dem nächsten Wert)
               // es genügt ebenfalls eine Abfrage, nämlich auf Gleichheit (bzw. Ungleichheit) mit dem aktuellen Wert
            }
            aktuellerWert = NächsterWert;
         }
         // letzten Wert noch berücksichtigen
         if (aktuellerWert != Basiswert)
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

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         ConditionToStringbuilder(sb)
           .Append("    then:  accept; then: ");
         if (NextAction == null)
            return sb.Append("no action (halt)");
         else
            return NextAction.NameToSb(sb);
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
         emptyHashSet = new HashSet<NonterminalTransition>(0);

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

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         sb.Append("if nonterminal == ")
           .AppendLine(InputSymbol.Identifier)
           .Append("    then: ");
         if (NextAction == null)
            sb.Append("no action (halt)");
         else
            NextAction.NameToSb(sb);
         return sb;
      }

      internal override void CountUsage(Boolean Accept)
          => throw new ErrorInGrammlatorProgramException(
              $"Program error: NonterminalTransition.CountUsage({Accept}) must never be called.");
   }

   /// <summary>
   /// Look ahead actions are generated in phase 2
   /// </summary>
   internal sealed class LookaheadAction : LookaheadOrNonterminalTransition {
      //internal override HashSet<NonterminalTransition> Includes {
      //   get; set;
      //}

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isLookaheadAction;

      internal readonly Int32 ConstantPriority = 0;
      internal readonly IntMethodClass? PriorityFunction = null;

      /// <summary>
      /// constructor, assigning a definition as NextAction
      /// </summary>
      /// <param name="number">a unique number within all look ahead actions</param>
      /// <param name="definition">the <see cref="Definition"/> to be applied when the parser executes the <see cref="LookaheadAction"/></param>
      internal LookaheadAction(Int32 number, Definition definition, BitArray lookAheadSet, ParserAction nextAction)
         : base(lookAheadSet, nextAction)
      {
         this.IdNumber = number;
         this.NextAction = definition;
         this.ConstantPriority = definition.ConstantPriority;
         this.PriorityFunction = definition.PriorityFunction;
      }

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         base.ToStringbuilder(sb);
         return sb;
      }
   }

   internal sealed class PriorityBranchAction : ParserAction {
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

      internal ParserAction? ConstantPriorityAction;
      // ConstantPriority is copied from ConstantPriorityAction because ConstantPriorityAction may be modified later
      internal readonly Int32 ConstantPriority;
      internal readonly ListOfParserActions DynamicPriorityActions; // the elements of the list may be modified later !
                                                                    // The PriorityFunctions are copied from PriorityActions because those may be modified later
      internal readonly IntMethodClass[] PriorityFunctions;

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         sb.AppendLine("Select by dynamic priority: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");

         if (ConstantPriorityAction != null)
         {
            sb.Append("    Priority ").Append(ConstantPriority).Append(": ")
              .Append("    ");

            if (ConstantPriorityAction is ConditionalAction ca)
               ca.NextAction.NameToSb(sb); // the condition (terminal symbols) must not be used
            else
               ConstantPriorityAction.NameToSb(sb);
            sb.AppendLine();
         }
         // PriorityFunctions and DynamicPriorityActions ToStringbuilder(sb);
         for (int i = 0; i < DynamicPriorityActions.Count; i++)
         {
            sb.Append("    ")
              .Append(PriorityFunctions[i].MethodName)
              .Append(": ");
            if (DynamicPriorityActions[i] is ConditionalAction ca)
               ca.NextAction.NameToSb(sb); // the condition (terminal symbols) must not be used
            else
               DynamicPriorityActions[i].NameToSb(sb);
            sb.AppendLine();
         }

         return sb;
      }

      internal override void CountUsage(Boolean Accept)
      {
         base.CountUsage(Accept);

         ConstantPriorityAction?.CountUsage(false);
         foreach (ParserAction a in DynamicPriorityActions)
            ((ConditionalAction)a).NextAction.CountUsage(false);  // TOCHECK accept of conditional action within prioritybranch ??
      }
   }
   internal sealed class PrioritySelectAction : ConditionalAction {

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

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         sb.Append("if Symbol == ");
         TerminalSymbols.BitsToStringbuilder(
            sb, GlobalVariables.TerminalSymbolByIndex, " | ", "all terminal symbols", "no terminal symbols")
            .AppendLine("; ");

         sb.Append("    then: ");
         NextAction.ToStringbuilder(sb);

         return sb;
      }
   }

   internal sealed class AcceptAction : ConditionalAction {
      internal readonly String InputClass;
      internal AcceptAction(BitArray terminalSymbols, String InputClass, ParserAction nextAction)
         : base(terminalSymbols, nextAction)
         => this.InputClass = InputClass;
      internal override ParserActionEnum ParserActionType => ParserActionEnum.isAcceptAction;

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         sb.Append("accept, then :");
         return base.ToStringbuilder(sb);
      }
   }

   /// <summary>
   /// <see cref="ErrorhandlingAction"/>s are added to the states in Phase 4 and used for code generation in phase5
   /// </summary>
   internal sealed class ErrorhandlingAction : ConditionalAction {
      internal override ParserActionEnum ParserActionType => ParserActionEnum.isErrorhandlingAction;

      ///<summary>
      /// Constructs the <see cref="ErrorhandlingAction"/> and adds it to the <see cref="GlobalVariables.ListOfAllErrorhandlingActions"/>.
      /// </summary>
      /// <param name="lookAhead"></param>
      /// <param name="idNumber"></param>
      /// <param name="state"></param>
      internal ErrorhandlingAction(BitArray lookAhead, Int32 idNumber, ParserState state)
         : base(lookAhead, GlobalVariables.TheOnlyOneErrorHaltAction)
      {
         this.IdNumber = idNumber;
         GlobalVariables.ListOfAllErrorhandlingActions.Add(this);
         State = state;
      }

      internal override void CountUsage(Boolean Accept)
      {
         Debug.Assert(this.Calls == 0 && this.AcceptCalls == 0);
         base.CountUsage(Accept);

         // A goto to State will be generated if and only if ErrorHandlerIsDefined
         if (GlobalVariables.ErrorHandlerIsDefined)
            State.CountUsage(false);
      }

      internal ParserState State;

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         return ConditionToStringbuilder(sb)
           .Append("    then: call error handler from state ")
           .Append(State.IdNumber + 1);
      }

      /// <summary>
      /// Generates the assignment to StateNumber and the call of ErrorHandler dependent on the global variables or
      /// if there is no error handling method defined, returns only the next action (error halt)
      /// </summary>
      /// <param name="accept"></param>
      /// <returns>next action: <see cref="ErrorHaltAction>"/> </returns>
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         if (!string.IsNullOrEmpty(GlobalVariables.ErrorHandlerMethod))
         {
            //codegen.IndentExactly();

            if (!string.IsNullOrEmpty(GlobalVariables.ErrorHandlerMethod))
            {   // generate ErrorHandlerCall   ErrorHandler(ErrorStateNumber, StateDescription, ParserInput);
               codegen
                  .Indent()
                  .Append("if (")
                  .Append(GlobalVariables.ErrorHandlerMethod)
                  .Append('(')
                  .Append(this.IdNumber + 1)
                  .Append(", ")
                  .Append(GlobalVariables.VariableNameStateDescription)
                  .Append(this.State.IdNumber + 1)
                  .Append(", ")
                  .Append(GlobalVariables.VariableNameSymbol)
                  .AppendLine("))");

               if (this.State.StateStackNumber >= 0)
               { // generate {_s.POP(..); goto state...;}
                  codegen
                     .IncrementIndentationLevel()
                     .Indent()
                     .AppendLine("{")
                     .IndentAndAppend(GlobalVariables.StateStack)
                     .AppendLine(".Pop(); ")
                     .GenerateGoto(this.State, accept: false)
                     .IndentAndAppendLine("};")
                     .DecrementIndentationLevel();
               }
               else
               { // generate goto state...;
                  codegen
                     .IncrementIndentationLevel()
                     .Indent()
                     .GenerateGoto(this.State, accept: false)
                     .DecrementIndentationLevel();
               }
            }
         }

         accept = false;
         return this.NextAction; // errorhandlingAction.NextAction == ErrorHalt  
      }
   }

   internal sealed class HaltAction : ParserActionWithNextAction {
      /// <summary>
      /// Construct halt action with unique number>=1 and number of attributes to store
      /// </summary>
      /// <param name="IdNumber">unique number >= 0</param>
      /// <param name="AttributestackAdjustement">=0</param>
      internal HaltAction(Int32 IdNumber, Int32 AttributestackAdjustement) : base(GlobalVariables.TheEndOfGeneratedCodeAction)
      {
         Debug.Assert(AttributestackAdjustement >= 0, $"{nameof(AttributestackAdjustement)} must be >= 0");
         this.AttributestackAdjustment = AttributestackAdjustement;
         this.IdNumber = IdNumber;
      }

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isHaltAction;

      internal Int32 AttributestackAdjustment {
         get; set;
      }

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         sb.Append(P5CodegenCS.GotoLabel(this, false)).
            Append(", AttributestackAdjustment ").
            Append(AttributestackAdjustment);
         base.ToStringbuilder(sb);
         return sb;
      }

      internal override StringBuilder NameToSb(StringBuilder sb)
          => sb.Append(P5CodegenCS.GotoLabel(this, false));

      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         codegen.IndentExactly();
         Int32 numberOfAttributesToStore = this.AttributestackAdjustment;
         Debug.Assert(numberOfAttributesToStore >= 0);

         codegen.IndentAndAppendLine("// Halt: a definition of the startsymbol with " + numberOfAttributesToStore.ToString() + " attributes has been recognized.");

         if (GlobalVariables.ListOfAllStates[0].StateStackNumber >= 0)
            codegen.IndentExactly()
               .Append(GlobalVariables.StateStack)
               .Append(".Pop();");

         if (numberOfAttributesToStore != 0)
         {
            codegen.Append("AttributesOfSymbol.CopyAndRemoveFrom(")
               .Append(GlobalVariables.AttributeStack)
               .Append(", ")
               .Append(numberOfAttributesToStore)
               .AppendLine(");");
         }

         accept = false;
         return this.NextAction;
      }
   }

   /// <summary>
   /// There is only one instance of this class: <see cref="GlobalVariables.TheOnlyOneErrorHaltAction"/>,
   /// which is referenced by <see cref="ErrorhandlingAction"/>s.
   /// This action typically calls a user method, resets the stack pointers and jumps to the end of gnerated code
   /// </summary>
   internal sealed class ErrorHaltAction : ParserActionWithNextAction {
      internal ErrorHaltAction() : base(GlobalVariables.TheEndOfGeneratedCodeAction) { }
      internal override ParserActionEnum ParserActionType => ParserActionEnum.isErrorhaltAction;

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         sb.Append(P5CodegenCS.GotoLabel(this, false));
         base.ToStringbuilder(sb);
         return sb;
      }

      internal override StringBuilder NameToSb(StringBuilder sb)
          => sb.Append(P5CodegenCS.GotoLabel(this, false));

      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         codegen.IndentAndAppendLine(
           "// This point is reached after an input error has been found");

         // generate _s.Pop(x)
         if (GlobalVariables.CountOfStatesWithStateStackNumber > 0)
            codegen.IndentExactly()
               .Append(GlobalVariables.StateStack)
               .Append(".Discard(")
               .Append(GlobalVariables.StateStack)
               .Append(".Count - ")
               .Append(GlobalVariables.StateStackInitialCountVariable)
               .AppendLine(");");
         // codegen.IndentAndAppendLine("_s.Discard(_s.Count - StateStackInitialCount);");  // TODO als Ressource definieren

         // generate _a.Free(x)
         if (GlobalVariables.reductionsModifyAttributStack)
            codegen.IndentExactly()
               .Append(GlobalVariables.AttributeStack)
               .Append(".Free(")
               .Append(GlobalVariables.AttributeStack)
               .Append(".Count - ")
               .Append(GlobalVariables.AttributeStackInitialCountVariable)
               .AppendLine(");");

         // generate additional instruction
         if (!String.IsNullOrEmpty(GlobalVariables.InstructionErrorHalt))
            codegen.IndentAndAppendLine(GlobalVariables.InstructionErrorHalt);

         accept = false;
         return GlobalVariables.TheOnlyOneErrorHaltAction.NextAction;
      }
   }

   internal sealed class DeletedParserAction : ParserAction { // : ActionWithNextAction" ??? no, NextAction should be ignored because it can not be reached
      public ParserAction NextAction {
         get;
      }

      public DeletedParserAction(ParserAction NextAction) => this.NextAction = NextAction;

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isDeletedParserAction;

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         sb.AppendLine("action deleted by optimization: ")
           .Append("      ");
         return NextAction.NameToSb(sb);
      }
   }

   /// <summary>
   /// There is only one instance of this class: <see cref="GlobalVariables.TheOnlyOneErrorHaltAction"/>,
   /// which is referenced by <see cref="ErrorhandlingAction"/>s.
   /// This action typically calls a user method, resets the stack pointers and jumps to the end of gnerated code
   /// </summary>
   internal sealed class EndOfGeneratedCodeAction : ParserAction {
      internal override ParserActionEnum ParserActionType => ParserActionEnum.isEndOfGeneratedCode;

      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         accept = false;
         return codegen.GenerateEndOfCodeAction();
      }

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         sb.Append(P5CodegenCS.GotoLabel(this, false));
         base.ToStringbuilder(sb);
         return sb;
      }

      internal override StringBuilder NameToSb(StringBuilder sb)
          => sb.Append(P5CodegenCS.GotoLabel(this, false));
   }

   internal class PushStateAction : ParserActionWithNextAction {

      internal PushStateAction(int idNumber, int stateStackNumber, ParserAction nextAction)
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
      internal override ParserAction? Generate(P5CodegenCS codegen, out Boolean accept)
      {
         accept = false;
         codegen.GenerateStateStackPushWithOptionalLinebreak(this.StateStackNumber);
         return this.NextAction;
      }

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
      {
         sb.Append(P5CodegenCS.GotoLabel(this, false)).
            Append("(push value: ").
            Append(StateStackNumber.ToString()).
            AppendLine(") ");
         base.ToStringbuilder(sb);
         return sb;
      }

      internal override StringBuilder NameToSb(StringBuilder sb)
      {
         sb.Append(P5CodegenCS.GotoLabel(this, false)).
            Append(" (push value: ").
            Append(StateStackNumber.ToString()).
            AppendLine(")");

         return base.NameToSb(sb);
      }




   }
}