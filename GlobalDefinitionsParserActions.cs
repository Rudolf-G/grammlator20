﻿using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Text;

namespace Grammlator {
   /// <summary>
   /// <see cref="ParserActionEnum"/> is used to sort the actions of a state and to output the name of the type of actions.
   /// The order of the enum elements corresponds to the order of the elements
   /// of <see cref="GlobalVariables.LabelPrefixes"/>
   /// </summary>
   internal enum ParserActionEnum {
      isDefinition,
      isParserState,
      isLookaheadAction,
      isReduceAction,
      isHaltAction,
      isErrorhaltAction,
      isTerminalTransition,
      isAcceptAction,
      isNonterminalTransition,
      isBranchAction,
      isPrioritySelectAction,
      isErrorhandlingAction,
      isDeletedParserAction,
      isEndOfGeneratedCode,
      isSomethingElse
      }  

   internal abstract class ParserAction:
      IEqualityComparer<ParserAction>,
      IComparable<ParserAction>, // implements: Int32 CompareTo([AllowNull] T other);
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
      public virtual Int32 CompareTo(ParserAction other)
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

      public Boolean Equals(ParserAction a1, ParserAction a2) {
         return a1.ParserActionType == a2.ParserActionType && a1.IdNumber == a2.IdNumber;
         }

      public Int32 GetHashCode(ParserAction a)
         {
         return (Int32)a.ParserActionType + a.IdNumber << 5;
         }

      /// <summary>
      /// A number &gt;= 0, unique within subclass (halts, reductions and branches are numbered separately.
      /// In protocols IdNumber+1 is used (human friendly).
      /// </summary>
      public Int32 IdNumber {
         get; internal set;
         }

      /// <summary>
      /// in phase3: used in digraph (0=&gt;not yet calculated); 
      /// in phase4: set to 0 (implements interface IParserAction)
      /// in phase5: Calls is used directly 
      /// </summary>
      public Int32 Codenumber {
         get { return Calls; }
         set { Calls = value; }
         }

      /// <summary>
      /// Used in Phase5: &gt; 0: number of usages, 0: never used or code has been generated, &lt; 0: -number of usages, code should not yet be generated
      /// </summary>
      internal Int32 Calls, AcceptCalls;

      /// <summary>
      /// Called in Phase5 to compute the values of Calls and AcceptCalls for Startaction and recursively for all accessible actions
      /// </summary>
      /// <param name="Accept"></param>
      internal virtual void CountUsage(Boolean Accept)
         {
         if (Accept)
            {
            AcceptCalls++;
            if (AcceptCalls == 1)
               Calls++; // accept will call the not accepting part
            }
         else
            {
            Calls++;
            }
         }

      /// <summary>
      /// Generate the code imlementing this action. 
      /// Return the next action to generate or null.
      /// </summary>
      /// <param name="codegen">The class that implements the generation of the code</param>
      /// <param name="accept"></param>
      /// <returns>the next action to generate or null</returns>
      internal virtual ParserAction Generate(ICodegen codegen, out Boolean accept)
         => throw new NotImplementedException($"Codegeneration is not mplemented for {ParserActionType}");

      internal virtual StringBuilder ToStringbuilder(StringBuilder sb)
         {
         sb.Append("Action ")
           .Append(IdNumber + 1)
           .Append(", ")
           .AppendLine(ParserActionType.ToString());
         return sb;
         }

      /// <summary>
      /// nonrecursive short description of the <see cref="ParserAction"/>
      /// </summary>
      /// <param name="sb"></param>
      internal virtual void NameToSb(StringBuilder sb)
          => sb.Append("Action Nr. ")
            .Append(IdNumber + 1)
            .Append(", ")
            .AppendLine(ParserActionType.ToString());// kein Bezug zu this.ToStringbuilder, da das zu Rekursion führen könnte
      }

   internal class ListOfParserActions: List<ParserAction> {
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
      }

   internal sealed class Definition: ParserAction {
      //   Nummer gibt an, um die wievielte Alternative des
      //     erzeugten Symbols es sich handelt
      //             Kontext in dieser Version noch nicht implementiert !!!}

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isDefinition;

      /// <summary>
      /// Compares this and other by ParserActionType, DefinedSymbol.SymbolNumer and ParserAction.IdNumber
      /// </summary>
      /// <param name="other"></param>
      /// <returns>if this &lt; other: result is &lt; 0; if equal result is &gt; 0 else is  0</returns>
      public override Int32 CompareTo(ParserAction other)
         {
         if (other == null)
            throw new ArgumentNullException($"CompareTo({nameof(other)})");

         Int32 result;

         if ((result = this.ParserActionType - other.ParserActionType) != 0)
            return result;

         // other is a Definition
         var otherNumber = ((Definition)other).DefinedSymbol.SymbolNumber;
         return (result = this.DefinedSymbol.SymbolNumber - otherNumber) != 0
             ? result
             : this.IdNumber - other.IdNumber;
         }

      internal NonterminalSymbol DefinedSymbol {
         get; set;
         }

      internal Int32 ConstantPriority;

      /// <summary>
      /// A Int32 method
      /// </summary>
      internal IntMethodClass PriorityFunction;

      /// <summary>
      /// A void method
      /// </summary>
      internal VoidMethodClass SemanticMethod;

      /// <summary>
      /// The code for <see cref="AttributestackAdjustment"/> &gt;0 has to be generated before semantic method calls,
      /// for &lt;0 after semantic method calls.
      /// </summary>
      internal Int32 AttributestackAdjustment;
      internal Symbol[] Elements;
      internal Int32[] AttributIdentifierStringIndexArray;
      //   function ist_geordnet (v: t_Aktion): boolean; override;

      /// <summary>
      /// Returns true, if no SemanticMethod, no AttributeStackCorrection, no PriorityFunction. 
      /// Does not (!) check, if DefinedSymbol is the startsymbol.
      /// </summary>
      /// <returns>true if no semantics are associated with the definition</returns>
      internal Boolean HasNoSemantics()
          => (SemanticMethod == null)
              && (AttributestackAdjustment == 0)
              && (PriorityFunction == null);
      // TOCHECK: Must dynamic priority be considered here or has it been evaluated in phase 4 ??

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
         {
         DefinedSymbol.IdentifierAndAttributesToSB(sb).Append("= ");
         ToStringbuilder(sb, Int32.MaxValue); // do not mark any element
         return sb;
         }

      internal override void NameToSb(StringBuilder sb) => ToStringbuilder(sb);

      internal void ElementsToStringbuilder(StringBuilder sb, Int32 MarkiertesElement)
          => Elements.ToStringbuilder(sb, MarkiertesElement, this.AttributIdentifierStringIndexArray);

      internal StringBuilder ToStringbuilder(StringBuilder sb, Int32 MarkiertesElement)
         {
         ElementsToStringbuilder(sb, MarkiertesElement);

         String Delimiter = "";

         sb.Append(' ');
         if (ConstantPriority != 0)
            {
            sb.Append("Priority: ")
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

   internal class ListOfDefinitions: List<Definition> {
      internal ListOfDefinitions(Int32 Anzahlelemente) : base(Anzahlelemente) { }

      internal ListOfDefinitions(Int32 AnzahlAlternativen, ListOfDefinitions Alternativen) :
          base(Alternativen.GetRange(Alternativen.Count - AnzahlAlternativen, AnzahlAlternativen))
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

   internal sealed class BranchAction: ParserAction {
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

      internal override void NameToSb(StringBuilder sb)
          => sb.Append("Verzweigung ")
               .Append(IdNumber + 1);// kein Bezug zu this.ToStringbuilder, da das zu Rekursion führen könnte
      }

   internal class BranchcasesList: List<BranchcaseStruct> {
      internal BranchcasesList(Int32 capacity) : base(capacity) { }

      internal BranchcasesList()
         {
         }

      internal BranchcasesList(IEnumerable<BranchcaseStruct> Quelle) : base(Quelle) { }
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

   internal abstract class ParserActionWithNextAction: ParserAction {
      public ParserAction NextAction {
         get; set;
         }

      internal override void CountUsage(Boolean Accept)
         {
         if (Calls > 0)
            {
            base.CountUsage(Accept);
            }
         else
            {
            base.CountUsage(Accept);
            NextAction?.CountUsage(false);
            }
         }

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
         {
         sb.Append("    next action: ");
         if (NextAction == null)
            sb.Append("no action (halt)");
         else
            NextAction.NameToSb(sb);
         return sb;
         }
      }

   /// <summary>
   /// In phase4 definitions and nonterminal transitions are replaced by reduce actions with appropriate next actions
   /// (ParserState, BranchAction ...)
   /// </summary>
   internal sealed class ReduceAction: ParserActionWithNextAction {
      internal override ParserActionEnum ParserActionType => ParserActionEnum.isReduceAction;

      /// <summary>
      /// A comment describing this reduce action
      /// </summary>
      internal String Description = null;

      internal Int32 StateStackAdjustment;
      internal Int32 AttributeStackAdjustment;
      internal IntMethodClass PriorityFunction;
      internal VoidMethodClass SemanticMethod;

      /// <summary>
      /// If true then the code for correction of the attribute stack has to be generated before the code caling the semantic method.
      /// This field can not be replaced by a check of AttributeStackCorrection after combining chains of reductions!
      /// </summary>
      internal Boolean FirstAdjustAttributeStackThenCallMethod;

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
         {
         sb.Append("reduction ").
             Append((IdNumber + 1).ToString()).Append(' ').
             Append("(sStack: ").
             Append(StateStackAdjustment.ToString()).Append(", ").
             Append("method: ").
             Append(SemanticMethod?.MethodName ?? "---").
             Append(", ").
             Append("aStack: ").
             Append(AttributeStackAdjustment.ToString()).
             AppendLine(") ");
         base.ToStringbuilder(sb);
         return sb;
         }

      internal override void NameToSb(StringBuilder sb)
         {
         sb.Append("reduction ").
             Append((IdNumber + 1).ToString()).
             Append(" (sStack: ").
             Append(StateStackAdjustment.ToString()).
             Append(", method: ").
             Append(SemanticMethod?.MethodName ?? "---").
             Append(", aStack: ").
             Append(AttributeStackAdjustment.ToString()).
             AppendLine(") "
             );

         base.ToStringbuilder(sb);
         }
      }

   internal abstract class ConditionalAction: ParserActionWithNextAction {
      /// <summary>
      /// <see cref="TerminalSymbols"/> are accepted by <see cref="TerminalTransition"/>s or checked
      /// by other actions e.g. <see cref="LookaheadAction"/>s 
      /// </summary>
      public BitArray TerminalSymbols {
         get; set;
         } // als Folgesymbole oder als Eingabesymbole 

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
         {
         if (TerminalSymbols == null)
            {
            sb.AppendLine(" no terminal symbols ");
            }
         else
            {
            TerminalSymbols.BitsToStringbuilder(
               sb, GlobalVariables.TerminalSymbolByIndex, " | ", "all terminal symbols", "no terminal symbols")
               .AppendLine("; ");
            }

         base.ToStringbuilder(sb);
         return sb;
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
         Int32 Index = 0;
         foreach (bool element in TerminalSymbols)
            {
            if (element)
               {
               Terminalcount++;
               SumOfWeights += TerminalSymbolByIndex[Index].Weight;
               }
            Index++;
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
   internal sealed class TerminalTransition: ConditionalAction {
      internal TerminalTransition(Int32 number, BitArray terminalSymbols, ParserAction nextAction)
         {
         this.IdNumber = number;
         this.TerminalSymbols = terminalSymbols;
         this.NextAction = nextAction;
         }

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isTerminalTransition;

      internal override void CountUsage(Boolean Accept)
         {
         Boolean HasNotYetBeenCounted = Calls == 0;

         if (Accept)
            {
            AcceptCalls++;
            if (AcceptCalls == 1)
               Calls++; // the 1st accept call calls 
            }
         else
            {
            Calls++;
            }

         if (HasNotYetBeenCounted)
            NextAction?.CountUsage(true); // terminal transition => true !!!
         }

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
         {
         sb.Append("accept: ");
         base.ToStringbuilder(sb);
         return sb;
         }
      }  

   // <summary>
   /// This class is used in phase 3
   /// </summary>
   internal abstract class LookaheadOrNonterminalTransition: ConditionalAction {
      internal abstract HashSet<NonterminalTransition> Includes {
         get; set;
         }
      }

   /// <summary>
   /// A nonterminal transition denotes a parser state change caused by a nonterminal.
   /// In phase 4 all nonterminal transitions are replaced by reduce actions.
   /// </summary>
   internal sealed class NonterminalTransition: LookaheadOrNonterminalTransition {
      internal override HashSet<NonterminalTransition> Includes {
         get; set;
         }

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isNonterminalTransition;
      internal NonterminalSymbol InputSymbol;

      internal NonterminalTransition(Int32 number, NonterminalSymbol inputSymbol, ParserAction nextAction)
         {
         this.IdNumber = number;
         this.InputSymbol = inputSymbol;
         this.NextAction = nextAction;
         }

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
         {
         sb.Append("Change state if nonterminal: ")
           .AppendLine(InputSymbol.Identifier)
           .Append("    terminal symbols following: ");
         base.ToStringbuilder(sb);
         return sb;
         }

      internal override void CountUsage(Boolean Accept)
          => throw new ErrorInGrammlatorProgramException(
              $"Program error: NonterminalTransition.CountUsage({Accept}) must never be called.");
      }

   /// <summary>
   /// Look ahead actions are generated in phase 2
   /// </summary>
   internal sealed class LookaheadAction: LookaheadOrNonterminalTransition {
      internal override HashSet<NonterminalTransition> Includes {
         get; set;
         }

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isLookaheadAction;

      internal Int32 ConstantPriority = 0;
      internal IntMethodClass PriorityFunction = null;

      internal LookaheadAction(Int32 number, Definition nextAction)
         {
         this.IdNumber = number;
         this.NextAction = nextAction;
         this.ConstantPriority = nextAction.ConstantPriority;
         this.PriorityFunction = nextAction.PriorityFunction;
         }

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
         {
         sb.Append("if Symbol ==  ");
         base.ToStringbuilder(sb);
         return sb;
         }
      }

   internal sealed class PrioritySelectAction: ConditionalAction {
      internal PrioritySelectAction(BitArray InputSymbols, ListOfParserActions Actions)
         {
         TerminalSymbols = InputSymbols;
         PriorityActions = Actions;
         }

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isPrioritySelectAction;
#pragma warning disable IDE0052 // Ungelesene private Member entfernen
      private readonly ListOfParserActions PriorityActions;
#pragma warning restore IDE0052 // Ungelesene private Member entfernen
      }

#pragma warning disable CA1812 // Avoid uninstantiated internal classes
   internal sealed class AcceptAction: ConditionalAction
#pragma warning restore CA1812 // Avoid uninstantiated internal classes
   {
      internal readonly String InputClass;
      internal AcceptAction(String InputClass) => this.InputClass = InputClass;
      internal override ParserActionEnum ParserActionType => ParserActionEnum.isAcceptAction;
      }

   /// <summary>
   /// <see cref="ErrorhandlingAction"/>s are added to the states in Phase 4 and used for code generation in phase5
   /// </summary>
   internal sealed class ErrorhandlingAction: ConditionalAction {
      internal override ParserActionEnum ParserActionType => ParserActionEnum.isErrorhandlingAction;

      ///<summary>
      /// Constructs the <see cref="ErrorhandlingAction"/> and adds it to the <see cref="GlobalVariables.ListOfAllErrorhandlingActions"/>.
      /// </summary>
      /// <param name="Folgesymbole"></param>
      /// <param name="idNumber"></param>
      /// <param name="state"></param>
      internal ErrorhandlingAction(BitArray Folgesymbole, Int32 idNumber, ParserState state)
         {
         this.TerminalSymbols = new BitArray(Folgesymbole);
         this.IdNumber = idNumber;
         this.NextAction = GlobalVariables.TheOnlyOneErrorHaltAction;
         GlobalVariables.ListOfAllErrorhandlingActions.Add(this);
         State = state;
         }

      internal override void CountUsage(Boolean Accept)
         {
         if (Calls > 0)
            {
            if (GlobalVariables.ErrorHandlerIsDefined)
               base.CountUsage(Accept);
            }
         else
            {
            if (GlobalVariables.ErrorHandlerIsDefined)
               base.CountUsage(Accept);
            NextAction?.CountUsage(false);
            }
         }

      internal ParserState State;

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
         {
         sb.Append("Error ")
           .Append(IdNumber + 1)
           .Append(" if: ");
         base.ToStringbuilder(sb);
         return sb;
         }

      internal override void NameToSb(StringBuilder sb)
          => sb.Append("Error handling ")
            .Append(IdNumber + 1);
      }

   internal sealed class HaltAction: ParserActionWithNextAction {
      /// <summary>
      /// Construct halt action with unique number>=1 and number of attributes to store
      /// </summary>
      /// <param name="IdNumber">unique number >= 0</param>
      /// <param name="AttributestackAdjustement">=0</param>
      internal HaltAction(Int32 IdNumber, Int32 AttributestackAdjustement)
         {
         Debug.Assert(AttributestackAdjustement >= 0, $"{nameof(AttributestackAdjustement)} must be >= 0");
         this.AttributestackAdjustment = AttributestackAdjustement;
         this.IdNumber = IdNumber;
         this.NextAction = GlobalVariables.TheEndOfGeneratedCodeAction;
         }

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isHaltAction;

      internal Int32 AttributestackAdjustment {
         get; set;
         }

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
         {
         sb.Append("halt action ")
             .Append(IdNumber).
             Append(", AttributestackAdjustment ").
             Append(AttributestackAdjustment);
         base.ToStringbuilder(sb);
         return sb;
         }

      internal override void NameToSb(StringBuilder sb)
          => sb.Append("halt nr. ")
            .Append(IdNumber + 1);
      }

   /// <summary>
   /// There is only one instance of this class: <see cref="GlobalVariables.TheOnlyOneErrorHaltAction"/>,
   /// which is referenced by <see cref="ErrorhandlingAction"/>s.
   /// This action typically calls a user method, resets the stack pointers and jumps to the end of gnerated code
   /// </summary>
   internal sealed class ErrorHaltAction: ParserActionWithNextAction {
      internal ErrorHaltAction() => NextAction = GlobalVariables.TheEndOfGeneratedCodeAction;
      internal override ParserActionEnum ParserActionType => ParserActionEnum.isErrorhaltAction;

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
         {
         sb.Append("error halt action: ");
         base.ToStringbuilder(sb);
         return sb;
         }

      internal override void NameToSb(StringBuilder sb)
          => sb.Append("error halt ");
      }

   internal sealed class DeletedParserAction: ParserAction { // : ActionWithNextAction" ??? no, NextAction should be ignored because it can not be reached
      public ParserAction NextAction {
         get;
         }

      public DeletedParserAction(ParserAction NextAction) => this.NextAction = NextAction;

      internal override ParserActionEnum ParserActionType => ParserActionEnum.isDeletedParserAction;

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
         {
         sb.AppendLine("action deleted by optimization: ")
           .Append("      ");
         return NextAction?.ToStringbuilder(sb);
         }
      }

   /// <summary>
   /// There is only one instance of this class: <see cref="GlobalVariables.TheOnlyOneErrorHaltAction"/>,
   /// which is referenced by <see cref="ErrorhandlingAction"/>s.
   /// This action typically calls a user method, resets the stack pointers and jumps to the end of gnerated code
   /// </summary>
   internal sealed class EndOfGeneratedCodeAction: ParserAction {
      internal override ParserActionEnum ParserActionType => ParserActionEnum.isEndOfGeneratedCode;

      internal override ParserAction Generate(ICodegen codegen, out Boolean accept)
         {
         accept = false;
         return codegen.GenerateEndOfCodeAction();
         }

      internal override StringBuilder ToStringbuilder(StringBuilder sb)
         {
         sb.Append("End of generated Code: ");
         base.ToStringbuilder(sb);
         return sb;
         }

      internal override void NameToSb(StringBuilder sb)
          => sb.Append("error halt ");
      }
   }
