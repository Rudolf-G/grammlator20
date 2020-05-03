using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Text;

namespace Grammlator {
   /// <summary>
   /// <see cref="ErrorInSourcedataException"/> is thrown in case of errors found by grammlator in the source file.
   /// The field Position (LineNumber and ColumnNumber) can be set as parameter of the constructor
   /// and accessed by e.ErrorPosition in exception handlers
   /// </summary>
   public class ErrorInSourcedataException: Exception {
      internal Int32 Position { get; private set; }

      /// <summary>
      /// Exception without additional information, sets Position to undefined
      /// </summary>
      public ErrorInSourcedataException()
          => Position = 0;

      /// <summary>
      /// Exception with message, sets Position to undefined
      /// </summary>
      /// <param name = "message" ></param>
      public ErrorInSourcedataException(String message) : base(message)
          => Position = 0;

      /// <summary>
      /// Is thrown when grammlator detects an error in the source.
      /// The given position is added ahead of the exception message in the form "Line ... column ..."
      /// </summary>
      /// <param name="position">The position where the error has been detected in the source</param>
      /// <param name="message"></param>
      public ErrorInSourcedataException(Int32 position, String message)
          //: base(String.Format("Line {1} column {2} {0}", message, position.LineNumber + 1, position.ColumnNumber + 1)) 
          : base(message) => Position = position;

      /// <summary>
      /// <see cref="ErrorInSourcedataException"/> is thrown when grammlator detects an error in the source.
      /// The given position is added ahead of the exception message in the form "Line ... column ..."
      /// </summary>
      /// <param name="position"></param>
      /// <param name="message"></param>
      /// <param name="innerException"></param>
      public ErrorInSourcedataException(Int32 position, String message, Exception innerException)
          : base(String.Format("{0} in source position {1}}.", message, position + 1), innerException) => Position = position;

      /// <summary>
      /// Exception caused by some unknown inner exception
      /// </summary>
      /// <param name="message"></param>
      /// <param name="innerException"></param>
      public ErrorInSourcedataException(String message, Exception innerException)
          : base(message, innerException) { }

      }

   /// <summary>
   /// Exception thrown in case of program errors
   /// </summary>
   public class ErrorInGrammlatorProgramException: Exception {
      /// <summary>
      /// Exception thrown in case of program errors
      /// </summary>
      public ErrorInGrammlatorProgramException()
         {
         }

      /// <summary>
      /// Exception thrown in case of program errors
      /// </summary>
      /// <param name="message"></param>
      public ErrorInGrammlatorProgramException(String message) : base(message)
         {
         }

      /// <summary>
      /// Exception thrown in case of program errors
      /// </summary>
      /// <param name="message"></param>
      /// <param name="innerException"></param>
      public ErrorInGrammlatorProgramException(String message, Exception innerException)
          : base(message, innerException) { }
      }

   internal enum MessageTypeOrDestinationEnum {
      noMessageType,
      SymbolProtocol, ConflictProtocol, StateProtocol1, StateProtocol2,
      Information, Warning, Status, Error, AbortIfErrors, Abort
      };

   /// <summary>
   ///  A definition is empty if it contains no elements or if all contained elements are empty
   /// </summary>
   internal enum EmptyComputationResultEnum {
      /// <summary>
      /// As default initial value meaning NotYetComputed, as result of a computation 
      /// meaning: the search limited by recursion didn't find an empty definition
      /// </summary>
      NotYetComputedOrRecursion = 0,

      /// <summary>
      /// IsJustBeingComputed must be replaced by another value by exactly
      /// the same method that sets this value  
      /// </summary>
      IsJustBeingComputed,

      /// <summary>
      /// Does not contain an empty definition or symbol
      /// </summary>
      NotEmpty,

      /// <summary>
      /// contains an empty definition or or a definition with all symbols IsOrContainsEmptyDefinition
      /// </summary>
      IsOrContainsEmptyDefinition
      }

   internal interface IUniqueIndex {
      Int32 IdNumber {
         get;
         }
      }

   /// <summary>
   /// The <see cref="SymmetricRelation{T}"/> is a dictionary 
   /// </summary>
   /// <typeparam name="T"></typeparam>
   [Serializable]
   internal class SymmetricRelation<T>: Dictionary<Int32, HashSet<T>>
       where T : IUniqueIndex {
      internal SymmetricRelation(Int32 size) : base(size / 4)
         {
         // try a quarter of the size (if needed, dictionary will be expanded)
         // the full size will be needed, if the implementation uses an array
         }

      protected SymmetricRelation(SerializationInfo info, StreamingContext context) : base(info, context)
         {
         }

      internal Boolean Contains(T element1, T element2)
         {
         return this.ContainsKey(element1.IdNumber)
               && this[element1.IdNumber].Contains(element2);
         //return (element1.IdNumber < element2.IdNumber)
         //    ? this.ContainsKey(element1.IdNumber)
         //      && this[element1.IdNumber].Contains(element2)
         //    : this.ContainsKey(element2.IdNumber)
         //      && this[element2.IdNumber].Contains(element1);
         }

      /// <summary>
      /// ContainsKey may be used to test the element if it is part of some relation, 
      /// so Add must be used for each element of a relation
      /// </summary>
      /// <param name="element"></param>
      /// <returns>true if added, false if already contained</returns>
      internal Boolean Add(T element)
         {
         if (ContainsKey(element.IdNumber))
            return false;
         base.Add(element.IdNumber, new HashSet<T>());
         return true;
         }

      /// <summary>
      /// adds element1->element2 and element2->element1 to the symmetric relation
      /// </summary>
      /// <param name="element1"></param>
      /// <param name="element2"></param>
      /// <returns>true if new relation, false if existing relation or element1==element2</returns>
      internal Boolean Add(T element1, T element2)
         {
         if (element1.IdNumber == element2.IdNumber)
            return false;

         // Symmetry can not be used to reduce storage as long as
         // methods of Dictionary can be called to evaluate the relation
         //if (element1.IdNumber > element2.IdNumber)
         //    return Add(element2, element1);

         // Prepare Relation State1->..., if not yet present
         if (!ContainsKey(element1.IdNumber))
            base.Add(element1.IdNumber, new HashSet<T>());

         // Store Relation State1->..., if not yet present
         HashSet<T> HashSetOfElement1 = this[element1.IdNumber];
         if (HashSetOfElement1.Contains(element2))
            return false; // the relation is already present
         HashSetOfElement1.Add(element2);

         // Prepare Relation State2->..., if not yet present 
         // so that ContainsKey(element2.IdNumber) returns true
         if (!ContainsKey(element2.IdNumber))
            base.Add(element2.IdNumber, new HashSet<T>());

         /* To allow usage of methods of Dictionary 
          * the symmetric relation is added 
          */
         // Store Relation State2->..., if not yet present
         HashSet<T> HashSetOfElement2 = this[element2.IdNumber];
         Debug.Assert(!HashSetOfElement2.Contains(element1));
         HashSetOfElement2.Add(element1);

         return true;
         }

#pragma warning disable IDE0051 // Nicht verwendete private Member entfernen
#pragma warning disable IDE0060 // Nicht verwendete private Member entfernen
#pragma warning disable RCS1163 // Unused parameter.
#pragma warning disable RCS1213 // Remove unused member declaration.
      /// <summary>
      /// This method hides base.Add(int Key, HashSet&lt;cAktionZustand&gt; hashSet) to prevent bugs
      /// </summary>
      /// <param name="keyIsIgnored">doesn't matter</param>
      /// <param name="hashSetIsIgnored">doesn't matter</param>
      /// <returns>always false</returns>
      /// <exception cref="ErrorInGrammlatorProgramException"></exception>
      new private static Boolean Add(Int32 keyIsIgnored, HashSet<T> hashSetIsIgnored)
         {
         Debug.Fail("this private method must not be called");
         throw new ErrorInGrammlatorProgramException
             ($"SymmetricRelation<>.Add({keyIsIgnored}, {hashSetIsIgnored} must not be called.");
         }
#pragma warning restore RCS1213 // Remove unused member declaration.
#pragma warning restore RCS1163 // Unused parameter.
#pragma warning disable IDE0060 // Nicht verwendete private Member entfernen
#pragma warning restore IDE0051 // Nicht verwendete private Member entfernen
      }

   internal struct AttributeStruct {
      /// <summary>
      /// The type of the attribute e.g."Int32"
      /// </summary>
      internal Int32 TypeStringIndex;
      internal Int32 NameStringIndex;

      /// <summary>
      /// true if the attribute occurs as attribute of the defined nonterminal (in the left side of a definition), default is false
      /// </summary>
      internal Boolean LeftSide;

      /// <summary>
      /// level of parenthesis (lowest is 0)
      /// </summary>
      internal Int32 Level;

      /// <summary>
      /// the first attribute of an outer production and of each of its definitions has position 1
      /// </summary>
      internal Int32 PositionInProduction;

      internal enum OverlayEnum {
         /// <summary>
         /// Not overlaying attribute in the right side or overlaying with same type and different name.
         /// May be not used (may create a warning), else must be a formal value parameter.
         /// Access by "PeekRef".
         /// </summary>
         inAttribute,

         /// <summary>
         /// Not overlaying attribute in the left side or overlaying with same type and different name.
         /// Must be associated to a formal out parameter.
         /// Access by "PeekRef".
         /// </summary>
         outAttribute,

         /// <summary>
         /// Overlaying attribute in the right side with different name and type as overlayed attribute.
         /// May be associated to a formal value parameter or not used (may create a warning).
         /// Access by "PeekClear" (!).
         /// </summary>
         inClearAttribute,

         /// <summary>
         /// Attribute in the left side overlayed with different type.
         /// Must be associated to a formal out-parameter.
         /// Access  by "PeekRef" if the overlaying attribute is associated to a formal parameter,
         /// else by "PeekRefClear".
         /// </summary>
         outClearAttribute,

         /// <summary>
         /// Overlaying attributes (left and right side) with same name (must be same type).
         /// Maybe associated to formal in-, out-, ref- or value-parameter (or not used).
         /// Access by "PeekRef".
         /// </summary>
         inOutAttribute
         }

      /// <summary>
      /// outAttribute, inAttribute (default), inOutAttribute
      /// </summary>
      internal OverlayEnum OverlayType;

      /// <summary>
      ///  The implementation of the associated parameter or <see cref="ParameterImplementation.NotAssigned"/>
      /// </summary>
      internal ParameterImplementation Implementation;

      internal void ToStringbuilder(StringBuilder sb)
          => sb.Append(GlobalVariables.GetStringOfIndex(TypeStringIndex))
               .Append(' ')
               .Append(GlobalVariables.GetStringOfIndex(NameStringIndex));

      /// <summary>
      /// returns an attribute struct with <see cref="LeftSide"/> == false,
      ///  <see cref="OverlayType"/> == inAttribute (to be updated later)
      ///  and <see cref="Implementation"/> == <see cref="ParameterImplementation.NotAssigned"/>
      /// </summary>
      /// <param name="type">The type of the attribute (e.g. Int32)</param>
      /// <param name="name">The name / identifier  of the attribute</param>
      /// <param name="level">the level of parenthesis (lowest level is level 0)</param>
      /// <param name="positionInProduction">first attribute of left side has position 1, 1st attribute of right side equally has position 1</param>
      internal AttributeStruct(Int32 typeStringIndex, Int32 nameStringIndex, Int32 level, Int32 positionInProduction)
         {
         TypeStringIndex = typeStringIndex;
         NameStringIndex = nameStringIndex;
         LeftSide = false; // has to be added later (as soon as the left side is recognized)
         Level = level;
         PositionInProduction = positionInProduction;
         OverlayType = OverlayEnum.inAttribute;
         Implementation = ParameterImplementation.NotAssigned;
         }
      }

   /// <summary>
   /// Stores the attributes while parsing a (maybe nested) definition
   /// </summary>
   internal class ListOfAttributes: List<AttributeStruct> {
      /// <summary>
      /// returns an attribute struct with LeftSide == true and Usage == inAttribute (to be updated later)
      /// </summary>
      /// <param name="type">The type of the attribute (e.g. Int32)</param>
      /// <param name="name">The name / identifier  of the attribute</param>
      /// <param name="level">the level of parenthesis (lowest level is level 0)</param>
      /// <param name="positionInProduction">first attribute of left side has position 1, 1st attribute of right side equally has position 1</param>

      internal Int32 Add(Int32 typeStringIndex, Int32 nameStringIndex, Int32 level, Int32 positionInProduction)
         {
         Debug.Assert(Count == 0 || level >= this[Count - 1].Level, "Level nicht aufsteigend.");
         // if (!(this.Count == 0 || level >= this[this.Count - 1].Level)) throw new Exception("Leveltest: Fehler");

         Add(new AttributeStruct(typeStringIndex, nameStringIndex, level, positionInProduction));
         return Count;
         }

      /// <summary>
      /// removes  the given number of elements (>=0) from the end of the list of attributes
      /// </summary>
      /// <param name="numberOfElementsToRemove"></param>
      internal void RemoveFromEnd(Int32 numberOfElementsToRemove)
         {
         if (numberOfElementsToRemove > 0)
            RemoveRange(Count - numberOfElementsToRemove, numberOfElementsToRemove);
         }

      internal void ToStringBuilder(StringBuilder sb)
         {
         Boolean isFirstElement = true;
         foreach (AttributeStruct p in this)
            {
            if (!isFirstElement)
               sb.Append(", ");
            p.ToStringbuilder(sb);
            isFirstElement = false;
            }
         }

      /// <summary>
      /// Returns an array of the identifiers of the last count attributes.  The array may be empty.
      /// </summary>
      /// <param name="count">The number of attributes to copy from the end of the ListOfAttributes, may be 0</param>
      /// <returns>array of the type identifiers</returns>
      internal Int32[] GetAttributeIdentifierStringIndexes(Int32 count)
         {
         if (count == 0)
            return Array.Empty<Int32>();

         var AttributeIdentifierStringIndexes = new Int32[count];
         for (Int32 i = 0; i < count; i++)
            AttributeIdentifierStringIndexes[i] = this[Count - count + i].NameStringIndex;

         return AttributeIdentifierStringIndexes;
         }

      /// <summary>
      /// Returns an array of the type identifiers of the last count attributes.  The array may be empty.
      /// </summary>
      /// <param name="count">The number of attributes to copy from the end of the ListOfAttributes, may be 0</param>
      /// <returns>array of the type identifiers</returns>
      internal Int32[] GetAttributeTypeStringIndexes(Int32 count)
         {
         if (count == 0)
            return Array.Empty<Int32>();

         var AttributeTypesStringIndex = new Int32[count];
         for (Int32 i = 0; i < count; i++)
            AttributeTypesStringIndex[i] = this[Count - count + i].TypeStringIndex;

         return AttributeTypesStringIndex;
         }
      }

   /// <summary>
   /// Modifier used for the formal parameter of the method,
   /// specifying the actual parameter to be used
   /// </summary>
   internal enum ParameterImplementation {
      /// <summary>
      ///  This default value is assigned to each attribute as long as no associated parameter is found
      ///  and to parameters which can not be assigned to attributes
      /// </summary>
      NotAssigned = 0,

      /// <summary>
      /// This formal parameter of the method has a ref modifier.
      /// The actual parameter will be "ref PeekRef(..)".
      /// </summary>      
      RefCall,

      /// <summary>
      /// This formal parameter of the method has an out modifier.
      /// The actual parameter will be "out PeekRef(..)".
      /// </summary>
      OutCall,

      /// <summary>
      /// This formal parameter of the method has an out modifier. The actual parameter
      /// must be cleared before the method is executed, because it is overlayed
      /// by an unused attribute of another type.
      /// The actual parameter will be "out PeekRefClear(..)".
      /// </summary>
      OutClearCall,

      /// <summary>
      /// This formal parameter of the method has no modifier (is a value parameter) or an in modifier
      /// and does not overlay an out parameter with a different type.
      /// The actual parameter may be "PeekRef(..)" or "PeekClear(..)".
      /// </summary>
      ValueOrInCall,

      /// <summary>
      /// This formal parameter of the method has no modifier (is a value parameter) or an in modifier.
      /// The actual parameter must be cleared, because it overlays an attribute in the left side with a different type.
      /// The actual parameter will be "PeekClear(..)".
      /// </summary>
      ValueOrInClearCall
      }

   /// <summary>
   /// Describes the characteristics of a formal method parameter assiciated to a grammlator attribute
   /// </summary>
   internal struct MethodParameterStruct {
      internal Int32 NameStringIndex;
      internal Int32 TypeStringIndex;

      /// <summary>
      /// The negative distance from the top of the attribute stack
      /// </summary>
      internal Int32 Offset;
      internal ParameterImplementation Implementation;
      }

   /// <summary>
   /// Stores the name and the parameters of a semantic method or priority specified in the grammar
   /// </summary>
   internal class MethodClass {
      internal readonly String MethodName;

      /// <summary>
      /// Array with one entry for each of the formal parameters of the method
      /// </summary>
      internal MethodParameterStruct[] MethodParameters; // TODO make non-nullable (change attributes of grammar!)

      internal MethodClass(String methodName)
         {
         MethodName = methodName;
         }
      }

   /// <summary>
   /// Stores the name and the parameters of a semantic method specified in the grammar
   /// </summary>
   internal class VoidMethodClass: MethodClass {
      internal VoidMethodClass(String methodName) : base(methodName)
         {
         }
      }

   /// <summary>
   /// Stores the name and the parameters of a semantic priority specified in the grammar
   /// </summary>
   internal class IntMethodClass: MethodClass {
      internal IntMethodClass(String methodName) : base(methodName)
         {
         }
      }

   /// <summary>
   ///  Extensions to Symbol[]
   /// </summary>
   internal static class ArrayOfSymbolExtensions {
      /// <summary>
      /// Checks if one of the Symbols ContainsAnEmptyAlternative
      /// </summary>
      /// <param name="SymbolArray"></param>
      /// <returns>
      /// <see cref="EmptyComputationResultEnum.IsOrContainsEmptyDefinition"/>,
      /// <see cref="EmptyComputationResultEnum.NotEmpty"/>
      /// <see cref="EmptyComputationResultEnum.NotYetComputedOrRecursion"/>
      /// </returns>
      /// <exception cref="ErrorInGrammlatorProgramException"></exception>
      internal static EmptyComputationResultEnum OneOfTheSymbolsContainsAnEmptyDefinition(this Symbol[] SymbolArray)
         {
         // Search for a symbol with an empty definition, assume there will be none
         var Result = EmptyComputationResultEnum.NotEmpty;

         foreach (Symbol Symbol in SymbolArray)
            {
            switch (Symbol.ContainsAnEmptyDefinition()) // may cause recursion
               {
            case EmptyComputationResultEnum.NotEmpty:
               break; // this symbol doesn't contain an empty definition, check next symbol of list

            case EmptyComputationResultEnum.IsOrContainsEmptyDefinition:
               return EmptyComputationResultEnum.IsOrContainsEmptyDefinition; // Success

            case EmptyComputationResultEnum.NotYetComputedOrRecursion:
                  {
                  Result = EmptyComputationResultEnum.NotYetComputedOrRecursion;
                  break; // this computation ended in recursion, check next symbol of list
                  }

            default:
                  {
                  Debug.Fail($"Error in {nameof(OneOfTheSymbolsContainsAnEmptyDefinition)}");
                  throw new ErrorInGrammlatorProgramException(
                      $"Error in {nameof(OneOfTheSymbolsContainsAnEmptyDefinition)}: illegal value");
                  }
               }
            } // foreach

         // Didn't find an empty definition
         return Result; // NotYetComputedOrRecursion or NotEmpty
         }

      internal static Int32 MarkAndCountAllUsedSymbols(this Symbol[] Symbolliste)
         {
         Int32 Zähler = 0;
         foreach (Symbol s in Symbolliste)
            Zähler += s.MarkAndCountAllUsedSymbols();

         return Zähler;
         }

      internal static void ToStringbuilder(this Symbol[] SymbolArray, StringBuilder sb, String separator = ", ")
          => SymbolArray.ToStringbuilder(sb, Int32.MaxValue, null, separator); // am Ende markieren

      internal static void ToStringbuilder(
            this Symbol[] SymbolArray,
            StringBuilder sb,
            Int32 Markierung,
            Int32[]? AttributnameStringIndexes,
            String separator = ", ")
         {
         Int32 Elementzähler = 0;
         Int32 NumberOfParameter = 0;

         foreach (Symbol s in SymbolArray)
            {
            Int32[]? NameStringIndexes = AttributnameStringIndexes;
            if (NameStringIndexes == null)
               {
               NameStringIndexes = s.AttributenameStringIndexList;

               NumberOfParameter = 0; // Zählen pro Symbol statt ab Listenanfang
               }

            if (Elementzähler != 0)
               {
               sb.Append(separator);
               }
            if (Elementzähler == Markierung)
               sb.Append("►");
            sb.Append(s.Identifier);

            if (s.NumberOfAttributes > 0)
               {
               sb.Append('(')
                 //.Append(Parameternummer + 1);
                 //.Append(':');
                 .Append(GlobalVariables.GetStringOfIndex(s.AttributetypeStringIndexList[0]))
                 .Append(' ')
                 .Append(
                  GlobalVariables.GetStringOfIndex(NameStringIndexes[NumberOfParameter++])
                 );

               for (Int32 i = 1; i < s.NumberOfAttributes; i++)
                  {
                  sb.Append(", ")
                    //.Append(Parameternummer + 1);
                    //.Append(':');
                    .Append(GlobalVariables.GetStringOfIndex(s.AttributetypeStringIndexList[i]))
                    .Append(' ')
                    .Append(
                     GlobalVariables.GetStringOfIndex(NameStringIndexes[NumberOfParameter++])
                     );
                  }

               sb.Append(')');
               }
            // nicht s.ToStringBuilder(sb), da sonst endlose Rekursion über terminale Symbole!
            Elementzähler++;
            }

         if (Markierung == SymbolArray.Length)
            sb.Append("●;"); // Enditem
         else if (Markierung == SymbolArray.Length + 1)
            sb.Append(";◄"); // Reduktion
         else
            sb.Append(';'); // Standard
         }
      }

   internal abstract class Symbol {

      internal Symbol(String identifier, Int32 position)
         {
         this.Identifier = identifier;
         this.FirstPosition = position;
         this.AttributetypeStringIndexList = Array.Empty<Int32>();
         this.AttributenameStringIndexList = Array.Empty<Int32>();
         }

      internal Symbol(
            String identifier,
            Int32 position,
            Int32 symbolNumber,
            Int32[] attributetypeStringIndexList)
         {
         this.Identifier = identifier;
         this.FirstPosition = position;
         this.SymbolNumber = symbolNumber;
         this.AttributetypeStringIndexList = attributetypeStringIndexList;
         this.AttributenameStringIndexList = Array.Empty<Int32>();
         }

      internal Symbol(
            String identifier,
            Int32 position,
            Int32 symbolNumber,
            Int32[] attributetypeStringIndexList,
            Int32[] attributenameStringIndexList
         )
         {
         this.Identifier = identifier;
         this.FirstPosition = position;
         this.SymbolNumber = symbolNumber;
         this.AttributetypeStringIndexList = attributetypeStringIndexList;
         this.AttributenameStringIndexList = attributenameStringIndexList;
         }

      internal readonly String Identifier;

      public readonly Int32 FirstPosition;

      public override String ToString() => Identifier;

      protected EmptyComputationResultEnum _EmptyComputationResult;

      internal virtual Boolean IsNonterminalWhichHasOnlyTrivialAlternatives
          => false; // overridden by NonterminalSymbol

      /// <summary>
      /// Only nonterminal symbols may be nullable. A nonterminal is nullable, if it produces the empty string.
      /// If this has not yet been computed
      /// <see cref="IsNullable"/> checks recursively if the symbol contains an empty definition or a definition
      /// which contains only symbols for which SymbolContainsAnEmptyDefinition is true.
      /// </summary>
      /// <exception cref="ErrorInGrammlatorProgramException"></exception>
      internal Boolean IsNullable {
         get {

            switch (_EmptyComputationResult)
               {
            case EmptyComputationResultEnum.NotEmpty:
            case EmptyComputationResultEnum.IsOrContainsEmptyDefinition:
               return _EmptyComputationResult == EmptyComputationResultEnum.IsOrContainsEmptyDefinition;

            case EmptyComputationResultEnum.NotYetComputedOrRecursion:
               // has not yet been computed: compute it (only once)
               _EmptyComputationResult = ContainsAnEmptyDefinition();

               if (_EmptyComputationResult == EmptyComputationResultEnum.NotYetComputedOrRecursion)
                  {
                  /* computation hase bean started from outside any recursion, 
                   * so recursion means that no empty alternative has been found */
                  _EmptyComputationResult = EmptyComputationResultEnum.NotEmpty;
                  return false;
                  }

               return _EmptyComputationResult == EmptyComputationResultEnum.IsOrContainsEmptyDefinition;

            default: // case eEmptyComputationResult.IsJustBeingComputed 
                  {
                  Debug.Fail($"Error in {nameof(IsNullable)}");
                  throw new ErrorInGrammlatorProgramException(
                      $"Error in {nameof(IsNullable)}");
                  }
               }
            }
         }

      /// <summary>
      /// abstract method, overriden in TerminalSymbol and NonterminalSymbol, 
      /// recursively computes, whether the symbol contains an empty definition.
      /// result may be IsOrContainsEmptyDefinition, NotEmpty, 
      /// IsJustBeingComputed (only if called by itself), NotYetComputedOrRecursion (if part of a recursion)
      /// </summary>
      internal abstract EmptyComputationResultEnum ContainsAnEmptyDefinition();

      /// <summary>
      /// The terminal symbols are numbered starting with 1, the nonterminal symbols are also numbered starting with 1
      /// </summary>
      internal Int32 SymbolNumber;
      internal Int32[] AttributetypeStringIndexList;
      internal Int32[] AttributenameStringIndexList;

      internal Int32 NumberOfAttributes => AttributetypeStringIndexList.Length;

      internal Boolean isUsed = false;

      /// <summary>
      /// Equivalent to "<code>this is <see cref="TerminalSymbol"/></code>"
      /// </summary>
      internal Boolean IsTerminalSymbol => this is TerminalSymbol;

      internal abstract String SymboltypeString {
         get;
         }

      /// <summary>
      /// Bestimmt, wie viele Symbole von diesem Symbol ausgehen erreichbar sind und
      /// setzt jeweils IstErreichbar auf true. Voraussetzung: IstErreichbar muss vorher false sein.
      /// </summary>
      /// <returns>Anzahl der von false auf true gesetzten IstErreichbar</returns>
      internal virtual Int32 MarkAndCountAllUsedSymbols()
         {
         Int32 Zähler = 0;
         if (!isUsed)
            {
            Zähler++;
            isUsed = true;
            }
         return Zähler;
         }

      internal StringBuilder IdentifierAndAttributesToSB(StringBuilder sb)
         {
         sb.Append(Identifier);
         if (NumberOfAttributes <= 0)
            {
            return sb;
            }

         sb.Append('(')
           .Append(GlobalVariables.GetStringOfIndex(AttributetypeStringIndexList[0]))
           .Append(' ')
           .Append(GlobalVariables.GetStringOfIndex(AttributenameStringIndexList[0]));
         for (Int32 i = 2; i <= NumberOfAttributes; i++)
            {
            sb.Append(", ")
              .Append(GlobalVariables.GetStringOfIndex(AttributetypeStringIndexList[i - 1]))
              .Append(' ')
              .Append(GlobalVariables.GetStringOfIndex(AttributenameStringIndexList[i - 1]));
            }
         sb.Append(')');
         return sb;
         }

      internal virtual void ToStringbuilder(StringBuilder sb)
         {
         sb.Append(SymboltypeString)
           .Append(" nr. ")
           .Append(SymbolNumber + 1)
           .Append(": ");
         IdentifierAndAttributesToSB(sb);

         if (IsNullable)
            sb.Append(", generates the empty string");

         if (!isUsed)
            sb.Append(", is not used in any definition");
         }

      }

   internal sealed class TerminalSymbol: Symbol {
      internal TerminalSymbol(String s, Int32 Position) : base(s, Position)
          => _EmptyComputationResult = EmptyComputationResultEnum.NotEmpty;  // Terminal symbols are never empty

      internal Int32 Weight;

      internal override String SymboltypeString => "terminal symbol";

      internal override EmptyComputationResultEnum ContainsAnEmptyDefinition()
          => EmptyComputationResultEnum.NotEmpty;

      internal override void ToStringbuilder(StringBuilder sb)
         {
         sb.Append(SymboltypeString)
             .Append(" nr. ")
             .Append((SymbolNumber + 1).ToString("D2"))
             .Append(": ");

         if (!String.IsNullOrEmpty(GlobalVariables.TerminalSymbolEnum))
            sb.Append(GlobalVariables.TerminalSymbolEnum).Append('.');

         IdentifierAndAttributesToSB(sb);

         if (!isUsed)
            sb.Append(", is not used  in any definition");
         }
      }

   internal sealed class NonterminalSymbol: Symbol {

      internal NonterminalSymbol(
            String identifier,
            Int32 position,
            Int32 symbolNumber,
            Int32[] attributetypeStringIndexList
         )
         : base(identifier, position, symbolNumber, attributetypeStringIndexList)
         {
            {
            NontrivialDefinitionsList = EmptyDefinitionsList;
            TrivalDefinitionsArray = Array.Empty<Symbol>();
            }
         }

      internal NonterminalSymbol(
            String identifier,
            Int32 position,
            Int32 symbolNumber,
            Int32[] attributetypeStringIndexList,
            Int32[] attributenameStringIndexList
         )
         : base(identifier, position, symbolNumber, attributetypeStringIndexList, attributenameStringIndexList)
         {
         NontrivialDefinitionsList = EmptyDefinitionsList;
         TrivalDefinitionsArray = Array.Empty<Symbol>();
         }

      internal NonterminalSymbol(
      String identifier,
      Int32 position,
      Int32 symbolNumber,
      Int32[] attributetypeStringIndexList,
      Int32[] attributenameStringIndexList,
      ListOfDefinitions nontrivalDefinitionsList,
      Symbol[] trivalDefinitionsArray
   )
   : base(identifier, position, symbolNumber, attributetypeStringIndexList, attributenameStringIndexList)
         {
         NontrivialDefinitionsList = nontrivalDefinitionsList;
         TrivalDefinitionsArray = trivalDefinitionsArray;
         }

      internal Boolean IsDefined {
         get { return TrivalDefinitionsArray.Length != 0 || NontrivialDefinitionsList.Count != 0; }
         }

      /// <summary>
      /// returns <see cref="NontrivialDefinitionsList"/>.Count == 0;
      /// </summary>
      internal override Boolean IsNonterminalWhichHasOnlyTrivialAlternatives
          => NontrivialDefinitionsList.Count == 0;

      internal override String SymboltypeString => "nonterminal symbol";
      internal ListOfDefinitions NontrivialDefinitionsList;
      private static readonly ListOfDefinitions EmptyDefinitionsList = new ListOfDefinitions(0);
      internal Symbol[] TrivalDefinitionsArray;

      /// <summary>
      /// Does nothing if Symbol.isUsed is true else Symbol.isUsed is set to true and all the symbols
      /// in the nonterminal symbols DefinitionLists are marked and counted.
      /// </summary>
      /// <returns>number of directly and indirectly used symbols</returns>
      internal override Int32 MarkAndCountAllUsedSymbols()
         {
         Int32 Counter = 0;
         if (!isUsed)
            {
            Counter++;
            isUsed = true;
            Counter += NontrivialDefinitionsList?.MarkAndCountAllUsedSymbols() ?? 0;
            Counter += TrivalDefinitionsArray?.MarkAndCountAllUsedSymbols() ?? 0;
            }
         return Counter;
         }

      internal override void ToStringbuilder(StringBuilder sb)
         {
         base.ToStringbuilder(sb);
         sb.AppendLine();
         if (TrivalDefinitionsArray.Length > 0)
            {
            sb.Append("    0. trivial rule(s): ");
            TrivalDefinitionsArray.ToStringbuilder(sb, separator: " | ");
            sb.AppendLine();
            }
         if (NontrivialDefinitionsList.Count == 0)
            {
            return;
            }
         // sb.AppendLine("    rules:");
         NontrivialDefinitionsList.ToStringbuilder(sb);
         }

      /// <summary>
      /// Tests the trivial and nontrivial definitions of the nonterminal symbol for an empty definition,
      /// <see cref="Symbol._EmptyComputationResult"/> must have a defined value!
      /// </summary>
      /// <returns>One of the values of <see cref="EmptyComputationResultEnum"/></returns>
      /// <exception cref="ErrorInGrammlatorProgramException"></exception>
      internal override EmptyComputationResultEnum ContainsAnEmptyDefinition()
         {
         switch (_EmptyComputationResult)
            {
         // if value has been already computed then return value
         case EmptyComputationResultEnum.NotEmpty:
         case EmptyComputationResultEnum.IsOrContainsEmptyDefinition:
            return _EmptyComputationResult;

         // if value is marked as "just beeing computed" do not change the value, but return "NotYetComputedOrRecursion"
         case EmptyComputationResultEnum.IsJustBeingComputed:
            return EmptyComputationResultEnum.NotYetComputedOrRecursion;

            }

         /* default: */

         Debug.Assert(_EmptyComputationResult == EmptyComputationResultEnum.NotYetComputedOrRecursion);

         // Try to compute the value. The computation may be stopped by recursion.

         _EmptyComputationResult = EmptyComputationResultEnum.IsJustBeingComputed; // avoid endless recursion
         Boolean SearchLimitedByRecursion = false;

         // Check nontrivial definitions of the nonterminal symbol
         switch (NontrivialDefinitionsList.ListContainsEmptyDefinition()) // this may cause recursion
            {
         case EmptyComputationResultEnum.IsOrContainsEmptyDefinition:
               {// success: an empty definition has been found
               _EmptyComputationResult = EmptyComputationResultEnum.IsOrContainsEmptyDefinition;
               return EmptyComputationResultEnum.IsOrContainsEmptyDefinition;
               }

         case EmptyComputationResultEnum.NotEmpty: // the symbol does not contain an empty alternative 
            break; // Search in nontrivial definitions

         case EmptyComputationResultEnum.NotYetComputedOrRecursion:
               {
               // The search is stopped caused by recursion without finding an empty definition.
               // If ContainsAnEmptyDefinition has been called from outside this means there is no empty definition.
               // If it has been called during another search process that must be continued and
               // computing the result for the current symbol must be reated later.
               // Then the chance of finding a result is better.
               // TODO Expand this simple algorithm to the Digraph algorithm which runs in linear time.
               SearchLimitedByRecursion = true;
               break;  // Search in nontrivial definitions
               }

         case EmptyComputationResultEnum.IsJustBeingComputed:
            break;

         default: // case ... .IsJustBeingComputed:
               {
               Debug.Fail($"Error in {nameof(ContainsAnEmptyDefinition)}");
               throw new ErrorInGrammlatorProgramException(
                   $"Error in {nameof(ContainsAnEmptyDefinition)}");
               }
            }

         // Check trivial definitions of the nonterminal symbol
         switch (TrivalDefinitionsArray.OneOfTheSymbolsContainsAnEmptyDefinition()
             )
            {
         // this computation may cause recursion
         case EmptyComputationResultEnum.IsOrContainsEmptyDefinition: // a descendant contains an empty alternative
               {
               _EmptyComputationResult = EmptyComputationResultEnum.IsOrContainsEmptyDefinition;
               return EmptyComputationResultEnum.IsOrContainsEmptyDefinition;
               }

         case EmptyComputationResultEnum.NotEmpty: // es wurde keine leere Alternative gefunden, bei den Alternativen trat eventuell Rekursion auf
            break;

         case EmptyComputationResultEnum.NotYetComputedOrRecursion:
               {
               SearchLimitedByRecursion = true;
               break;
               }

         default: //  case eEmptyComputationResult.IsJustBeingComputed:
               {
               Debug.Fail($"Error in {nameof(ContainsAnEmptyDefinition)}");
               throw new ErrorInGrammlatorProgramException(
                   $"Error in {nameof(ContainsAnEmptyDefinition)}");
               }
            }

         // keine Alternative bzw. Nachfolger gefunden, die bzw. der die leere Zeichenkette erzeugt
         _EmptyComputationResult =
            SearchLimitedByRecursion
             ? EmptyComputationResultEnum.NotYetComputedOrRecursion
             : EmptyComputationResultEnum.NotEmpty;

         return _EmptyComputationResult;
         }
      }
   }


