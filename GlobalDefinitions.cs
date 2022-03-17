using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Text;
using System.Threading;

namespace grammlator {
   /// <summary>
   /// <see cref="ErrorInSourcedataException"/> is thrown in case of errors found by grammlator in the source file.
   /// The field Position (LineNumber and ColumnNumber) can be set as parameter of the constructor
   /// and accessed by e.ErrorPosition in exception handlers
   /// </summary>
   public class ErrorInSourcedataException : Exception {
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
          : base(String.Format("{0} in source position {1}.", message, position + 1), innerException) => Position = position;

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
   public class ErrorInGrammlatorProgramException : Exception {
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

   internal struct AttributeStruct {
      /// <summary>
      /// The type of the attribute e.g."Int32"
      /// </summary>
      internal readonly UnifiedString TypeString;
      internal readonly UnifiedString NameString;

      /// <summary>
      /// the first attribute of an outer production and of each of its definitions has position 1
      /// </summary>
      internal readonly Int32 PositionInProduction;

      /// <summary>
      /// true if the attribute occurs as attribute of the defined nonterminal (in the left side of a definition), default is false
      /// </summary>
      internal Boolean LeftSide;

      /// <summary>
      /// level of parenthesis (lowest is 0)
      /// </summary>
      internal Int32 Level;

      /// <summary>
      /// There are different possibilities how left and right side attributes can overlay
      /// (reference the same position in the attriobute stack). In some cases the right side
      /// attribute must be copied and cleared when accessed so that no unused reference may
      /// remain in the attribute stack
      /// </summary>
      internal enum OverlayEnum: Byte {
         /// <summary>
         /// Attribute in the right side not overlaying  or overlaying with same type and different name.
         /// <para>May not be used or may be associated to a formal value or in parameter.</para>
         /// Access by "PeekRef".
         /// </summary>
         inAttribute,

         /// <summary>
         /// Attribute in the left side not overlaying or overlaying with same type and different name.
         /// <para>Must be associated to a formal out parameter.</para>
         /// Access by "PeekRef".
         /// </summary>
         outAttribute,

         /// <summary>
         /// Attribute in the right side overlaying with different type (and name) as overlayed attribute.
         /// <para>May not be used or may be associated to a formal value or in parameter.</para>
         /// Access by "PeekClear" (!)
         /// </summary>
         inClearAttribute,

         /// <summary>
         /// Attribute in the left side overlayed with different type (and name).
         /// <para>Must be associated to a formal out-parameter.</para>
         /// Access  by "PeekRef" if the overlaying attribute is associated to a formal parameter (using PeekClear!),
         /// else by "PeekRefClear" (overlaying attribute is not used).
         /// </summary>
         outClearAttribute,

         /// <summary>
         /// Overlaying attributes (left and right side) with same name (must be same type).
         /// <para>Maybe associated to formal in-, out-, ref- or value-parameter (or not used).</para>
         /// Access by "PeekRef".
         /// </summary>
         inOutAttribute
      }

      /// <summary>
      /// inAttribute (default), outAttribute (if left side), other values while checking parameters
      /// </summary>
      internal OverlayEnum OverlayType;

      /// <summary>
      ///  default <see cref="ParameterImplementation.NotAssigned"/>, while checking parameters: implementation of the associated parameter (if any)
      /// </summary>
      internal ParameterImplementation Implementation;

      internal void Append(StringBuilder sb)
          => sb.Append(TypeString.ToString())
               .Append(' ')
               .Append(NameString.ToString());

      /// <summary>
      /// returns an attribute struct with <see cref="LeftSide"/> == false,
      ///  <see cref="OverlayType"/> == inAttribute (to be updated later)
      ///  and <see cref="Implementation"/> == <see cref="ParameterImplementation.NotAssigned"/>
      /// </summary>
      /// <param name="type">The type of the attribute (e.g. Int32)</param>
      /// <param name="name">The name / identifier  of the attribute</param>
      /// <param name="level">the level of parenthesis (lowest level is level 0)</param>
      /// <param name="positionInProduction">first attribute of left side has position 1, 1st attribute of right side equally has position 1</param>
      internal AttributeStruct(UnifiedString typeString, UnifiedString nameString, Int32 level, Int32 positionInProduction)
      {
         TypeString = typeString;
         NameString = nameString;
         PositionInProduction = positionInProduction;
         LeftSide = false; // has to be added later (as soon as the left side is recognized)
         Level = level; // may be changed if in left side within parentheses
         OverlayType = OverlayEnum.inAttribute;
         Implementation = ParameterImplementation.NotAssigned;
      }
      
   }

   /// <summary>
   /// Stores the attributes while parsing a (maybe nested) definition
   /// </summary>
   internal class ListOfAttributes : List<AttributeStruct> {
      /// <summary>
      /// returns an attribute struct with LeftSide == true and Usage == inAttribute (to be updated later)
      /// </summary>
      /// <param name="type">The type of the attribute (e.g. Int32)</param>
      /// <param name="name">The name / identifier  of the attribute</param>
      /// <param name="level">the level of parenthesis (lowest level is level 0)</param>
      /// <param name="positionInProduction">first attribute of left side has position 1, 1st attribute of right side equally has position 1</param>

      /// <summary>
      /// removes  the given number of elements (>=0) from the end of the list of attributes
      /// </summary>
      /// <param name="numberOfElementsToRemove"></param>
      internal void RemoveFromEnd(Int32 numberOfElementsToRemove)
      {
         if (numberOfElementsToRemove > 0)
            RemoveRange(Count - numberOfElementsToRemove, numberOfElementsToRemove);
      }

      /// <summary>
      /// Returns an array of the identifiers of the last count attributes.  The array may be empty.
      /// </summary>
      /// <param name="count">The number of attributes to copy from the end of the ListOfAttributes, may be 0</param>
      /// <returns>array of the type identifiers</returns>
      internal UnifiedString[] GetAttributeIdentifierStringIndexes(Int32 count)
      {
         if (count == 0)
            return Array.Empty<UnifiedString>();

         var AttributeIdentifierStringIndexes = new UnifiedString[count];
         for (Int32 i = 0; i < count; i++)
            AttributeIdentifierStringIndexes[i] = this[Count - count + i].NameString;

         return AttributeIdentifierStringIndexes;
      }

      /// <summary>
      /// Returns an array of the type identifiers of the last count attributes.  The array may be empty.
      /// </summary>
      /// <param name="count">The number of attributes to copy from the end of the ListOfAttributes, may be 0</param>
      /// <returns>array of the type identifiers</returns>
      internal UnifiedString[] GetAttributeTypeStringIndexes(Int32 count)
      {
         if (count == 0)
            return Array.Empty<UnifiedString>();

         var AttributeTypesString = new UnifiedString[count];
         for (Int32 i = 0; i < count; i++)
            AttributeTypesString[i] = this[Count - count + i].TypeString;

         return AttributeTypesString;
      }
   }

   /// <summary>
   /// One of these values is assigned to the implementation field of each formal parameter of semantic methods or dynamic priorities,
   /// depending on the type of the formal parameter (value, in, out, ref)
   /// and the overlay of left and right side attributes.
   /// </summary>
   internal enum ParameterImplementation: Byte {
      /// <summary>
      ///  This default value is assigned to a formal parameter until grammlator associates this parameter to an attribute.
      ///  Parameters which can not be assigned to attributes are an error in the grammlator input.
      /// </summary>
      NotAssigned = 0,

      /// <summary>
      /// The formal parameter of the method has a ref modifier.
      /// <para>The generated actual parameter will be "ref PeekRef(..)".</para>
      /// </summary>      
      RefCall,

      /// <summary>
      /// The formal parameter of the method has an out modifier.
      /// <para>The generated actual parameter will be "out PeekRef(..)".</para>
      /// </summary>
      OutCall,

      /// <summary>
      /// The formal parameter of the method has an out modifier.
      /// The actual parameter must be cleared before the method is executed,
      /// because it is overlayed by an unused attribute of another type.
      /// <para>The generated actual parameter will be "out PeekRefClear(..)".</para>
      /// </summary>
      OutClearCall,

      /// <summary>
      /// The formal parameter of the method has no modifier (is a value parameter) or an in modifier
      /// and does not overlay an out parameter with a different type.
      /// <para>The generated actual parameter will be "PeekRef(..)".</para>
      /// </summary>
      ValueOrInCall,

      /// <summary>
      /// The formal parameter of the method has no modifier (is a value parameter) or an in modifier.
      /// The actual parameter must be cleared, because it overlays an attribute in the left side with a different type.
      /// <para>The generated actual parameter will be "PeekClear(..)".</para>
      /// </summary>
      ValueOrInClearCall
   }

   /// <summary>
   /// Describes the characteristics of a formal method parameter assiciated to a grammlator attribute
   /// </summary>
   internal struct MethodParameterStruct {
      internal UnifiedString NameString;
      internal UnifiedString TypeString;

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
      internal readonly Int32 Position;

      /// <summary>
      /// Array with one entry for each of the formal parameters of the method
      /// </summary>
      internal MethodParameterStruct[] MethodParameters;

      internal MethodClass(String methodName, Int32 position)
      {
         MethodName = methodName;
         MethodParameters = Array.Empty<MethodParameterStruct>();
         Position = position;
      }
   }

   /// <summary>
   /// Stores the name and the parameters of a semantic method specified in the grammar
   /// </summary>
   internal class VoidMethodClass : MethodClass {
      internal VoidMethodClass(String methodName, Int32 position) : base(methodName, position)
      {
      }
   }

   /// <summary>
   /// Stores the name and the parameters of a semantic priority specified in the grammar
   /// </summary>
   internal class IntMethodClass : MethodClass {
      internal IntMethodClass(String methodName, Int32 position) : base(methodName, position)
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

      internal static void Append(this Symbol[] SymbolArray, StringBuilder sb, String separator = ", ")
          => SymbolArray.Append(sb, Int32.MaxValue, null, separator); // am Ende markieren

      internal static void Append(
            this Symbol[] SymbolArray,
            StringBuilder sb,
            Int32 Markierung,
            UnifiedString[]? AttributnameStringIndexes,
            String separator = ", ")
      {
         Int32 Elementzähler = 0;
         Int32 NumberOfParameter = 0;

         foreach (Symbol s in SymbolArray)
         {
            UnifiedString[]? NameStrings = AttributnameStringIndexes;
            if (NameStrings == null || NameStrings.Length == 0)
            {
               NameStrings = s.AttributenameStrings;

               NumberOfParameter = 0; // Zählen pro Symbol statt ab Listenanfang
            }

            if (Elementzähler != 0)
            {
               sb.Append(separator);
            }
            if (Elementzähler == Markierung)
               sb.Append('►');
            sb.Append(s.Identifier);

            if (s.NumberOfAttributes > 0)
            {
               sb.Append('(')
                 //.Append(Parameternummer + 1);
                 //.Append(':');
                 .Append(s.AttributetypeStrings[0].ToString())
                 .Append(' ')
                 .Append(NameStrings[NumberOfParameter++].ToString()
                 );

               for (Int32 i = 1; i < s.NumberOfAttributes; i++)
               {
                  sb.Append(", ")
                    //.Append(Parameternummer + 1);
                    //.Append(':');
                    .Append(s.AttributetypeStrings[i].ToString())
                    .Append(' ')
                    .Append(NameStrings[NumberOfParameter++].ToString()
                     );
               }

               sb.Append(')');
            }
            // not s.Append(sb): this might cause an infinite recursion
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

      internal Symbol(UnifiedString identifier, Int32 position, Int32 symbolNumber)
      {
         this.Identifier = identifier;
         this.FirstPosition = position;
         this.SymbolNumber = symbolNumber;
         this.AttributetypeStrings = Array.Empty<UnifiedString>();
         this.AttributenameStrings = Array.Empty<UnifiedString>();
      }

      internal Symbol(
            UnifiedString identifier,
            Int32 position,
            Int32 symbolNumber,
            UnifiedString[] attributetypeStringList)
      {
         this.Identifier = identifier;
         this.FirstPosition = position;
         this.SymbolNumber = symbolNumber;
         this.AttributetypeStrings = attributetypeStringList;
         this.AttributenameStrings = Array.Empty<UnifiedString>();
      }

      internal Symbol(
            UnifiedString identifier,
            Int32 position,
            Int32 symbolNumber,
            UnifiedString[] attributetypeStringIndexList,
            UnifiedString[] attributenameStringIndexList
         )
      {
         this.Identifier = identifier;
         this.FirstPosition = position;
         this.SymbolNumber = symbolNumber;
         this.AttributetypeStrings = attributetypeStringIndexList;
         this.AttributenameStrings = attributenameStringIndexList;
      }

      internal readonly UnifiedString Identifier;

      public readonly Int32 FirstPosition;

      public override String ToString() => Identifier.ToString();

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
      /// The terminal symbols are numbered starting with 0, the nonterminal symbols are also numbered starting with 0.
      /// The display in logs and comments is <see cref="SymbolNumber"/>+1.
      /// </summary>
      internal Int32 SymbolNumber;
      internal UnifiedString[] AttributetypeStrings;
      internal UnifiedString[] AttributenameStrings;

      internal Int32 NumberOfAttributes => AttributetypeStrings.Length;

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
           .Append(AttributetypeStrings[0].ToString())
           .Append(' ')
           .Append(AttributenameStrings[0].ToString());
         for (Int32 i = 2; i <= NumberOfAttributes; i++)
         {
            sb.Append(", ")
              .Append(AttributetypeStrings[i - 1].ToString())
              .Append(' ')
              .Append(AttributenameStrings[i - 1].ToString());
         }
         sb.Append(')');
         return sb;
      }

      internal virtual void Append(StringBuilder sb)
      {
         sb.Append(SymboltypeString)
           .Append(" nr. ")
           .Append(SymbolNumber + 1)
           .Append(": ");
         IdentifierAndAttributesToSB(sb);

         if (IsNullable)
            sb.Append(", generates the empty sequence");

         if (!isUsed)
            sb.Append(", is not used in any definition");
      }

   }

   internal sealed class TerminalSymbol : Symbol {
      internal TerminalSymbol(UnifiedString identifier, Int32 Position, Int32 symbolNumber, Int64 enumValue) 
         : base(identifier, Position, symbolNumber)
      {
         EnumValue = enumValue;
         _EmptyComputationResult = EmptyComputationResultEnum.NotEmpty;  // Terminal symbols are never empty
         FlagName = MakeFlagIdentifier(EnumValue, identifier.ToString());
      }


      internal Int64 EnumValue;
      internal Int64 Weight;
      internal Boolean IsUsedInIsIn = false;

      /// <summary>
      /// If the identifier contains special characters (other than letter or digit) the <see cref="FlagName"/>
      /// consists of the value of the terminal symbol followed by all letters and digits of the identifier ignoring
      /// all other characters. Else it is equal to the identifier.
      /// </summary>
      internal String FlagName { get; private set; }

      private String? _NameToGenerate;
      internal String NameToGenerate {
         // Lazy evaluation because GlobalSettings.TerminalSymbolEnum may not yet be
         // assigned at the time when the declaration of the terminal in grammlator source
         // is evaluated (an enum my follow)
         get {
            if (_NameToGenerate == null)
            {
               if (GlobalSettings.TerminalSymbolEnum.Value == GlobalSettings.TerminalSymbolUndefinedValue
                  || GlobalSettings.TerminalSymbolEnum.Value == "")
                  _NameToGenerate = Identifier.ToString();
               else
                  _NameToGenerate = GlobalSettings.TerminalSymbolEnum.Value + '.' + Identifier;
            }
            return _NameToGenerate;
         }
      }

      internal override String SymboltypeString => "terminal symbol";

      internal override EmptyComputationResultEnum ContainsAnEmptyDefinition()
          => EmptyComputationResultEnum.NotEmpty;

      internal override void Append(StringBuilder sb)
      {
         sb.AppendFormat
            ("{0} nr.{1,3}, value:{2,4}: ",
            SymboltypeString, SymbolNumber + 1, EnumValue);

         IdentifierAndAttributesToSB(sb);

         if (!isUsed)
            sb.Append(", is not used  in any definition");
      }

      static readonly StringBuilder FlagNameBuilder = new(50);
      /// <summary>
      /// Construct a unique identifier to be used for the flag representation of the terminal symbol
      /// </summary>
      /// <param name="value"></param>
      /// <param name="identifier"></param>
      /// <returns></returns>
      private static String MakeFlagIdentifier(Int64 value, String identifier)
      {
         FlagNameBuilder.Clear(); // .Append(GlobalSettings.PrefixOfFlagConstants);
         foreach (char c in identifier)
            if (char.IsLetter(c) || char.IsDigit(c))
               FlagNameBuilder.Append(c);

         if (FlagNameBuilder.Length == identifier.Length)
            return identifier; // no characters removed: return identifier

         // special characters have been removed, make it unique
         FlagNameBuilder.Insert(0, value); // use unique number, keep the shortened identifier to make it readable
         return FlagNameBuilder.ToString();
      }
   }

   internal sealed class NonterminalSymbol : Symbol {

      internal NonterminalSymbol(
            UnifiedString identifier,
            Int32 position,
            Int32 symbolNumber,
            UnifiedString[] attributetypeStringList
         )
         : base(identifier, position, symbolNumber, attributetypeStringList)
      {
         {
            NontrivialDefinitionsList = EmptyDefinitionsList;
            TrivalDefinitionsArray = Array.Empty<Symbol>();
         }
      }

      internal NonterminalSymbol(
            UnifiedString identifier,
            Int32 position,
            Int32 symbolNumber,
            UnifiedString[] attributetypeStringList,
            UnifiedString[] attributenameStringList
         )
         : base(identifier, position, symbolNumber, attributetypeStringList, attributenameStringList)
      {
         NontrivialDefinitionsList = EmptyDefinitionsList;
         TrivalDefinitionsArray = Array.Empty<Symbol>();
      }

      internal NonterminalSymbol(
      UnifiedString identifier,
      Int32 position,
      Int32 symbolNumber,
      UnifiedString[] attributetypeStringList,
      UnifiedString[] attributenameStringList,
      ListOfDefinitions nontrivalDefinitionsList,
      Symbol[] trivalDefinitionsArray
   )
   : base(identifier, position, symbolNumber, attributetypeStringList, attributenameStringList)
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
      private static readonly ListOfDefinitions EmptyDefinitionsList = new(0);
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

      internal override void Append(StringBuilder sb)
      {
         base.Append(sb);
         sb.AppendLine();
         if (TrivalDefinitionsArray.Length > 0)
         {
            sb.Append("    0. trivial rule(s): ");
            TrivalDefinitionsArray.Append(sb, separator: " | ");
            sb.AppendLine();
         }
         if (NontrivialDefinitionsList.Count == 0)
         {
            return;
         }
         // sb.AppendLine("    rules:");
         NontrivialDefinitionsList.AppendToSB(sb);
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

   /// <summary>
   /// <see cref="UnifiedString"/> is a readonly record type which stores strings in a static directory
   /// and identifies each string by a unique index. To get the string use ToString().
   /// The comparision of unified strings by "==" is the comparision of the integer value.
   /// </summary>
   internal readonly struct UnifiedString {
      /// <summary>
      /// All <see cref="UnifiedString"/>s which represent the same string have the same <see cref="Index"/>.
      /// The <see cref="Index"/> of "" is 0;
      /// </summary>
      internal readonly Int32 Index;

      private static readonly List<String> UnifiedStrings = new(1000) { "" };
      private static readonly Dictionary<ReadOnlyMemory<Char>, Int32> MemoryToIndexDictionary
         = new(1000, new MemoryComparer()) { { "".AsMemory(), 0 } };
      internal static void Reset()
      {
         MemoryToIndexDictionary.Clear();
         UnifiedStrings.Clear();

         // The default index 0 always is the index of the empty string ""
         MemoryToIndexDictionary["".AsMemory()] = 0;
         UnifiedStrings.Add("");
      }

      internal UnifiedString(ReadOnlyMemory<char> memorySpan)
      {
         if (MemoryToIndexDictionary.TryGetValue(memorySpan, out Int32 I))
         {
            Index = I;
            return;
         }

         String unifiedString = memorySpan.ToString();
         Index = UnifiedStrings.Count;
         MemoryToIndexDictionary.Add(unifiedString.AsMemory(), Index);
         UnifiedStrings.Add(unifiedString);
      }

      internal UnifiedString(Int32 index)
      {
         Index = index;
      }

      internal UnifiedString(String s) : this(s.AsMemory()) { }

      /// <summary>
      /// Returns the unified string stored in the directory. Does not allocate additional memory.
      /// </summary>
      /// <returns></returns>
      public override String ToString() => UnifiedStrings[Index];

      public int Length => UnifiedStrings[Index].Length;

      public Boolean IsEmpty { get { return Index == 0; } }

      public override bool Equals(object? obj)
         => obj != null && (obj is UnifiedString objAsUnified) && (objAsUnified.Index == Index);

      public override int GetHashCode() => Index.GetHashCode();

      public static bool operator ==(UnifiedString x, UnifiedString y)
      {
         return x.Index == y.Index;
      }

      public static bool operator !=(UnifiedString x, UnifiedString y)
      {
         return x.Index != y.Index;
      }

      public static implicit operator string(UnifiedString u) => UnifiedStrings[u.Index];
      public static implicit operator ReadOnlySpan<char>(UnifiedString u) => UnifiedStrings[u.Index].AsSpan();
      public static explicit operator UnifiedString(string s) => new(s);

   }
}


