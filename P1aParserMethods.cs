using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Collections;

namespace grammlator {
   internal partial class P1aParser {
      /* In phase 1 the grammar is read and all information is stored
       * in SymbolDictionary,  g.NumberOfTerminalSymbols,  g.NumberOfNonterminalSymbols ...
       * and g.Startsymbol which references terminal and nonterminal symbols ...
       */

      // Syntax is alike ebnf https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form
      // but compatibility to C# has higher priority 

      // TODO Check compatibility with ANTLR https://github.com/antlr/antlr4/blob/master/doc/index.md
      // TODO Analyze the effect of OptimizeTrivialDefinitions
      // TODO Implement terminal symbols from different sources, Accept
      // TODO Additional Analysis of conflicts
      // TODO Phase4 & CodeGen: use variables instead of stack where appopriate
      // TODO Analyse grammar and give recommendations for order of terminals symbols
      // TODO Analyse the grammar and give recommendations to split the grammar

      /// <summary>
      /// Is set to false while processing the definitions of the startsymbol,
      /// may be initialized to false to suppress this optimization.
      /// </summary>
      private Boolean OptimizeTrivialDefinitions = true;

      private static readonly ListOfDefinitions EmptyListOfNontrivialDefinitions = new ListOfDefinitions(0);

      public class ListOfSymbols : List<Symbol> {
         public ListOfSymbols(Int32 capacity) : base(capacity) { }

         public void RemoveFromEnd(Int32 n) => this.RemoveRange(this.Count - n, n);
      }

      /// <summary>
      /// Stores the StringIndexes of all elements of the last C# enum.
      /// Is the empty list, if an enum with no elements has been recognized. 
      /// </summary>
      private readonly List<Int32> EnumNames = new List<Int32>();

      /// <summary>
      /// <see cref="EnumValues"/>[i] is the value of <see cref="EnumNames"/>[i]
      /// </summary>
      private readonly List<Int64> EnumValues = new List<Int64>();

      /// <summary>
      /// The StringIndex of the name of the last enum the parser found in the source.
      /// <see cref="EnumName"/>.Index is 0 if an optional enum has been empty.
      /// </summary>
      public UnifiedString EnumName = new UnifiedString();

      // TODO test all error messages
      private void CreateParserErrorMessage(String message)
          => P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error, message);

      /*****************************************************************************
       *******                                                             *********
       *******            METHODS CALLED BY PARSER                         *********
       *******                                                             *********
       *****************************************************************************/

      /// <summary>
      /// The parser recognized the definition of a terminal symbol.
      /// Create and store a TerminalSymbol instance in the <see cref="SymbolDictionary"/>.
      /// Assign the attributes (from the end of <see cref="ListOfAttributesOfGrammarRule"/>) and remove them from the list.
      /// </summary>
      /// <param name="Name">Name of the terminal symbol</param>
      /// <param name="numberOfAttributes">Number of attributes</param>
      /// <param name="weight">the terminal symbols weight, which influences the order of generated if clauses</param>
      private void TerminalSymbolDeclaration(UnifiedString Name, Int32 numberOfAttributes, Int64 weight)
      {
         //UnifiedString Name = GlobalVariables.GetStringOfIndex(Name);  // TODO use nameIndex as Key in SymbolDictionary
         Debug.Assert(!String.IsNullOrEmpty(Name.ToString()), $"{nameof(Name)} is 0 or empty");

         if (SymbolDictionary.ContainsKey(Name))
         {
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                $"The terminal symbol {Name} has already been defined");
            // continue to keep integrity of the data structures
         }
         else
         {
            SymbolDictionary[Name] =
               new TerminalSymbol(Name.ToString(), Lexer.LexerTextPos) {
                  EnumValue = SymbolDictionary.Count,
                  Weight = weight,
                  SymbolNumber = SymbolDictionary.Count,
                  AttributetypeStrings = ListOfAttributesOfGrammarRule.GetAttributeTypeStringIndexes(numberOfAttributes),
                  AttributenameStrings = ListOfAttributesOfGrammarRule.GetAttributeIdentifierStringIndexes(numberOfAttributes)
               };
         }

         // Remove the attributes
         ListOfAttributesOfGrammarRule.RemoveFromEnd(numberOfAttributes);

         // and reset the attribute counter for the next terminal symbols declaration
         AttributeCounter = 0;

         return;
      }

      private void EvaluateEnumElement(String description, UnifiedString enumElementName, Int64 enumElementValue)
      {
         ReadOnlySpan<char> Description = description.AsSpan();
         List<String> ArgumentTypes = new List<string>(20);
         List<String> ArgumentNames = new List<string>(20);
         if (Description != "")
         {
            if (!EvalDescription(enumElementName, Description, ArgumentTypes, ArgumentNames))
               P1OutputMessageAndLexerPosition(
                  MessageTypeOrDestinationEnum.Warning,
                  @$"Error in [Description(""..."") of enum element {enumElementName}");

         }

         if (!SymbolDictionary.TryGetValue(enumElementName, out Symbol _))
         {
            // There is no terminal with the same name as the enum element
            // Declare a corresponding terminal
            Int32 NumberOfAttributes = 0;
            SymbolDictionary[enumElementName] =
               new TerminalSymbol(enumElementName.ToString(), Lexer.LexerTextPos) {
                  EnumValue = enumElementValue,
                  Weight = GlobalSettings.TerminalDefaultWeight.Value,
                  SymbolNumber = SymbolDictionary.Count,
                  AttributetypeStrings = ListOfAttributesOfGrammarRule.GetAttributeTypeStringIndexes(NumberOfAttributes),
                  AttributenameStrings = ListOfAttributesOfGrammarRule.GetAttributeIdentifierStringIndexes(NumberOfAttributes)
               };
         }

         EnumNames.Add(enumElementName.Index);
         EnumValues.Add(enumElementValue);
      }

      
      private void SortTerminalsByEnumValue()
      {
         var Keys = new UnifiedString[SymbolDictionary.Count];
         var Values = new Symbol[SymbolDictionary.Count];
         SymbolDictionary.Keys.CopyTo(Keys, 0);
         SymbolDictionary.Values.CopyTo(Values, 0);
         Array.Sort<Symbol, UnifiedString>(Values, Keys, new EnumComparer());
         SymbolDictionary.Clear();
         for (int i = 0; i < Values.Length; i++)
         {
            SymbolDictionary.Add(Keys[i], Values[i]);
            (Values[i] as TerminalSymbol)!.SymbolNumber = i;
         }
      }

      private void ErrorIfMissingEnumElements(out Boolean error, out Boolean ascending)
      {
         error = false;
         ascending = true;
         if (EnumName.Index == 0)
            return; // no enum

         Int64 LastEnumValue = Int64.MinValue;
         foreach (var terminal in SymbolDictionary)
         {
            var TerminalStringIndex = terminal.Key.Index;
            Int32 IndexOfEnumElement = EnumNames.FindIndex
               (enumStringIndex => enumStringIndex == TerminalStringIndex);
            if (IndexOfEnumElement < 0)
            {
               error = true;
               String name = new UnifiedString(TerminalStringIndex).ToString();
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                  @$"The name ""{name}"" in the terminal definition does not occur in the enum.");
            }
            else
            {
               Int64 EnumValue = EnumValues[IndexOfEnumElement];
               (terminal.Value as TerminalSymbol)!.EnumValue = EnumValue;
               if (EnumValue < LastEnumValue)
                  ascending = false; // have to resort the terminal symbols (if no error)
               LastEnumValue = EnumValue;
            }
         }
      }

      /// <summary>
      /// Add the nonterminal symbol to the <see cref="SymbolDictionary"/> or, if already contained test for compatibility, 
      /// update (LeftSide=true, OverlayType=... out) and assign the attributes, set <see cref="NumberOfLastAttributeOfLeftSide"/>
      /// and reset <see cref="AttributeCounter"/> to <see cref="AttributeNumberAtStartOfDefinition"/>
      /// </summary>
      /// <param name="symbolName"></param>
      /// <param name="numberOfAttributes"></param>
      /// <returns>The <see cref="NonterminalSymbol"/></returns>
      private NonterminalSymbol NonterminalSymbolDefinition(UnifiedString symbolName, Int32 numberOfAttributes)
      {
         // The parser recognized the left side of a rule and has not yet evaluated any definition of this symbol

         Debug.Assert(symbolName.Index > 0, "Identifier IsNullOrEmpty");

         NonterminalSymbol ns;

         if (SymbolDictionary.TryGetValue(symbolName, out Symbol? Symbol))
         {
            // The nonterminal symbol has been used already als element of a definition. The types of its attributes are known.
            if (Symbol is NonterminalSymbol symbolAsNonterminal)
            {
               ns = symbolAsNonterminal;
               if (ns.IsDefined)
               {
                  P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                      $"{symbolName} has already been defined as nonterminal symbol. ");
                  // TODO test behaviour after error
               }

               else if (ns.NumberOfAttributes != numberOfAttributes)
               {
                  P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                      $"{symbolName} has already been used with a different number of attributes. ");
                  // TODO test behaviour after error
               }

               else if (!AttributeTypesCoincide(ns))
               {
                  P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                     $"{symbolName} has already been used with at least one attribute with a different type. ");
                  // TODO test behaviour after error
               }
               // The AttributtypeList has been assigned at first usage and has just been checked
            }
            else
            {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                   $"{symbolName} has been already used as terminal symbol. ");
               Symbol = null; // will cause the terminal symbol to be replaced in SymbolDictionary
                              // TODO test behaviour after error
            }
         }

         if (Symbol != null)
         {
            ns = (NonterminalSymbol)Symbol;
            // The names of the attributes are copied now from the ListOfAttributesOfSyntaxRule.
            // They are not removed from ListOfAttributesOfSyntaxRule, because it may be a definition inside another definition.
            // They will be removed when the end of a syntax rule in the syntax rule list or the end of the enclosing definition is recognized.
            Debug.Assert(ns!.AttributenameStrings == null || ns.AttributenameStrings.Length == 0);
            ns.AttributenameStrings = ListOfAttributesOfGrammarRule.GetAttributeIdentifierStringIndexes(numberOfAttributes);
         }
         else // Symbol == null
         {

            // The nonterminal symbol has not yet been used (Symbol == null)
            // or there has been an error and we proceed as if it has been a new nonterminal symbol
            // TOCHECK proceed with a modified  symbolname ???
            ns = new NonterminalSymbol(symbolName.ToString(),
               Lexer.LexerTextPos,
               symbolNumber: SymbolDictionary.Count - GlobalVariables.NumberOfTerminalSymbols,
               attributetypeStringList: ListOfAttributesOfGrammarRule.GetAttributeTypeStringIndexes(numberOfAttributes),
               attributenameStringList: ListOfAttributesOfGrammarRule.GetAttributeIdentifierStringIndexes(numberOfAttributes)
               );
            SymbolDictionary[symbolName] = ns;
         }

         // Mark the attributes of the left side in the ListOfAttributesOfProduction as LeftSide attributes
         for (Int32 i = 1; i <= numberOfAttributes; i++)
         {
            AttributeStruct a = ListOfAttributesOfGrammarRule[^i];
            a.LeftSide = true;
            a.OverlayType = AttributeStruct.OverlayEnum.outAttribute;
            ListOfAttributesOfGrammarRule[^i] = a;
         }

         NumberOfLastAttributeOfLeftSide = AttributeCounter;
         AttributeCounter = AttributeNumberAtStartOfDefinition;

         return ns;
      }

      /// <summary>
      /// If the name is unknown a <see cref="Symbol"/> instance will be created and stored in the dictionary,
      /// else consistent usage of the attributes will be checked. Returns the <see cref="Symbol"/> instance.
      /// </summary>
      /// <param name="Name">the name of the nonterminal symbol</param>
      /// <param name="NumberOfAttributes">the number of attributes of the nonterminal symbol</param>
      /// <returns>The found or created <see cref="Symbol"/> instance</returns>
      private Symbol EvaluateSymbolnameFoundInRightSide(UnifiedString name, Int32 NumberOfAttributes)
      {
         if (SymbolDictionary.TryGetValue(name, out Symbol? symbol) /*Symbol == null*/)
         {
            if (symbol.NumberOfAttributes != NumberOfAttributes)
            {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                  $"The symbol {name} has been used at its first occurence with a different number of attributes. "
                  );
            }
            else if (!AttributeTypesCoincide(symbol))
            {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                  $"The symbol {name} has been used at its first occurence with at least one attribute of a different type. "
                  );
            }
         }
         else
         {
            // New symbol: create instance
            symbol = new NonterminalSymbol(name.ToString(),
                  Lexer.LexerTextPos,
                  symbolNumber: SymbolDictionary.Count - GlobalVariables.NumberOfTerminalSymbols,
                  attributetypeStringList:
                     NumberOfAttributes == 0
                     ? emptyListOfStringIndexes
                     : ListOfAttributesOfGrammarRule.GetAttributeTypeStringIndexes(NumberOfAttributes)
                  );

            SymbolDictionary[name] = symbol;
            // Assign AttributetypeList, let AttributenameList undefined until the symbol becomes defined in left side
         }
         return symbol;
      }

      //{-*****************************************************************************
      //--*******               Evaluate Grammar Rule                        **********
      //--*****************************************************************************

      /// <summary>
      /// The parser recognized a nested element.
      /// The nonterminal symbol, which has been defined in parentheses now is used at
      /// one level lower as symbol in the right side.
      /// For all of its attributes: decrement level and set LeftSide to false
      /// </summary>
      /// <param name="symbol"></param>
      private void UseNonterminalInRightSide(Symbol symbol)
      {
         for (Int32 i = 1; i <= symbol.NumberOfAttributes; i++)
         {
            // decrement Level and set LeftSide=false
            AttributeStruct Attribut = ListOfAttributesOfGrammarRule[^i];
            Attribut.Level--;
            Attribut.LeftSide = false;
            Attribut.OverlayType = AttributeStruct.OverlayEnum.inAttribute;
            ListOfAttributesOfGrammarRule[^i] = Attribut;
         }
      }

      /// <summary>
      /// The parser recognized the end of a (maybe nested) grammar rule.
      /// The definitions and trival definitions are assigned to the symbol and are removed from
      /// ActualListOfDefinitions and ActualListOfTrivialDefinitions.
      /// The attributes of the symbol remain in ListOfAttributesOfSyntaxRule.
      /// </summary>
      /// <param name="Symbol"></param>
      private void EvaluateGrammarRule(Symbol Symbol)
      {
         // The symbol must have been defined (in the left side or by MakeGrammarRule)
         if (!(Symbol is NonterminalSymbol nt))
         {
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Abort,
                 $"Error in program {nameof(EvaluateGrammarRule)}: Symbol as NonterminalSymbol is null."
                 );
            return;
         }

         if (nt.IsDefined)
         {
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                   $"The identifier \"{Symbol.Identifier}\" is already defined." // TODO add position
                   );
         }

         if (NumberOfTrivalDefinitions != 0)
         {
            nt.TrivalDefinitionsArray = new Symbol[NumberOfTrivalDefinitions];
            for (Int32 i = 0; i < NumberOfTrivalDefinitions; i++)
            {
               nt.TrivalDefinitionsArray[i] =
                   ActualListOfTrivialDefinitions[ActualListOfTrivialDefinitions.Count - NumberOfTrivalDefinitions + i];
            }
            ActualListOfTrivialDefinitions.RemoveFromEnd(NumberOfTrivalDefinitions);
            NumberOfTrivalDefinitions = 0;
         }

         if (NumberOfNontrivialDefinitions != 0)
         {
            // Copy and remove NumberOfNontrivialDefinitions from the ActualListOfNontrivialDefinitions
            // to a new list nt.NontrivalDefinitionsList
            nt.NontrivialDefinitionsList = new ListOfDefinitions(NumberOfNontrivialDefinitions, ActualListOfNontrivialDefinitions);
            ActualListOfNontrivialDefinitions.RemoveFromEnd(NumberOfNontrivialDefinitions);
            NumberOfNontrivialDefinitions = 0;
            // Assign nt to .DefinedSymbol (has been null) of each copied definition 
            foreach (Definition thisDefinition in nt.NontrivialDefinitionsList)
               thisDefinition.DefinedSymbol = nt;
         }
      }

      //{-***********************************************************************************
      //--*******  D e f i n i t i o n s   o f   t h e  S t a r t s y m b o l      **********
      //--***********************************************************************************}

      /// <summary>
      /// Assigns an adapted copy of <see cref="ActualListOfNontrivialDefinitions"/> to <see cref="GlobalVariables.Startsymbol"/>,
      /// then clears <see cref="ActualListOfNontrivialDefinitions"/>
      /// </summary>
      private void EvaluateDefinitionsOftheStartsymbol()
      {
         GlobalVariables.Startsymbol.NontrivialDefinitionsList
             = new ListOfDefinitions(
                 NumberOfNontrivialDefinitions,
                 ActualListOfNontrivialDefinitions);

         // Assign 
         foreach (Definition definition in ActualListOfNontrivialDefinitions)
            definition.DefinedSymbol = GlobalVariables.Startsymbol;

         Debug.Assert(ActualListOfTrivialDefinitions.Count == 0, "Error in the program: the startsymbol has trivial definitions");
         Debug.Assert(ActualListOfNontrivialDefinitions.Count - NumberOfNontrivialDefinitions == 0);

         ActualListOfNontrivialDefinitions.Clear();
         NumberOfNontrivialDefinitions = 0;
      }

      //{-*****************************************************************************
      //--*******           M A K E   N E W   N A M E                         *********
      //--*****************************************************************************}
      private Int32 CountOfGeneratedNames = 0;

      ///<summary>
      /// Generates a unique synthetic name enclosed in (), which starts with the given prefix followed by a number
      ///</summary>
      /// <param name="prefix"></param>
      ///<returns>new name</returns>
      private UnifiedString MakeNewNameStringIndex(String prefix)
         => new UnifiedString($"({prefix}{++CountOfGeneratedNames})");

      ///<summary>
      /// Generates an internal name by adding a postfix to a symbols name
      ///</summary>
      /// <param name="type">the type of the grammar rule determines the postfix</param>
      /// <param name="nameOfSymbol">name of the symbol to which the postfix has to be appended</param>
      ///<returns>new name</returns>
      private static UnifiedString MakeNewName(TypeOfGrammarRule type, String nameOfSymbol)
      {
         String Postfix = type switch
         {
            TypeOfGrammarRule.optional => "?",
            TypeOfGrammarRule.repeat0lr => "*",
            TypeOfGrammarRule.repeat0rr => "**",
            TypeOfGrammarRule.repeat1lr => "+",
            TypeOfGrammarRule.repeat1rr => "++",
            _ => "-??",
         };

         return new UnifiedString(nameOfSymbol + Postfix);
      }

      //{-*****************************************************************************
      //--*******                  Make Grammar Rule                          *********
      //--*****************************************************************************}

      private enum TypeOfGrammarRule { optional, repeat0lr, repeat1lr, repeat0rr, repeat1rr };

      private Symbol MakeGrammarRule(Symbol existingSymbol, TypeOfGrammarRule type)
      {
         /* Repeat is implemented by left recursion "lr" or right recursion "rr"
          *   type "optional":  // "?"
          *      newSymbol =  existingSymbol || ;
          *   type "repeat0lr": // "*"
          *      newSymbol =  || newSymbol, existingSymbol;
          *   type "repeat1lr": // "+"
          *      newSymbol =  existingSymbol || newSymbol, existingSymbol;
          *   type "repeat0rr": // "**"
          *      newSymbol =  || existingSymbol, newSymbol;
          *   type "repeat1rr": // "++"
          *      newSymbol =  existingSymbol || existingSymbol, newSymbol;
          */

         // Create (synthetic) name of new nonterminal symbol by adding postfix to existing name
         UnifiedString NewName = MakeNewName(type, existingSymbol.Identifier);

         // use existing (synthetic) symbol / definition if name already has been defined
         if (!SymbolDictionary.TryGetValue(NewName, out Symbol? MadeSymbol))
         {
            // else create, store in dictionary and define new symbol
            MadeSymbol = DefineNewSymbol(existingSymbol, type, NewName);
         }
         ListOfAttributesOfGrammarRule.RemoveFromEnd(existingSymbol.NumberOfAttributes);
         AttributeCounter -= existingSymbol.NumberOfAttributes;

         return MadeSymbol;
      }

      private NonterminalSymbol DefineNewSymbol(
            Symbol existingSymbol,
            TypeOfGrammarRule type,
            UnifiedString newNname)
      {
         var newSymbol = new NonterminalSymbol(newNname.ToString(),
            Lexer.LexerTextPos,
            symbolNumber: SymbolDictionary.Count - GlobalVariables.NumberOfTerminalSymbols,
            attributetypeStringList: ListOfAttributesOfGrammarRule.GetAttributeTypeStringIndexes(0), // has no attributes
            attributenameStringList: ListOfAttributesOfGrammarRule.GetAttributeIdentifierStringIndexes(0)
            );

         SymbolDictionary[newNname] = newSymbol;
         ListOfDefinitions nontrivialDefinitions;
         Symbol[] trivialDefinitions = Array.Empty<Symbol>(); // preset: no trival definition

         switch (type)
         {
         case TypeOfGrammarRule.optional:
         {
            if (existingSymbol.NumberOfAttributes == 0)
            {
               // make "newSymbol =  || existingSymbol();"
               nontrivialDefinitions      // 1 definition
                   = new ListOfDefinitions(1)
                   {
                                new Definition ( // 1st definition: empty definition
                                    idNumber: 0,
                                    definedSymbol: newSymbol,
                                    elements: Array.Empty<Symbol>(),
                                    attributestackAdjustment: 0
                                    )
                    };
               trivialDefinitions
                   = new Symbol[1] { existingSymbol }; // 1 trivial definition: existingSymbol;
            }
            else
            {
               //  make "newSymbol =  || existingSymbol(xxx);"
               nontrivialDefinitions
                   = new ListOfDefinitions(2) // 2 definitions
               {
                                new Definition ( // 1st definition: empty definition
                                    idNumber: 0,
                                    definedSymbol: newSymbol,
                                    elements: Array.Empty<Symbol>(),
                                    attributestackAdjustment: 0
                                ),

                                new Definition ( // 2nd definition: existingSymbol(xxx)
                                    idNumber : 1,
                                    definedSymbol : newSymbol,
                                    elements : new Symbol[1] { existingSymbol },
                                    attributestackAdjustment : -existingSymbol.NumberOfAttributes
                                )
                      // no trivial definition (as has been preset)
                   };
            }
            break;
         }

         case TypeOfGrammarRule.repeat0lr:
         {
            // make "newSymbol =  || newSymbol, existingSymbol(xxx);"
            nontrivialDefinitions
                = new ListOfDefinitions(2) // 2 definitions
                {
                            new Definition ( // 1st definition: empty definition
                            idNumber: 0,
                            definedSymbol: newSymbol,
                            elements: Array.Empty<Symbol>(),
                            attributestackAdjustment:0
                            ),

                            new Definition ( // 2nd definition: newSymbol, existingSymbol(xxx)
                                idNumber: 1,
                                definedSymbol: newSymbol,
                                elements: new Symbol[2] { newSymbol, existingSymbol },
                                attributestackAdjustment: -existingSymbol.NumberOfAttributes
                            )
                };
            // no trivial definition (as has been preset)
            break;
         }

         case TypeOfGrammarRule.repeat1lr:
         {
            if (existingSymbol.NumberOfAttributes == 0)
            {
               // make "newSymbol = existingSymbol() || newSymbol, existingSymbol() ||;"
               trivialDefinitions
                   = new Symbol[1] { existingSymbol }; // 1 trivial definition: existingSymbol() 

               nontrivialDefinitions
                   = new ListOfDefinitions(1) // 1 definition
                   {
                                new Definition ( // 1st definition: newSymbol, existingSymbol()
                                    idNumber: 0,
                                    definedSymbol: newSymbol,
                                    elements: new Symbol[2] { newSymbol, existingSymbol },
                                    attributestackAdjustment: 0
                                )
                   };
            }
            else
            {
               // make "newSymbol = existingSymbol(xxx) || newSymbol, existingSymbol(xxx);"
               nontrivialDefinitions
                   = new ListOfDefinitions(1) // 2 definitions 
                   {
                                new Definition (
                                    idNumber : 0,  // 1st definition: existingSymbol(xxx)
                                    definedSymbol : newSymbol,
                                    elements : new Symbol[1]{ existingSymbol },
                                    attributestackAdjustment : -existingSymbol.NumberOfAttributes
                                ),

                                new Definition (
                                    idNumber : 1,  // 2nd definition: newSymbol, existingSymbol(xxx);
                                    definedSymbol : newSymbol,
                                    elements : new Symbol[2] { newSymbol, existingSymbol },
                                    attributestackAdjustment : -existingSymbol.NumberOfAttributes
                                )
                   };

               // no trivial definition (as has been preset)
            }
            break;
         }

         case TypeOfGrammarRule.repeat0rr:
         {
            // make "newSymbol =  || existingSymbol, newSymbol;"
            nontrivialDefinitions
                = new ListOfDefinitions(2) // 2 definitions
                {
                            new Definition ( // 1st definition: empty
                                idNumber: 0,
                                definedSymbol: newSymbol,
                                elements: Array.Empty<Symbol>(),
                                attributestackAdjustment: 0
                            ),
                            new Definition ( // 2nd definition: existingSymbol, newSymbol;
                                idNumber: 1,
                                definedSymbol: newSymbol,
                                elements: new Symbol[2] { existingSymbol, newSymbol },
                                attributestackAdjustment: -existingSymbol.NumberOfAttributes
                            )
                };
            // no trivial definition (as has been preset)
            break;
         }

         default: // case TypeOfGrammarRule.repeat1rr:
         {
            // make "newSymbol = existingSymbol() || existingSymbol(), newSymbol;"
            if (existingSymbol.NumberOfAttributes == 0)
            {
               trivialDefinitions
                   = new Symbol[1] { existingSymbol }; // 1 trivial definition: existingSymbol()

               nontrivialDefinitions
                   = new ListOfDefinitions(1) // 1 definition
                   {
                             new Definition ( // 1st definition: existingSymbol(), newSymbol
                                idNumber: 0,
                                definedSymbol: newSymbol,
                                elements: new Symbol[2] { existingSymbol, newSymbol },
                                attributestackAdjustment: 0
                                )
                   };
            }
            else
            {
               // make "newSymbol = existingSymbol(xxx) || existingSymbol(xxx), newSymbol;"

               // no trivial definition (as has been preset)

               nontrivialDefinitions
                   = new ListOfDefinitions(2) // 2 definitions 
                   {
                                new Definition (
                                    idNumber: 0,  // 1st definition: existingSymbol(xxx)
                                    definedSymbol: newSymbol,
                                    elements: new Symbol[1]{ existingSymbol },
                                    attributestackAdjustment: -existingSymbol.NumberOfAttributes
                                ),

                                new Definition (
                                    idNumber: 1,  // 2nd definition: existingSymbol(xxx), newSymbol;
                                    definedSymbol: newSymbol,
                                    elements: new Symbol[2] { existingSymbol, newSymbol },
                                    attributestackAdjustment: -existingSymbol.NumberOfAttributes
                                )
                   };
            }
            break;
         }
         }

         newSymbol.NontrivialDefinitionsList = nontrivialDefinitions;
         newSymbol.TrivalDefinitionsArray = trivialDefinitions;
         return newSymbol;
      }

      ////--****************************************************************************
      ////--*******               handle attributes                            *********
      ////--****************************************************************************

      /// <summary>
      /// While parsing a (maybe nested) definition each attribute (left side, right side) is stored
      /// in the <see cref="ListOfAttributesOfGrammarRule"/>. 
      /// When the end of a definition is recognized the attributes of the definition are evaluated
      /// and removed, the attributes of the left side remain.
      /// </summary>
      private readonly ListOfAttributes ListOfAttributesOfGrammarRule = new ListOfAttributes();

      /// <summary>
      /// Check attribute and add attribute to TemporaryListOfAttributes;
      /// set its number to <see cref="AttributeCounter"/>
      /// </summary>
      /// <param name="NewType"></param>
      /// <param name="NewName"></param>
      /// <exception cref="ErrorInGrammlatorProgramException"></exception>
      private void PushAttributeToListOfAttributesOfGrammarRule(UnifiedString newTypeString, UnifiedString newNameString)
      {

         var newAttribute = new AttributeStruct(
           newTypeString, newNameString,
           // leftSide: false, // is considered false as default and may be reset when a left side is evaluated
           // Usage:  inAttribute, //is considered to be inAttribute as default 
           level: NestingLevel,
           positionInProduction: AttributeCounter
           );

         // Look for an overlayed attribute with the same name or the same position 
         //  at the same level in the list of attributes
         Int32 IndexOfOverlayedAttribute =
             ListOfAttributesOfGrammarRule.FindLastIndex(
                 (x) =>
                 x.NameString == newNameString
                 || x.PositionInProduction == AttributeCounter
                 );

         if (IndexOfOverlayedAttribute < 0)
         {
            // Did not find an attribute with same name or same position.
            // The new attribute may be a left side attribute (fields will be modified when left side is recognized)
            // or a not overlaying right side attribute (the default values are correct)
            ListOfAttributesOfGrammarRule.Add(newAttribute);
            return;
         }

         AttributeStruct overlayedAttribute = ListOfAttributesOfGrammarRule[IndexOfOverlayedAttribute];

         if (overlayedAttribute.Level != NestingLevel)
         {
            // Attribute is at a different level, it might be possible to handle it as new attribute.
            // The new attribute may be a left side attribute (fields will be modified when left side is recognized)
            // or a not overlaying right side attribute (the default values are correct)
            ListOfAttributesOfGrammarRule.Add(newAttribute);

            // TODO check the case when attributes of a left side symbol on a higher level overlay left side at lower level !!!

            //// This case may not be an error !!
            //P1OutputMessage(MessageTypeOrDestinationEnum.Error,
            //    $"The production already contains an attribute \"{NewName}\", but at different level."
            //    );
            return;
         }

         /* 
          * Found an attribute with the same position or same name (must have same position):
          *   because both have same position the new attribute will be a right side attribute
          *   and the overlayed attribute is a left side attribute
          *   
          * same name (must have same position and same type) 
          *   (and then has same level, overlayed attribute is left side, new attribute will be right side)
          *      the left side attribute is "defined by overlay"
          *   OverlayType of left side attribute is set to "inOut"
          *      of right side attribute is set to "inOut"
          *   generated code: peekRef, if associated by name to a formal parameter,
          *      no code if not associated by name to a formal parameter (or no method given)
          * ---
          * same position and different name and different types (!):
          *    OverlayType of left side attribute becomes "outClear"
          *       of right side attribute becomes "inClear"
          *    generated code for right side attribute:
          *       "peekClear"(!) (may be not used, else must be a formal value parameter)
          *    generated code for left side attribute 
          *    (must be associated to a formal out parameter)
          *       if right side attribute is not associated to a formal parameter
          *          then "peekRefClear"(!)
          *       else peekRef
          * ---
          * same position and different name and same types:
          *   OverlayType of left side attribute remains "out"
          *      of right side attribute remains "in"
          *    generated code for right side attribute:
          *       peekRef (may be not used, else must be a formal value parameter)
          *    generated code for left side attribute 
          *    (must be associated to a formal out parameter)
          *       peekRef
          * */

         if (overlayedAttribute.NameString == newNameString)
         {
            //same name: must have same position and same type, usage is inOut
            if (overlayedAttribute.PositionInProduction != AttributeCounter)
            { // the overlaying attributes with same name don't have the same (overlaying) position
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                   $"The defined nonterminal or the definition already contains an attribute \"{newNameString}\", but with different position (does not overlay)."
                   );
            }
            else if (overlayedAttribute.TypeString != newTypeString)
            {// the overlaying attributes with same name don't have the same type 
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                   $"The defined nonterminal or the definition already contains an overlaying attribute \"{newNameString}\", but with different type."
                   );
            }
            else
            { // overlaying with same name and type (may be different level if nested, even if same position)
              // Debug.Assert(overlayedAttribute.LeftSide);
            }

            newAttribute.OverlayType = AttributeStruct.OverlayEnum.inOutAttribute;
            overlayedAttribute.OverlayType = AttributeStruct.OverlayEnum.inOutAttribute;
         }
         else
         {
            // overlayedAttribute.Name != NewName and 
            // overlayedAttribute.PositionInProduction == ActualAttributeNumber
            if (overlayedAttribute.TypeString != newAttribute.TypeString)
            {
               newAttribute.OverlayType = AttributeStruct.OverlayEnum.inClearAttribute;
               overlayedAttribute.OverlayType = AttributeStruct.OverlayEnum.outClearAttribute;
            }
            else
            {
               newAttribute.OverlayType = AttributeStruct.OverlayEnum.inAttribute;
               overlayedAttribute.OverlayType = AttributeStruct.OverlayEnum.outAttribute;
            }
         }

         ListOfAttributesOfGrammarRule[IndexOfOverlayedAttribute] = overlayedAttribute;
         ListOfAttributesOfGrammarRule.Add(newAttribute);
      }

      private readonly UnifiedString[] emptyListOfStringIndexes = Array.Empty<UnifiedString>();

      /// <summary>
      /// returns true if the types of the attributes of symbol are equal to the types
      /// of the attribute at the end of  <see cref="ListOfAttributesOfGrammarRule"/>
      /// </summary>
      /// <param name="symbol">The symbol whos attributes are to be compared</param>
      /// <returns>true if attribute types coincide</returns>
      private Boolean AttributeTypesCoincide(Symbol symbol)
      {
         Int32 NumberOfAttributes = symbol.NumberOfAttributes;
         for (Int32 i = 0; i < NumberOfAttributes; i++)
         {
            if (symbol.AttributetypeStrings[i]
                != ListOfAttributesOfGrammarRule[ListOfAttributesOfGrammarRule.Count - NumberOfAttributes + i].TypeString)
            {
               return false;
            }
         }
         return true;
      }

      ////--****************************************************************************
      ////--*******          interpret C#-method as semantic action            *********
      ////--****************************************************************************

      // lokale Variablenvereinbarungen

      public List<MethodParameterStruct> LastFormalParameterList = new List<MethodParameterStruct>();

      // Methoden:       
      /// <summary>
      /// List of string containing all legal method properties of C#, e.g. "new", "public", ... 
      /// </summary>
      private static readonly List<String> CSharpMethodProperties = new List<String> {
                "", "new",
                "public", "protected", "internal", "private",
                "static", "sealed", "virtual"
            // not allowed "abstract", "extern", "async"
            };

      /// <summary>
      /// Checks the parameters and assigns a new method (only with its name) to method
      /// </summary>
      /// <param name="method"></param>
      /// <param name="methodModifier">"" or "new" or "public" or "protected" or ...</param>
      /// <param name="type">must bei "void" else NotImplementedException</param>
      /// <param name="name">Name of the method</param>
      private void MethodProperties(out MethodClass method, UnifiedString methodModifier, UnifiedString typeString, UnifiedString name)
      {
         if (!CSharpMethodProperties.Contains(methodModifier.ToString()))
            CreateParserErrorMessage($"unknown method modifier \"{methodModifier}\"");

         if (typeString.ToString() == "int" || typeString.ToString() == "Int32")
         {
            method = new IntMethodClass(methodName: name.ToString(), Lexer.LexerTextPos);
         }
         else if (typeString.ToString() == "void")
         {
            method = new VoidMethodClass(methodName: name.ToString(), Lexer.LexerTextPos);
         }
         else
         {
            CreateParserErrorMessage($"expected \"int\" or \"Int32\" or \"void\", found \"{typeString}\" (will be interpreted as \"void\").");
            method = new VoidMethodClass(methodName: name.ToString(), Lexer.LexerTextPos);
         }
      }

      /// <summary>
      /// Checks the parameters and assigns a new method (only with its name) to method
      /// </summary>
      /// <param name="method"></param>
      /// <param name="methodModifier1">"" or "new" or "public" or "protected" or ...</param>
      /// <param name="methodModifier2">"" or "new" or "public" or "protected" or ...</param>
      /// <param name="type">must bei "void" else NotImplementedException</param>
      /// <param name="name">Name of the method</param>
      private void MethodProperties(out MethodClass method, UnifiedString methodModifier1,
         UnifiedString methodModifier2, UnifiedString typeString, UnifiedString nameString)
      {
         // String methodModifier1 = GlobalVariables.GetStringOfIndex(methodModifier1StringIndex);

         if (!CSharpMethodProperties.Contains(methodModifier1.ToString()))
            CreateParserErrorMessage($"unknown method modifier \"{methodModifier1}\"");
         MethodProperties(out method, methodModifier2, typeString, nameString);
      }

      /// <summary>
      /// Parser recognized a formal parameter.
      /// Add the details of the formal parameter to <see cref="LastFormalParameterList"/>
      /// </summary>
      /// <param name="ParameterModifierOpt">A C# formal parameter modifier: "ref", "out" or ""</param>
      /// <param name="type">A C# or user defined type (e.g. "Int32" or "Object" or "MyType")</param>
      /// <param name="name">The name (identifier) of the formal parameter</param>
      private void FormalParameter(UnifiedString parameterModifierOpt, UnifiedString typeString, UnifiedString nameString)
      {
         ParameterImplementation callType;
         switch (parameterModifierOpt.ToString())
         { // 
         case "":
         case "in":
         {
            callType = ParameterImplementation.ValueOrInCall;
            // may be changed later to ValueClearCall
            break;
         }

         case "ref":
         {
            callType = ParameterImplementation.RefCall;
            break;
         }

         case "out":
         {
            callType = ParameterImplementation.OutCall;
            // may be changed later to OutClearCall
            break;
         }

         default:
         {
            CreateParserErrorMessage(
               $"Illegal or not implemented parameter modifier {parameterModifierOpt}");
            callType = ParameterImplementation.ValueOrInCall;
            break;
         }
         }

         LastFormalParameterList.Add(new MethodParameterStruct {
            Implementation = callType,
            NameString = nameString,
            TypeString = typeString
         }
         );
      }

      /* *************************************************************************************************
       *                    C h e c k    U s a g e    O f    S y m b o l s                               *
       * ************************************************************************************************* */

      public MessageTypeOrDestinationEnum CheckUsageOfSymbols(Action<MessageTypeOrDestinationEnum, String, Int32> outputMessage)
      {
         var maxMessageType = MessageTypeOrDestinationEnum.noMessageType;
         void OutputMessage(MessageTypeOrDestinationEnum type, String message, Int32 position)
         {
            outputMessage(type, message, position);
            if (maxMessageType < type)
               maxMessageType = type;
         }
         Int32 UsedSymbolsCounter = GlobalVariables.Startsymbol.MarkAndCountAllUsedSymbols();
         if (UsedSymbolsCounter != SymbolDictionary.Count)
         {
            OutputMessage(MessageTypeOrDestinationEnum.Information,
               $"Only {UsedSymbolsCounter} of {SymbolDictionary.Count} symbols are used.",
               Lexer.LexerTextPos);
         }
         else
         {
            OutputMessage(MessageTypeOrDestinationEnum.Information,
               $"All {SymbolDictionary.Count} symbols are used.",
               Lexer.LexerTextPos
               );
         }

         Boolean AllSymbolsAreUsed = true;
         Int32 NotUsedPosition = 0;
         foreach (KeyValuePair<UnifiedString, Symbol> KeyValue in SymbolDictionary)
         {
            Symbol Symbol = KeyValue.Value;

            String NameOfSymbol()
               => (KeyValue.Key).ToString();

            if (Symbol != null)
            {
               if (Symbol is NonterminalSymbol nt)
               {
                  if (nt.NontrivialDefinitionsList.Count == 0 && nt.TrivalDefinitionsArray.Length == 0) // undefeined if no trivial or nontrivial definitions
                  {
                     OutputMessage(MessageTypeOrDestinationEnum.Abort,
                        $"{NameOfSymbol()} is used as terminal or nonterminal symbol but not defined.",
                        nt.FirstPosition
                        );
                  }

                  if (!nt.isUsed)
                  {
                     OutputMessage(MessageTypeOrDestinationEnum.Warning,
                        $"The nonterminal symbol {NameOfSymbol()} is not used.",
                        nt.FirstPosition
                        );
                  }
               }

               if ((Symbol is TerminalSymbol t) && !t.isUsed)
               {
                  AllSymbolsAreUsed = false;
                  NotUsedPosition = t.FirstPosition;
                  //OutputMessage(MessageTypeOrDestinationEnum.Information,
                  //   $"The terminal symbol {GetNameOfSymbol()} is not used in any definition (may be used in look ahead)",
                  //   t.FirstPosition
                  //   );
               }
            }
         }
         if (!AllSymbolsAreUsed)
            OutputMessage(MessageTypeOrDestinationEnum.Information,
               $"Not all terminal symbols are used in a definition (see list of symbols)",
               NotUsedPosition
               );

         return maxMessageType;
      }

      /*****************************************************************************
       *******            evaluate definition                              *********
       *****************************************************************************/

      /// <summary>
      /// Store the definition in ActualListOfNontrivialDefinitions or in ActualListOfTrivialDefinitions (if OptimizeTrivialDefinitions).
      /// <see cref="EvaluateVoidMethodParameters(VoidMethodClass)"/>. Remove attributes from <see cref="ListOfAttributesOfGrammarRule"/>.
      /// </summary>
      /// <param name="constantPriority">0 or explicitly defined constant priority</param>
      /// <param name="priorityFunction">null or C# Int32 method</param>
      /// <param name="semanticMethod">null or C# void method</param>
      /// <param name="optimizeTrivialDefinitions">if false there will be no special handling of trivial definitions</param>
      private void EvaluateDefinition(Int64 constantPriority, IntMethodClass? priorityFunction, VoidMethodClass? semanticMethod, Boolean optimizeTrivialDefinitions)
      {
         // ActualAttributeNumber is the number of the last attribute of the right side
         Int32 AttributestackAdjustment = NumberOfLastAttributeOfLeftSide - AttributeCounter;
         /* The code for positive adjustments of the attribute stack will be generated preceding the semantic actions,
          * the code for negative adjustments following the semantic action, so that the semantic action may access all attributes. */

         Debug.Assert(semanticMethod == null || !String.IsNullOrEmpty(semanticMethod.MethodName));

         if ((NumberOfElements == 1) && (semanticMethod == null) && (priorityFunction == null) && (constantPriority == 0)
             && (AttributestackAdjustment == 0) && optimizeTrivialDefinitions)
         {
            // Get and remove trival definition from ActualListOfElements 
            Symbol TrivialElement = ActualListOfElements[^1];
            ActualListOfElements.RemoveAt(ActualListOfElements.Count - 1);
            NumberOfElements = 0;

            // Add trivial definition to ActualListOfTrivialDefinitions if not yet present
            if (ActualListOfTrivialDefinitions.IndexOf(TrivialElement, ActualListOfTrivialDefinitions.Count - NumberOfTrivalDefinitions) != -1)
            {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Warning,
                  $"The duplicate declaration of a trivial definition has been ignored: {TrivialElement.Identifier}."
                  );
            }
            else
            {
               ActualListOfTrivialDefinitions.Add(TrivialElement);
               NumberOfTrivalDefinitions++;
            }

            // This checks if there are assignments to all attributes of the defined symbol and set their fields
            CheckAndResetLeftSideAttributes(semanticMethod: null);

            // remove the attributes of the trivial definition from ListOfAttributesOfSyntaxRule
            ListOfAttributesOfGrammarRule.RemoveFromEnd(TrivialElement.NumberOfAttributes);
            AttributeCounter -= TrivialElement.NumberOfAttributes;
         }
         else
         {
            // Add new nontrivial definition to ActualListOfNontrivialDefinitions

            Symbol[] ElementArray;
            if (NumberOfElements <= 0)
            {
               ElementArray = Array.Empty<Symbol>();
            }
            else
            {
               ElementArray = new Symbol[NumberOfElements];
               ActualListOfElements.CopyTo(ActualListOfElements.Count - NumberOfElements, ElementArray, 0, NumberOfElements);
               ActualListOfElements.RemoveFromEnd(NumberOfElements);
               NumberOfElements = 0;
            }

            // Compute the NumberOfAttributesOfNewDefinition
            Int32 NumberOfAttributesOfNewDefinition = 0;
            foreach (Symbol s in ElementArray)
               NumberOfAttributesOfNewDefinition += s.NumberOfAttributes;

            var NewDefinition =
                new Definition(
                   idNumber: NumberOfNontrivialDefinitions++,
                   definedSymbol: null, // will be assigned when the end of the rule is recognized
                   elements: ElementArray,
                   attributestackAdjustment: AttributestackAdjustment
                   ) {
                   ConstantPriority = constantPriority,
                   PriorityFunction = priorityFunction,
                   SemanticMethod = semanticMethod,
                   AttributeIdentifiers
                   = ListOfAttributesOfGrammarRule.GetAttributeIdentifierStringIndexes(NumberOfAttributesOfNewDefinition)
                };

            ActualListOfNontrivialDefinitions.Add(NewDefinition);

            EvaluateIntMethodParameters(NewDefinition);
            EvaluateVoidMethodParameters(NewDefinition);

            // Remove the attributes of the new definition from ListOfAttributesOfSyntaxRule
            ListOfAttributesOfGrammarRule.RemoveFromEnd(NumberOfAttributesOfNewDefinition);
            AttributeCounter -= NumberOfAttributesOfNewDefinition;
         }

         NumberOfElements = 0;
      }

      /// <summary>
      /// Generate a definition for all terminal symbols without the terminal symbols in <paramref name="excludedTerminalSymbols"/>
      /// </summary>
      /// <param name="excludedTerminalSymbols"></param>
      private void EvaluateExcludedTerminalSymbols(BitArray excludedTerminalSymbols)
      {
         for (Int32 IndexOfTerminalSymbol = 0; IndexOfTerminalSymbol < excludedTerminalSymbols.Length; IndexOfTerminalSymbol++)
         {
            if (excludedTerminalSymbols[IndexOfTerminalSymbol])
               continue;
            Symbol s = GlobalVariables.GetTerminalSymbolByIndex(IndexOfTerminalSymbol);
            for (Int32 IndexOfAttribute = 0; IndexOfAttribute < s.AttributenameStrings.Length; IndexOfAttribute++)
            {
               AttributeCounter++;
               PushAttributeToListOfAttributesOfGrammarRule(
                  s.AttributetypeStrings[IndexOfAttribute],
                  s.AttributenameStrings[IndexOfAttribute]
                  );
            }
            Debug.Assert(NumberOfElements == 0);
            ElementVariantRecognized(s);
            EndOfDefinitionWithPriorityAndMethodRecognized(constPriority: 0, dynPriority: null, method: null);
         }
      }

      /*********************************************************************************
       *******  This Checks the  Method Parameters And Computes Stack Offsets  *********
       *********************************************************************************/

      /// <summary>
      ///  an empty list of method parameters
      /// </summary>
      private readonly MethodParameterStruct[] NoMethodParameters = Array.Empty<MethodParameterStruct>();

      private static String AttributeParameterMatchError(AttributeStruct Attribute, MethodParameterStruct Parameter)
      {
         // Parameter.NameStringIndex == Attribute.NameStringIndex 
         switch (Parameter.Implementation)
         {
         case ParameterImplementation.ValueOrInCall:
         case ParameterImplementation.ValueOrInClearCall:
            // A value parameter must not be associated with a left side attribute
            if (Attribute.LeftSide)
               return "left side attributes are only compatible with out or ref parameters";
            break;
         case ParameterImplementation.RefCall:
            // A ref parameter must only be associated with an inOut attribute
            // (which hides a left side attribute with the same name)
            if (Attribute.OverlayType != AttributeStruct.OverlayEnum.inOutAttribute)
               return "ref parameters must have an associated attribute at the right side and the left side of the definition";
            break;
         case ParameterImplementation.OutCall:
         case ParameterImplementation.OutClearCall:
            // An out parameter must be only associated with a left side attribute
            if (!Attribute.LeftSide && Attribute.OverlayType != AttributeStruct.OverlayEnum.inOutAttribute)
               return "out parameters must not access an attribute of the right side of the definition";
            break;
         default:
            return "error in grammlator implementation (Parameter.Implementation not assigned or unknown)";
         }

         return "";
      }


      /// <summary>
      /// Checks the methods parameters, computes their stack offsets and updates their implementation
      /// </summary>
      /// <param name="semanticMethod"></param>
      private void EvaluateVoidMethodParameters(Definition definition)
      {
         VoidMethodClass? semanticMethod = definition.SemanticMethod;
         Int32 NumberOfFirstAttribute = AttributeNumberAtStartOfDefinition + 1;
         Int32 NumberOfLastAttributeOfRightSide = AttributeCounter;
         Int32 maximalAttributenumber
             = NumberOfLastAttributeOfLeftSide >= NumberOfLastAttributeOfRightSide
             ? NumberOfLastAttributeOfLeftSide
             : NumberOfLastAttributeOfRightSide;
         Int32 CountOfAttributesOfLeftSide = NumberOfLastAttributeOfLeftSide - NumberOfFirstAttribute + 1;
         Int32 CountOfAttributesOfRightSide = NumberOfLastAttributeOfRightSide - NumberOfFirstAttribute + 1;

         // A local copy of the reference to the method parameters array or - if no method - to an empty array
         MethodParameterStruct[] MethodParameters = semanticMethod?.MethodParameters ?? NoMethodParameters;

         /**************************************************************************************
          ****  Part 1: Evaluate And Update Method Parameters and Attribute.Implementation   ***
          **************************************************************************************/

         // for each method parameter find a corresponding attribute, check compatibility and assign offset
         Boolean error = false;
         for (Int32 parameterIndex = 0; parameterIndex < MethodParameters.Length; parameterIndex++)
         {
            /*** check method parameter ***/
            ref MethodParameterStruct MethodParameter = ref MethodParameters[parameterIndex];
            String methodParameterName = MethodParameter.NameString.ToString();

            /*** find last attribute in the list of attributes which has the same name ***/
            Int32 AttributeIndex =
                ListOfAttributesOfGrammarRule.FindLastIndex
                (
                    (x) => x.NameString == MethodParameters[parameterIndex].NameString
                );
            if (AttributeIndex < 0)
            {
               // no attribute with the same name as the formal parameter
               GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Error,
                  $"Error in method \"{semanticMethod!.MethodName }\" formal parameter \"{methodParameterName}\": "
                  + "missing attribute with the same name.",
                  semanticMethod!.Position
                  );
               error = true;
               MethodParameter.Implementation = ParameterImplementation.NotAssigned;
               continue;
            }

            /*** check compatibility of method parameter and last attribute with the same name***/

            AttributeStruct Attribute = ListOfAttributesOfGrammarRule[AttributeIndex];

            if (Attribute.Level < NestingLevel // access to context is not implemented
                || Attribute.PositionInProduction > maximalAttributenumber // redundant, might be relevant if access to context would be allowed
                || Attribute.PositionInProduction < NumberOfFirstAttribute // redundant, might be relevant if access to context would be allowed
                )
            {
               // last attribute with same name as formal parameter is defined at a different level
               GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Error,
                  $"Error in method \"{semanticMethod!.MethodName }\" formal parameter \"{methodParameterName}\": "
                  + "the attribute with the same name is not inside the parantheses of the nested grammar rule.",
                  semanticMethod!.Position
                  );
               // TODO allow restricted access to attributes left of parantheses (restrictions?)
               // TODO design and allow access (from priority method) to the attributes of the look ahead terminal symbol (to be implemented in analysis of a lower level definition)
               error = true;
               MethodParameter.Implementation = ParameterImplementation.NotAssigned;
               continue;
            }

            Debug.Assert(MethodParameter.NameString == Attribute.NameString);

            // Are attribute and parameter compatible?
            String errorDescription = AttributeParameterMatchError(Attribute, MethodParameter);
            if (!String.IsNullOrEmpty(errorDescription))
            {
               GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Error,
                  $"Error in method \"{semanticMethod!.MethodName }\": the formal parameter \"{methodParameterName}\" "
                  + "and the attribute with the same name are not compatible:"
                   + errorDescription,
                  semanticMethod!.Position
                   );
               error = true;
               MethodParameter.Implementation = ParameterImplementation.NotAssigned;
               continue;
            }

            // Compare types:
            if (Attribute.TypeString != MethodParameter.TypeString)
            {
               GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Error,
                  $"Error in method \"{semanticMethod!.MethodName }\": the type of the formal parameter  "
                  + $"\"{MethodParameter.TypeString} {methodParameterName}\""
                  + $" is different from the type of the attribute "
                  + $"\"{Attribute.TypeString} {Attribute.NameString}\"",
                  semanticMethod.Position
                   );
               error = true;
               MethodParameter.Implementation = ParameterImplementation.NotAssigned;
               continue;
            }

            /* The MethodParameter and the attribute passed all checks,
             * set MethodParameter.Implementation, Attribute.Implementation, OverlayedAttribute.Implementation */
            MethodParameter.Offset = Attribute.PositionInProduction - maximalAttributenumber;

            switch (Attribute.OverlayType)
            {
            case AttributeStruct.OverlayEnum.inOutAttribute:
            {
               // FindLastIndex found the right side Attribute.
               Debug.Assert(!Attribute.LeftSide);
               // There is an overlayed left side attribute with same position and same name.
               // Both are associated to the parameter.
               Int32 OverlayedAttributeIndex = AttributeIndex - CountOfAttributesOfLeftSide;
               AttributeStruct OverlayedAttribute = ListOfAttributesOfGrammarRule[OverlayedAttributeIndex];

               Debug.Assert(OverlayedAttribute.LeftSide);
               Debug.Assert(OverlayedAttribute.NameString == MethodParameter.NameString);
               Debug.Assert(OverlayedAttribute.PositionInProduction == Attribute.PositionInProduction);

               OverlayedAttribute.Implementation = MethodParameter.Implementation;
               ListOfAttributesOfGrammarRule[OverlayedAttributeIndex] = OverlayedAttribute;
            }
            break;

            case AttributeStruct.OverlayEnum.outClearAttribute:
            {
               Debug.Assert(Attribute.LeftSide);
               Debug.Assert(MethodParameter.Implementation == ParameterImplementation.OutCall);
               // There is an overlayed right side attribute with same position and different name
               Int32 OverlayedAttributeIndex = AttributeIndex + CountOfAttributesOfLeftSide;
               AttributeStruct OverlayedAttribute = ListOfAttributesOfGrammarRule[OverlayedAttributeIndex];
               // There may (!) be a parameter associated to this OverlayedAttribute.
               // Assume there is no such parameter, then clearing must be done by Attribute out access:
               MethodParameter.Implementation = ParameterImplementation.OutClearCall;

               // Search that formal parameter
               for (Int32 SearchIndex = 0; SearchIndex < MethodParameters.Length; SearchIndex++)
               {
                  if (MethodParameters[SearchIndex].NameString == OverlayedAttribute.NameString)
                  {
                     // Found: clearing will be done by OverlayedAttribute in access
                     //   and must not (!) be done by Attribute out access
                     MethodParameter.Implementation = ParameterImplementation.OutCall;
                     break;
                  }
               }
            }
            break;

            case AttributeStruct.OverlayEnum.inClearAttribute:
               MethodParameter.Implementation = ParameterImplementation.ValueOrInClearCall;
               break;
            }

            Attribute.Implementation = MethodParameter.Implementation;
            ListOfAttributesOfGrammarRule[AttributeIndex] = Attribute;

            // end of loop over all parameters of method
         }

         if (error)
            return;

         /**************************************************************************************
          ********  Part 2: Check Assignments To And Reset Left Side Attributes          *******
          **************************************************************************************/

         CheckAndResetLeftSideAttributes(semanticMethod);
      }

      private void CheckAndResetLeftSideAttributes(VoidMethodClass? semanticMethod)
      {
         /* For each attribute of the definitions left side check if a value will be assigned
                       * (implementation commented out:) either by default assignment (no overlay with an attribute of the right side)
                       * or by overlay with an attribute of same name and type
                       * or by an out or ref parameter of the method
                       */

         for (Int32 indexOfLeftSideAttrib = ListOfAttributesOfGrammarRule.Count - 1
             ; indexOfLeftSideAttrib >= 0 && ListOfAttributesOfGrammarRule[indexOfLeftSideAttrib].Level == NestingLevel
             ; indexOfLeftSideAttrib--)
         {
            AttributeStruct Attribute = ListOfAttributesOfGrammarRule[indexOfLeftSideAttrib];
            if (Attribute.LeftSide)
            {
               if (Attribute.OverlayType != AttributeStruct.OverlayEnum.inOutAttribute
                   && Attribute.Implementation == ParameterImplementation.NotAssigned
                  )
               {
                  // The following variant of implicit assignment of default values is commented out to force explicit assignment :
                  //    if (ListOfAttributesOfGrammarRule[attributIndex].PositionInProduction > aktuelleAttributnummer)
                  //       break; // value is set to standard by reserve()

                  // no value assigned to the attribute of the left side : create error message
                  if (semanticMethod == null)
                  {
                     P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                        $"The attribute \"{ListOfAttributesOfGrammarRule[indexOfLeftSideAttrib].NameString}\""
                        + " of the left side of the definition must be assigned a value by an overlaying attribute or a method."
                        );
                  }
                  else
                  {
                     GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Error,
                        $"The method \"{semanticMethod.MethodName}\" "
                        + "doesn't have a ref or out parameter with the name and type of the attribute "
                        + $"\"{ListOfAttributesOfGrammarRule[indexOfLeftSideAttrib].NameString}\""
                        + " of the left side of the definition.",
                        semanticMethod.Position
                        );
                  }
               }
               // The left side attribute may be used in another definition:
               //    reset OverlayType to outAttribute and Implementation to NotAssigned
               Attribute.OverlayType = AttributeStruct.OverlayEnum.outAttribute;
               Attribute.Implementation = ParameterImplementation.NotAssigned;
               ListOfAttributesOfGrammarRule[indexOfLeftSideAttrib] = Attribute;
            }
         }
      }

      private void EvaluateIntMethodParameters(Definition definition)
      {
         IntMethodClass? semanticPriority = definition.PriorityFunction;
         Int32 NumberOfFirstAttribute = AttributeNumberAtStartOfDefinition + 1;
         Int32 NumberOfLastAttributeOfRightSide = AttributeCounter;
         Int32 maximalAttributenumber
             = NumberOfLastAttributeOfLeftSide >= NumberOfLastAttributeOfRightSide
             ? NumberOfLastAttributeOfLeftSide
             : NumberOfLastAttributeOfRightSide;
         Int32 CountOfAttributesOfLeftSide = NumberOfLastAttributeOfLeftSide - NumberOfFirstAttribute + 1;
         Int32 CountOfAttributesOfRightSide = NumberOfLastAttributeOfRightSide - NumberOfFirstAttribute + 1;

         if (semanticPriority == null || semanticPriority.MethodParameters == null)
            return;
         // A local copy of the reference to the method parameters array or - if no method - to an empty array
         MethodParameterStruct[] MethodParameters = semanticPriority.MethodParameters;
         // for each method parameter find a corresponding attribute, check compatibility and assign offset
         for (Int32 parameterIndex = 0; parameterIndex < MethodParameters.Length; parameterIndex++)
         {
            ref MethodParameterStruct MethodParameter = ref MethodParameters[parameterIndex];
            String methodParameterName = MethodParameter.NameString.ToString();

            // find last attribute in the list of attributes which has the same name
            Int32 AttributeIndex =
                ListOfAttributesOfGrammarRule.FindLastIndex
                (
                    (x) => x.NameString == MethodParameters[parameterIndex].NameString
                );
            if (AttributeIndex < 0)
            {
               // no attribute with the same name as the formal parameter
               // no attribute with the same name as the formal parameter
               GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Error,
                   $"Error in semantic priority \"{semanticPriority!.MethodName }\" formal parameter \"{methodParameterName}\": "
                   + "missing attribute with the same name.",
                   semanticPriority!.Position
                   );
               MethodParameter.Implementation = ParameterImplementation.NotAssigned;
               continue;
            }

            AttributeStruct Attribute = ListOfAttributesOfGrammarRule[AttributeIndex];

            Debug.Assert(MethodParameter.NameString == Attribute.NameString);

            if (Attribute.Level < NestingLevel // access to context is not implemented
                || Attribute.PositionInProduction > maximalAttributenumber // redundant, might be relevant if access to context would be allowed
                || Attribute.PositionInProduction < NumberOfFirstAttribute // redundant, might be relevant if access to context would be allowed
                )
            {
               // last attribute with same name as formal parameter is defined at a different level
               GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Error,
                  $"Error in  priority method \"{semanticPriority!.MethodName }\" formal parameter \"{methodParameterName}\": "
                  + "the attribute with the same name is not inside the parantheses of the nested grammar rule.",
                  semanticPriority!.Position
                  );
               // TODO allow restricted access to attributes left of parantheses (restrictions?)
               // TODO design and allow access (from priority function) to the attributes of the look ahead terminal symbol (to be implemented in analysis of a lower level definition)
               MethodParameter.Implementation = ParameterImplementation.NotAssigned;
               continue;
            }

            if (Attribute.LeftSide)
            {
               // A parameter of a priority method must not be associated with a left side attribute
               GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Error,
                  $"Error in  priority method \"{semanticPriority!.MethodName }\": the formal parameter \"{methodParameterName}\""
                     + " has the same name as an attribute of the left side of the definition",
                     semanticPriority!.Position
                     );
               MethodParameter.Implementation = ParameterImplementation.NotAssigned;
               continue;
            }

            // A parameter of a priority method must be a value or an in parameter
            if (MethodParameter.Implementation != ParameterImplementation.ValueOrInCall
                && MethodParameter.Implementation != ParameterImplementation.ValueOrInClearCall)
            {
               GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Error,
                  $"Error in  priority method \"{semanticPriority!.MethodName }\": the formal parameter \"{methodParameterName}\""
                     + "is not a value parameter or an in parameter.",
                     semanticPriority!.Position
                     );
               MethodParameter.Implementation = ParameterImplementation.NotAssigned;
               continue;
            }

            // Compare types:
            if (Attribute.TypeString != MethodParameter.TypeString)
            {
               GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Error,
                  $"Error in priority method \"{semanticPriority!.MethodName }\": the type of the formal parameter "
                  + $"\"{MethodParameter.TypeString} {methodParameterName}\""
                  + $" is different from the type of the attribute "
                  + $"\"{Attribute.TypeString} {Attribute.NameString}\"",
                  semanticPriority.Position
                   );
               MethodParameter.Implementation = ParameterImplementation.NotAssigned;
               continue;
            }

            // MethodParameter passed all checks 
            MethodParameter.Offset = Attribute.PositionInProduction - maximalAttributenumber;
         }
      }
   }
}