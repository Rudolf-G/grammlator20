using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Collections;

namespace Grammlator {
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
      // TODO Dynamic priorites
      // TODO Additional Analysis of conflicts
      // TODO CodeGen: allow for different options
      // TODO Phase4 & CodeGen: use variables instead of stack where appopriate
      // TODO Analyse grammar and give recommendations for order of terminals symbols
      // TODO Analyse the grammar and give recommendations to split the grammar

      /// <summary>
      /// Is set to false while processing the definitions of the startsymbol,
      /// may be initialized to false to suppress this optimization.
      /// </summary>
      private Boolean OptimizeTrivialDefinitions = true;

      private static readonly ListOfDefinitions EmptyListOfNontrivialDefinitions = new ListOfDefinitions(0);

      public class ListOfSymbols: List<Symbol> {
         public ListOfSymbols(Int32 capacity) : base(capacity) { }

         public void RemoveFromEnd(Int32 n) => this.RemoveRange(this.Count - n, n);
         }

      /// <summary>
      /// Stores the StringIndexes of all elements of the last C# enum.
      /// Is null if the last optional enum has been omitted.
      /// Is the empty list, if an enum with no elements has been recognized. 
      /// Is null or empty after the last enum has been evaluated.
      /// </summary>
      public List<Int32>? Enumlist = new List<Int32>();

      /// <summary>
      /// The StringIndex of the name of the last enum the parser found in the source.
      /// Is -1 if an optional enum has been empty.
      /// </summary>
      public Int32 EnumNameStringIndex;

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
      private void TerminalSymbolDeclaration(Int32 nameIndex, Int32 numberOfAttributes, Int32 weight)
         {
         String Name = GlobalVariables.GetStringOfIndex(nameIndex);  // TODO use nameIndex as Key in SymbolDictionary
         Debug.Assert(!String.IsNullOrEmpty(Name), $"{nameof(Name)} is 0 or empty");

         if (SymbolDictionary.ContainsKey(nameIndex))
            {
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                $"The terminal symbol {Name} has been already defined");
            // continue to keep integrity of the data structures
            }
         else
            {
            SymbolDictionary[nameIndex] =
               new TerminalSymbol(Name, Lexer.LexerTextPos) {
                  Weight = weight,
                  SymbolNumber = SymbolDictionary.Count,
                  AttributetypeStringIndexList = ListOfAttributesOfGrammarRule.GetAttributeTypeStringIndexes(numberOfAttributes),
                  AttributenameStringIndexList = ListOfAttributesOfGrammarRule.GetAttributeIdentifierStringIndexes(numberOfAttributes)
                  };
            }

         // Remove the attributes
         ListOfAttributesOfGrammarRule.RemoveFromEnd(numberOfAttributes);

         // and reset the attribute counter for the next terminal symbols declaration
         AttributeCounter = 0;

         return;
         }

      /// <summary>
      /// Add the nonterminal symbol to the <see cref="SymbolDictionary"/> or, if already contained test for compatibility, 
      /// update (LeftSide=true, OverlayType=... out) and assign the attributes, set <see cref="NumberOfLastAttributeOfLeftSide"/>
      /// and reset <see cref="AttributeCounter"/> to <see cref="AttributeNumberAtStartOfDefinition"/>
      /// </summary>
      /// <param name="symbolName"></param>
      /// <param name="numberOfAttributes"></param>
      /// <returns>The <see cref="NonterminalSymbol"/></returns>
      private NonterminalSymbol NonterminalSymbolDefinition(Int32 symbolNameIndex, Int32 numberOfAttributes)
         {
         // The parser recognized the left side of a rule and has not yet evaluated any right side

         string symbolName = GlobalVariables.GetStringOfIndex(symbolNameIndex);

         Debug.Assert(!String.IsNullOrEmpty(symbolName), "Identifier IsNullOrEmpty");

         NonterminalSymbol ns;

         if (SymbolDictionary.TryGetValue(symbolNameIndex, out Symbol? Symbol))
            {
            // The nonterminal symbol has been used already als element of a definition. The types of its attributes are known.
            ns = (Symbol as NonterminalSymbol)!;
            if (ns == null)
               {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                   $"{symbolName} has been already used as terminal symbol. ");
               // TODO check behaviour after error
               }

            else if (ns.IsDefined)
               {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                   $"{symbolName} has been already defined as nonterminal symbol. ");
               // TODO check behaviour after error
               }

            else if (ns.NumberOfAttributes != numberOfAttributes)
               {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                   $"{symbolName} has already been used with a different number of attributes. ");
               // TODO check behaviour after error
               }

            else if (!AttributeTypesCoincide(ns))
               {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                  $"{symbolName} has already been used with at least one attribute with a different type. ");
               // TODO check behaviour after error
               }

            // The AttributtypeList has been assigned at first usage and has just been checked

            // The names of the attributes are copied now from the ListOfAttributesOfSyntaxRule.
            // They are not removed from ListOfAttributesOfSyntaxRule, because it may be a definition inside another definition.
            // They will be removed when the end of a syntax rule in the syntax rule list or the end of the enclosing definition is recognized.
            Debug.Assert(ns.AttributenameStringIndexList == null || ns.AttributenameStringIndexList.Length == 0);
            ns.AttributenameStringIndexList = ListOfAttributesOfGrammarRule.GetAttributeIdentifierStringIndexes(numberOfAttributes);
            }

         // The nonterminal symbol has not yet been used (Symbol == null)
         // or there has been an error and we proceed as if it has been a new nonterminal symbol
         // TOCHECK proceed with a modified  symbolname ???
         ns = new NonterminalSymbol(symbolName,
            Lexer.LexerTextPos,
            symbolNumber: SymbolDictionary.Count - GlobalVariables.NumberOfTerminalSymbols,
            attributetypeStringIndexList: ListOfAttributesOfGrammarRule.GetAttributeTypeStringIndexes(numberOfAttributes),
            attributenameStringIndexList: ListOfAttributesOfGrammarRule.GetAttributeIdentifierStringIndexes(numberOfAttributes)
            );
         SymbolDictionary[symbolNameIndex] = ns;

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
      private Symbol EvaluateSymbolnameFoundInRightSide(Int32 nameIndex, Int32 NumberOfAttributes)
         {
         string Name = GlobalVariables.GetStringOfIndex(nameIndex);

         if (SymbolDictionary.TryGetValue(nameIndex, out Symbol? symbol) /*Symbol == null*/)
            {
            if (symbol.NumberOfAttributes != NumberOfAttributes)
               {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                  $"The symbol {Name} has been used at its first occurence with a different number of attributes. "
                  );
               }
            else if (!AttributeTypesCoincide(symbol))
               {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                  $"The symbol {Name} has been used at its first occurence with at least one attribute of a different type. "
                  );
               }
            }
         else
            {
            // New symbol: create instance
            symbol = new NonterminalSymbol(Name,
                  Lexer.LexerTextPos,
                  symbolNumber: SymbolDictionary.Count - GlobalVariables.NumberOfTerminalSymbols,
                  attributetypeStringIndexList:
                     NumberOfAttributes == 0
                     ? emptyListOfStringIndexes
                     : ListOfAttributesOfGrammarRule.GetAttributeTypeStringIndexes(NumberOfAttributes)
                  );

            SymbolDictionary[nameIndex] = symbol;
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
      private Int32 MakeNewNameStringIndex(String prefix)
         => GlobalVariables.GetIndexOfString($"({prefix}{++CountOfGeneratedNames})");

      ///<summary>
      /// Generates an internal name by adding a postfix to a symbols name
      ///</summary>
      /// <param name="type">the type of the grammar rule determines the postfix</param>
      /// <param name="nameOfSymbol">name of the symbol to which the postfix has to be appended</param>
      ///<returns>new name</returns>
      private static String MakeNewName(TypeOfGrammarRule type, string nameOfSymbol)
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
         String result = nameOfSymbol + Postfix;

         return result;
         }

      //{-*****************************************************************************
      //--*******                  Make Grammar Rule                          *********
      //--*****************************************************************************}

      private enum TypeOfGrammarRule { optional, repeat0lr, repeat1lr, repeat0rr, repeat1rr };

      private Symbol MakeGrammarRule(Symbol existingSymbol, TypeOfGrammarRule type)
         {
         // TODO check if and how different variants are implemented in the grammar: optional, repeat0lr, ... repeat1rr

         /* Repeat is implemented by left recursion "lr" or right recursion "rr"
          *   type "optional":
          *      newSymbol =  existingSymbol || ;
          *   type "repeat0lr":
          *      newSymbol =  || newSymbol, existingSymbol;
          *   type "repeat1lr":
          *      newSymbol =  existingSymbol || newSymbol, existingSymbol;
          *   type "repeat0rr":
          *      newSymbol =  || existingSymbol, newSymbol;
          *   type "repeat1rr":
          *      newSymbol =  existingSymbol || existingSymbol, newSymbol;
          */

         // Create (synthetic) name of new nonterminal symbol by adding postfix to existing name
         string NewName = MakeNewName(type, existingSymbol.Identifier);
         Int32 NewNameIndex = GlobalVariables.GetIndexOfString(NewName);

         // use existing (synthetic) symbol / definition if name already has been defined
         if (!SymbolDictionary.TryGetValue(NewNameIndex, out Symbol? MadeSymbol))
            {
            // else create, store in dictionary and define new symbol
            MadeSymbol = DefineNewSymbol(existingSymbol, type, NewNameIndex, NewName);
            }
         ListOfAttributesOfGrammarRule.RemoveFromEnd(existingSymbol.NumberOfAttributes);
         AttributeCounter -= existingSymbol.NumberOfAttributes;

         return MadeSymbol;
         }

      private NonterminalSymbol DefineNewSymbol(
            Symbol existingSymbol,
            TypeOfGrammarRule type,
            Int32 newNameIndex,
            string newName)
         {
         var newSymbol = new NonterminalSymbol(newName,
            Lexer.LexerTextPos,
            symbolNumber: SymbolDictionary.Count - GlobalVariables.NumberOfTerminalSymbols,
            attributetypeStringIndexList: ListOfAttributesOfGrammarRule.GetAttributeTypeStringIndexes(0), // has no attributes
            attributenameStringIndexList: ListOfAttributesOfGrammarRule.GetAttributeIdentifierStringIndexes(0)
            );

         SymbolDictionary[newNameIndex] = newSymbol;
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
      private void PushAttributeToListOfAttributesOfGrammarRule(Int32 NewTypeStringIndex, Int32 NewNameStringIndex)
         {

         var newAttribute = new AttributeStruct(
           NewTypeStringIndex, NewNameStringIndex,
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
                 x.NameStringIndex == NewNameStringIndex
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

         if (overlayedAttribute.NameStringIndex == NewNameStringIndex)
            {
            //same name: must have same position and same type, usage is inOut
            if (overlayedAttribute.PositionInProduction != AttributeCounter)
               { // the attributes must have the same (overlaying) position
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                   $"The defined nonterminal or the definition already contains an attribute \"{GlobalVariables.GetStringOfIndex(NewNameStringIndex)}\", but with different position (does not overlay)."
                   );
               }
            else if (overlayedAttribute.TypeStringIndex != NewTypeStringIndex)
               {// overlaying attributes must have the same type
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                   $"The defined nonterminal or the definition already contains an overlaying attribute \"{GlobalVariables.GetStringOfIndex(NewNameStringIndex)}\", but with different type."
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
            if (overlayedAttribute.TypeStringIndex != newAttribute.TypeStringIndex)
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

      private readonly Int32[] emptyListOfStringIndexes = Array.Empty<Int32>();

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
            if (symbol.AttributetypeStringIndexList[i]
                != ListOfAttributesOfGrammarRule[ListOfAttributesOfGrammarRule.Count - NumberOfAttributes + i].TypeStringIndex)
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
      private void MethodProperties(out MethodClass method, Int32 methodModifierStringIndex, Int32 typeStringIndex, Int32 nameStringIndex)
         {
         String methodModifier = GlobalVariables.GetStringOfIndex(methodModifierStringIndex);
         String type = GlobalVariables.GetStringOfIndex(typeStringIndex);
         String name = GlobalVariables.GetStringOfIndex(nameStringIndex);

         if (!CSharpMethodProperties.Contains(methodModifier))
            CreateParserErrorMessage($"unknown method modifier \"{methodModifier}\"");

         if (type == "int")
            {
            method = new IntMethodClass(methodName: name);
            }
         else if (type == "void")
            {
            method = new VoidMethodClass(methodName: name);
            }
         else
            {
            CreateParserErrorMessage($"expected int or void method, found \"{type}\" method.");
            method = new VoidMethodClass(methodName: name);
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
      private void MethodProperties(out MethodClass method, Int32 methodModifier1StringIndex,
         Int32 methodModifier2StringIndex, Int32 typeStringIndex, Int32 nameStringIndex)
         {
         String methodModifier1 = GlobalVariables.GetStringOfIndex(methodModifier1StringIndex);

         if (!CSharpMethodProperties.Contains(methodModifier1))
            CreateParserErrorMessage($"unknown method modifier \"{methodModifier1}\"");
         MethodProperties(out method, methodModifier2StringIndex, typeStringIndex, nameStringIndex);
         }

      /// <summary>
      /// Parser recognized a formal parameter.
      /// Add the details of the formal parameter to <see cref="LastFormalParameterList"/>
      /// </summary>
      /// <param name="ParameterModifierOpt">A C# formal parameter modifier: "ref", "out" or ""</param>
      /// <param name="type">A C# or user defined type (e.g. "Int32" or "Object" or "MyType")</param>
      /// <param name="name">The name (identifier) of the formal parameter</param>
      private void FormalParameter(Int32 parameterModifierOptStringIndex, Int32 typeStringIndex, Int32 nameStringIndex)
         {
         String parameterModifierOpt = GlobalVariables.GetStringOfIndex(parameterModifierOptStringIndex);

         ParameterImplementation callType;
         switch (parameterModifierOpt)
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
            Implementation = callType, NameStringIndex = nameStringIndex, TypeStringIndex = typeStringIndex
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

         foreach (KeyValuePair<Int32, Symbol> KeyValue in SymbolDictionary)
            {
            Symbol Symbol = KeyValue.Value;

            string GetNameOfSymbol()
               => GlobalVariables.GetStringOfIndex(KeyValue.Key);

            if (Symbol != null)
               {
               if (Symbol is NonterminalSymbol nt)
                  {
                  if (nt.NontrivialDefinitionsList == null) // null != empty list !
                     {
                     OutputMessage(MessageTypeOrDestinationEnum.Error,
                        $"{GetNameOfSymbol()} is used as terminal or nonterminal symbol but not defined.",
                        nt.FirstPosition
                        );
                     }

                  if (!nt.isUsed)
                     {
                     OutputMessage(MessageTypeOrDestinationEnum.Warning,
                        $"The nonterminal symbol {GetNameOfSymbol()} is not used.",
                        nt.FirstPosition
                        );
                     }
                  }

               if ((Symbol is TerminalSymbol t) && !t.isUsed)
                  {
                  OutputMessage(MessageTypeOrDestinationEnum.Information,
                     $"The terminal symbol {GetNameOfSymbol()} is not used in any definition (may be used in look ahead)",
                     t.FirstPosition
                     );
                  }
               }
            }
         return maxMessageType;
         }

      /*****************************************************************************
       *******            evaluate definition                              *********
       *****************************************************************************/

      /// <summary>
      /// Store the definition in ActualListOfNontrivialDefinitions or in ActualListOfTrivialDefinitions (if OptimizeTrivialDefinitions).
      /// <see cref="EvaluateMethodParameters(VoidMethodClass)"/>. Remove attributes from <see cref="ListOfAttributesOfGrammarRule"/>.
      /// </summary>
      /// <param name="constantPriority">0 or explicitly defined constant priority</param>
      /// <param name="priorityFunction">null or C# Int32 method</param>
      /// <param name="semanticMethod">null or C# void method</param>
      /// <param name="optimizeTrivialDefinitions">if false there will be no special handling of trivial definitions</param>
      private void EvaluateDefinition(Int32 constantPriority, IntMethodClass? priorityFunction, VoidMethodClass? semanticMethod, bool optimizeTrivialDefinitions)
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
            EvaluateMethodParameters(semanticMethod: null);

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
                   AttributeIdentifierStringIndexArray = ListOfAttributesOfGrammarRule.GetAttributeIdentifierStringIndexes(NumberOfAttributesOfNewDefinition)
                   };

            ActualListOfNontrivialDefinitions.Add(NewDefinition);

            EvaluateMethodParameters(NewDefinition.SemanticMethod);

            // Remove the attributes of the new defintiion from ListOfAttributesOfSyntaxRule
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
         for (int IndexOfTerminalSymbol = 0; IndexOfTerminalSymbol < excludedTerminalSymbols.Length; IndexOfTerminalSymbol++)
            {
            if (excludedTerminalSymbols[IndexOfTerminalSymbol])
               continue;
            Symbol s = GlobalVariables.GetTerminalSymbolByIndex(IndexOfTerminalSymbol);
            for (int IndexOfAttribute = 0; IndexOfAttribute < s.AttributenameStringIndexList.Length; IndexOfAttribute++)
               {
               AttributeCounter++;
               PushAttributeToListOfAttributesOfGrammarRule(s.AttributetypeStringIndexList[IndexOfAttribute], s.AttributenameStringIndexList[IndexOfAttribute]);
               }
            Debug.Assert(NumberOfElements == 0);
            ElementVariantRecognized(s);
            EndOfDefinitionRecognized(constPriority: 0, dynPriority: null, method: null);
            }
         }

      /*********************************************************************************
       *******  This Checks the  Method Parameters And Computes Stack Offsets  *********
       *********************************************************************************/

      /// <summary>
      ///  an empty list of method parameters
      /// </summary>
      private readonly MethodParameterStruct[] NoMethodParameters = Array.Empty<MethodParameterStruct>();

      /// <summary>
      /// Checks the methods parameters, computes their stack offsets and updates their implementation
      /// </summary>
      /// <param name="semanticMethod"></param>
      private void EvaluateMethodParameters(VoidMethodClass semanticMethod)
         {
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

         /***** local method *****/
         static String AttributeParameterMatchError(AttributeStruct Attribute, MethodParameterStruct Parameter)
            {
            Debug.Assert(Parameter.NameStringIndex == Attribute.NameStringIndex);

            // A value parameter must not be associated with a left side attribute
            if (Attribute.LeftSide
                && (Parameter.Implementation == ParameterImplementation.ValueOrInCall
                   || Parameter.Implementation >= ParameterImplementation.ValueOrInClearCall)
                   )
               {
               return "left side attributes are only compatible with out or ref parameters";
               }

            // A ref parameter must only be associated with an inOut attribute
            // (which hides a left side attribute with the same name)
            if (Parameter.Implementation == ParameterImplementation.RefCall
                && Attribute.OverlayType != AttributeStruct.OverlayEnum.inOutAttribute)
               {
               return "ref parameters must have an associated attribute at the right side and the left side of the definition";
               }

            // An out parameter must be only associated with a left side attribute
            if (!Attribute.LeftSide
                && (Parameter.Implementation == ParameterImplementation.OutCall
                    || Parameter.Implementation == ParameterImplementation.OutClearCall))
               {
               return "out parameters must not access an attribute of the right side of the definition";
               }
            return "";
            }

         /**************************************************************************************
          ****  Part 1: Evaluate And Update Method Parameters and Attribute.Implementation   ***
          **************************************************************************************/

         // for each method parameter find a corresponding attribute, check compatibility and assign offset
         for (Int32 parameterIndex = 0; parameterIndex < MethodParameters.Length; parameterIndex++)
            {
            ref MethodParameterStruct MethodParameter = ref MethodParameters[parameterIndex];
            string methodParameterName = GlobalVariables.GetStringOfIndex(MethodParameter.NameStringIndex);

            // find last attribute in the list of attributes which has the same name
            Int32 AttributeIndex =
                ListOfAttributesOfGrammarRule.FindLastIndex
                (
                    (x) => x.NameStringIndex == MethodParameters[parameterIndex].NameStringIndex
                );
            if (AttributeIndex < 0)
               {
               // no attribute with the same name as the formal parameter
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
              $"There is no attribute with the same name as the formal parameter \"{methodParameterName}\""
              + $" of method \"{semanticMethod.MethodName }\".");
               MethodParameter.Implementation = ParameterImplementation.NotAssigned;
               continue;
               }

            AttributeStruct Attribute = ListOfAttributesOfGrammarRule[AttributeIndex];

            if (Attribute.Level < NestingLevel // access to context is not implemented
                || Attribute.PositionInProduction > maximalAttributenumber // redundant, might be relevant if access to context would be allowed
                || Attribute.PositionInProduction < NumberOfFirstAttribute // redundant, might be relevant if access to context would be allowed
                )
               {
               // last attribute with same name as formal parameter is defined at a different level
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                   $"The attribute \"{methodParameterName}\", "
                   + $"which is used as formal parameter of method \"{semanticMethod.MethodName}\", "
                   + "is outside of the paranthesized grammar rule. "
                   );
               // TODO allow restricted access to attributes left of parantheses (restrictions?)
               // TODO design and allow access (from priority function) to the attributes of the look ahead terminal symbol (to be implemented in analysis of a lower level definition)
               MethodParameter.Implementation = ParameterImplementation.NotAssigned;
               continue;
               }

            // Are attribute and parameter compatible?
            String errorDescription = AttributeParameterMatchError(Attribute, MethodParameter);
            if (!string.IsNullOrEmpty(errorDescription))
               {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                   $"The attribute \"{methodParameterName}\", "
                   + $"and the associated formal parameter of method \"{semanticMethod.MethodName}\", "
                   + "are not compatible: "
                   + errorDescription
                   );
               MethodParameter.Implementation = ParameterImplementation.NotAssigned;
               continue;
               }

            // Has been checked already when the attribute had been added to MethodParameters[]:
            if (Attribute.TypeStringIndex != MethodParameter.TypeStringIndex)
               {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
              $"The type \"{Attribute.TypeStringIndex}\" of the attribute \"{GlobalVariables.GetStringOfIndex(Attribute.NameStringIndex)})\"" +
              $" differs from the parameters type \"{GlobalVariables.GetStringOfIndex(MethodParameter.TypeStringIndex)}\" ");
               MethodParameter.Implementation = ParameterImplementation.NotAssigned;
               MethodParameter.Implementation = ParameterImplementation.NotAssigned;
               continue;
               }

            // MethodParameter passed all checks 
            MethodParameter.Offset = Attribute.PositionInProduction - maximalAttributenumber;

            if (Attribute.OverlayType == AttributeStruct.OverlayEnum.inOutAttribute)
               {
               // FindLastIndex found the right side Attribute.
               Debug.Assert(!Attribute.LeftSide);
               // There is an overlayed left side attribute with same position and same name.
               // Both are associated to the parameter.
               Int32 OverlayedAttributeIndex = AttributeIndex - CountOfAttributesOfLeftSide;
               AttributeStruct OverlayedAttribute = ListOfAttributesOfGrammarRule[OverlayedAttributeIndex];

               Debug.Assert(OverlayedAttribute.LeftSide);
               Debug.Assert(OverlayedAttribute.NameStringIndex == MethodParameter.NameStringIndex);
               Debug.Assert(OverlayedAttribute.PositionInProduction == Attribute.PositionInProduction);

               OverlayedAttribute.Implementation = MethodParameter.Implementation;
               ListOfAttributesOfGrammarRule[OverlayedAttributeIndex] = OverlayedAttribute;
               }
            else if (Attribute.OverlayType == AttributeStruct.OverlayEnum.outClearAttribute)
               {
               Debug.Assert(MethodParameter.Implementation == ParameterImplementation.OutCall);
               // Attribute is in left side
               Debug.Assert(Attribute.LeftSide);
               // There is an overlayed right side attribute with same position and different name
               Int32 OverlayedAttributeIndex = AttributeIndex + CountOfAttributesOfLeftSide;
               AttributeStruct OverlayedAttribute = ListOfAttributesOfGrammarRule[OverlayedAttributeIndex];
               // There may (!) be a parameter associated to this OverlayedAttribute.
               // Assume there is no such parameter, then clearing must be done by Attribute out access:
               MethodParameter.Implementation = ParameterImplementation.OutClearCall;

               // Search such parameter
               for (Int32 SearchIndex = 0; SearchIndex < MethodParameters.Length; SearchIndex++)
                  {
                  if (MethodParameters[SearchIndex].NameStringIndex == OverlayedAttribute.NameStringIndex)
                     {
                     // Found: clearing will be done by OverlayedAttribute in access
                     //   and must not (!) be done by Attribute out access
                     MethodParameter.Implementation = ParameterImplementation.OutCall;
                     break;
                     }
                  }
               }
            else if (Attribute.OverlayType == AttributeStruct.OverlayEnum.inClearAttribute)
               {
               MethodParameter.Implementation = ParameterImplementation.ValueOrInClearCall;
               }

            Attribute.Implementation = MethodParameter.Implementation;
            ListOfAttributesOfGrammarRule[AttributeIndex] = Attribute;

            // end of loop over all parameters of method
            }

         /**************************************************************************************
          ********  Part 2: Check Assignments To And Reset Left Side Attributes          *******
          **************************************************************************************/

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
                        "There must be an overlaying attribute or a method with a ref or out parameter with the name and type of the attribute "
                        + $"\"{ListOfAttributesOfGrammarRule[indexOfLeftSideAttrib].NameStringIndex}\""
                        );
                     }
                  else
                     {
                     P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                        $"The method \"{semanticMethod.MethodName}\" "
                        + "must have a ref or out parameter with the name and type of the attribute "
                        + $"\"{ListOfAttributesOfGrammarRule[indexOfLeftSideAttrib].NameStringIndex}\"");
                     }
                  }
               // is left side attribute may be used in one mor definition:
               //    reset OverlayType to outAttribute and Implementation to NotAssigned
               Attribute.OverlayType = AttributeStruct.OverlayEnum.outAttribute;
               Attribute.Implementation = ParameterImplementation.NotAssigned;
               ListOfAttributesOfGrammarRule[indexOfLeftSideAttrib] = Attribute;
               }
            }
         }
      }
   }