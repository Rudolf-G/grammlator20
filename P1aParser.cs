using GrammlatorRuntime;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Collections;

namespace Grammlator {
   /// <summary>
   /// Grammlator Parser (uses Lexer which uses InputClassifier)
   /// </summary>
   internal sealed partial class P1aParser : GrammlatorApplication {
      /// <summary>
      /// Create an instance of the parser and execute it
      /// </summary>
      /// <param name="SbResult"></param>
      /// <param name="SourceReader"></param>
      /// <param name="symbolDictionary"></param>
      public static void MakeInstanceAndExecute(
          SpanReaderWithCharacterAndLineCounter SourceReader,
          Dictionary<Int32, Symbol> symbolDictionary)
          => new P1aParser(SourceReader, symbolDictionary).DoPhase1();

      /// <summary>
      /// Constructor of <see cref="P1aParser"/>, creates <see cref="SymbolDictionary"/> and <see cref="Lexer"/>,
      /// copies all read lines to <paramref name="SbResult"/>
      /// </summary>
      /// <param name="SbResult"></param>
      /// <param name="SourceReader"></param>
      /// <param name="SymbolDictionary"></param>
      private P1aParser(
          SpanReaderWithCharacterAndLineCounter SourceReader,
          Dictionary<Int32, Symbol> SymbolDictionary) : base(initialSizeOfAttributeStack: 100, initialSizeOfStateStack: 100)
      {
         this.SymbolDictionary = SymbolDictionary;
         this.Source = SourceReader.Source;

         Lexer = new P1bLexer(SourceReader, _a, _s);
      }

      private void DoPhase1()
      {
         // Grammlator parameterization
         String RegionString = GlobalVariables.RegionString.Value;
         String EndregionString = GlobalVariables.EndregionString.Value;
         String GrammarString = GlobalVariables.GrammarString.Value;

         try
         {
            AnalyzeGrammlatorGrammar();

            if (Lexer.MarkedLineFollows(EndregionString, GrammarString))
            {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Status,
                  $"Found \"{EndregionString} {GrammarString}\".");
            }
            else
            {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Abort,
                  $"Expected \"{EndregionString} {GrammarString}\"");
            }

            GlobalVariables.NumberOfNonterminalSymbols = SymbolDictionary.Count - GlobalVariables.NumberOfTerminalSymbols;

            // Check usage of symbols
            if (CheckUsageOfSymbols(GlobalVariables.OutputMessageAndPosition) >= MessageTypeOrDestinationEnum.Abort)
            {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Abort, "translation aborted: error(s) in source"); // throws an exception
               Debug.Fail("This debug instruction should never be executed");
            }
         }
         finally { }
      }

      private readonly P1bLexer Lexer;

      private readonly ReadOnlyMemory<Char> Source;

      #region declaration of fields
      /// <summary>
      /// Set by constructor, contains the terminal symbols followed by the nonterminal symbols
      /// </summary>
      private readonly Dictionary<Int32, Symbol> SymbolDictionary;

      /// <summary>
      /// level of paranthesis, initialized with 0
      /// </summary>
      private Int32 NestingLevel = 0;

      /// <summary>
      /// Number (>=0) of the last attribute of the left side of the actual production, counted from beginning of the outermost production
      /// </summary>
      private Int32 NumberOfLastAttributeOfLeftSide = 0;

      /// <summary>
      ///  Number (>=0, 0 if no attribute) of the actual attribute, counted from beginning of the outermost production
      /// </summary>
      private Int32 AttributeCounter = 0;

      /// <summary>
      /// Is 0 when the analysis of an outer production starts, is changed only when the nesting level changes
      /// </summary>
      private Int32 AttributeNumberAtStartOfDefinition = 0; // Number der Attribute vor der rechten Seite bzw. öffnenden Klammer, wird nur durch Klammern geändert!

      /// <summary>
      /// Number of definitions of the actual (nested) rule less or equal ActualListOfDefinitions.Count; descendants are not counted.
      /// </summary>
      private Int32 NumberOfNontrivialDefinitions = 0; // Number of nontrivial definitions of the actual nichtterminalen Symbols <= aktuelleDefinitionnListe.Count

      /// <summary>
      /// Contains all (perhaps nested) Definitions which are not yet assigned to a nonterminal symbol
      /// </summary>
      private readonly ListOfDefinitions ActualListOfNontrivialDefinitions = new ListOfDefinitions(100);

      /// <summary>
      /// Number of the trival definitions of the actual (nested) production &lt;= ActualListOfTrivialDefinitions.Count
      /// </summary>
      private Int32 NumberOfTrivalDefinitions = 0;

      /// <summary>
      /// Contains all (perhaps nested) trivial definitions which are not yet assigned to a nonterminal symbol.
      /// A trivial definition contains 1 element, no condition, no action and the same number of attributes as the left side.
      /// </summary>
      private readonly ListOfSymbols ActualListOfTrivialDefinitions = new ListOfSymbols(100);

      /// <summary>
      /// Number of the elements of the actual definition less or equal ActualListOfElements.Count
      /// </summary>
      private Int32 NumberOfElements = 0;

      /// <summary>
      /// Contains all elements of the actual (perhaps nested) definition.
      /// </summary>
      private readonly ListOfSymbols ActualListOfElements = new ListOfSymbols(100);
      #endregion declaration of fields

      /// <summary>
      /// Output the given message together with the actual input position, throw exception if Abort
      /// </summary>
      /// <param name="messageType"></param>
      /// <param name="message"></param>
      private void P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum messageType, String message)
          => GlobalVariables.OutputMessageAndPosition(messageType, message, Lexer.LexerTextPos);

      #region grammar
      //| /* ---- Start of grammlator grammar as control structure  ---- */
      //|
      //| // Compiler settings:
      //| IfToSwitchBorder: "5";
      //| VariableNameSymbol: "ParserInput";
      //| AssignSymbolInstruction: "ParserInput = Lexer.PeekSymbol();";
      //| AcceptSymbolInstruction: "Lexer.AcceptSymbol();";
      //| TerminalSymbolEnum: "LexerResult";
      //| StateDescriptionPrefix: "StateDescription";
      //| ErrorHandlerMethod: "ErrorHandler";
      //| LineLengthLimit: "150";
      //|
      //| // Terminal symbols and their probabilty to appear in input:
      //|     DefiningSymbol % 19
      //|     | Colon % 35        
      //|     | Percent
      //|     | CSharpEnd
      //|     | Error 
      //|     | Minus
      //|     | Number(Int32 value)
      //|     | StarEqual  
      //|     | MinusEqual
      //|     | Questionmark
      //|     | Asterisk 
      //|     | Plus 
      //|     | Comma
      //|     | NumberSign
      //|     
      //|     | GroupStart %35| OptionStart %35 | RepeatStart %35
      //|     | DoubleQuestionmark 
      //|     | CSharpStart %19
      //|     | GroupEnd | RepeatEnd | OptionEnd
      //|     | Name(Int32 stringIndex) % 45
      //|     | LexerString(Int32 stringIndex) % 30
      //|     | DefinitionSeparatorSymbol % 45  
      //|     | TerminatorSymbol % 42
      public enum LexerResultCopy { // Defines the output of the lexer, which is assigned to Symbol to be used by the parser
                                    // The elements of LexerResult are ordered such that grammlator can
                                    // generate efficient code for the conditions of the parsers actions
         DefiningSymbol, // =
         Colon,          // :
         Percent,        // %
         CSharpEnd,      // represents the change from CSharp lines to grammlator lines
         Error,          // Error is the result if some input could not be assigned to any other LexerResult
         Minus,          // Part of "-="
         Number/* (Int32 i) */,
         StarEqual,      // "*=", added by the lexer
         MinusEqual,     // "-=", addeed by the lexer
         Questionmark,   // part of "??"
         Asterisk,       // Part of "*="
         Plus,           // +
         Comma,          // ,
         NumberSign,     // #

         GroupStart, OptionStart, RepeatStart, // these are the characters ( [ {

         DoubleQuestionmark,
         CSharpStart, // represents the change from grammlator lines to CSharp lines

         GroupEnd, RepeatEnd, OptionEnd,  // these are the characters ) ] } #

         Name/* (Int32 stringIndex) */,
         LexerString/* (Int32 stringIndex) */,

         DefinitionSeparatorSymbol, // |
         TerminatorSymbol           // ;
      };

      //|  *= GrammlatorGrammar /* one startsymbol, which has no attributes */

      //| // renaming some symbols to improve readability
      //|  "," = Comma; // "," is used where provided in EBNF, Comma in other cases
      //|  "=" = DefiningSymbol; "|" = DefinitionSeparatorSymbol; ";" = TerminatorSymbol; ":" = Colon; "%" = Percent;
      //|  "-" = Minus; "+" = Plus; "*" = Asterisk; "(" = GroupStart; "[" = OptionStart; "{" = RepeatStart;
      //|  ")" = GroupEnd; "]" = OptionEnd; "}" = RepeatEnd; "?" = Questionmark;
      //|  "-=" = MinusEqual; "??" = DoubleQuestionmark  /* see below "*=" = StarEqual ... */ 

      //| GrammlatorGrammar=
      //|     OptionalGrammlatorSettings, 
      //|     OptionalDeclarationOfTerminalSymbols,
      //|     GrammarRuleList,
      //|     TerminatorAtEndOfGrammar

      //| TerminatorAtEndOfGrammar=
      //|      NumberSign
      private void TerminatorAtEndOfGrammar()
          => P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Information,
              "Found end of grammar."
              );
      //|
      //| OptionalGrammlatorSettings=
      //|      /* empty */
      //|    | OptionalGrammlatorSettings, GrammlatorSetting

      //| GrammlatorSetting=
      //|    Name(Int32 nameIndex), ":", LexerString(Int32 stringIndex), ";"?
      private void SetGrammlatorStringSetting(Int32 nameIndex, Int32 stringIndex)
      {
         String Name = GlobalVariables.GetStringOfIndex(nameIndex);
         String NameToLower = Name.ToLower();
         String NewValue = GlobalVariables.GetStringOfIndex(stringIndex);
         NewValue = NewValue[1..^1]; // remove leading and trailing "

         Setting? s = GlobalVariables.VisibleSettings.Find(s => s.NameToLower == NameToLower);
         if (s == null)
         {
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error, $"Unknow grammlator setting \"{Name}\"");
            return;
         }

         switch (s)
         {
         case StringSetting ss:
            ss.Value = NewValue;
            break;
         case BooleanSetting bs:
            bs.Value = NewValue == "true";
            if (NewValue!="true" && NewValue!="false")
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                  $"\"{NewValue}\" is neither \"true\" nor \"false\"");
            break;
         case Int32Setting i32s:
            if (Int32.TryParse(NewValue, out Int32 newIntValue))
               i32s.Value = newIntValue;
            else
            {
               i32s.Reset();
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error, $"\"{NewValue}\" is not a number");
               return;

            }
            break;
         }
         return;         
      }

      //| OptionalDeclarationOfTerminalSymbols=
      //|      /* empty */
      //|    | TerminalSymbolsList, SemikolonOrEnum
      private void CompareTerminalDeclarationsWithEnum()
      {
         if (Enumlist.Count == 0)
            return;
         if (Enumlist.Count != SymbolDictionary.Count)
         {
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                $"The number of elements in enum \"{GlobalVariables.GetStringOfIndex(EnumNameStringIndex)}\" differs from the number of terminal symbols."
                );
         }
         Int32 enumIndex = -1;
         foreach (Int32 dictNameIndex in SymbolDictionary.Keys)
         {
            if (++enumIndex >= Enumlist.Count)
               break;
            if (dictNameIndex != Enumlist[enumIndex])
            {
               String name = GlobalVariables.GetStringOfIndex(dictNameIndex);
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                   $"The name \"{name}\" in the terminal definition and the corresponding name \"{GlobalVariables.GetStringOfIndex(Enumlist[enumIndex])}\""
                   + " in the enum are different.");
               // break; // no break: test for more errors in the enum
            }
         }
         Enumlist.Clear();
      }

      //|  TerminalSymbolsList=
      //|     TerminalSymbol
      //|     | TerminalSymbolsList, "|", TerminalSymbol

      //|  TerminalSymbol=
      //|      "Name(Attributes)"(Int32 nameIndex, Int32 NumberOfAttributes), OptionalWeight(Int32 Weight)
      private void TerminalSymbol(Int32 nameIndex, Int32 NumberOfAttributes, Int32 Weight)
          => TerminalSymbolDeclaration(nameIndex, NumberOfAttributes, Weight);

      //|  OptionalWeight(Int32 weight)=
      //|      /* empty */
      private static void DefaultWeightOne(out Int32 weight) => weight = 20;
      //|     | "%", Number(Int32 weight)

      //|  ExtendedName (Int32 stringIndex)=
      //|       Name(Int32 stringIndex)
      //|     | LexerString(Int32 stringIndex)

      //|  GrammarRuleList=
      //|     FirstGrammarRule
      //|     | GrammarRuleList, GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)
      private void EndOfGrammarRuleRecognized(Symbol SymbolAtLeftSide)
      {
         EvaluateGrammarRule(SymbolAtLeftSide);
         // The following assertion will fail if there are errors in the input 
         //  "... has been used at its first occurence with a different number of attributes
         // Debug.Assert(ListOfAttributesOfGrammarRule.Count == SymbolAtLeftSide.NumberOfAttributes);
         ListOfAttributesOfGrammarRule.Clear();
         AttributeCounter = 0;
      }

      //|  FirstGrammarRule=
      //|     "*=", outerDefinitions
      private void FirstGrammarRuleRecognized()
      {
         EvaluateDefinitionsOftheStartsymbol();

         // OptimizeTrivialDefinitions has been set false for the first grammar rule: reset to previous value
         OptimizeTrivialDefinitions = OptimizeTrivialDefinitionsBackup;
      }

      //| NoEnum=
      private void EnumOmitted()
      {
         Enumlist.Clear();
         EnumNameStringIndex = -1;
      }

      //| SemikolonOrEnum= // at end of terminal definitions
      //|    ";", NoEnum
      //|    | optionalEnum

      //| optionalEnum=
      //|       CSharpStart,  CSEnumDeclaration, CSharpEnd
      //|    |  CSharpStart, CSharpEnd, NoEnum

      //|  "*="=
      //|     StarEqual
      private void StartOfFirstGrammarRule()
      {
         /* If no terminal symbol has been defined then define a default terminal symbol.
          * This will be never used. It is necessary that grammlator can distinguish
          * between "no terminal symbol" and "all terminal symbols".
          */
         if (SymbolDictionary.Count == 0)
            TerminalSymbolDeclaration(
               GlobalVariables.GetIndexOfString("*".AsMemory()),
               numberOfAttributes: 0,
               weight: 0);
         GlobalVariables.NumberOfTerminalSymbols = SymbolDictionary.Count;
         GlobalVariables.DefineArrayTerminalSymbolByIndex(SymbolDictionary);
         OptimizeTrivialDefinitionsBackup = OptimizeTrivialDefinitions;
         OptimizeTrivialDefinitions = false; // must be disabled while evaluating the definitions of the startsymbol
         SymbolDictionary.Add(
            GlobalVariables.GetIndexOfString(GlobalVariables.Startsymbol.Identifier),
            GlobalVariables.Startsymbol);
      }

      private Boolean OptimizeTrivialDefinitionsBackup;

      //|  GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)=
      //|      outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "=", outerDefinitions
      //|     | outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "-=" ListOfExcludedTerminalSymbols, ";"
      private void EndOfListOfExcludedTerminalSymbols() => EvaluateExcludedTerminalSymbols(ExcludedTerminalSymbols!);

      //| ListOfExcludedTerminalSymbols=
      //|      Name(Int32 stringIndex)
      private void FirstExcludedTerminalSymbol(Int32 stringIndex)
      {
         if (ExcludedTerminalSymbols == null || ExcludedTerminalSymbols.Length != GlobalVariables.NumberOfTerminalSymbols)
            ExcludedTerminalSymbols = new BitArray(GlobalVariables.NumberOfTerminalSymbols);
         ExcludedTerminalSymbols.SetAll(false);
         OneMoreExcludedTerminalSymbol(stringIndex);
      }

      private BitArray? ExcludedTerminalSymbols;

      //|     | ListOfExcludedTerminalSymbols, "|", Name(Int32 nameIndex)
      private void OneMoreExcludedTerminalSymbol(Int32 nameIndex)
      {
         String name = GlobalVariables.GetStringOfIndex(nameIndex);
         if (!SymbolDictionary.TryGetValue(nameIndex, out Symbol? Symbol))
         {
            P1OutputMessageAndLexerPosition(
                MessageTypeOrDestinationEnum.Error,
                $"{name} is unknown");
            return; // ignore this name
         }
         if (!Symbol.IsTerminalSymbol)
         {
            P1OutputMessageAndLexerPosition(
                MessageTypeOrDestinationEnum.Error,
                $"{name} is not the name of a terminal symbol");
            return; // ignore this name
         }
         ExcludedTerminalSymbols!.Set(Symbol.SymbolNumber, true);
      }

      //| NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= /* Usage see siehe NestedElement */
      //|      NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions
      //|     | NestedDefinitions
      private void NestedGrammarRuleWithEmptyLeftside(out Symbol SymbolAtLeftSide, out Int32 NumberOfAttributes)
      {
         SymbolAtLeftSide = NonterminalSymbolDefinition(MakeNewNameStringIndex("Local"), 0);
         NumberOfAttributes = 0;
      }

      //| NestedDefinitions= // never ends with ";"
      //|      EndOfDefinition // empty sequence as single definition
      //|    | EndOfDefinition, "|", NestedDefinitionList // empty sequence only as first definition
      //|    | NestedDefinitionList // no empty sequence in definitions

      //| NestedDefinitionList=  
      //|    Definition
      //|    | NestedDefinitionList, "|", Definition

      //| Definition=
      //|    SequenceOfElements, EndOfDefinition

      //| EndOfDefinition=
      //|      EndOfDefinitionWithSemantics 
      //|    | EndOfDefinitionWithoutSemantics

      //| outerDefinitions=            
      //|      EndOfDefinitionWithoutSemantics, ";"  // empty sequence only in first definition
      //|    | EndOfDefinitionWithSemantics, ";"?    // empty sequence
      //|    | EndOfDefinition, "|", outerDefinitionList // empty sequence only as first definition
      //|    | outerDefinitionList       

      //| outerDefinitionList=  // if no semantics then must end with terminator symbol
      //|    SequenceOfElements, EndOfDefinition, "|", outerDefinitionList
      //|    | outerLastDefinitionOfSequence

      //| outerLastDefinitionOfSequence=
      //|    SequenceOfElements, EndOfDefinitionWithoutSemantics, ";"
      //|  | SequenceOfElements, EndOfDefinitionWithSemantics, ";"?  // optional ";" after semantics / codelines

      //| EndOfDefinitionWithoutSemantics=
      //|    /* empty */
      private void EndOfDefinitionWithoutSemanticsRecognized()
      {
         EvaluateDefinition(constantPriority: 0, priorityFunction: null, semanticMethod: null, OptimizeTrivialDefinitions);
         AttributeCounter = AttributeNumberAtStartOfDefinition;
      }

      //| EndOfDefinitionWithSemantics=
      //|    PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority)
      private void EndOfDefinitionWithPriorityRecognized(Int32 constPriority, IntMethodClass? dynPriority)
      {
         EvaluateDefinition(constantPriority: constPriority, priorityFunction: dynPriority, semanticMethod: null, OptimizeTrivialDefinitions);
         AttributeCounter = AttributeNumberAtStartOfDefinition;
      }

      //|    | SemanticAction(VoidMethodClass method)
      private void EndOfDefinitionWithMethodRecognized(VoidMethodClass? method)
      {
         EvaluateDefinition(constantPriority: 0, priorityFunction: null, semanticMethod: method, OptimizeTrivialDefinitions);
         AttributeCounter = AttributeNumberAtStartOfDefinition;
      }

      //|    | PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority), SemanticAction(VoidMethodClass method)
      private void EndOfDefinitionWithPriorityAndMethodRecognized(Int32 constPriority, IntMethodClass? dynPriority, VoidMethodClass? method)
      {
         EvaluateDefinition(constPriority, dynPriority, method, OptimizeTrivialDefinitions);
         AttributeCounter = AttributeNumberAtStartOfDefinition;
      }

      //| PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)=
      //|      "??", signedNumber(Int32 constPriority), "??"
      private static void ConstantPriorityGiven(out IntMethodClass? dynamicPriority) => dynamicPriority = null;

      //|    | "??",  // ???? "Name(Attributes)"(String Name, Int32 NumberOfAttributes), 
      //|        CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, ["??"]
      private static void DynamicPriorityRecognized(out Int32 constPriority, out IntMethodClass? dynamicPriority, IntMethodClass? intMethod)
      {
         constPriority = 0;
         dynamicPriority = intMethod;
      }

      //|  signedNumber(Int32 value)=
      //|     Number (Int32 value)
      //|     | "+", Number (Int32 value)
      //|     | "-", Number (Int32 value)
      private static void NegateNumber(ref Int32 value) => value = -value;

      //|  SemanticAction(VoidMethodClass method)=
      //|       CSharpStart, CSvoidMethod(VoidMethodClass method), CSharpEnd
      //|     | CSharpStart, CSharpEnd
      private static void EmptySemanticAction(out VoidMethodClass? method) => method = null;

      //|  "Name(Attributes)" (Int32 stringIndex, Int32 NumberOfAttributes)=
      //|       ExtendedName(Int32 stringIndex), "(Attributes)"(Int32 NumberOfAttributes)
      //|     | ExtendedName(Int32 stringIndex) ??-10?? // low priority: if "(" follows then assume that attributes follow
      private static void NameWithoutAttributes(out Int32 NumberOfAttributes) => NumberOfAttributes = 0;

      //|  outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)=
      //|     "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes)
      private void LeftSideOfOuterProduction(out Symbol SymbolAtLeftSide, Int32 stringIndex, Int32 NumberOfAttributes)
          => SymbolAtLeftSide = NonterminalSymbolDefinition(stringIndex, NumberOfAttributes);

      //|  NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)=
      //|        "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes), "="
      private void LeftSideOfNestedProduction(out Symbol SymbolAtLeftSide, Int32 stringIndex, Int32 NumberOfAttributes)
          => SymbolAtLeftSide = NonterminalSymbolDefinition(stringIndex, NumberOfAttributes); // same as LeftSideOfOuterProduction();

      //| SequenceOfElements=
      //|      Element
      //|    | SequenceOfElements, ","?, Element  // allow to omit the "," (but not between an ExtendedName without attributes and a grouped definition)

      //| Element=
      //|    RepeatedElement(Symbol Symbol)
      private void ElementVariantRecognized(Symbol Symbol)
      {
         NumberOfElements++;
         ActualListOfElements.Add(Symbol);
      }

      //| RepeatedElement(Symbol Symbol)=
      //|       SimpleElement(Symbol Symbol)
      //|    | "{", NestedElement(Symbol Symbol), "}"
      private void RepeatGroupRecognized(ref Symbol Symbol)
          => Symbol = MakeGrammarRule(Symbol, TypeOfGrammarRule.repeat0lr);

      //|    | "[", NestedElement(Symbol Symbol), "]"
      private void OptionGroupRecognized(ref Symbol Symbol)
          => Symbol = MakeGrammarRule(Symbol, TypeOfGrammarRule.optional);
      //|    | SimpleElement(Symbol Symbol), "?"
      private void OptionalElementRecognized(ref Symbol Symbol)
          => Symbol = MakeGrammarRule(Symbol, TypeOfGrammarRule.optional);

      //|    | SimpleElement(Symbol Symbol), "+"
      private void Repeat1lrRecognized(ref Symbol Symbol)
          => Symbol = MakeGrammarRule(Symbol, TypeOfGrammarRule.repeat1lr);

      //|    | SimpleElement(Symbol Symbol), "+", "+"
      private void Repeat1rrRecognized(ref Symbol Symbol)
          => Symbol = MakeGrammarRule(Symbol, TypeOfGrammarRule.repeat1rr);

      //|    | SimpleElement(Symbol Symbol), "*"
      private void Repeat0lrRecognized(ref Symbol Symbol)
          => Symbol = MakeGrammarRule(Symbol, TypeOfGrammarRule.repeat0lr);

      //|    | SimpleElement(Symbol Symbol), "*", "*"
      private void Repeat0rrRecognized(ref Symbol Symbol)
          => Symbol = MakeGrammarRule(Symbol, TypeOfGrammarRule.repeat0rr);

      //| SimpleElement(Symbol Symbol)=
      //|      "(", NestedElement(Symbol Symbol), ")"
      //|    | "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes)
      private void FoundSymbolnameInRightSide(out Symbol Symbol, Int32 stringIndex, Int32 NumberOfAttributes)
          => Symbol = EvaluateSymbolnameFoundInRightSide(stringIndex, NumberOfAttributes);

      //|  NestedElement(Symbol Symbol)=
      //|       SaveVariables(Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions, Int32 SavedNumberOfTrivialDefinitions,
      //|               Int32 SavedNumberOfElements, Int32 SavedNumberOfSymbolAttributes)
      //|       , NestedGrammarRule(Symbol NestedSymbol, Int32 NumberOfAttributes)
      private void EndOfNestedGrammarRuleRecognized(out Symbol Symbol, Symbol NestedSymbol,
          Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions, Int32 SavedNumberOfTrivialDefinitions, Int32 SavedNumberOfElements, Int32 SavedNumberOfSymbolAttributes
          )
      {
         // a) evaluate end of rule
         EvaluateGrammarRule(NestedSymbol);
         UseNonterminalInRightSide(NestedSymbol);
         AttributeCounter = AttributeNumberAtStartOfDefinition + NestedSymbol.NumberOfAttributes;

         // b) restore variables
         AttributeNumberAtStartOfDefinition = SavedAttributeNumberAtStartOfDefinition;
         NumberOfNontrivialDefinitions = SavedNumberOfDefinitions;
         NumberOfTrivalDefinitions = SavedNumberOfTrivialDefinitions;
         NumberOfElements = SavedNumberOfElements; // will be incremented in Element
         NumberOfLastAttributeOfLeftSide = SavedNumberOfSymbolAttributes;

         NestingLevel--; // end of nested production
         Symbol = NestedSymbol;
      }

      //| SaveVariables(Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions, Int32 SavedNumberOfTrivialDefinitions,
      //|               Int32 SavedNumberOfElements, Int32 SavedNumberOfSymbolAttributes)=
      private void SaveVariablesToAttributes(
          out Int32 SavedAttributeNumberAtStartOfDefinition, out Int32 SavedNumberOfDefinitions, out Int32 SavedNumberOfTrivialDefinitions,
          out Int32 SavedNumberOfElements, out Int32 SavedNumberOfSymbolAttributes
          )
      {
         // Found start of a nested rule: save and then reset variables
         SavedAttributeNumberAtStartOfDefinition = AttributeNumberAtStartOfDefinition;
         AttributeNumberAtStartOfDefinition = AttributeCounter;

         SavedNumberOfDefinitions = NumberOfNontrivialDefinitions;
         NumberOfNontrivialDefinitions = 0;

         SavedNumberOfTrivialDefinitions = NumberOfTrivalDefinitions;
         NumberOfTrivalDefinitions = 0;

         SavedNumberOfElements = NumberOfElements;
         NumberOfElements = 0;

         SavedNumberOfSymbolAttributes = NumberOfLastAttributeOfLeftSide;
         NumberOfLastAttributeOfLeftSide = AttributeCounter;

         NestingLevel++; // start of nested production
      }

      //|  Attribut(Int32 number)=
      //|    Name(Int32 typeStringIndex), Name(Int32 nameStringIndex)
      private void AttributeTypeAndName(out Int32 number, Int32 typeStringIndex, Int32 nameStringIndex)
      {
         AttributeCounter++;
         PushAttributeToListOfAttributesOfGrammarRule(typeStringIndex, nameStringIndex);
         number = AttributeCounter;
      }

      //|  "Attributes)"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)=
      //|     Attribut(Int32 numberBeforeGroup), Comma, "Attributes)"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup) /* Rekursion */
      private static void AnotherAttributeOfGroup(out Int32 numberOfAttributesOfGroup, out Int32 smallestNumber, Int32 numberBeforeGroup, Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup)
      {
         if (numberBeforeGroup != smallestNumberOfRightGroup - 1)
         {
            // should not happen because each attributes number is set correctly  or checked immediately after an attribute is recognized
            Debug.Fail("The numbers of the attributes are not successive (Error in grammlator?)");
         }
         numberOfAttributesOfGroup = numberOfAttributesOfRightGroup + 1;
         smallestNumber = numberBeforeGroup;
      }

      //|     | Attribut(Int32 number), ")" /* Ende der Rekursion */
      private static void FirstAttributeOfGroup(out Int32 numberOfAttributesOfGroup, out Int32 smallestNumber, Int32 number)
      {
         smallestNumber = number;
         numberOfAttributesOfGroup = 1;
      }

      //| "(Attributes)" (Int32 numberOfAttributes)= /* erhöht Attribute */
      //|     "(", ")"
      private static void EmptyListOfAttributes(out Int32 numberOfAttributes) => numberOfAttributes = 0;

      //|     | "(", "Attributes)" (Int32 numberOfAttributes, Int32 smallestNumber)

      //| /* ------------------------------- simplified CSharp grammar ------------------- */
      //|
      //| CSvoidMethod(VoidMethodClass voidMethod)=
      //|    CSMethodProperties(MethodClass method), "(", eFormalParameters, ")"
      private void CSvoidMethodRecognized(out VoidMethodClass? voidMethod, MethodClass? method)
      {
         Debug.Assert(method != null);
         if (method is VoidMethodClass vMethod)
         {
            voidMethod = vMethod;
         }
         else
         {
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error, $"Expected a void method, got {method.GetType()}");
            voidMethod = null;
         }

         method.MethodParameters = LastFormalParameterList.ToArray();
         LastFormalParameterList.Clear();
         Lexer.SkipToEndOfCSLines();
      }

      //| /* The same definition as for CSvoidMethod is used for CSintMethod, but different semantics */
      //| CSintMethod(IntMethodClass intMethod)=
      //|    CSMethodProperties(MethodClass method), "(", eFormalParameters, ")"
      private void CSintMethodRecognized(out IntMethodClass? intMethod, MethodClass? method)
      {
         if (method is IntMethodClass iMethod)
            intMethod = iMethod;
         else
         {
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
               $"Expected {typeof(IntMethodClass)} method, got {method?.GetType()}");
            intMethod = new IntMethodClass("@error", Lexer.LexerTextPos);
            method = intMethod;
         }

         method.MethodParameters = LastFormalParameterList.ToArray();
         LastFormalParameterList.Clear();
         Lexer.SkipToEndOfCSLines();
      }

      //| CSMethodProperties(MethodClass method)=
      //|    Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex)
      private void MethodTypeAndNameRecognized(out MethodClass method, Int32 methodTypeStringIndex, Int32 methodNameStringIndex)
          => MethodProperties(out method,
             GlobalVariables.GetIndexOfString("".AsMemory()), methodTypeStringIndex, methodNameStringIndex);

      //|    | Name(Int32 modifierStringIndex), Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex) /* AccessModifier (private, public ...), type, methodname */
      private void MethodModifierTypeAndNameRecognized(
          out MethodClass method, Int32 modifierStringIndex, Int32 methodTypeStringIndex, Int32 methodNameStringIndex)
          => MethodProperties(out method, modifierStringIndex, methodTypeStringIndex, methodNameStringIndex);

      //|    | Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex) /* AccessModifier (private, public ...), type, methodname */
      private void MethodModifierTypeAndNameRecognized2(
          out MethodClass method, Int32 modifier1StringIndex, Int32 modifier2StringIndex, Int32 methodTypeStringIndex, Int32 methodNameStringIndex)
          => MethodProperties(out method, modifier1StringIndex, modifier2StringIndex, methodTypeStringIndex, methodNameStringIndex);

      //| eFormalParameters=
      //|    /* empty */
      //|    | formalParameters

      //| formalParameters=
      //|    formalParameter
      //|    | formalParameters, Comma, formalParameter

      //| formalParameter=
      //|    Name(Int32 typeStringIndex), Name(Int32 nameStringIndex) // allow nullable types // TODO ignore if calls, but if record use n generated code
      private void FormalParameterWithTypeAndName(Int32 typeStringIndex, Int32 nameStringIndex)
          => FormalParameter(GlobalVariables.GetIndexOfEmptyString(), typeStringIndex, nameStringIndex);

      //|    | Name(Int32 typeStringIndex), "?", Name(Int32 nameStringIndex) // allow nullable types // TODO ignore if calls, but if record use n generated code
      private void FormalParameterWithNullableTypeAndName(Int32 typeStringIndex, Int32 nameStringIndex)
          => FormalParameter(GlobalVariables.GetIndexOfEmptyString(), typeStringIndex, nameStringIndex);

      //|    | Name(Int32 ParameterModifierOptStringIndex), Name(Int32 typeStringIndex), Name(Int32 nameStringIndex)  /* ref or out, type, identifier */
      private void FormalParameterWithModifierTypeAndName(Int32 ParameterModifierOptStringIndex, Int32 typeStringIndex, Int32 nameStringIndex)
          => FormalParameter(ParameterModifierOptStringIndex, typeStringIndex, nameStringIndex);

      //|    | Name(Int32 ParameterModifierOptStringIndex), Name(Int32 typeStringIndex), "?", Name(Int32 nameStringIndex)  /* ref or out, type, identifier */
      private void FormalParameterWithModifierNullableTypeAndName(Int32 ParameterModifierOptStringIndex, Int32 typeStringIndex, Int32 nameStringIndex)
          => FormalParameter(ParameterModifierOptStringIndex, typeStringIndex, nameStringIndex);

      //| CSEnumDeclaration=
      //|    CSEnumProperties, optionalBaseType, CSEnumMembers
      private void CSEnumRecognized()
      {
         // all elements of the enum have been added to Enumlist by CSEnumMember
         Lexer.SkipToEndOfCSLines(); // allows some unchecked code after the enum
         if (EnumMaxValue > 63)
            GlobalVariables.IsInMethod.Value = "";
      }

      //| CSEnumProperties=
      //|    Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), Name(Int32 enumStringIndex), CSEnumName
      //|    | Name(Int32 modifierStringIndex), Name(Int32 enumStringIndex), CSEnumName
      //|    | Name(Int32 enumStringIndex), CSEnumName

      //| CSEnumName= Name(Int32 nameStringIndex)
      private void EnumNameRecognized(Int32 nameStringIndex) =>
          EnumNameStringIndex = nameStringIndex;

      //| optionalBaseType=
      //|    /* empty */
      //|    | ":", Name(Int32 Ignored);
      //|
      //| CSEnumMembers=
      //|    "{", "}"
      private void NoEnumRecognized()
      {
         ResetEnumDefaults();
      }
      //|    | "{", CSEnumMemberList, "}"

      //| CSEnumMemberList=
      //|    ResetEnumDefaults, CSEnumMember
      //|    | CSEnumMemberList, Comma, CSEnumMember

      //| CSEnumMember=
      //|    Name(Int32 enumElementStringIndex)
      private void EnumElementRecognized(Int32 enumElementStringIndex)
      {
         EnumLastValue++;
         if (EnumLastValue > EnumMaxValue)
            EnumMaxValue = EnumLastValue;
         Enumlist.Add(enumElementStringIndex);
      }

      //|  | Name(Int32 enumElementStringIndex), "=", Number(Int32 enumNumber)
      private void EnumElementWithNumberRecognized(Int32 enumElementStringIndex, Int32 enumNumber)
      {
         EnumLastValue = enumNumber;
         if (EnumLastValue > EnumMaxValue)
            EnumMaxValue = EnumLastValue;
         Enumlist.Add(enumElementStringIndex);
      }

      //| ResetEnumDefaults= /*empty*/
      private void ResetEnumDefaults()
      {
         EnumLastValue = -1;
         EnumMaxValue = Int64.MinValue;
         Enumlist.Clear();
      }
      //|
      //|   /* end of grammar */
      #endregion grammar
      /* ************************ code written by programmer ******************** */

      private Boolean ErrorHandler(Int32 stateNumber, String stateDescription, LexerResult symbol)
      {
         String nl = Environment.NewLine;

         if (Lexer.PeekSymbol() == LexerResult.NumberSign)
         {
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                $"Grammar analysis error:{nl}input symbol \"{symbol.MyToString()}\" not allowed in state {stateNumber}{nl}{stateDescription}{nl}");
            return false; // do not skip # because it may be #endregion grammar
         }

         var aCountBeforeAccept = _a.Count;

         Lexer.AcceptSymbol(); // accept the wrong symbol to make its position available in Lexer.Lex1TextPos and to discard it

         P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
             $"Grammar analysis error:{nl}input symbol \"{symbol.MyToString()}\" ignored: not allowed in state {stateNumber}{nl}{stateDescription}{nl}");

         if (symbol == LexerResult.CSharpStart)
         {
            Lexer.SkipToEndOfCSLines(); // if C#Start no allowed then skip the totral C# code
            if (symbol == LexerResult.CSharpStart)
               Lexer.AcceptSymbol();
         }
         _a.Free(_a.Count - aCountBeforeAccept);  // discard the attributes of the discarded terminal symbol
         return true; // true: continue analysis (goto state ...), else "goto EndWithError..."
                      // TODO Errorhandling: design and implement a concept to insert a missing character (e.g. if state accepts only a single terminal symbol)
      }

      private void AnalyzeGrammlatorGrammar()
      {
         // Declare local variables used by grammlator generated code
         LexerResult ParserInput;
         /* ************************ end of code written by programmer ******************** */
         #region grammlator generated Wed, 23 Sep 2020 18:38:54 GMT (grammlator, File version 2020.09.14.0 23.09.2020 18:37:31)
         Int32 StateStackInitialCount = _s.Count;
         Int32 AttributeStackInitialCount = _a.Count;
         const Int64 _DefiningSymbol = 2L << (Int32)LexerResult.DefiningSymbol;
         const Int64 _Colon = 2L << (Int32)LexerResult.Colon;
         const Int64 _Percent = 2L << (Int32)LexerResult.Percent;
         const Int64 _CSharpEnd = 2L << (Int32)LexerResult.CSharpEnd;
         const Int64 _Error = 2L << (Int32)LexerResult.Error;
         const Int64 _Minus = 2L << (Int32)LexerResult.Minus;
         const Int64 _Number = 2L << (Int32)LexerResult.Number;
         const Int64 _StarEqual = 2L << (Int32)LexerResult.StarEqual;
         const Int64 _MinusEqual = 2L << (Int32)LexerResult.MinusEqual;
         const Int64 _NumberSign = 2L << (Int32)LexerResult.NumberSign;
         const Int64 _GroupStart = 2L << (Int32)LexerResult.GroupStart;
         const Int64 _CSharpStart = 2L << (Int32)LexerResult.CSharpStart;
         const Int64 _Name = 2L << (Int32)LexerResult.Name;
         const Int64 _LexerString = 2L << (Int32)LexerResult.LexerString;
         const Int64 _DefinitionSeparatorSymbol = 2L << (Int32)LexerResult.DefinitionSeparatorSymbol;
         const Int64 _TerminatorSymbol = 2L << (Int32)LexerResult.TerminatorSymbol;
         Boolean _1In(Int64 flags) => ((2L << (Int32)ParserInput) & flags) != 0;

      State2:
         const String StateDescription2 =
              "GrammlatorGrammar= OptionalGrammlatorSettings, ►OptionalDeclarationOfTerminalSymbols, GrammarRuleList, TerminatorAtEndOfGrammar;\r\n"
            + "OptionalGrammlatorSettings= OptionalGrammlatorSettings, ►GrammlatorSetting;";
         _s.Push(0);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            goto State90;
         }
         if (ParserInput == LexerResult.LexerString)
            goto AcceptState85;
         if (ParserInput != LexerResult.StarEqual)
         {
            if (ErrorHandler(2, StateDescription2, ParserInput))
            {
               _s.Pop();
               goto State2;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.StarEqual);
      State23:
         const String StateDescription23 =
              "GrammlatorGrammar= OptionalGrammlatorSettings, OptionalDeclarationOfTerminalSymbols, ►GrammarRuleList, TerminatorAtEndOfGrammar;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.StarEqual)
         {
            if (ErrorHandler(23, StateDescription23, ParserInput))
               goto State23;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.StarEqual);
         Lexer.AcceptSymbol();
         // Reduce22:
         /* "*="= StarEqual;◄ */

         StartOfFirstGrammarRule();

      State24:
         const String StateDescription24 =
              "FirstGrammarRule= \"*=\", ►outerDefinitions;";
         _s.Push(2);
         ParserInput = Lexer.PeekSymbol();
         switch (ParserInput)
         {
         // <= LexerResult.NumberSign: goto HandleError24 // see end of switch
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
            goto HandleError24;
         case LexerResult.GroupStart:
            goto AcceptState77;
         case LexerResult.OptionStart:
            goto AcceptState75;
         case LexerResult.RepeatStart:
            goto AcceptState62;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState51;
         case LexerResult.CSharpStart:
            goto AcceptState37;
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto AcceptState85;
            // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce23 // see end of switch
         } // end of switch
         if (ParserInput <= LexerResult.NumberSign)
            goto HandleError24;
         Debug.Assert(ParserInput >= LexerResult.DefinitionSeparatorSymbol);

      Reduce23:
         /* EndOfDefinitionWithoutSemantics= ;◄ */

         EndOfDefinitionWithoutSemanticsRecognized();

         // State29:
         /* outerDefinitions= EndOfDefinitionWithoutSemantics, ►";";
          * outerDefinitions= EndOfDefinition, ►"|", outerDefinitionList; */
         if (ParserInput <= LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState30;
         Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
      AcceptBranch6:
         Lexer.AcceptSymbol();
      Branch6:
         if (_s.Peek() == 2)
            goto Reduce25;
         Reduce82:
         /* sAdjust: -1
          * GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "=", outerDefinitions;◄ */
         _s.Pop();
      Reduce78:
         /* sAdjust: -1, aAdjust: -2
          * GrammarRuleList= GrammarRuleList, GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes);◄ */
         _s.Pop();

         EndOfGrammarRuleRecognized(
            SymbolAtLeftSide: _a.PeekRef(-1)._Symbol
            );

         _a.Free(2);
      State79:
         const String StateDescription79 =
              "GrammlatorGrammar= OptionalGrammlatorSettings, OptionalDeclarationOfTerminalSymbols, GrammarRuleList, ►TerminatorAtEndOfGrammar;\r\n"
            + "GrammarRuleList= GrammarRuleList, ►GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes);";
         _s.Push(10);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.NumberSign)
         {
            Lexer.AcceptSymbol();
            // Reduce75:
            /* sAdjust: -2
             * TerminatorAtEndOfGrammar= NumberSign;◄
             * then: GrammlatorGrammar= OptionalGrammlatorSettings, OptionalDeclarationOfTerminalSymbols, GrammarRuleList, TerminatorAtEndOfGrammar;◄ */
            _s.Discard(2);

            TerminatorAtEndOfGrammar();

            goto EndOfGeneratedCode;
         }
         if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(79, StateDescription79, ParserInput))
            {
               _s.Pop();
               goto State79;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
      AcceptState85:
         Lexer.AcceptSymbol();
      State85:
         const String StateDescription85 =
              "\"Name(Attributes)\"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex), ►\"(Attributes)\"(Int32 NumberOfAttributes);\r\n"
            + "\"Name(Attributes)\"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex)●;";
         // *Push(0)
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.GroupStart)
         {
            Lexer.AcceptSymbol();
            // PushState5:
            _s.Push(0);
            goto State86;
         }
         if (_1In(_Colon + _CSharpEnd + _Error + _Minus + _Number + _StarEqual + _NumberSign))
         {
            if (ErrorHandler(85, StateDescription85, ParserInput))
               goto State85;
            // PushState6:
            _s.Push(0);
            goto EndWithError;
         }
         Debug.Assert(!_1In(_Colon + _CSharpEnd + _Error + _Minus + _Number + _StarEqual + _NumberSign + _GroupStart));
         // Reduce83:
         /* aAdjust: 1
          * "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex);◄ */
         _a.Allocate();

         NameWithoutAttributes(
            NumberOfAttributes: out _a.PeekRef(0)._Int32
            );

      Branch15:
         switch (_s.Peek())
         {
         case 0:
         case 1:
            goto State3;
         case 6:
            goto State73;
         case 10:
         // Reduce77:
         {
            /* outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes);◄ */

            LeftSideOfOuterProduction(
               SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
               stringIndex: _a.PeekClear(-1)._Int32,
               NumberOfAttributes: _a.PeekRef(0)._Int32
               );

            goto State80;
         }
         /*case 2: case 3: case 4: case 5: case 7: case 8: case 9: case 11:
         default: break; */
         }
      Reduce24:
         /* aAdjust: -1
          * SimpleElement(Symbol Symbol)= "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes);◄ */

         FoundSymbolnameInRightSide(
            Symbol: out _a.PeekRef(-1)._Symbol,
            stringIndex: _a.PeekClear(-1)._Int32,
            NumberOfAttributes: _a.PeekRef(0)._Int32
            );

         _a.Free();
      State25:
         const String StateDescription25 =
              "Element= RepeatedElement(Symbol Symbol)●;\r\n"
            + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), ►\"?\";\r\n"
            + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), ►\"+\";\r\n"
            + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), ►\"+\", \"+\";\r\n"
            + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), ►\"*\";\r\n"
            + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), ►\"*\", \"*\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Questionmark)
         {
            Lexer.AcceptSymbol();
            // Reduce29:
            /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "?";◄ */

            OptionalElementRecognized(
               Symbol: ref _a.PeekRef(0)._Symbol
               );

            goto Branch4;
         }
         if (ParserInput == LexerResult.Asterisk)
         {
            Lexer.AcceptSymbol();
            goto State27;
         }
         if (ParserInput == LexerResult.Plus)
         {
            Lexer.AcceptSymbol();
            goto State26;
         }
         if (ParserInput <= LexerResult.MinusEqual
            || ParserInput == LexerResult.NumberSign)
         {
            if (ErrorHandler(25, StateDescription25, ParserInput))
               goto State25;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Comma
            || ParserInput >= LexerResult.GroupStart);
         // Reduce28:
         /* aAdjust: -1
          * Element= RepeatedElement(Symbol Symbol);◄ */

         ElementVariantRecognized(
            Symbol: _a.PeekRef(0)._Symbol
            );

         _a.Free();
      Branch3:
         switch (_s.Peek())
         {
         case 5:
         // Reduce41:
         {
            /* sAdjust: -2
             * SequenceOfElements= SequenceOfElements, ","?, Element;◄ */
            _s.Discard(2);
            goto Branch3;
         }
         case 6:
         case 7:
         case 8:
         case 9:
            goto State68;
            /*case 2: case 3: case 4: case 11:
            default: break; */
         }
      State32:
         const String StateDescription32 =
              "SequenceOfElements= SequenceOfElements, ►\",\"?, Element;\r\n"
            + "outerDefinitionList= SequenceOfElements, ►EndOfDefinition, \"|\", outerDefinitionList;\r\n"
            + "outerLastDefinitionOfSequence= SequenceOfElements, ►EndOfDefinitionWithoutSemantics, \";\";\r\n"
            + "outerLastDefinitionOfSequence= SequenceOfElements, ►EndOfDefinitionWithSemantics, \";\"?;";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         switch (ParserInput)
         {
         // <= LexerResult.Plus: goto HandleError32 // see end of switch
         case LexerResult.NumberSign:
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
            goto HandleError32;
         case LexerResult.Comma:
            goto AcceptState36;
         case LexerResult.GroupStart:
         case LexerResult.OptionStart:
         case LexerResult.RepeatStart:
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto State36;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState51;
         case LexerResult.CSharpStart:
            goto AcceptState37;
            // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce37 // see end of switch
         } // end of switch
         if (ParserInput <= LexerResult.Plus)
            goto HandleError32;
         Debug.Assert(ParserInput >= LexerResult.DefinitionSeparatorSymbol);

         // Reduce37:
         /* EndOfDefinitionWithoutSemantics= ;◄ */

         EndOfDefinitionWithoutSemanticsRecognized();

         // State33:
         /* outerDefinitionList= SequenceOfElements, EndOfDefinition, ►"|", outerDefinitionList;
          * outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithoutSemantics, ►";"; */
         if (ParserInput <= LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState34;
         Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
      AcceptReduce39:
         Lexer.AcceptSymbol();
      Reduce39:
         /* sAdjust: -1
          * outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithoutSemantics, ";";◄
          * or: outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithSemantics, ";"?;◄ */
         _s.Pop();
         // Branch7:
         switch (_s.Peek())
         {
         case 3:
            goto Reduce36;
         case 4:
            goto Reduce40;
         case 11:
            goto Reduce82;
            /*case 2:
            default: break; */
         }
      Reduce25:
         /* sAdjust: -1
          * FirstGrammarRule= "*=", outerDefinitions;◄ */
         _s.Pop();

         FirstGrammarRuleRecognized();

         goto State79;

      AcceptReduce6:
         Lexer.AcceptSymbol();
         // Reduce6:
         /* NoEnum= ;◄ */

         EnumOmitted();

      Reduce5:
         /* OptionalDeclarationOfTerminalSymbols= TerminalSymbolsList, SemikolonOrEnum;◄ */

         CompareTerminalDeclarationsWithEnum();

         goto State23;

      Reduce8:
         /* CSEnumDeclaration= CSEnumProperties, optionalBaseType, CSEnumMembers;◄ */

         CSEnumRecognized();

      State18:
         const String StateDescription18 =
              "optionalEnum= CSharpStart, CSEnumDeclaration, ►CSharpEnd;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.CSharpEnd)
         {
            if (ErrorHandler(18, StateDescription18, ParserInput))
               goto State18;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpEnd);
         Lexer.AcceptSymbol();
         goto Reduce5;

      Reduce26:
         /* aAdjust: -1
          * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ */

         EndOfDefinitionWithMethodRecognized(
            method: _a.PeekRef(0)._VoidMethodClass
            );

         _a.Free();
      State31:
         const String StateDescription31 =
              "outerDefinitions= EndOfDefinitionWithSemantics, ►\";\"?;\r\n"
            + "outerDefinitions= EndOfDefinition, ►\"|\", outerDefinitionList;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState30;
         if (ParserInput >= LexerResult.TerminatorSymbol)
            goto AcceptBranch6;
         if (ParserInput != LexerResult.NumberSign
            && ParserInput < LexerResult.Name)
         {
            if (ErrorHandler(31, StateDescription31, ParserInput))
               goto State31;
            goto EndWithError;
         }
         Debug.Assert(_1In(_NumberSign + _Name + _LexerString));
         goto Branch6;

      AcceptReduce44:
         Lexer.AcceptSymbol();
         // Reduce44:
         /* sAdjust: -1
          * CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), "(", eFormalParameters, ")";◄ */
         _s.Pop();

         CSvoidMethodRecognized(
            voidMethod: out _a.PeekRef(0)._VoidMethodClass,
            method: _a.PeekClear(0)._MethodClass
            );

      State47:
         const String StateDescription47 =
              "SemanticAction(VoidMethodClass method)= CSharpStart, CSvoidMethod(VoidMethodClass method), ►CSharpEnd;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.CSharpEnd)
         {
            if (ErrorHandler(47, StateDescription47, ParserInput))
               goto State47;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpEnd);
         Lexer.AcceptSymbol();
         // Reduce50:
         /* sAdjust: -1
          * SemanticAction(VoidMethodClass method)= CSharpStart, CSvoidMethod(VoidMethodClass method), CSharpEnd;◄ */
         _s.Pop();
      Branch9:
         switch (_s.Peek())
         {
         case 1:
         // Reduce38:
         {
            /* aAdjust: -1
             * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ */

            EndOfDefinitionWithMethodRecognized(
               method: _a.PeekRef(0)._VoidMethodClass
               );

            _a.Free();
            goto State35;
         }
         case 2:
         case 11:
            goto Reduce26;
         case 3:
         // Reduce67:
         {
            /* sAdjust: -1, aAdjust: -1
             * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄
             * then: NestedDefinitionList= NestedDefinitionList, "|", Definition;◄
             * or: Definition= SequenceOfElements, EndOfDefinition;◄ */
            _s.Pop();

            EndOfDefinitionWithMethodRecognized(
               method: _a.PeekRef(0)._VoidMethodClass
               );

            _a.Free();
            goto Branch14;
         }
         case 6:
         // Reduce62:
         {
            /* aAdjust: -1
             * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ */

            EndOfDefinitionWithMethodRecognized(
               method: _a.PeekRef(0)._VoidMethodClass
               );

            _a.Free();
            goto State64;
         }
         case 9:
         // Reduce70:
         {
            /* aAdjust: -1
             * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ */

            EndOfDefinitionWithMethodRecognized(
               method: _a.PeekRef(0)._VoidMethodClass
               );

            _a.Free();
            goto State71;
         }
         /*case 0:
         default: break; */
         }
         // Reduce35:
         /* sAdjust: -1, aAdjust: -3
          * EndOfDefinitionWithSemantics= PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority), SemanticAction(VoidMethodClass method);◄ */
         _s.Pop();

         EndOfDefinitionWithPriorityAndMethodRecognized(
            constPriority: _a.PeekRef(-2)._Int32,
            dynPriority: _a.PeekRef(-1)._IntMethodClass,
            method: _a.PeekRef(0)._VoidMethodClass
            );

         _a.Free(3);
      Branch5:
         switch (_s.Peek())
         {
         case 2:
         case 11:
            goto State31;
         case 3:
            goto Reduce65;
         case 6:
            goto State64;
         case 9:
            goto State71;
            /*case 1:
            default: break; */
         }
      State35:
         const String StateDescription35 =
              "outerDefinitionList= SequenceOfElements, EndOfDefinition, ►\"|\", outerDefinitionList;\r\n"
            + "outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithSemantics, ►\";\"?;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState34;
         if (ParserInput >= LexerResult.TerminatorSymbol)
            goto AcceptReduce39;
         if (ParserInput != LexerResult.NumberSign
            && ParserInput < LexerResult.Name)
         {
            if (ErrorHandler(35, StateDescription35, ParserInput))
               goto State35;
            goto EndWithError;
         }
         Debug.Assert(_1In(_NumberSign + _Name + _LexerString));
         goto Reduce39;

      AcceptReduce54:
         Lexer.AcceptSymbol();
         // Reduce54:
         /* sAdjust: -1
          * CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), "(", eFormalParameters, ")";◄ */
         _s.Pop();

         CSintMethodRecognized(
            intMethod: out _a.PeekRef(0)._IntMethodClass,
            method: _a.PeekClear(0)._MethodClass
            );

      State57:
         const String StateDescription57 =
              "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, CSintMethod(IntMethodClass intMethod), ►CSharpEnd, (Local1)?;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.CSharpEnd)
         {
            if (ErrorHandler(57, StateDescription57, ParserInput))
               goto State57;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpEnd);
         Lexer.AcceptSymbol();
      State58:
         const String StateDescription58 =
              "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, ►(Local1)?;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DoubleQuestionmark)
         {
            Lexer.AcceptSymbol();
            goto Reduce55;
         }
         if (ParserInput != LexerResult.NumberSign
            && ParserInput < LexerResult.CSharpStart)
         {
            if (ErrorHandler(58, StateDescription58, ParserInput))
               goto State58;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.NumberSign
            || ParserInput >= LexerResult.CSharpStart);
      Reduce55:
         /* sAdjust: -1, aAdjust: 1
          * PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= "??", CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, (Local1)?;◄ */
         _s.Pop();
         _a.Allocate();

         DynamicPriorityRecognized(
            constPriority: out _a.PeekRef(-1)._Int32,
            dynamicPriority: out _a.PeekRef(0)._IntMethodClass,
            intMethod: _a.PeekClear(-1)._IntMethodClass
            );

      State28:
         const String StateDescription28 =
              "EndOfDefinitionWithSemantics= PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority)●;\r\n"
            + "EndOfDefinitionWithSemantics= PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority), ►SemanticAction(VoidMethodClass method);";
         // *Push(0)
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.CSharpStart)
         {
            Lexer.AcceptSymbol();
            // PushState1:
            _s.Push(0);
            goto State37;
         }
         if (ParserInput != LexerResult.NumberSign
            && ParserInput < LexerResult.GroupEnd)
         {
            if (ErrorHandler(28, StateDescription28, ParserInput))
               goto State28;
            // PushState2:
            _s.Push(0);
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.NumberSign
            || ParserInput >= LexerResult.GroupEnd);
         // Reduce34:
         /* aAdjust: -2
          * EndOfDefinitionWithSemantics= PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority);◄ */

         EndOfDefinitionWithPriorityRecognized(
            constPriority: _a.PeekRef(-1)._Int32,
            dynPriority: _a.PeekRef(0)._IntMethodClass
            );

         _a.Free(2);
         goto Branch5;

      Reduce58:
         /* aAdjust: 5
          * SaveVariables(Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions, Int32 SavedNumberOfTrivialDefinitions, Int32 SavedNumberOfElements, Int32 SavedNumberOfSymbolAttributes)= ;◄ */
         _a.Allocate(5);

         SaveVariablesToAttributes(
            SavedAttributeNumberAtStartOfDefinition: out _a.PeekRef(-4)._Int32,
            SavedNumberOfDefinitions: out _a.PeekRef(-3)._Int32,
            SavedNumberOfTrivialDefinitions: out _a.PeekRef(-2)._Int32,
            SavedNumberOfElements: out _a.PeekRef(-1)._Int32,
            SavedNumberOfSymbolAttributes: out _a.PeekRef(0)._Int32
            );

      State63:
         const String StateDescription63 =
              "NestedElement(Symbol Symbol)= SaveVariables(Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions, Int32 SavedNumberOfTrivialDefinitions, Int32 SavedNumberOfElements, Int32 SavedNumberOfSymbolAttributes), ►NestedGrammarRule(Symbol NestedSymbol, Int32 NumberOfAttributes);";
         _s.Push(6);
         ParserInput = Lexer.PeekSymbol();
         switch (ParserInput)
         {
         // <= LexerResult.NumberSign
         // >= LexerResult.TerminatorSymbol: goto HandleError63 // see end of switch
         case LexerResult.GroupStart:
            goto AcceptState77;
         case LexerResult.OptionStart:
            goto AcceptState75;
         case LexerResult.RepeatStart:
            goto AcceptState62;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState51;
         case LexerResult.CSharpStart:
            goto AcceptState37;
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
         case LexerResult.DefinitionSeparatorSymbol:
         // Reduce59:
         {
            /* EndOfDefinitionWithoutSemantics= ;◄ */

            EndOfDefinitionWithoutSemanticsRecognized();

            goto State64;
         }
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto AcceptState85;
         } // end of switch
         Debug.Assert(ParserInput <= LexerResult.NumberSign || ParserInput >= LexerResult.TerminatorSymbol);

         if (ErrorHandler(63, StateDescription63, ParserInput))
         {
            _s.Pop();
            goto State63;
         };
         goto EndWithError;

      Reduce61:
         /* aAdjust: 2
          * NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions;◄ */
         _a.Allocate(2);

         NestedGrammarRuleWithEmptyLeftside(
            SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
            NumberOfAttributes: out _a.PeekRef(0)._Int32
            );

      Reduce60:
         /* sAdjust: -1, aAdjust: -6
          * NestedElement(Symbol Symbol)= SaveVariables(Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions, Int32 SavedNumberOfTrivialDefinitions, Int32 SavedNumberOfElements, Int32 SavedNumberOfSymbolAttributes), NestedGrammarRule(Symbol NestedSymbol, Int32 NumberOfAttributes);◄ */
         _s.Pop();

         EndOfNestedGrammarRuleRecognized(
            Symbol: out _a.PeekRef(-6)._Symbol,
            NestedSymbol: _a.PeekRef(-1)._Symbol,
            SavedAttributeNumberAtStartOfDefinition: _a.PeekClear(-6)._Int32,
            SavedNumberOfDefinitions: _a.PeekRef(-5)._Int32,
            SavedNumberOfTrivialDefinitions: _a.PeekRef(-4)._Int32,
            SavedNumberOfElements: _a.PeekRef(-3)._Int32,
            SavedNumberOfSymbolAttributes: _a.PeekRef(-2)._Int32
            );

         _a.Free(6);
         // Branch12:
         switch (_s.Peek())
         {
         case 0:
            goto State74;
         case 1:
            goto State76;
            /*case 2:
            default: break; */
         }
      State78:
         const String StateDescription78 =
              "SimpleElement(Symbol Symbol)= \"(\", NestedElement(Symbol Symbol), ►\")\";";
         if (ParserInput > LexerResult.GroupEnd)
         {
            if (ErrorHandler(78, StateDescription78, ParserInput))
               goto State78;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         Lexer.AcceptSymbol();
         // Reduce74:
         /* sAdjust: -1
          * SimpleElement(Symbol Symbol)= "(", NestedElement(Symbol Symbol), ")";◄ */
         _s.Pop();
         goto State25;

      Reduce63:
         /* aAdjust: -1
          * Element= RepeatedElement(Symbol Symbol);◄ */

         ElementVariantRecognized(
            Symbol: _a.PeekRef(0)._Symbol
            );

         _a.Free();
      State68:
         const String StateDescription68 =
              "Definition= SequenceOfElements, ►EndOfDefinition;\r\n"
            + "SequenceOfElements= SequenceOfElements, ►\",\"?, Element;";
         _s.Push(3);
         ParserInput = Lexer.PeekSymbol();
         switch (ParserInput)
         {
         // <= LexerResult.Plus
         // >= LexerResult.TerminatorSymbol: goto HandleError68 // see end of switch
         case LexerResult.NumberSign:
            goto HandleError68;
         case LexerResult.Comma:
            goto AcceptState36;
         case LexerResult.GroupStart:
         case LexerResult.OptionStart:
         case LexerResult.RepeatStart:
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto State36;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState51;
         case LexerResult.CSharpStart:
            goto AcceptState37;
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
         case LexerResult.DefinitionSeparatorSymbol:
         // Reduce66:
         {
            /* sAdjust: -1
             * EndOfDefinitionWithoutSemantics= ;◄
             * then: NestedDefinitionList= NestedDefinitionList, "|", Definition;◄
             * or: Definition= SequenceOfElements, EndOfDefinition;◄ */
            _s.Pop();

            EndOfDefinitionWithoutSemanticsRecognized();

            goto Branch14;
         }
         } // end of switch
         Debug.Assert(ParserInput <= LexerResult.Plus || ParserInput >= LexerResult.TerminatorSymbol);

      HandleError68:
         if (ErrorHandler(68, StateDescription68, ParserInput))
         {
            _s.Pop();
            goto State68;
         };
         goto EndWithError;

      Reduce65:
         /* sAdjust: -1
          * NestedDefinitionList= NestedDefinitionList, "|", Definition;◄
          * or: Definition= SequenceOfElements, EndOfDefinition;◄ */
         _s.Pop();
      Branch14:
         switch (_s.Peek())
         {
         case 6:
            goto State69;
         case 7:
            goto State66;
         case 8:
            goto Reduce65;
            /*case 9:
            default: break; */
         }
      State72:
         const String StateDescription72 =
              "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions●;\r\n"
            + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState67;
         if (ParserInput <= LexerResult.CSharpStart
            || ParserInput >= LexerResult.Name)
         {
            if (ErrorHandler(72, StateDescription72, ParserInput))
               goto State72;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
      Reduce69:
         /* sAdjust: -1
          * NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions;◄ */
         _s.Pop();
         goto Reduce60;

      State3:
         const String StateDescription3 =
              "TerminalSymbol= \"Name(Attributes)\"(Int32 nameIndex, Int32 NumberOfAttributes), ►OptionalWeight(Int32 Weight);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Percent)
         {
            Lexer.AcceptSymbol();
            goto State4;
         }
         if (ParserInput != LexerResult.CSharpStart
            && ParserInput < LexerResult.DefinitionSeparatorSymbol)
         {
            if (ErrorHandler(3, StateDescription3, ParserInput))
               goto State3;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpStart
            || ParserInput >= LexerResult.DefinitionSeparatorSymbol);
         // Reduce3:
         /* aAdjust: 1
          * OptionalWeight(Int32 weight)= ;◄ */
         _a.Allocate();

         DefaultWeightOne(
            weight: out _a.PeekRef(0)._Int32
            );

      Reduce4:
         /* aAdjust: -3
          * TerminalSymbol= "Name(Attributes)"(Int32 nameIndex, Int32 NumberOfAttributes), OptionalWeight(Int32 Weight);◄ */

         TerminalSymbol(
            nameIndex: _a.PeekRef(-2)._Int32,
            NumberOfAttributes: _a.PeekRef(-1)._Int32,
            Weight: _a.PeekRef(0)._Int32
            );

         _a.Free(3);
         // Branch1:
         if (_s.Peek() == 0)
            goto State5;
         // Reduce7:
         /* sAdjust: -1
          * TerminalSymbolsList= TerminalSymbolsList, "|", TerminalSymbol;◄ */
         _s.Pop();
      State5:
         const String StateDescription5 =
              "OptionalDeclarationOfTerminalSymbols= TerminalSymbolsList, ►SemikolonOrEnum;\r\n"
            + "TerminalSymbolsList= TerminalSymbolsList, ►\"|\", TerminalSymbol;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
         {
            Lexer.AcceptSymbol();
            goto State7;
         }
         if (ParserInput >= LexerResult.TerminatorSymbol)
            goto AcceptReduce6;
         if (ParserInput != LexerResult.CSharpStart)
         {
            if (ErrorHandler(5, StateDescription5, ParserInput))
               goto State5;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpStart);
         Lexer.AcceptSymbol();
      State8:
         const String StateDescription8 =
              "optionalEnum= CSharpStart, ►CSEnumDeclaration, CSharpEnd;\r\n"
            + "optionalEnum= CSharpStart, ►CSharpEnd, NoEnum;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            goto State19;
         }
         if (ParserInput != LexerResult.CSharpEnd)
         {
            if (ErrorHandler(8, StateDescription8, ParserInput))
               goto State8;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpEnd);
         goto AcceptReduce6;

      State4:
         const String StateDescription4 =
              "OptionalWeight(Int32 weight)= \"%\", ►Number(Int32 weight);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Number)
         {
            if (ErrorHandler(4, StateDescription4, ParserInput))
               goto State4;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Number);
         Lexer.AcceptSymbol();
         goto Reduce4;

      State7:
         const String StateDescription7 =
              "TerminalSymbolsList= TerminalSymbolsList, \"|\", ►TerminalSymbol;";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(7, StateDescription7, ParserInput))
            {
               _s.Pop();
               goto State7;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
         goto AcceptState85;

      State9:
         const String StateDescription9 =
              "CSEnumDeclaration= CSEnumProperties, ►optionalBaseType, CSEnumMembers;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Colon)
         {
            Lexer.AcceptSymbol();
            goto State17;
         }
         if (ParserInput != LexerResult.RepeatStart)
         {
            if (ErrorHandler(9, StateDescription9, ParserInput))
               goto State9;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.RepeatStart);
      State10:
         const String StateDescription10 =
              "CSEnumDeclaration= CSEnumProperties, optionalBaseType, ►CSEnumMembers;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.RepeatStart)
         {
            if (ErrorHandler(10, StateDescription10, ParserInput))
               goto State10;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.RepeatStart);
         Lexer.AcceptSymbol();
      State11:
         const String StateDescription11 =
              "CSEnumMembers= \"{\", ►\"}\";\r\n"
            + "CSEnumMembers= \"{\", ►CSEnumMemberList, \"}\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         // Reduce9:
         {
            /* ResetEnumDefaults= ;◄ */

            ResetEnumDefaults();

            // State12:
            /* CSEnumMemberList= ResetEnumDefaults, ►CSEnumMember; */
            _s.Push(0);
            Debug.Assert(ParserInput == LexerResult.Name);
            goto AcceptState13;
         }
         if (ParserInput != LexerResult.RepeatEnd)
         {
            if (ErrorHandler(11, StateDescription11, ParserInput))
               goto State11;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.RepeatEnd);
         Lexer.AcceptSymbol();
         // Reduce10:
         /* CSEnumMembers= "{", "}";◄ */

         NoEnumRecognized();

         goto Reduce8;

      AcceptState13:
         Lexer.AcceptSymbol();
      State13:
         const String StateDescription13 =
              "CSEnumMember= Name(Int32 enumElementStringIndex)●;\r\n"
            + "CSEnumMember= Name(Int32 enumElementStringIndex), ►\"=\", Number(Int32 enumNumber);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput <= LexerResult.DefiningSymbol)
         {
            Lexer.AcceptSymbol();
            goto State14;
         }
         if (ParserInput != LexerResult.Comma
            && ParserInput != LexerResult.RepeatEnd)
         {
            if (ErrorHandler(13, StateDescription13, ParserInput))
               goto State13;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Comma
            || ParserInput == LexerResult.RepeatEnd);
         // Reduce12:
         /* sAdjust: -1, aAdjust: -1
          * CSEnumMember= Name(Int32 enumElementStringIndex);◄
          * then: CSEnumMemberList= ResetEnumDefaults, CSEnumMember;◄
          * or: CSEnumMemberList= CSEnumMemberList, Comma, CSEnumMember;◄ */
         _s.Pop();

         EnumElementRecognized(
            enumElementStringIndex: _a.PeekRef(0)._Int32
            );

         _a.Free();
      State15:
         const String StateDescription15 =
              "CSEnumMembers= \"{\", CSEnumMemberList, ►\"}\";\r\n"
            + "CSEnumMemberList= CSEnumMemberList, ►Comma, CSEnumMember;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Comma)
         {
            Lexer.AcceptSymbol();
            goto State16;
         }
         if (ParserInput != LexerResult.RepeatEnd)
         {
            if (ErrorHandler(15, StateDescription15, ParserInput))
               goto State15;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.RepeatEnd);
         Lexer.AcceptSymbol();
         goto Reduce8;

      State14:
         const String StateDescription14 =
              "CSEnumMember= Name(Int32 enumElementStringIndex), \"=\", ►Number(Int32 enumNumber);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Number)
         {
            if (ErrorHandler(14, StateDescription14, ParserInput))
               goto State14;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Number);
         Lexer.AcceptSymbol();
         // Reduce13:
         /* sAdjust: -1, aAdjust: -2
          * CSEnumMember= Name(Int32 enumElementStringIndex), "=", Number(Int32 enumNumber);◄
          * then: CSEnumMemberList= ResetEnumDefaults, CSEnumMember;◄
          * or: CSEnumMemberList= CSEnumMemberList, Comma, CSEnumMember;◄ */
         _s.Pop();

         EnumElementWithNumberRecognized(
            enumElementStringIndex: _a.PeekRef(-1)._Int32,
            enumNumber: _a.PeekRef(0)._Int32
            );

         _a.Free(2);
         goto State15;

      State16:
         const String StateDescription16 =
              "CSEnumMemberList= CSEnumMemberList, Comma, ►CSEnumMember;";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(16, StateDescription16, ParserInput))
            {
               _s.Pop();
               goto State16;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         goto AcceptState13;

      State17:
         const String StateDescription17 =
              "optionalBaseType= \":\", ►Name(Int32 Ignored);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(17, StateDescription17, ParserInput))
               goto State17;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
         // Reduce14:
         /* aAdjust: -1
          * optionalBaseType= ":", Name(Int32 Ignored);◄ */
         _a.Free();
         goto State10;

      State19:
         const String StateDescription19 =
              "CSEnumProperties= Name(Int32 modifier1StringIndex), ►Name(Int32 modifier2StringIndex), Name(Int32 enumStringIndex), CSEnumName;\r\n"
            + "CSEnumProperties= Name(Int32 modifierStringIndex), ►Name(Int32 enumStringIndex), CSEnumName;\r\n"
            + "CSEnumProperties= Name(Int32 enumStringIndex), ►CSEnumName;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(19, StateDescription19, ParserInput))
               goto State19;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
      State20:
         const String StateDescription20 =
              "CSEnumProperties= Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), ►Name(Int32 enumStringIndex), CSEnumName;\r\n"
            + "CSEnumProperties= Name(Int32 modifierStringIndex), Name(Int32 enumStringIndex), ►CSEnumName;\r\n"
            + "CSEnumName= Name(Int32 nameStringIndex)●;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            goto State21;
         }
         if (ParserInput != LexerResult.Colon
            && ParserInput != LexerResult.RepeatStart)
         {
            if (ErrorHandler(20, StateDescription20, ParserInput))
               goto State20;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Colon
            || ParserInput == LexerResult.RepeatStart);
         // Reduce16:
         /* aAdjust: -2
          * CSEnumName= Name(Int32 nameStringIndex);◄
          * then: CSEnumProperties= Name(Int32 enumStringIndex), CSEnumName;◄ */

         EnumNameRecognized(
            nameStringIndex: _a.PeekRef(0)._Int32
            );

         _a.Free(2);
         goto State9;

      State21:
         const String StateDescription21 =
              "CSEnumProperties= Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), Name(Int32 enumStringIndex), ►CSEnumName;\r\n"
            + "CSEnumName= Name(Int32 nameStringIndex)●;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            // Reduce19:
            /* aAdjust: -4
             * CSEnumName= Name(Int32 nameStringIndex);◄
             * then: CSEnumProperties= Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), Name(Int32 enumStringIndex), CSEnumName;◄ */

            EnumNameRecognized(
               nameStringIndex: _a.PeekRef(0)._Int32
               );

            _a.Free(4);
            goto State9;
         }
         if (ParserInput != LexerResult.Colon
            && ParserInput != LexerResult.RepeatStart)
         {
            if (ErrorHandler(21, StateDescription21, ParserInput))
               goto State21;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Colon
            || ParserInput == LexerResult.RepeatStart);
         // Reduce18:
         /* aAdjust: -3
          * CSEnumName= Name(Int32 nameStringIndex);◄
          * then: CSEnumProperties= Name(Int32 modifierStringIndex), Name(Int32 enumStringIndex), CSEnumName;◄ */

         EnumNameRecognized(
            nameStringIndex: _a.PeekRef(0)._Int32
            );

         _a.Free(3);
         goto State9;

      State26:
         const String StateDescription26 =
              "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"+\"●;\r\n"
            + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"+\", ►\"+\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Plus)
         {
            Lexer.AcceptSymbol();
            // Reduce31:
            /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "+", "+";◄ */

            Repeat1rrRecognized(
               Symbol: ref _a.PeekRef(0)._Symbol
               );

            goto Branch4;
         }
         if (ParserInput <= LexerResult.Asterisk
            || ParserInput == LexerResult.NumberSign)
         {
            if (ErrorHandler(26, StateDescription26, ParserInput))
               goto State26;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Comma
            || ParserInput >= LexerResult.GroupStart);
         // Reduce30:
         /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "+";◄ */

         Repeat1lrRecognized(
            Symbol: ref _a.PeekRef(0)._Symbol
            );

      Branch4:
         switch (_s.Peek())
         {
         case 5:
         // Reduce42:
         {
            /* sAdjust: -2, aAdjust: -1
             * Element= RepeatedElement(Symbol Symbol);◄
             * then: SequenceOfElements= SequenceOfElements, ","?, Element;◄ */
            _s.Discard(2);

            ElementVariantRecognized(
               Symbol: _a.PeekRef(0)._Symbol
               );

            _a.Free();
            goto Branch3;
         }
         case 6:
         case 7:
         case 8:
         case 9:
            goto Reduce63;
            /*case 2: case 3: case 4: case 11:
            default: break; */
         }
         // Reduce27:
         /* aAdjust: -1
          * Element= RepeatedElement(Symbol Symbol);◄ */

         ElementVariantRecognized(
            Symbol: _a.PeekRef(0)._Symbol
            );

         _a.Free();
         goto State32;

      State27:
         const String StateDescription27 =
              "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"*\"●;\r\n"
            + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"*\", ►\"*\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Asterisk)
         {
            Lexer.AcceptSymbol();
            // Reduce33:
            /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "*", "*";◄ */

            Repeat0rrRecognized(
               Symbol: ref _a.PeekRef(0)._Symbol
               );

            goto Branch4;
         }
         if (ParserInput <= LexerResult.Plus
            || ParserInput == LexerResult.NumberSign)
         {
            if (ErrorHandler(27, StateDescription27, ParserInput))
               goto State27;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Comma
            || ParserInput >= LexerResult.GroupStart);
         // Reduce32:
         /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "*";◄ */

         Repeat0lrRecognized(
            Symbol: ref _a.PeekRef(0)._Symbol
            );

         goto Branch4;

      AcceptState30:
         Lexer.AcceptSymbol();
      State30:
         const String StateDescription30 =
              "outerDefinitions= EndOfDefinition, \"|\", ►outerDefinitionList;";
         _s.Push(3);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.GroupStart)
            goto AcceptState77;
         if (ParserInput == LexerResult.OptionStart)
            goto AcceptState75;
         if (ParserInput == LexerResult.RepeatStart)
            goto AcceptState62;
         if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(30, StateDescription30, ParserInput))
            {
               _s.Pop();
               goto State30;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
         goto AcceptState85;

      AcceptState34:
         Lexer.AcceptSymbol();
      State34:
         const String StateDescription34 =
              "outerDefinitionList= SequenceOfElements, EndOfDefinition, \"|\", ►outerDefinitionList;";
         _s.Push(4);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.GroupStart)
            goto AcceptState77;
         if (ParserInput == LexerResult.OptionStart)
            goto AcceptState75;
         if (ParserInput == LexerResult.RepeatStart)
            goto AcceptState62;
         if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(34, StateDescription34, ParserInput))
            {
               _s.Pop();
               goto State34;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
         goto AcceptState85;

      AcceptState36:
         Lexer.AcceptSymbol();
      State36:
         const String StateDescription36 =
              "SequenceOfElements= SequenceOfElements, \",\"?, ►Element;";
         _s.Push(5);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.GroupStart)
            goto AcceptState77;
         if (ParserInput == LexerResult.OptionStart)
            goto AcceptState75;
         if (ParserInput == LexerResult.RepeatStart)
            goto AcceptState62;
         if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(36, StateDescription36, ParserInput))
            {
               _s.Pop();
               goto State36;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
         goto AcceptState85;

      AcceptState37:
         Lexer.AcceptSymbol();
      State37:
         const String StateDescription37 =
              "SemanticAction(VoidMethodClass method)= CSharpStart, ►CSvoidMethod(VoidMethodClass method), CSharpEnd;\r\n"
            + "SemanticAction(VoidMethodClass method)= CSharpStart, ►CSharpEnd;";
         // *Push(0)
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            // PushState3:
            _s.Push(0);
            goto State48;
         }
         if (ParserInput != LexerResult.CSharpEnd)
         {
            if (ErrorHandler(37, StateDescription37, ParserInput))
               goto State37;
            // PushState4:
            _s.Push(0);
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpEnd);
         Lexer.AcceptSymbol();
         // Reduce43:
         /* aAdjust: 1
          * SemanticAction(VoidMethodClass method)= CSharpStart, CSharpEnd;◄ */
         _a.Allocate();

         EmptySemanticAction(
            method: out _a.PeekRef(0)._VoidMethodClass
            );

         goto Branch9;

      State38:
         const String StateDescription38 =
              "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), ►\"(\", eFormalParameters, \")\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.GroupStart)
         {
            if (ErrorHandler(38, StateDescription38, ParserInput))
               goto State38;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupStart);
         Lexer.AcceptSymbol();
      State39:
         const String StateDescription39 =
              "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), \"(\", ►eFormalParameters, \")\";";
         _s.Push(0);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
            goto AcceptState43;
         if (ParserInput != LexerResult.GroupEnd)
         {
            if (ErrorHandler(39, StateDescription39, ParserInput))
            {
               _s.Pop();
               goto State39;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         // State42:
         /* CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), "(", eFormalParameters, ►")"; */
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         goto AcceptReduce44;

      State40:
         const String StateDescription40 =
              "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), \"(\", eFormalParameters, ►\")\";\r\n"
            + "formalParameters= formalParameters, ►Comma, formalParameter;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.GroupEnd)
            goto AcceptReduce44;
         if (ParserInput != LexerResult.Comma)
         {
            if (ErrorHandler(40, StateDescription40, ParserInput))
               goto State40;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Comma);
      AcceptState41:
         Lexer.AcceptSymbol();
      State41:
         const String StateDescription41 =
              "formalParameters= formalParameters, Comma, ►formalParameter;";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(41, StateDescription41, ParserInput))
            {
               _s.Pop();
               goto State41;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
      AcceptState43:
         Lexer.AcceptSymbol();
      State43:
         const String StateDescription43 =
              "formalParameter= Name(Int32 typeStringIndex), ►Name(Int32 nameStringIndex);\r\n"
            + "formalParameter= Name(Int32 typeStringIndex), ►\"?\", Name(Int32 nameStringIndex);\r\n"
            + "formalParameter= Name(Int32 ParameterModifierOptStringIndex), ►Name(Int32 typeStringIndex), Name(Int32 nameStringIndex);\r\n"
            + "formalParameter= Name(Int32 ParameterModifierOptStringIndex), ►Name(Int32 typeStringIndex), \"?\", Name(Int32 nameStringIndex);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            goto State44;
         }
         if (ParserInput != LexerResult.Questionmark)
         {
            if (ErrorHandler(43, StateDescription43, ParserInput))
               goto State43;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Questionmark);
         Lexer.AcceptSymbol();
      State46:
         const String StateDescription46 =
              "formalParameter= Name(Int32 typeStringIndex), \"?\", ►Name(Int32 nameStringIndex);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(46, StateDescription46, ParserInput))
               goto State46;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
         // Reduce49:
         /* aAdjust: -2
          * formalParameter= Name(Int32 typeStringIndex), "?", Name(Int32 nameStringIndex);◄ */

         FormalParameterWithNullableTypeAndName(
            typeStringIndex: _a.PeekRef(-1)._Int32,
            nameStringIndex: _a.PeekRef(0)._Int32
            );

         _a.Free(2);
      Branch10:
         switch (_s.Peek())
         {
         case 0:
            goto State40;
         case 1:
         // Reduce45:
         {
            /* sAdjust: -1
             * formalParameters= formalParameters, Comma, formalParameter;◄ */
            _s.Pop();
            goto Branch10;
         }
         /*case 2:
         default: break; */
         }
      State55:
         const String StateDescription55 =
              "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), \"(\", eFormalParameters, ►\")\";\r\n"
            + "formalParameters= formalParameters, ►Comma, formalParameter;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Comma)
            goto AcceptState41;
         if (ParserInput != LexerResult.GroupEnd)
         {
            if (ErrorHandler(55, StateDescription55, ParserInput))
               goto State55;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         goto AcceptReduce54;

      State44:
         const String StateDescription44 =
              "formalParameter= Name(Int32 typeStringIndex), Name(Int32 nameStringIndex)●;\r\n"
            + "formalParameter= Name(Int32 ParameterModifierOptStringIndex), Name(Int32 typeStringIndex), ►Name(Int32 nameStringIndex);\r\n"
            + "formalParameter= Name(Int32 ParameterModifierOptStringIndex), Name(Int32 typeStringIndex), ►\"?\", Name(Int32 nameStringIndex);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            // Reduce47:
            /* aAdjust: -3
             * formalParameter= Name(Int32 ParameterModifierOptStringIndex), Name(Int32 typeStringIndex), Name(Int32 nameStringIndex);◄ */

            FormalParameterWithModifierTypeAndName(
               ParameterModifierOptStringIndex: _a.PeekRef(-2)._Int32,
               typeStringIndex: _a.PeekRef(-1)._Int32,
               nameStringIndex: _a.PeekRef(0)._Int32
               );

            _a.Free(3);
            goto Branch10;
         }
         if (ParserInput == LexerResult.Questionmark)
         {
            Lexer.AcceptSymbol();
            goto State45;
         }
         if (ParserInput != LexerResult.Comma
            && ParserInput != LexerResult.GroupEnd)
         {
            if (ErrorHandler(44, StateDescription44, ParserInput))
               goto State44;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Comma
            || ParserInput == LexerResult.GroupEnd);
         // Reduce46:
         /* aAdjust: -2
          * formalParameter= Name(Int32 typeStringIndex), Name(Int32 nameStringIndex);◄ */

         FormalParameterWithTypeAndName(
            typeStringIndex: _a.PeekRef(-1)._Int32,
            nameStringIndex: _a.PeekRef(0)._Int32
            );

         _a.Free(2);
         goto Branch10;

      State45:
         const String StateDescription45 =
              "formalParameter= Name(Int32 ParameterModifierOptStringIndex), Name(Int32 typeStringIndex), \"?\", ►Name(Int32 nameStringIndex);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(45, StateDescription45, ParserInput))
               goto State45;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
         // Reduce48:
         /* aAdjust: -3
          * formalParameter= Name(Int32 ParameterModifierOptStringIndex), Name(Int32 typeStringIndex), "?", Name(Int32 nameStringIndex);◄ */

         FormalParameterWithModifierNullableTypeAndName(
            ParameterModifierOptStringIndex: _a.PeekRef(-2)._Int32,
            typeStringIndex: _a.PeekRef(-1)._Int32,
            nameStringIndex: _a.PeekRef(0)._Int32
            );

         _a.Free(3);
         goto Branch10;

      State48:
         const String StateDescription48 =
              "CSMethodProperties(MethodClass method)= Name(Int32 methodTypeStringIndex), ►Name(Int32 methodNameStringIndex);\r\n"
            + "CSMethodProperties(MethodClass method)= Name(Int32 modifierStringIndex), ►Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex);\r\n"
            + "CSMethodProperties(MethodClass method)= Name(Int32 modifier1StringIndex), ►Name(Int32 modifier2StringIndex), Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(48, StateDescription48, ParserInput))
               goto State48;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
      State49:
         const String StateDescription49 =
              "CSMethodProperties(MethodClass method)= Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex)●;\r\n"
            + "CSMethodProperties(MethodClass method)= Name(Int32 modifierStringIndex), Name(Int32 methodTypeStringIndex), ►Name(Int32 methodNameStringIndex);\r\n"
            + "CSMethodProperties(MethodClass method)= Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), ►Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            goto State50;
         }
         if (ParserInput != LexerResult.GroupStart)
         {
            if (ErrorHandler(49, StateDescription49, ParserInput))
               goto State49;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupStart);
         // Reduce51:
         /* aAdjust: -1
          * CSMethodProperties(MethodClass method)= Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex);◄ */

         MethodTypeAndNameRecognized(
            method: out _a.PeekRef(-1)._MethodClass,
            methodTypeStringIndex: _a.PeekClear(-1)._Int32,
            methodNameStringIndex: _a.PeekRef(0)._Int32
            );

         _a.Free();
      Branch11:
         if (_s.Peek() == 0)
            goto State38;
         State53:
         const String StateDescription53 =
              "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), ►\"(\", eFormalParameters, \")\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.GroupStart)
         {
            if (ErrorHandler(53, StateDescription53, ParserInput))
               goto State53;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupStart);
         Lexer.AcceptSymbol();
      State54:
         const String StateDescription54 =
              "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), \"(\", ►eFormalParameters, \")\";";
         _s.Push(2);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
            goto AcceptState43;
         if (ParserInput != LexerResult.GroupEnd)
         {
            if (ErrorHandler(54, StateDescription54, ParserInput))
            {
               _s.Pop();
               goto State54;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         // State56:
         /* CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), "(", eFormalParameters, ►")"; */
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         goto AcceptReduce54;

      State50:
         const String StateDescription50 =
              "CSMethodProperties(MethodClass method)= Name(Int32 modifierStringIndex), Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex)●;\r\n"
            + "CSMethodProperties(MethodClass method)= Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), Name(Int32 methodTypeStringIndex), ►Name(Int32 methodNameStringIndex);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            // Reduce53:
            /* aAdjust: -3
             * CSMethodProperties(MethodClass method)= Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex);◄ */

            MethodModifierTypeAndNameRecognized2(
               method: out _a.PeekRef(-3)._MethodClass,
               modifier1StringIndex: _a.PeekClear(-3)._Int32,
               modifier2StringIndex: _a.PeekRef(-2)._Int32,
               methodTypeStringIndex: _a.PeekRef(-1)._Int32,
               methodNameStringIndex: _a.PeekRef(0)._Int32
               );

            _a.Free(3);
            goto Branch11;
         }
         if (ParserInput != LexerResult.GroupStart)
         {
            if (ErrorHandler(50, StateDescription50, ParserInput))
               goto State50;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupStart);
         // Reduce52:
         /* aAdjust: -2
          * CSMethodProperties(MethodClass method)= Name(Int32 modifierStringIndex), Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex);◄ */

         MethodModifierTypeAndNameRecognized(
            method: out _a.PeekRef(-2)._MethodClass,
            modifierStringIndex: _a.PeekClear(-2)._Int32,
            methodTypeStringIndex: _a.PeekRef(-1)._Int32,
            methodNameStringIndex: _a.PeekRef(0)._Int32
            );

         _a.Free(2);
         goto Branch11;

      AcceptState51:
         Lexer.AcceptSymbol();
      State51:
         const String StateDescription51 =
              "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", ►signedNumber(Int32 constPriority), \"??\";\r\n"
            + "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", ►CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, (Local1)?;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Minus)
         {
            Lexer.AcceptSymbol();
            goto State61;
         }
         if (ParserInput == LexerResult.Plus)
         {
            Lexer.AcceptSymbol();
            goto State59;
         }
         if (ParserInput == LexerResult.Number)
            goto AcceptState60;
         if (ParserInput != LexerResult.CSharpStart)
         {
            if (ErrorHandler(51, StateDescription51, ParserInput))
               goto State51;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpStart);
         Lexer.AcceptSymbol();
      State52:
         const String StateDescription52 =
              "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, ►CSintMethod(IntMethodClass intMethod), CSharpEnd, (Local1)?;";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(52, StateDescription52, ParserInput))
            {
               _s.Pop();
               goto State52;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
         goto State48;

      State59:
         const String StateDescription59 =
              "signedNumber(Int32 value)= \"+\", ►Number(Int32 value);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Number)
         {
            if (ErrorHandler(59, StateDescription59, ParserInput))
               goto State59;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Number);
      AcceptState60:
         Lexer.AcceptSymbol();
      State60:
         const String StateDescription60 =
              "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", signedNumber(Int32 constPriority), ►\"??\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.DoubleQuestionmark)
         {
            if (ErrorHandler(60, StateDescription60, ParserInput))
               goto State60;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.DoubleQuestionmark);
         Lexer.AcceptSymbol();
         // Reduce56:
         /* aAdjust: 1
          * PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= "??", signedNumber(Int32 constPriority), "??";◄ */
         _a.Allocate();

         ConstantPriorityGiven(
            dynamicPriority: out _a.PeekRef(0)._IntMethodClass
            );

         goto State28;

      State61:
         const String StateDescription61 =
              "signedNumber(Int32 value)= \"-\", ►Number(Int32 value);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Number)
         {
            if (ErrorHandler(61, StateDescription61, ParserInput))
               goto State61;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Number);
         Lexer.AcceptSymbol();
         // Reduce57:
         /* signedNumber(Int32 value)= "-", Number(Int32 value);◄ */

         NegateNumber(
            value: ref _a.PeekRef(0)._Int32
            );

         goto State60;

      AcceptState62:
         Lexer.AcceptSymbol();
         // State62:
         /* RepeatedElement(Symbol Symbol)= "{", ►NestedElement(Symbol Symbol), "}"; */
         _s.Push(0);
         goto Reduce58;

      State64:
         const String StateDescription64 =
              "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions●;\r\n"
            + "NestedDefinitions= EndOfDefinition, ►\"|\", NestedDefinitionList;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState65;
         if (ParserInput <= LexerResult.CSharpStart
            || ParserInput >= LexerResult.Name)
         {
            if (ErrorHandler(64, StateDescription64, ParserInput))
               goto State64;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
         goto Reduce61;

      AcceptState65:
         Lexer.AcceptSymbol();
      State65:
         const String StateDescription65 =
              "NestedDefinitions= EndOfDefinition, \"|\", ►NestedDefinitionList;";
         _s.Push(7);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.RepeatStart)
            goto AcceptState62;
         if (ParserInput == LexerResult.GroupStart)
            goto AcceptState77;
         if (ParserInput == LexerResult.OptionStart)
            goto AcceptState75;
         if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(65, StateDescription65, ParserInput))
            {
               _s.Pop();
               goto State65;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
         goto AcceptState85;

      State66:
         const String StateDescription66 =
              "NestedDefinitions= EndOfDefinition, \"|\", NestedDefinitionList●;\r\n"
            + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState67;
         if (ParserInput <= LexerResult.CSharpStart
            || ParserInput >= LexerResult.Name)
         {
            if (ErrorHandler(66, StateDescription66, ParserInput))
               goto State66;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
         // Reduce64:
         /* sAdjust: -1
          * NestedDefinitions= EndOfDefinition, "|", NestedDefinitionList;◄ */
         _s.Pop();
         // Branch13:
         if (_s.Peek() == 6)
            goto Reduce61;
         goto Reduce69;

      AcceptState67:
         Lexer.AcceptSymbol();
      State67:
         const String StateDescription67 =
              "NestedDefinitionList= NestedDefinitionList, \"|\", ►Definition;";
         _s.Push(8);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.RepeatStart)
            goto AcceptState62;
         if (ParserInput == LexerResult.GroupStart)
            goto AcceptState77;
         if (ParserInput == LexerResult.OptionStart)
            goto AcceptState75;
         if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(67, StateDescription67, ParserInput))
            {
               _s.Pop();
               goto State67;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
         goto AcceptState85;

      State69:
         const String StateDescription69 =
              "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions●;\r\n"
            + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState67;
         if (ParserInput <= LexerResult.CSharpStart
            || ParserInput >= LexerResult.Name)
         {
            if (ErrorHandler(69, StateDescription69, ParserInput))
               goto State69;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
         goto Reduce61;

      State70:
         const String StateDescription70 =
              "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►NestedDefinitions;";
         _s.Push(9);
         ParserInput = Lexer.PeekSymbol();
         switch (ParserInput)
         {
         // <= LexerResult.NumberSign
         // >= LexerResult.TerminatorSymbol: goto HandleError70 // see end of switch
         case LexerResult.GroupStart:
            goto AcceptState77;
         case LexerResult.OptionStart:
            goto AcceptState75;
         case LexerResult.RepeatStart:
            goto AcceptState62;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState51;
         case LexerResult.CSharpStart:
            goto AcceptState37;
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
         case LexerResult.DefinitionSeparatorSymbol:
         // Reduce68:
         {
            /* EndOfDefinitionWithoutSemantics= ;◄ */

            EndOfDefinitionWithoutSemanticsRecognized();

            goto State71;
         }
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto AcceptState85;
         } // end of switch
         Debug.Assert(ParserInput <= LexerResult.NumberSign || ParserInput >= LexerResult.TerminatorSymbol);

         if (ErrorHandler(70, StateDescription70, ParserInput))
         {
            _s.Pop();
            goto State70;
         };
         goto EndWithError;

      State71:
         const String StateDescription71 =
              "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions●;\r\n"
            + "NestedDefinitions= EndOfDefinition, ►\"|\", NestedDefinitionList;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState65;
         if (ParserInput <= LexerResult.CSharpStart
            || ParserInput >= LexerResult.Name)
         {
            if (ErrorHandler(71, StateDescription71, ParserInput))
               goto State71;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
         goto Reduce69;

      State73:
         const String StateDescription73 =
              "NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= \"Name(Attributes)\"(Int32 stringIndex, Int32 NumberOfAttributes), ►\"=\";\r\n"
            + "SimpleElement(Symbol Symbol)= \"Name(Attributes)\"(Int32 stringIndex, Int32 NumberOfAttributes)●;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput <= LexerResult.DefiningSymbol)
         {
            Lexer.AcceptSymbol();
            // Reduce71:
            /* NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes), "=";◄ */

            LeftSideOfNestedProduction(
               SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
               stringIndex: _a.PeekClear(-1)._Int32,
               NumberOfAttributes: _a.PeekRef(0)._Int32
               );

            goto State70;
         }
         if (_1In(_Colon + _Percent + _CSharpEnd + _Error + _Minus + _Number + _StarEqual + _MinusEqual + _NumberSign + _TerminatorSymbol))
         {
            if (ErrorHandler(73, StateDescription73, ParserInput))
               goto State73;
            goto EndWithError;
         }
         Debug.Assert(!_1In(_DefiningSymbol + _Colon + _Percent + _CSharpEnd + _Error + _Minus + _Number + _StarEqual + _MinusEqual + _NumberSign + _TerminatorSymbol)
            );
         goto Reduce24;

      State74:
         const String StateDescription74 =
              "RepeatedElement(Symbol Symbol)= \"{\", NestedElement(Symbol Symbol), ►\"}\";";
         if (ParserInput != LexerResult.RepeatEnd)
         {
            if (ErrorHandler(74, StateDescription74, ParserInput))
               goto State74;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.RepeatEnd);
         Lexer.AcceptSymbol();
         // Reduce72:
         /* sAdjust: -1
          * RepeatedElement(Symbol Symbol)= "{", NestedElement(Symbol Symbol), "}";◄ */
         _s.Pop();

         RepeatGroupRecognized(
            Symbol: ref _a.PeekRef(0)._Symbol
            );

         goto Branch4;

      AcceptState75:
         Lexer.AcceptSymbol();
         // State75:
         /* RepeatedElement(Symbol Symbol)= "[", ►NestedElement(Symbol Symbol), "]"; */
         _s.Push(1);
         goto Reduce58;

      State76:
         const String StateDescription76 =
              "RepeatedElement(Symbol Symbol)= \"[\", NestedElement(Symbol Symbol), ►\"]\";";
         if (ParserInput < LexerResult.OptionEnd)
         {
            if (ErrorHandler(76, StateDescription76, ParserInput))
               goto State76;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.OptionEnd);
         Lexer.AcceptSymbol();
         // Reduce73:
         /* sAdjust: -1
          * RepeatedElement(Symbol Symbol)= "[", NestedElement(Symbol Symbol), "]";◄ */
         _s.Pop();

         OptionGroupRecognized(
            Symbol: ref _a.PeekRef(0)._Symbol
            );

         goto Branch4;

      AcceptState77:
         Lexer.AcceptSymbol();
         // State77:
         /* SimpleElement(Symbol Symbol)= "(", ►NestedElement(Symbol Symbol), ")"; */
         _s.Push(2);
         goto Reduce58;

      State80:
         const String StateDescription80 =
              "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►\"=\", outerDefinitions;\r\n"
            + "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►\"-=\", ListOfExcludedTerminalSymbols, \";\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.MinusEqual)
         {
            Lexer.AcceptSymbol();
            goto State81;
         }
         if (ParserInput > LexerResult.DefiningSymbol)
         {
            if (ErrorHandler(80, StateDescription80, ParserInput))
               goto State80;
            goto EndWithError;
         }
         Debug.Assert(ParserInput <= LexerResult.DefiningSymbol);
         Lexer.AcceptSymbol();
      State84:
         const String StateDescription84 =
              "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"=\", ►outerDefinitions;";
         _s.Push(11);
         ParserInput = Lexer.PeekSymbol();
         switch (ParserInput)
         {
         // <= LexerResult.NumberSign: goto HandleError84 // see end of switch
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
            goto HandleError84;
         case LexerResult.GroupStart:
            goto AcceptState77;
         case LexerResult.OptionStart:
            goto AcceptState75;
         case LexerResult.RepeatStart:
            goto AcceptState62;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState51;
         case LexerResult.CSharpStart:
            goto AcceptState37;
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto AcceptState85;
            // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce23 // see end of switch
         } // end of switch
         if (ParserInput >= LexerResult.DefinitionSeparatorSymbol)
            goto Reduce23;
         Debug.Assert(ParserInput <= LexerResult.NumberSign);

      HandleError84:
         if (ErrorHandler(84, StateDescription84, ParserInput))
         {
            _s.Pop();
            goto State84;
         };
         goto EndWithError;

      State81:
         const String StateDescription81 =
              "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"-=\", ►ListOfExcludedTerminalSymbols, \";\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(81, StateDescription81, ParserInput))
               goto State81;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
         // Reduce79:
         /* aAdjust: -1
          * ListOfExcludedTerminalSymbols= Name(Int32 stringIndex);◄ */

         FirstExcludedTerminalSymbol(
            stringIndex: _a.PeekRef(0)._Int32
            );

         _a.Free();
      State82:
         const String StateDescription82 =
              "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"-=\", ListOfExcludedTerminalSymbols, ►\";\";\r\n"
            + "ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, ►\"|\", Name(Int32 nameIndex);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
         {
            Lexer.AcceptSymbol();
            goto State83;
         }
         if (ParserInput < LexerResult.TerminatorSymbol)
         {
            if (ErrorHandler(82, StateDescription82, ParserInput))
               goto State82;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
         Lexer.AcceptSymbol();
         // Reduce80:
         /* GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "-=", ListOfExcludedTerminalSymbols, ";";◄ */

         EndOfListOfExcludedTerminalSymbols();

         goto Reduce78;

      State83:
         const String StateDescription83 =
              "ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, \"|\", ►Name(Int32 nameIndex);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(83, StateDescription83, ParserInput))
               goto State83;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
         // Reduce81:
         /* aAdjust: -1
          * ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, "|", Name(Int32 nameIndex);◄ */

         OneMoreExcludedTerminalSymbol(
            nameIndex: _a.PeekRef(0)._Int32
            );

         _a.Free();
         goto State82;

      State86:
         const String StateDescription86 =
              "\"(Attributes)\"(Int32 numberOfAttributes)= \"(\", ►\")\";\r\n"
            + "\"(Attributes)\"(Int32 numberOfAttributes)= \"(\", ►\"Attributes)\"(Int32 numberOfAttributes, Int32 smallestNumber);";
         // *Push(0)
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            // PushState7:
            _s.Push(0);
            goto State89;
         }
         if (ParserInput != LexerResult.GroupEnd)
         {
            if (ErrorHandler(86, StateDescription86, ParserInput))
               goto State86;
            // PushState8:
            _s.Push(0);
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         Lexer.AcceptSymbol();
         // Reduce85:
         /* aAdjust: 1
          * "(Attributes)"(Int32 numberOfAttributes)= "(", ")";◄ */
         _a.Allocate();

         EmptyListOfAttributes(
            numberOfAttributes: out _a.PeekRef(0)._Int32
            );

      Branch16:
         if (_s.Peek() == 0)
         // Reduce84:
         {
            /* sAdjust: -1
             * "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex), "(Attributes)"(Int32 NumberOfAttributes);◄ */
            _s.Pop();
            goto Branch15;
         }
         // Reduce91:
         /* sAdjust: -1
          * "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex), "(Attributes)"(Int32 NumberOfAttributes);◄ */
         _s.Pop();
         goto State3;

      State87:
         const String StateDescription87 =
              "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 numberBeforeGroup), ►Comma, \"Attributes)\"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup);\r\n"
            + "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 number), ►\")\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Comma)
         {
            Lexer.AcceptSymbol();
            goto State88;
         }
         if (ParserInput != LexerResult.GroupEnd)
         {
            if (ErrorHandler(87, StateDescription87, ParserInput))
               goto State87;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         Lexer.AcceptSymbol();
         // Reduce87:
         /* aAdjust: 1
          * "Attributes)"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 number), ")";◄ */
         _a.Allocate();

         FirstAttributeOfGroup(
            numberOfAttributesOfGroup: out _a.PeekRef(-1)._Int32,
            smallestNumber: out _a.PeekRef(0)._Int32,
            number: _a.PeekRef(-1)._Int32
            );

      Branch17:
         if (_s.Peek() == 1)
         // Reduce88:
         {
            /* sAdjust: -1, aAdjust: -1
             * "Attributes)"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 numberBeforeGroup), Comma, "Attributes)"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup);◄ */
            _s.Pop();

            AnotherAttributeOfGroup(
               numberOfAttributesOfGroup: out _a.PeekRef(-2)._Int32,
               smallestNumber: out _a.PeekRef(-1)._Int32,
               numberBeforeGroup: _a.PeekRef(-2)._Int32,
               numberOfAttributesOfRightGroup: _a.PeekRef(-1)._Int32,
               smallestNumberOfRightGroup: _a.PeekRef(0)._Int32
               );

            _a.Free();
            goto Branch17;
         }
         // Reduce86:
         /* sAdjust: -1, aAdjust: -1
          * "(Attributes)"(Int32 numberOfAttributes)= "(", "Attributes)"(Int32 numberOfAttributes, Int32 smallestNumber);◄ */
         _s.Pop();
         _a.Free();
         goto Branch16;

      State88:
         const String StateDescription88 =
              "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 numberBeforeGroup), Comma, ►\"Attributes)\"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup);";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(88, StateDescription88, ParserInput))
            {
               _s.Pop();
               goto State88;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
      State89:
         const String StateDescription89 =
              "Attribut(Int32 number)= Name(Int32 typeStringIndex), ►Name(Int32 nameStringIndex);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(89, StateDescription89, ParserInput))
               goto State89;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
         // Reduce89:
         /* aAdjust: -1
          * Attribut(Int32 number)= Name(Int32 typeStringIndex), Name(Int32 nameStringIndex);◄ */

         AttributeTypeAndName(
            number: out _a.PeekRef(-1)._Int32,
            typeStringIndex: _a.PeekRef(-1)._Int32,
            nameStringIndex: _a.PeekRef(0)._Int32
            );

         _a.Free();
         goto State87;

      State90:
         const String StateDescription90 =
              "GrammlatorSetting= Name(Int32 nameIndex), ►\":\", LexerString(Int32 stringIndex), \";\"?;\r\n"
            + "\"Name(Attributes)\"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex), ►\"(Attributes)\"(Int32 NumberOfAttributes);\r\n"
            + "\"Name(Attributes)\"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex)●;";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Colon)
         {
            Lexer.AcceptSymbol();
            goto State91;
         }
         if (ParserInput == LexerResult.GroupStart)
         {
            Lexer.AcceptSymbol();
            goto State86;
         }
         if (!_1In(_Percent + _CSharpStart + _DefinitionSeparatorSymbol + _TerminatorSymbol))
         {
            if (ErrorHandler(90, StateDescription90, ParserInput))
            {
               _s.Pop();
               goto State90;
            };
            goto EndWithError;
         }
         Debug.Assert(_1In(_Percent + _CSharpStart + _DefinitionSeparatorSymbol + _TerminatorSymbol));
         // Reduce90:
         /* sAdjust: -1, aAdjust: 1
          * "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex);◄ */
         _s.Pop();
         _a.Allocate();

         NameWithoutAttributes(
            NumberOfAttributes: out _a.PeekRef(0)._Int32
            );

         goto State3;

      State91:
         const String StateDescription91 =
              "GrammlatorSetting= Name(Int32 nameIndex), \":\", ►LexerString(Int32 stringIndex), \";\"?;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(91, StateDescription91, ParserInput))
               goto State91;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.LexerString);
         Lexer.AcceptSymbol();
      State92:
         const String StateDescription92 =
              "GrammlatorSetting= Name(Int32 nameIndex), \":\", LexerString(Int32 stringIndex), ►\";\"?;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput >= LexerResult.TerminatorSymbol)
         {
            Lexer.AcceptSymbol();
            goto Reduce92;
         }
         if (!_1In(_StarEqual + _Name + _LexerString))
         {
            if (ErrorHandler(92, StateDescription92, ParserInput))
               goto State92;
            goto EndWithError;
         }
         Debug.Assert(_1In(_StarEqual + _Name + _LexerString));
      Reduce92:
         /* sAdjust: -2, aAdjust: -2
          * GrammlatorSetting= Name(Int32 nameIndex), ":", LexerString(Int32 stringIndex), ";"?;◄
          * then: OptionalGrammlatorSettings= OptionalGrammlatorSettings, GrammlatorSetting;◄ */
         _s.Discard(2);

         SetGrammlatorStringSetting(
            nameIndex: _a.PeekRef(-1)._Int32,
            stringIndex: _a.PeekRef(0)._Int32
            );

         _a.Free(2);
         goto State2;

      Reduce36:
         /* sAdjust: -1
          * outerDefinitions= EndOfDefinition, "|", outerDefinitionList;◄ */
         _s.Pop();
         goto Branch6;

      Reduce40:
         /* sAdjust: -2
          * outerDefinitionList= SequenceOfElements, EndOfDefinition, "|", outerDefinitionList;◄ */
         _s.Discard(2);
         // Branch8:
         switch (_s.Peek())
         {
         case 2:
            goto Reduce25;
         case 3:
            goto Reduce36;
         case 4:
            goto Reduce40;
            /*case 11:
            default: break; */
         }
         goto Reduce82;

      HandleError24:
         if (ErrorHandler(24, StateDescription24, ParserInput))
         {
            _s.Pop();
            goto State24;
         };
         goto EndWithError;

      HandleError32:
         if (ErrorHandler(32, StateDescription32, ParserInput))
         {
            _s.Pop();
            goto State32;
         };
         goto EndWithError;

      EndWithError:
         // This point is reached after an input error has been found
         _s.Discard(_s.Count - StateStackInitialCount);
         _a.Free(_a.Count - AttributeStackInitialCount);

      EndOfGeneratedCode:
         ;

         #endregion grammlator generated Wed, 23 Sep 2020 18:38:54 GMT (grammlator, File version 2020.09.14.0 23.09.2020 18:37:31)
         /* ************************ code written by programmer ******************** */
      }
   }
}
