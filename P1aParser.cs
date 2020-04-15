using GrammlatorRuntime;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Collections;

namespace Grammlator {
   /// <summary>
   /// Grammlator Parser (uses Lexer which uses InputClassifier)
   /// </summary>
   internal sealed partial class P1aParser: GrammlatorApplication {
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
         String RegionString = GlobalVariables.RegionString;
         String EndregionString = GlobalVariables.EndregionString;
         String GrammarString = GlobalVariables.GrammarString;

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
            if (CheckUsageOfSymbols(GlobalVariables.OutputPositionAndMessage) >= MessageTypeOrDestinationEnum.Error)
               {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Abort, "translation aborted: error(s) in source"); // throws an exception
               Debug.Fail("This debug instruction should never be executed");
               }
            }
         finally { }
         }

      private readonly P1bLexer Lexer;

      private readonly ReadOnlyMemory<char> Source;

      #region declaration of fields
      /// <summary>
      /// Set by constructor, contains at the first places the terminal symbols followed by the nonterminal symbols
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
          => GlobalVariables.OutputPositionAndMessage(messageType, message, Lexer.LexerTextPos);

      #region grammar
      //| /* ---- Start of grammlator grammar as control structure  ---- */
      //|
      //| // Compiler settings:
      //| IfToSwitchBorder: "5";
      //| Symbol: "ParserInput";
      //| AssignSymbol: "ParserInput = Lexer.PeekSymbol();";
      //| AcceptSymbol: "Lexer.AcceptSymbol();";
      //| TerminalSymbolEnum: "LexerResult";
      //| StateDescription: "StateDescription";
      //| ErrorHandlerMethod: "ErrorHandler";
      //|
      //| // Terminal symbols:
      //|     DefiningSymbol%18
      //|     | Colon%15        
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
      //|     | Comma%20 
      //|     | NumberSign
      //|     
      //|     | GroupStart | OptionStart | RepeatStart
      //|     | DoubleQuestionmark 
      //|     | CSharpStart%2 
      //|     | GroupEnd | RepeatEnd | OptionEnd
      //|     | Name(Int32 stringIndex)%25
      //|     | StringResult(Int32 stringIndex)%23
      //|     | DefinitionSeparatorSymbol%25  
      //|     | TerminatorSymbol%22
      public enum LexerResultCopy { // Defines the output of the lexer, which is assigned to Symbol to be used by the parser
                                    // The elements of LexerResult are order such that grammlator can
                                    // generate efficientcode for the conditions of the parsers actions
         DefiningSymbol, // =
         Colon, // :
         Percent, // %
         CSharpEnd,  // represents the change from CSharp lines to grammlator lines
         Error, // Error is the result if some input could not be assigned to any other LexerResult
         Minus, // Part of "-="
         Number /* (Int32 i) */,
         StarEqual, // "*=", added by the lexer
         MinusEqual,// "-=", addeed by the lexer
         Questionmark, // part of "??"
         Asterisk, // Part of "*="
         Plus, // +
         Comma, // ,
         NumberSign, // #

         GroupStart, OptionStart, RepeatStart,                // these are the characters ( [ {

         DoubleQuestionmark,
         CSharpStart, // represents the change from grammlator lines to CSharp lines

         GroupEnd, RepeatEnd, OptionEnd,          // these are the characters ) ] } #

         Name /* (Int32 stringIndex) */,
         StringResult /* (Int32 stringIndex) */,

         DefinitionSeparatorSymbol, // |
         TerminatorSymbol // ;
         };

      //|  *= GrammlatorGrammar /* one startsymbol, which has no attributes */

      //| // renaming some symbols
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
      //|    Name(Int32 nameIndex), ":", StringResult(Int32 stringIndex), ";"?
      private void SetGrammlatorSetting(Int32 nameIndex, Int32 stringIndex)
         {
         string name = GlobalVariables.GetStringOfIndex(nameIndex);
         string value = GlobalVariables.GetStringOfIndex(stringIndex);
         value = value.Substring(1, value.Length - 2); // remove leading and trailing "

         switch (name.ToLower())
            {
         case "iftoswitchborder":
            if (!Int32.TryParse(value, out GlobalVariables.IfToSwitchBorder))
               {
               GlobalVariables.IfToSwitchBorder = (int)grammlator.App.Current.Properties["IfToSwitchBorder"];
               P1OutputMessageAndLexerPosition(
                   MessageTypeOrDestinationEnum.Warning,
                   $"Compiler setting iftoswitchborder can be set only to a string representing an integer value"
                   );
               }
            break;
         case "assignsymbol":
            GlobalVariables.InstructionAssignSymbol = value;
            break;
         case "symbol":
            GlobalVariables.VariableNameSymbol = value;
            break;
         case "acceptsymbol":
            GlobalVariables.InstructionAcceptSymbol = value;
            break;
         case "terminalsymbolenum":
            GlobalVariables.TerminalSymbolEnum = value;
            break;
         case "statedescription":
            GlobalVariables.VariableNameStateDescription = value;
            break;
         case "errorhandlermethod":
            GlobalVariables.ErrorHandlerMethod = value;
            break;
         case "instructionerrorhalt":
            GlobalVariables.InstructionErrorHalt = value;
            break;
         case "statestackinitialcountvariable":
            GlobalVariables.StateStackInitialCountVariable = value;
            break;
         case "statestack":
            GlobalVariables.StateStack = value;
            break;
         case "attributestack":
            GlobalVariables.AttributeStack = value;
            break;
         default:
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error, $"Unknow compiler setting \"{name}\"");
            break;
            }
         }

      //| OptionalDeclarationOfTerminalSymbols=
      //|      /* empty */
      //|    | TerminalSymbolsList, SemikolonOrEnum
      private void CompareTerminalDeclarationsWithEnum()
         {
         if (Enumlist == null)
            return;
         if (Enumlist.Count != SymbolDictionary.Count)
            {
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                $"The number of elements in enum \"{GlobalVariables.GetStringOfIndex(EnumNameStringIndex)}\" differs from the number of terminal symbols."
                );
            }
         Int32 enumIndex = 0;
         foreach (Int32 dictNameIndex in SymbolDictionary.Keys)
            {
            if (dictNameIndex != Enumlist[enumIndex]) 
               {
               string name = GlobalVariables.GetStringOfIndex(dictNameIndex);
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                   $"The name \"{name}\" in the terminal definition and the corresponding name \"{Enumlist[enumIndex - 1]}\" in the enum are different.");
               // break; // no break: test for more errors in the enum
               }
            if (++enumIndex >= Enumlist.Count)
               break;
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
      private static void DefaultWeightOne(out Int32 weight) => weight = 1;
      //|     | "%", Number(Int32 weight)

      //|  ExtendedName (Int32 stringIndex)=
      //|       Name(Int32 stringIndex)
      //|     | StringResult(Int32 stringIndex)

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
      //|     "*=", outerDefinitions // , EmptyOrEnum
      private void FirstGrammarRuleRecognized()
         {
         EvaluateDefinitionsOftheStartsymbol();

         // OptimizeTrivialDefinitions has been set false for the first grammar rule: reset to previous value
         OptimizeTrivialDefinitions = OptimizeTrivialDefinitionsBackup;

         // Ignore the enum of the startsymbol
         Enumlist = null;
         EnumNameStringIndex = -1;
         }

      //| NoEnum=
      private void EnumOmitted()
         {
         Enumlist = null;
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
      private void EndOfListOfExcludedTerminalSymbols() => EvaluateExcludedTerminalSymbols(ExcludedTerminalSymbols);

      //| ListOfExcludedTerminalSymbols=
      //|      Name(Int32 stringIndex)
      private void FirstExcludedTerminalSymbol(Int32 stringIndex)
         {
         if (ExcludedTerminalSymbols == null || ExcludedTerminalSymbols.Length != GlobalVariables.NumberOfTerminalSymbols)
            ExcludedTerminalSymbols = new BitArray(GlobalVariables.NumberOfTerminalSymbols);
         ExcludedTerminalSymbols.SetAll(false);
         OneMoreExcludedTerminalSymbol(stringIndex);
         }

      private BitArray ExcludedTerminalSymbols;

      //|     | ListOfExcludedTerminalSymbols, "|", Name(Int32 nameIndex)
      private void OneMoreExcludedTerminalSymbol(Int32 nameIndex)
         {
         string name = GlobalVariables.GetStringOfIndex(nameIndex);
         if (!SymbolDictionary.TryGetValue(nameIndex, out Symbol Symbol))
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
         ExcludedTerminalSymbols.Set(Symbol.SymbolNumber, true);
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
          => EndOfDefinitionRecognized(constPriority: 0, dynPriority: null, method: null);

      //| EndOfDefinitionWithSemantics=
      //|    PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority)
      private void EndOfDefinitionWithSemanticsRecognized(Int32 constPriority, IntMethodClass dynPriority)
          => EndOfDefinitionRecognized(constPriority, dynPriority, method: null);

      //|    | SemanticAction(VoidMethodClass method)
      private void OuterEndOfDefinitionRecognized(VoidMethodClass method)
          => EndOfDefinitionRecognized(constPriority: 0, dynPriority: null, method: method);

      //|    | PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority), SemanticAction(VoidMethodClass method)
      private void EndOfDefinitionRecognized(Int32 constPriority, IntMethodClass dynPriority, VoidMethodClass method)
         {
         EvaluateDefinition(constPriority, dynPriority, method, OptimizeTrivialDefinitions);
         AttributeCounter = AttributeNumberAtStartOfDefinition;
         }

      //| PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)=
      //|      "??", signedNumber(Int32 constPriority), "??"
      private static void ConstantPriorityGiven(out IntMethodClass dynamicPriority) => dynamicPriority = null;

      //|    | "??",  // ???? "Name(Attributes)"(String Name, Int32 NumberOfAttributes), 
      //|        CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, ["??"]
      private static void DynamicPriorityRecognized(out Int32 constPriority, out IntMethodClass dynamicPriority, IntMethodClass intMethod)
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
      private static void EmptySemanticAction(out VoidMethodClass method) => method = null;

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
      private void CSvoidMethodRecognized(out VoidMethodClass voidMethod, MethodClass method)
         {
         if (!(method is VoidMethodClass))
            {
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error, $"Expected a void method, got {method.GetType()}");
            voidMethod = null;
            }
         else
            {
            voidMethod = (VoidMethodClass)method;
            }

         method.MethodParameters = LastFormalParameterList.ToArray();
         LastFormalParameterList.Clear();
         Lexer.SkipToEndOfCSLines();
         }

      //| /* The same definition as for CSvoidMethod is used for CSintMethod, but different semantics */
      //| CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), "(", eFormalParameters, ")"
      private void CSintMethodRecognized(out IntMethodClass intMethod, MethodClass method)
         {
         intMethod = (IntMethodClass)method;
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
      //|    Name(Int32 typeStringIndex), Name(Int32 nameStringIndex)
      private void FormalParameterWithTypeAndName(Int32 typeStringIndex, Int32 nameStringIndex)
          => FormalParameter(GlobalVariables.GetIndexOfEmptyString(), typeStringIndex, nameStringIndex);

      //|    | Name(Int32 ParameterModifierOptStringIndex), Name(Int32 typeStringIndex), Name(Int32 nameStringIndex)  /* ref or out, type, identifier */
      private void FormalParameterWithModifierTypeAndName(Int32 ParameterModifierOptStringIndex, Int32 typeStringIndex, Int32 nameStringIndex)
          => FormalParameter(ParameterModifierOptStringIndex, typeStringIndex, nameStringIndex);

      //| CSEnumDeclaration=
      //|    CSEnumProperties, optionalBaseType, CSEnumElements
      private void CSEnumRecognized() =>
          // all elements of the enum have been added to Enumlist by CSEnumElement
          Lexer.SkipToEndOfCSLines(); // allows some unchecked code after the enum

      //| CSEnumProperties=
      //|    Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), Name(Int32 enumStringIndex), CSEnumName(Int32 nameStringIndex)
      //|    | Name(Int32 modifierStringIndex), Name(Int32 enumStringIndex), CSEnumName(Int32 nameStringIndex)
      //|    | Name(Int32 enumStringIndex), CSEnumName(Int32 nameStringIndex)

      //| CSEnumName(Int32 nameStringIndex)= Name(Int32 nameStringIndex)
      private void EnumNameRecognized(Int32 nameStringIndex) =>
          EnumNameStringIndex = nameStringIndex; // Assign name to field

      //| optionalBaseType=
      //|    /* empty */
      //|    | ":", Name(Int32 Ignored);
      //|
      //| CSEnumElements=
      //|    "{", "}"
      private void NoEnumRecognized()
         {
         if (Enumlist == null)
            Enumlist = new List<Int32>(1);
         }
      //|    | "{", CSEnumElementList, "}"

      //| CSEnumElementList=
      //|    FirstCSEnumElement
      //|    | CSEnumElementList, Comma, CSEnumElement

      //| CSEnumElement=
      //|    Name(Int32 enumElementStringIndex)
      private void EnumElementRecognized(Int32 enumElementStringIndex)
          => (Enumlist ?? (Enumlist
         = new List<Int32>(50))).Add(enumElementStringIndex);

      //| FirstCSEnumElement= // allow assignment to the first enum element only
      //|   CSEnumElement
      //|   | CSEnumElement, "=", Name(Int32 ignored);
      //|
      //|   /* end of grammar */
      #endregion grammar
      /* ************************ code written by programmer ******************** */

      private bool ErrorHandler(Int32 stateNumber, String stateDescription, LexerResult symbol)
         {


         // _s.Discard(_s.Count - StateStackInitialCount); // need this ??
         var aCountBeforeAccept = _a.Count;
         Lexer.AcceptSymbol(); // accept the wrong symbol to make its position available in Lexer.Lex1TextPos and to discard it
         _a.Free(_a.Count - aCountBeforeAccept);  // discard the attributes of the discarded terminal symbol

         String nl = Environment.NewLine;
         P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Information,
             $"Grammar analysis error:{nl}input symbol \"{symbol.MyToString()}\" ignored: not allowed in state {stateNumber}{nl}{stateDescription}{nl}");
         return true; // true: continue analysis (goto state ...), else "goto EndWithError..."
         // TODO: design and implement a concept to insert a missing character (e.g. if state accepts only a single terminal symbol)
         }

#pragma warning disable CA1505 // Avoid unmaintainable code
#pragma warning disable CA1502 // Avoid excessive complexity
      private void AnalyzeGrammlatorGrammar()
#pragma warning restore CA1502 // Avoid excessive complexity
#pragma warning restore CA1505 // Avoid unmaintainable code
         {
         // Declare local variables used by grammlator generated code
         LexerResult ParserInput;
#pragma warning disable IDE0059 // Der Wert, der dem Symbol zugeordnet ist, wird niemals verwendet.
         /* ************************ end of code written by programmer ******************** */
#region grammlator generated Tue, 14 Apr 2020 19:10:09 GMT (grammlator, File version 2020.04.07.1 14.04.2020 19:09:56)
  Int32 StateStackInitialCount = _s.Count;
  Int32 AttributeStackInitialCount = _a.Count;
  /* State 1
   * *Startsymbol= ►GrammlatorGrammar;
   */
State2:
  /* State 2 (0)*/
  const String StateDescription2 =
       "GrammlatorGrammar= OptionalGrammlatorSettings, ►OptionalDeclarationOfTerminalSymbols, GrammarRuleList, TerminatorAtEndOfGrammar;\r\n"
     + "OptionalGrammlatorSettings= OptionalGrammlatorSettings, ►GrammlatorSetting;";
  _s.Push(0);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     goto State87;
     }
  if (ParserInput == LexerResult.StringResult)
     goto AcceptState82;
  if (ParserInput != LexerResult.StarEqual)
     {
     if (ErrorHandler(2, StateDescription2, ParserInput))
        goto State2;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.StarEqual);
State22:
  /* State 22 */
  const String StateDescription22 =
       "GrammlatorGrammar= OptionalGrammlatorSettings, OptionalDeclarationOfTerminalSymbols, ►GrammarRuleList, TerminatorAtEndOfGrammar;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.StarEqual)
     {
     if (ErrorHandler(22, StateDescription22, ParserInput))
        goto State22;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.StarEqual);
  Lexer.AcceptSymbol();
  /* Reduction 21
   * "*="= StarEqual;◄ method: StartOfFirstGrammarRule
   */

  StartOfFirstGrammarRule();

State23:
  /* State 23 (2)*/
  const String StateDescription23 =
       "FirstGrammarRule= \"*=\", ►outerDefinitions;";
  _s.Push(2);
  ParserInput = Lexer.PeekSymbol();
  switch (ParserInput)
  {
  // <= LexerResult.NumberSign: goto HandleError23 // see end of switch
  case LexerResult.GroupEnd:
  case LexerResult.RepeatEnd:
  case LexerResult.OptionEnd:
     goto HandleError23;
  case LexerResult.GroupStart:
     goto AcceptState74;
  case LexerResult.OptionStart:
     goto AcceptState72;
  case LexerResult.RepeatStart:
     goto AcceptState59;
  case LexerResult.DoubleQuestionmark:
     goto AcceptState48;
  case LexerResult.CSharpStart:
     goto AcceptState36;
  case LexerResult.Name:
  case LexerResult.StringResult:
     goto AcceptState82;
  // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce22 // see end of switch
  } // end of switch
  if (ParserInput <= LexerResult.NumberSign)
     goto HandleError23;
  Debug.Assert(ParserInput >= LexerResult.DefinitionSeparatorSymbol);

Reduce22:
  /* Reduction 22
   * EndOfDefinitionWithoutSemantics= ;◄ method: EndOfDefinitionWithoutSemanticsRecognized
   */

  EndOfDefinitionWithoutSemanticsRecognized();

State28:
  /* State 28 */
  const String StateDescription28 =
       "outerDefinitions= EndOfDefinitionWithoutSemantics, ►\";\";\r\n"
     + "outerDefinitions= EndOfDefinition, ►\"|\", outerDefinitionList;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState29;
  if (ParserInput < LexerResult.TerminatorSymbol)
     {
     if (ErrorHandler(28, StateDescription28, ParserInput))
        goto State28;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
AcceptBranch5:
  Lexer.AcceptSymbol();
Branch5:
  /* Branch 5*/
  if (_s.Peek() == 11)
     goto Reduce80;

Reduce24:
  /* Reduction 24, sStack: -1
   * FirstGrammarRule= "*=", outerDefinitions;◄ method: FirstGrammarRuleRecognized
   */
  _s.Pop();

  FirstGrammarRuleRecognized();

State76:
  /* State 76 (10)*/
  const String StateDescription76 =
       "GrammlatorGrammar= OptionalGrammlatorSettings, OptionalDeclarationOfTerminalSymbols, GrammarRuleList, ►TerminatorAtEndOfGrammar;\r\n"
     + "GrammarRuleList= GrammarRuleList, ►GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes);";
  _s.Push(10);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.NumberSign)
     {
     Lexer.AcceptSymbol();
     /* Reduction 73, sStack: -2
      * TerminatorAtEndOfGrammar= NumberSign;◄ method: TerminatorAtEndOfGrammar
      * then: GrammlatorGrammar= OptionalGrammlatorSettings, OptionalDeclarationOfTerminalSymbols, GrammarRuleList, TerminatorAtEndOfGrammar;◄
      * then: *Startsymbol= GrammlatorGrammar;◄
      */
     _s.Discard(2);

     TerminatorAtEndOfGrammar();

     goto EndOfGeneratedCode1;
     }
  if (ParserInput != LexerResult.Name && ParserInput != LexerResult.StringResult)
     {
     if (ErrorHandler(76, StateDescription76, ParserInput))
        goto State76;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.StringResult);
AcceptState82:
  Lexer.AcceptSymbol();
State82:
  /* State 82 (0)*/
  const String StateDescription82 =
       "\"Name(Attributes)\"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex), ►\"(Attributes)\"(Int32 NumberOfAttributes);\r\n"
     + "\"Name(Attributes)\"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex)●;";
  _s.Push(0);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.GroupStart)
     goto AcceptState83;
  if (ParserInput == LexerResult.Colon
     || (ParserInput >= LexerResult.CSharpEnd && ParserInput <= LexerResult.StarEqual)
     || ParserInput == LexerResult.NumberSign)
     {
     if (ErrorHandler(82, StateDescription82, ParserInput))
        goto State82;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput != LexerResult.Colon
     && (ParserInput < LexerResult.CSharpEnd || ParserInput > LexerResult.StarEqual)
     && ParserInput != LexerResult.NumberSign && ParserInput != LexerResult.GroupStart);
  /* Reduction 81, sStack: -1, aStack: 1
   * "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex);◄ Priority: -10, aStack: 1, method: NameWithoutAttributes
   */
  _s.Pop();
  _a.Allocate();

  NameWithoutAttributes(
     NumberOfAttributes: out _a.PeekRef(0)._Int32
     );

Branch16:
  /* Branch 16*/
  switch (_s.Peek())
  {
  case 0:
  case 1:
     goto State3;
  case 6:
     goto State70;
  case 10:
  {
     /* Reduction 75
      * outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes);◄ method: LeftSideOfOuterProduction
      */

     LeftSideOfOuterProduction(
        SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
        stringIndex: _a.PeekClear(-1)._Int32,
        NumberOfAttributes: _a.PeekRef(0)._Int32
        );

     goto State77;
     }
  /*case 2: case 3: case 4: case 5: case 7: case 8: case 9: case 11:
  default: break;
  */
  }
Reduce23:
  /* Reduction 23, aStack: -1
   * SimpleElement(Symbol Symbol)= "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes);◄ method: FoundSymbolnameInRightSide, aStack: -1
   */

  FoundSymbolnameInRightSide(
     Symbol: out _a.PeekRef(-1)._Symbol,
     stringIndex: _a.PeekClear(-1)._Int32,
     NumberOfAttributes: _a.PeekRef(0)._Int32
     );

  _a.Free();
State24:
  /* State 24 */
  const String StateDescription24 =
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
     /* Reduction 28
      * RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "?";◄ method: OptionalElementRecognized
      */

     OptionalElementRecognized(
        Symbol: ref _a.PeekRef(0)._Symbol
        );

     goto Branch3;
     }
  if (ParserInput == LexerResult.Asterisk)
     {
     Lexer.AcceptSymbol();
     goto State26;
     }
  if (ParserInput == LexerResult.Plus)
     {
     Lexer.AcceptSymbol();
     goto State25;
     }
  if (ParserInput <= LexerResult.MinusEqual
     || ParserInput == LexerResult.NumberSign)
     {
     if (ErrorHandler(24, StateDescription24, ParserInput))
        goto State24;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Comma
     || ParserInput >= LexerResult.GroupStart);
  /* Reduction 27, aStack: -1
   * Element= RepeatedElement(Symbol Symbol);◄ method: ElementVariantRecognized, aStack: -1
   */

  ElementVariantRecognized(
     Symbol: _a.PeekRef(0)._Symbol
     );

  _a.Free();
Branch2:
  /* Branch 2*/
  switch (_s.Peek())
  {
  case 5:
  {
     /* Reduction 40, sStack: -2
      * SequenceOfElements= SequenceOfElements, ","?, Element;◄
      */
     _s.Discard(2);
     goto Branch2;
     }
  case 6:
  case 7:
  case 8:
  case 9:
     goto State65;
  /*case 2: case 3: case 4: case 11:
  default: break;
  */
  }
State31:
  /* State 31 (1)*/
  const String StateDescription31 =
       "SequenceOfElements= SequenceOfElements, ►\",\"?, Element;\r\n"
     + "outerDefinitionList= SequenceOfElements, ►EndOfDefinition, \"|\", outerDefinitionList;\r\n"
     + "outerLastDefinitionOfSequence= SequenceOfElements, ►EndOfDefinitionWithoutSemantics, \";\";\r\n"
     + "outerLastDefinitionOfSequence= SequenceOfElements, ►EndOfDefinitionWithSemantics, \";\"?;";
  _s.Push(1);
  ParserInput = Lexer.PeekSymbol();
  switch (ParserInput)
  {
  // <= LexerResult.Plus: goto HandleError31 // see end of switch
  case LexerResult.NumberSign:
  case LexerResult.GroupEnd:
  case LexerResult.RepeatEnd:
  case LexerResult.OptionEnd:
     goto HandleError31;
  case LexerResult.Comma:
     goto AcceptState35;
  case LexerResult.GroupStart:
  case LexerResult.OptionStart:
  case LexerResult.RepeatStart:
  case LexerResult.Name:
  case LexerResult.StringResult:
     goto State35;
  case LexerResult.DoubleQuestionmark:
     goto AcceptState48;
  case LexerResult.CSharpStart:
     goto AcceptState36;
  // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce36 // see end of switch
  } // end of switch
  if (ParserInput <= LexerResult.Plus)
     goto HandleError31;
  Debug.Assert(ParserInput >= LexerResult.DefinitionSeparatorSymbol);

  /* Reduction 36
   * EndOfDefinitionWithoutSemantics= ;◄ method: EndOfDefinitionWithoutSemanticsRecognized
   */

  EndOfDefinitionWithoutSemanticsRecognized();

State32:
  /* State 32 */
  const String StateDescription32 =
       "outerDefinitionList= SequenceOfElements, EndOfDefinition, ►\"|\", outerDefinitionList;\r\n"
     + "outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithoutSemantics, ►\";\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState33;
  if (ParserInput < LexerResult.TerminatorSymbol)
     {
     if (ErrorHandler(32, StateDescription32, ParserInput))
        goto State32;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
AcceptReduce38:
  Lexer.AcceptSymbol();
Reduce38:
  /* Reduction 38, sStack: -1
   * outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithoutSemantics, ";";◄
   */
  _s.Pop();
  /* Branch 6*/
  switch (_s.Peek())
  {
  case 2:
     goto Reduce24;
  case 4:
     goto Reduce39;
  case 11:
     goto Reduce80;
  /*case 3:
  default: break;
  */
  }
Reduce35:
  /* Reduction 35, sStack: -1
   * outerDefinitions= EndOfDefinition, "|", outerDefinitionList;◄
   */
  _s.Pop();
  goto Branch5;

AcceptReduce8:
  Lexer.AcceptSymbol();
Reduce8:
  /* Reduction 8
   * CSEnumDeclaration= CSEnumProperties, optionalBaseType, CSEnumElements;◄ method: CSEnumRecognized
   */

  CSEnumRecognized();

State17:
  /* State 17 */
  const String StateDescription17 =
       "optionalEnum= CSharpStart, CSEnumDeclaration, ►CSharpEnd;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.CSharpEnd)
     {
     if (ErrorHandler(17, StateDescription17, ParserInput))
        goto State17;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.CSharpEnd);
  Lexer.AcceptSymbol();
Reduce5:
  /* Reduction 5
   * OptionalDeclarationOfTerminalSymbols= TerminalSymbolsList, SemikolonOrEnum;◄ method: CompareTerminalDeclarationsWithEnum
   */

  CompareTerminalDeclarationsWithEnum();

  goto State22;

Reduce25:
  /* Reduction 25, aStack: -1
   * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ method: OuterEndOfDefinitionRecognized, aStack: -1
   */

  OuterEndOfDefinitionRecognized(
     method: _a.PeekRef(0)._VoidMethodClass
     );

  _a.Free();
State30:
  /* State 30 */
  const String StateDescription30 =
       "outerDefinitions= EndOfDefinitionWithSemantics, ►\";\"?;\r\n"
     + "outerDefinitions= EndOfDefinition, ►\"|\", outerDefinitionList;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState29;
  if (ParserInput >= LexerResult.TerminatorSymbol)
     goto AcceptBranch5;
  if (ParserInput != LexerResult.NumberSign
     && ParserInput < LexerResult.Name)
     {
     if (ErrorHandler(30, StateDescription30, ParserInput))
        goto State30;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.NumberSign
     || ParserInput == LexerResult.Name || ParserInput == LexerResult.StringResult);
  goto Branch5;

AcceptReduce43:
  Lexer.AcceptSymbol();
  /* Reduction 43, sStack: -1
   * CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), "(", eFormalParameters, ")";◄ method: CSvoidMethodRecognized
   */
  _s.Pop();

  CSvoidMethodRecognized(
     voidMethod: out _a.PeekRef(0)._VoidMethodClass,
     method: _a.PeekClear(0)._MethodClass
     );

State44:
  /* State 44 */
  const String StateDescription44 =
       "SemanticAction(VoidMethodClass method)= CSharpStart, CSvoidMethod(VoidMethodClass method), ►CSharpEnd;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.CSharpEnd)
     {
     if (ErrorHandler(44, StateDescription44, ParserInput))
        goto State44;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.CSharpEnd);
  Lexer.AcceptSymbol();
  /* Reduction 47, sStack: -1
   * SemanticAction(VoidMethodClass method)= CSharpStart, CSvoidMethod(VoidMethodClass method), CSharpEnd;◄
   */
  _s.Pop();
Branch8:
  /* Branch 8*/
  switch (_s.Peek())
  {
  case 2:
  case 11:
     goto Reduce25;
  case 1:
  {
     /* Reduction 37, aStack: -1
      * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ method: OuterEndOfDefinitionRecognized, aStack: -1
      */

     OuterEndOfDefinitionRecognized(
        method: _a.PeekRef(0)._VoidMethodClass
        );

     _a.Free();
     goto State34;
     }
  case 6:
  {
     /* Reduction 59, aStack: -1
      * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ method: OuterEndOfDefinitionRecognized, aStack: -1
      */

     OuterEndOfDefinitionRecognized(
        method: _a.PeekRef(0)._VoidMethodClass
        );

     _a.Free();
     goto State61;
     }
  case 3:
  {
     /* Reduction 65, sStack: -1, aStack: -1
      * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ method: OuterEndOfDefinitionRecognized, aStack: -1
      * then: Definition= SequenceOfElements, EndOfDefinition;◄
      */
     _s.Pop();

     OuterEndOfDefinitionRecognized(
        method: _a.PeekRef(0)._VoidMethodClass
        );

     _a.Free();
     goto Branch15;
     }
  case 9:
  {
     /* Reduction 68, aStack: -1
      * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ method: OuterEndOfDefinitionRecognized, aStack: -1
      */

     OuterEndOfDefinitionRecognized(
        method: _a.PeekRef(0)._VoidMethodClass
        );

     _a.Free();
     goto State68;
     }
  /*case 0:
  default: break;
  */
  }
  /* Reduction 34, sStack: -1, aStack: -3
   * EndOfDefinitionWithSemantics= PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority), SemanticAction(VoidMethodClass method);◄ method: EndOfDefinitionRecognized, aStack: -3
   */
  _s.Pop();

  EndOfDefinitionRecognized(
     constPriority: _a.PeekRef(-2)._Int32,
     dynPriority: _a.PeekRef(-1)._IntMethodClass,
     method: _a.PeekRef(0)._VoidMethodClass
     );

  _a.Free(3);
Branch4:
  /* Branch 4*/
  switch (_s.Peek())
  {
  case 2:
  case 11:
     goto State30;
  case 6:
     goto State61;
  case 3:
  {
     /* Reduction 64, sStack: -1
      * Definition= SequenceOfElements, EndOfDefinition;◄
      */
     _s.Pop();
     goto Branch15;
     }
  case 9:
     goto State68;
  /*case 1:
  default: break;
  */
  }
State34:
  /* State 34 */
  const String StateDescription34 =
       "outerDefinitionList= SequenceOfElements, EndOfDefinition, ►\"|\", outerDefinitionList;\r\n"
     + "outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithSemantics, ►\";\"?;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState33;
  if (ParserInput >= LexerResult.TerminatorSymbol)
     goto AcceptReduce38;
  if (ParserInput != LexerResult.NumberSign
     && ParserInput < LexerResult.Name)
     {
     if (ErrorHandler(34, StateDescription34, ParserInput))
        goto State34;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.NumberSign
     || ParserInput == LexerResult.Name || ParserInput == LexerResult.StringResult);
  goto Reduce38;

AcceptReduce51:
  Lexer.AcceptSymbol();
  /* Reduction 51, sStack: -1
   * CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), "(", eFormalParameters, ")";◄ method: CSintMethodRecognized
   */
  _s.Pop();

  CSintMethodRecognized(
     intMethod: out _a.PeekRef(0)._IntMethodClass,
     method: _a.PeekClear(0)._MethodClass
     );

State54:
  /* State 54 */
  const String StateDescription54 =
       "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, CSintMethod(IntMethodClass intMethod), ►CSharpEnd, (Local1)?;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.CSharpEnd)
     {
     if (ErrorHandler(54, StateDescription54, ParserInput))
        goto State54;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.CSharpEnd);
  Lexer.AcceptSymbol();
State55:
  /* State 55 */
  const String StateDescription55 =
       "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, ►(Local1)?;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DoubleQuestionmark)
     {
     Lexer.AcceptSymbol();
     goto Reduce52;
     }
  if (ParserInput != LexerResult.NumberSign
     && ParserInput < LexerResult.CSharpStart)
     {
     if (ErrorHandler(55, StateDescription55, ParserInput))
        goto State55;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.NumberSign
     || ParserInput >= LexerResult.CSharpStart);
Reduce52:
  /* Reduction 52, sStack: -1, aStack: 1
   * PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= "??", CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, (Local1)?;◄ aStack: 1, method: DynamicPriorityRecognized
   */
  _s.Pop();
  _a.Allocate();

  DynamicPriorityRecognized(
     constPriority: out _a.PeekRef(-1)._Int32,
     dynamicPriority: out _a.PeekRef(0)._IntMethodClass,
     intMethod: _a.PeekClear(-1)._IntMethodClass
     );

State27:
  /* State 27 (0)*/
  const String StateDescription27 =
       "EndOfDefinitionWithSemantics= PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority)●;\r\n"
     + "EndOfDefinitionWithSemantics= PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority), ►SemanticAction(VoidMethodClass method);";
  _s.Push(0);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.CSharpStart)
     goto AcceptState36;
  if (ParserInput != LexerResult.NumberSign
     && ParserInput < LexerResult.GroupEnd)
     {
     if (ErrorHandler(27, StateDescription27, ParserInput))
        goto State27;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.NumberSign
     || ParserInput >= LexerResult.GroupEnd);
  /* Reduction 33, sStack: -1, aStack: -2
   * EndOfDefinitionWithSemantics= PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority);◄ method: EndOfDefinitionWithSemanticsRecognized, aStack: -2
   */
  _s.Pop();

  EndOfDefinitionWithSemanticsRecognized(
     constPriority: _a.PeekRef(-1)._Int32,
     dynPriority: _a.PeekRef(0)._IntMethodClass
     );

  _a.Free(2);
  goto Branch4;

Reduce55:
  /* Reduction 55, aStack: 5
   * SaveVariables(Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions, Int32 SavedNumberOfTrivialDefinitions, Int32 SavedNumberOfElements, Int32 SavedNumberOfSymbolAttributes)= ;◄ aStack: 5, method: SaveVariablesToAttributes
   */
  _a.Allocate(5);

  SaveVariablesToAttributes(
     SavedAttributeNumberAtStartOfDefinition: out _a.PeekRef(-4)._Int32,
     SavedNumberOfDefinitions: out _a.PeekRef(-3)._Int32,
     SavedNumberOfTrivialDefinitions: out _a.PeekRef(-2)._Int32,
     SavedNumberOfElements: out _a.PeekRef(-1)._Int32,
     SavedNumberOfSymbolAttributes: out _a.PeekRef(0)._Int32
     );

State60:
  /* State 60 (6)*/
  const String StateDescription60 =
       "NestedElement(Symbol Symbol)= SaveVariables(Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions, Int32 SavedNumberOfTrivialDefinitions, Int32 SavedNumberOfElements, Int32 SavedNumberOfSymbolAttributes), ►NestedGrammarRule(Symbol NestedSymbol, Int32 NumberOfAttributes);";
  _s.Push(6);
  ParserInput = Lexer.PeekSymbol();
  switch (ParserInput)
  {
  // <= LexerResult.NumberSign
  // >= LexerResult.TerminatorSymbol: goto HandleError60 // see end of switch
  case LexerResult.GroupStart:
     goto AcceptState74;
  case LexerResult.OptionStart:
     goto AcceptState72;
  case LexerResult.RepeatStart:
     goto AcceptState59;
  case LexerResult.DoubleQuestionmark:
     goto AcceptState48;
  case LexerResult.CSharpStart:
     goto AcceptState36;
  case LexerResult.GroupEnd:
  case LexerResult.RepeatEnd:
  case LexerResult.OptionEnd:
  case LexerResult.DefinitionSeparatorSymbol:
     {
     /* Reduction 56
      * EndOfDefinitionWithoutSemantics= ;◄ method: EndOfDefinitionWithoutSemanticsRecognized
      */

     EndOfDefinitionWithoutSemanticsRecognized();

     goto State61;
     }
  case LexerResult.Name:
  case LexerResult.StringResult:
     goto AcceptState82;
  } // end of switch
  Debug.Assert(ParserInput <= LexerResult.NumberSign || ParserInput >= LexerResult.TerminatorSymbol);

  if (ErrorHandler(60, StateDescription60, ParserInput))
     goto State60;
  goto EndWithError1;

Reduce58:
  /* Reduction 58, aStack: 2
   * NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions;◄ aStack: 2, method: NestedGrammarRuleWithEmptyLeftside
   */
  _a.Allocate(2);

  NestedGrammarRuleWithEmptyLeftside(
     SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
     NumberOfAttributes: out _a.PeekRef(0)._Int32
     );

Reduce57:
  /* Reduction 57, sStack: -1, aStack: -6
   * NestedElement(Symbol Symbol)= SaveVariables(Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions, Int32 SavedNumberOfTrivialDefinitions, Int32 SavedNumberOfElements, Int32 SavedNumberOfSymbolAttributes), NestedGrammarRule(Symbol NestedSymbol, Int32 NumberOfAttributes);◄ method: EndOfNestedGrammarRuleRecognized, aStack: -6
   */
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
  /* Branch 12*/
  switch (_s.Peek())
  {
  case 1:
     goto State73;
  case 2:
     goto State75;
  /*case 0:
  default: break;
  */
  }
State71:
  /* State 71 */
  const String StateDescription71 =
       "RepeatedElement(Symbol Symbol)= \"{\", NestedElement(Symbol Symbol), ►\"}\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.RepeatEnd)
     {
     if (ErrorHandler(71, StateDescription71, ParserInput))
        goto State71;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatEnd);
  Lexer.AcceptSymbol();
  /* Reduction 70, sStack: -1
   * RepeatedElement(Symbol Symbol)= "{", NestedElement(Symbol Symbol), "}";◄ method: RepeatGroupRecognized
   */
  _s.Pop();

  RepeatGroupRecognized(
     Symbol: ref _a.PeekRef(0)._Symbol
     );

Branch3:
  /* Branch 3*/
  switch (_s.Peek())
  {
  case 5:
  {
     /* Reduction 41, sStack: -2, aStack: -1
      * Element= RepeatedElement(Symbol Symbol);◄ method: ElementVariantRecognized, aStack: -1
      * then: SequenceOfElements= SequenceOfElements, ","?, Element;◄
      */
     _s.Discard(2);

     ElementVariantRecognized(
        Symbol: _a.PeekRef(0)._Symbol
        );

     _a.Free();
     goto Branch2;
     }
  case 6:
  case 7:
  case 8:
  case 9:
     goto Reduce60;
  /*case 2: case 3: case 4: case 11:
  default: break;
  */
  }
  /* Reduction 26, aStack: -1
   * Element= RepeatedElement(Symbol Symbol);◄ method: ElementVariantRecognized, aStack: -1
   */

  ElementVariantRecognized(
     Symbol: _a.PeekRef(0)._Symbol
     );

  _a.Free();
  goto State31;

Reduce60:
  /* Reduction 60, aStack: -1
   * Element= RepeatedElement(Symbol Symbol);◄ method: ElementVariantRecognized, aStack: -1
   */

  ElementVariantRecognized(
     Symbol: _a.PeekRef(0)._Symbol
     );

  _a.Free();
State65:
  /* State 65 (3)*/
  const String StateDescription65 =
       "Definition= SequenceOfElements, ►EndOfDefinition;\r\n"
     + "SequenceOfElements= SequenceOfElements, ►\",\"?, Element;";
  _s.Push(3);
  ParserInput = Lexer.PeekSymbol();
  switch (ParserInput)
  {
  // <= LexerResult.Plus
  // >= LexerResult.TerminatorSymbol: goto HandleError65 // see end of switch
  case LexerResult.NumberSign:
     goto HandleError65;
  case LexerResult.Comma:
     goto AcceptState35;
  case LexerResult.GroupStart:
  case LexerResult.OptionStart:
  case LexerResult.RepeatStart:
  case LexerResult.Name:
  case LexerResult.StringResult:
     goto State35;
  case LexerResult.DoubleQuestionmark:
     goto AcceptState48;
  case LexerResult.CSharpStart:
     goto AcceptState36;
  case LexerResult.GroupEnd:
  case LexerResult.RepeatEnd:
  case LexerResult.OptionEnd:
  case LexerResult.DefinitionSeparatorSymbol:
     {
     /* Reduction 63, sStack: -1
      * EndOfDefinitionWithoutSemantics= ;◄ method: EndOfDefinitionWithoutSemanticsRecognized
      * then: Definition= SequenceOfElements, EndOfDefinition;◄
      */
     _s.Pop();

     EndOfDefinitionWithoutSemanticsRecognized();

     goto Branch15;
     }
  } // end of switch
  Debug.Assert(ParserInput <= LexerResult.Plus || ParserInput >= LexerResult.TerminatorSymbol);

HandleError65:
  if (ErrorHandler(65, StateDescription65, ParserInput))
     goto State65;
  goto EndWithError1;

Reduce80:
  /* Reduction 80, sStack: -1
   * GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "=", outerDefinitions;◄
   */
  _s.Pop();
Reduce76:
  /* Reduction 76, sStack: -1, aStack: -2
   * GrammarRuleList= GrammarRuleList, GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes);◄ method: EndOfGrammarRuleRecognized, aStack: -2
   */
  _s.Pop();

  EndOfGrammarRuleRecognized(
     SymbolAtLeftSide: _a.PeekRef(-1)._Symbol
     );

  _a.Free(2);
  goto State76;

State3:
  /* State 3 */
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
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.CSharpStart
     || ParserInput >= LexerResult.DefinitionSeparatorSymbol);
  /* Reduction 3, aStack: 1
   * OptionalWeight(Int32 weight)= ;◄ aStack: 1, method: DefaultWeightOne
   */
  _a.Allocate();

  DefaultWeightOne(
     weight: out _a.PeekRef(0)._Int32
     );

Reduce4:
  /* Reduction 4, aStack: -3
   * TerminalSymbol= "Name(Attributes)"(Int32 nameIndex, Int32 NumberOfAttributes), OptionalWeight(Int32 Weight);◄ method: TerminalSymbol, aStack: -3
   */

  TerminalSymbol(
     nameIndex: _a.PeekRef(-2)._Int32,
     NumberOfAttributes: _a.PeekRef(-1)._Int32,
     Weight: _a.PeekRef(0)._Int32
     );

  _a.Free(3);
  /* Branch 1*/
  if (_s.Peek() == 1)
  {
     /* Reduction 7, sStack: -1
      * TerminalSymbolsList= TerminalSymbolsList, "|", TerminalSymbol;◄
      */
     _s.Pop();
     goto State5;
     }

State5:
  /* State 5 */
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
     {
     Lexer.AcceptSymbol();
     /* Reduction 6
      * NoEnum= ;◄ method: EnumOmitted
      */

     EnumOmitted();

     goto Reduce5;
     }
  if (ParserInput != LexerResult.CSharpStart)
     {
     if (ErrorHandler(5, StateDescription5, ParserInput))
        goto State5;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.CSharpStart);
  Lexer.AcceptSymbol();
State8:
  /* State 8 */
  const String StateDescription8 =
       "optionalEnum= CSharpStart, ►CSEnumDeclaration, CSharpEnd;\r\n"
     + "optionalEnum= CSharpStart, ►CSharpEnd, NoEnum;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     goto State18;
     }
  if (ParserInput != LexerResult.CSharpEnd)
     {
     if (ErrorHandler(8, StateDescription8, ParserInput))
        goto State8;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.CSharpEnd);
  Lexer.AcceptSymbol();
  /* Reduction 20
   * NoEnum= ;◄ method: EnumOmitted
   */

  EnumOmitted();

  goto Reduce5;

State4:
  /* State 4 */
  const String StateDescription4 =
       "OptionalWeight(Int32 weight)= \"%\", ►Number(Int32 weight);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Number)
     {
     if (ErrorHandler(4, StateDescription4, ParserInput))
        goto State4;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Number);
  Lexer.AcceptSymbol();
  goto Reduce4;

State7:
  /* State 7 (1)*/
  const String StateDescription7 =
       "TerminalSymbolsList= TerminalSymbolsList, \"|\", ►TerminalSymbol;";
  _s.Push(1);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name && ParserInput != LexerResult.StringResult)
     {
     if (ErrorHandler(7, StateDescription7, ParserInput))
        goto State7;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.StringResult);
  goto AcceptState82;

State9:
  /* State 9 */
  const String StateDescription9 =
       "CSEnumDeclaration= CSEnumProperties, ►optionalBaseType, CSEnumElements;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Colon)
     {
     Lexer.AcceptSymbol();
     goto State16;
     }
  if (ParserInput != LexerResult.RepeatStart)
     {
     if (ErrorHandler(9, StateDescription9, ParserInput))
        goto State9;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatStart);
State10:
  /* State 10 */
  const String StateDescription10 =
       "CSEnumDeclaration= CSEnumProperties, optionalBaseType, ►CSEnumElements;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.RepeatStart)
     {
     if (ErrorHandler(10, StateDescription10, ParserInput))
        goto State10;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatStart);
  Lexer.AcceptSymbol();
State11:
  /* State 11 */
  const String StateDescription11 =
       "CSEnumElements= \"{\", ►\"}\";\r\n"
     + "CSEnumElements= \"{\", ►CSEnumElementList, \"}\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     /* Reduction 10, aStack: -1
      * CSEnumElement= Name(Int32 enumElementStringIndex);◄ method: EnumElementRecognized, aStack: -1
      */

     EnumElementRecognized(
        enumElementStringIndex: _a.PeekRef(0)._Int32
        );

     _a.Free();
     goto State12;
     }
  if (ParserInput != LexerResult.RepeatEnd)
     {
     if (ErrorHandler(11, StateDescription11, ParserInput))
        goto State11;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatEnd);
  Lexer.AcceptSymbol();
  /* Reduction 9
   * CSEnumElements= "{", "}";◄ method: NoEnumRecognized
   */

  NoEnumRecognized();

  goto Reduce8;

State12:
  /* State 12 */
  const String StateDescription12 =
       "CSEnumElements= \"{\", CSEnumElementList, ►\"}\";\r\n"
     + "CSEnumElementList= CSEnumElementList, ►Comma, CSEnumElement;\r\n"
     + "FirstCSEnumElement= CSEnumElement, ►\"=\", Name(Int32 ignored);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Comma)
     goto AcceptState13;
  if (ParserInput <= LexerResult.DefiningSymbol)
     {
     Lexer.AcceptSymbol();
     goto State14;
     }
  if (ParserInput != LexerResult.RepeatEnd)
     {
     if (ErrorHandler(12, StateDescription12, ParserInput))
        goto State12;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatEnd);
  goto AcceptReduce8;

AcceptState13:
  Lexer.AcceptSymbol();
State13:
  /* State 13 */
  const String StateDescription13 =
       "CSEnumElementList= CSEnumElementList, Comma, ►CSEnumElement;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(13, StateDescription13, ParserInput))
        goto State13;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
  /* Reduction 11, aStack: -1
   * CSEnumElement= Name(Int32 enumElementStringIndex);◄ method: EnumElementRecognized, aStack: -1
   */

  EnumElementRecognized(
     enumElementStringIndex: _a.PeekRef(0)._Int32
     );

  _a.Free();
State15:
  /* State 15 */
  const String StateDescription15 =
       "CSEnumElements= \"{\", CSEnumElementList, ►\"}\";\r\n"
     + "CSEnumElementList= CSEnumElementList, ►Comma, CSEnumElement;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Comma)
     goto AcceptState13;
  if (ParserInput != LexerResult.RepeatEnd)
     {
     if (ErrorHandler(15, StateDescription15, ParserInput))
        goto State15;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatEnd);
  goto AcceptReduce8;

State14:
  /* State 14 */
  const String StateDescription14 =
       "FirstCSEnumElement= CSEnumElement, \"=\", ►Name(Int32 ignored);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(14, StateDescription14, ParserInput))
        goto State14;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
  /* Reduction 12, aStack: -1
   * FirstCSEnumElement= CSEnumElement, "=", Name(Int32 ignored);◄ aStack: -1
   */
  _a.Free();
  goto State15;

State16:
  /* State 16 */
  const String StateDescription16 =
       "optionalBaseType= \":\", ►Name(Int32 Ignored);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(16, StateDescription16, ParserInput))
        goto State16;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
  /* Reduction 13, aStack: -1
   * optionalBaseType= ":", Name(Int32 Ignored);◄ aStack: -1
   */
  _a.Free();
  goto State10;

State18:
  /* State 18 */
  const String StateDescription18 =
       "CSEnumProperties= Name(Int32 modifier1StringIndex), ►Name(Int32 modifier2StringIndex), Name(Int32 enumStringIndex), CSEnumName(Int32 nameStringIndex);\r\n"
     + "CSEnumProperties= Name(Int32 modifierStringIndex), ►Name(Int32 enumStringIndex), CSEnumName(Int32 nameStringIndex);\r\n"
     + "CSEnumProperties= Name(Int32 enumStringIndex), ►CSEnumName(Int32 nameStringIndex);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(18, StateDescription18, ParserInput))
        goto State18;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
State19:
  /* State 19 */
  const String StateDescription19 =
       "CSEnumProperties= Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), ►Name(Int32 enumStringIndex), CSEnumName(Int32 nameStringIndex);\r\n"
     + "CSEnumProperties= Name(Int32 modifierStringIndex), Name(Int32 enumStringIndex), ►CSEnumName(Int32 nameStringIndex);\r\n"
     + "CSEnumName(Int32 nameStringIndex)= Name(Int32 nameStringIndex)●;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     goto State20;
     }
  if (ParserInput != LexerResult.Colon
     && ParserInput != LexerResult.RepeatStart)
     {
     if (ErrorHandler(19, StateDescription19, ParserInput))
        goto State19;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Colon
     || ParserInput == LexerResult.RepeatStart);
  /* Reduction 15, aStack: -2
   * CSEnumName(Int32 nameStringIndex)= Name(Int32 nameStringIndex);◄ method: EnumNameRecognized
   * then: CSEnumProperties= Name(Int32 enumStringIndex), CSEnumName(Int32 nameStringIndex);◄ aStack: -2
   */

  EnumNameRecognized(
     nameStringIndex: _a.PeekRef(0)._Int32
     );

  _a.Free(2);
  goto State9;

State20:
  /* State 20 */
  const String StateDescription20 =
       "CSEnumProperties= Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), Name(Int32 enumStringIndex), ►CSEnumName(Int32 nameStringIndex);\r\n"
     + "CSEnumName(Int32 nameStringIndex)= Name(Int32 nameStringIndex)●;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     /* Reduction 18, aStack: -4
      * CSEnumName(Int32 nameStringIndex)= Name(Int32 nameStringIndex);◄ method: EnumNameRecognized
      * then: CSEnumProperties= Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), Name(Int32 enumStringIndex), CSEnumName(Int32 nameStringIndex);◄ aStack: -4
      */

     EnumNameRecognized(
        nameStringIndex: _a.PeekRef(0)._Int32
        );

     _a.Free(4);
     goto State9;
     }
  if (ParserInput != LexerResult.Colon
     && ParserInput != LexerResult.RepeatStart)
     {
     if (ErrorHandler(20, StateDescription20, ParserInput))
        goto State20;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Colon
     || ParserInput == LexerResult.RepeatStart);
  /* Reduction 17, aStack: -3
   * CSEnumName(Int32 nameStringIndex)= Name(Int32 nameStringIndex);◄ method: EnumNameRecognized
   * then: CSEnumProperties= Name(Int32 modifierStringIndex), Name(Int32 enumStringIndex), CSEnumName(Int32 nameStringIndex);◄ aStack: -3
   */

  EnumNameRecognized(
     nameStringIndex: _a.PeekRef(0)._Int32
     );

  _a.Free(3);
  goto State9;

State25:
  /* State 25 */
  const String StateDescription25 =
       "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"+\"●;\r\n"
     + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"+\", ►\"+\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Plus)
     {
     Lexer.AcceptSymbol();
     /* Reduction 30
      * RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "+", "+";◄ method: Repeat1rrRecognized
      */

     Repeat1rrRecognized(
        Symbol: ref _a.PeekRef(0)._Symbol
        );

     goto Branch3;
     }
  if (ParserInput <= LexerResult.Asterisk
     || ParserInput == LexerResult.NumberSign)
     {
     if (ErrorHandler(25, StateDescription25, ParserInput))
        goto State25;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Comma
     || ParserInput >= LexerResult.GroupStart);
  /* Reduction 29
   * RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "+";◄ method: Repeat1lrRecognized
   */

  Repeat1lrRecognized(
     Symbol: ref _a.PeekRef(0)._Symbol
     );

  goto Branch3;

State26:
  /* State 26 */
  const String StateDescription26 =
       "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"*\"●;\r\n"
     + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"*\", ►\"*\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Asterisk)
     {
     Lexer.AcceptSymbol();
     /* Reduction 32
      * RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "*", "*";◄ method: Repeat0rrRecognized
      */

     Repeat0rrRecognized(
        Symbol: ref _a.PeekRef(0)._Symbol
        );

     goto Branch3;
     }
  if (ParserInput <= LexerResult.Plus
     || ParserInput == LexerResult.NumberSign)
     {
     if (ErrorHandler(26, StateDescription26, ParserInput))
        goto State26;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Comma
     || ParserInput >= LexerResult.GroupStart);
  /* Reduction 31
   * RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "*";◄ method: Repeat0lrRecognized
   */

  Repeat0lrRecognized(
     Symbol: ref _a.PeekRef(0)._Symbol
     );

  goto Branch3;

AcceptState29:
  Lexer.AcceptSymbol();
State29:
  /* State 29 (3)*/
  const String StateDescription29 =
       "outerDefinitions= EndOfDefinition, \"|\", ►outerDefinitionList;";
  _s.Push(3);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.GroupStart)
     goto AcceptState74;
  if (ParserInput == LexerResult.OptionStart)
     goto AcceptState72;
  if (ParserInput == LexerResult.RepeatStart)
     goto AcceptState59;
  if (ParserInput != LexerResult.Name && ParserInput != LexerResult.StringResult)
     {
     if (ErrorHandler(29, StateDescription29, ParserInput))
        goto State29;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.StringResult);
  goto AcceptState82;

AcceptState33:
  Lexer.AcceptSymbol();
State33:
  /* State 33 (4)*/
  const String StateDescription33 =
       "outerDefinitionList= SequenceOfElements, EndOfDefinition, \"|\", ►outerDefinitionList;";
  _s.Push(4);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.GroupStart)
     goto AcceptState74;
  if (ParserInput == LexerResult.OptionStart)
     goto AcceptState72;
  if (ParserInput == LexerResult.RepeatStart)
     goto AcceptState59;
  if (ParserInput != LexerResult.Name && ParserInput != LexerResult.StringResult)
     {
     if (ErrorHandler(33, StateDescription33, ParserInput))
        goto State33;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.StringResult);
  goto AcceptState82;

AcceptState35:
  Lexer.AcceptSymbol();
State35:
  /* State 35 (5)*/
  const String StateDescription35 =
       "SequenceOfElements= SequenceOfElements, \",\"?, ►Element;";
  _s.Push(5);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.GroupStart)
     goto AcceptState74;
  if (ParserInput == LexerResult.OptionStart)
     goto AcceptState72;
  if (ParserInput == LexerResult.RepeatStart)
     goto AcceptState59;
  if (ParserInput != LexerResult.Name && ParserInput != LexerResult.StringResult)
     {
     if (ErrorHandler(35, StateDescription35, ParserInput))
        goto State35;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.StringResult);
  goto AcceptState82;

AcceptState36:
  Lexer.AcceptSymbol();
State36:
  /* State 36 (0)*/
  const String StateDescription36 =
       "SemanticAction(VoidMethodClass method)= CSharpStart, ►CSvoidMethod(VoidMethodClass method), CSharpEnd;\r\n"
     + "SemanticAction(VoidMethodClass method)= CSharpStart, ►CSharpEnd;";
  _s.Push(0);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState45;
  if (ParserInput != LexerResult.CSharpEnd)
     {
     if (ErrorHandler(36, StateDescription36, ParserInput))
        goto State36;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.CSharpEnd);
  Lexer.AcceptSymbol();
  /* Reduction 42, sStack: -1, aStack: 1
   * SemanticAction(VoidMethodClass method)= CSharpStart, CSharpEnd;◄ aStack: 1, method: EmptySemanticAction
   */
  _s.Pop();
  _a.Allocate();

  EmptySemanticAction(
     method: out _a.PeekRef(0)._VoidMethodClass
     );

  goto Branch8;

State37:
  /* State 37 */
  const String StateDescription37 =
       "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), ►\"(\", eFormalParameters, \")\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.GroupStart)
     {
     if (ErrorHandler(37, StateDescription37, ParserInput))
        goto State37;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupStart);
  Lexer.AcceptSymbol();
State38:
  /* State 38 (0)*/
  const String StateDescription38 =
       "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), \"(\", ►eFormalParameters, \")\";";
  _s.Push(0);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState42;
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(38, StateDescription38, ParserInput))
        goto State38;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
State41:
  /* State 41 */
  const String StateDescription41 =
       "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), \"(\", eFormalParameters, ►\")\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(41, StateDescription41, ParserInput))
        goto State41;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
  goto AcceptReduce43;

State39:
  /* State 39 */
  const String StateDescription39 =
       "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), \"(\", eFormalParameters, ►\")\";\r\n"
     + "formalParameters= formalParameters, ►Comma, formalParameter;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Comma)
     goto AcceptState40;
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(39, StateDescription39, ParserInput))
        goto State39;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
  goto AcceptReduce43;

AcceptState40:
  Lexer.AcceptSymbol();
State40:
  /* State 40 (1)*/
  const String StateDescription40 =
       "formalParameters= formalParameters, Comma, ►formalParameter;";
  _s.Push(1);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(40, StateDescription40, ParserInput))
        goto State40;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
AcceptState42:
  Lexer.AcceptSymbol();
State42:
  /* State 42 */
  const String StateDescription42 =
       "formalParameter= Name(Int32 typeStringIndex), ►Name(Int32 nameStringIndex);\r\n"
     + "formalParameter= Name(Int32 ParameterModifierOptStringIndex), ►Name(Int32 typeStringIndex), Name(Int32 nameStringIndex);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(42, StateDescription42, ParserInput))
        goto State42;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
State43:
  /* State 43 */
  const String StateDescription43 =
       "formalParameter= Name(Int32 typeStringIndex), Name(Int32 nameStringIndex)●;\r\n"
     + "formalParameter= Name(Int32 ParameterModifierOptStringIndex), Name(Int32 typeStringIndex), ►Name(Int32 nameStringIndex);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     /* Reduction 46, aStack: -3
      * formalParameter= Name(Int32 ParameterModifierOptStringIndex), Name(Int32 typeStringIndex), Name(Int32 nameStringIndex);◄ method: FormalParameterWithModifierTypeAndName, aStack: -3
      */

     FormalParameterWithModifierTypeAndName(
        ParameterModifierOptStringIndex: _a.PeekRef(-2)._Int32,
        typeStringIndex: _a.PeekRef(-1)._Int32,
        nameStringIndex: _a.PeekRef(0)._Int32
        );

     _a.Free(3);
     goto Branch10;
     }
  if (ParserInput != LexerResult.Comma
     && ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(43, StateDescription43, ParserInput))
        goto State43;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Comma
     || ParserInput == LexerResult.GroupEnd);
  /* Reduction 45, aStack: -2
   * formalParameter= Name(Int32 typeStringIndex), Name(Int32 nameStringIndex);◄ method: FormalParameterWithTypeAndName, aStack: -2
   */

  FormalParameterWithTypeAndName(
     typeStringIndex: _a.PeekRef(-1)._Int32,
     nameStringIndex: _a.PeekRef(0)._Int32
     );

  _a.Free(2);
Branch10:
  /* Branch 10*/
  switch (_s.Peek())
  {
  case 0:
     goto State39;
  case 2:
     goto State52;
  /*case 1:
  default: break;
  */
  }
  /* Reduction 44, sStack: -1
   * formalParameters= formalParameters, Comma, formalParameter;◄
   */
  _s.Pop();
  /* Branch 9*/
  if (_s.Peek() == 0)
     goto State39;

State52:
  /* State 52 */
  const String StateDescription52 =
       "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), \"(\", eFormalParameters, ►\")\";\r\n"
     + "formalParameters= formalParameters, ►Comma, formalParameter;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Comma)
     goto AcceptState40;
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(52, StateDescription52, ParserInput))
        goto State52;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
  goto AcceptReduce51;

AcceptState45:
  Lexer.AcceptSymbol();
State45:
  /* State 45 */
  const String StateDescription45 =
       "CSMethodProperties(MethodClass method)= Name(Int32 methodTypeStringIndex), ►Name(Int32 methodNameStringIndex);\r\n"
     + "CSMethodProperties(MethodClass method)= Name(Int32 modifierStringIndex), ►Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex);\r\n"
     + "CSMethodProperties(MethodClass method)= Name(Int32 modifier1StringIndex), ►Name(Int32 modifier2StringIndex), Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(45, StateDescription45, ParserInput))
        goto State45;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
State46:
  /* State 46 */
  const String StateDescription46 =
       "CSMethodProperties(MethodClass method)= Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex)●;\r\n"
     + "CSMethodProperties(MethodClass method)= Name(Int32 modifierStringIndex), Name(Int32 methodTypeStringIndex), ►Name(Int32 methodNameStringIndex);\r\n"
     + "CSMethodProperties(MethodClass method)= Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), ►Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     goto State47;
     }
  if (ParserInput != LexerResult.GroupStart)
     {
     if (ErrorHandler(46, StateDescription46, ParserInput))
        goto State46;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupStart);
  /* Reduction 48, aStack: -1
   * CSMethodProperties(MethodClass method)= Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex);◄ method: MethodTypeAndNameRecognized, aStack: -1
   */

  MethodTypeAndNameRecognized(
     method: out _a.PeekRef(-1)._MethodClass,
     methodTypeStringIndex: _a.PeekClear(-1)._Int32,
     methodNameStringIndex: _a.PeekRef(0)._Int32
     );

  _a.Free();
Branch11:
  /* Branch 11*/
  if (_s.Peek() == 0)
     goto State37;

State50:
  /* State 50 */
  const String StateDescription50 =
       "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), ►\"(\", eFormalParameters, \")\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.GroupStart)
     {
     if (ErrorHandler(50, StateDescription50, ParserInput))
        goto State50;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupStart);
  Lexer.AcceptSymbol();
State51:
  /* State 51 (2)*/
  const String StateDescription51 =
       "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), \"(\", ►eFormalParameters, \")\";";
  _s.Push(2);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState42;
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(51, StateDescription51, ParserInput))
        goto State51;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
State53:
  /* State 53 */
  const String StateDescription53 =
       "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), \"(\", eFormalParameters, ►\")\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(53, StateDescription53, ParserInput))
        goto State53;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
  goto AcceptReduce51;

State47:
  /* State 47 */
  const String StateDescription47 =
       "CSMethodProperties(MethodClass method)= Name(Int32 modifierStringIndex), Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex)●;\r\n"
     + "CSMethodProperties(MethodClass method)= Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), Name(Int32 methodTypeStringIndex), ►Name(Int32 methodNameStringIndex);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     /* Reduction 50, aStack: -3
      * CSMethodProperties(MethodClass method)= Name(Int32 modifier1StringIndex), Name(Int32 modifier2StringIndex), Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex);◄ method: MethodModifierTypeAndNameRecognized2, aStack: -3
      */

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
     if (ErrorHandler(47, StateDescription47, ParserInput))
        goto State47;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupStart);
  /* Reduction 49, aStack: -2
   * CSMethodProperties(MethodClass method)= Name(Int32 modifierStringIndex), Name(Int32 methodTypeStringIndex), Name(Int32 methodNameStringIndex);◄ method: MethodModifierTypeAndNameRecognized, aStack: -2
   */

  MethodModifierTypeAndNameRecognized(
     method: out _a.PeekRef(-2)._MethodClass,
     modifierStringIndex: _a.PeekClear(-2)._Int32,
     methodTypeStringIndex: _a.PeekRef(-1)._Int32,
     methodNameStringIndex: _a.PeekRef(0)._Int32
     );

  _a.Free(2);
  goto Branch11;

AcceptState48:
  Lexer.AcceptSymbol();
State48:
  /* State 48 */
  const String StateDescription48 =
       "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", ►signedNumber(Int32 constPriority), \"??\";\r\n"
     + "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", ►CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, (Local1)?;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.CSharpStart)
     {
     Lexer.AcceptSymbol();
     goto State49;
     }
  if (ParserInput == LexerResult.Minus)
     {
     Lexer.AcceptSymbol();
     goto State58;
     }
  if (ParserInput == LexerResult.Number)
     goto AcceptState57;
  if (ParserInput != LexerResult.Plus)
     {
     if (ErrorHandler(48, StateDescription48, ParserInput))
        goto State48;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Plus);
  Lexer.AcceptSymbol();
State56:
  /* State 56 */
  const String StateDescription56 =
       "signedNumber(Int32 value)= \"+\", ►Number(Int32 value);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Number)
     {
     if (ErrorHandler(56, StateDescription56, ParserInput))
        goto State56;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Number);
AcceptState57:
  Lexer.AcceptSymbol();
State57:
  /* State 57 */
  const String StateDescription57 =
       "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", signedNumber(Int32 constPriority), ►\"??\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.DoubleQuestionmark)
     {
     if (ErrorHandler(57, StateDescription57, ParserInput))
        goto State57;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.DoubleQuestionmark);
  Lexer.AcceptSymbol();
  /* Reduction 53, aStack: 1
   * PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= "??", signedNumber(Int32 constPriority), "??";◄ aStack: 1, method: ConstantPriorityGiven
   */
  _a.Allocate();

  ConstantPriorityGiven(
     dynamicPriority: out _a.PeekRef(0)._IntMethodClass
     );

  goto State27;

State49:
  /* State 49 (1)*/
  const String StateDescription49 =
       "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, ►CSintMethod(IntMethodClass intMethod), CSharpEnd, (Local1)?;";
  _s.Push(1);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(49, StateDescription49, ParserInput))
        goto State49;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  goto AcceptState45;

State58:
  /* State 58 */
  const String StateDescription58 =
       "signedNumber(Int32 value)= \"-\", ►Number(Int32 value);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Number)
     {
     if (ErrorHandler(58, StateDescription58, ParserInput))
        goto State58;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Number);
  Lexer.AcceptSymbol();
  /* Reduction 54
   * signedNumber(Int32 value)= "-", Number(Int32 value);◄ method: NegateNumber
   */

  NegateNumber(
     value: ref _a.PeekRef(0)._Int32
     );

  goto State57;

AcceptState59:
  Lexer.AcceptSymbol();
  /* State 59 (0)
   * RepeatedElement(Symbol Symbol)= "{", ►NestedElement(Symbol Symbol), "}";
   */
  _s.Push(0);
  goto Reduce55;

State61:
  /* State 61 */
  const String StateDescription61 =
       "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions●;\r\n"
     + "NestedDefinitions= EndOfDefinition, ►\"|\", NestedDefinitionList;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState62;
  if (ParserInput <= LexerResult.CSharpStart
     || ParserInput >= LexerResult.Name)
     {
     if (ErrorHandler(61, StateDescription61, ParserInput))
        goto State61;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
  goto Reduce58;

AcceptState62:
  Lexer.AcceptSymbol();
State62:
  /* State 62 (7)*/
  const String StateDescription62 =
       "NestedDefinitions= EndOfDefinition, \"|\", ►NestedDefinitionList;";
  _s.Push(7);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.GroupStart)
     goto AcceptState74;
  if (ParserInput == LexerResult.OptionStart)
     goto AcceptState72;
  if (ParserInput == LexerResult.RepeatStart)
     goto AcceptState59;
  if (ParserInput != LexerResult.Name && ParserInput != LexerResult.StringResult)
     {
     if (ErrorHandler(62, StateDescription62, ParserInput))
        goto State62;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.StringResult);
  goto AcceptState82;

State63:
  /* State 63 */
  const String StateDescription63 =
       "NestedDefinitions= EndOfDefinition, \"|\", NestedDefinitionList●;\r\n"
     + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState64;
  if (ParserInput <= LexerResult.CSharpStart
     || ParserInput >= LexerResult.Name)
     {
     if (ErrorHandler(63, StateDescription63, ParserInput))
        goto State63;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
  /* Reduction 61, sStack: -1
   * NestedDefinitions= EndOfDefinition, "|", NestedDefinitionList;◄
   */
  _s.Pop();
  /* Branch 13*/
  if (_s.Peek() == 6)
     goto Reduce58;

Reduce67:
  /* Reduction 67, sStack: -1
   * NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions;◄
   */
  _s.Pop();
  goto Reduce57;

AcceptState64:
  Lexer.AcceptSymbol();
State64:
  /* State 64 (8)*/
  const String StateDescription64 =
       "NestedDefinitionList= NestedDefinitionList, \"|\", ►Definition;";
  _s.Push(8);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.GroupStart)
     goto AcceptState74;
  if (ParserInput == LexerResult.OptionStart)
     goto AcceptState72;
  if (ParserInput == LexerResult.RepeatStart)
     goto AcceptState59;
  if (ParserInput != LexerResult.Name && ParserInput != LexerResult.StringResult)
     {
     if (ErrorHandler(64, StateDescription64, ParserInput))
        goto State64;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.StringResult);
  goto AcceptState82;

State66:
  /* State 66 */
  const String StateDescription66 =
       "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions●;\r\n"
     + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState64;
  if (ParserInput <= LexerResult.CSharpStart
     || ParserInput >= LexerResult.Name)
     {
     if (ErrorHandler(66, StateDescription66, ParserInput))
        goto State66;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
  goto Reduce58;

State67:
  /* State 67 (9)*/
  const String StateDescription67 =
       "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►NestedDefinitions;";
  _s.Push(9);
  ParserInput = Lexer.PeekSymbol();
  switch (ParserInput)
  {
  // <= LexerResult.NumberSign
  // >= LexerResult.TerminatorSymbol: goto HandleError67 // see end of switch
  case LexerResult.GroupStart:
     goto AcceptState74;
  case LexerResult.OptionStart:
     goto AcceptState72;
  case LexerResult.RepeatStart:
     goto AcceptState59;
  case LexerResult.DoubleQuestionmark:
     goto AcceptState48;
  case LexerResult.CSharpStart:
     goto AcceptState36;
  case LexerResult.GroupEnd:
  case LexerResult.RepeatEnd:
  case LexerResult.OptionEnd:
  case LexerResult.DefinitionSeparatorSymbol:
     {
     /* Reduction 66
      * EndOfDefinitionWithoutSemantics= ;◄ method: EndOfDefinitionWithoutSemanticsRecognized
      */

     EndOfDefinitionWithoutSemanticsRecognized();

     goto State68;
     }
  case LexerResult.Name:
  case LexerResult.StringResult:
     goto AcceptState82;
  } // end of switch
  Debug.Assert(ParserInput <= LexerResult.NumberSign || ParserInput >= LexerResult.TerminatorSymbol);

  if (ErrorHandler(67, StateDescription67, ParserInput))
     goto State67;
  goto EndWithError1;

State68:
  /* State 68 */
  const String StateDescription68 =
       "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions●;\r\n"
     + "NestedDefinitions= EndOfDefinition, ►\"|\", NestedDefinitionList;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState62;
  if (ParserInput <= LexerResult.CSharpStart
     || ParserInput >= LexerResult.Name)
     {
     if (ErrorHandler(68, StateDescription68, ParserInput))
        goto State68;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
  goto Reduce67;

State69:
  /* State 69 */
  const String StateDescription69 =
       "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions●;\r\n"
     + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState64;
  if (ParserInput <= LexerResult.CSharpStart
     || ParserInput >= LexerResult.Name)
     {
     if (ErrorHandler(69, StateDescription69, ParserInput))
        goto State69;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
  goto Reduce67;

State70:
  /* State 70 */
  const String StateDescription70 =
       "NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= \"Name(Attributes)\"(Int32 stringIndex, Int32 NumberOfAttributes), ►\"=\";\r\n"
     + "SimpleElement(Symbol Symbol)= \"Name(Attributes)\"(Int32 stringIndex, Int32 NumberOfAttributes)●;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput <= LexerResult.DefiningSymbol)
     {
     Lexer.AcceptSymbol();
     /* Reduction 69
      * NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes), "=";◄ method: LeftSideOfNestedProduction
      */

     LeftSideOfNestedProduction(
        SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
        stringIndex: _a.PeekClear(-1)._Int32,
        NumberOfAttributes: _a.PeekRef(0)._Int32
        );

     goto State67;
     }
  if (ParserInput <= LexerResult.MinusEqual
     || ParserInput == LexerResult.NumberSign
     || ParserInput >= LexerResult.TerminatorSymbol)
     {
     if (ErrorHandler(70, StateDescription70, ParserInput))
        goto State70;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput > LexerResult.MinusEqual
     && ParserInput != LexerResult.NumberSign
     && ParserInput < LexerResult.TerminatorSymbol);
  goto Reduce23;

AcceptState72:
  Lexer.AcceptSymbol();
  /* State 72 (1)
   * RepeatedElement(Symbol Symbol)= "[", ►NestedElement(Symbol Symbol), "]";
   */
  _s.Push(1);
  goto Reduce55;

State73:
  /* State 73 */
  const String StateDescription73 =
       "RepeatedElement(Symbol Symbol)= \"[\", NestedElement(Symbol Symbol), ►\"]\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.OptionEnd)
     {
     if (ErrorHandler(73, StateDescription73, ParserInput))
        goto State73;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.OptionEnd);
  Lexer.AcceptSymbol();
  /* Reduction 71, sStack: -1
   * RepeatedElement(Symbol Symbol)= "[", NestedElement(Symbol Symbol), "]";◄ method: OptionGroupRecognized
   */
  _s.Pop();

  OptionGroupRecognized(
     Symbol: ref _a.PeekRef(0)._Symbol
     );

  goto Branch3;

AcceptState74:
  Lexer.AcceptSymbol();
  /* State 74 (2)
   * SimpleElement(Symbol Symbol)= "(", ►NestedElement(Symbol Symbol), ")";
   */
  _s.Push(2);
  goto Reduce55;

State75:
  /* State 75 */
  const String StateDescription75 =
       "SimpleElement(Symbol Symbol)= \"(\", NestedElement(Symbol Symbol), ►\")\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(75, StateDescription75, ParserInput))
        goto State75;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
  Lexer.AcceptSymbol();
  /* Reduction 72, sStack: -1
   * SimpleElement(Symbol Symbol)= "(", NestedElement(Symbol Symbol), ")";◄
   */
  _s.Pop();
  goto State24;

State77:
  /* State 77 */
  const String StateDescription77 =
       "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►\"=\", outerDefinitions;\r\n"
     + "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►\"-=\", ListOfExcludedTerminalSymbols, \";\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput <= LexerResult.DefiningSymbol)
     {
     Lexer.AcceptSymbol();
     goto State81;
     }
  if (ParserInput != LexerResult.MinusEqual)
     {
     if (ErrorHandler(77, StateDescription77, ParserInput))
        goto State77;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.MinusEqual);
  Lexer.AcceptSymbol();
State78:
  /* State 78 */
  const String StateDescription78 =
       "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"-=\", ►ListOfExcludedTerminalSymbols, \";\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(78, StateDescription78, ParserInput))
        goto State78;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
  /* Reduction 77, aStack: -1
   * ListOfExcludedTerminalSymbols= Name(Int32 stringIndex);◄ method: FirstExcludedTerminalSymbol, aStack: -1
   */

  FirstExcludedTerminalSymbol(
     stringIndex: _a.PeekRef(0)._Int32
     );

  _a.Free();
State79:
  /* State 79 */
  const String StateDescription79 =
       "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"-=\", ListOfExcludedTerminalSymbols, ►\";\";\r\n"
     + "ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, ►\"|\", Name(Int32 nameIndex);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     {
     Lexer.AcceptSymbol();
     goto State80;
     }
  if (ParserInput < LexerResult.TerminatorSymbol)
     {
     if (ErrorHandler(79, StateDescription79, ParserInput))
        goto State79;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
  Lexer.AcceptSymbol();
  /* Reduction 78
   * GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "-=", ListOfExcludedTerminalSymbols, ";";◄ method: EndOfListOfExcludedTerminalSymbols
   */

  EndOfListOfExcludedTerminalSymbols();

  goto Reduce76;

State80:
  /* State 80 */
  const String StateDescription80 =
       "ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, \"|\", ►Name(Int32 nameIndex);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(80, StateDescription80, ParserInput))
        goto State80;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
  /* Reduction 79, aStack: -1
   * ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, "|", Name(Int32 nameIndex);◄ method: OneMoreExcludedTerminalSymbol, aStack: -1
   */

  OneMoreExcludedTerminalSymbol(
     nameIndex: _a.PeekRef(0)._Int32
     );

  _a.Free();
  goto State79;

State81:
  /* State 81 (11)*/
  const String StateDescription81 =
       "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"=\", ►outerDefinitions;";
  _s.Push(11);
  ParserInput = Lexer.PeekSymbol();
  switch (ParserInput)
  {
  // <= LexerResult.NumberSign: goto HandleError81 // see end of switch
  case LexerResult.GroupEnd:
  case LexerResult.RepeatEnd:
  case LexerResult.OptionEnd:
     goto HandleError81;
  case LexerResult.GroupStart:
     goto AcceptState74;
  case LexerResult.OptionStart:
     goto AcceptState72;
  case LexerResult.RepeatStart:
     goto AcceptState59;
  case LexerResult.DoubleQuestionmark:
     goto AcceptState48;
  case LexerResult.CSharpStart:
     goto AcceptState36;
  case LexerResult.Name:
  case LexerResult.StringResult:
     goto AcceptState82;
  // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce22 // see end of switch
  } // end of switch
  if (ParserInput >= LexerResult.DefinitionSeparatorSymbol)
     goto Reduce22;
  Debug.Assert(ParserInput <= LexerResult.NumberSign);

HandleError81:
  if (ErrorHandler(81, StateDescription81, ParserInput))
     goto State81;
  goto EndWithError1;

AcceptState83:
  Lexer.AcceptSymbol();
State83:
  /* State 83 (0)*/
  const String StateDescription83 =
       "\"(Attributes)\"(Int32 numberOfAttributes)= \"(\", ►\")\";\r\n"
     + "\"(Attributes)\"(Int32 numberOfAttributes)= \"(\", ►\"Attributes)\"(Int32 numberOfAttributes, Int32 smallestNumber);";
  _s.Push(0);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState86;
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(83, StateDescription83, ParserInput))
        goto State83;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
  Lexer.AcceptSymbol();
  /* Reduction 83, sStack: -1, aStack: 1
   * "(Attributes)"(Int32 numberOfAttributes)= "(", ")";◄ aStack: 1, method: EmptyListOfAttributes
   */
  _s.Pop();
  _a.Allocate();

  EmptyListOfAttributes(
     numberOfAttributes: out _a.PeekRef(0)._Int32
     );

Branch17:
  /* Branch 17*/
  if (_s.Peek() == 1)
  {
     /* Reduction 89, sStack: -1
      * "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex), "(Attributes)"(Int32 NumberOfAttributes);◄
      */
     _s.Pop();
     goto State3;
     }
  /* Reduction 82, sStack: -1
   * "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex), "(Attributes)"(Int32 NumberOfAttributes);◄
   */
  _s.Pop();
  goto Branch16;

State84:
  /* State 84 */
  const String StateDescription84 =
       "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 numberBeforeGroup), ►Comma, \"Attributes)\"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup);\r\n"
     + "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 number), ►\")\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Comma)
     {
     Lexer.AcceptSymbol();
     goto State85;
     }
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(84, StateDescription84, ParserInput))
        goto State84;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
  Lexer.AcceptSymbol();
  /* Reduction 85, aStack: 1
   * "Attributes)"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 number), ")";◄ aStack: 1, method: FirstAttributeOfGroup
   */
  _a.Allocate();

  FirstAttributeOfGroup(
     numberOfAttributesOfGroup: out _a.PeekRef(-1)._Int32,
     smallestNumber: out _a.PeekRef(0)._Int32,
     number: _a.PeekRef(-1)._Int32
     );

Branch18:
  /* Branch 18*/
  if (_s.Peek() == 1)
  {
     /* Reduction 86, sStack: -1, aStack: -1
      * "Attributes)"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 numberBeforeGroup), Comma, "Attributes)"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup);◄ method: AnotherAttributeOfGroup, aStack: -1
      */
     _s.Pop();

     AnotherAttributeOfGroup(
        numberOfAttributesOfGroup: out _a.PeekRef(-2)._Int32,
        smallestNumber: out _a.PeekRef(-1)._Int32,
        numberBeforeGroup: _a.PeekRef(-2)._Int32,
        numberOfAttributesOfRightGroup: _a.PeekRef(-1)._Int32,
        smallestNumberOfRightGroup: _a.PeekRef(0)._Int32
        );

     _a.Free();
     goto Branch18;
     }
  /* Reduction 84, sStack: -1, aStack: -1
   * "(Attributes)"(Int32 numberOfAttributes)= "(", "Attributes)"(Int32 numberOfAttributes, Int32 smallestNumber);◄ aStack: -1
   */
  _s.Pop();
  _a.Free();
  goto Branch17;

State85:
  /* State 85 (1)*/
  const String StateDescription85 =
       "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 numberBeforeGroup), Comma, ►\"Attributes)\"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup);";
  _s.Push(1);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(85, StateDescription85, ParserInput))
        goto State85;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
AcceptState86:
  Lexer.AcceptSymbol();
State86:
  /* State 86 */
  const String StateDescription86 =
       "Attribut(Int32 number)= Name(Int32 typeStringIndex), ►Name(Int32 nameStringIndex);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(86, StateDescription86, ParserInput))
        goto State86;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
  /* Reduction 87, aStack: -1
   * Attribut(Int32 number)= Name(Int32 typeStringIndex), Name(Int32 nameStringIndex);◄ method: AttributeTypeAndName, aStack: -1
   */

  AttributeTypeAndName(
     number: out _a.PeekRef(-1)._Int32,
     typeStringIndex: _a.PeekRef(-1)._Int32,
     nameStringIndex: _a.PeekRef(0)._Int32
     );

  _a.Free();
  goto State84;

State87:
  /* State 87 (1)*/
  const String StateDescription87 =
       "GrammlatorSetting= Name(Int32 nameIndex), ►\":\", StringResult(Int32 stringIndex), \";\"?;\r\n"
     + "\"Name(Attributes)\"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex), ►\"(Attributes)\"(Int32 NumberOfAttributes);\r\n"
     + "\"Name(Attributes)\"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex)●;";
  _s.Push(1);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Colon)
     {
     Lexer.AcceptSymbol();
     goto State88;
     }
  if (ParserInput == LexerResult.GroupStart)
     goto AcceptState83;
  if (ParserInput != LexerResult.Percent
     && ParserInput != LexerResult.CSharpStart
     && ParserInput < LexerResult.DefinitionSeparatorSymbol)
     {
     if (ErrorHandler(87, StateDescription87, ParserInput))
        goto State87;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Percent
     || ParserInput == LexerResult.CSharpStart
     || ParserInput >= LexerResult.DefinitionSeparatorSymbol);
  /* Reduction 88, sStack: -1, aStack: 1
   * "Name(Attributes)"(Int32 stringIndex, Int32 NumberOfAttributes)= ExtendedName(Int32 stringIndex);◄ Priority: -10, aStack: 1, method: NameWithoutAttributes
   */
  _s.Pop();
  _a.Allocate();

  NameWithoutAttributes(
     NumberOfAttributes: out _a.PeekRef(0)._Int32
     );

  goto State3;

State88:
  /* State 88 */
  const String StateDescription88 =
       "GrammlatorSetting= Name(Int32 nameIndex), \":\", ►StringResult(Int32 stringIndex), \";\"?;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.StringResult)
     {
     if (ErrorHandler(88, StateDescription88, ParserInput))
        goto State88;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.StringResult);
  Lexer.AcceptSymbol();
State89:
  /* State 89 */
  const String StateDescription89 =
       "GrammlatorSetting= Name(Int32 nameIndex), \":\", StringResult(Int32 stringIndex), ►\";\"?;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput >= LexerResult.TerminatorSymbol)
     {
     Lexer.AcceptSymbol();
     goto Reduce90;
     }
  if (ParserInput != LexerResult.StarEqual
     && ParserInput != LexerResult.Name && ParserInput != LexerResult.StringResult)
     {
     if (ErrorHandler(89, StateDescription89, ParserInput))
        goto State89;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.StarEqual
     || ParserInput == LexerResult.Name || ParserInput == LexerResult.StringResult);
Reduce90:
  /* Reduction 90, sStack: -2, aStack: -2
   * GrammlatorSetting= Name(Int32 nameIndex), ":", StringResult(Int32 stringIndex), ";"?;◄ method: SetGrammlatorSetting, aStack: -2
   * then: OptionalGrammlatorSettings= OptionalGrammlatorSettings, GrammlatorSetting;◄
   */
  _s.Discard(2);

  SetGrammlatorSetting(
     nameIndex: _a.PeekRef(-1)._Int32,
     stringIndex: _a.PeekRef(0)._Int32
     );

  _a.Free(2);
  goto State2;

Reduce39:
  /* Reduction 39, sStack: -2
   * outerDefinitionList= SequenceOfElements, EndOfDefinition, "|", outerDefinitionList;◄
   */
  _s.Discard(2);
  /* Branch 7*/
  switch (_s.Peek())
  {
  case 3:
     goto Reduce35;
  case 4:
     goto Reduce39;
  case 11:
     goto Reduce80;
  /*case 2:
  default: break;
  */
  }
  goto Reduce24;

Branch15:
  /* Branch 15*/
  switch (_s.Peek())
  {
  case 6:
     goto State66;
  case 7:
     goto State63;
  case 9:
     goto State69;
  /*case 8:
  default: break;
  */
  }
  /* Reduction 62, sStack: -1
   * NestedDefinitionList= NestedDefinitionList, "|", Definition;◄
   */
  _s.Pop();
  /* Branch 14*/
  switch (_s.Peek())
  {
  case 6:
     goto State66;
  case 9:
     goto State69;
  /*case 7:
  default: break;
  */
  }
  goto State63;

HandleError23:
  if (ErrorHandler(23, StateDescription23, ParserInput))
     goto State23;
  goto EndWithError1;

HandleError31:
  if (ErrorHandler(31, StateDescription31, ParserInput))
     goto State31;
  goto EndWithError1;

EndWithError1:
  // This point is reached after an input error has been found
  _s.Discard(_s.Count - StateStackInitialCount);
  _a.Free(_a.Count - AttributeStackInitialCount);

EndOfGeneratedCode1:
  ;
#endregion grammlator generated Tue, 14 Apr 2020 19:10:09 GMT (grammlator, File version 2020.04.07.1 14.04.2020 19:09:56)
         /* ************************ code written by programmer ******************** */
#pragma warning restore IDE0059 // Der Wert, der dem Symbol zugeordnet ist, wird niemals verwendet.
         }
      }
   }
