using GrammlatorRuntime;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Collections;

namespace Grammlator
{
   /// <summary>
   /// Grammlator Parser (uses Lexer which uses InputClassifier)
   /// </summary>
   internal sealed partial class P1aParser : GrammlatorApplication
   {
      /// <summary>
      /// Create an instance of the parser and execute it
      /// </summary>
      /// <param name="SbResult"></param>
      /// <param name="SourceReader"></param>
      /// <param name="symbolDictionary"></param>
      public static void MakeInstanceAndExecute(
          SpanReaderWithCharacterAndLineCounter SourceReader,
          Dictionary<String, Symbol> symbolDictionary)
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
          Dictionary<String, Symbol> SymbolDictionary) : base(initialSizeOfAttributeStack: 100, initialSizeOfStateStack: 100)
      {
         this.SymbolDictionary = SymbolDictionary;

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
                  $"Missing \"{EndregionString} {GrammarString}\"");
            }

            GlobalVariables.NumberOfNonterminalSymbols = SymbolDictionary.Count - GlobalVariables.NumberOfTerminalSymbols;

            // Check usage of symbols
            if (CheckUsageOfSymbols(P1OutputMessageAndLexerPosition) >= MessageTypeOrDestinationEnum.Error)
            {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Abort, "translation aborted: error(s) in source"); // throws an exception
               Debug.Fail("This debug instruction should never be executed");
            }
         }
         finally { }
      }

      private readonly P1bLexer Lexer;

      #region declaration of fields
      /// <summary>
      /// Set by constructor, contains at the first places the terminal symbols followed by the nonterminal symbols
      /// </summary>
      private readonly Dictionary<String, Symbol> SymbolDictionary;

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
      //|     | Name(String name)%25
      //|     | StringResult(String s)%23
      //|     | DefinitionSeparatorSymbol%25  
      //|     | TerminatorSymbol%22
      public enum LexerResultCopy
      { // Defines the output of the lexer, which is assigned to Symbol to be used by the parser
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

         Name /* (string s) */,
         StringResult /* (String s) */,

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
      //|    Name(String name), ":", StringResult(String value), ";"?
      private void SetGrammlatorSetting(String name, String value)
      {
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
                $"The number of elements in enum \"{EnumName}\" differs from the number of terminal symbols."
                );
         }
         Int32 index = 0;
         foreach (String name in SymbolDictionary.Keys)
         {
            if (name != Enumlist[index])
            {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                   $"The name \"{name}\" in the terminal definition and the corresponding name \"{Enumlist[index - 1]}\" in the enum are different.");
               // break; // no break: test for more errors in the enum
            }
            if (++index >= Enumlist.Count)
               break;
         }
         Enumlist.Clear();
      }

      //|  TerminalSymbolsList=
      //|     TerminalSymbol
      //|     | TerminalSymbolsList, "|", TerminalSymbol

      //|  TerminalSymbol=
      //|      "Name(Attributes)"(String Name, Int32 NumberOfAttributes), OptionalWeight(Int32 Weight)
      private void TerminalSymbol(String Name, Int32 NumberOfAttributes, Int32 Weight)
          => TerminalSymbolDeclaration(Name, NumberOfAttributes, Weight);

      //|  OptionalWeight(Int32 weight)=
      //|      /* empty */
      private static void DefaultWeightOne(out Int32 weight) => weight = 1;
      //|     | "%", Number(Int32 weight)

      //|  ExtendedName (String s)=
      //|       Name(String s)
      //|     | StringResult(String s)
      private static void MakeStringToName(ref String s) => s = "\"" + s + "\"";

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
         EnumName = null;
      }

      //| NoEnum=
      private void EnumOmitted()
      {
         Enumlist = null;
         EnumName = null;
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
            TerminalSymbolDeclaration("*", numberOfAttributes: 0, weight: 0);
         GlobalVariables.NumberOfTerminalSymbols = SymbolDictionary.Count;
         GlobalVariables.DefineArrayTerminalSymbolByIndex(SymbolDictionary);
         OptimizeTrivialDefinitionsBackup = OptimizeTrivialDefinitions;
         OptimizeTrivialDefinitions = false; // must be disabled while evaluating the definitions of the startsymbol
         SymbolDictionary.Add(GlobalVariables.Startsymbol.Identifier, GlobalVariables.Startsymbol);
      }

      private Boolean OptimizeTrivialDefinitionsBackup;

      //|  GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)=
      //|      outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "=", outerDefinitions
      //|     | outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "-=" ListOfExcludedTerminalSymbols, ";"
      private void EndOfListOfExcludedTerminalSymbols() => EvaluateExcludedTerminalSymbols(ExcludedTerminalSymbols);

      //| ListOfExcludedTerminalSymbols=
      //|      Name(String name)
      private void FirstExcludedTerminalSymbol(String name)
      {
         if (ExcludedTerminalSymbols == null || ExcludedTerminalSymbols.Length != GlobalVariables.NumberOfTerminalSymbols)
            ExcludedTerminalSymbols = new BitArray(GlobalVariables.NumberOfTerminalSymbols);
         ExcludedTerminalSymbols.SetAll(false);
         OneMoreExcludedTerminalSymbol(name);
      }

      private BitArray ExcludedTerminalSymbols;

      //|     | ListOfExcludedTerminalSymbols, "|", Name(String name)
      private void OneMoreExcludedTerminalSymbol(String name)
      {
         if (!SymbolDictionary.TryGetValue(name, out Symbol Symbol))
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
         SymbolAtLeftSide = NonterminalSymbolDefinition(MakeNewName("Local"), 0);
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

      //|  "Name(Attributes)" (String name, Int32 NumberOfAttributes)=
      //|       ExtendedName(String name), "(Attributes)"(Int32 NumberOfAttributes)
      //|     | ExtendedName(String name) ??-10?? // low priority: if "(" follows then assume that attributes follow
      private static void NameWithoutAttributes(out Int32 NumberOfAttributes) => NumberOfAttributes = 0;

      //|  outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)=
      //|     "Name(Attributes)"(String Name, Int32 NumberOfAttributes)
      private void LeftSideOfOuterProduction(out Symbol SymbolAtLeftSide, String Name, Int32 NumberOfAttributes)
          => SymbolAtLeftSide = NonterminalSymbolDefinition(Name, NumberOfAttributes);

      //|  NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)=
      //|        "Name(Attributes)"(String eLeftSideName, Int32 NumberOfAttributes), "="
      private void LeftSideOfNestedProduction(out Symbol SymbolAtLeftSide, String eLeftSideName, Int32 NumberOfAttributes)
          => SymbolAtLeftSide = NonterminalSymbolDefinition(eLeftSideName, NumberOfAttributes); // same as LeftSideOfOuterProduction();

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
      //|    | "Name(Attributes)"(String Name, Int32 NumberOfAttributes)
      private void FoundSymbolnameInRightSide(out Symbol Symbol, String Name, Int32 NumberOfAttributes)
          => Symbol = EvaluateSymbolnameFoundInRightSide(Name, NumberOfAttributes);

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
      //|    Name(String type), Name(String name)
      private void AttributeTypeAndName(out Int32 number, String type, String name)
      {
         AttributeCounter++;
         PushAttributeToListOfAttributesOfGrammarRule(type, name);
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
      //|    Name(String methodType), Name(String methodName)
      private void MethodTypeAndNameRecognized(out MethodClass method, String methodType, String methodName)
          => MethodProperties(out method, "", methodType, methodName);

      //|    | Name(String modifier), Name(String methodType), Name(String methodName) /* AccessModifier (private, public ...), type, methodname */
      private void MethodModifierTypeAndNameRecognized(
          out MethodClass method, String modifier, String methodType, String methodName)
          => MethodProperties(out method, modifier, methodType, methodName);

      //|    | Name(String modifier1), Name(String modifier2), Name(String methodType), Name(String methodName) /* AccessModifier (private, public ...), type, methodname */
      private void MethodModifierTypeAndNameRecognized2(
          out MethodClass method, String modifier1, String modifier2, String methodType, String methodName)
          => MethodProperties(out method, modifier1, modifier2, methodType, methodName);

      //| eFormalParameters=
      //|    /* empty */
      //|    | formalParameters

      //| formalParameters=
      //|    formalParameter
      //|    | formalParameters, Comma, formalParameter

      //| formalParameter=
      //|    Name(String type), Name(String name)
      private void FormalParameterWithTypeAndName(String type, String name)
          => FormalParameter("", type, name);

      //|    | Name(String ParameterModifierOpt), Name(String type), Name(String name)  /* ref or out, type, identifier */
      private void FormalParameterWithModifierTypeAndName(String ParameterModifierOpt, String type, String name)
          => FormalParameter(ParameterModifierOpt, type, name);

      //| CSEnumDeclaration=
      //|    CSEnumProperties, optionalBaseType, CSEnumElements
      private void CSEnumRecognized() =>
          // all elements of the enum have been added to Enumlist by CSEnumElement
          Lexer.SkipToEndOfCSLines(); // allows some unchecked code after the enum

      //| CSEnumProperties=
      //|    Name(String modifier1), Name(String modifier2), Name(String enum), CSEnumName(String name)
      //|    | Name(String modifier), Name(String enum), CSEnumName(String name)
      //|    | Name(String enum), CSEnumName(String name)

      //| CSEnumName(String name)= Name(String name)
      private void EnumNameRecognized(String name) =>
          EnumName = name; // Assign name to field

      //| optionalBaseType=
      //|    /* empty */
      //|    | ":", Name(String Ignored);
      //|
      //| CSEnumElements=
      //|    "{", "}"
      private void NoEnumRecognized()
      {
         if (Enumlist == null)
            Enumlist = new List<String>(1);
      }
      //|    | "{", CSEnumElementList, "}"

      //| CSEnumElementList=
      //|    FirstCSEnumElement
      //|    | CSEnumElementList, Comma, CSEnumElement

      //| CSEnumElement=
      //|    Name(String enumElement)
      private void EnumElementRecognized(String enumElement)
          => (Enumlist ?? (Enumlist = new List<String>(50))).Add(enumElement);

      //| FirstCSEnumElement= // allow assignment to the first enum element only
      //|   CSEnumElement
      //|   | CSEnumElement, "=", Name(String ignored);
      //|
      //|   /* end of grammar */
      #endregion grammar
      /* ************************ code written by programmer ******************** */

      private bool ErrorHandler(Int32 stateNumber, String stateDescription, LexerResult symbol)
      {
         String nl = Environment.NewLine;
         Lexer.AcceptSymbol(); // accept the wrong symbol to make its position available in Lexer.Lex1TextPos
         P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Abort,
             $"Grammar analysis error:{nl}input symbol \"{symbol.MyToString()}\" not allowed in state {stateNumber}{nl}{stateDescription}{nl}");
         return false;
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
#region grammlator generated 26.11.2019 by Grammlator version 0:21 (build 26.11.2019 23:39:18 +00:00)
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
     goto AcceptReduce2;
  if (ParserInput != LexerResult.StarEqual)
     {
     if (ErrorHandler(2, StateDescription2, ParserInput))
        goto State2;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.StarEqual);
State27:
  /* State 27 */
  const String StateDescription27 =
       "GrammlatorGrammar= OptionalGrammlatorSettings, OptionalDeclarationOfTerminalSymbols, ►GrammarRuleList, TerminatorAtEndOfGrammar;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.StarEqual)
     {
     if (ErrorHandler(27, StateDescription27, ParserInput))
        goto State27;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.StarEqual);
  Lexer.AcceptSymbol();
  /* Reduction 29
   * "*="= StarEqual;◄ method: StartOfFirstGrammarRule
   */

  StartOfFirstGrammarRule();

State28:
  /* State 28 (2)*/
  const String StateDescription28 =
       "FirstGrammarRule= \"*=\", ►outerDefinitions;";
  _s.Push(2);
  ParserInput = Lexer.PeekSymbol();
  switch (ParserInput)
  {
  // <= LexerResult.NumberSign: goto HandleError28 // see end of switch
  case LexerResult.GroupEnd:
  case LexerResult.RepeatEnd:
  case LexerResult.OptionEnd:
     goto HandleError28;
  case LexerResult.GroupStart:
     goto AcceptState79;
  case LexerResult.OptionStart:
     goto AcceptState77;
  case LexerResult.RepeatStart:
     goto AcceptState64;
  case LexerResult.DoubleQuestionmark:
     goto AcceptState53;
  case LexerResult.CSharpStart:
     goto AcceptState41;
  case LexerResult.Name:
     goto AcceptState3;
  case LexerResult.StringResult:
     goto AcceptReduce2;
  // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce30 // see end of switch
  } // end of switch
  if (ParserInput <= LexerResult.NumberSign)
     goto HandleError28;
  Debug.Assert(ParserInput >= LexerResult.DefinitionSeparatorSymbol);

Reduce30:
  /* Reduction 30
   * EndOfDefinitionWithoutSemantics= ;◄ method: EndOfDefinitionWithoutSemanticsRecognized
   */

  EndOfDefinitionWithoutSemanticsRecognized();

State33:
  /* State 33 */
  const String StateDescription33 =
       "outerDefinitions= EndOfDefinitionWithoutSemantics, ►\";\";\r\n"
     + "outerDefinitions= EndOfDefinition, ►\"|\", outerDefinitionList;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState34;
  if (ParserInput < LexerResult.TerminatorSymbol)
     {
     if (ErrorHandler(33, StateDescription33, ParserInput))
        goto State33;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
AcceptBranch8:
  Lexer.AcceptSymbol();
Branch8:
  /* Branch 8*/
  if (_s.Peek() == 11)
     goto Reduce88;

Reduce32:
  /* Reduction 32, sStack: -1
   * FirstGrammarRule= "*=", outerDefinitions;◄ method: FirstGrammarRuleRecognized
   */
  _s.Pop();

  FirstGrammarRuleRecognized();

State81:
  /* State 81 (10)*/
  const String StateDescription81 =
       "GrammlatorGrammar= OptionalGrammlatorSettings, OptionalDeclarationOfTerminalSymbols, GrammarRuleList, ►TerminatorAtEndOfGrammar;\r\n"
     + "GrammarRuleList= GrammarRuleList, ►GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes);";
  _s.Push(10);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState3;
  if (ParserInput == LexerResult.StringResult)
     goto AcceptReduce2;
  if (ParserInput != LexerResult.NumberSign)
     {
     if (ErrorHandler(81, StateDescription81, ParserInput))
        goto State81;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.NumberSign);
  Lexer.AcceptSymbol();
  /* Reduction 81, sStack: -2
   * TerminatorAtEndOfGrammar= NumberSign;◄ method: TerminatorAtEndOfGrammar
   * then: GrammlatorGrammar= OptionalGrammlatorSettings, OptionalDeclarationOfTerminalSymbols, GrammarRuleList, TerminatorAtEndOfGrammar;◄
   * then: *Startsymbol= GrammlatorGrammar;◄
   */
  _s.Discard(2);

  TerminatorAtEndOfGrammar();

  goto EndOfGeneratedCode1;

AcceptReduce2:
  Lexer.AcceptSymbol();
  /* Reduction 2
   * ExtendedName(String s)= StringResult(String s);◄ method: MakeStringToName
   */

  MakeStringToName(
     s: ref _a.PeekRef(0)._String
     );

State3:
  /* State 3 (0)*/
  const String StateDescription3 =
       "\"Name(Attributes)\"(String name, Int32 NumberOfAttributes)= ExtendedName(String name), ►\"(Attributes)\"(Int32 NumberOfAttributes);\r\n"
     + "\"Name(Attributes)\"(String name, Int32 NumberOfAttributes)= ExtendedName(String name)●;";
  _s.Push(0);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.GroupStart)
     goto AcceptState4;
  if (ParserInput == LexerResult.Colon
     || (ParserInput >= LexerResult.CSharpEnd && ParserInput <= LexerResult.StarEqual)
     || ParserInput == LexerResult.NumberSign)
     {
     if (ErrorHandler(3, StateDescription3, ParserInput))
        goto State3;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput != LexerResult.Colon
     && (ParserInput < LexerResult.CSharpEnd || ParserInput > LexerResult.StarEqual)
     && ParserInput != LexerResult.NumberSign && ParserInput != LexerResult.GroupStart);
  /* Reduction 4, sStack: -1, aStack: 1
   * "Name(Attributes)"(String name, Int32 NumberOfAttributes)= ExtendedName(String name);◄ Priority: -10, aStack: 1, method: NameWithoutAttributes
   */
  _s.Pop();
  _a.Allocate();

  NameWithoutAttributes(
     NumberOfAttributes: out _a.PeekRef(0)._Int32
     );

Branch1:
  /* Branch 1*/
  switch (_s.Peek())
  {
  case 0:
  case 1:
     goto State8;
  case 6:
     goto State75;
  case 10:
  {
     /* Reduction 83
      * outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= "Name(Attributes)"(String Name, Int32 NumberOfAttributes);◄ method: LeftSideOfOuterProduction
      */

     LeftSideOfOuterProduction(
        SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
        Name: _a.PeekClear(-1)._String,
        NumberOfAttributes: _a.PeekRef(0)._Int32
        );

     goto State82;
     }
  /*case 2: case 3: case 4: case 5: case 7: case 8: case 9: case 11:
  default: break;
  */
  }
Reduce31:
  /* Reduction 31, aStack: -1
   * SimpleElement(Symbol Symbol)= "Name(Attributes)"(String Name, Int32 NumberOfAttributes);◄ method: FoundSymbolnameInRightSide, aStack: -1
   */

  FoundSymbolnameInRightSide(
     Symbol: out _a.PeekRef(-1)._Symbol,
     Name: _a.PeekClear(-1)._String,
     NumberOfAttributes: _a.PeekRef(0)._Int32
     );

  _a.Free();
State29:
  /* State 29 */
  const String StateDescription29 =
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
     /* Reduction 36
      * RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "?";◄ method: OptionalElementRecognized
      */

     OptionalElementRecognized(
        Symbol: ref _a.PeekRef(0)._Symbol
        );

     goto Branch6;
     }
  if (ParserInput == LexerResult.Asterisk)
     {
     Lexer.AcceptSymbol();
     goto State31;
     }
  if (ParserInput == LexerResult.Plus)
     {
     Lexer.AcceptSymbol();
     goto State30;
     }
  if (ParserInput <= LexerResult.MinusEqual
     || ParserInput == LexerResult.NumberSign)
     {
     if (ErrorHandler(29, StateDescription29, ParserInput))
        goto State29;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Comma
     || ParserInput >= LexerResult.GroupStart);
  /* Reduction 35, aStack: -1
   * Element= RepeatedElement(Symbol Symbol);◄ method: ElementVariantRecognized, aStack: -1
   */

  ElementVariantRecognized(
     Symbol: _a.PeekRef(0)._Symbol
     );

  _a.Free();
Branch5:
  /* Branch 5*/
  switch (_s.Peek())
  {
  case 5:
  {
     /* Reduction 48, sStack: -2
      * SequenceOfElements= SequenceOfElements, ","?, Element;◄
      */
     _s.Discard(2);
     goto Branch5;
     }
  case 6:
  case 7:
  case 8:
  case 9:
     goto State70;
  /*case 2: case 3: case 4: case 11:
  default: break;
  */
  }
State36:
  /* State 36 (1)*/
  const String StateDescription36 =
       "SequenceOfElements= SequenceOfElements, ►\",\"?, Element;\r\n"
     + "outerDefinitionList= SequenceOfElements, ►EndOfDefinition, \"|\", outerDefinitionList;\r\n"
     + "outerLastDefinitionOfSequence= SequenceOfElements, ►EndOfDefinitionWithoutSemantics, \";\";\r\n"
     + "outerLastDefinitionOfSequence= SequenceOfElements, ►EndOfDefinitionWithSemantics, \";\"?;";
  _s.Push(1);
  ParserInput = Lexer.PeekSymbol();
  switch (ParserInput)
  {
  // <= LexerResult.Plus: goto HandleError36 // see end of switch
  case LexerResult.NumberSign:
  case LexerResult.GroupEnd:
  case LexerResult.RepeatEnd:
  case LexerResult.OptionEnd:
     goto HandleError36;
  case LexerResult.Comma:
     goto AcceptState40;
  case LexerResult.GroupStart:
  case LexerResult.OptionStart:
  case LexerResult.RepeatStart:
  case LexerResult.Name:
  case LexerResult.StringResult:
     goto State40;
  case LexerResult.DoubleQuestionmark:
     goto AcceptState53;
  case LexerResult.CSharpStart:
     goto AcceptState41;
  // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce44 // see end of switch
  } // end of switch
  if (ParserInput <= LexerResult.Plus)
     goto HandleError36;
  Debug.Assert(ParserInput >= LexerResult.DefinitionSeparatorSymbol);

  /* Reduction 44
   * EndOfDefinitionWithoutSemantics= ;◄ method: EndOfDefinitionWithoutSemanticsRecognized
   */

  EndOfDefinitionWithoutSemanticsRecognized();

State37:
  /* State 37 */
  const String StateDescription37 =
       "outerDefinitionList= SequenceOfElements, EndOfDefinition, ►\"|\", outerDefinitionList;\r\n"
     + "outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithoutSemantics, ►\";\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState38;
  if (ParserInput < LexerResult.TerminatorSymbol)
     {
     if (ErrorHandler(37, StateDescription37, ParserInput))
        goto State37;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
AcceptReduce46:
  Lexer.AcceptSymbol();
Reduce46:
  /* Reduction 46, sStack: -1
   * outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithoutSemantics, ";";◄
   */
  _s.Pop();
  /* Branch 9*/
  switch (_s.Peek())
  {
  case 2:
     goto Reduce32;
  case 4:
     goto Reduce47;
  case 11:
     goto Reduce88;
  /*case 3:
  default: break;
  */
  }
Reduce43:
  /* Reduction 43, sStack: -1
   * outerDefinitions= EndOfDefinition, "|", outerDefinitionList;◄
   */
  _s.Pop();
  goto Branch8;

AcceptReduce16:
  Lexer.AcceptSymbol();
Reduce16:
  /* Reduction 16
   * CSEnumDeclaration= CSEnumProperties, optionalBaseType, CSEnumElements;◄ method: CSEnumRecognized
   */

  CSEnumRecognized();

State22:
  /* State 22 */
  const String StateDescription22 =
       "optionalEnum= CSharpStart, CSEnumDeclaration, ►CSharpEnd;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.CSharpEnd)
     {
     if (ErrorHandler(22, StateDescription22, ParserInput))
        goto State22;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.CSharpEnd);
  Lexer.AcceptSymbol();
Reduce13:
  /* Reduction 13
   * OptionalDeclarationOfTerminalSymbols= TerminalSymbolsList, SemikolonOrEnum;◄ method: CompareTerminalDeclarationsWithEnum
   */

  CompareTerminalDeclarationsWithEnum();

  goto State27;

Reduce33:
  /* Reduction 33, aStack: -1
   * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ method: OuterEndOfDefinitionRecognized, aStack: -1
   */

  OuterEndOfDefinitionRecognized(
     method: _a.PeekRef(0)._VoidMethodClass
     );

  _a.Free();
State35:
  /* State 35 */
  const String StateDescription35 =
       "outerDefinitions= EndOfDefinitionWithSemantics, ►\";\"?;\r\n"
     + "outerDefinitions= EndOfDefinition, ►\"|\", outerDefinitionList;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState34;
  if (ParserInput >= LexerResult.TerminatorSymbol)
     goto AcceptBranch8;
  if (ParserInput != LexerResult.NumberSign
     && ParserInput < LexerResult.Name)
     {
     if (ErrorHandler(35, StateDescription35, ParserInput))
        goto State35;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.NumberSign
     || ParserInput == LexerResult.Name || ParserInput == LexerResult.StringResult);
  goto Branch8;

AcceptReduce51:
  Lexer.AcceptSymbol();
  /* Reduction 51, sStack: -1
   * CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), "(", eFormalParameters, ")";◄ method: CSvoidMethodRecognized
   */
  _s.Pop();

  CSvoidMethodRecognized(
     voidMethod: out _a.PeekRef(0)._VoidMethodClass,
     method: _a.PeekClear(0)._MethodClass
     );

State49:
  /* State 49 */
  const String StateDescription49 =
       "SemanticAction(VoidMethodClass method)= CSharpStart, CSvoidMethod(VoidMethodClass method), ►CSharpEnd;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.CSharpEnd)
     {
     if (ErrorHandler(49, StateDescription49, ParserInput))
        goto State49;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.CSharpEnd);
  Lexer.AcceptSymbol();
  /* Reduction 55, sStack: -1
   * SemanticAction(VoidMethodClass method)= CSharpStart, CSvoidMethod(VoidMethodClass method), CSharpEnd;◄
   */
  _s.Pop();
Branch11:
  /* Branch 11*/
  switch (_s.Peek())
  {
  case 2:
  case 11:
     goto Reduce33;
  case 1:
  {
     /* Reduction 45, aStack: -1
      * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ method: OuterEndOfDefinitionRecognized, aStack: -1
      */

     OuterEndOfDefinitionRecognized(
        method: _a.PeekRef(0)._VoidMethodClass
        );

     _a.Free();
     goto State39;
     }
  case 6:
  {
     /* Reduction 67, aStack: -1
      * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ method: OuterEndOfDefinitionRecognized, aStack: -1
      */

     OuterEndOfDefinitionRecognized(
        method: _a.PeekRef(0)._VoidMethodClass
        );

     _a.Free();
     goto State66;
     }
  case 3:
  {
     /* Reduction 73, sStack: -1, aStack: -1
      * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ method: OuterEndOfDefinitionRecognized, aStack: -1
      * then: Definition= SequenceOfElements, EndOfDefinition;◄
      */
     _s.Pop();

     OuterEndOfDefinitionRecognized(
        method: _a.PeekRef(0)._VoidMethodClass
        );

     _a.Free();
     goto Branch18;
     }
  case 9:
  {
     /* Reduction 76, aStack: -1
      * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ method: OuterEndOfDefinitionRecognized, aStack: -1
      */

     OuterEndOfDefinitionRecognized(
        method: _a.PeekRef(0)._VoidMethodClass
        );

     _a.Free();
     goto State73;
     }
  /*case 0:
  default: break;
  */
  }
  /* Reduction 42, sStack: -1, aStack: -3
   * EndOfDefinitionWithSemantics= PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority), SemanticAction(VoidMethodClass method);◄ method: EndOfDefinitionRecognized, aStack: -3
   */
  _s.Pop();

  EndOfDefinitionRecognized(
     constPriority: _a.PeekRef(-2)._Int32,
     dynPriority: _a.PeekRef(-1)._IntMethodClass,
     method: _a.PeekRef(0)._VoidMethodClass
     );

  _a.Free(3);
Branch7:
  /* Branch 7*/
  switch (_s.Peek())
  {
  case 2:
  case 11:
     goto State35;
  case 6:
     goto State66;
  case 3:
  {
     /* Reduction 72, sStack: -1
      * Definition= SequenceOfElements, EndOfDefinition;◄
      */
     _s.Pop();
     goto Branch18;
     }
  case 9:
     goto State73;
  /*case 1:
  default: break;
  */
  }
State39:
  /* State 39 */
  const String StateDescription39 =
       "outerDefinitionList= SequenceOfElements, EndOfDefinition, ►\"|\", outerDefinitionList;\r\n"
     + "outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithSemantics, ►\";\"?;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState38;
  if (ParserInput >= LexerResult.TerminatorSymbol)
     goto AcceptReduce46;
  if (ParserInput != LexerResult.NumberSign
     && ParserInput < LexerResult.Name)
     {
     if (ErrorHandler(39, StateDescription39, ParserInput))
        goto State39;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.NumberSign
     || ParserInput == LexerResult.Name || ParserInput == LexerResult.StringResult);
  goto Reduce46;

AcceptReduce59:
  Lexer.AcceptSymbol();
  /* Reduction 59, sStack: -1
   * CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), "(", eFormalParameters, ")";◄ method: CSintMethodRecognized
   */
  _s.Pop();

  CSintMethodRecognized(
     intMethod: out _a.PeekRef(0)._IntMethodClass,
     method: _a.PeekClear(0)._MethodClass
     );

State59:
  /* State 59 */
  const String StateDescription59 =
       "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, CSintMethod(IntMethodClass intMethod), ►CSharpEnd, (Local1)?;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.CSharpEnd)
     {
     if (ErrorHandler(59, StateDescription59, ParserInput))
        goto State59;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.CSharpEnd);
  Lexer.AcceptSymbol();
State60:
  /* State 60 */
  const String StateDescription60 =
       "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, ►(Local1)?;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DoubleQuestionmark)
     {
     Lexer.AcceptSymbol();
     goto Reduce60;
     }
  if (ParserInput != LexerResult.NumberSign
     && ParserInput < LexerResult.CSharpStart)
     {
     if (ErrorHandler(60, StateDescription60, ParserInput))
        goto State60;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.NumberSign
     || ParserInput >= LexerResult.CSharpStart);
Reduce60:
  /* Reduction 60, sStack: -1, aStack: 1
   * PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= "??", CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, (Local1)?;◄ aStack: 1, method: DynamicPriorityRecognized
   */
  _s.Pop();
  _a.Allocate();

  DynamicPriorityRecognized(
     constPriority: out _a.PeekRef(-1)._Int32,
     dynamicPriority: out _a.PeekRef(0)._IntMethodClass,
     intMethod: _a.PeekClear(-1)._IntMethodClass
     );

State32:
  /* State 32 (0)*/
  const String StateDescription32 =
       "EndOfDefinitionWithSemantics= PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority)●;\r\n"
     + "EndOfDefinitionWithSemantics= PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority), ►SemanticAction(VoidMethodClass method);";
  _s.Push(0);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.CSharpStart)
     goto AcceptState41;
  if (ParserInput != LexerResult.NumberSign
     && ParserInput < LexerResult.GroupEnd)
     {
     if (ErrorHandler(32, StateDescription32, ParserInput))
        goto State32;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.NumberSign
     || ParserInput >= LexerResult.GroupEnd);
  /* Reduction 41, sStack: -1, aStack: -2
   * EndOfDefinitionWithSemantics= PriorityDeclaration(Int32 constPriority, IntMethodClass dynPriority);◄ method: EndOfDefinitionWithSemanticsRecognized, aStack: -2
   */
  _s.Pop();

  EndOfDefinitionWithSemanticsRecognized(
     constPriority: _a.PeekRef(-1)._Int32,
     dynPriority: _a.PeekRef(0)._IntMethodClass
     );

  _a.Free(2);
  goto Branch7;

Reduce63:
  /* Reduction 63, aStack: 5
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

State65:
  /* State 65 (6)*/
  const String StateDescription65 =
       "NestedElement(Symbol Symbol)= SaveVariables(Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions, Int32 SavedNumberOfTrivialDefinitions, Int32 SavedNumberOfElements, Int32 SavedNumberOfSymbolAttributes), ►NestedGrammarRule(Symbol NestedSymbol, Int32 NumberOfAttributes);";
  _s.Push(6);
  ParserInput = Lexer.PeekSymbol();
  switch (ParserInput)
  {
  // <= LexerResult.NumberSign
  // >= LexerResult.TerminatorSymbol: goto HandleError65 // see end of switch
  case LexerResult.GroupStart:
     goto AcceptState79;
  case LexerResult.OptionStart:
     goto AcceptState77;
  case LexerResult.RepeatStart:
     goto AcceptState64;
  case LexerResult.DoubleQuestionmark:
     goto AcceptState53;
  case LexerResult.CSharpStart:
     goto AcceptState41;
  case LexerResult.GroupEnd:
  case LexerResult.RepeatEnd:
  case LexerResult.OptionEnd:
  case LexerResult.DefinitionSeparatorSymbol:
     {
     /* Reduction 64
      * EndOfDefinitionWithoutSemantics= ;◄ method: EndOfDefinitionWithoutSemanticsRecognized
      */

     EndOfDefinitionWithoutSemanticsRecognized();

     goto State66;
     }
  case LexerResult.Name:
     goto AcceptState3;
  case LexerResult.StringResult:
     goto AcceptReduce2;
  } // end of switch
  Debug.Assert(ParserInput <= LexerResult.NumberSign || ParserInput >= LexerResult.TerminatorSymbol);

  if (ErrorHandler(65, StateDescription65, ParserInput))
     goto State65;
  goto EndWithError1;

Reduce66:
  /* Reduction 66, aStack: 2
   * NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions;◄ aStack: 2, method: NestedGrammarRuleWithEmptyLeftside
   */
  _a.Allocate(2);

  NestedGrammarRuleWithEmptyLeftside(
     SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
     NumberOfAttributes: out _a.PeekRef(0)._Int32
     );

Reduce65:
  /* Reduction 65, sStack: -1, aStack: -6
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
  /* Branch 15*/
  switch (_s.Peek())
  {
  case 1:
     goto State78;
  case 2:
     goto State80;
  /*case 0:
  default: break;
  */
  }
State76:
  /* State 76 */
  const String StateDescription76 =
       "RepeatedElement(Symbol Symbol)= \"{\", NestedElement(Symbol Symbol), ►\"}\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.RepeatEnd)
     {
     if (ErrorHandler(76, StateDescription76, ParserInput))
        goto State76;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatEnd);
  Lexer.AcceptSymbol();
  /* Reduction 78, sStack: -1
   * RepeatedElement(Symbol Symbol)= "{", NestedElement(Symbol Symbol), "}";◄ method: RepeatGroupRecognized
   */
  _s.Pop();

  RepeatGroupRecognized(
     Symbol: ref _a.PeekRef(0)._Symbol
     );

Branch6:
  /* Branch 6*/
  switch (_s.Peek())
  {
  case 5:
  {
     /* Reduction 49, sStack: -2, aStack: -1
      * Element= RepeatedElement(Symbol Symbol);◄ method: ElementVariantRecognized, aStack: -1
      * then: SequenceOfElements= SequenceOfElements, ","?, Element;◄
      */
     _s.Discard(2);

     ElementVariantRecognized(
        Symbol: _a.PeekRef(0)._Symbol
        );

     _a.Free();
     goto Branch5;
     }
  case 6:
  case 7:
  case 8:
  case 9:
     goto Reduce68;
  /*case 2: case 3: case 4: case 11:
  default: break;
  */
  }
  /* Reduction 34, aStack: -1
   * Element= RepeatedElement(Symbol Symbol);◄ method: ElementVariantRecognized, aStack: -1
   */

  ElementVariantRecognized(
     Symbol: _a.PeekRef(0)._Symbol
     );

  _a.Free();
  goto State36;

Reduce68:
  /* Reduction 68, aStack: -1
   * Element= RepeatedElement(Symbol Symbol);◄ method: ElementVariantRecognized, aStack: -1
   */

  ElementVariantRecognized(
     Symbol: _a.PeekRef(0)._Symbol
     );

  _a.Free();
State70:
  /* State 70 (3)*/
  const String StateDescription70 =
       "Definition= SequenceOfElements, ►EndOfDefinition;\r\n"
     + "SequenceOfElements= SequenceOfElements, ►\",\"?, Element;";
  _s.Push(3);
  ParserInput = Lexer.PeekSymbol();
  switch (ParserInput)
  {
  // <= LexerResult.Plus
  // >= LexerResult.TerminatorSymbol: goto HandleError70 // see end of switch
  case LexerResult.NumberSign:
     goto HandleError70;
  case LexerResult.Comma:
     goto AcceptState40;
  case LexerResult.GroupStart:
  case LexerResult.OptionStart:
  case LexerResult.RepeatStart:
  case LexerResult.Name:
  case LexerResult.StringResult:
     goto State40;
  case LexerResult.DoubleQuestionmark:
     goto AcceptState53;
  case LexerResult.CSharpStart:
     goto AcceptState41;
  case LexerResult.GroupEnd:
  case LexerResult.RepeatEnd:
  case LexerResult.OptionEnd:
  case LexerResult.DefinitionSeparatorSymbol:
     {
     /* Reduction 71, sStack: -1
      * EndOfDefinitionWithoutSemantics= ;◄ method: EndOfDefinitionWithoutSemanticsRecognized
      * then: Definition= SequenceOfElements, EndOfDefinition;◄
      */
     _s.Pop();

     EndOfDefinitionWithoutSemanticsRecognized();

     goto Branch18;
     }
  } // end of switch
  Debug.Assert(ParserInput <= LexerResult.Plus || ParserInput >= LexerResult.TerminatorSymbol);

HandleError70:
  if (ErrorHandler(70, StateDescription70, ParserInput))
     goto State70;
  goto EndWithError1;

Reduce88:
  /* Reduction 88, sStack: -1
   * GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "=", outerDefinitions;◄
   */
  _s.Pop();
Reduce84:
  /* Reduction 84, sStack: -1, aStack: -2
   * GrammarRuleList= GrammarRuleList, GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes);◄ method: EndOfGrammarRuleRecognized, aStack: -2
   */
  _s.Pop();

  EndOfGrammarRuleRecognized(
     SymbolAtLeftSide: _a.PeekRef(-1)._Symbol
     );

  _a.Free(2);
  goto State81;

AcceptState3:
  Lexer.AcceptSymbol();
  goto State3;

AcceptState4:
  Lexer.AcceptSymbol();
State4:
  /* State 4 (0)*/
  const String StateDescription4 =
       "\"(Attributes)\"(Int32 numberOfAttributes)= \"(\", ►\")\";\r\n"
     + "\"(Attributes)\"(Int32 numberOfAttributes)= \"(\", ►\"Attributes)\"(Int32 numberOfAttributes, Int32 smallestNumber);";
  _s.Push(0);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState7;
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(4, StateDescription4, ParserInput))
        goto State4;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
  Lexer.AcceptSymbol();
  /* Reduction 6, sStack: -1, aStack: 1
   * "(Attributes)"(Int32 numberOfAttributes)= "(", ")";◄ aStack: 1, method: EmptyListOfAttributes
   */
  _s.Pop();
  _a.Allocate();

  EmptyListOfAttributes(
     numberOfAttributes: out _a.PeekRef(0)._Int32
     );

Branch2:
  /* Branch 2*/
  if (_s.Peek() == 1)
  {
     /* Reduction 90, sStack: -1
      * "Name(Attributes)"(String name, Int32 NumberOfAttributes)= ExtendedName(String name), "(Attributes)"(Int32 NumberOfAttributes);◄
      */
     _s.Pop();
     goto State8;
     }
  /* Reduction 5, sStack: -1
   * "Name(Attributes)"(String name, Int32 NumberOfAttributes)= ExtendedName(String name), "(Attributes)"(Int32 NumberOfAttributes);◄
   */
  _s.Pop();
  goto Branch1;

State5:
  /* State 5 */
  const String StateDescription5 =
       "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 numberBeforeGroup), ►Comma, \"Attributes)\"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup);\r\n"
     + "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 number), ►\")\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Comma)
     {
     Lexer.AcceptSymbol();
     goto State6;
     }
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(5, StateDescription5, ParserInput))
        goto State5;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
  Lexer.AcceptSymbol();
  /* Reduction 8, aStack: 1
   * "Attributes)"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 number), ")";◄ aStack: 1, method: FirstAttributeOfGroup
   */
  _a.Allocate();

  FirstAttributeOfGroup(
     numberOfAttributesOfGroup: out _a.PeekRef(-1)._Int32,
     smallestNumber: out _a.PeekRef(0)._Int32,
     number: _a.PeekRef(-1)._Int32
     );

Branch3:
  /* Branch 3*/
  if (_s.Peek() == 1)
  {
     /* Reduction 9, sStack: -1, aStack: -1
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
     goto Branch3;
     }
  /* Reduction 7, sStack: -1, aStack: -1
   * "(Attributes)"(Int32 numberOfAttributes)= "(", "Attributes)"(Int32 numberOfAttributes, Int32 smallestNumber);◄ aStack: -1
   */
  _s.Pop();
  _a.Free();
  goto Branch2;

State6:
  /* State 6 (1)*/
  const String StateDescription6 =
       "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 numberBeforeGroup), Comma, ►\"Attributes)\"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup);";
  _s.Push(1);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(6, StateDescription6, ParserInput))
        goto State6;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
AcceptState7:
  Lexer.AcceptSymbol();
State7:
  /* State 7 */
  const String StateDescription7 =
       "Attribut(Int32 number)= Name(String type), ►Name(String name);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(7, StateDescription7, ParserInput))
        goto State7;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
  /* Reduction 10, aStack: -1
   * Attribut(Int32 number)= Name(String type), Name(String name);◄ method: AttributeTypeAndName, aStack: -1
   */

  AttributeTypeAndName(
     number: out _a.PeekRef(-1)._Int32,
     type: _a.PeekClear(-1)._String,
     name: _a.PeekRef(0)._String
     );

  _a.Free();
  goto State5;

State8:
  /* State 8 */
  const String StateDescription8 =
       "TerminalSymbol= \"Name(Attributes)\"(String Name, Int32 NumberOfAttributes), ►OptionalWeight(Int32 Weight);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Percent)
     {
     Lexer.AcceptSymbol();
     goto State9;
     }
  if (ParserInput != LexerResult.CSharpStart
     && ParserInput < LexerResult.DefinitionSeparatorSymbol)
     {
     if (ErrorHandler(8, StateDescription8, ParserInput))
        goto State8;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.CSharpStart
     || ParserInput >= LexerResult.DefinitionSeparatorSymbol);
  /* Reduction 11, aStack: 1
   * OptionalWeight(Int32 weight)= ;◄ aStack: 1, method: DefaultWeightOne
   */
  _a.Allocate();

  DefaultWeightOne(
     weight: out _a.PeekRef(0)._Int32
     );

Reduce12:
  /* Reduction 12, aStack: -3
   * TerminalSymbol= "Name(Attributes)"(String Name, Int32 NumberOfAttributes), OptionalWeight(Int32 Weight);◄ method: TerminalSymbol, aStack: -3
   */

  TerminalSymbol(
     Name: _a.PeekRef(-2)._String,
     NumberOfAttributes: _a.PeekRef(-1)._Int32,
     Weight: _a.PeekRef(0)._Int32
     );

  _a.Free(3);
  /* Branch 4*/
  if (_s.Peek() == 1)
  {
     /* Reduction 15, sStack: -1
      * TerminalSymbolsList= TerminalSymbolsList, "|", TerminalSymbol;◄
      */
     _s.Pop();
     goto State10;
     }

State10:
  /* State 10 */
  const String StateDescription10 =
       "OptionalDeclarationOfTerminalSymbols= TerminalSymbolsList, ►SemikolonOrEnum;\r\n"
     + "TerminalSymbolsList= TerminalSymbolsList, ►\"|\", TerminalSymbol;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     {
     Lexer.AcceptSymbol();
     goto State12;
     }
  if (ParserInput >= LexerResult.TerminatorSymbol)
     {
     Lexer.AcceptSymbol();
     /* Reduction 14
      * NoEnum= ;◄ method: EnumOmitted
      */

     EnumOmitted();

     goto Reduce13;
     }
  if (ParserInput != LexerResult.CSharpStart)
     {
     if (ErrorHandler(10, StateDescription10, ParserInput))
        goto State10;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.CSharpStart);
  Lexer.AcceptSymbol();
State13:
  /* State 13 */
  const String StateDescription13 =
       "optionalEnum= CSharpStart, ►CSEnumDeclaration, CSharpEnd;\r\n"
     + "optionalEnum= CSharpStart, ►CSharpEnd, NoEnum;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     goto State23;
     }
  if (ParserInput != LexerResult.CSharpEnd)
     {
     if (ErrorHandler(13, StateDescription13, ParserInput))
        goto State13;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.CSharpEnd);
  Lexer.AcceptSymbol();
  /* Reduction 28
   * NoEnum= ;◄ method: EnumOmitted
   */

  EnumOmitted();

  goto Reduce13;

State9:
  /* State 9 */
  const String StateDescription9 =
       "OptionalWeight(Int32 weight)= \"%\", ►Number(Int32 weight);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Number)
     {
     if (ErrorHandler(9, StateDescription9, ParserInput))
        goto State9;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Number);
  Lexer.AcceptSymbol();
  goto Reduce12;

State12:
  /* State 12 (1)*/
  const String StateDescription12 =
       "TerminalSymbolsList= TerminalSymbolsList, \"|\", ►TerminalSymbol;";
  _s.Push(1);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState3;
  if (ParserInput != LexerResult.StringResult)
     {
     if (ErrorHandler(12, StateDescription12, ParserInput))
        goto State12;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.StringResult);
  goto AcceptReduce2;

State14:
  /* State 14 */
  const String StateDescription14 =
       "CSEnumDeclaration= CSEnumProperties, ►optionalBaseType, CSEnumElements;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Colon)
     {
     Lexer.AcceptSymbol();
     goto State21;
     }
  if (ParserInput != LexerResult.RepeatStart)
     {
     if (ErrorHandler(14, StateDescription14, ParserInput))
        goto State14;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatStart);
State15:
  /* State 15 */
  const String StateDescription15 =
       "CSEnumDeclaration= CSEnumProperties, optionalBaseType, ►CSEnumElements;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.RepeatStart)
     {
     if (ErrorHandler(15, StateDescription15, ParserInput))
        goto State15;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatStart);
  Lexer.AcceptSymbol();
State16:
  /* State 16 */
  const String StateDescription16 =
       "CSEnumElements= \"{\", ►\"}\";\r\n"
     + "CSEnumElements= \"{\", ►CSEnumElementList, \"}\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     /* Reduction 18, aStack: -1
      * CSEnumElement= Name(String enumElement);◄ method: EnumElementRecognized, aStack: -1
      */

     EnumElementRecognized(
        enumElement: _a.PeekRef(0)._String
        );

     _a.Free();
     goto State17;
     }
  if (ParserInput != LexerResult.RepeatEnd)
     {
     if (ErrorHandler(16, StateDescription16, ParserInput))
        goto State16;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatEnd);
  Lexer.AcceptSymbol();
  /* Reduction 17
   * CSEnumElements= "{", "}";◄ method: NoEnumRecognized
   */

  NoEnumRecognized();

  goto Reduce16;

State17:
  /* State 17 */
  const String StateDescription17 =
       "CSEnumElements= \"{\", CSEnumElementList, ►\"}\";\r\n"
     + "CSEnumElementList= CSEnumElementList, ►Comma, CSEnumElement;\r\n"
     + "FirstCSEnumElement= CSEnumElement, ►\"=\", Name(String ignored);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Comma)
     goto AcceptState18;
  if (ParserInput <= LexerResult.DefiningSymbol)
     {
     Lexer.AcceptSymbol();
     goto State19;
     }
  if (ParserInput != LexerResult.RepeatEnd)
     {
     if (ErrorHandler(17, StateDescription17, ParserInput))
        goto State17;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatEnd);
  goto AcceptReduce16;

AcceptState18:
  Lexer.AcceptSymbol();
State18:
  /* State 18 */
  const String StateDescription18 =
       "CSEnumElementList= CSEnumElementList, Comma, ►CSEnumElement;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(18, StateDescription18, ParserInput))
        goto State18;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
  /* Reduction 19, aStack: -1
   * CSEnumElement= Name(String enumElement);◄ method: EnumElementRecognized, aStack: -1
   */

  EnumElementRecognized(
     enumElement: _a.PeekRef(0)._String
     );

  _a.Free();
State20:
  /* State 20 */
  const String StateDescription20 =
       "CSEnumElements= \"{\", CSEnumElementList, ►\"}\";\r\n"
     + "CSEnumElementList= CSEnumElementList, ►Comma, CSEnumElement;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Comma)
     goto AcceptState18;
  if (ParserInput != LexerResult.RepeatEnd)
     {
     if (ErrorHandler(20, StateDescription20, ParserInput))
        goto State20;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatEnd);
  goto AcceptReduce16;

State19:
  /* State 19 */
  const String StateDescription19 =
       "FirstCSEnumElement= CSEnumElement, \"=\", ►Name(String ignored);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(19, StateDescription19, ParserInput))
        goto State19;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
  /* Reduction 20, aStack: -1
   * FirstCSEnumElement= CSEnumElement, "=", Name(String ignored);◄ aStack: -1
   */
  _a.Free();
  goto State20;

State21:
  /* State 21 */
  const String StateDescription21 =
       "optionalBaseType= \":\", ►Name(String Ignored);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(21, StateDescription21, ParserInput))
        goto State21;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
  /* Reduction 21, aStack: -1
   * optionalBaseType= ":", Name(String Ignored);◄ aStack: -1
   */
  _a.Free();
  goto State15;

State23:
  /* State 23 */
  const String StateDescription23 =
       "CSEnumProperties= Name(String modifier1), ►Name(String modifier2), Name(String enum), CSEnumName(String name);\r\n"
     + "CSEnumProperties= Name(String modifier), ►Name(String enum), CSEnumName(String name);\r\n"
     + "CSEnumProperties= Name(String enum), ►CSEnumName(String name);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(23, StateDescription23, ParserInput))
        goto State23;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
State24:
  /* State 24 */
  const String StateDescription24 =
       "CSEnumProperties= Name(String modifier1), Name(String modifier2), ►Name(String enum), CSEnumName(String name);\r\n"
     + "CSEnumProperties= Name(String modifier), Name(String enum), ►CSEnumName(String name);\r\n"
     + "CSEnumName(String name)= Name(String name)●;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     goto State25;
     }
  if (ParserInput != LexerResult.Colon
     && ParserInput != LexerResult.RepeatStart)
     {
     if (ErrorHandler(24, StateDescription24, ParserInput))
        goto State24;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Colon
     || ParserInput == LexerResult.RepeatStart);
  /* Reduction 23, aStack: -2
   * CSEnumName(String name)= Name(String name);◄ method: EnumNameRecognized
   * then: CSEnumProperties= Name(String enum), CSEnumName(String name);◄ aStack: -2
   */

  EnumNameRecognized(
     name: _a.PeekRef(0)._String
     );

  _a.Free(2);
  goto State14;

State25:
  /* State 25 */
  const String StateDescription25 =
       "CSEnumProperties= Name(String modifier1), Name(String modifier2), Name(String enum), ►CSEnumName(String name);\r\n"
     + "CSEnumName(String name)= Name(String name)●;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     /* Reduction 26, aStack: -4
      * CSEnumName(String name)= Name(String name);◄ method: EnumNameRecognized
      * then: CSEnumProperties= Name(String modifier1), Name(String modifier2), Name(String enum), CSEnumName(String name);◄ aStack: -4
      */

     EnumNameRecognized(
        name: _a.PeekRef(0)._String
        );

     _a.Free(4);
     goto State14;
     }
  if (ParserInput != LexerResult.Colon
     && ParserInput != LexerResult.RepeatStart)
     {
     if (ErrorHandler(25, StateDescription25, ParserInput))
        goto State25;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Colon
     || ParserInput == LexerResult.RepeatStart);
  /* Reduction 25, aStack: -3
   * CSEnumName(String name)= Name(String name);◄ method: EnumNameRecognized
   * then: CSEnumProperties= Name(String modifier), Name(String enum), CSEnumName(String name);◄ aStack: -3
   */

  EnumNameRecognized(
     name: _a.PeekRef(0)._String
     );

  _a.Free(3);
  goto State14;

State30:
  /* State 30 */
  const String StateDescription30 =
       "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"+\"●;\r\n"
     + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"+\", ►\"+\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Plus)
     {
     Lexer.AcceptSymbol();
     /* Reduction 38
      * RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "+", "+";◄ method: Repeat1rrRecognized
      */

     Repeat1rrRecognized(
        Symbol: ref _a.PeekRef(0)._Symbol
        );

     goto Branch6;
     }
  if (ParserInput <= LexerResult.Asterisk
     || ParserInput == LexerResult.NumberSign)
     {
     if (ErrorHandler(30, StateDescription30, ParserInput))
        goto State30;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Comma
     || ParserInput >= LexerResult.GroupStart);
  /* Reduction 37
   * RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "+";◄ method: Repeat1lrRecognized
   */

  Repeat1lrRecognized(
     Symbol: ref _a.PeekRef(0)._Symbol
     );

  goto Branch6;

State31:
  /* State 31 */
  const String StateDescription31 =
       "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"*\"●;\r\n"
     + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"*\", ►\"*\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Asterisk)
     {
     Lexer.AcceptSymbol();
     /* Reduction 40
      * RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "*", "*";◄ method: Repeat0rrRecognized
      */

     Repeat0rrRecognized(
        Symbol: ref _a.PeekRef(0)._Symbol
        );

     goto Branch6;
     }
  if (ParserInput <= LexerResult.Plus
     || ParserInput == LexerResult.NumberSign)
     {
     if (ErrorHandler(31, StateDescription31, ParserInput))
        goto State31;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Comma
     || ParserInput >= LexerResult.GroupStart);
  /* Reduction 39
   * RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "*";◄ method: Repeat0lrRecognized
   */

  Repeat0lrRecognized(
     Symbol: ref _a.PeekRef(0)._Symbol
     );

  goto Branch6;

AcceptState34:
  Lexer.AcceptSymbol();
State34:
  /* State 34 (3)*/
  const String StateDescription34 =
       "outerDefinitions= EndOfDefinition, \"|\", ►outerDefinitionList;";
  _s.Push(3);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState3;
  if (ParserInput == LexerResult.StringResult)
     goto AcceptReduce2;
  if (ParserInput == LexerResult.GroupStart)
     goto AcceptState79;
  if (ParserInput == LexerResult.OptionStart)
     goto AcceptState77;
  if (ParserInput != LexerResult.RepeatStart)
     {
     if (ErrorHandler(34, StateDescription34, ParserInput))
        goto State34;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatStart);
AcceptState64:
  Lexer.AcceptSymbol();
  /* State 64 (0)
   * RepeatedElement(Symbol Symbol)= "{", ►NestedElement(Symbol Symbol), "}";
   */
  _s.Push(0);
  goto Reduce63;

AcceptState38:
  Lexer.AcceptSymbol();
State38:
  /* State 38 (4)*/
  const String StateDescription38 =
       "outerDefinitionList= SequenceOfElements, EndOfDefinition, \"|\", ►outerDefinitionList;";
  _s.Push(4);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState3;
  if (ParserInput == LexerResult.StringResult)
     goto AcceptReduce2;
  if (ParserInput == LexerResult.GroupStart)
     goto AcceptState79;
  if (ParserInput == LexerResult.OptionStart)
     goto AcceptState77;
  if (ParserInput != LexerResult.RepeatStart)
     {
     if (ErrorHandler(38, StateDescription38, ParserInput))
        goto State38;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatStart);
  goto AcceptState64;

AcceptState40:
  Lexer.AcceptSymbol();
State40:
  /* State 40 (5)*/
  const String StateDescription40 =
       "SequenceOfElements= SequenceOfElements, \",\"?, ►Element;";
  _s.Push(5);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState3;
  if (ParserInput == LexerResult.StringResult)
     goto AcceptReduce2;
  if (ParserInput == LexerResult.GroupStart)
     goto AcceptState79;
  if (ParserInput == LexerResult.OptionStart)
     goto AcceptState77;
  if (ParserInput != LexerResult.RepeatStart)
     {
     if (ErrorHandler(40, StateDescription40, ParserInput))
        goto State40;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatStart);
  goto AcceptState64;

AcceptState41:
  Lexer.AcceptSymbol();
State41:
  /* State 41 (0)*/
  const String StateDescription41 =
       "SemanticAction(VoidMethodClass method)= CSharpStart, ►CSvoidMethod(VoidMethodClass method), CSharpEnd;\r\n"
     + "SemanticAction(VoidMethodClass method)= CSharpStart, ►CSharpEnd;";
  _s.Push(0);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState50;
  if (ParserInput != LexerResult.CSharpEnd)
     {
     if (ErrorHandler(41, StateDescription41, ParserInput))
        goto State41;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.CSharpEnd);
  Lexer.AcceptSymbol();
  /* Reduction 50, sStack: -1, aStack: 1
   * SemanticAction(VoidMethodClass method)= CSharpStart, CSharpEnd;◄ aStack: 1, method: EmptySemanticAction
   */
  _s.Pop();
  _a.Allocate();

  EmptySemanticAction(
     method: out _a.PeekRef(0)._VoidMethodClass
     );

  goto Branch11;

State42:
  /* State 42 */
  const String StateDescription42 =
       "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), ►\"(\", eFormalParameters, \")\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.GroupStart)
     {
     if (ErrorHandler(42, StateDescription42, ParserInput))
        goto State42;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupStart);
  Lexer.AcceptSymbol();
State43:
  /* State 43 (0)*/
  const String StateDescription43 =
       "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), \"(\", ►eFormalParameters, \")\";";
  _s.Push(0);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState47;
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(43, StateDescription43, ParserInput))
        goto State43;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
State46:
  /* State 46 */
  const String StateDescription46 =
       "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), \"(\", eFormalParameters, ►\")\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(46, StateDescription46, ParserInput))
        goto State46;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
  goto AcceptReduce51;

State44:
  /* State 44 */
  const String StateDescription44 =
       "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), \"(\", eFormalParameters, ►\")\";\r\n"
     + "formalParameters= formalParameters, ►Comma, formalParameter;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Comma)
     goto AcceptState45;
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(44, StateDescription44, ParserInput))
        goto State44;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
  goto AcceptReduce51;

AcceptState45:
  Lexer.AcceptSymbol();
State45:
  /* State 45 (1)*/
  const String StateDescription45 =
       "formalParameters= formalParameters, Comma, ►formalParameter;";
  _s.Push(1);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(45, StateDescription45, ParserInput))
        goto State45;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
AcceptState47:
  Lexer.AcceptSymbol();
State47:
  /* State 47 */
  const String StateDescription47 =
       "formalParameter= Name(String type), ►Name(String name);\r\n"
     + "formalParameter= Name(String ParameterModifierOpt), ►Name(String type), Name(String name);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(47, StateDescription47, ParserInput))
        goto State47;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
State48:
  /* State 48 */
  const String StateDescription48 =
       "formalParameter= Name(String type), Name(String name)●;\r\n"
     + "formalParameter= Name(String ParameterModifierOpt), Name(String type), ►Name(String name);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     /* Reduction 54, aStack: -3
      * formalParameter= Name(String ParameterModifierOpt), Name(String type), Name(String name);◄ method: FormalParameterWithModifierTypeAndName, aStack: -3
      */

     FormalParameterWithModifierTypeAndName(
        ParameterModifierOpt: _a.PeekRef(-2)._String,
        type: _a.PeekRef(-1)._String,
        name: _a.PeekRef(0)._String
        );

     _a.Free(3);
     goto Branch13;
     }
  if (ParserInput != LexerResult.Comma
     && ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(48, StateDescription48, ParserInput))
        goto State48;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Comma
     || ParserInput == LexerResult.GroupEnd);
  /* Reduction 53, aStack: -2
   * formalParameter= Name(String type), Name(String name);◄ method: FormalParameterWithTypeAndName, aStack: -2
   */

  FormalParameterWithTypeAndName(
     type: _a.PeekRef(-1)._String,
     name: _a.PeekRef(0)._String
     );

  _a.Free(2);
Branch13:
  /* Branch 13*/
  switch (_s.Peek())
  {
  case 0:
     goto State44;
  case 2:
     goto State57;
  /*case 1:
  default: break;
  */
  }
  /* Reduction 52, sStack: -1
   * formalParameters= formalParameters, Comma, formalParameter;◄
   */
  _s.Pop();
  /* Branch 12*/
  if (_s.Peek() == 0)
     goto State44;

State57:
  /* State 57 */
  const String StateDescription57 =
       "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), \"(\", eFormalParameters, ►\")\";\r\n"
     + "formalParameters= formalParameters, ►Comma, formalParameter;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Comma)
     goto AcceptState45;
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(57, StateDescription57, ParserInput))
        goto State57;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
  goto AcceptReduce59;

AcceptState50:
  Lexer.AcceptSymbol();
State50:
  /* State 50 */
  const String StateDescription50 =
       "CSMethodProperties(MethodClass method)= Name(String methodType), ►Name(String methodName);\r\n"
     + "CSMethodProperties(MethodClass method)= Name(String modifier), ►Name(String methodType), Name(String methodName);\r\n"
     + "CSMethodProperties(MethodClass method)= Name(String modifier1), ►Name(String modifier2), Name(String methodType), Name(String methodName);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(50, StateDescription50, ParserInput))
        goto State50;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
State51:
  /* State 51 */
  const String StateDescription51 =
       "CSMethodProperties(MethodClass method)= Name(String methodType), Name(String methodName)●;\r\n"
     + "CSMethodProperties(MethodClass method)= Name(String modifier), Name(String methodType), ►Name(String methodName);\r\n"
     + "CSMethodProperties(MethodClass method)= Name(String modifier1), Name(String modifier2), ►Name(String methodType), Name(String methodName);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     goto State52;
     }
  if (ParserInput != LexerResult.GroupStart)
     {
     if (ErrorHandler(51, StateDescription51, ParserInput))
        goto State51;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupStart);
  /* Reduction 56, aStack: -1
   * CSMethodProperties(MethodClass method)= Name(String methodType), Name(String methodName);◄ method: MethodTypeAndNameRecognized, aStack: -1
   */

  MethodTypeAndNameRecognized(
     method: out _a.PeekRef(-1)._MethodClass,
     methodType: _a.PeekClear(-1)._String,
     methodName: _a.PeekRef(0)._String
     );

  _a.Free();
Branch14:
  /* Branch 14*/
  if (_s.Peek() == 0)
     goto State42;

State55:
  /* State 55 */
  const String StateDescription55 =
       "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), ►\"(\", eFormalParameters, \")\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.GroupStart)
     {
     if (ErrorHandler(55, StateDescription55, ParserInput))
        goto State55;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupStart);
  Lexer.AcceptSymbol();
State56:
  /* State 56 (2)*/
  const String StateDescription56 =
       "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), \"(\", ►eFormalParameters, \")\";";
  _s.Push(2);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState47;
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(56, StateDescription56, ParserInput))
        goto State56;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
State58:
  /* State 58 */
  const String StateDescription58 =
       "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), \"(\", eFormalParameters, ►\")\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(58, StateDescription58, ParserInput))
        goto State58;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
  goto AcceptReduce59;

State52:
  /* State 52 */
  const String StateDescription52 =
       "CSMethodProperties(MethodClass method)= Name(String modifier), Name(String methodType), Name(String methodName)●;\r\n"
     + "CSMethodProperties(MethodClass method)= Name(String modifier1), Name(String modifier2), Name(String methodType), ►Name(String methodName);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     {
     Lexer.AcceptSymbol();
     /* Reduction 58, aStack: -3
      * CSMethodProperties(MethodClass method)= Name(String modifier1), Name(String modifier2), Name(String methodType), Name(String methodName);◄ method: MethodModifierTypeAndNameRecognized2, aStack: -3
      */

     MethodModifierTypeAndNameRecognized2(
        method: out _a.PeekRef(-3)._MethodClass,
        modifier1: _a.PeekClear(-3)._String,
        modifier2: _a.PeekRef(-2)._String,
        methodType: _a.PeekRef(-1)._String,
        methodName: _a.PeekRef(0)._String
        );

     _a.Free(3);
     goto Branch14;
     }
  if (ParserInput != LexerResult.GroupStart)
     {
     if (ErrorHandler(52, StateDescription52, ParserInput))
        goto State52;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupStart);
  /* Reduction 57, aStack: -2
   * CSMethodProperties(MethodClass method)= Name(String modifier), Name(String methodType), Name(String methodName);◄ method: MethodModifierTypeAndNameRecognized, aStack: -2
   */

  MethodModifierTypeAndNameRecognized(
     method: out _a.PeekRef(-2)._MethodClass,
     modifier: _a.PeekClear(-2)._String,
     methodType: _a.PeekRef(-1)._String,
     methodName: _a.PeekRef(0)._String
     );

  _a.Free(2);
  goto Branch14;

AcceptState53:
  Lexer.AcceptSymbol();
State53:
  /* State 53 */
  const String StateDescription53 =
       "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", ►signedNumber(Int32 constPriority), \"??\";\r\n"
     + "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", ►CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, (Local1)?;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.CSharpStart)
     {
     Lexer.AcceptSymbol();
     goto State54;
     }
  if (ParserInput == LexerResult.Minus)
     {
     Lexer.AcceptSymbol();
     goto State63;
     }
  if (ParserInput == LexerResult.Number)
     goto AcceptState62;
  if (ParserInput != LexerResult.Plus)
     {
     if (ErrorHandler(53, StateDescription53, ParserInput))
        goto State53;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Plus);
  Lexer.AcceptSymbol();
State61:
  /* State 61 */
  const String StateDescription61 =
       "signedNumber(Int32 value)= \"+\", ►Number(Int32 value);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Number)
     {
     if (ErrorHandler(61, StateDescription61, ParserInput))
        goto State61;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Number);
AcceptState62:
  Lexer.AcceptSymbol();
State62:
  /* State 62 */
  const String StateDescription62 =
       "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", signedNumber(Int32 constPriority), ►\"??\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.DoubleQuestionmark)
     {
     if (ErrorHandler(62, StateDescription62, ParserInput))
        goto State62;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.DoubleQuestionmark);
  Lexer.AcceptSymbol();
  /* Reduction 61, aStack: 1
   * PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= "??", signedNumber(Int32 constPriority), "??";◄ aStack: 1, method: ConstantPriorityGiven
   */
  _a.Allocate();

  ConstantPriorityGiven(
     dynamicPriority: out _a.PeekRef(0)._IntMethodClass
     );

  goto State32;

State54:
  /* State 54 (1)*/
  const String StateDescription54 =
       "PriorityDeclaration(Int32 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, ►CSintMethod(IntMethodClass intMethod), CSharpEnd, (Local1)?;";
  _s.Push(1);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(54, StateDescription54, ParserInput))
        goto State54;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  goto AcceptState50;

State63:
  /* State 63 */
  const String StateDescription63 =
       "signedNumber(Int32 value)= \"-\", ►Number(Int32 value);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Number)
     {
     if (ErrorHandler(63, StateDescription63, ParserInput))
        goto State63;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Number);
  Lexer.AcceptSymbol();
  /* Reduction 62
   * signedNumber(Int32 value)= "-", Number(Int32 value);◄ method: NegateNumber
   */

  NegateNumber(
     value: ref _a.PeekRef(0)._Int32
     );

  goto State62;

State66:
  /* State 66 */
  const String StateDescription66 =
       "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions●;\r\n"
     + "NestedDefinitions= EndOfDefinition, ►\"|\", NestedDefinitionList;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState67;
  if (ParserInput <= LexerResult.CSharpStart
     || ParserInput >= LexerResult.Name)
     {
     if (ErrorHandler(66, StateDescription66, ParserInput))
        goto State66;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
  goto Reduce66;

AcceptState67:
  Lexer.AcceptSymbol();
State67:
  /* State 67 (7)*/
  const String StateDescription67 =
       "NestedDefinitions= EndOfDefinition, \"|\", ►NestedDefinitionList;";
  _s.Push(7);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState3;
  if (ParserInput == LexerResult.StringResult)
     goto AcceptReduce2;
  if (ParserInput == LexerResult.GroupStart)
     goto AcceptState79;
  if (ParserInput == LexerResult.OptionStart)
     goto AcceptState77;
  if (ParserInput != LexerResult.RepeatStart)
     {
     if (ErrorHandler(67, StateDescription67, ParserInput))
        goto State67;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatStart);
  goto AcceptState64;

State68:
  /* State 68 */
  const String StateDescription68 =
       "NestedDefinitions= EndOfDefinition, \"|\", NestedDefinitionList●;\r\n"
     + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState69;
  if (ParserInput <= LexerResult.CSharpStart
     || ParserInput >= LexerResult.Name)
     {
     if (ErrorHandler(68, StateDescription68, ParserInput))
        goto State68;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
  /* Reduction 69, sStack: -1
   * NestedDefinitions= EndOfDefinition, "|", NestedDefinitionList;◄
   */
  _s.Pop();
  /* Branch 16*/
  if (_s.Peek() == 6)
     goto Reduce66;

Reduce75:
  /* Reduction 75, sStack: -1
   * NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions;◄
   */
  _s.Pop();
  goto Reduce65;

AcceptState69:
  Lexer.AcceptSymbol();
State69:
  /* State 69 (8)*/
  const String StateDescription69 =
       "NestedDefinitionList= NestedDefinitionList, \"|\", ►Definition;";
  _s.Push(8);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Name)
     goto AcceptState3;
  if (ParserInput == LexerResult.StringResult)
     goto AcceptReduce2;
  if (ParserInput == LexerResult.GroupStart)
     goto AcceptState79;
  if (ParserInput == LexerResult.OptionStart)
     goto AcceptState77;
  if (ParserInput != LexerResult.RepeatStart)
     {
     if (ErrorHandler(69, StateDescription69, ParserInput))
        goto State69;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.RepeatStart);
  goto AcceptState64;

State71:
  /* State 71 */
  const String StateDescription71 =
       "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions●;\r\n"
     + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState69;
  if (ParserInput <= LexerResult.CSharpStart
     || ParserInput >= LexerResult.Name)
     {
     if (ErrorHandler(71, StateDescription71, ParserInput))
        goto State71;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
  goto Reduce66;

State72:
  /* State 72 (9)*/
  const String StateDescription72 =
       "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►NestedDefinitions;";
  _s.Push(9);
  ParserInput = Lexer.PeekSymbol();
  switch (ParserInput)
  {
  // <= LexerResult.NumberSign
  // >= LexerResult.TerminatorSymbol: goto HandleError72 // see end of switch
  case LexerResult.GroupStart:
     goto AcceptState79;
  case LexerResult.OptionStart:
     goto AcceptState77;
  case LexerResult.RepeatStart:
     goto AcceptState64;
  case LexerResult.DoubleQuestionmark:
     goto AcceptState53;
  case LexerResult.CSharpStart:
     goto AcceptState41;
  case LexerResult.GroupEnd:
  case LexerResult.RepeatEnd:
  case LexerResult.OptionEnd:
  case LexerResult.DefinitionSeparatorSymbol:
     {
     /* Reduction 74
      * EndOfDefinitionWithoutSemantics= ;◄ method: EndOfDefinitionWithoutSemanticsRecognized
      */

     EndOfDefinitionWithoutSemanticsRecognized();

     goto State73;
     }
  case LexerResult.Name:
     goto AcceptState3;
  case LexerResult.StringResult:
     goto AcceptReduce2;
  } // end of switch
  Debug.Assert(ParserInput <= LexerResult.NumberSign || ParserInput >= LexerResult.TerminatorSymbol);

  if (ErrorHandler(72, StateDescription72, ParserInput))
     goto State72;
  goto EndWithError1;

State73:
  /* State 73 */
  const String StateDescription73 =
       "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions●;\r\n"
     + "NestedDefinitions= EndOfDefinition, ►\"|\", NestedDefinitionList;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState67;
  if (ParserInput <= LexerResult.CSharpStart
     || ParserInput >= LexerResult.Name)
     {
     if (ErrorHandler(73, StateDescription73, ParserInput))
        goto State73;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
  goto Reduce75;

State74:
  /* State 74 */
  const String StateDescription74 =
       "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions●;\r\n"
     + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     goto AcceptState69;
  if (ParserInput <= LexerResult.CSharpStart
     || ParserInput >= LexerResult.Name)
     {
     if (ErrorHandler(74, StateDescription74, ParserInput))
        goto State74;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
  goto Reduce75;

State75:
  /* State 75 */
  const String StateDescription75 =
       "NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= \"Name(Attributes)\"(String eLeftSideName, Int32 NumberOfAttributes), ►\"=\";\r\n"
     + "SimpleElement(Symbol Symbol)= \"Name(Attributes)\"(String Name, Int32 NumberOfAttributes)●;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput <= LexerResult.DefiningSymbol)
     {
     Lexer.AcceptSymbol();
     /* Reduction 77
      * NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= "Name(Attributes)"(String eLeftSideName, Int32 NumberOfAttributes), "=";◄ method: LeftSideOfNestedProduction
      */

     LeftSideOfNestedProduction(
        SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
        eLeftSideName: _a.PeekClear(-1)._String,
        NumberOfAttributes: _a.PeekRef(0)._Int32
        );

     goto State72;
     }
  if (ParserInput <= LexerResult.MinusEqual
     || ParserInput == LexerResult.NumberSign
     || ParserInput >= LexerResult.TerminatorSymbol)
     {
     if (ErrorHandler(75, StateDescription75, ParserInput))
        goto State75;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput > LexerResult.MinusEqual
     && ParserInput != LexerResult.NumberSign
     && ParserInput < LexerResult.TerminatorSymbol);
  goto Reduce31;

AcceptState77:
  Lexer.AcceptSymbol();
  /* State 77 (1)
   * RepeatedElement(Symbol Symbol)= "[", ►NestedElement(Symbol Symbol), "]";
   */
  _s.Push(1);
  goto Reduce63;

State78:
  /* State 78 */
  const String StateDescription78 =
       "RepeatedElement(Symbol Symbol)= \"[\", NestedElement(Symbol Symbol), ►\"]\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.OptionEnd)
     {
     if (ErrorHandler(78, StateDescription78, ParserInput))
        goto State78;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.OptionEnd);
  Lexer.AcceptSymbol();
  /* Reduction 79, sStack: -1
   * RepeatedElement(Symbol Symbol)= "[", NestedElement(Symbol Symbol), "]";◄ method: OptionGroupRecognized
   */
  _s.Pop();

  OptionGroupRecognized(
     Symbol: ref _a.PeekRef(0)._Symbol
     );

  goto Branch6;

AcceptState79:
  Lexer.AcceptSymbol();
  /* State 79 (2)
   * SimpleElement(Symbol Symbol)= "(", ►NestedElement(Symbol Symbol), ")";
   */
  _s.Push(2);
  goto Reduce63;

State80:
  /* State 80 */
  const String StateDescription80 =
       "SimpleElement(Symbol Symbol)= \"(\", NestedElement(Symbol Symbol), ►\")\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.GroupEnd)
     {
     if (ErrorHandler(80, StateDescription80, ParserInput))
        goto State80;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.GroupEnd);
  Lexer.AcceptSymbol();
  /* Reduction 80, sStack: -1
   * SimpleElement(Symbol Symbol)= "(", NestedElement(Symbol Symbol), ")";◄
   */
  _s.Pop();
  goto State29;

State82:
  /* State 82 */
  const String StateDescription82 =
       "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►\"=\", outerDefinitions;\r\n"
     + "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►\"-=\", ListOfExcludedTerminalSymbols, \";\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput <= LexerResult.DefiningSymbol)
     {
     Lexer.AcceptSymbol();
     goto State86;
     }
  if (ParserInput != LexerResult.MinusEqual)
     {
     if (ErrorHandler(82, StateDescription82, ParserInput))
        goto State82;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.MinusEqual);
  Lexer.AcceptSymbol();
State83:
  /* State 83 */
  const String StateDescription83 =
       "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"-=\", ►ListOfExcludedTerminalSymbols, \";\";";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(83, StateDescription83, ParserInput))
        goto State83;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
  /* Reduction 85, aStack: -1
   * ListOfExcludedTerminalSymbols= Name(String name);◄ method: FirstExcludedTerminalSymbol, aStack: -1
   */

  FirstExcludedTerminalSymbol(
     name: _a.PeekRef(0)._String
     );

  _a.Free();
State84:
  /* State 84 */
  const String StateDescription84 =
       "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"-=\", ListOfExcludedTerminalSymbols, ►\";\";\r\n"
     + "ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, ►\"|\", Name(String name);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
     {
     Lexer.AcceptSymbol();
     goto State85;
     }
  if (ParserInput < LexerResult.TerminatorSymbol)
     {
     if (ErrorHandler(84, StateDescription84, ParserInput))
        goto State84;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
  Lexer.AcceptSymbol();
  /* Reduction 86
   * GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "-=", ListOfExcludedTerminalSymbols, ";";◄ method: EndOfListOfExcludedTerminalSymbols
   */

  EndOfListOfExcludedTerminalSymbols();

  goto Reduce84;

State85:
  /* State 85 */
  const String StateDescription85 =
       "ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, \"|\", ►Name(String name);";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput != LexerResult.Name)
     {
     if (ErrorHandler(85, StateDescription85, ParserInput))
        goto State85;
     goto EndWithError1;
     }
  Debug.Assert(ParserInput == LexerResult.Name);
  Lexer.AcceptSymbol();
  /* Reduction 87, aStack: -1
   * ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, "|", Name(String name);◄ method: OneMoreExcludedTerminalSymbol, aStack: -1
   */

  OneMoreExcludedTerminalSymbol(
     name: _a.PeekRef(0)._String
     );

  _a.Free();
  goto State84;

State86:
  /* State 86 (11)*/
  const String StateDescription86 =
       "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"=\", ►outerDefinitions;";
  _s.Push(11);
  ParserInput = Lexer.PeekSymbol();
  switch (ParserInput)
  {
  // <= LexerResult.NumberSign: goto HandleError86 // see end of switch
  case LexerResult.GroupEnd:
  case LexerResult.RepeatEnd:
  case LexerResult.OptionEnd:
     goto HandleError86;
  case LexerResult.GroupStart:
     goto AcceptState79;
  case LexerResult.OptionStart:
     goto AcceptState77;
  case LexerResult.RepeatStart:
     goto AcceptState64;
  case LexerResult.DoubleQuestionmark:
     goto AcceptState53;
  case LexerResult.CSharpStart:
     goto AcceptState41;
  case LexerResult.Name:
     goto AcceptState3;
  case LexerResult.StringResult:
     goto AcceptReduce2;
  // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce30 // see end of switch
  } // end of switch
  if (ParserInput >= LexerResult.DefinitionSeparatorSymbol)
     goto Reduce30;
  Debug.Assert(ParserInput <= LexerResult.NumberSign);

HandleError86:
  if (ErrorHandler(86, StateDescription86, ParserInput))
     goto State86;
  goto EndWithError1;

State87:
  /* State 87 (1)*/
  const String StateDescription87 =
       "GrammlatorSetting= Name(String name), ►\":\", StringResult(String value), \";\"?;\r\n"
     + "\"Name(Attributes)\"(String name, Int32 NumberOfAttributes)= ExtendedName(String name), ►\"(Attributes)\"(Int32 NumberOfAttributes);\r\n"
     + "\"Name(Attributes)\"(String name, Int32 NumberOfAttributes)= ExtendedName(String name)●;";
  _s.Push(1);
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput == LexerResult.Colon)
     {
     Lexer.AcceptSymbol();
     goto State88;
     }
  if (ParserInput == LexerResult.GroupStart)
     goto AcceptState4;
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
  /* Reduction 89, sStack: -1, aStack: 1
   * "Name(Attributes)"(String name, Int32 NumberOfAttributes)= ExtendedName(String name);◄ Priority: -10, aStack: 1, method: NameWithoutAttributes
   */
  _s.Pop();
  _a.Allocate();

  NameWithoutAttributes(
     NumberOfAttributes: out _a.PeekRef(0)._Int32
     );

  goto State8;

State88:
  /* State 88 */
  const String StateDescription88 =
       "GrammlatorSetting= Name(String name), \":\", ►StringResult(String value), \";\"?;";
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
       "GrammlatorSetting= Name(String name), \":\", StringResult(String value), ►\";\"?;";
  ParserInput = Lexer.PeekSymbol();
  if (ParserInput >= LexerResult.TerminatorSymbol)
     {
     Lexer.AcceptSymbol();
     goto Reduce91;
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
Reduce91:
  /* Reduction 91, sStack: -2, aStack: -2
   * GrammlatorSetting= Name(String name), ":", StringResult(String value), ";"?;◄ method: SetGrammlatorSetting, aStack: -2
   * then: OptionalGrammlatorSettings= OptionalGrammlatorSettings, GrammlatorSetting;◄
   */
  _s.Discard(2);

  SetGrammlatorSetting(
     name: _a.PeekRef(-1)._String,
     value: _a.PeekRef(0)._String
     );

  _a.Free(2);
  goto State2;

Reduce47:
  /* Reduction 47, sStack: -2
   * outerDefinitionList= SequenceOfElements, EndOfDefinition, "|", outerDefinitionList;◄
   */
  _s.Discard(2);
  /* Branch 10*/
  switch (_s.Peek())
  {
  case 3:
     goto Reduce43;
  case 4:
     goto Reduce47;
  case 11:
     goto Reduce88;
  /*case 2:
  default: break;
  */
  }
  goto Reduce32;

Branch18:
  /* Branch 18*/
  switch (_s.Peek())
  {
  case 6:
     goto State71;
  case 7:
     goto State68;
  case 9:
     goto State74;
  /*case 8:
  default: break;
  */
  }
  /* Reduction 70, sStack: -1
   * NestedDefinitionList= NestedDefinitionList, "|", Definition;◄
   */
  _s.Pop();
  /* Branch 17*/
  switch (_s.Peek())
  {
  case 6:
     goto State71;
  case 9:
     goto State74;
  /*case 7:
  default: break;
  */
  }
  goto State68;

HandleError28:
  if (ErrorHandler(28, StateDescription28, ParserInput))
     goto State28;
  goto EndWithError1;

HandleError36:
  if (ErrorHandler(36, StateDescription36, ParserInput))
     goto State36;
  goto EndWithError1;

EndWithError1:
  // This point is reached after an input error has been found
  _s.Discard(_s.Count - StateStackInitialCount);
  _a.Free(_a.Count - AttributeStackInitialCount);

EndOfGeneratedCode1:
  ;
#endregion grammlator generated 26.11.2019 by Grammlator version 0:21 (build 26.11.2019 23:39:18 +00:00)
         /* ************************ code written by programmer ******************** */
#pragma warning restore IDE0059 // Der Wert, der dem Symbol zugeordnet ist, wird niemals verwendet.
      }
   }
}
