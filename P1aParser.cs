using GrammlatorRuntime;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Collections;
using System.Runtime.Intrinsics;
using System.Windows.Markup;
using System.ComponentModel;

namespace grammlator {

   #region grammar
   //| /* ---- Start of grammlator grammar as control structure  ---- */
   //|
   //| // Compiler settings:
   //| TerminalSymbolEnum: "LexerResult";
   //| SymbolNameOrFunctionCall: "ParserInput";
   //| SymbolAssignInstruction: "ParserInput = Lexer.PeekSymbol();";
   //| SymbolAcceptInstruction: "Lexer.AcceptSymbol();";
   //| StateDescriptionPrefix: "StateDescription";
   //| ErrorHandlerMethod: "ErrorHandler";
   //| IfToSwitchBorder: 5;
   //| CompareToFlagTestBorder: 4;
   //| LineLengthLimit: 150;
   //| OptimizeStateStackNumbers: true;
   //| IsMethod: "_is";
   //| DebugAssertMethod: "Debug.Assert";    
   //| GenerateComments: true;    
   //|     
   //| // Terminal symbols and their probabilty to appear in input:

   /// <summary>
   /// LexerResult defines the output of the lexer, which is assigned to Symbol to be used by the parser
   /// The elements of LexerResult are ordered such that grammlator can
   /// generate efficient code for the conditions of the parsers actions 
   /// </summary>
   public enum LexerResult {
      [Description("DefiningSymbol() %19")]
      DefiningSymbol, // =
      [Description("Colon() %35")]
      Colon,          // :
      Percent,        // %
      CSharpEnd,      // represents the change from CSharp lines to grammlator lines
      Error,          // Error is the result if some input could not be assigned to any other LexerResult
      Minus,          // Part of "-="
      [Description("Number(Int64 value)")]
      Number,
      StarEqual,      // "*=", added by the lexer
      MinusEqual,     // "-=", addeed by the lexer
      Questionmark,   // part of "??"
      Asterisk,       // Part of "*="
      Plus,           // +
      Comma,          // ,
      NumberSign,     // #

      [Description(@"GroupStart() %35 ""("" ")]
      GroupStart,
      [Description(@"OptionStart() %35 ""[""")]
      OptionStart,
      [Description(@"RepeatStart() %35 ""{"" ")]
      RepeatStart, // these are the characters ( [ {

      DoubleQuestionmark,
      [Description(@"CSharpStart() %19")]
      CSharpStart, // represents the change from grammlator lines to CSharp lines

      GroupEnd, RepeatEnd, OptionEnd,  // these are the characters ) ] } #

      [Description("Name(UnifiedString string) %45")]
      Name,
      [Description("LexerString(UnifiedString string) %30")]
      LexerString,

      [Description(@"DefinitionSeparatorSymbol() %45 ""|"" ")]
      DefinitionSeparatorSymbol, // |
      [Description(@"TerminatorSymbol() %42 "";"" ")]
      TerminatorSymbol
   };
   #endregion grammar

   public static class LexerResultExtensions {
      public static String MyToString(this LexerResult lr)
      {
         const String MyDisplay
            = "=:%xx-xxx?*+,#([{\u2047x)}]xx|;"; // \u2047 is "??" as one character

         Debug.Assert(lr != LexerResult.Error);
         if ((Int32)lr >= MyDisplay.Length)
            return lr.ToString();

         Char result = MyDisplay[(Int32)lr];

         if (result != 'x')
            return result.ToString();
         String s = lr switch
         {
            LexerResult.MinusEqual => "-=",
            LexerResult.DoubleQuestionmark => "??",
            LexerResult.StarEqual => "*=",
            LexerResult.LexerString => "string",
            LexerResult.Name => "name",
            LexerResult.CSharpStart => "C# code",
            LexerResult.CSharpEnd => "end of C# Code",
            _ => lr.ToString(),
         };
         return s;
      }
   }

   /// <summary>
   /// Grammlator Parser (uses Lexer which uses InputClassifier)
   /// </summary>
   internal sealed partial class P1aParser : GrammlatorApplication {
      /// <summary>
      /// Create an instance of the parser (and the lexer and the classifier) and execute it.
      /// Return the SourceReader position of the line containing "region grammlator generated"
      /// </summary>
      /// <param name="SbResult"></param>
      /// <param name="SourceReader"></param>
      /// <param name="symbolDictionary"></param>
      public static Int32 MakeInstanceAndExecute(
          SpanReaderWithCharacterAndLineCounter SourceReader,
          Dictionary<UnifiedString, Symbol> symbolDictionary)
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
          Dictionary<UnifiedString, Symbol> SymbolDictionary
         ) : base(initialSizeOfAttributeStack: 100, initialSizeOfStateStack: 100)
      {
         this.SymbolDictionary = SymbolDictionary;
         this.Source = SourceReader.Source;

         Lexer = new P1bLexer(SourceReader, _a, _s);
      }

      /// <summary>
      /// Analyze the grammar, 
      /// return the SourceReader position of the line containing "region grammlator generated"
      /// </summary>
      /// <returns></returns>
      private Int32 DoPhase1()
      {
         try
         {
            AnalyzeGrammlatorGrammar();

            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Status,
               @$"Found """
              + @$"{GlobalSettings.RegionString.Value} {GlobalSettings.GrammlatorString.Value} {GlobalSettings.GeneratedString.Value}"
              + @$"""");

            GlobalVariables.NumberOfNonterminalSymbols = SymbolDictionary.Count - GlobalVariables.NumberOfTerminalSymbols;

            // Check usage of symbols
            if (CheckUsageOfSymbols(GlobalVariables.OutputMessageAndPosition) >= MessageTypeOrDestinationEnum.Abort)
            {
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Abort, "translation aborted: error(s) in source"); // throws an exception
               Debug.Fail("This debug instruction should never be executed");
            }
         }
         finally { }

         return Lexer.StartOf1stGrammlatorGeneratedLine;
      }

      private readonly P1bLexer Lexer;

      private readonly ReadOnlyMemory<Char> Source;

      #region declaration of fields
      /// <summary>
      /// Set by constructor, contains the terminal symbols followed by the nonterminal symbols
      /// </summary>
      private readonly Dictionary<UnifiedString, Symbol> SymbolDictionary;

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

      //|  *= GrammlatorGrammar /* one startsymbol, which has no attributes */

      //| // renaming some symbols to improve readability
      //|  "," = Comma; // "," is used where provided in EBNF, Comma in other cases
      //|  "=" = DefiningSymbol; "|" = DefinitionSeparatorSymbol; ";" = TerminatorSymbol; ":" = Colon; "%" = Percent;
      //|  "-" = Minus; "+" = Plus; "*" = Asterisk; "(" = GroupStart; "[" = OptionStart; "{" = RepeatStart;
      //|  ")" = GroupEnd; "]" = OptionEnd; "}" = RepeatEnd; "?" = Questionmark;
      //|  "-=" = MinusEqual; "??" = DoubleQuestionmark  /* see below "*=" = StarEqual ... */ 

      //| GrammlatorGrammar=
      //|     OptionalGrammlatorSettings, 
      //|     DeclarationOfTerminalSymbols,
      //|     GrammarRuleList,
      //|     TerminatorAtEndOfGrammar

      //| TerminatorAtEndOfGrammar=
      //|      NumberSign

      //| OptionalGrammlatorSettings=
      //|      /* empty */
      //|    | OptionalGrammlatorSettings, GrammlatorSetting

      //| GrammlatorSetting=
      //|    Name(UnifiedString name), ":", LexerString(UnifiedString value), ";"
      private void SetGrammlatorStringSetting(UnifiedString name, UnifiedString value)
      {
         String NewValue = value.ToString();
         NewValue = NewValue[1..^1]; // remove leading and trailing "
         SetGrammlatorStringSetting(name, NewValue);
      }

      private void SetGrammlatorStringSetting(UnifiedString name, String assignedString)
      {
         String NameToLower = name.ToString().ToLower();
         Setting? s = GlobalSettings.VisibleSettings.Find(s => s.NameToLower == NameToLower);
         if (s == null)
         {
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error, $"Unknow grammlator setting \"{name}\"");
            return;
         }

         switch (s)
         {
         case StringSetting ss:
            ss.Value = assignedString;
            break;
         case BooleanSetting bs:
            if (Boolean.TryParse(assignedString, out Boolean newBooleanValue))
               bs.Value = newBooleanValue;
            else
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
                  $"\"{assignedString}\" is neither \"true\" nor \"false\"");
            break;
         case Int64Setting i64s:
            if (Int64.TryParse(assignedString, out Int64 newIntValue))
               i64s.Value = newIntValue;
            else
               P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error, $"\"{assignedString}\" is not a number");
            break;
         }
         return;
      }


      //|   | Name(UnifiedString name), ":", Number(Int64 value), ";"
      private void SetGrammlatorInt32Setting(UnifiedString name, Int64 value)
      {
         String NameToLower = name.ToString().ToLower(); // TODO avoid new string by better comparer
         Setting? s = GlobalSettings.VisibleSettings.Find(s => s.NameToLower == NameToLower);
         if (s == null)
         {
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error, $"Unknow grammlator setting \"{name}\"");
            return;
         }

         switch (s)
         {
         case StringSetting ss:
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
               $"\"{value}\" isn't a string");
            break;
         case BooleanSetting bs:
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
               $"\"{value}\" is neither \"true\" nor \"false\"");
            break;
         case Int64Setting i32s:
            i32s.Value = (Int32)value;
            break;
         }
         return;
      }
      //|   | Name(UnifiedString name), ":", Name(UnifiedString value), ";"
      private void SetGrammlatorNameSetting(UnifiedString name, UnifiedString value)
         => SetGrammlatorStringSetting(
            name,
            value.ToString()
            );

      //| DeclarationOfTerminalSymbols=
      //|   OptionalDeclarationOfTerminalSymbols
      private void CompareTerminalDeclarationsWithEnum()
      {
         if (EnumName.Index == 0)
            return; // no enum, nothing to add or to compare

         int NewTerminalsDeclaredInEnum = SymbolDictionary.Count - DictCountBeforeEnum;
         if (DictCountBeforeEnum > 0 && NewTerminalsDeclaredInEnum > 0)
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Warning,
@$"{DictCountBeforeEnum} terminals are defined in grammlator lines. The enum defines {NewTerminalsDeclaredInEnum} additional elements.");

         // Use the name of the enum only if not defined by settings in the source
         // because it may be "CopyOfxxx"
         // An empty enum remains possible if no C# enum is defined
         if (GlobalSettings.TerminalSymbolEnum.Value == "")
            GlobalSettings.TerminalSymbolEnum.Value = EnumName.ToString();

         // Find missig enum elements
         ErrorIfMissingEnumElements(out Boolean error, out Boolean ascending);

         // Sort terminal symbols by their enum value
         if (!error && !ascending)
         {
            SortTerminalsByEnumValue();
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Status,
               $"The order of the terminal definitions differs from the order in the enum. The terminals have been sorted according to the enum.");
         }

         // Clear enum info
         EnumNames.Clear();
         EnumValues.Clear();
         EnumName = new UnifiedString();
         ;
      }

      class EnumComparer : IComparer<Symbol> {
         public int Compare(Symbol? s1, Symbol? s2)
         {
            return
               (int)((s1 as TerminalSymbol)!.EnumValue - (s2 as TerminalSymbol)!.EnumValue);

         }
      }

      //|
      //| OptionalDeclarationOfTerminalSymbols=
      //|      OptionalSemikolonOrEnum
      //|    | TerminalSymbolsList, OptionalSemikolonOrEnum

      //|  TerminalSymbolsList=
      //|     TerminalSymbol
      //|     | TerminalSymbolsList, "|", TerminalSymbol

      //|  TerminalSymbol=
      //|      "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes), OptionalWeight(Int64 Weight)
      private void TerminalSymbol(UnifiedString name, Int32 NumberOfAttributes, Int64 Weight)
             => TerminalSymbolDeclaration(name, NumberOfAttributes, Weight);

      //|  OptionalWeight(Int64 weight)=
      //|      /* empty */
      private static void OptionalDefaultWeight(out Int64 weight)
         => weight = GlobalSettings.TerminalDefaultWeight.Value;
      //|     | "%", Number(Int64 weight)

      //|  ExtendedName (UnifiedString name)=
      //|       Name(UnifiedString name)
      //|     | LexerString(UnifiedString name)

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

      //| OptionalSemikolonOrEnum= // at end of terminal definitions
      //|    /* empty */
      //|    | ";"
      //|    | EnumOrEmptyCode

      //| EnumOrEmptyCode=
      //|       CSharpStart,  CSEnumDeclaration, CSharpEnd
      //|    |  CSharpStart, CSharpEnd

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
               new UnifiedString("*"),
               numberOfAttributes: 0,
               weight: 0);
         GlobalVariables.NumberOfTerminalSymbols = SymbolDictionary.Count;
         GlobalVariables.DefineArrayTerminalSymbolByIndex(SymbolDictionary);
         OptimizeTrivialDefinitionsBackup = OptimizeTrivialDefinitions;
         OptimizeTrivialDefinitions = false; // must be disabled while evaluating the definitions of the startsymbol
         SymbolDictionary.Add(
            new UnifiedString(GlobalVariables.Startsymbol.Identifier),
            GlobalVariables.Startsymbol);
      }

      private Boolean OptimizeTrivialDefinitionsBackup;

      //|  GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)=
      //|      outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "=", outerDefinitions
      //|     | outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "-=" ListOfExcludedTerminalSymbols, ";"
      private void EndOfListOfExcludedTerminalSymbols() => EvaluateExcludedTerminalSymbols(ExcludedTerminalSymbols!);

      //| ListOfExcludedTerminalSymbols=
      //|      Name(UnifiedString terminalName)
      private void FirstExcludedTerminalSymbol(UnifiedString terminalName)
      {
         if (ExcludedTerminalSymbols == null || ExcludedTerminalSymbols.Length != GlobalVariables.NumberOfTerminalSymbols)
            ExcludedTerminalSymbols = new BitArray(GlobalVariables.NumberOfTerminalSymbols);
         ExcludedTerminalSymbols.SetAll(false);
         OneMoreExcludedTerminalSymbol(terminalName);
      }

      private BitArray? ExcludedTerminalSymbols;

      //|     | ListOfExcludedTerminalSymbols, "|", Name(UnifiedString name)
      private void OneMoreExcludedTerminalSymbol(UnifiedString name)
      {
         if (!SymbolDictionary.TryGetValue(name, out Symbol? Symbol))
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
      //|    PriorityDeclaration(Int64 constPriority, IntMethodClass dynPriority)
      private void EndOfDefinitionWithPriorityRecognized(Int64 constPriority, IntMethodClass? dynPriority)
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

      //|    | PriorityDeclaration(Int64 constPriority, IntMethodClass dynPriority), SemanticAction(VoidMethodClass method)
      private void EndOfDefinitionWithPriorityAndMethodRecognized(Int64 constPriority, IntMethodClass? dynPriority, VoidMethodClass? method)
      {
         EvaluateDefinition(constPriority, dynPriority, method, OptimizeTrivialDefinitions);
         AttributeCounter = AttributeNumberAtStartOfDefinition;
      }

      //| PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)=
      //|      "??", signedNumber(Int64 constPriority), "??"
      private static void ConstantPriorityGiven(out IntMethodClass? dynamicPriority) => dynamicPriority = null;

      //|    | "??",  // TODO lookahead to be used by dynamic priotity method? "Name(Attributes)"(String Name, Int32 NumberOfAttributes), 
      //|        CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, ["??"]
      private static void DynamicPriorityRecognized(out Int64 constPriority, out IntMethodClass? dynamicPriority, IntMethodClass? intMethod)
      {
         constPriority = 0;
         dynamicPriority = intMethod;
      }

      //|  signedNumber(Int64 value)=
      //|     Number (Int64 value)
      //|     | "+", Number (Int64 value)
      //|     | "-", Number (Int64 value)
      private static void NegateNumber(ref Int64 value) => value = -value;

      //|  SemanticAction(VoidMethodClass method)=
      //|       CSharpStart, CSvoidMethod(VoidMethodClass method), CSharpEnd
      //|     | CSharpStart, CSharpEnd
      private static void EmptySemanticAction(out VoidMethodClass? method) => method = null;

      //|  "Name(Attributes)" (UnifiedString name, Int32 NumberOfAttributes)=
      //|       ExtendedName(UnifiedString name), "(Attributes)"(Int32 NumberOfAttributes)
      //|     | ExtendedName(UnifiedString name) ??-10?? // low priority: if "(" follows then assume that attributes follow
      private static void NameWithoutAttributes(out Int32 NumberOfAttributes) => NumberOfAttributes = 0;

      //|  outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)=
      //|     "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes)
      private void LeftSideOfOuterProduction(out Symbol SymbolAtLeftSide, UnifiedString name, Int32 NumberOfAttributes)
          => SymbolAtLeftSide = NonterminalSymbolDefinition(name, NumberOfAttributes);

      //|  NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)=
      //|        "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes), "="
      private void LeftSideOfNestedProduction(out Symbol SymbolAtLeftSide, UnifiedString name, Int32 NumberOfAttributes)
          => SymbolAtLeftSide = NonterminalSymbolDefinition(name, NumberOfAttributes); // same as LeftSideOfOuterProduction();

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
      //|    | "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes)
      private void FoundSymbolnameInRightSide(out Symbol Symbol, UnifiedString name, Int32 NumberOfAttributes)
          => Symbol = EvaluateSymbolnameFoundInRightSide(name, NumberOfAttributes);

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
      //|    Name(UnifiedString typeString), Name(UnifiedString nameString)
      private void AttributeTypeAndName(out Int32 number, UnifiedString typeString, UnifiedString nameString)
      {
         AttributeCounter++;
         PushAttributeToListOfAttributesOfGrammarRule(typeString, nameString);
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

      //|     | Attribut(Int32 number), ")"
      private static void FirstAttributeOfGroup(out Int32 numberOfAttributesOfGroup, out Int32 smallestNumber, Int32 number)
      {
         smallestNumber = number;
         numberOfAttributesOfGroup = 1;
      }

      //| "(Attributes)" (Int32 numberOfAttributes)=
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
      //|    Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString)
      private void MethodTypeAndNameRecognized(out MethodClass method, UnifiedString methodTypeString, UnifiedString methodNameString)
          => MethodProperties(out method, methodModifier: new UnifiedString(), typeString: methodTypeString, name: methodNameString);

      //|    | Name(UnifiedString modifierString), Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString) /* AccessModifier (private, public ...), type, methodname */
      private void MethodModifierTypeAndNameRecognized(
          out MethodClass method, UnifiedString modifierString, UnifiedString methodTypeString, UnifiedString methodNameString)
          => MethodProperties(out method, modifierString, methodTypeString, methodNameString);

      //|    | Name(UnifiedString modifier1String), Name(UnifiedString modifier2String), Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString) /* AccessModifier (private, public ...), type, methodname */
      private void MethodModifierTypeAndNameRecognized2(
          out MethodClass method, UnifiedString modifier1String, UnifiedString modifier2String, UnifiedString methodTypeString, UnifiedString methodNameString)
          => MethodProperties(out method, modifier1String, modifier2String, methodTypeString, methodNameString);

      //| eFormalParameters=
      //|    /* empty */
      //|    | formalParameters

      //| formalParameters=
      //|    formalParameter
      //|    | formalParameters, Comma, formalParameter

      //| formalParameter=
      //|    Name(UnifiedString typeString), Name(UnifiedString nameString) // allow nullable types // TODO ignore if calls, but if record use n generated code
      private void FormalParameterWithTypeAndName(UnifiedString typeString, UnifiedString nameString)
          => FormalParameter(new UnifiedString(0), typeString, nameString);

      //|    | Name(UnifiedString typeString), "?", Name(UnifiedString nameString) // allow nullable types // TODO ignore if calls, but if record use n generated code
      private void FormalParameterWithNullableTypeAndName(UnifiedString typeString, UnifiedString nameString)
          => FormalParameter(new UnifiedString(), typeString, nameString);

      //|    | Name(UnifiedString ParameterModifierOptString), Name(UnifiedString typeString), Name(UnifiedString nameString)  /* ref or out, type, identifier */
      private void FormalParameterWithModifierTypeAndName(UnifiedString ParameterModifierOptString, UnifiedString typeString, UnifiedString nameString)
          => FormalParameter(ParameterModifierOptString, typeString, nameString);

      //|    | Name(UnifiedString ParameterModifierOptString), Name(UnifiedString typeString), "?", Name(UnifiedString nameString)  /* ref or out, type, identifier */
      private void FormalParameterWithModifierNullableTypeAndName(UnifiedString ParameterModifierOptString, UnifiedString typeString, UnifiedString nameString)
          => FormalParameter(ParameterModifierOptString, typeString, nameString);

      //| CSEnumDeclaration=
      //|    CSEnumProperties, optionalBaseType, CSEnumMembers
      private void CSEnumRecognized()
      {
         // all elements of the enum have been added to Enumlist by CSEnumMember
         Lexer.SkipToEndOfCSLines(); // allows some unchecked code after the enum
      }

      //| CSEnumProperties=
      //|    Name(UnifiedString modifier1StringIndex), Name(UnifiedString modifier2StringIndex), Name(UnifiedString enumStringIndex), CSEnumName
      //|    | Name(UnifiedString modifierStringIndex), Name(UnifiedString enumStringIndex), CSEnumName
      //|    | Name(UnifiedString enumStringIndex), CSEnumName

      //| CSEnumName= Name(UnifiedString nameString)
      private void EnumNameRecognized(UnifiedString nameString)
      {
         DictCountBeforeEnum = SymbolDictionary.Count;
         EnumName = nameString;
      }
      int DictCountBeforeEnum = 0;

      //| optionalBaseType=
      //|    /* empty */
      //|    | ":", Name(UnifiedString Ignored);
      //|
      //| CSEnumMembers=
      //|    "{", "}"
      private void EmptyEnumRecognized()
      {
         ResetEnumDefaults();
      }
      //|    | "{", CSEnumMemberList, "}"

      //| CSEnumMemberList=
      //|    ResetEnumDefaults, CSEnumMember
      //|    | CSEnumMemberList, Comma, CSEnumMember

      //| CSEnumMember=
      //|    OptionalDescriptionAttribute(String description), Name(UnifiedString enumElementString), OptionalEnumElementNumber(Int64 enumNumber)
      private void EnumElementRecognized(String description, UnifiedString enumElementString, Int64 enumNumber)
      {
         EvaluateEnumElement(description, enumElementString, enumNumber);
      }

      //| OptionalEnumElementNumber(Int64 enumNumber)=
      //|   /* empty */
      private void NoEnumElementNumber(out Int64 enumNumber)
      {
         Int64 NextValue = EnumValues.Count <= 0 ? 0 : EnumValues[^1] + 1;
         enumNumber = NextValue; // TODO use Int64
      }
      //|   | "=", Number(Int64 enumNumber);
      //|
      //| ResetEnumDefaults= /*empty*/
      private void ResetEnumDefaults()
      {
         EnumNames.Clear();
         EnumValues.Clear();
      }
      //|
      //| OptionalDescriptionAttribute(String description)=
      //|      /* empty */ 
      private void NoDescriptionAttribute(out String description)
      {
         description = "";
      }
      //|    | "[", Name(UnifiedString attributeIdentifier) , "(", LexerString(UnifiedString descriptionString), ")", "]"
      private void DescriptionAttribute(out String description, UnifiedString attributeIdentifier, UnifiedString descriptionString)
      {
         if (attributeIdentifier.ToString() != "Description")
            P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
               @$"Expected attribute ""Description"", found ""{attributeIdentifier}""");
         description = descriptionString.ToString();
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
         _a.Remove(_a.Count - aCountBeforeAccept);  // discard the attributes of the discarded terminal symbol
         return true; // true: continue analysis (goto state ...), else "goto EndWithError..."
                      // TODO Errorhandling: design and implement a concept to insert a missing character (e.g. if state accepts only a single terminal symbol)
      }

      private void AnalyzeGrammlatorGrammar()
      {
         // Declare local variables used by grammlator generated code
         LexerResult ParserInput;
         /* ************************ end of code written by programmer ******************** */
         #region grammlator generated 15 Okt 2020 (grammlator file version/date 2020.10.15.0/15 Okt 2020)
         Int32 _StateStackInitialCount = _s.Count;
         Int32 _AttributeStackInitialCount = _a.Count;
         const Int64 _fColon = 1L << (Int32)(LexerResult.Colon);
         const Int64 _fPercent = 1L << (Int32)(LexerResult.Percent);
         const Int64 _fCSharpEnd = 1L << (Int32)(LexerResult.CSharpEnd);
         const Int64 _fError = 1L << (Int32)(LexerResult.Error);
         const Int64 _fMinus = 1L << (Int32)(LexerResult.Minus);
         const Int64 _fNumber = 1L << (Int32)(LexerResult.Number);
         const Int64 _fStarEqual = 1L << (Int32)(LexerResult.StarEqual);
         const Int64 _fMinusEqual = 1L << (Int32)(LexerResult.MinusEqual);
         const Int64 _fNumberSign = 1L << (Int32)(LexerResult.NumberSign);
         const Int64 _fGroupStart = 1L << (Int32)(LexerResult.GroupStart);
         const Int64 _fCSharpStart = 1L << (Int32)(LexerResult.CSharpStart);
         const Int64 _fName = 1L << (Int32)(LexerResult.Name);
         const Int64 _fLexerString = 1L << (Int32)(LexerResult.LexerString);
         const Int64 _fDefinitionSeparatorSymbol = 1L << (Int32)(LexerResult.DefinitionSeparatorSymbol);
         Boolean _is(Int64 flags) => (1L << (Int32)((ParserInput)) & flags) != 0;

      State2:
         const String StateDescription2 =
              "GrammlatorGrammar= OptionalGrammlatorSettings, ►DeclarationOfTerminalSymbols, GrammarRuleList, TerminatorAtEndOfGrammar;\r\n"
            + "OptionalGrammlatorSettings= OptionalGrammlatorSettings, ►GrammlatorSetting;";
         _s.Push(0);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            goto State74;
         }
         if (ParserInput >= LexerResult.TerminatorSymbol)
         {
            Lexer.AcceptSymbol();
            goto Reduce2;
         }
         if (ParserInput == LexerResult.LexerString)
            goto AcceptState69;
         if (ParserInput == LexerResult.StarEqual)
            goto Reduce2;
         if (ParserInput != LexerResult.CSharpStart)
         {
            if (ErrorHandler(2, StateDescription2, ParserInput))
            {
               _s.Pop();
               goto State2;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpStart);
      AcceptState79:
         Lexer.AcceptSymbol();
      State79:
         const String StateDescription79 =
              "EnumOrEmptyCode= CSharpStart, ►CSEnumDeclaration, CSharpEnd;\r\n"
            + "EnumOrEmptyCode= CSharpStart, ►CSharpEnd;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            goto State96;
         }
         if (ParserInput != LexerResult.CSharpEnd)
         {
            if (ErrorHandler(79, StateDescription79, ParserInput))
               goto State79;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpEnd);
      AcceptBranch17:
         Lexer.AcceptSymbol();
         // Branch17:
         if (_s.Peek() == 0)
            goto Reduce2;
         Reduce6:
         /* sAdjust: -1
          * OptionalDeclarationOfTerminalSymbols= TerminalSymbolsList, OptionalSemikolonOrEnum;◄ */
         _s.Pop();
      Reduce2:
         /* DeclarationOfTerminalSymbols= OptionalDeclarationOfTerminalSymbols;◄ */

         CompareTerminalDeclarationsWithEnum();

      State7:
         const String StateDescription7 =
              "GrammlatorGrammar= OptionalGrammlatorSettings, DeclarationOfTerminalSymbols, ►GrammarRuleList, TerminatorAtEndOfGrammar;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.StarEqual)
         {
            if (ErrorHandler(7, StateDescription7, ParserInput))
               goto State7;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.StarEqual);
         Lexer.AcceptSymbol();
         // Reduce8:
         /* "*="= StarEqual;◄ */

         StartOfFirstGrammarRule();

      State8:
         const String StateDescription8 =
              "FirstGrammarRule= \"*=\", ►outerDefinitions;";
         _s.Push(2);
         ParserInput = Lexer.PeekSymbol();
         switch (ParserInput)
         {
         // <= LexerResult.NumberSign: goto HandleError8 // see end of switch
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
            goto HandleError8;
         case LexerResult.GroupStart:
            goto AcceptState61;
         case LexerResult.OptionStart:
            goto AcceptState59;
         case LexerResult.RepeatStart:
            goto AcceptState46;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState35;
         case LexerResult.CSharpStart:
            goto AcceptState21;
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto AcceptState69;
            // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce9 // see end of switch
         } // end of switch
         if (ParserInput <= LexerResult.NumberSign)
            goto HandleError8;
         Debug.Assert(ParserInput >= LexerResult.DefinitionSeparatorSymbol);

      Reduce9:
         /* EndOfDefinitionWithoutSemantics= ;◄ */

         EndOfDefinitionWithoutSemanticsRecognized();

         // State13:
         /* outerDefinitions= EndOfDefinitionWithoutSemantics, ►";";
          * outerDefinitions= EndOfDefinition, ►"|", outerDefinitionList; */
         if (ParserInput <= LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState14;
         Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
      AcceptBranch5:
         Lexer.AcceptSymbol();
      Branch5:
         if (_s.Peek() == 2)
            goto Reduce11;
         Reduce67:
         /* sAdjust: -1
          * GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "=", outerDefinitions;◄ */
         _s.Pop();
      Reduce63:
         /* sAdjust: -1, aAdjust: -2
          * GrammarRuleList= GrammarRuleList, GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes);◄ */
         _s.Pop();

         EndOfGrammarRuleRecognized(
            SymbolAtLeftSide: _a.PeekRef(-1)._Symbol
            );

         _a.Remove(2);
      State63:
         const String StateDescription63 =
              "GrammlatorGrammar= OptionalGrammlatorSettings, DeclarationOfTerminalSymbols, GrammarRuleList, ►TerminatorAtEndOfGrammar;\r\n"
            + "GrammarRuleList= GrammarRuleList, ►GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes);";
         _s.Push(10);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.NumberSign)
         {
            Lexer.AcceptSymbol();
            // Reduce61:
            /* sAdjust: -2
             * GrammlatorGrammar= OptionalGrammlatorSettings, DeclarationOfTerminalSymbols, GrammarRuleList, TerminatorAtEndOfGrammar;◄ */
            _s.Discard(2);
            goto EndOfGeneratedCode;
         }
         if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(63, StateDescription63, ParserInput))
            {
               _s.Pop();
               goto State63;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
      AcceptState69:
         Lexer.AcceptSymbol();
      State69:
         const String StateDescription69 =
              "\"Name(Attributes)\"(UnifiedString name, Int32 NumberOfAttributes)= ExtendedName(UnifiedString name), ►\"(Attributes)\"(Int32 NumberOfAttributes);\r\n"
            + "\"Name(Attributes)\"(UnifiedString name, Int32 NumberOfAttributes)= ExtendedName(UnifiedString name)●;";
         // *Push(0)
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.GroupStart)
         {
            Lexer.AcceptSymbol();
            // PushState5:
            _s.Push(0);
            goto State70;
         }
         if (_is(_fColon | _fCSharpEnd | _fError | _fMinus | _fNumber | _fNumberSign))
         {
            if (ErrorHandler(69, StateDescription69, ParserInput))
               goto State69;
            // PushState6:
            _s.Push(0);
            goto EndWithError;
         }
         Debug.Assert(!_is(_fColon | _fCSharpEnd | _fError | _fMinus | _fNumber | _fNumberSign | _fGroupStart));
         // Reduce68:
         /* aAdjust: 1
          * "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes)= ExtendedName(UnifiedString name);◄ */
         _a.Allocate();

         NameWithoutAttributes(
            NumberOfAttributes: out _a.PeekRef(0)._Int32
            );

      Branch14:
         switch (_s.Peek())
         {
         case 0:
         case 1:
            goto State3;
         case 6:
            goto State57;
         case 10:
         // Reduce62:
         {
            /* outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes);◄ */

            LeftSideOfOuterProduction(
               SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
               name: _a.PeekClear(-1)._UnifiedString,
               NumberOfAttributes: _a.PeekRef(0)._Int32
               );

            goto State64;
         }
         /*case 2: case 3: case 4: case 5: case 7: case 8: case 9: case 11:
         default: break; */
         }
      Reduce10:
         /* aAdjust: -1
          * SimpleElement(Symbol Symbol)= "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes);◄ */

         FoundSymbolnameInRightSide(
            Symbol: out _a.PeekRef(-1)._Symbol,
            name: _a.PeekClear(-1)._UnifiedString,
            NumberOfAttributes: _a.PeekRef(0)._Int32
            );

         _a.Remove();
      State9:
         const String StateDescription9 =
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
            // Reduce15:
            /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "?";◄ */

            OptionalElementRecognized(
               Symbol: ref _a.PeekRef(0)._Symbol
               );

            goto Branch3;
         }
         if (ParserInput == LexerResult.Asterisk)
         {
            Lexer.AcceptSymbol();
            goto State11;
         }
         if (ParserInput == LexerResult.Plus)
         {
            Lexer.AcceptSymbol();
            goto State10;
         }
         if (ParserInput <= LexerResult.MinusEqual
            || ParserInput == LexerResult.NumberSign)
         {
            if (ErrorHandler(9, StateDescription9, ParserInput))
               goto State9;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Comma
            || ParserInput >= LexerResult.GroupStart);
         // Reduce14:
         /* aAdjust: -1
          * Element= RepeatedElement(Symbol Symbol);◄ */

         ElementVariantRecognized(
            Symbol: _a.PeekRef(0)._Symbol
            );

         _a.Remove();
      Branch2:
         switch (_s.Peek())
         {
         case 5:
         // Reduce27:
         {
            /* sAdjust: -2
             * SequenceOfElements= SequenceOfElements, ","?, Element;◄ */
            _s.Discard(2);
            goto Branch2;
         }
         case 6:
         case 7:
         case 8:
         case 9:
            goto State52;
            /*case 2: case 3: case 4: case 11:
            default: break; */
         }
      State16:
         const String StateDescription16 =
              "SequenceOfElements= SequenceOfElements, ►\",\"?, Element;\r\n"
            + "outerDefinitionList= SequenceOfElements, ►EndOfDefinition, \"|\", outerDefinitionList;\r\n"
            + "outerLastDefinitionOfSequence= SequenceOfElements, ►EndOfDefinitionWithoutSemantics, \";\";\r\n"
            + "outerLastDefinitionOfSequence= SequenceOfElements, ►EndOfDefinitionWithSemantics, \";\"?;";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         switch (ParserInput)
         {
         // <= LexerResult.Plus: goto HandleError16 // see end of switch
         case LexerResult.NumberSign:
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
            goto HandleError16;
         case LexerResult.Comma:
            goto AcceptState20;
         case LexerResult.GroupStart:
         case LexerResult.OptionStart:
         case LexerResult.RepeatStart:
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto State20;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState35;
         case LexerResult.CSharpStart:
            goto AcceptState21;
            // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce23 // see end of switch
         } // end of switch
         if (ParserInput <= LexerResult.Plus)
            goto HandleError16;
         Debug.Assert(ParserInput >= LexerResult.DefinitionSeparatorSymbol);

         // Reduce23:
         /* EndOfDefinitionWithoutSemantics= ;◄ */

         EndOfDefinitionWithoutSemanticsRecognized();

         // State17:
         /* outerDefinitionList= SequenceOfElements, EndOfDefinition, ►"|", outerDefinitionList;
          * outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithoutSemantics, ►";"; */
         if (ParserInput <= LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState18;
         Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
      AcceptReduce25:
         Lexer.AcceptSymbol();
      Reduce25:
         /* sAdjust: -1
          * outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithoutSemantics, ";";◄
          * or: outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithSemantics, ";"?;◄ */
         _s.Pop();
         // Branch6:
         switch (_s.Peek())
         {
         case 3:
            goto Reduce22;
         case 4:
            goto Reduce26;
         case 11:
            goto Reduce67;
            /*case 2:
            default: break; */
         }
      Reduce11:
         /* sAdjust: -1
          * FirstGrammarRule= "*=", outerDefinitions;◄ */
         _s.Pop();

         FirstGrammarRuleRecognized();

         goto State63;

      Reduce12:
         /* aAdjust: -1
          * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ */

         EndOfDefinitionWithMethodRecognized(
            method: _a.PeekRef(0)._VoidMethodClass
            );

         _a.Remove();
      State15:
         const String StateDescription15 =
              "outerDefinitions= EndOfDefinitionWithSemantics, ►\";\"?;\r\n"
            + "outerDefinitions= EndOfDefinition, ►\"|\", outerDefinitionList;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState14;
         if (ParserInput >= LexerResult.TerminatorSymbol)
            goto AcceptBranch5;
         if (ParserInput != LexerResult.NumberSign
            && ParserInput < LexerResult.Name)
         {
            if (ErrorHandler(15, StateDescription15, ParserInput))
               goto State15;
            goto EndWithError;
         }
         Debug.Assert(_is(_fNumberSign | _fName | _fLexerString));
         goto Branch5;

      AcceptReduce30:
         Lexer.AcceptSymbol();
         // Reduce30:
         /* sAdjust: -1
          * CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), "(", eFormalParameters, ")";◄ */
         _s.Pop();

         CSvoidMethodRecognized(
            voidMethod: out _a.PeekRef(0)._VoidMethodClass,
            method: _a.PeekClear(0)._MethodClass
            );

      State31:
         const String StateDescription31 =
              "SemanticAction(VoidMethodClass method)= CSharpStart, CSvoidMethod(VoidMethodClass method), ►CSharpEnd;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.CSharpEnd)
         {
            if (ErrorHandler(31, StateDescription31, ParserInput))
               goto State31;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpEnd);
         Lexer.AcceptSymbol();
         // Reduce36:
         /* sAdjust: -1
          * SemanticAction(VoidMethodClass method)= CSharpStart, CSvoidMethod(VoidMethodClass method), CSharpEnd;◄ */
         _s.Pop();
      Branch8:
         switch (_s.Peek())
         {
         case 1:
         // Reduce24:
         {
            /* aAdjust: -1
             * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ */

            EndOfDefinitionWithMethodRecognized(
               method: _a.PeekRef(0)._VoidMethodClass
               );

            _a.Remove();
            goto State19;
         }
         case 2:
         case 11:
            goto Reduce12;
         case 3:
         // Reduce53:
         {
            /* sAdjust: -1, aAdjust: -1
             * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄
             * then: NestedDefinitionList= NestedDefinitionList, "|", Definition;◄
             * or: Definition= SequenceOfElements, EndOfDefinition;◄ */
            _s.Pop();

            EndOfDefinitionWithMethodRecognized(
               method: _a.PeekRef(0)._VoidMethodClass
               );

            _a.Remove();
            goto Branch13;
         }
         case 6:
         // Reduce48:
         {
            /* aAdjust: -1
             * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ */

            EndOfDefinitionWithMethodRecognized(
               method: _a.PeekRef(0)._VoidMethodClass
               );

            _a.Remove();
            goto State48;
         }
         case 9:
         // Reduce56:
         {
            /* aAdjust: -1
             * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ */

            EndOfDefinitionWithMethodRecognized(
               method: _a.PeekRef(0)._VoidMethodClass
               );

            _a.Remove();
            goto State55;
         }
         /*case 0:
         default: break; */
         }
         // Reduce21:
         /* sAdjust: -1, aAdjust: -3
          * EndOfDefinitionWithSemantics= PriorityDeclaration(Int64 constPriority, IntMethodClass dynPriority), SemanticAction(VoidMethodClass method);◄ */
         _s.Pop();

         EndOfDefinitionWithPriorityAndMethodRecognized(
            constPriority: _a.PeekRef(-2)._Int64,
            dynPriority: _a.PeekRef(-1)._IntMethodClass,
            method: _a.PeekRef(0)._VoidMethodClass
            );

         _a.Remove(3);
      Branch4:
         switch (_s.Peek())
         {
         case 2:
         case 11:
            goto State15;
         case 3:
            goto Reduce51;
         case 6:
            goto State48;
         case 9:
            goto State55;
            /*case 1:
            default: break; */
         }
      State19:
         const String StateDescription19 =
              "outerDefinitionList= SequenceOfElements, EndOfDefinition, ►\"|\", outerDefinitionList;\r\n"
            + "outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithSemantics, ►\";\"?;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState18;
         if (ParserInput >= LexerResult.TerminatorSymbol)
            goto AcceptReduce25;
         if (ParserInput != LexerResult.NumberSign
            && ParserInput < LexerResult.Name)
         {
            if (ErrorHandler(19, StateDescription19, ParserInput))
               goto State19;
            goto EndWithError;
         }
         Debug.Assert(_is(_fNumberSign | _fName | _fLexerString));
         goto Reduce25;

      AcceptReduce40:
         Lexer.AcceptSymbol();
         // Reduce40:
         /* sAdjust: -1
          * CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), "(", eFormalParameters, ")";◄ */
         _s.Pop();

         CSintMethodRecognized(
            intMethod: out _a.PeekRef(0)._IntMethodClass,
            method: _a.PeekClear(0)._MethodClass
            );

      State41:
         const String StateDescription41 =
              "PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, CSintMethod(IntMethodClass intMethod), ►CSharpEnd, (Local1)?;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.CSharpEnd)
         {
            if (ErrorHandler(41, StateDescription41, ParserInput))
               goto State41;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpEnd);
         Lexer.AcceptSymbol();
      State42:
         const String StateDescription42 =
              "PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, ►(Local1)?;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DoubleQuestionmark)
         {
            Lexer.AcceptSymbol();
            goto Reduce41;
         }
         if (ParserInput != LexerResult.NumberSign
            && ParserInput < LexerResult.CSharpStart)
         {
            if (ErrorHandler(42, StateDescription42, ParserInput))
               goto State42;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.NumberSign
            || ParserInput >= LexerResult.CSharpStart);
      Reduce41:
         /* sAdjust: -1, aAdjust: 1
          * PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= "??", CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, (Local1)?;◄ */
         _s.Pop();
         _a.Allocate();

         DynamicPriorityRecognized(
            constPriority: out _a.PeekRef(-1)._Int64,
            dynamicPriority: out _a.PeekRef(0)._IntMethodClass,
            intMethod: _a.PeekClear(-1)._IntMethodClass
            );

      State12:
         const String StateDescription12 =
              "EndOfDefinitionWithSemantics= PriorityDeclaration(Int64 constPriority, IntMethodClass dynPriority)●;\r\n"
            + "EndOfDefinitionWithSemantics= PriorityDeclaration(Int64 constPriority, IntMethodClass dynPriority), ►SemanticAction(VoidMethodClass method);";
         // *Push(0)
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.CSharpStart)
         {
            Lexer.AcceptSymbol();
            // PushState1:
            _s.Push(0);
            goto State21;
         }
         if (ParserInput != LexerResult.NumberSign
            && ParserInput < LexerResult.GroupEnd)
         {
            if (ErrorHandler(12, StateDescription12, ParserInput))
               goto State12;
            // PushState2:
            _s.Push(0);
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.NumberSign
            || ParserInput >= LexerResult.GroupEnd);
         // Reduce20:
         /* aAdjust: -2
          * EndOfDefinitionWithSemantics= PriorityDeclaration(Int64 constPriority, IntMethodClass dynPriority);◄ */

         EndOfDefinitionWithPriorityRecognized(
            constPriority: _a.PeekRef(-1)._Int64,
            dynPriority: _a.PeekRef(0)._IntMethodClass
            );

         _a.Remove(2);
         goto Branch4;

      Reduce44:
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

      State47:
         const String StateDescription47 =
              "NestedElement(Symbol Symbol)= SaveVariables(Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions, Int32 SavedNumberOfTrivialDefinitions, Int32 SavedNumberOfElements, Int32 SavedNumberOfSymbolAttributes), ►NestedGrammarRule(Symbol NestedSymbol, Int32 NumberOfAttributes);";
         _s.Push(6);
         ParserInput = Lexer.PeekSymbol();
         switch (ParserInput)
         {
         // <= LexerResult.NumberSign
         // >= LexerResult.TerminatorSymbol: goto HandleError47 // see end of switch
         case LexerResult.GroupStart:
            goto AcceptState61;
         case LexerResult.OptionStart:
            goto AcceptState59;
         case LexerResult.RepeatStart:
            goto AcceptState46;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState35;
         case LexerResult.CSharpStart:
            goto AcceptState21;
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
         case LexerResult.DefinitionSeparatorSymbol:
         // Reduce45:
         {
            /* EndOfDefinitionWithoutSemantics= ;◄ */

            EndOfDefinitionWithoutSemanticsRecognized();

            goto State48;
         }
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto AcceptState69;
         } // end of switch
         Debug.Assert(ParserInput <= LexerResult.NumberSign || ParserInput >= LexerResult.TerminatorSymbol);

         if (ErrorHandler(47, StateDescription47, ParserInput))
         {
            _s.Pop();
            goto State47;
         };
         goto EndWithError;

      Reduce47:
         /* aAdjust: 2
          * NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions;◄ */
         _a.Allocate(2);

         NestedGrammarRuleWithEmptyLeftside(
            SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
            NumberOfAttributes: out _a.PeekRef(0)._Int32
            );

      Reduce46:
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

         _a.Remove(6);
         // Branch11:
         switch (_s.Peek())
         {
         case 0:
            goto State58;
         case 1:
            goto State60;
            /*case 2:
            default: break; */
         }
      State62:
         const String StateDescription62 =
              "SimpleElement(Symbol Symbol)= \"(\", NestedElement(Symbol Symbol), ►\")\";";
         if (ParserInput > LexerResult.GroupEnd)
         {
            if (ErrorHandler(62, StateDescription62, ParserInput))
               goto State62;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         Lexer.AcceptSymbol();
         // Reduce60:
         /* sAdjust: -1
          * SimpleElement(Symbol Symbol)= "(", NestedElement(Symbol Symbol), ")";◄ */
         _s.Pop();
         goto State9;

      Reduce49:
         /* aAdjust: -1
          * Element= RepeatedElement(Symbol Symbol);◄ */

         ElementVariantRecognized(
            Symbol: _a.PeekRef(0)._Symbol
            );

         _a.Remove();
      State52:
         const String StateDescription52 =
              "Definition= SequenceOfElements, ►EndOfDefinition;\r\n"
            + "SequenceOfElements= SequenceOfElements, ►\",\"?, Element;";
         _s.Push(3);
         ParserInput = Lexer.PeekSymbol();
         switch (ParserInput)
         {
         // <= LexerResult.Plus
         // >= LexerResult.TerminatorSymbol: goto HandleError52 // see end of switch
         case LexerResult.NumberSign:
            goto HandleError52;
         case LexerResult.Comma:
            goto AcceptState20;
         case LexerResult.GroupStart:
         case LexerResult.OptionStart:
         case LexerResult.RepeatStart:
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto State20;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState35;
         case LexerResult.CSharpStart:
            goto AcceptState21;
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
         case LexerResult.DefinitionSeparatorSymbol:
         // Reduce52:
         {
            /* sAdjust: -1
             * EndOfDefinitionWithoutSemantics= ;◄
             * then: NestedDefinitionList= NestedDefinitionList, "|", Definition;◄
             * or: Definition= SequenceOfElements, EndOfDefinition;◄ */
            _s.Pop();

            EndOfDefinitionWithoutSemanticsRecognized();

            goto Branch13;
         }
         } // end of switch
         Debug.Assert(ParserInput <= LexerResult.Plus || ParserInput >= LexerResult.TerminatorSymbol);

      HandleError52:
         if (ErrorHandler(52, StateDescription52, ParserInput))
         {
            _s.Pop();
            goto State52;
         };
         goto EndWithError;

      Reduce51:
         /* sAdjust: -1
          * NestedDefinitionList= NestedDefinitionList, "|", Definition;◄
          * or: Definition= SequenceOfElements, EndOfDefinition;◄ */
         _s.Pop();
      Branch13:
         switch (_s.Peek())
         {
         case 6:
            goto State53;
         case 7:
            goto State50;
         case 8:
            goto Reduce51;
            /*case 9:
            default: break; */
         }
      State56:
         const String StateDescription56 =
              "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions●;\r\n"
            + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState51;
         if (ParserInput <= LexerResult.CSharpStart
            || ParserInput >= LexerResult.Name)
         {
            if (ErrorHandler(56, StateDescription56, ParserInput))
               goto State56;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
      Reduce55:
         /* sAdjust: -1
          * NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions;◄ */
         _s.Pop();
         goto Reduce46;

      Reduce80:
         /* CSEnumDeclaration= CSEnumProperties, optionalBaseType, CSEnumMembers;◄ */

         CSEnumRecognized();

      State95:
         const String StateDescription95 =
              "EnumOrEmptyCode= CSharpStart, CSEnumDeclaration, ►CSharpEnd;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.CSharpEnd)
         {
            if (ErrorHandler(95, StateDescription95, ParserInput))
               goto State95;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpEnd);
         goto AcceptBranch17;

      Reduce83:
         /* aAdjust: 1
          * OptionalDescriptionAttribute(String description)= ;◄ */
         _a.Allocate();

         NoDescriptionAttribute(
            description: out _a.PeekRef(0)._String
            );

      State84:
         const String StateDescription84 =
              "CSEnumMember= OptionalDescriptionAttribute(String description), ►Name(UnifiedString enumElementString), OptionalEnumElementNumber(Int64 enumNumber);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(84, StateDescription84, ParserInput))
               goto State84;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
      State85:
         const String StateDescription85 =
              "CSEnumMember= OptionalDescriptionAttribute(String description), Name(UnifiedString enumElementString), ►OptionalEnumElementNumber(Int64 enumNumber);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput <= LexerResult.DefiningSymbol)
         {
            Lexer.AcceptSymbol();
            goto State86;
         }
         if (ParserInput != LexerResult.Comma
            && ParserInput != LexerResult.RepeatEnd)
         {
            if (ErrorHandler(85, StateDescription85, ParserInput))
               goto State85;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Comma
            || ParserInput == LexerResult.RepeatEnd);
         // Reduce85:
         /* aAdjust: 1
          * OptionalEnumElementNumber(Int64 enumNumber)= ;◄ */
         _a.Allocate();

         NoEnumElementNumber(
            enumNumber: out _a.PeekRef(0)._Int64
            );

      Reduce86:
         /* sAdjust: -1, aAdjust: -3
          * CSEnumMember= OptionalDescriptionAttribute(String description), Name(UnifiedString enumElementString), OptionalEnumElementNumber(Int64 enumNumber);◄
          * then: CSEnumMemberList= ResetEnumDefaults, CSEnumMember;◄
          * or: CSEnumMemberList= CSEnumMemberList, Comma, CSEnumMember;◄ */
         _s.Pop();

         EnumElementRecognized(
            description: _a.PeekRef(-2)._String,
            enumElementString: _a.PeekRef(-1)._UnifiedString,
            enumNumber: _a.PeekRef(0)._Int64
            );

         _a.Remove(3);
      State92:
         const String StateDescription92 =
              "CSEnumMembers= \"{\", CSEnumMemberList, ►\"}\";\r\n"
            + "CSEnumMemberList= CSEnumMemberList, ►Comma, CSEnumMember;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Comma)
         {
            Lexer.AcceptSymbol();
            goto State93;
         }
         if (ParserInput != LexerResult.RepeatEnd)
         {
            if (ErrorHandler(92, StateDescription92, ParserInput))
               goto State92;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.RepeatEnd);
         Lexer.AcceptSymbol();
         goto Reduce80;

      State3:
         const String StateDescription3 =
              "TerminalSymbol= \"Name(Attributes)\"(UnifiedString name, Int32 NumberOfAttributes), ►OptionalWeight(Int64 Weight);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Percent)
         {
            Lexer.AcceptSymbol();
            goto State4;
         }
         if (ParserInput < LexerResult.TerminatorSymbol && !_is(_fStarEqual | _fCSharpStart | _fDefinitionSeparatorSymbol))
         {
            if (ErrorHandler(3, StateDescription3, ParserInput))
               goto State3;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol || _is(_fStarEqual | _fCSharpStart | _fDefinitionSeparatorSymbol));
         // Reduce4:
         /* aAdjust: 1
          * OptionalWeight(Int64 weight)= ;◄ */
         _a.Allocate();

         OptionalDefaultWeight(
            weight: out _a.PeekRef(0)._Int64
            );

      Reduce5:
         /* aAdjust: -3
          * TerminalSymbol= "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes), OptionalWeight(Int64 Weight);◄ */

         TerminalSymbol(
            name: _a.PeekRef(-2)._UnifiedString,
            NumberOfAttributes: _a.PeekRef(-1)._Int32,
            Weight: _a.PeekRef(0)._Int64
            );

         _a.Remove(3);
         // Branch1:
         if (_s.Peek() == 0)
            goto State5;
         // Reduce7:
         /* sAdjust: -2
          * TerminalSymbolsList= TerminalSymbolsList, "|", TerminalSymbol;◄ */
         _s.Discard(2);
      State5:
         const String StateDescription5 =
              "OptionalDeclarationOfTerminalSymbols= TerminalSymbolsList, ►OptionalSemikolonOrEnum;\r\n"
            + "TerminalSymbolsList= TerminalSymbolsList, ►\"|\", TerminalSymbol;";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
         {
            Lexer.AcceptSymbol();
            goto State6;
         }
         if (ParserInput >= LexerResult.TerminatorSymbol)
         {
            Lexer.AcceptSymbol();
            goto Reduce6;
         }
         if (ParserInput == LexerResult.StarEqual)
            goto Reduce6;
         if (ParserInput != LexerResult.CSharpStart)
         {
            if (ErrorHandler(5, StateDescription5, ParserInput))
            {
               _s.Pop();
               goto State5;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpStart);
         goto AcceptState79;

      State4:
         const String StateDescription4 =
              "OptionalWeight(Int64 weight)= \"%\", ►Number(Int64 weight);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Number)
         {
            if (ErrorHandler(4, StateDescription4, ParserInput))
               goto State4;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Number);
         Lexer.AcceptSymbol();
         goto Reduce5;

      State6:
         const String StateDescription6 =
              "TerminalSymbolsList= TerminalSymbolsList, \"|\", ►TerminalSymbol;";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(6, StateDescription6, ParserInput))
            {
               _s.Pop();
               goto State6;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
         goto AcceptState69;

      State10:
         const String StateDescription10 =
              "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"+\"●;\r\n"
            + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"+\", ►\"+\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Plus)
         {
            Lexer.AcceptSymbol();
            // Reduce17:
            /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "+", "+";◄ */

            Repeat1rrRecognized(
               Symbol: ref _a.PeekRef(0)._Symbol
               );

            goto Branch3;
         }
         if (ParserInput <= LexerResult.Asterisk
            || ParserInput == LexerResult.NumberSign)
         {
            if (ErrorHandler(10, StateDescription10, ParserInput))
               goto State10;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Comma
            || ParserInput >= LexerResult.GroupStart);
         // Reduce16:
         /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "+";◄ */

         Repeat1lrRecognized(
            Symbol: ref _a.PeekRef(0)._Symbol
            );

      Branch3:
         switch (_s.Peek())
         {
         case 5:
         // Reduce28:
         {
            /* sAdjust: -2, aAdjust: -1
             * Element= RepeatedElement(Symbol Symbol);◄
             * then: SequenceOfElements= SequenceOfElements, ","?, Element;◄ */
            _s.Discard(2);

            ElementVariantRecognized(
               Symbol: _a.PeekRef(0)._Symbol
               );

            _a.Remove();
            goto Branch2;
         }
         case 6:
         case 7:
         case 8:
         case 9:
            goto Reduce49;
            /*case 2: case 3: case 4: case 11:
            default: break; */
         }
         // Reduce13:
         /* aAdjust: -1
          * Element= RepeatedElement(Symbol Symbol);◄ */

         ElementVariantRecognized(
            Symbol: _a.PeekRef(0)._Symbol
            );

         _a.Remove();
         goto State16;

      State11:
         const String StateDescription11 =
              "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"*\"●;\r\n"
            + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"*\", ►\"*\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Asterisk)
         {
            Lexer.AcceptSymbol();
            // Reduce19:
            /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "*", "*";◄ */

            Repeat0rrRecognized(
               Symbol: ref _a.PeekRef(0)._Symbol
               );

            goto Branch3;
         }
         if (ParserInput <= LexerResult.Plus
            || ParserInput == LexerResult.NumberSign)
         {
            if (ErrorHandler(11, StateDescription11, ParserInput))
               goto State11;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Comma
            || ParserInput >= LexerResult.GroupStart);
         // Reduce18:
         /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "*";◄ */

         Repeat0lrRecognized(
            Symbol: ref _a.PeekRef(0)._Symbol
            );

         goto Branch3;

      AcceptState14:
         Lexer.AcceptSymbol();
      State14:
         const String StateDescription14 =
              "outerDefinitions= EndOfDefinition, \"|\", ►outerDefinitionList;";
         _s.Push(3);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.GroupStart)
            goto AcceptState61;
         if (ParserInput == LexerResult.OptionStart)
            goto AcceptState59;
         if (ParserInput == LexerResult.RepeatStart)
            goto AcceptState46;
         if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(14, StateDescription14, ParserInput))
            {
               _s.Pop();
               goto State14;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
         goto AcceptState69;

      AcceptState18:
         Lexer.AcceptSymbol();
      State18:
         const String StateDescription18 =
              "outerDefinitionList= SequenceOfElements, EndOfDefinition, \"|\", ►outerDefinitionList;";
         _s.Push(4);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.GroupStart)
            goto AcceptState61;
         if (ParserInput == LexerResult.OptionStart)
            goto AcceptState59;
         if (ParserInput == LexerResult.RepeatStart)
            goto AcceptState46;
         if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(18, StateDescription18, ParserInput))
            {
               _s.Pop();
               goto State18;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
         goto AcceptState69;

      AcceptState20:
         Lexer.AcceptSymbol();
      State20:
         const String StateDescription20 =
              "SequenceOfElements= SequenceOfElements, \",\"?, ►Element;";
         _s.Push(5);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.GroupStart)
            goto AcceptState61;
         if (ParserInput == LexerResult.OptionStart)
            goto AcceptState59;
         if (ParserInput == LexerResult.RepeatStart)
            goto AcceptState46;
         if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(20, StateDescription20, ParserInput))
            {
               _s.Pop();
               goto State20;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
         goto AcceptState69;

      AcceptState21:
         Lexer.AcceptSymbol();
      State21:
         const String StateDescription21 =
              "SemanticAction(VoidMethodClass method)= CSharpStart, ►CSvoidMethod(VoidMethodClass method), CSharpEnd;\r\n"
            + "SemanticAction(VoidMethodClass method)= CSharpStart, ►CSharpEnd;";
         // *Push(0)
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            // PushState3:
            _s.Push(0);
            goto State32;
         }
         if (ParserInput != LexerResult.CSharpEnd)
         {
            if (ErrorHandler(21, StateDescription21, ParserInput))
               goto State21;
            // PushState4:
            _s.Push(0);
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpEnd);
         Lexer.AcceptSymbol();
         // Reduce29:
         /* aAdjust: 1
          * SemanticAction(VoidMethodClass method)= CSharpStart, CSharpEnd;◄ */
         _a.Allocate();

         EmptySemanticAction(
            method: out _a.PeekRef(0)._VoidMethodClass
            );

         goto Branch8;

      State22:
         const String StateDescription22 =
              "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), ►\"(\", eFormalParameters, \")\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.GroupStart)
         {
            if (ErrorHandler(22, StateDescription22, ParserInput))
               goto State22;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupStart);
         Lexer.AcceptSymbol();
      State23:
         const String StateDescription23 =
              "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), \"(\", ►eFormalParameters, \")\";";
         _s.Push(0);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
            goto AcceptState27;
         if (ParserInput != LexerResult.GroupEnd)
         {
            if (ErrorHandler(23, StateDescription23, ParserInput))
            {
               _s.Pop();
               goto State23;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         // State26:
         /* CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), "(", eFormalParameters, ►")"; */
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         goto AcceptReduce30;

      State24:
         const String StateDescription24 =
              "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), \"(\", eFormalParameters, ►\")\";\r\n"
            + "formalParameters= formalParameters, ►Comma, formalParameter;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.GroupEnd)
            goto AcceptReduce30;
         if (ParserInput != LexerResult.Comma)
         {
            if (ErrorHandler(24, StateDescription24, ParserInput))
               goto State24;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Comma);
      AcceptState25:
         Lexer.AcceptSymbol();
      State25:
         const String StateDescription25 =
              "formalParameters= formalParameters, Comma, ►formalParameter;";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(25, StateDescription25, ParserInput))
            {
               _s.Pop();
               goto State25;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
      AcceptState27:
         Lexer.AcceptSymbol();
      State27:
         const String StateDescription27 =
              "formalParameter= Name(UnifiedString typeString), ►Name(UnifiedString nameString);\r\n"
            + "formalParameter= Name(UnifiedString typeString), ►\"?\", Name(UnifiedString nameString);\r\n"
            + "formalParameter= Name(UnifiedString ParameterModifierOptString), ►Name(UnifiedString typeString), Name(UnifiedString nameString);\r\n"
            + "formalParameter= Name(UnifiedString ParameterModifierOptString), ►Name(UnifiedString typeString), \"?\", Name(UnifiedString nameString);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            goto State28;
         }
         if (ParserInput != LexerResult.Questionmark)
         {
            if (ErrorHandler(27, StateDescription27, ParserInput))
               goto State27;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Questionmark);
         Lexer.AcceptSymbol();
      State30:
         const String StateDescription30 =
              "formalParameter= Name(UnifiedString typeString), \"?\", ►Name(UnifiedString nameString);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(30, StateDescription30, ParserInput))
               goto State30;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
         // Reduce35:
         /* aAdjust: -2
          * formalParameter= Name(UnifiedString typeString), "?", Name(UnifiedString nameString);◄ */

         FormalParameterWithNullableTypeAndName(
            typeString: _a.PeekRef(-1)._UnifiedString,
            nameString: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove(2);
      Branch9:
         switch (_s.Peek())
         {
         case 0:
            goto State24;
         case 1:
         // Reduce31:
         {
            /* sAdjust: -1
             * formalParameters= formalParameters, Comma, formalParameter;◄ */
            _s.Pop();
            goto Branch9;
         }
         /*case 2:
         default: break; */
         }
      State39:
         const String StateDescription39 =
              "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), \"(\", eFormalParameters, ►\")\";\r\n"
            + "formalParameters= formalParameters, ►Comma, formalParameter;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Comma)
            goto AcceptState25;
         if (ParserInput != LexerResult.GroupEnd)
         {
            if (ErrorHandler(39, StateDescription39, ParserInput))
               goto State39;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         goto AcceptReduce40;

      State28:
         const String StateDescription28 =
              "formalParameter= Name(UnifiedString typeString), Name(UnifiedString nameString)●;\r\n"
            + "formalParameter= Name(UnifiedString ParameterModifierOptString), Name(UnifiedString typeString), ►Name(UnifiedString nameString);\r\n"
            + "formalParameter= Name(UnifiedString ParameterModifierOptString), Name(UnifiedString typeString), ►\"?\", Name(UnifiedString nameString);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            // Reduce33:
            /* aAdjust: -3
             * formalParameter= Name(UnifiedString ParameterModifierOptString), Name(UnifiedString typeString), Name(UnifiedString nameString);◄ */

            FormalParameterWithModifierTypeAndName(
               ParameterModifierOptString: _a.PeekRef(-2)._UnifiedString,
               typeString: _a.PeekRef(-1)._UnifiedString,
               nameString: _a.PeekRef(0)._UnifiedString
               );

            _a.Remove(3);
            goto Branch9;
         }
         if (ParserInput == LexerResult.Questionmark)
         {
            Lexer.AcceptSymbol();
            goto State29;
         }
         if (ParserInput != LexerResult.Comma
            && ParserInput != LexerResult.GroupEnd)
         {
            if (ErrorHandler(28, StateDescription28, ParserInput))
               goto State28;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Comma
            || ParserInput == LexerResult.GroupEnd);
         // Reduce32:
         /* aAdjust: -2
          * formalParameter= Name(UnifiedString typeString), Name(UnifiedString nameString);◄ */

         FormalParameterWithTypeAndName(
            typeString: _a.PeekRef(-1)._UnifiedString,
            nameString: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove(2);
         goto Branch9;

      State29:
         const String StateDescription29 =
              "formalParameter= Name(UnifiedString ParameterModifierOptString), Name(UnifiedString typeString), \"?\", ►Name(UnifiedString nameString);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(29, StateDescription29, ParserInput))
               goto State29;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
         // Reduce34:
         /* aAdjust: -3
          * formalParameter= Name(UnifiedString ParameterModifierOptString), Name(UnifiedString typeString), "?", Name(UnifiedString nameString);◄ */

         FormalParameterWithModifierNullableTypeAndName(
            ParameterModifierOptString: _a.PeekRef(-2)._UnifiedString,
            typeString: _a.PeekRef(-1)._UnifiedString,
            nameString: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove(3);
         goto Branch9;

      State32:
         const String StateDescription32 =
              "CSMethodProperties(MethodClass method)= Name(UnifiedString methodTypeString), ►Name(UnifiedString methodNameString);\r\n"
            + "CSMethodProperties(MethodClass method)= Name(UnifiedString modifierString), ►Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);\r\n"
            + "CSMethodProperties(MethodClass method)= Name(UnifiedString modifier1String), ►Name(UnifiedString modifier2String), Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(32, StateDescription32, ParserInput))
               goto State32;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
      State33:
         const String StateDescription33 =
              "CSMethodProperties(MethodClass method)= Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString)●;\r\n"
            + "CSMethodProperties(MethodClass method)= Name(UnifiedString modifierString), Name(UnifiedString methodTypeString), ►Name(UnifiedString methodNameString);\r\n"
            + "CSMethodProperties(MethodClass method)= Name(UnifiedString modifier1String), Name(UnifiedString modifier2String), ►Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            goto State34;
         }
         if (ParserInput != LexerResult.GroupStart)
         {
            if (ErrorHandler(33, StateDescription33, ParserInput))
               goto State33;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupStart);
         // Reduce37:
         /* aAdjust: -1
          * CSMethodProperties(MethodClass method)= Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);◄ */

         MethodTypeAndNameRecognized(
            method: out _a.PeekRef(-1)._MethodClass,
            methodTypeString: _a.PeekClear(-1)._UnifiedString,
            methodNameString: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove();
      Branch10:
         if (_s.Peek() == 0)
            goto State22;
         State37:
         const String StateDescription37 =
              "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), ►\"(\", eFormalParameters, \")\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.GroupStart)
         {
            if (ErrorHandler(37, StateDescription37, ParserInput))
               goto State37;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupStart);
         Lexer.AcceptSymbol();
      State38:
         const String StateDescription38 =
              "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), \"(\", ►eFormalParameters, \")\";";
         _s.Push(2);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
            goto AcceptState27;
         if (ParserInput != LexerResult.GroupEnd)
         {
            if (ErrorHandler(38, StateDescription38, ParserInput))
            {
               _s.Pop();
               goto State38;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         // State40:
         /* CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), "(", eFormalParameters, ►")"; */
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         goto AcceptReduce40;

      State34:
         const String StateDescription34 =
              "CSMethodProperties(MethodClass method)= Name(UnifiedString modifierString), Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString)●;\r\n"
            + "CSMethodProperties(MethodClass method)= Name(UnifiedString modifier1String), Name(UnifiedString modifier2String), Name(UnifiedString methodTypeString), ►Name(UnifiedString methodNameString);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            // Reduce39:
            /* aAdjust: -3
             * CSMethodProperties(MethodClass method)= Name(UnifiedString modifier1String), Name(UnifiedString modifier2String), Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);◄ */

            MethodModifierTypeAndNameRecognized2(
               method: out _a.PeekRef(-3)._MethodClass,
               modifier1String: _a.PeekClear(-3)._UnifiedString,
               modifier2String: _a.PeekRef(-2)._UnifiedString,
               methodTypeString: _a.PeekRef(-1)._UnifiedString,
               methodNameString: _a.PeekRef(0)._UnifiedString
               );

            _a.Remove(3);
            goto Branch10;
         }
         if (ParserInput != LexerResult.GroupStart)
         {
            if (ErrorHandler(34, StateDescription34, ParserInput))
               goto State34;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupStart);
         // Reduce38:
         /* aAdjust: -2
          * CSMethodProperties(MethodClass method)= Name(UnifiedString modifierString), Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);◄ */

         MethodModifierTypeAndNameRecognized(
            method: out _a.PeekRef(-2)._MethodClass,
            modifierString: _a.PeekClear(-2)._UnifiedString,
            methodTypeString: _a.PeekRef(-1)._UnifiedString,
            methodNameString: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove(2);
         goto Branch10;

      AcceptState35:
         Lexer.AcceptSymbol();
      State35:
         const String StateDescription35 =
              "PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= \"??\", ►signedNumber(Int64 constPriority), \"??\";\r\n"
            + "PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= \"??\", ►CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, (Local1)?;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Minus)
         {
            Lexer.AcceptSymbol();
            goto State45;
         }
         if (ParserInput == LexerResult.Plus)
         {
            Lexer.AcceptSymbol();
            goto State43;
         }
         if (ParserInput == LexerResult.Number)
            goto AcceptState44;
         if (ParserInput != LexerResult.CSharpStart)
         {
            if (ErrorHandler(35, StateDescription35, ParserInput))
               goto State35;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.CSharpStart);
         Lexer.AcceptSymbol();
      State36:
         const String StateDescription36 =
              "PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, ►CSintMethod(IntMethodClass intMethod), CSharpEnd, (Local1)?;";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(36, StateDescription36, ParserInput))
            {
               _s.Pop();
               goto State36;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
         goto State32;

      State43:
         const String StateDescription43 =
              "signedNumber(Int64 value)= \"+\", ►Number(Int64 value);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Number)
         {
            if (ErrorHandler(43, StateDescription43, ParserInput))
               goto State43;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Number);
      AcceptState44:
         Lexer.AcceptSymbol();
      State44:
         const String StateDescription44 =
              "PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= \"??\", signedNumber(Int64 constPriority), ►\"??\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.DoubleQuestionmark)
         {
            if (ErrorHandler(44, StateDescription44, ParserInput))
               goto State44;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.DoubleQuestionmark);
         Lexer.AcceptSymbol();
         // Reduce42:
         /* aAdjust: 1
          * PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= "??", signedNumber(Int64 constPriority), "??";◄ */
         _a.Allocate();

         ConstantPriorityGiven(
            dynamicPriority: out _a.PeekRef(0)._IntMethodClass
            );

         goto State12;

      State45:
         const String StateDescription45 =
              "signedNumber(Int64 value)= \"-\", ►Number(Int64 value);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Number)
         {
            if (ErrorHandler(45, StateDescription45, ParserInput))
               goto State45;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Number);
         Lexer.AcceptSymbol();
         // Reduce43:
         /* signedNumber(Int64 value)= "-", Number(Int64 value);◄ */

         NegateNumber(
            value: ref _a.PeekRef(0)._Int64
            );

         goto State44;

      AcceptState46:
         Lexer.AcceptSymbol();
         // State46:
         /* RepeatedElement(Symbol Symbol)= "{", ►NestedElement(Symbol Symbol), "}"; */
         _s.Push(0);
         goto Reduce44;

      State48:
         const String StateDescription48 =
              "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions●;\r\n"
            + "NestedDefinitions= EndOfDefinition, ►\"|\", NestedDefinitionList;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState49;
         if (ParserInput <= LexerResult.CSharpStart
            || ParserInput >= LexerResult.Name)
         {
            if (ErrorHandler(48, StateDescription48, ParserInput))
               goto State48;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
         goto Reduce47;

      AcceptState49:
         Lexer.AcceptSymbol();
      State49:
         const String StateDescription49 =
              "NestedDefinitions= EndOfDefinition, \"|\", ►NestedDefinitionList;";
         _s.Push(7);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.RepeatStart)
            goto AcceptState46;
         if (ParserInput == LexerResult.GroupStart)
            goto AcceptState61;
         if (ParserInput == LexerResult.OptionStart)
            goto AcceptState59;
         if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(49, StateDescription49, ParserInput))
            {
               _s.Pop();
               goto State49;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
         goto AcceptState69;

      State50:
         const String StateDescription50 =
              "NestedDefinitions= EndOfDefinition, \"|\", NestedDefinitionList●;\r\n"
            + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState51;
         if (ParserInput <= LexerResult.CSharpStart
            || ParserInput >= LexerResult.Name)
         {
            if (ErrorHandler(50, StateDescription50, ParserInput))
               goto State50;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
         // Reduce50:
         /* sAdjust: -1
          * NestedDefinitions= EndOfDefinition, "|", NestedDefinitionList;◄ */
         _s.Pop();
         // Branch12:
         if (_s.Peek() == 6)
            goto Reduce47;
         goto Reduce55;

      AcceptState51:
         Lexer.AcceptSymbol();
      State51:
         const String StateDescription51 =
              "NestedDefinitionList= NestedDefinitionList, \"|\", ►Definition;";
         _s.Push(8);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.RepeatStart)
            goto AcceptState46;
         if (ParserInput == LexerResult.GroupStart)
            goto AcceptState61;
         if (ParserInput == LexerResult.OptionStart)
            goto AcceptState59;
         if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(51, StateDescription51, ParserInput))
            {
               _s.Pop();
               goto State51;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
         goto AcceptState69;

      State53:
         const String StateDescription53 =
              "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions●;\r\n"
            + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState51;
         if (ParserInput <= LexerResult.CSharpStart
            || ParserInput >= LexerResult.Name)
         {
            if (ErrorHandler(53, StateDescription53, ParserInput))
               goto State53;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
         goto Reduce47;

      State54:
         const String StateDescription54 =
              "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►NestedDefinitions;";
         _s.Push(9);
         ParserInput = Lexer.PeekSymbol();
         switch (ParserInput)
         {
         // <= LexerResult.NumberSign
         // >= LexerResult.TerminatorSymbol: goto HandleError54 // see end of switch
         case LexerResult.GroupStart:
            goto AcceptState61;
         case LexerResult.OptionStart:
            goto AcceptState59;
         case LexerResult.RepeatStart:
            goto AcceptState46;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState35;
         case LexerResult.CSharpStart:
            goto AcceptState21;
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
         case LexerResult.DefinitionSeparatorSymbol:
         // Reduce54:
         {
            /* EndOfDefinitionWithoutSemantics= ;◄ */

            EndOfDefinitionWithoutSemanticsRecognized();

            goto State55;
         }
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto AcceptState69;
         } // end of switch
         Debug.Assert(ParserInput <= LexerResult.NumberSign || ParserInput >= LexerResult.TerminatorSymbol);

         if (ErrorHandler(54, StateDescription54, ParserInput))
         {
            _s.Pop();
            goto State54;
         };
         goto EndWithError;

      State55:
         const String StateDescription55 =
              "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions●;\r\n"
            + "NestedDefinitions= EndOfDefinition, ►\"|\", NestedDefinitionList;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
            goto AcceptState49;
         if (ParserInput <= LexerResult.CSharpStart
            || ParserInput >= LexerResult.Name)
         {
            if (ErrorHandler(55, StateDescription55, ParserInput))
               goto State55;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
         goto Reduce55;

      State57:
         const String StateDescription57 =
              "NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= \"Name(Attributes)\"(UnifiedString name, Int32 NumberOfAttributes), ►\"=\";\r\n"
            + "SimpleElement(Symbol Symbol)= \"Name(Attributes)\"(UnifiedString name, Int32 NumberOfAttributes)●;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput <= LexerResult.DefiningSymbol)
         {
            Lexer.AcceptSymbol();
            // Reduce57:
            /* NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes), "=";◄ */

            LeftSideOfNestedProduction(
               SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
               name: _a.PeekClear(-1)._UnifiedString,
               NumberOfAttributes: _a.PeekRef(0)._Int32
               );

            goto State54;
         }
         if (ParserInput >= LexerResult.TerminatorSymbol || _is(_fColon | _fPercent | _fCSharpEnd | _fError | _fMinus | _fNumber | _fStarEqual | _fMinusEqual
                | _fNumberSign))
         {
            if (ErrorHandler(57, StateDescription57, ParserInput))
               goto State57;
            goto EndWithError;
         }
         Debug.Assert(ParserInput > LexerResult.DefiningSymbol && ParserInput < LexerResult.TerminatorSymbol && !_is(_fColon | _fPercent | _fCSharpEnd | _fError
                | _fMinus | _fNumber | _fStarEqual | _fMinusEqual | _fNumberSign));
         goto Reduce10;

      State58:
         const String StateDescription58 =
              "RepeatedElement(Symbol Symbol)= \"{\", NestedElement(Symbol Symbol), ►\"}\";";
         if (ParserInput != LexerResult.RepeatEnd)
         {
            if (ErrorHandler(58, StateDescription58, ParserInput))
               goto State58;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.RepeatEnd);
         Lexer.AcceptSymbol();
         // Reduce58:
         /* sAdjust: -1
          * RepeatedElement(Symbol Symbol)= "{", NestedElement(Symbol Symbol), "}";◄ */
         _s.Pop();

         RepeatGroupRecognized(
            Symbol: ref _a.PeekRef(0)._Symbol
            );

         goto Branch3;

      AcceptState59:
         Lexer.AcceptSymbol();
         // State59:
         /* RepeatedElement(Symbol Symbol)= "[", ►NestedElement(Symbol Symbol), "]"; */
         _s.Push(1);
         goto Reduce44;

      State60:
         const String StateDescription60 =
              "RepeatedElement(Symbol Symbol)= \"[\", NestedElement(Symbol Symbol), ►\"]\";";
         if (ParserInput < LexerResult.OptionEnd)
         {
            if (ErrorHandler(60, StateDescription60, ParserInput))
               goto State60;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.OptionEnd);
         Lexer.AcceptSymbol();
         // Reduce59:
         /* sAdjust: -1
          * RepeatedElement(Symbol Symbol)= "[", NestedElement(Symbol Symbol), "]";◄ */
         _s.Pop();

         OptionGroupRecognized(
            Symbol: ref _a.PeekRef(0)._Symbol
            );

         goto Branch3;

      AcceptState61:
         Lexer.AcceptSymbol();
         // State61:
         /* SimpleElement(Symbol Symbol)= "(", ►NestedElement(Symbol Symbol), ")"; */
         _s.Push(2);
         goto Reduce44;

      State64:
         const String StateDescription64 =
              "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►\"=\", outerDefinitions;\r\n"
            + "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►\"-=\", ListOfExcludedTerminalSymbols, \";\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.MinusEqual)
         {
            Lexer.AcceptSymbol();
            goto State65;
         }
         if (ParserInput > LexerResult.DefiningSymbol)
         {
            if (ErrorHandler(64, StateDescription64, ParserInput))
               goto State64;
            goto EndWithError;
         }
         Debug.Assert(ParserInput <= LexerResult.DefiningSymbol);
         Lexer.AcceptSymbol();
      State68:
         const String StateDescription68 =
              "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"=\", ►outerDefinitions;";
         _s.Push(11);
         ParserInput = Lexer.PeekSymbol();
         switch (ParserInput)
         {
         // <= LexerResult.NumberSign: goto HandleError68 // see end of switch
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
            goto HandleError68;
         case LexerResult.GroupStart:
            goto AcceptState61;
         case LexerResult.OptionStart:
            goto AcceptState59;
         case LexerResult.RepeatStart:
            goto AcceptState46;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState35;
         case LexerResult.CSharpStart:
            goto AcceptState21;
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto AcceptState69;
            // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce9 // see end of switch
         } // end of switch
         if (ParserInput >= LexerResult.DefinitionSeparatorSymbol)
            goto Reduce9;
         Debug.Assert(ParserInput <= LexerResult.NumberSign);

      HandleError68:
         if (ErrorHandler(68, StateDescription68, ParserInput))
         {
            _s.Pop();
            goto State68;
         };
         goto EndWithError;

      State65:
         const String StateDescription65 =
              "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"-=\", ►ListOfExcludedTerminalSymbols, \";\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(65, StateDescription65, ParserInput))
               goto State65;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
         // Reduce64:
         /* aAdjust: -1
          * ListOfExcludedTerminalSymbols= Name(UnifiedString terminalName);◄ */

         FirstExcludedTerminalSymbol(
            terminalName: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove();
      State66:
         const String StateDescription66 =
              "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"-=\", ListOfExcludedTerminalSymbols, ►\";\";\r\n"
            + "ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, ►\"|\", Name(UnifiedString name);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
         {
            Lexer.AcceptSymbol();
            goto State67;
         }
         if (ParserInput < LexerResult.TerminatorSymbol)
         {
            if (ErrorHandler(66, StateDescription66, ParserInput))
               goto State66;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
         Lexer.AcceptSymbol();
         // Reduce65:
         /* GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "-=", ListOfExcludedTerminalSymbols, ";";◄ */

         EndOfListOfExcludedTerminalSymbols();

         goto Reduce63;

      State67:
         const String StateDescription67 =
              "ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, \"|\", ►Name(UnifiedString name);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(67, StateDescription67, ParserInput))
               goto State67;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
         // Reduce66:
         /* aAdjust: -1
          * ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, "|", Name(UnifiedString name);◄ */

         OneMoreExcludedTerminalSymbol(
            name: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove();
         goto State66;

      State70:
         const String StateDescription70 =
              "\"(Attributes)\"(Int32 numberOfAttributes)= \"(\", ►\")\";\r\n"
            + "\"(Attributes)\"(Int32 numberOfAttributes)= \"(\", ►\"Attributes)\"(Int32 numberOfAttributes, Int32 smallestNumber);";
         // *Push(0)
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            // PushState7:
            _s.Push(0);
            goto State73;
         }
         if (ParserInput != LexerResult.GroupEnd)
         {
            if (ErrorHandler(70, StateDescription70, ParserInput))
               goto State70;
            // PushState8:
            _s.Push(0);
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         Lexer.AcceptSymbol();
         // Reduce70:
         /* aAdjust: 1
          * "(Attributes)"(Int32 numberOfAttributes)= "(", ")";◄ */
         _a.Allocate();

         EmptyListOfAttributes(
            numberOfAttributes: out _a.PeekRef(0)._Int32
            );

      Branch15:
         if (_s.Peek() == 0)
         // Reduce69:
         {
            /* sAdjust: -1
             * "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes)= ExtendedName(UnifiedString name), "(Attributes)"(Int32 NumberOfAttributes);◄ */
            _s.Pop();
            goto Branch14;
         }
         // Reduce76:
         /* sAdjust: -1
          * "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes)= ExtendedName(UnifiedString name), "(Attributes)"(Int32 NumberOfAttributes);◄ */
         _s.Pop();
         goto State3;

      State71:
         const String StateDescription71 =
              "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 numberBeforeGroup), ►Comma, \"Attributes)\"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup);\r\n"
            + "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 number), ►\")\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Comma)
         {
            Lexer.AcceptSymbol();
            goto State72;
         }
         if (ParserInput != LexerResult.GroupEnd)
         {
            if (ErrorHandler(71, StateDescription71, ParserInput))
               goto State71;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         Lexer.AcceptSymbol();
         // Reduce72:
         /* aAdjust: 1
          * "Attributes)"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 number), ")";◄ */
         _a.Allocate();

         FirstAttributeOfGroup(
            numberOfAttributesOfGroup: out _a.PeekRef(-1)._Int32,
            smallestNumber: out _a.PeekRef(0)._Int32,
            number: _a.PeekRef(-1)._Int32
            );

      Branch16:
         if (_s.Peek() == 1)
         // Reduce73:
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

            _a.Remove();
            goto Branch16;
         }
         // Reduce71:
         /* sAdjust: -1, aAdjust: -1
          * "(Attributes)"(Int32 numberOfAttributes)= "(", "Attributes)"(Int32 numberOfAttributes, Int32 smallestNumber);◄ */
         _s.Pop();
         _a.Remove();
         goto Branch15;

      State72:
         const String StateDescription72 =
              "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 numberBeforeGroup), Comma, ►\"Attributes)\"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup);";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(72, StateDescription72, ParserInput))
            {
               _s.Pop();
               goto State72;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
      State73:
         const String StateDescription73 =
              "Attribut(Int32 number)= Name(UnifiedString typeString), ►Name(UnifiedString nameString);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(73, StateDescription73, ParserInput))
               goto State73;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
         // Reduce74:
         /* aAdjust: -1
          * Attribut(Int32 number)= Name(UnifiedString typeString), Name(UnifiedString nameString);◄ */

         AttributeTypeAndName(
            number: out _a.PeekRef(-1)._Int32,
            typeString: _a.PeekClear(-1)._UnifiedString,
            nameString: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove();
         goto State71;

      State74:
         const String StateDescription74 =
              "GrammlatorSetting= Name(UnifiedString name), ►\":\", LexerString(UnifiedString value), \";\";\r\n"
            + "GrammlatorSetting= Name(UnifiedString name), ►\":\", Number(Int64 value), \";\";\r\n"
            + "GrammlatorSetting= Name(UnifiedString name), ►\":\", Name(UnifiedString value), \";\";\r\n"
            + "\"Name(Attributes)\"(UnifiedString name, Int32 NumberOfAttributes)= ExtendedName(UnifiedString name), ►\"(Attributes)\"(Int32 NumberOfAttributes);\r\n"
            + "\"Name(Attributes)\"(UnifiedString name, Int32 NumberOfAttributes)= ExtendedName(UnifiedString name)●;";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         switch (ParserInput)
         {
         // <= LexerResult.DefiningSymbol: goto HandleError74 // see end of switch
         case LexerResult.CSharpEnd:
         case LexerResult.Error:
         case LexerResult.Minus:
         case LexerResult.Number:
         case LexerResult.MinusEqual:
         case LexerResult.Questionmark:
         case LexerResult.Asterisk:
         case LexerResult.Plus:
         case LexerResult.Comma:
         case LexerResult.NumberSign:
         case LexerResult.OptionStart:
         case LexerResult.RepeatStart:
         case LexerResult.DoubleQuestionmark:
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto HandleError74;
         case LexerResult.Colon:
         {
            Lexer.AcceptSymbol();
            goto State75;
         }
         // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce75 // see end of switch
         case LexerResult.Percent:
         case LexerResult.StarEqual:
         case LexerResult.CSharpStart:
            goto Reduce75;
         case LexerResult.GroupStart:
         {
            Lexer.AcceptSymbol();
            goto State70;
         }
         } // end of switch
         if (ParserInput <= LexerResult.DefiningSymbol)
            goto HandleError74;
         Debug.Assert(ParserInput >= LexerResult.DefinitionSeparatorSymbol);

      Reduce75:
         /* sAdjust: -1, aAdjust: 1
          * "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes)= ExtendedName(UnifiedString name);◄ */
         _s.Pop();
         _a.Allocate();

         NameWithoutAttributes(
            NumberOfAttributes: out _a.PeekRef(0)._Int32
            );

         goto State3;

      State75:
         const String StateDescription75 =
              "GrammlatorSetting= Name(UnifiedString name), \":\", ►LexerString(UnifiedString value), \";\";\r\n"
            + "GrammlatorSetting= Name(UnifiedString name), \":\", ►Number(Int64 value), \";\";\r\n"
            + "GrammlatorSetting= Name(UnifiedString name), \":\", ►Name(UnifiedString value), \";\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            goto State77;
         }
         if (ParserInput == LexerResult.LexerString)
         {
            Lexer.AcceptSymbol();
            goto State76;
         }
         if (ParserInput != LexerResult.Number)
         {
            if (ErrorHandler(75, StateDescription75, ParserInput))
               goto State75;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Number);
         Lexer.AcceptSymbol();
      State78:
         const String StateDescription78 =
              "GrammlatorSetting= Name(UnifiedString name), \":\", Number(Int64 value), ►\";\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput < LexerResult.TerminatorSymbol)
         {
            if (ErrorHandler(78, StateDescription78, ParserInput))
               goto State78;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
         Lexer.AcceptSymbol();
         // Reduce79:
         /* sAdjust: -2, aAdjust: -2
          * GrammlatorSetting= Name(UnifiedString name), ":", Number(Int64 value), ";";◄
          * then: OptionalGrammlatorSettings= OptionalGrammlatorSettings, GrammlatorSetting;◄ */
         _s.Discard(2);

         SetGrammlatorInt32Setting(
            name: _a.PeekRef(-1)._UnifiedString,
            value: _a.PeekRef(0)._Int64
            );

         _a.Remove(2);
         goto State2;

      State76:
         const String StateDescription76 =
              "GrammlatorSetting= Name(UnifiedString name), \":\", LexerString(UnifiedString value), ►\";\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput < LexerResult.TerminatorSymbol)
         {
            if (ErrorHandler(76, StateDescription76, ParserInput))
               goto State76;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
         Lexer.AcceptSymbol();
         // Reduce77:
         /* sAdjust: -2, aAdjust: -2
          * GrammlatorSetting= Name(UnifiedString name), ":", LexerString(UnifiedString value), ";";◄
          * then: OptionalGrammlatorSettings= OptionalGrammlatorSettings, GrammlatorSetting;◄ */
         _s.Discard(2);

         SetGrammlatorStringSetting(
            name: _a.PeekRef(-1)._UnifiedString,
            value: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove(2);
         goto State2;

      State77:
         const String StateDescription77 =
              "GrammlatorSetting= Name(UnifiedString name), \":\", Name(UnifiedString value), ►\";\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput < LexerResult.TerminatorSymbol)
         {
            if (ErrorHandler(77, StateDescription77, ParserInput))
               goto State77;
            goto EndWithError;
         }
         Debug.Assert(ParserInput >= LexerResult.TerminatorSymbol);
         Lexer.AcceptSymbol();
         // Reduce78:
         /* sAdjust: -2, aAdjust: -2
          * GrammlatorSetting= Name(UnifiedString name), ":", Name(UnifiedString value), ";";◄
          * then: OptionalGrammlatorSettings= OptionalGrammlatorSettings, GrammlatorSetting;◄ */
         _s.Discard(2);

         SetGrammlatorNameSetting(
            name: _a.PeekRef(-1)._UnifiedString,
            value: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove(2);
         goto State2;

      State80:
         const String StateDescription80 =
              "CSEnumDeclaration= CSEnumProperties, ►optionalBaseType, CSEnumMembers;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Colon)
         {
            Lexer.AcceptSymbol();
            goto State94;
         }
         if (ParserInput != LexerResult.RepeatStart)
         {
            if (ErrorHandler(80, StateDescription80, ParserInput))
               goto State80;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.RepeatStart);
      State81:
         const String StateDescription81 =
              "CSEnumDeclaration= CSEnumProperties, optionalBaseType, ►CSEnumMembers;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.RepeatStart)
         {
            if (ErrorHandler(81, StateDescription81, ParserInput))
               goto State81;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.RepeatStart);
         Lexer.AcceptSymbol();
      State82:
         const String StateDescription82 =
              "CSEnumMembers= \"{\", ►\"}\";\r\n"
            + "CSEnumMembers= \"{\", ►CSEnumMemberList, \"}\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.RepeatEnd)
         {
            Lexer.AcceptSymbol();
            // Reduce82:
            /* CSEnumMembers= "{", "}";◄ */

            EmptyEnumRecognized();

            goto Reduce80;
         }
         if (ParserInput != LexerResult.OptionStart
            && ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(82, StateDescription82, ParserInput))
               goto State82;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.OptionStart
            || ParserInput == LexerResult.Name);
         // Reduce81:
         /* ResetEnumDefaults= ;◄ */

         ResetEnumDefaults();

         // State83:
         /* CSEnumMemberList= ResetEnumDefaults, ►CSEnumMember; */
         _s.Push(0);
         if (ParserInput >= LexerResult.Name)
            goto Reduce83;
         Debug.Assert(ParserInput == LexerResult.OptionStart);
      AcceptState87:
         Lexer.AcceptSymbol();
      State87:
         const String StateDescription87 =
              "OptionalDescriptionAttribute(String description)= \"[\", ►Name(UnifiedString attributeIdentifier), \"(\", LexerString(UnifiedString descriptionString), \")\", \"]\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(87, StateDescription87, ParserInput))
               goto State87;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
      State88:
         const String StateDescription88 =
              "OptionalDescriptionAttribute(String description)= \"[\", Name(UnifiedString attributeIdentifier), ►\"(\", LexerString(UnifiedString descriptionString), \")\", \"]\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.GroupStart)
         {
            if (ErrorHandler(88, StateDescription88, ParserInput))
               goto State88;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupStart);
         Lexer.AcceptSymbol();
      State89:
         const String StateDescription89 =
              "OptionalDescriptionAttribute(String description)= \"[\", Name(UnifiedString attributeIdentifier), \"(\", ►LexerString(UnifiedString descriptionString), \")\", \"]\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.LexerString)
         {
            if (ErrorHandler(89, StateDescription89, ParserInput))
               goto State89;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.LexerString);
         Lexer.AcceptSymbol();
      State90:
         const String StateDescription90 =
              "OptionalDescriptionAttribute(String description)= \"[\", Name(UnifiedString attributeIdentifier), \"(\", LexerString(UnifiedString descriptionString), ►\")\", \"]\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.GroupEnd)
         {
            if (ErrorHandler(90, StateDescription90, ParserInput))
               goto State90;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.GroupEnd);
         Lexer.AcceptSymbol();
      State91:
         const String StateDescription91 =
              "OptionalDescriptionAttribute(String description)= \"[\", Name(UnifiedString attributeIdentifier), \"(\", LexerString(UnifiedString descriptionString), \")\", ►\"]\";";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.OptionEnd)
         {
            if (ErrorHandler(91, StateDescription91, ParserInput))
               goto State91;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.OptionEnd);
         Lexer.AcceptSymbol();
         // Reduce87:
         /* aAdjust: -1
          * OptionalDescriptionAttribute(String description)= "[", Name(UnifiedString attributeIdentifier), "(", LexerString(UnifiedString descriptionString), ")", "]";◄ */

         DescriptionAttribute(
            description: out _a.PeekRef(-1)._String,
            attributeIdentifier: _a.PeekClear(-1)._UnifiedString,
            descriptionString: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove();
         goto State84;

      State86:
         const String StateDescription86 =
              "OptionalEnumElementNumber(Int64 enumNumber)= \"=\", ►Number(Int64 enumNumber);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Number)
         {
            if (ErrorHandler(86, StateDescription86, ParserInput))
               goto State86;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Number);
         Lexer.AcceptSymbol();
         goto Reduce86;

      State93:
         const String StateDescription93 =
              "CSEnumMemberList= CSEnumMemberList, Comma, ►CSEnumMember;";
         _s.Push(1);
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
            goto Reduce83;
         if (ParserInput != LexerResult.OptionStart)
         {
            if (ErrorHandler(93, StateDescription93, ParserInput))
            {
               _s.Pop();
               goto State93;
            };
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.OptionStart);
         goto AcceptState87;

      State94:
         const String StateDescription94 =
              "optionalBaseType= \":\", ►Name(UnifiedString Ignored);";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(94, StateDescription94, ParserInput))
               goto State94;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
         // Reduce88:
         /* aAdjust: -1
          * optionalBaseType= ":", Name(UnifiedString Ignored);◄ */
         _a.Remove();
         goto State81;

      State96:
         const String StateDescription96 =
              "CSEnumProperties= Name(UnifiedString modifier1StringIndex), ►Name(UnifiedString modifier2StringIndex), Name(UnifiedString enumStringIndex), CSEnumName;\r\n"
            + "CSEnumProperties= Name(UnifiedString modifierStringIndex), ►Name(UnifiedString enumStringIndex), CSEnumName;\r\n"
            + "CSEnumProperties= Name(UnifiedString enumStringIndex), ►CSEnumName;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput != LexerResult.Name)
         {
            if (ErrorHandler(96, StateDescription96, ParserInput))
               goto State96;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Name);
         Lexer.AcceptSymbol();
      State97:
         const String StateDescription97 =
              "CSEnumProperties= Name(UnifiedString modifier1StringIndex), Name(UnifiedString modifier2StringIndex), ►Name(UnifiedString enumStringIndex), CSEnumName;\r\n"
            + "CSEnumProperties= Name(UnifiedString modifierStringIndex), Name(UnifiedString enumStringIndex), ►CSEnumName;\r\n"
            + "CSEnumName= Name(UnifiedString nameString)●;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            goto State98;
         }
         if (ParserInput != LexerResult.Colon
            && ParserInput != LexerResult.RepeatStart)
         {
            if (ErrorHandler(97, StateDescription97, ParserInput))
               goto State97;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Colon
            || ParserInput == LexerResult.RepeatStart);
         // Reduce90:
         /* aAdjust: -2
          * CSEnumName= Name(UnifiedString nameString);◄
          * then: CSEnumProperties= Name(UnifiedString enumStringIndex), CSEnumName;◄ */

         EnumNameRecognized(
            nameString: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove(2);
         goto State80;

      State98:
         const String StateDescription98 =
              "CSEnumProperties= Name(UnifiedString modifier1StringIndex), Name(UnifiedString modifier2StringIndex), Name(UnifiedString enumStringIndex), ►CSEnumName;\r\n"
            + "CSEnumName= Name(UnifiedString nameString)●;";
         ParserInput = Lexer.PeekSymbol();
         if (ParserInput == LexerResult.Name)
         {
            Lexer.AcceptSymbol();
            // Reduce93:
            /* aAdjust: -4
             * CSEnumName= Name(UnifiedString nameString);◄
             * then: CSEnumProperties= Name(UnifiedString modifier1StringIndex), Name(UnifiedString modifier2StringIndex), Name(UnifiedString enumStringIndex), CSEnumName;◄ */

            EnumNameRecognized(
               nameString: _a.PeekRef(0)._UnifiedString
               );

            _a.Remove(4);
            goto State80;
         }
         if (ParserInput != LexerResult.Colon
            && ParserInput != LexerResult.RepeatStart)
         {
            if (ErrorHandler(98, StateDescription98, ParserInput))
               goto State98;
            goto EndWithError;
         }
         Debug.Assert(ParserInput == LexerResult.Colon
            || ParserInput == LexerResult.RepeatStart);
         // Reduce92:
         /* aAdjust: -3
          * CSEnumName= Name(UnifiedString nameString);◄
          * then: CSEnumProperties= Name(UnifiedString modifierStringIndex), Name(UnifiedString enumStringIndex), CSEnumName;◄ */

         EnumNameRecognized(
            nameString: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove(3);
         goto State80;

      Reduce22:
         /* sAdjust: -1
          * outerDefinitions= EndOfDefinition, "|", outerDefinitionList;◄ */
         _s.Pop();
         goto Branch5;

      Reduce26:
         /* sAdjust: -2
          * outerDefinitionList= SequenceOfElements, EndOfDefinition, "|", outerDefinitionList;◄ */
         _s.Discard(2);
         // Branch7:
         switch (_s.Peek())
         {
         case 2:
            goto Reduce11;
         case 3:
            goto Reduce22;
         case 4:
            goto Reduce26;
            /*case 11:
            default: break; */
         }
         goto Reduce67;

      HandleError8:
         if (ErrorHandler(8, StateDescription8, ParserInput))
         {
            _s.Pop();
            goto State8;
         };
         goto EndWithError;

      HandleError16:
         if (ErrorHandler(16, StateDescription16, ParserInput))
         {
            _s.Pop();
            goto State16;
         };
         goto EndWithError;

      HandleError74:
         if (ErrorHandler(74, StateDescription74, ParserInput))
         {
            _s.Pop();
            goto State74;
         };
         goto EndWithError;

      EndWithError:
         // This point is reached after an input error has been found
         _s.Discard(_s.Count - _StateStackInitialCount);
         _a.Remove(_a.Count - _AttributeStackInitialCount);

      EndOfGeneratedCode:
         ;

         #endregion grammlator generated 15 Okt 2020 (grammlator file version/date 2020.10.15.0/15 Okt 2020)

      }
   }
}
