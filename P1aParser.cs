using GrammlatorRuntime;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Collections;
using System.ComponentModel;
using System.Runtime.CompilerServices;

namespace grammlator;

#region grammar grammlator settings and terminal definitions
//| // grammlator settings:
//| TerminalSymbolEnum:               "LexerResult";
//| InputExpression:                  "ParserInput";
//| InputAssignInstruction:           "ParserInput = Lexer.PeekSymbol();";
//| InputAcceptInstruction:           "Lexer.AcceptSymbol();";
//| PrefixOfStateDescriptionConstant: "StateDescription";
//| NameOfErrorHandlerMethod:         "ErrorHandler";
//| GenerateSwitchStartingLevel:       5;
//| GenerateFlagTestStartingLevel:     4;
//| LineLengthLimit:                   150;
//| GenerateSmallStateStackNumbers:    true;
//| NameOfFlagTestMethod:             "_is";
//| NameOfAssertMethod:               "Debug.Assert";    
//| GenerateComments:                  true;    
//|     
//| // Terminal symbols (LexerResult) and their probability to appear in input:

/// <summary>
/// LexerResult defines the output of the lexer, which is assigned to Symbol to be used by the parser.
/// The elements of LexerResult are ordered such that grammlator can
/// generate efficient code for the conditions of the parsers actions 
/// </summary>
public enum LexerResult : Byte
{
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
   MinusEqual,     // "-=", added by the lexer
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
   RepeatStart,

   DoubleQuestionmark,
   [Description(@"CSharpStart() %19")]

   CSharpStart, // represents the change from grammlator lines to CSharp lines

   GroupEnd, RepeatEnd, OptionEnd,  // these are the characters ) ] } #

   [Description("Name(UnifiedString string) %45")]
   Name,
   [Description("LexerString(UnifiedString string) %30")]
   LexerString,

   [Description(@"DefinitionSeparatorSymbol() %45 ""|"" ")]
   DefinitionSeparatorSymbol,
   [Description(@"TerminatorSymbol() %42 "";"" ")]
   TerminatorSymbol
};
#endregion grammar

public static class LexerResultExtensions
{
   /// <summary>
   /// Map a LexerResult to a string 
   /// </summary>
   /// <param name="lr"></param>
   /// <returns></returns>
   public static String LexerResultToString(this LexerResult lr)
   {
      const String MyDisplay
         = "=:%xx-xxx?*+,#([{\u2047x)}]xx|;"; 
      // \u2047 is "??" as one character "double question mark"; 'x' denotes special handling (below)

      Debug.Assert(lr != LexerResult.Error);
      if ((Int32)lr >= MyDisplay.Length)
         return lr.ToString();

      Char result = MyDisplay[(Int32)lr];

      if (result != 'x')
         return result.ToString();
      String s = lr switch
      {
         LexerResult.CSharpEnd  => "End of C# Code",
         LexerResult.Error      => "(Unknown lexer result)",
         // LexerResult.Number  => "Number",
         LexerResult.StarEqual  => "*=",
         LexerResult.MinusEqual => "-=",
         LexerResult.DoubleQuestionmark => "??",
         LexerResult.CSharpStart => "C# code",
         // LexerResult.Name     => "Name",
         LexerResult.LexerString => "String",
         _ => lr.ToString(), // Number, Name 
      };
      return s;
   }
}

/// <summary>
/// Grammlator Parser (uses Lexer which uses InputClassifier)
/// </summary>
internal sealed partial class P1aParser : GrammlatorApplication
{
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
           + @$"{GlobalSettings.RegionBegin.Value} {GlobalSettings.RegionGrammlatorMarker.Value} {GlobalSettings.RegionGeneratedMarker.Value}"
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
   private readonly ListOfDefinitions ActualListOfNontrivialDefinitions = new(100);

   /// <summary>
   /// Number of the trival definitions of the actual (nested) production &lt;= ActualListOfTrivialDefinitions.Count
   /// </summary>
   private Int32 NumberOfTrivalDefinitions = 0;

   /// <summary>
   /// Contains all (perhaps nested) trivial definitions which are not yet assigned to a nonterminal symbol.
   /// A trivial definition contains 1 element, no condition, no action and the same number of attributes as the left side.
   /// </summary>
   private readonly ListOfSymbols ActualListOfTrivialDefinitions = new(100);

   /// <summary>
   /// Number of the elements of the actual definition less or equal ActualListOfElements.Count
   /// </summary>
   private Int32 NumberOfElements = 0;

   /// <summary>
   /// Contains all elements of the actual (perhaps nested) definition.
   /// </summary>
   private readonly ListOfSymbols ActualListOfElements = new(100);
   #endregion declaration of fields

   /// <summary>
   /// Output the given message together with the actual input position, throw exception if Abort
   /// </summary>
   /// <param name="messageType"></param>
   /// <param name="message"></param>
   private void P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum messageType, String message)
       => GlobalVariables.OutputMessageAndPosition(messageType, message, Lexer.LexerTextPos);

   #region grammar of grammlator parser

   //|  *= GrammlatorGrammar /* one startsymbol, which has no attributes */

   //| // renaming some symbols to improve readability
   //|  "," = Comma; // "," is used where provided in EBNF, Comma in other cases
   //|  "=" = DefiningSymbol; "|" = DefinitionSeparatorSymbol; ";" = TerminatorSymbol; ":" = Colon; "%" = Percent;
   //|  "-" = Minus; "+" = Plus; "*" = Asterisk; "(" = GroupStart; "[" = OptionStart; "{" = RepeatStart;
   //|  ")" = GroupEnd; "]" = OptionEnd; "}" = RepeatEnd; "?" = Questionmark;
   //|  "-=" = MinusEqual; "??" = DoubleQuestionmark  /* see below "*=" = StarEqual ... */ 

   //| GrammlatorGrammar
   //| =   OptionalGrammlatorSettings, 
   //|     DeclarationOfTerminalSymbols,
   //|     GrammarRuleList,
   //|     TerminatorAtEndOfGrammar;
   //|
   //| TerminatorAtEndOfGrammar= NumberSign; // the 1st char of "#region grammlator generated"
   //|
   //| OptionalGrammlatorSettings
   //| = /* empty */
   //| | OptionalGrammlatorSettings, GrammlatorSetting

   //| GrammlatorSetting
   //| = Name(UnifiedString name), ":", LexerString(UnifiedString value), ";"
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


   //| | Name(UnifiedString name), ":", Number(Int64 value), ";"
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
   //| | Name(UnifiedString name), ":", Name(UnifiedString value), ";"
   private void SetGrammlatorNameSetting(UnifiedString name, UnifiedString value)
      => SetGrammlatorStringSetting(
         name,
         value.ToString()
         );

   //| DeclarationOfTerminalSymbols
   //| = OptionalDeclarationOfTerminalSymbols
   private void CompareTerminalDeclarationsWithEnum()
   {
      if (EnumName.Index == 0)
         return; // no enum, nothing to add or to compare

      int NewTerminalsDeclaredInEnum = SymbolDictionary.Count - DictCountBeforeEnum;
      if (DictCountBeforeEnum > 0 && NewTerminalsDeclaredInEnum > 0)
         P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Warning,
@$"{DictCountBeforeEnum} terminals are defined in grammlator lines. The enum defines {NewTerminalsDeclaredInEnum} additional elements.");

      // Use the name of the enum if not defined by settings in the source
      if (GlobalSettings.TerminalSymbolEnum.Value == GlobalSettings.TerminalSymbolUndefinedValue)
         GlobalSettings.TerminalSymbolEnum.Value = EnumName.ToString();

      // GlobalSettings.TerminalSymbolEnum.Value my be "" if no C# enum is defined

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

   //|
   //| OptionalDeclarationOfTerminalSymbols
   //| = OptionalSemikolonOrEnum
   //| | TerminalSymbolsList, OptionalSemikolonOrEnum

   //| TerminalSymbolsList
   //| = TerminalSymbol
   //| | TerminalSymbolsList, "|", TerminalSymbol

   //|  TerminalSymbol
   //|  = "Name(Attributes)"
   //|       (UnifiedString name, Int32 numberOfAttributes), OptionalValue(Int64 value), OptionalWeight(Int64 weight)
   private void TerminalSymbol(UnifiedString name, Int32 numberOfAttributes, Int64 value, Int64 weight)
          => TerminalSymbolDeclaration(name, numberOfAttributes, value, weight);

   //| OptionalWeight(Int64 weight)
   //| = /* empty */
   private static void OptionalDefaultWeight(out Int64 weight)
      => weight = GlobalSettings.TerminalDefaultWeight.Value;
   //| | "%", Number(Int64 weight)

   //| OptionalValue(Int64 value)
   //| = /* empty */
   private void OptionalValueDefault(out Int64 value) => value = ++LastTerminalValue;
   Int64 LastTerminalValue = -1;
   //| | "=", Number(Int64 value)
   private void OptionalValueAssignment(Int64 value) => LastTerminalValue = value;

   //| NameOrString (UnifiedString name)
   //| = Name(UnifiedString name)
   //| | LexerString(UnifiedString name);
   //|
   //| GrammarRuleList
   //| = FirstGrammarRule
   //| | GrammarRuleList, GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)
   private void EndOfGrammarRuleRecognized(Symbol SymbolAtLeftSide)
   {
      EvaluateGrammarRule(SymbolAtLeftSide);
      // The following assertion will fail if there are errors in the input 
      //  "... has been used at its first occurence with a different number of attributes
      // Debug.Assert(ListOfAttributesOfGrammarRule.Count == SymbolAtLeftSide.NumberOfAttributes);
      ListOfAttributesOfGrammarRule.Clear();
      AttributeCounter = 0;
   }

   //| FirstGrammarRule
   //| = "*=", outerDefinitions
   private void FirstGrammarRuleRecognized()
   {
      EvaluateDefinitionsOftheStartsymbol();

      // OptimizeTrivialDefinitions has been set false for the first grammar rule: reset to previous value
      OptimizeTrivialDefinitions = OptimizeTrivialDefinitionsBackup;
   }

   //| OptionalSemikolonOrEnum // at end of terminal definitions
   //| = /* empty */
   //| | ";"
   //| | EnumOrEmptyCode

   //| EnumOrEmptyCode
   //| = CSharpStart,  CSEnumDeclaration, CSharpEnd
   //| | CSharpStart, CSharpEnd

   //| "*="
   //| = StarEqual
   private void StartOfFirstGrammarRule()
   {
      /* If no terminal symbol has been defined then define a default terminal symbol.
       * This will be never used. It is necessary that grammlator can distinguish
       * between "no terminal symbol" and "all terminal symbols".
       */
      if (SymbolDictionary.Count == 0)
         TerminalSymbolDeclaration(
            new UnifiedString(GlobalSettings.TerminalSymbolDefaultName.Value),
            numberOfAttributes: 0,
            value: 0,
            weight: 0);
      GlobalVariables.NumberOfTerminalSymbols = SymbolDictionary.Count;
      GlobalVariables.DefineArrayTerminalSymbolByIndex(SymbolDictionary);
      OptimizeTrivialDefinitionsBackup = OptimizeTrivialDefinitions;
      OptimizeTrivialDefinitions = false; // must be disabled while evaluating the definitions of the startsymbol
      SymbolDictionary.Add(GlobalVariables.Startsymbol.Identifier, GlobalVariables.Startsymbol);
   }

   private Boolean OptimizeTrivialDefinitionsBackup;

   //| GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)
   //| = outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "=", outerDefinitions
   //| | outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "-=" ListOfExcludedTerminalSymbols, ";"
   private void EndOfListOfExcludedTerminalSymbols() => EvaluateExcludedTerminalSymbols(ExcludedTerminalSymbols!);

   //| ListOfExcludedTerminalSymbols
   //| = Name(UnifiedString terminalName)
   private void FirstExcludedTerminalSymbol(UnifiedString terminalName)
   {
      if (ExcludedTerminalSymbols == null || ExcludedTerminalSymbols.Length != GlobalVariables.NumberOfTerminalSymbols)
         ExcludedTerminalSymbols = new BitArray(GlobalVariables.NumberOfTerminalSymbols);
      ExcludedTerminalSymbols.SetAll(false);
      OneMoreExcludedTerminalSymbol(terminalName);
   }

   private BitArray? ExcludedTerminalSymbols;

   //| | ListOfExcludedTerminalSymbols, "|", Name(UnifiedString name)
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

   //| NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes) /* Usage see siehe NestedElement */
   //| = NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions
   //| | NestedDefinitions
   private void NestedGrammarRuleWithEmptyLeftside(out Symbol SymbolAtLeftSide, out Int32 NumberOfAttributes)
   {
      SymbolAtLeftSide = NonterminalSymbolDefinition(MakeNewNameStringIndex("Local"), 0);
      NumberOfAttributes = 0;
   }

   //| NestedDefinitions // never ends with ";"
   //| = EndOfDefinition // empty sequence as single definition
   //| | EndOfDefinition, "|", NestedDefinitionList // empty sequence only as first definition
   //| | NestedDefinitionList // no empty sequence in definitions

   //| NestedDefinitionList 
   //| = Definition
   //| | NestedDefinitionList, "|", Definition

   //| Definition
   //| = SequenceOfElements, EndOfDefinition

   //| EndOfDefinition
   //| = EndOfDefinitionWithSemantics 
   //| | EndOfDefinitionWithoutSemantics

   //| outerDefinitions           
   //| = EndOfDefinitionWithoutSemantics, ";"  // empty sequence only in first definition
   //| | EndOfDefinitionWithSemantics, ";"?    // empty sequence
   //| | EndOfDefinition, "|", outerDefinitionList // empty sequence only as first definition
   //| | outerDefinitionList       

   //| outerDefinitionList  // if no semantics then must end with terminator symbol
   //| = SequenceOfElements, EndOfDefinition, "|", outerDefinitionList
   //| | outerLastDefinitionOfSequence

   //| outerLastDefinitionOfSequence
   //| = SequenceOfElements, EndOfDefinitionWithoutSemantics, ";"
   //| | SequenceOfElements, EndOfDefinitionWithSemantics, ";"?  // optional ";" after semantics / codelines

   //| EndOfDefinitionWithoutSemantics
   //| = /* empty */
   private void EndOfDefinitionWithoutSemanticsRecognized()
   {
      EvaluateDefinition(constantPriority: 0, priorityFunction: null, semanticMethod: null, OptimizeTrivialDefinitions);
      AttributeCounter = AttributeNumberAtStartOfDefinition;
   }

   //| EndOfDefinitionWithSemantics
   //| = PriorityDeclaration(Int64 constPriority, IntMethodClass dynPriority)
   private void EndOfDefinitionWithPriorityRecognized(Int64 constPriority, IntMethodClass? dynPriority)
   {
      EvaluateDefinition(constantPriority: constPriority, priorityFunction: dynPriority, semanticMethod: null, OptimizeTrivialDefinitions);
      AttributeCounter = AttributeNumberAtStartOfDefinition;
   }

   //| | SemanticAction(VoidMethodClass method)
   private void EndOfDefinitionWithMethodRecognized(VoidMethodClass? method)
   {
      EvaluateDefinition(constantPriority: 0, priorityFunction: null, semanticMethod: method, OptimizeTrivialDefinitions);
      AttributeCounter = AttributeNumberAtStartOfDefinition;
   }

   //| | PriorityDeclaration(Int64 constPriority, IntMethodClass dynPriority), SemanticAction(VoidMethodClass method)
   private void EndOfDefinitionWithPriorityAndMethodRecognized(Int64 constPriority, IntMethodClass? dynPriority, VoidMethodClass? method)
   {
      EvaluateDefinition(constPriority, dynPriority, method, OptimizeTrivialDefinitions);
      AttributeCounter = AttributeNumberAtStartOfDefinition;
   }

   //| PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)
   //| = "??", signedNumber(Int64 constPriority), "??"
   private static void ConstantPriorityGiven(out IntMethodClass? dynamicPriority) => dynamicPriority = null;

   //| | "??", CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, "??"?
   private static void DynamicPriorityRecognized(out Int64 constPriority, out IntMethodClass? dynamicPriority, IntMethodClass? intMethod)
   {
      constPriority = 0;
      dynamicPriority = intMethod;
   }

   //| signedNumber(Int64 value)
   //| = Number (Int64 value)
   //| | "+", Number (Int64 value)
   //| | "-", Number (Int64 value)
   private static void NegateNumber(ref Int64 value) => value = -value;

   //| SemanticAction(VoidMethodClass method)
   //| = CSharpStart, CSvoidMethod(VoidMethodClass method), CSharpEnd
   //| | CSharpStart, CSharpEnd
   private static void EmptySemanticAction(out VoidMethodClass? method) => method = null;

   //| "Name(Attributes)" (UnifiedString name, Int32 NumberOfAttributes)
   //| = NameOrString(UnifiedString name), "(Attributes)"(Int32 NumberOfAttributes)
   //| | NameOrString(UnifiedString name) ??-10?? // low priority: if "(" follows then assume that attributes follow
   private static void NameWithoutAttributes(out Int32 NumberOfAttributes) => NumberOfAttributes = 0;

   //| outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)
   //| = "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes)
   private void LeftSideOfOuterProduction(out Symbol SymbolAtLeftSide, UnifiedString name, Int32 NumberOfAttributes)
       => SymbolAtLeftSide = NonterminalSymbolDefinition(name, NumberOfAttributes);

   //| NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)
   //| ="Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes), "="
   private void LeftSideOfNestedProduction(out Symbol SymbolAtLeftSide, UnifiedString name, Int32 NumberOfAttributes)
       => SymbolAtLeftSide = NonterminalSymbolDefinition(name, NumberOfAttributes); // same as LeftSideOfOuterProduction();

   //| SequenceOfElements
   //| = Element
   //| | SequenceOfElements, ","?, Element  // allow to omit the "," (but not between an NameOrString without attributes and a grouped definition)

   //| Element
   //| = RepeatedElement(Symbol Symbol)
   private void ElementVariantRecognized(Symbol Symbol)
   {
      NumberOfElements++;
      ActualListOfElements.Add(Symbol);
   }

   //| RepeatedElement(Symbol Symbol)
   //| = SimpleElement(Symbol Symbol)
   //| | "{", NestedElement(Symbol Symbol), "}"
   private void RepeatGroupRecognized(ref Symbol Symbol)
       => Symbol = MakeGrammarRule(Symbol, TypeOfGrammarRule.repeat0lr);

   //| | "[", NestedElement(Symbol Symbol), "]"
   private void OptionGroupRecognized(ref Symbol Symbol)
       => Symbol = MakeGrammarRule(Symbol, TypeOfGrammarRule.optional);
   //| | SimpleElement(Symbol Symbol), "?"
   private void OptionalElementRecognized(ref Symbol Symbol)
       => Symbol = MakeGrammarRule(Symbol, TypeOfGrammarRule.optional);

   //| | SimpleElement(Symbol Symbol), "+"
   private void Repeat1lrRecognized(ref Symbol Symbol)
       => Symbol = MakeGrammarRule(Symbol, TypeOfGrammarRule.repeat1lr);

   //| | SimpleElement(Symbol Symbol), "+", "+"
   private void Repeat1rrRecognized(ref Symbol Symbol)
       => Symbol = MakeGrammarRule(Symbol, TypeOfGrammarRule.repeat1rr);

   //| | SimpleElement(Symbol Symbol), "*"
   private void Repeat0lrRecognized(ref Symbol Symbol)
       => Symbol = MakeGrammarRule(Symbol, TypeOfGrammarRule.repeat0lr);

   //| | SimpleElement(Symbol Symbol), "*", "*"
   private void Repeat0rrRecognized(ref Symbol Symbol)
       => Symbol = MakeGrammarRule(Symbol, TypeOfGrammarRule.repeat0rr);

   //| SimpleElement(Symbol Symbol)
   //| = "(", NestedElement(Symbol Symbol), ")"
   //| | "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes)
   private void FoundSymbolnameInRightSide(out Symbol Symbol, UnifiedString name, Int32 NumberOfAttributes)
       => Symbol = EvaluateSymbolnameFoundInRightSide(name, NumberOfAttributes);

   //| NestedElement(Symbol Symbol)
   //| = SaveVariables(Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions
   //|    , Int32 SavedNumberOfTrivialDefinitions, Int32 SavedNumberOfElements, Int32 SavedNumberOfSymbolAttributes)
   //|    , NestedGrammarRule(Symbol NestedSymbol, Int32 NumberOfAttributes)
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

   //| SaveVariables(Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions
   //|            , Int32 SavedNumberOfTrivialDefinitions, Int32 SavedNumberOfElements
   //|            , Int32 SavedNumberOfSymbolAttributes)
   //| =
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

   //| Attribut(Int32 number)
   //| = Name(UnifiedString typeString), Name(UnifiedString nameString)
   private void AttributeTypeAndName(out Int32 number, UnifiedString typeString, UnifiedString nameString)
   {
      AttributeCounter++;
      PushAttributeToListOfAttributesOfGrammarRule(typeString, nameString);
      number = AttributeCounter;
   }

   //| "Attributes)"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)
   //| = Attribut(Int32 numberBeforeGroup), Comma, "Attributes)"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup) /* Rekursion */
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

   //| | Attribut(Int32 number), ")"
   private static void FirstAttributeOfGroup(out Int32 numberOfAttributesOfGroup, out Int32 smallestNumber, Int32 number)
   {
      smallestNumber = number;
      numberOfAttributesOfGroup = 1;
   }

   //| "(Attributes)" (Int32 numberOfAttributes)
   //| = "(", ")"
   private static void EmptyListOfAttributes(out Int32 numberOfAttributes) => numberOfAttributes = 0;

   //| | "(", "Attributes)" (Int32 numberOfAttributes, Int32 smallestNumber)

   //| /* ------------------------------- simplified CSharp grammar ------------------- */
   //|
   //| CSvoidMethod(VoidMethodClass voidMethod)=
   //|    CSMethodProperties(MethodClass method), "(", formalParameters?, ")"
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
   //| CSintMethod(IntMethodClass intMethod)
   //| = CSMethodProperties(MethodClass method), "(", formalParameters?, ")"
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

   //| CSMethodProperties(MethodClass method)
   //| = CSAttribute?, Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString)
   private void MethodTypeAndNameRecognized(out MethodClass method, UnifiedString methodTypeString, UnifiedString methodNameString)
       => MethodProperties(out method, methodModifier: new UnifiedString(), typeString: methodTypeString, name: methodNameString);

   //| | CSAttribute?, Name(UnifiedString modifierString), Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString) /* AccessModifier (private, public ...), type, methodname */
   private void MethodModifierTypeAndNameRecognized(
       out MethodClass method, UnifiedString modifierString, UnifiedString methodTypeString, UnifiedString methodNameString)
       => MethodProperties(out method, modifierString, methodTypeString, methodNameString);

   //| | CSAttribute?, Name(UnifiedString modifier1String), Name(UnifiedString modifier2String), Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString) /* AccessModifier (private, public ...), type, methodname */
   private void MethodModifierTypeAndNameRecognized2(
       out MethodClass method, UnifiedString modifier1String, UnifiedString modifier2String, UnifiedString methodTypeString, UnifiedString methodNameString)
       => MethodProperties(out method, modifier1String, modifier2String, methodTypeString, methodNameString);

   //|
   //| CSAttribute
   //| = "[" OptCSAttributeChars  "]"
   //| | "[" OptCSAttributeChars CSAttribute OptCSAttributeChars  "]";
   //|
   //| OptCSAttributeChars
   //| = /* empty */
   //| | OptCSAttributeChars CSAttributeChar;
   //|
   //| CSAttributeChar
   //| -= OptionStart | OptionEnd | CSharpStart | CSharpEnd  ;
   //|
   //| formalParameters
   //| = formalParameter
   //| | formalParameters, Comma, formalParameter

   //| formalParameter
   //| = Name(UnifiedString typeString), Name(UnifiedString nameString) // allow nullable types // TODO ignore if calls, but if record use n generated code
   [MethodImpl(MethodImplOptions.AggressiveInlining)]

   private void FormalParameterWithTypeAndName(UnifiedString typeString, UnifiedString nameString)
       => FormalParameter(new UnifiedString(0), typeString, nameString);

   //| | Name(UnifiedString typeString), "?", Name(UnifiedString nameString) // allow nullable types // TODO ignore if calls, but if record use n generated code
   [MethodImpl(MethodImplOptions.AggressiveInlining)]

   private void FormalParameterWithNullableTypeAndName(UnifiedString typeString, UnifiedString nameString)
       => FormalParameter(new UnifiedString(), typeString, nameString);

   //| | Name(UnifiedString ParameterModifierOptString), Name(UnifiedString typeString), Name(UnifiedString nameString)  /* ref or out, type, identifier */
   [MethodImpl(MethodImplOptions.AggressiveInlining)]

   private void FormalParameterWithModifierTypeAndName(UnifiedString ParameterModifierOptString, UnifiedString typeString, UnifiedString nameString)
       => FormalParameter(ParameterModifierOptString, typeString, nameString);

   //| | Name(UnifiedString ParameterModifierOptString), Name(UnifiedString typeString), "?", Name(UnifiedString nameString)  /* ref or out, type, identifier */
   [MethodImpl(MethodImplOptions.AggressiveInlining)]

   private void FormalParameterWithModifierNullableTypeAndName(UnifiedString ParameterModifierOptString, UnifiedString typeString, UnifiedString nameString)
       => FormalParameter(ParameterModifierOptString, typeString, nameString);

   //| CSEnumDeclaration
   //| = CSEnumProperties, optionalBaseType, CSEnumMembers
   [MethodImpl(MethodImplOptions.AggressiveInlining)]

   private void CSEnumRecognized()
   {
      // all elements of the enum have been added to Enumlist by CSEnumMember
      Lexer.SkipToEndOfCSLines(); // allows some unchecked code after the enum
   }

   //| CSEnumProperties
   //| = Name(UnifiedString modifier1StringIndex), Name(UnifiedString modifier2StringIndex), Name(UnifiedString enumStringIndex), CSEnumName
   //| | Name(UnifiedString modifierStringIndex), Name(UnifiedString enumStringIndex), CSEnumName
   //| | Name(UnifiedString enumStringIndex), CSEnumName

   //| CSEnumName= Name(UnifiedString nameString)
   private void EnumNameRecognized(UnifiedString nameString)
   {
      DictCountBeforeEnum = SymbolDictionary.Count;
      EnumName = nameString;
   }
   int DictCountBeforeEnum = 0;

   //| optionalBaseType
   //| = /* empty */
   //| | ":", Name(UnifiedString Ignored);
   //|
   //| CSEnumMembers
   //| = "{", "}"
   [MethodImpl(MethodImplOptions.AggressiveInlining)]

   private void EmptyEnumRecognized()
   {
      ResetEnumDefaults();
   }
   //| | "{", CSEnumMemberList, "}"

   //| CSEnumMemberList
   //| = ResetEnumDefaults, CSEnumMember
   //| | CSEnumMemberList, Comma, CSEnumMember

   //| CSEnumMember
   //| = OptionalDescriptionAttribute(String description), Name(UnifiedString enumElementString), OptionalEnumElementNumber(Int64 enumNumber)
   [MethodImpl(MethodImplOptions.AggressiveInlining)]

   private void EnumElementRecognized(String description, UnifiedString enumElementString, Int64 enumNumber)
   {
      EvaluateEnumElement(description, enumElementString, enumNumber);
   }

   //| OptionalEnumElementNumber(Int64 enumNumber)
   //| = /* empty */
   [MethodImpl(MethodImplOptions.AggressiveInlining)]

   private void NoEnumElementNumber(out Int64 enumNumber)
   {
      Int64 NextValue = EnumValues.Count <= 0 ? 0 : EnumValues[^1] + 1;
      enumNumber = NextValue;
   }
   //| | "=", Number(Int64 enumNumber);
   //|
   //| ResetEnumDefaults= /*empty*/
   private void ResetEnumDefaults()
   {
      EnumNames.Clear();
      EnumValues.Clear();
   }
   //|
   //| OptionalDescriptionAttribute(String description)
   //| = /* empty */ 
   [MethodImpl(MethodImplOptions.AggressiveInlining)]

   private static void NoDescriptionAttribute(out String description)
   {
      description = "";
   }
   //| | "[", Name(UnifiedString attributeIdentifier) , "(", LexerString(UnifiedString descriptionString), ")", "]"
   [MethodImpl(MethodImplOptions.AggressiveInlining)]

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
             $"Grammar analysis error:{nl}input symbol \"{symbol.LexerResultToString()}\" not allowed in state {stateNumber}{nl}{stateDescription}{nl}");
         return false; // do not skip # because it may be #endregion grammar
      }

      var aCountBeforeAccept = _a.Count;

      // accept the symbol that caused the error to make its position available in Lexer.Lex1TextPos and to discard it
      Lexer.AcceptSymbol();

      P1OutputMessageAndLexerPosition(MessageTypeOrDestinationEnum.Error,
          $"Grammar analysis error:{nl}input symbol \"{symbol.LexerResultToString()}\" ignored: not allowed in state {stateNumber}{nl}{stateDescription}{nl}");

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
      LastTerminalValue = -1;
      /* ************************ end of code written by programmer ******************** */
      #region grammlator generated 13 Dez. 2020 (grammlator file version/date 2020.11.09.0/13 Dez. 2020)
      Int32 _StateStackInitialCount = _s.Count;
      Int32 _AttributeStackInitialCount = _a.Count;
      const Int64 _fDefiningSymbol = 1L << (Int32)LexerResult.DefiningSymbol;
      const Int64 _fColon = 1L << (Int32)LexerResult.Colon;
      const Int64 _fPercent = 1L << (Int32)LexerResult.Percent;
      const Int64 _fCSharpEnd = 1L << (Int32)LexerResult.CSharpEnd;
      const Int64 _fError = 1L << (Int32)LexerResult.Error;
      const Int64 _fMinus = 1L << (Int32)LexerResult.Minus;
      const Int64 _fNumber = 1L << (Int32)LexerResult.Number;
      const Int64 _fStarEqual = 1L << (Int32)LexerResult.StarEqual;
      const Int64 _fMinusEqual = 1L << (Int32)LexerResult.MinusEqual;
      const Int64 _fNumberSign = 1L << (Int32)LexerResult.NumberSign;
      const Int64 _fGroupStart = 1L << (Int32)LexerResult.GroupStart;
      const Int64 _fCSharpStart = 1L << (Int32)LexerResult.CSharpStart;
      const Int64 _fName = 1L << (Int32)LexerResult.Name;
      const Int64 _fLexerString = 1L << (Int32)LexerResult.LexerString;
      const Int64 _fDefinitionSeparatorSymbol = 1L << (Int32)LexerResult.DefinitionSeparatorSymbol;
      const Int64 _fTerminatorSymbol = 1L << (Int32)LexerResult.TerminatorSymbol;
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
         goto State81;
      }
      if (ParserInput == LexerResult.TerminatorSymbol)
      {
         Lexer.AcceptSymbol();
         goto Reduce2;
      }
      if (ParserInput == LexerResult.LexerString)
         goto AcceptState76;
      if (ParserInput == LexerResult.StarEqual)
         goto Reduce2;
      if (ParserInput != LexerResult.CSharpStart)
      {
         if (ErrorHandler(2, StateDescription2, ParserInput))
         {
            _s.Remove(1);
            goto State2;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.CSharpStart);
   AcceptState86:
      Lexer.AcceptSymbol();
   State86:
      const String StateDescription86 =
           "EnumOrEmptyCode= CSharpStart, ►CSEnumDeclaration, CSharpEnd;\r\n"
         + "EnumOrEmptyCode= CSharpStart, ►CSharpEnd;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Name)
      {
         Lexer.AcceptSymbol();
         goto State103;
      }
      if (ParserInput != LexerResult.CSharpEnd)
      {
         if (ErrorHandler(86, StateDescription86, ParserInput))
            goto State86;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.CSharpEnd);
   AcceptBranch18:
      Lexer.AcceptSymbol();
      // Branch18:
      if (_s.Peek() == 0)
         goto Reduce2;
      Reduce8:
      /* sAdjust: -1
       * OptionalDeclarationOfTerminalSymbols= TerminalSymbolsList, OptionalSemikolonOrEnum;◄ */
      _s.Remove(1);
   Reduce2:
      /* DeclarationOfTerminalSymbols= OptionalDeclarationOfTerminalSymbols;◄ */

      CompareTerminalDeclarationsWithEnum();

   State9:
      const String StateDescription9 =
           "GrammlatorGrammar= OptionalGrammlatorSettings, DeclarationOfTerminalSymbols, ►GrammarRuleList, TerminatorAtEndOfGrammar;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.StarEqual)
      {
         if (ErrorHandler(9, StateDescription9, ParserInput))
            goto State9;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.StarEqual);
      Lexer.AcceptSymbol();
      // Reduce10:
      /* "*="= StarEqual;◄ */

      StartOfFirstGrammarRule();

   State10:
      const String StateDescription10 =
           "FirstGrammarRule= \"*=\", ►outerDefinitions;";
      _s.Push(2);
      ParserInput = Lexer.PeekSymbol();
      switch (ParserInput)
      {
         // <= LexerResult.NumberSign: goto HandleError10; // see end of switch
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
            goto HandleError10;
         case LexerResult.GroupStart:
            goto AcceptState68;
         case LexerResult.OptionStart:
            goto AcceptState66;
         case LexerResult.RepeatStart:
            goto AcceptState53;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState42;
         case LexerResult.CSharpStart:
            goto AcceptState23;
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto AcceptState76;
            // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce11; // see end of switch
      } // end of switch
      if (ParserInput <= LexerResult.NumberSign)
         goto HandleError10;
      Debug.Assert(ParserInput >= LexerResult.DefinitionSeparatorSymbol);

   Reduce11:
      /* EndOfDefinitionWithoutSemantics= ;◄ */

      EndOfDefinitionWithoutSemanticsRecognized();

      // State15:
      /* outerDefinitions= EndOfDefinitionWithoutSemantics, ►";";
       * outerDefinitions= EndOfDefinition, ►"|", outerDefinitionList; */
      Debug.Assert(ParserInput >= LexerResult.DefinitionSeparatorSymbol);
      if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
         goto AcceptState16;
      Debug.Assert(ParserInput == LexerResult.TerminatorSymbol);
   AcceptBranch5:
      Lexer.AcceptSymbol();
   Branch5:
      if (_s.Peek() == 2)
         goto Reduce13;
      Reduce73:
      /* sAdjust: -1
       * GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "=", outerDefinitions;◄ */
      _s.Remove(1);
   Reduce69:
      /* sAdjust: -1, aAdjust: -2
       * GrammarRuleList= GrammarRuleList, GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes);◄ */
      _s.Remove(1);

      EndOfGrammarRuleRecognized(
         SymbolAtLeftSide: _a.PeekRef(-1)._Symbol
         );

      _a.Remove(2);
   State70:
      const String StateDescription70 =
           "GrammlatorGrammar= OptionalGrammlatorSettings, DeclarationOfTerminalSymbols, GrammarRuleList, ►TerminatorAtEndOfGrammar;\r\n"
         + "GrammarRuleList= GrammarRuleList, ►GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes);";
      _s.Push(10);
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.NumberSign)
      {
         Lexer.AcceptSymbol();
         // Reduce67:
         /* sAdjust: -2
          * GrammlatorGrammar= OptionalGrammlatorSettings, DeclarationOfTerminalSymbols, GrammarRuleList, TerminatorAtEndOfGrammar;◄ */
         _s.Remove(2);
         goto EndOfGeneratedCode;
      }
      if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
      {
         if (ErrorHandler(70, StateDescription70, ParserInput))
         {
            _s.Remove(1);
            goto State70;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
   AcceptState76:
      Lexer.AcceptSymbol();
   State76:
      const String StateDescription76 =
           "\"Name(Attributes)\"(UnifiedString name, Int32 NumberOfAttributes)= NameOrString(UnifiedString name), ►\"(Attributes)\"(Int32 NumberOfAttributes);\r\n"
         + "\"Name(Attributes)\"(UnifiedString name, Int32 NumberOfAttributes)= NameOrString(UnifiedString name)●;";
      // *Push(0)
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.GroupStart)
      {
         Lexer.AcceptSymbol();
         // PushState6:
         _s.Push(0);
         goto State77;
      }
      if (_is(_fColon | _fCSharpEnd | _fError | _fMinus | _fNumber | _fNumberSign))
      {
         if (ErrorHandler(76, StateDescription76, ParserInput))
            goto State76;
         goto EndWithError;
      }
      Debug.Assert(!_is(_fColon | _fCSharpEnd | _fError | _fMinus | _fNumber | _fNumberSign | _fGroupStart));
      // Reduce74:
      /* aAdjust: 1
       * "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes)= NameOrString(UnifiedString name);◄ */
      _a.Allocate(1);

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
            goto State64;
         case 10:
            // Reduce68:
            {
               /* outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes);◄ */

               LeftSideOfOuterProduction(
                  SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
                  name: _a.PeekClear(-1)._UnifiedString,
                  NumberOfAttributes: _a.PeekRef(0)._Int32
                  );

               goto State71;
            }
            /*case 2: case 3: case 4: case 5: case 7: case 8: case 9: case 11:
            default: break; */
      }
   Reduce12:
      /* aAdjust: -1
       * SimpleElement(Symbol Symbol)= "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes);◄ */

      FoundSymbolnameInRightSide(
         Symbol: out _a.PeekRef(-1)._Symbol,
         name: _a.PeekClear(-1)._UnifiedString,
         NumberOfAttributes: _a.PeekRef(0)._Int32
         );

      _a.Remove(1);
   State11:
      const String StateDescription11 =
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
         // Reduce17:
         /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "?";◄ */

         OptionalElementRecognized(
            Symbol: ref _a.PeekRef(0)._Symbol
            );

         goto Branch3;
      }
      if (ParserInput == LexerResult.Asterisk)
      {
         Lexer.AcceptSymbol();
         goto State13;
      }
      if (ParserInput == LexerResult.Plus)
      {
         Lexer.AcceptSymbol();
         goto State12;
      }
      if (ParserInput <= LexerResult.MinusEqual
         || ParserInput == LexerResult.NumberSign)
      {
         if (ErrorHandler(11, StateDescription11, ParserInput))
            goto State11;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Comma
         || ParserInput >= LexerResult.GroupStart);
      // Reduce16:
      /* aAdjust: -1
       * Element= RepeatedElement(Symbol Symbol);◄ */

      ElementVariantRecognized(
         Symbol: _a.PeekRef(0)._Symbol
         );

      _a.Remove(1);
   Branch2:
      switch (_s.Peek())
      {
         case 5:
            // Reduce29:
            {
               /* sAdjust: -2
                * SequenceOfElements= SequenceOfElements, ","?, Element;◄ */
               _s.Remove(2);
               goto Branch2;
            }
         case 6:
         case 7:
         case 8:
         case 9:
            goto State59;
            /*case 2: case 3: case 4: case 11:
            default: break; */
      }
   State18:
      const String StateDescription18 =
           "SequenceOfElements= SequenceOfElements, ►\",\"?, Element;\r\n"
         + "outerDefinitionList= SequenceOfElements, ►EndOfDefinition, \"|\", outerDefinitionList;\r\n"
         + "outerLastDefinitionOfSequence= SequenceOfElements, ►EndOfDefinitionWithoutSemantics, \";\";\r\n"
         + "outerLastDefinitionOfSequence= SequenceOfElements, ►EndOfDefinitionWithSemantics, \";\"?;";
      _s.Push(1);
      ParserInput = Lexer.PeekSymbol();
      switch (ParserInput)
      {
         // <= LexerResult.Plus: goto HandleError18; // see end of switch
         case LexerResult.NumberSign:
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
            goto HandleError18;
         case LexerResult.Comma:
            goto AcceptState22;
         case LexerResult.GroupStart:
         case LexerResult.OptionStart:
         case LexerResult.RepeatStart:
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto State22;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState42;
         case LexerResult.CSharpStart:
            goto AcceptState23;
            // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce25; // see end of switch
      } // end of switch
      if (ParserInput <= LexerResult.Plus)
         goto HandleError18;
      Debug.Assert(ParserInput >= LexerResult.DefinitionSeparatorSymbol);

      // Reduce25:
      /* EndOfDefinitionWithoutSemantics= ;◄ */

      EndOfDefinitionWithoutSemanticsRecognized();

      // State19:
      /* outerDefinitionList= SequenceOfElements, EndOfDefinition, ►"|", outerDefinitionList;
       * outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithoutSemantics, ►";"; */
      Debug.Assert(ParserInput >= LexerResult.DefinitionSeparatorSymbol);
      if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
         goto AcceptState20;
      Debug.Assert(ParserInput == LexerResult.TerminatorSymbol);
   AcceptReduce27:
      Lexer.AcceptSymbol();
   Reduce27:
      /* sAdjust: -1
       * outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithoutSemantics, ";";◄
       * or: outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithSemantics, ";"?;◄ */
      _s.Remove(1);
      // Branch6:
      switch (_s.Peek())
      {
         case 3:
            goto Reduce24;
         case 4:
            goto Reduce28;
         case 11:
            goto Reduce73;
            /*case 2:
            default: break; */
      }
   Reduce13:
      /* sAdjust: -1
       * FirstGrammarRule= "*=", outerDefinitions;◄ */
      _s.Remove(1);

      FirstGrammarRuleRecognized();

      goto State70;

   Reduce14:
      /* aAdjust: -1
       * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ */

      EndOfDefinitionWithMethodRecognized(
         method: _a.PeekRef(0)._VoidMethodClass
         );

      _a.Remove(1);
   State17:
      const String StateDescription17 =
           "outerDefinitions= EndOfDefinitionWithSemantics, ►\";\"?;\r\n"
         + "outerDefinitions= EndOfDefinition, ►\"|\", outerDefinitionList;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
         goto AcceptState16;
      if (ParserInput == LexerResult.TerminatorSymbol)
         goto AcceptBranch5;
      if (ParserInput != LexerResult.NumberSign
         && ParserInput < LexerResult.Name)
      {
         if (ErrorHandler(17, StateDescription17, ParserInput))
            goto State17;
         goto EndWithError;
      }
      Debug.Assert(_is(_fNumberSign | _fName | _fLexerString));
      goto Branch5;

   AcceptReduce35:
      Lexer.AcceptSymbol();
      // Reduce35:
      /* sAdjust: -1
       * CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), "(", formalParameters?, ")";◄ */
      _s.Remove(1);

      CSvoidMethodRecognized(
         voidMethod: out _a.PeekRef(0)._VoidMethodClass,
         method: _a.PeekClear(0)._MethodClass
         );

   State37:
      const String StateDescription37 =
           "SemanticAction(VoidMethodClass method)= CSharpStart, CSvoidMethod(VoidMethodClass method), ►CSharpEnd;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.CSharpEnd)
      {
         if (ErrorHandler(37, StateDescription37, ParserInput))
            goto State37;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.CSharpEnd);
      Lexer.AcceptSymbol();
      // Reduce41:
      /* sAdjust: -1
       * SemanticAction(VoidMethodClass method)= CSharpStart, CSvoidMethod(VoidMethodClass method), CSharpEnd;◄ */
      _s.Remove(1);
   Branch8:
      switch (_s.Peek())
      {
         case 1:
            // Reduce26:
            {
               /* aAdjust: -1
                * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ */

               EndOfDefinitionWithMethodRecognized(
                  method: _a.PeekRef(0)._VoidMethodClass
                  );

               _a.Remove(1);
               goto State21;
            }
         case 2:
         case 11:
            goto Reduce14;
         case 3:
            // Reduce59:
            {
               /* sAdjust: -1, aAdjust: -1
                * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄
                * then: NestedDefinitionList= NestedDefinitionList, "|", Definition;◄
                * or: Definition= SequenceOfElements, EndOfDefinition;◄ */
               _s.Remove(1);

               EndOfDefinitionWithMethodRecognized(
                  method: _a.PeekRef(0)._VoidMethodClass
                  );

               _a.Remove(1);
               goto Branch14;
            }
         case 6:
            // Reduce54:
            {
               /* aAdjust: -1
                * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ */

               EndOfDefinitionWithMethodRecognized(
                  method: _a.PeekRef(0)._VoidMethodClass
                  );

               _a.Remove(1);
               goto State55;
            }
         case 9:
            // Reduce62:
            {
               /* aAdjust: -1
                * EndOfDefinitionWithSemantics= SemanticAction(VoidMethodClass method);◄ */

               EndOfDefinitionWithMethodRecognized(
                  method: _a.PeekRef(0)._VoidMethodClass
                  );

               _a.Remove(1);
               goto State62;
            }
            /*case 0:
            default: break; */
      }
      // Reduce23:
      /* sAdjust: -1, aAdjust: -3
       * EndOfDefinitionWithSemantics= PriorityDeclaration(Int64 constPriority, IntMethodClass dynPriority), SemanticAction(VoidMethodClass method);◄ */
      _s.Remove(1);

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
            goto State17;
         case 3:
            goto Reduce57;
         case 6:
            goto State55;
         case 9:
            goto State62;
            /*case 1:
            default: break; */
      }
   State21:
      const String StateDescription21 =
           "outerDefinitionList= SequenceOfElements, EndOfDefinition, ►\"|\", outerDefinitionList;\r\n"
         + "outerLastDefinitionOfSequence= SequenceOfElements, EndOfDefinitionWithSemantics, ►\";\"?;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
         goto AcceptState20;
      if (ParserInput == LexerResult.TerminatorSymbol)
         goto AcceptReduce27;
      if (ParserInput != LexerResult.NumberSign
         && ParserInput < LexerResult.Name)
      {
         if (ErrorHandler(21, StateDescription21, ParserInput))
            goto State21;
         goto EndWithError;
      }
      Debug.Assert(_is(_fNumberSign | _fName | _fLexerString));
      goto Reduce27;

   AcceptReduce46:
      Lexer.AcceptSymbol();
      // Reduce46:
      /* sAdjust: -1
       * CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), "(", formalParameters?, ")";◄ */
      _s.Remove(1);

      CSintMethodRecognized(
         intMethod: out _a.PeekRef(0)._IntMethodClass,
         method: _a.PeekClear(0)._MethodClass
         );

   State48:
      const String StateDescription48 =
           "PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, CSintMethod(IntMethodClass intMethod), ►CSharpEnd, \"??\"?;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.CSharpEnd)
      {
         if (ErrorHandler(48, StateDescription48, ParserInput))
            goto State48;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.CSharpEnd);
      Lexer.AcceptSymbol();
   State49:
      const String StateDescription49 =
           "PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, ►\"??\"?;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.DoubleQuestionmark)
      {
         Lexer.AcceptSymbol();
         goto Reduce47;
      }
      if (ParserInput != LexerResult.NumberSign
         && ParserInput < LexerResult.CSharpStart)
      {
         if (ErrorHandler(49, StateDescription49, ParserInput))
            goto State49;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.NumberSign
         || ParserInput >= LexerResult.CSharpStart);
   Reduce47:
      /* sAdjust: -1, aAdjust: 1
       * PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= "??", CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, "??"?;◄ */
      _s.Remove(1);
      _a.Allocate(1);

      DynamicPriorityRecognized(
         constPriority: out _a.PeekRef(-1)._Int64,
         dynamicPriority: out _a.PeekRef(0)._IntMethodClass,
         intMethod: _a.PeekClear(-1)._IntMethodClass
         );

   State14:
      const String StateDescription14 =
           "EndOfDefinitionWithSemantics= PriorityDeclaration(Int64 constPriority, IntMethodClass dynPriority)●;\r\n"
         + "EndOfDefinitionWithSemantics= PriorityDeclaration(Int64 constPriority, IntMethodClass dynPriority), ►SemanticAction(VoidMethodClass method);";
      // *Push(0)
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.CSharpStart)
      {
         Lexer.AcceptSymbol();
         // PushState1:
         _s.Push(0);
         goto State23;
      }
      if (ParserInput != LexerResult.NumberSign
         && ParserInput < LexerResult.GroupEnd)
      {
         if (ErrorHandler(14, StateDescription14, ParserInput))
            goto State14;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.NumberSign
         || ParserInput >= LexerResult.GroupEnd);
      // Reduce22:
      /* aAdjust: -2
       * EndOfDefinitionWithSemantics= PriorityDeclaration(Int64 constPriority, IntMethodClass dynPriority);◄ */

      EndOfDefinitionWithPriorityRecognized(
         constPriority: _a.PeekRef(-1)._Int64,
         dynPriority: _a.PeekRef(0)._IntMethodClass
         );

      _a.Remove(2);
      goto Branch4;

   Reduce50:
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

   State54:
      const String StateDescription54 =
           "NestedElement(Symbol Symbol)= SaveVariables(Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions, Int32 SavedNumberOfTrivialDefinitions, Int32 SavedNumberOfElements, Int32 SavedNumberOfSymbolAttributes), ►NestedGrammarRule(Symbol NestedSymbol, Int32 NumberOfAttributes);";
      _s.Push(6);
      ParserInput = Lexer.PeekSymbol();
      switch (ParserInput)
      {
         // <= LexerResult.NumberSign
         // >= LexerResult.TerminatorSymbol: goto HandleError54; // see end of switch
         case LexerResult.GroupStart:
            goto AcceptState68;
         case LexerResult.OptionStart:
            goto AcceptState66;
         case LexerResult.RepeatStart:
            goto AcceptState53;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState42;
         case LexerResult.CSharpStart:
            goto AcceptState23;
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
         case LexerResult.DefinitionSeparatorSymbol:
            // Reduce51:
            {
               /* EndOfDefinitionWithoutSemantics= ;◄ */

               EndOfDefinitionWithoutSemanticsRecognized();

               goto State55;
            }
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto AcceptState76;
      } // end of switch
      Debug.Assert(ParserInput <= LexerResult.NumberSign || ParserInput >= LexerResult.TerminatorSymbol);

      if (ErrorHandler(54, StateDescription54, ParserInput))
      {
         _s.Remove(1);
         goto State54;
      };
      goto EndWithError;

   Reduce53:
      /* aAdjust: 2
       * NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions;◄ */
      _a.Allocate(2);

      NestedGrammarRuleWithEmptyLeftside(
         SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
         NumberOfAttributes: out _a.PeekRef(0)._Int32
         );

   Reduce52:
      /* sAdjust: -1, aAdjust: -6
       * NestedElement(Symbol Symbol)= SaveVariables(Int32 SavedAttributeNumberAtStartOfDefinition, Int32 SavedNumberOfDefinitions, Int32 SavedNumberOfTrivialDefinitions, Int32 SavedNumberOfElements, Int32 SavedNumberOfSymbolAttributes), NestedGrammarRule(Symbol NestedSymbol, Int32 NumberOfAttributes);◄ */
      _s.Remove(1);

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
      // Branch12:
      switch (_s.Peek())
      {
         case 0:
            goto State65;
         case 1:
            goto State67;
            /*case 2:
            default: break; */
      }
   State69:
      const String StateDescription69 =
           "SimpleElement(Symbol Symbol)= \"(\", NestedElement(Symbol Symbol), ►\")\";";
      Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
      if (ParserInput != LexerResult.GroupEnd)
      {
         if (ErrorHandler(69, StateDescription69, ParserInput))
            goto State69;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.GroupEnd);
      Lexer.AcceptSymbol();
      // Reduce66:
      /* sAdjust: -1
       * SimpleElement(Symbol Symbol)= "(", NestedElement(Symbol Symbol), ")";◄ */
      _s.Remove(1);
      goto State11;

   Reduce55:
      /* aAdjust: -1
       * Element= RepeatedElement(Symbol Symbol);◄ */

      ElementVariantRecognized(
         Symbol: _a.PeekRef(0)._Symbol
         );

      _a.Remove(1);
   State59:
      const String StateDescription59 =
           "Definition= SequenceOfElements, ►EndOfDefinition;\r\n"
         + "SequenceOfElements= SequenceOfElements, ►\",\"?, Element;";
      _s.Push(3);
      ParserInput = Lexer.PeekSymbol();
      switch (ParserInput)
      {
         // <= LexerResult.Plus
         // >= LexerResult.TerminatorSymbol: goto HandleError59; // see end of switch
         case LexerResult.NumberSign:
            goto HandleError59;
         case LexerResult.Comma:
            goto AcceptState22;
         case LexerResult.GroupStart:
         case LexerResult.OptionStart:
         case LexerResult.RepeatStart:
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto State22;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState42;
         case LexerResult.CSharpStart:
            goto AcceptState23;
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
         case LexerResult.DefinitionSeparatorSymbol:
            // Reduce58:
            {
               /* sAdjust: -1
                * EndOfDefinitionWithoutSemantics= ;◄
                * then: NestedDefinitionList= NestedDefinitionList, "|", Definition;◄
                * or: Definition= SequenceOfElements, EndOfDefinition;◄ */
               _s.Remove(1);

               EndOfDefinitionWithoutSemanticsRecognized();

               goto Branch14;
            }
      } // end of switch
      Debug.Assert(ParserInput <= LexerResult.Plus || ParserInput >= LexerResult.TerminatorSymbol);

   HandleError59:
      if (ErrorHandler(59, StateDescription59, ParserInput))
      {
         _s.Remove(1);
         goto State59;
      };
      goto EndWithError;

   Reduce57:
      /* sAdjust: -1
       * NestedDefinitionList= NestedDefinitionList, "|", Definition;◄
       * or: Definition= SequenceOfElements, EndOfDefinition;◄ */
      _s.Remove(1);
   Branch14:
      switch (_s.Peek())
      {
         case 6:
            goto State60;
         case 7:
            goto State57;
         case 8:
            goto Reduce57;
            /*case 9:
            default: break; */
      }
   State63:
      const String StateDescription63 =
           "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions●;\r\n"
         + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
         goto AcceptState58;
      if (ParserInput <= LexerResult.CSharpStart
         || ParserInput >= LexerResult.Name)
      {
         if (ErrorHandler(63, StateDescription63, ParserInput))
            goto State63;
         goto EndWithError;
      }
      Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
   Reduce61:
      /* sAdjust: -1
       * NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions;◄ */
      _s.Remove(1);
      goto Reduce52;

   Reduce86:
      /* CSEnumDeclaration= CSEnumProperties, optionalBaseType, CSEnumMembers;◄ */

      CSEnumRecognized();

   State102:
      const String StateDescription102 =
           "EnumOrEmptyCode= CSharpStart, CSEnumDeclaration, ►CSharpEnd;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.CSharpEnd)
      {
         if (ErrorHandler(102, StateDescription102, ParserInput))
            goto State102;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.CSharpEnd);
      goto AcceptBranch18;

   Reduce89:
      /* aAdjust: 1
       * OptionalDescriptionAttribute(String description)= ;◄ */
      _a.Allocate(1);

      NoDescriptionAttribute(
         description: out _a.PeekRef(0)._String
         );

   State91:
      const String StateDescription91 =
           "CSEnumMember= OptionalDescriptionAttribute(String description), ►Name(UnifiedString enumElementString), OptionalEnumElementNumber(Int64 enumNumber);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Name)
      {
         if (ErrorHandler(91, StateDescription91, ParserInput))
            goto State91;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name);
      Lexer.AcceptSymbol();
   State92:
      const String StateDescription92 =
           "CSEnumMember= OptionalDescriptionAttribute(String description), Name(UnifiedString enumElementString), ►OptionalEnumElementNumber(Int64 enumNumber);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.DefiningSymbol)
      {
         Lexer.AcceptSymbol();
         goto State93;
      }
      if (ParserInput != LexerResult.Comma
         && ParserInput != LexerResult.RepeatEnd)
      {
         if (ErrorHandler(92, StateDescription92, ParserInput))
            goto State92;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Comma
         || ParserInput == LexerResult.RepeatEnd);
      // Reduce91:
      /* aAdjust: 1
       * OptionalEnumElementNumber(Int64 enumNumber)= ;◄ */
      _a.Allocate(1);

      NoEnumElementNumber(
         enumNumber: out _a.PeekRef(0)._Int64
         );

   Reduce92:
      /* sAdjust: -1, aAdjust: -3
       * CSEnumMember= OptionalDescriptionAttribute(String description), Name(UnifiedString enumElementString), OptionalEnumElementNumber(Int64 enumNumber);◄
       * then: CSEnumMemberList= ResetEnumDefaults, CSEnumMember;◄
       * or: CSEnumMemberList= CSEnumMemberList, Comma, CSEnumMember;◄ */
      _s.Remove(1);

      EnumElementRecognized(
         description: _a.PeekRef(-2)._String,
         enumElementString: _a.PeekRef(-1)._UnifiedString,
         enumNumber: _a.PeekRef(0)._Int64
         );

      _a.Remove(3);
   State99:
      const String StateDescription99 =
           "CSEnumMembers= \"{\", CSEnumMemberList, ►\"}\";\r\n"
         + "CSEnumMemberList= CSEnumMemberList, ►Comma, CSEnumMember;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Comma)
      {
         Lexer.AcceptSymbol();
         goto State100;
      }
      if (ParserInput != LexerResult.RepeatEnd)
      {
         if (ErrorHandler(99, StateDescription99, ParserInput))
            goto State99;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.RepeatEnd);
      Lexer.AcceptSymbol();
      goto Reduce86;

   State3:
      const String StateDescription3 =
           "TerminalSymbol= \"Name(Attributes)\"(UnifiedString name, Int32 numberOfAttributes), ►OptionalValue(Int64 value), OptionalWeight(Int64 weight);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.DefiningSymbol)
      {
         Lexer.AcceptSymbol();
         goto State6;
      }
      if (!_is(_fPercent | _fStarEqual | _fCSharpStart | _fDefinitionSeparatorSymbol | _fTerminatorSymbol))
      {
         if (ErrorHandler(3, StateDescription3, ParserInput))
            goto State3;
         goto EndWithError;
      }
      Debug.Assert(_is(_fPercent | _fStarEqual | _fCSharpStart | _fDefinitionSeparatorSymbol | _fTerminatorSymbol));
      // Reduce4:
      /* aAdjust: 1
       * OptionalValue(Int64 value)= ;◄ */
      _a.Allocate(1);

      OptionalValueDefault(
         value: out _a.PeekRef(0)._Int64
         );

   State4:
      const String StateDescription4 =
           "TerminalSymbol= \"Name(Attributes)\"(UnifiedString name, Int32 numberOfAttributes), OptionalValue(Int64 value), ►OptionalWeight(Int64 weight);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Percent)
      {
         Lexer.AcceptSymbol();
         goto State5;
      }
      if (!_is(_fStarEqual | _fCSharpStart | _fDefinitionSeparatorSymbol | _fTerminatorSymbol))
      {
         if (ErrorHandler(4, StateDescription4, ParserInput))
            goto State4;
         goto EndWithError;
      }
      Debug.Assert(_is(_fStarEqual | _fCSharpStart | _fDefinitionSeparatorSymbol | _fTerminatorSymbol));
      // Reduce5:
      /* aAdjust: 1
       * OptionalWeight(Int64 weight)= ;◄ */
      _a.Allocate(1);

      OptionalDefaultWeight(
         weight: out _a.PeekRef(0)._Int64
         );

   Reduce6:
      /* aAdjust: -4
       * TerminalSymbol= "Name(Attributes)"(UnifiedString name, Int32 numberOfAttributes), OptionalValue(Int64 value), OptionalWeight(Int64 weight);◄ */

      TerminalSymbol(
         name: _a.PeekRef(-3)._UnifiedString,
         numberOfAttributes: _a.PeekRef(-2)._Int32,
         value: _a.PeekRef(-1)._Int64,
         weight: _a.PeekRef(0)._Int64
         );

      _a.Remove(4);
      // Branch1:
      if (_s.Peek() == 0)
         goto State7;
      // Reduce9:
      /* sAdjust: -2
       * TerminalSymbolsList= TerminalSymbolsList, "|", TerminalSymbol;◄ */
      _s.Remove(2);
   State7:
      const String StateDescription7 =
           "OptionalDeclarationOfTerminalSymbols= TerminalSymbolsList, ►OptionalSemikolonOrEnum;\r\n"
         + "TerminalSymbolsList= TerminalSymbolsList, ►\"|\", TerminalSymbol;";
      _s.Push(1);
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
      {
         Lexer.AcceptSymbol();
         goto State8;
      }
      if (ParserInput == LexerResult.TerminatorSymbol)
      {
         Lexer.AcceptSymbol();
         goto Reduce8;
      }
      if (ParserInput == LexerResult.StarEqual)
         goto Reduce8;
      if (ParserInput != LexerResult.CSharpStart)
      {
         if (ErrorHandler(7, StateDescription7, ParserInput))
         {
            _s.Remove(1);
            goto State7;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.CSharpStart);
      goto AcceptState86;

   State5:
      const String StateDescription5 =
           "OptionalWeight(Int64 weight)= \"%\", ►Number(Int64 weight);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Number)
      {
         if (ErrorHandler(5, StateDescription5, ParserInput))
            goto State5;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Number);
      Lexer.AcceptSymbol();
      goto Reduce6;

   State6:
      const String StateDescription6 =
           "OptionalValue(Int64 value)= \"=\", ►Number(Int64 value);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Number)
      {
         if (ErrorHandler(6, StateDescription6, ParserInput))
            goto State6;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Number);
      Lexer.AcceptSymbol();
      // Reduce7:
      /* OptionalValue(Int64 value)= "=", Number(Int64 value);◄ */

      OptionalValueAssignment(
         value: _a.PeekRef(0)._Int64
         );

      goto State4;

   State8:
      const String StateDescription8 =
           "TerminalSymbolsList= TerminalSymbolsList, \"|\", ►TerminalSymbol;";
      _s.Push(1);
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
      {
         if (ErrorHandler(8, StateDescription8, ParserInput))
         {
            _s.Remove(1);
            goto State8;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
      goto AcceptState76;

   State12:
      const String StateDescription12 =
           "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"+\"●;\r\n"
         + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"+\", ►\"+\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Plus)
      {
         Lexer.AcceptSymbol();
         // Reduce19:
         /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "+", "+";◄ */

         Repeat1rrRecognized(
            Symbol: ref _a.PeekRef(0)._Symbol
            );

         goto Branch3;
      }
      if (ParserInput <= LexerResult.Asterisk
         || ParserInput == LexerResult.NumberSign)
      {
         if (ErrorHandler(12, StateDescription12, ParserInput))
            goto State12;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Comma
         || ParserInput >= LexerResult.GroupStart);
      // Reduce18:
      /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "+";◄ */

      Repeat1lrRecognized(
         Symbol: ref _a.PeekRef(0)._Symbol
         );

   Branch3:
      switch (_s.Peek())
      {
         case 5:
            // Reduce30:
            {
               /* sAdjust: -2, aAdjust: -1
                * Element= RepeatedElement(Symbol Symbol);◄
                * then: SequenceOfElements= SequenceOfElements, ","?, Element;◄ */
               _s.Remove(2);

               ElementVariantRecognized(
                  Symbol: _a.PeekRef(0)._Symbol
                  );

               _a.Remove(1);
               goto Branch2;
            }
         case 6:
         case 7:
         case 8:
         case 9:
            goto Reduce55;
            /*case 2: case 3: case 4: case 11:
            default: break; */
      }
      // Reduce15:
      /* aAdjust: -1
       * Element= RepeatedElement(Symbol Symbol);◄ */

      ElementVariantRecognized(
         Symbol: _a.PeekRef(0)._Symbol
         );

      _a.Remove(1);
      goto State18;

   State13:
      const String StateDescription13 =
           "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"*\"●;\r\n"
         + "RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), \"*\", ►\"*\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Asterisk)
      {
         Lexer.AcceptSymbol();
         // Reduce21:
         /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "*", "*";◄ */

         Repeat0rrRecognized(
            Symbol: ref _a.PeekRef(0)._Symbol
            );

         goto Branch3;
      }
      if (ParserInput <= LexerResult.Plus
         || ParserInput == LexerResult.NumberSign)
      {
         if (ErrorHandler(13, StateDescription13, ParserInput))
            goto State13;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Comma
         || ParserInput >= LexerResult.GroupStart);
      // Reduce20:
      /* RepeatedElement(Symbol Symbol)= SimpleElement(Symbol Symbol), "*";◄ */

      Repeat0lrRecognized(
         Symbol: ref _a.PeekRef(0)._Symbol
         );

      goto Branch3;

   AcceptState16:
      Lexer.AcceptSymbol();
   State16:
      const String StateDescription16 =
           "outerDefinitions= EndOfDefinition, \"|\", ►outerDefinitionList;";
      _s.Push(3);
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.GroupStart)
         goto AcceptState68;
      if (ParserInput == LexerResult.OptionStart)
         goto AcceptState66;
      if (ParserInput == LexerResult.RepeatStart)
         goto AcceptState53;
      if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
      {
         if (ErrorHandler(16, StateDescription16, ParserInput))
         {
            _s.Remove(1);
            goto State16;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
      goto AcceptState76;

   AcceptState20:
      Lexer.AcceptSymbol();
   State20:
      const String StateDescription20 =
           "outerDefinitionList= SequenceOfElements, EndOfDefinition, \"|\", ►outerDefinitionList;";
      _s.Push(4);
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.GroupStart)
         goto AcceptState68;
      if (ParserInput == LexerResult.OptionStart)
         goto AcceptState66;
      if (ParserInput == LexerResult.RepeatStart)
         goto AcceptState53;
      if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
      {
         if (ErrorHandler(20, StateDescription20, ParserInput))
         {
            _s.Remove(1);
            goto State20;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
      goto AcceptState76;

   AcceptState22:
      Lexer.AcceptSymbol();
   State22:
      const String StateDescription22 =
           "SequenceOfElements= SequenceOfElements, \",\"?, ►Element;";
      _s.Push(5);
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.GroupStart)
         goto AcceptState68;
      if (ParserInput == LexerResult.OptionStart)
         goto AcceptState66;
      if (ParserInput == LexerResult.RepeatStart)
         goto AcceptState53;
      if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
      {
         if (ErrorHandler(22, StateDescription22, ParserInput))
         {
            _s.Remove(1);
            goto State22;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
      goto AcceptState76;

   AcceptState23:
      Lexer.AcceptSymbol();
   State23:
      const String StateDescription23 =
           "SemanticAction(VoidMethodClass method)= CSharpStart, ►CSvoidMethod(VoidMethodClass method), CSharpEnd;\r\n"
         + "SemanticAction(VoidMethodClass method)= CSharpStart, ►CSharpEnd;";
      // *Push(0)
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Name)
      // PushState2:
      {
         _s.Push(0);
         goto State24;
      }
      if (ParserInput == LexerResult.OptionStart)
      {
         Lexer.AcceptSymbol();
         // PushState3:
         _s.Push(0);
         goto State39;
      }
      if (ParserInput != LexerResult.CSharpEnd)
      {
         if (ErrorHandler(23, StateDescription23, ParserInput))
            goto State23;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.CSharpEnd);
      Lexer.AcceptSymbol();
      // Reduce31:
      /* aAdjust: 1
       * SemanticAction(VoidMethodClass method)= CSharpStart, CSharpEnd;◄ */
      _a.Allocate(1);

      EmptySemanticAction(
         method: out _a.PeekRef(0)._VoidMethodClass
         );

      goto Branch8;

   State24:
      const String StateDescription24 =
           "CSMethodProperties(MethodClass method)= CSAttribute?, ►Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);\r\n"
         + "CSMethodProperties(MethodClass method)= CSAttribute?, ►Name(UnifiedString modifierString), Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);\r\n"
         + "CSMethodProperties(MethodClass method)= CSAttribute?, ►Name(UnifiedString modifier1String), Name(UnifiedString modifier2String), Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Name)
      {
         if (ErrorHandler(24, StateDescription24, ParserInput))
            goto State24;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name);
      Lexer.AcceptSymbol();
   State25:
      const String StateDescription25 =
           "CSMethodProperties(MethodClass method)= CSAttribute?, Name(UnifiedString methodTypeString), ►Name(UnifiedString methodNameString);\r\n"
         + "CSMethodProperties(MethodClass method)= CSAttribute?, Name(UnifiedString modifierString), ►Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);\r\n"
         + "CSMethodProperties(MethodClass method)= CSAttribute?, Name(UnifiedString modifier1String), ►Name(UnifiedString modifier2String), Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Name)
      {
         if (ErrorHandler(25, StateDescription25, ParserInput))
            goto State25;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name);
      Lexer.AcceptSymbol();
   State26:
      const String StateDescription26 =
           "CSMethodProperties(MethodClass method)= CSAttribute?, Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString)●;\r\n"
         + "CSMethodProperties(MethodClass method)= CSAttribute?, Name(UnifiedString modifierString), Name(UnifiedString methodTypeString), ►Name(UnifiedString methodNameString);\r\n"
         + "CSMethodProperties(MethodClass method)= CSAttribute?, Name(UnifiedString modifier1String), Name(UnifiedString modifier2String), ►Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Name)
      {
         Lexer.AcceptSymbol();
         goto State27;
      }
      if (ParserInput != LexerResult.GroupStart)
      {
         if (ErrorHandler(26, StateDescription26, ParserInput))
            goto State26;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.GroupStart);
      // Reduce32:
      /* aAdjust: -1
       * CSMethodProperties(MethodClass method)= CSAttribute?, Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);◄ */

      MethodTypeAndNameRecognized(
         method: out _a.PeekRef(-1)._MethodClass,
         methodTypeString: _a.PeekClear(-1)._UnifiedString,
         methodNameString: _a.PeekRef(0)._UnifiedString
         );

      _a.Remove(1);
   Branch9:
      if (_s.Peek() == 0)
         goto State28;
      State44:
      const String StateDescription44 =
           "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), ►\"(\", formalParameters?, \")\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.GroupStart)
      {
         if (ErrorHandler(44, StateDescription44, ParserInput))
            goto State44;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.GroupStart);
      Lexer.AcceptSymbol();
   State45:
      const String StateDescription45 =
           "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), \"(\", ►formalParameters?, \")\";";
      _s.Push(2);
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Name)
         goto AcceptState33;
      if (ParserInput != LexerResult.GroupEnd)
      {
         if (ErrorHandler(45, StateDescription45, ParserInput))
         {
            _s.Remove(1);
            goto State45;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.GroupEnd);
      // State46:
      /* CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), "(", formalParameters?, ►")"; */
      Debug.Assert(ParserInput == LexerResult.GroupEnd);
      goto AcceptReduce46;

   State27:
      const String StateDescription27 =
           "CSMethodProperties(MethodClass method)= CSAttribute?, Name(UnifiedString modifierString), Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString)●;\r\n"
         + "CSMethodProperties(MethodClass method)= CSAttribute?, Name(UnifiedString modifier1String), Name(UnifiedString modifier2String), Name(UnifiedString methodTypeString), ►Name(UnifiedString methodNameString);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Name)
      {
         Lexer.AcceptSymbol();
         // Reduce34:
         /* aAdjust: -3
          * CSMethodProperties(MethodClass method)= CSAttribute?, Name(UnifiedString modifier1String), Name(UnifiedString modifier2String), Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);◄ */

         MethodModifierTypeAndNameRecognized2(
            method: out _a.PeekRef(-3)._MethodClass,
            modifier1String: _a.PeekClear(-3)._UnifiedString,
            modifier2String: _a.PeekRef(-2)._UnifiedString,
            methodTypeString: _a.PeekRef(-1)._UnifiedString,
            methodNameString: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove(3);
         goto Branch9;
      }
      if (ParserInput != LexerResult.GroupStart)
      {
         if (ErrorHandler(27, StateDescription27, ParserInput))
            goto State27;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.GroupStart);
      // Reduce33:
      /* aAdjust: -2
       * CSMethodProperties(MethodClass method)= CSAttribute?, Name(UnifiedString modifierString), Name(UnifiedString methodTypeString), Name(UnifiedString methodNameString);◄ */

      MethodModifierTypeAndNameRecognized(
         method: out _a.PeekRef(-2)._MethodClass,
         modifierString: _a.PeekClear(-2)._UnifiedString,
         methodTypeString: _a.PeekRef(-1)._UnifiedString,
         methodNameString: _a.PeekRef(0)._UnifiedString
         );

      _a.Remove(2);
      goto Branch9;

   State28:
      const String StateDescription28 =
           "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), ►\"(\", formalParameters?, \")\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.GroupStart)
      {
         if (ErrorHandler(28, StateDescription28, ParserInput))
            goto State28;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.GroupStart);
      Lexer.AcceptSymbol();
   State29:
      const String StateDescription29 =
           "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), \"(\", ►formalParameters?, \")\";";
      _s.Push(0);
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Name)
         goto AcceptState33;
      if (ParserInput != LexerResult.GroupEnd)
      {
         if (ErrorHandler(29, StateDescription29, ParserInput))
         {
            _s.Remove(1);
            goto State29;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.GroupEnd);
      // State30:
      /* CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), "(", formalParameters?, ►")"; */
      Debug.Assert(ParserInput == LexerResult.GroupEnd);
      goto AcceptReduce35;

   State31:
      const String StateDescription31 =
           "CSvoidMethod(VoidMethodClass voidMethod)= CSMethodProperties(MethodClass method), \"(\", formalParameters?, ►\")\";\r\n"
         + "formalParameters= formalParameters, ►Comma, formalParameter;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.GroupEnd)
         goto AcceptReduce35;
      if (ParserInput != LexerResult.Comma)
      {
         if (ErrorHandler(31, StateDescription31, ParserInput))
            goto State31;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Comma);
   AcceptState32:
      Lexer.AcceptSymbol();
   State32:
      const String StateDescription32 =
           "formalParameters= formalParameters, Comma, ►formalParameter;";
      _s.Push(1);
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Name)
      {
         if (ErrorHandler(32, StateDescription32, ParserInput))
         {
            _s.Remove(1);
            goto State32;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name);
   AcceptState33:
      Lexer.AcceptSymbol();
   State33:
      const String StateDescription33 =
           "formalParameter= Name(UnifiedString typeString), ►Name(UnifiedString nameString);\r\n"
         + "formalParameter= Name(UnifiedString typeString), ►\"?\", Name(UnifiedString nameString);\r\n"
         + "formalParameter= Name(UnifiedString ParameterModifierOptString), ►Name(UnifiedString typeString), Name(UnifiedString nameString);\r\n"
         + "formalParameter= Name(UnifiedString ParameterModifierOptString), ►Name(UnifiedString typeString), \"?\", Name(UnifiedString nameString);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Name)
      {
         Lexer.AcceptSymbol();
         goto State34;
      }
      if (ParserInput != LexerResult.Questionmark)
      {
         if (ErrorHandler(33, StateDescription33, ParserInput))
            goto State33;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Questionmark);
      Lexer.AcceptSymbol();
   State36:
      const String StateDescription36 =
           "formalParameter= Name(UnifiedString typeString), \"?\", ►Name(UnifiedString nameString);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Name)
      {
         if (ErrorHandler(36, StateDescription36, ParserInput))
            goto State36;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name);
      Lexer.AcceptSymbol();
      // Reduce40:
      /* aAdjust: -2
       * formalParameter= Name(UnifiedString typeString), "?", Name(UnifiedString nameString);◄ */

      FormalParameterWithNullableTypeAndName(
         typeString: _a.PeekRef(-1)._UnifiedString,
         nameString: _a.PeekRef(0)._UnifiedString
         );

      _a.Remove(2);
   Branch10:
      switch (_s.Peek())
      {
         case 0:
            goto State31;
         case 1:
            // Reduce36:
            {
               /* sAdjust: -1
                * formalParameters= formalParameters, Comma, formalParameter;◄ */
               _s.Remove(1);
               goto Branch10;
            }
            /*case 2:
            default: break; */
      }
   State47:
      const String StateDescription47 =
           "CSintMethod(IntMethodClass intMethod)= CSMethodProperties(MethodClass method), \"(\", formalParameters?, ►\")\";\r\n"
         + "formalParameters= formalParameters, ►Comma, formalParameter;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Comma)
         goto AcceptState32;
      if (ParserInput != LexerResult.GroupEnd)
      {
         if (ErrorHandler(47, StateDescription47, ParserInput))
            goto State47;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.GroupEnd);
      goto AcceptReduce46;

   State34:
      const String StateDescription34 =
           "formalParameter= Name(UnifiedString typeString), Name(UnifiedString nameString)●;\r\n"
         + "formalParameter= Name(UnifiedString ParameterModifierOptString), Name(UnifiedString typeString), ►Name(UnifiedString nameString);\r\n"
         + "formalParameter= Name(UnifiedString ParameterModifierOptString), Name(UnifiedString typeString), ►\"?\", Name(UnifiedString nameString);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Name)
      {
         Lexer.AcceptSymbol();
         // Reduce38:
         /* aAdjust: -3
          * formalParameter= Name(UnifiedString ParameterModifierOptString), Name(UnifiedString typeString), Name(UnifiedString nameString);◄ */

         FormalParameterWithModifierTypeAndName(
            ParameterModifierOptString: _a.PeekRef(-2)._UnifiedString,
            typeString: _a.PeekRef(-1)._UnifiedString,
            nameString: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove(3);
         goto Branch10;
      }
      if (ParserInput == LexerResult.Questionmark)
      {
         Lexer.AcceptSymbol();
         goto State35;
      }
      if (ParserInput != LexerResult.Comma
         && ParserInput != LexerResult.GroupEnd)
      {
         if (ErrorHandler(34, StateDescription34, ParserInput))
            goto State34;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Comma
         || ParserInput == LexerResult.GroupEnd);
      // Reduce37:
      /* aAdjust: -2
       * formalParameter= Name(UnifiedString typeString), Name(UnifiedString nameString);◄ */

      FormalParameterWithTypeAndName(
         typeString: _a.PeekRef(-1)._UnifiedString,
         nameString: _a.PeekRef(0)._UnifiedString
         );

      _a.Remove(2);
      goto Branch10;

   State35:
      const String StateDescription35 =
           "formalParameter= Name(UnifiedString ParameterModifierOptString), Name(UnifiedString typeString), \"?\", ►Name(UnifiedString nameString);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Name)
      {
         if (ErrorHandler(35, StateDescription35, ParserInput))
            goto State35;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name);
      Lexer.AcceptSymbol();
      // Reduce39:
      /* aAdjust: -3
       * formalParameter= Name(UnifiedString ParameterModifierOptString), Name(UnifiedString typeString), "?", Name(UnifiedString nameString);◄ */

      FormalParameterWithModifierNullableTypeAndName(
         ParameterModifierOptString: _a.PeekRef(-2)._UnifiedString,
         typeString: _a.PeekRef(-1)._UnifiedString,
         nameString: _a.PeekRef(0)._UnifiedString
         );

      _a.Remove(3);
      goto Branch10;

   State39:
      const String StateDescription39 =
           "CSAttribute= \"[\", OptCSAttributeChars, ►\"]\";\r\n"
         + "CSAttribute= \"[\", OptCSAttributeChars, ►CSAttribute, OptCSAttributeChars, \"]\";\r\n"
         + "OptCSAttributeChars= OptCSAttributeChars, ►CSAttributeChar;";
      // *Push(1)
      ParserInput = Lexer.PeekSymbol();
      switch (ParserInput)
      {
         // <= LexerResult.Percent
         // >= LexerResult.DefinitionSeparatorSymbol: goto AcceptReduce42; // see end of switch
         case LexerResult.Error:
         case LexerResult.Minus:
         case LexerResult.StarEqual:
         case LexerResult.MinusEqual:
         case LexerResult.Questionmark:
         case LexerResult.Asterisk:
         case LexerResult.Plus:
         case LexerResult.Comma:
         case LexerResult.NumberSign:
         case LexerResult.GroupStart:
         case LexerResult.RepeatStart:
         case LexerResult.DoubleQuestionmark:
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
            goto AcceptReduce42;
         case LexerResult.CSharpEnd:
         case LexerResult.CSharpStart:
            {
               if (ErrorHandler(39, StateDescription39, ParserInput))
                  goto State39;
               goto EndWithError;
            }
         case LexerResult.Number:
         case LexerResult.Name:
         case LexerResult.LexerString:
            {
               Lexer.AcceptSymbol();
               // Reduce43:
               /* aAdjust: -1
                * CSAttributeChar= Number(Int64 value);◄
                * or: CSAttributeChar= Name(UnifiedString string);◄
                * or: CSAttributeChar= LexerString(UnifiedString string);◄
                * then: OptCSAttributeChars= OptCSAttributeChars, CSAttributeChar;◄ */
               _a.Remove(1);
               goto State39;
            }
         case LexerResult.OptionStart:
            {
               Lexer.AcceptSymbol();
               // PushState4:
               _s.Push(1);
               goto State39;
            }
         case LexerResult.OptionEnd:
            {
               Lexer.AcceptSymbol();
               // PushState5:
               _s.Push(1);
               goto Reduce44;
            }
      } // end of switch
      Debug.Assert(ParserInput <= LexerResult.Percent || ParserInput >= LexerResult.DefinitionSeparatorSymbol);

   AcceptReduce42:
      Lexer.AcceptSymbol();
      // Reduce42:
      /* OptCSAttributeChars= OptCSAttributeChars, CSAttributeChar;◄ */
      goto State39;

   State41:
      const String StateDescription41 =
           "CSAttribute= \"[\", OptCSAttributeChars, CSAttribute, OptCSAttributeChars, ►\"]\";\r\n"
         + "OptCSAttributeChars= OptCSAttributeChars, ►CSAttributeChar;";
      ParserInput = Lexer.PeekSymbol();
      switch (ParserInput)
      {
         // <= LexerResult.Percent
         // >= LexerResult.DefinitionSeparatorSymbol: goto AcceptState41; // see end of switch
         case LexerResult.Error:
         case LexerResult.Minus:
         case LexerResult.StarEqual:
         case LexerResult.MinusEqual:
         case LexerResult.Questionmark:
         case LexerResult.Asterisk:
         case LexerResult.Plus:
         case LexerResult.Comma:
         case LexerResult.NumberSign:
         case LexerResult.GroupStart:
         case LexerResult.RepeatStart:
         case LexerResult.DoubleQuestionmark:
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
            goto AcceptState41;
         case LexerResult.CSharpEnd:
         case LexerResult.OptionStart:
         case LexerResult.CSharpStart:
            {
               if (ErrorHandler(41, StateDescription41, ParserInput))
                  goto State41;
               goto EndWithError;
            }
         case LexerResult.Number:
         case LexerResult.Name:
         case LexerResult.LexerString:
            {
               Lexer.AcceptSymbol();
               // Reduce45:
               /* aAdjust: -1
                * CSAttributeChar= Number(Int64 value);◄
                * or: CSAttributeChar= Name(UnifiedString string);◄
                * or: CSAttributeChar= LexerString(UnifiedString string);◄ */
               _a.Remove(1);
               goto State41;
            }
         case LexerResult.OptionEnd:
            {
               Lexer.AcceptSymbol();
               goto Reduce44;
            }
      } // end of switch
      Debug.Assert(ParserInput <= LexerResult.Percent || ParserInput >= LexerResult.DefinitionSeparatorSymbol);

   AcceptState41:
      Lexer.AcceptSymbol();
      goto State41;

   AcceptState42:
      Lexer.AcceptSymbol();
   State42:
      const String StateDescription42 =
           "PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= \"??\", ►signedNumber(Int64 constPriority), \"??\";\r\n"
         + "PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= \"??\", ►CSharpStart, CSintMethod(IntMethodClass intMethod), CSharpEnd, \"??\"?;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Minus)
      {
         Lexer.AcceptSymbol();
         goto State52;
      }
      if (ParserInput == LexerResult.Plus)
      {
         Lexer.AcceptSymbol();
         goto State50;
      }
      if (ParserInput == LexerResult.Number)
         goto AcceptState51;
      if (ParserInput != LexerResult.CSharpStart)
      {
         if (ErrorHandler(42, StateDescription42, ParserInput))
            goto State42;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.CSharpStart);
      Lexer.AcceptSymbol();
   State43:
      const String StateDescription43 =
           "PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= \"??\", CSharpStart, ►CSintMethod(IntMethodClass intMethod), CSharpEnd, \"??\"?;";
      _s.Push(2);
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Name)
         goto State24;
      if (ParserInput != LexerResult.OptionStart)
      {
         if (ErrorHandler(43, StateDescription43, ParserInput))
         {
            _s.Remove(1);
            goto State43;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.OptionStart);
      Lexer.AcceptSymbol();
      goto State39;

   State50:
      const String StateDescription50 =
           "signedNumber(Int64 value)= \"+\", ►Number(Int64 value);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Number)
      {
         if (ErrorHandler(50, StateDescription50, ParserInput))
            goto State50;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Number);
   AcceptState51:
      Lexer.AcceptSymbol();
   State51:
      const String StateDescription51 =
           "PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= \"??\", signedNumber(Int64 constPriority), ►\"??\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.DoubleQuestionmark)
      {
         if (ErrorHandler(51, StateDescription51, ParserInput))
            goto State51;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.DoubleQuestionmark);
      Lexer.AcceptSymbol();
      // Reduce48:
      /* aAdjust: 1
       * PriorityDeclaration(Int64 constPriority, IntMethodClass dynamicPriority)= "??", signedNumber(Int64 constPriority), "??";◄ */
      _a.Allocate(1);

      ConstantPriorityGiven(
         dynamicPriority: out _a.PeekRef(0)._IntMethodClass
         );

      goto State14;

   State52:
      const String StateDescription52 =
           "signedNumber(Int64 value)= \"-\", ►Number(Int64 value);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Number)
      {
         if (ErrorHandler(52, StateDescription52, ParserInput))
            goto State52;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Number);
      Lexer.AcceptSymbol();
      // Reduce49:
      /* signedNumber(Int64 value)= "-", Number(Int64 value);◄ */

      NegateNumber(
         value: ref _a.PeekRef(0)._Int64
         );

      goto State51;

   AcceptState53:
      Lexer.AcceptSymbol();
      // State53:
      /* RepeatedElement(Symbol Symbol)= "{", ►NestedElement(Symbol Symbol), "}"; */
      _s.Push(0);
      goto Reduce50;

   State55:
      const String StateDescription55 =
           "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions●;\r\n"
         + "NestedDefinitions= EndOfDefinition, ►\"|\", NestedDefinitionList;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
         goto AcceptState56;
      if (ParserInput <= LexerResult.CSharpStart
         || ParserInput >= LexerResult.Name)
      {
         if (ErrorHandler(55, StateDescription55, ParserInput))
            goto State55;
         goto EndWithError;
      }
      Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
      goto Reduce53;

   AcceptState56:
      Lexer.AcceptSymbol();
   State56:
      const String StateDescription56 =
           "NestedDefinitions= EndOfDefinition, \"|\", ►NestedDefinitionList;";
      _s.Push(7);
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.RepeatStart)
         goto AcceptState53;
      if (ParserInput == LexerResult.GroupStart)
         goto AcceptState68;
      if (ParserInput == LexerResult.OptionStart)
         goto AcceptState66;
      if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
      {
         if (ErrorHandler(56, StateDescription56, ParserInput))
         {
            _s.Remove(1);
            goto State56;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
      goto AcceptState76;

   State57:
      const String StateDescription57 =
           "NestedDefinitions= EndOfDefinition, \"|\", NestedDefinitionList●;\r\n"
         + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
         goto AcceptState58;
      if (ParserInput <= LexerResult.CSharpStart
         || ParserInput >= LexerResult.Name)
      {
         if (ErrorHandler(57, StateDescription57, ParserInput))
            goto State57;
         goto EndWithError;
      }
      Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
      // Reduce56:
      /* sAdjust: -1
       * NestedDefinitions= EndOfDefinition, "|", NestedDefinitionList;◄ */
      _s.Remove(1);
      // Branch13:
      if (_s.Peek() == 6)
         goto Reduce53;
      goto Reduce61;

   AcceptState58:
      Lexer.AcceptSymbol();
   State58:
      const String StateDescription58 =
           "NestedDefinitionList= NestedDefinitionList, \"|\", ►Definition;";
      _s.Push(8);
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.RepeatStart)
         goto AcceptState53;
      if (ParserInput == LexerResult.GroupStart)
         goto AcceptState68;
      if (ParserInput == LexerResult.OptionStart)
         goto AcceptState66;
      if (ParserInput != LexerResult.Name && ParserInput != LexerResult.LexerString)
      {
         if (ErrorHandler(58, StateDescription58, ParserInput))
         {
            _s.Remove(1);
            goto State58;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name || ParserInput == LexerResult.LexerString);
      goto AcceptState76;

   State60:
      const String StateDescription60 =
           "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedDefinitions●;\r\n"
         + "NestedDefinitionList= NestedDefinitionList, ►\"|\", Definition;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
         goto AcceptState58;
      if (ParserInput <= LexerResult.CSharpStart
         || ParserInput >= LexerResult.Name)
      {
         if (ErrorHandler(60, StateDescription60, ParserInput))
            goto State60;
         goto EndWithError;
      }
      Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
      goto Reduce53;

   State61:
      const String StateDescription61 =
           "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►NestedDefinitions;";
      _s.Push(9);
      ParserInput = Lexer.PeekSymbol();
      switch (ParserInput)
      {
         // <= LexerResult.NumberSign
         // >= LexerResult.TerminatorSymbol: goto HandleError61; // see end of switch
         case LexerResult.GroupStart:
            goto AcceptState68;
         case LexerResult.OptionStart:
            goto AcceptState66;
         case LexerResult.RepeatStart:
            goto AcceptState53;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState42;
         case LexerResult.CSharpStart:
            goto AcceptState23;
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
         case LexerResult.DefinitionSeparatorSymbol:
            // Reduce60:
            {
               /* EndOfDefinitionWithoutSemantics= ;◄ */

               EndOfDefinitionWithoutSemanticsRecognized();

               goto State62;
            }
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto AcceptState76;
      } // end of switch
      Debug.Assert(ParserInput <= LexerResult.NumberSign || ParserInput >= LexerResult.TerminatorSymbol);

      if (ErrorHandler(61, StateDescription61, ParserInput))
      {
         _s.Remove(1);
         goto State61;
      };
      goto EndWithError;

   State62:
      const String StateDescription62 =
           "NestedGrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), NestedDefinitions●;\r\n"
         + "NestedDefinitions= EndOfDefinition, ►\"|\", NestedDefinitionList;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
         goto AcceptState56;
      if (ParserInput <= LexerResult.CSharpStart
         || ParserInput >= LexerResult.Name)
      {
         if (ErrorHandler(62, StateDescription62, ParserInput))
            goto State62;
         goto EndWithError;
      }
      Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
      goto Reduce61;

   State64:
      const String StateDescription64 =
           "NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= \"Name(Attributes)\"(UnifiedString name, Int32 NumberOfAttributes), ►\"=\";\r\n"
         + "SimpleElement(Symbol Symbol)= \"Name(Attributes)\"(UnifiedString name, Int32 NumberOfAttributes)●;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.DefiningSymbol)
      {
         Lexer.AcceptSymbol();
         // Reduce63:
         /* NestedLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes), "=";◄ */

         LeftSideOfNestedProduction(
            SymbolAtLeftSide: out _a.PeekRef(-1)._Symbol,
            name: _a.PeekClear(-1)._UnifiedString,
            NumberOfAttributes: _a.PeekRef(0)._Int32
            );

         goto State61;
      }
      if (_is(_fColon | _fPercent | _fCSharpEnd | _fError | _fMinus | _fNumber | _fStarEqual | _fMinusEqual | _fNumberSign | _fTerminatorSymbol))
      {
         if (ErrorHandler(64, StateDescription64, ParserInput))
            goto State64;
         goto EndWithError;
      }
      Debug.Assert(!_is(_fDefiningSymbol | _fColon | _fPercent | _fCSharpEnd | _fError | _fMinus | _fNumber | _fStarEqual | _fMinusEqual | _fNumberSign
             | _fTerminatorSymbol));
      goto Reduce12;

   State65:
      const String StateDescription65 =
           "RepeatedElement(Symbol Symbol)= \"{\", NestedElement(Symbol Symbol), ►\"}\";";
      Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
      if (ParserInput != LexerResult.RepeatEnd)
      {
         if (ErrorHandler(65, StateDescription65, ParserInput))
            goto State65;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.RepeatEnd);
      Lexer.AcceptSymbol();
      // Reduce64:
      /* sAdjust: -1
       * RepeatedElement(Symbol Symbol)= "{", NestedElement(Symbol Symbol), "}";◄ */
      _s.Remove(1);

      RepeatGroupRecognized(
         Symbol: ref _a.PeekRef(0)._Symbol
         );

      goto Branch3;

   AcceptState66:
      Lexer.AcceptSymbol();
      // State66:
      /* RepeatedElement(Symbol Symbol)= "[", ►NestedElement(Symbol Symbol), "]"; */
      _s.Push(1);
      goto Reduce50;

   State67:
      const String StateDescription67 =
           "RepeatedElement(Symbol Symbol)= \"[\", NestedElement(Symbol Symbol), ►\"]\";";
      Debug.Assert(ParserInput >= LexerResult.GroupEnd && ParserInput <= LexerResult.OptionEnd);
      if (ParserInput != LexerResult.OptionEnd)
      {
         if (ErrorHandler(67, StateDescription67, ParserInput))
            goto State67;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.OptionEnd);
      Lexer.AcceptSymbol();
      // Reduce65:
      /* sAdjust: -1
       * RepeatedElement(Symbol Symbol)= "[", NestedElement(Symbol Symbol), "]";◄ */
      _s.Remove(1);

      OptionGroupRecognized(
         Symbol: ref _a.PeekRef(0)._Symbol
         );

      goto Branch3;

   AcceptState68:
      Lexer.AcceptSymbol();
      // State68:
      /* SimpleElement(Symbol Symbol)= "(", ►NestedElement(Symbol Symbol), ")"; */
      _s.Push(2);
      goto Reduce50;

   State71:
      const String StateDescription71 =
           "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►\"=\", outerDefinitions;\r\n"
         + "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), ►\"-=\", ListOfExcludedTerminalSymbols, \";\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.MinusEqual)
      {
         Lexer.AcceptSymbol();
         goto State72;
      }
      if (ParserInput != LexerResult.DefiningSymbol)
      {
         if (ErrorHandler(71, StateDescription71, ParserInput))
            goto State71;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.DefiningSymbol);
      Lexer.AcceptSymbol();
   State75:
      const String StateDescription75 =
           "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"=\", ►outerDefinitions;";
      _s.Push(11);
      ParserInput = Lexer.PeekSymbol();
      switch (ParserInput)
      {
         // <= LexerResult.NumberSign: goto HandleError75; // see end of switch
         case LexerResult.GroupEnd:
         case LexerResult.RepeatEnd:
         case LexerResult.OptionEnd:
            goto HandleError75;
         case LexerResult.GroupStart:
            goto AcceptState68;
         case LexerResult.OptionStart:
            goto AcceptState66;
         case LexerResult.RepeatStart:
            goto AcceptState53;
         case LexerResult.DoubleQuestionmark:
            goto AcceptState42;
         case LexerResult.CSharpStart:
            goto AcceptState23;
         case LexerResult.Name:
         case LexerResult.LexerString:
            goto AcceptState76;
            // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce11; // see end of switch
      } // end of switch
      if (ParserInput >= LexerResult.DefinitionSeparatorSymbol)
         goto Reduce11;
      Debug.Assert(ParserInput <= LexerResult.NumberSign);

   HandleError75:
      if (ErrorHandler(75, StateDescription75, ParserInput))
      {
         _s.Remove(1);
         goto State75;
      };
      goto EndWithError;

   State72:
      const String StateDescription72 =
           "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"-=\", ►ListOfExcludedTerminalSymbols, \";\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Name)
      {
         if (ErrorHandler(72, StateDescription72, ParserInput))
            goto State72;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name);
      Lexer.AcceptSymbol();
      // Reduce70:
      /* aAdjust: -1
       * ListOfExcludedTerminalSymbols= Name(UnifiedString terminalName);◄ */

      FirstExcludedTerminalSymbol(
         terminalName: _a.PeekRef(0)._UnifiedString
         );

      _a.Remove(1);
   State73:
      const String StateDescription73 =
           "GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), \"-=\", ListOfExcludedTerminalSymbols, ►\";\";\r\n"
         + "ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, ►\"|\", Name(UnifiedString name);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.DefinitionSeparatorSymbol)
      {
         Lexer.AcceptSymbol();
         goto State74;
      }
      if (ParserInput != LexerResult.TerminatorSymbol)
      {
         if (ErrorHandler(73, StateDescription73, ParserInput))
            goto State73;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.TerminatorSymbol);
      Lexer.AcceptSymbol();
      // Reduce71:
      /* GrammarRule(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes)= outerLeftSide(Symbol SymbolAtLeftSide, Int32 NumberOfAttributes), "-=", ListOfExcludedTerminalSymbols, ";";◄ */

      EndOfListOfExcludedTerminalSymbols();

      goto Reduce69;

   State74:
      const String StateDescription74 =
           "ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, \"|\", ►Name(UnifiedString name);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Name)
      {
         if (ErrorHandler(74, StateDescription74, ParserInput))
            goto State74;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name);
      Lexer.AcceptSymbol();
      // Reduce72:
      /* aAdjust: -1
       * ListOfExcludedTerminalSymbols= ListOfExcludedTerminalSymbols, "|", Name(UnifiedString name);◄ */

      OneMoreExcludedTerminalSymbol(
         name: _a.PeekRef(0)._UnifiedString
         );

      _a.Remove(1);
      goto State73;

   State77:
      const String StateDescription77 =
           "\"(Attributes)\"(Int32 numberOfAttributes)= \"(\", ►\")\";\r\n"
         + "\"(Attributes)\"(Int32 numberOfAttributes)= \"(\", ►\"Attributes)\"(Int32 numberOfAttributes, Int32 smallestNumber);";
      // *Push(0)
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Name)
      {
         Lexer.AcceptSymbol();
         // PushState7:
         _s.Push(0);
         goto State80;
      }
      if (ParserInput != LexerResult.GroupEnd)
      {
         if (ErrorHandler(77, StateDescription77, ParserInput))
            goto State77;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.GroupEnd);
      Lexer.AcceptSymbol();
      // Reduce76:
      /* aAdjust: 1
       * "(Attributes)"(Int32 numberOfAttributes)= "(", ")";◄ */
      _a.Allocate(1);

      EmptyListOfAttributes(
         numberOfAttributes: out _a.PeekRef(0)._Int32
         );

   Branch16:
      if (_s.Peek() == 0)
      // Reduce75:
      {
         /* sAdjust: -1
          * "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes)= NameOrString(UnifiedString name), "(Attributes)"(Int32 NumberOfAttributes);◄ */
         _s.Remove(1);
         goto Branch15;
      }
      // Reduce82:
      /* sAdjust: -1
       * "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes)= NameOrString(UnifiedString name), "(Attributes)"(Int32 NumberOfAttributes);◄ */
      _s.Remove(1);
      goto State3;

   State78:
      const String StateDescription78 =
           "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 numberBeforeGroup), ►Comma, \"Attributes)\"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup);\r\n"
         + "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 number), ►\")\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Comma)
      {
         Lexer.AcceptSymbol();
         goto State79;
      }
      if (ParserInput != LexerResult.GroupEnd)
      {
         if (ErrorHandler(78, StateDescription78, ParserInput))
            goto State78;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.GroupEnd);
      Lexer.AcceptSymbol();
      // Reduce78:
      /* aAdjust: 1
       * "Attributes)"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 number), ")";◄ */
      _a.Allocate(1);

      FirstAttributeOfGroup(
         numberOfAttributesOfGroup: out _a.PeekRef(-1)._Int32,
         smallestNumber: out _a.PeekRef(0)._Int32,
         number: _a.PeekRef(-1)._Int32
         );

   Branch17:
      if (_s.Peek() == 1)
      // Reduce79:
      {
         /* sAdjust: -1, aAdjust: -1
          * "Attributes)"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 numberBeforeGroup), Comma, "Attributes)"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup);◄ */
         _s.Remove(1);

         AnotherAttributeOfGroup(
            numberOfAttributesOfGroup: out _a.PeekRef(-2)._Int32,
            smallestNumber: out _a.PeekRef(-1)._Int32,
            numberBeforeGroup: _a.PeekRef(-2)._Int32,
            numberOfAttributesOfRightGroup: _a.PeekRef(-1)._Int32,
            smallestNumberOfRightGroup: _a.PeekRef(0)._Int32
            );

         _a.Remove(1);
         goto Branch17;
      }
      // Reduce77:
      /* sAdjust: -1, aAdjust: -1
       * "(Attributes)"(Int32 numberOfAttributes)= "(", "Attributes)"(Int32 numberOfAttributes, Int32 smallestNumber);◄ */
      _s.Remove(1);
      _a.Remove(1);
      goto Branch16;

   State79:
      const String StateDescription79 =
           "\"Attributes)\"(Int32 numberOfAttributesOfGroup, Int32 smallestNumber)= Attribut(Int32 numberBeforeGroup), Comma, ►\"Attributes)\"(Int32 numberOfAttributesOfRightGroup, Int32 smallestNumberOfRightGroup);";
      _s.Push(1);
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Name)
      {
         if (ErrorHandler(79, StateDescription79, ParserInput))
         {
            _s.Remove(1);
            goto State79;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name);
      Lexer.AcceptSymbol();
   State80:
      const String StateDescription80 =
           "Attribut(Int32 number)= Name(UnifiedString typeString), ►Name(UnifiedString nameString);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Name)
      {
         if (ErrorHandler(80, StateDescription80, ParserInput))
            goto State80;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name);
      Lexer.AcceptSymbol();
      // Reduce80:
      /* aAdjust: -1
       * Attribut(Int32 number)= Name(UnifiedString typeString), Name(UnifiedString nameString);◄ */

      AttributeTypeAndName(
         number: out _a.PeekRef(-1)._Int32,
         typeString: _a.PeekClear(-1)._UnifiedString,
         nameString: _a.PeekRef(0)._UnifiedString
         );

      _a.Remove(1);
      goto State78;

   State81:
      const String StateDescription81 =
           "GrammlatorSetting= Name(UnifiedString name), ►\":\", LexerString(UnifiedString value), \";\";\r\n"
         + "GrammlatorSetting= Name(UnifiedString name), ►\":\", Number(Int64 value), \";\";\r\n"
         + "GrammlatorSetting= Name(UnifiedString name), ►\":\", Name(UnifiedString value), \";\";\r\n"
         + "\"Name(Attributes)\"(UnifiedString name, Int32 NumberOfAttributes)= NameOrString(UnifiedString name), ►\"(Attributes)\"(Int32 NumberOfAttributes);\r\n"
         + "\"Name(Attributes)\"(UnifiedString name, Int32 NumberOfAttributes)= NameOrString(UnifiedString name)●;";
      // *Push(1)
      ParserInput = Lexer.PeekSymbol();
      switch (ParserInput)
      {
         // <= LexerResult.DefiningSymbol
         // >= LexerResult.DefinitionSeparatorSymbol: goto Reduce81; // see end of switch
         case LexerResult.Percent:
         case LexerResult.StarEqual:
         case LexerResult.CSharpStart:
            goto Reduce81;
         case LexerResult.Colon:
            {
               Lexer.AcceptSymbol();
               // PushState8:
               _s.Push(1);
               goto State82;
            }
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
            {
               if (ErrorHandler(81, StateDescription81, ParserInput))
                  goto State81;
               goto EndWithError;
            }
         case LexerResult.GroupStart:
            {
               Lexer.AcceptSymbol();
               // PushState9:
               _s.Push(1);
               goto State77;
            }
      } // end of switch
      Debug.Assert(ParserInput <= LexerResult.DefiningSymbol || ParserInput >= LexerResult.DefinitionSeparatorSymbol);

   Reduce81:
      /* aAdjust: 1
       * "Name(Attributes)"(UnifiedString name, Int32 NumberOfAttributes)= NameOrString(UnifiedString name);◄ */
      _a.Allocate(1);

      NameWithoutAttributes(
         NumberOfAttributes: out _a.PeekRef(0)._Int32
         );

      goto State3;

   State82:
      const String StateDescription82 =
           "GrammlatorSetting= Name(UnifiedString name), \":\", ►LexerString(UnifiedString value), \";\";\r\n"
         + "GrammlatorSetting= Name(UnifiedString name), \":\", ►Number(Int64 value), \";\";\r\n"
         + "GrammlatorSetting= Name(UnifiedString name), \":\", ►Name(UnifiedString value), \";\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Name)
      {
         Lexer.AcceptSymbol();
         goto State84;
      }
      if (ParserInput == LexerResult.LexerString)
      {
         Lexer.AcceptSymbol();
         goto State83;
      }
      if (ParserInput != LexerResult.Number)
      {
         if (ErrorHandler(82, StateDescription82, ParserInput))
            goto State82;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Number);
      Lexer.AcceptSymbol();
   State85:
      const String StateDescription85 =
           "GrammlatorSetting= Name(UnifiedString name), \":\", Number(Int64 value), ►\";\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.TerminatorSymbol)
      {
         if (ErrorHandler(85, StateDescription85, ParserInput))
            goto State85;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.TerminatorSymbol);
      Lexer.AcceptSymbol();
      // Reduce85:
      /* sAdjust: -2, aAdjust: -2
       * GrammlatorSetting= Name(UnifiedString name), ":", Number(Int64 value), ";";◄
       * then: OptionalGrammlatorSettings= OptionalGrammlatorSettings, GrammlatorSetting;◄ */
      _s.Remove(2);

      SetGrammlatorInt32Setting(
         name: _a.PeekRef(-1)._UnifiedString,
         value: _a.PeekRef(0)._Int64
         );

      _a.Remove(2);
      goto State2;

   State83:
      const String StateDescription83 =
           "GrammlatorSetting= Name(UnifiedString name), \":\", LexerString(UnifiedString value), ►\";\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.TerminatorSymbol)
      {
         if (ErrorHandler(83, StateDescription83, ParserInput))
            goto State83;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.TerminatorSymbol);
      Lexer.AcceptSymbol();
      // Reduce83:
      /* sAdjust: -2, aAdjust: -2
       * GrammlatorSetting= Name(UnifiedString name), ":", LexerString(UnifiedString value), ";";◄
       * then: OptionalGrammlatorSettings= OptionalGrammlatorSettings, GrammlatorSetting;◄ */
      _s.Remove(2);

      SetGrammlatorStringSetting(
         name: _a.PeekRef(-1)._UnifiedString,
         value: _a.PeekRef(0)._UnifiedString
         );

      _a.Remove(2);
      goto State2;

   State84:
      const String StateDescription84 =
           "GrammlatorSetting= Name(UnifiedString name), \":\", Name(UnifiedString value), ►\";\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.TerminatorSymbol)
      {
         if (ErrorHandler(84, StateDescription84, ParserInput))
            goto State84;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.TerminatorSymbol);
      Lexer.AcceptSymbol();
      // Reduce84:
      /* sAdjust: -2, aAdjust: -2
       * GrammlatorSetting= Name(UnifiedString name), ":", Name(UnifiedString value), ";";◄
       * then: OptionalGrammlatorSettings= OptionalGrammlatorSettings, GrammlatorSetting;◄ */
      _s.Remove(2);

      SetGrammlatorNameSetting(
         name: _a.PeekRef(-1)._UnifiedString,
         value: _a.PeekRef(0)._UnifiedString
         );

      _a.Remove(2);
      goto State2;

   State87:
      const String StateDescription87 =
           "CSEnumDeclaration= CSEnumProperties, ►optionalBaseType, CSEnumMembers;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Colon)
      {
         Lexer.AcceptSymbol();
         goto State101;
      }
      if (ParserInput != LexerResult.RepeatStart)
      {
         if (ErrorHandler(87, StateDescription87, ParserInput))
            goto State87;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.RepeatStart);
   State88:
      const String StateDescription88 =
           "CSEnumDeclaration= CSEnumProperties, optionalBaseType, ►CSEnumMembers;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.RepeatStart)
      {
         if (ErrorHandler(88, StateDescription88, ParserInput))
            goto State88;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.RepeatStart);
      Lexer.AcceptSymbol();
   State89:
      const String StateDescription89 =
           "CSEnumMembers= \"{\", ►\"}\";\r\n"
         + "CSEnumMembers= \"{\", ►CSEnumMemberList, \"}\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.RepeatEnd)
      {
         Lexer.AcceptSymbol();
         // Reduce88:
         /* CSEnumMembers= "{", "}";◄ */

         EmptyEnumRecognized();

         goto Reduce86;
      }
      if (ParserInput != LexerResult.OptionStart
         && ParserInput != LexerResult.Name)
      {
         if (ErrorHandler(89, StateDescription89, ParserInput))
            goto State89;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.OptionStart
         || ParserInput == LexerResult.Name);
      // Reduce87:
      /* ResetEnumDefaults= ;◄ */

      ResetEnumDefaults();

      // State90:
      /* CSEnumMemberList= ResetEnumDefaults, ►CSEnumMember; */
      _s.Push(0);
      Debug.Assert(ParserInput == LexerResult.OptionStart
         || ParserInput == LexerResult.Name);
      if (ParserInput == LexerResult.Name)
         goto Reduce89;
      Debug.Assert(ParserInput == LexerResult.OptionStart);
   AcceptState94:
      Lexer.AcceptSymbol();
   State94:
      const String StateDescription94 =
           "OptionalDescriptionAttribute(String description)= \"[\", ►Name(UnifiedString attributeIdentifier), \"(\", LexerString(UnifiedString descriptionString), \")\", \"]\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Name)
      {
         if (ErrorHandler(94, StateDescription94, ParserInput))
            goto State94;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name);
      Lexer.AcceptSymbol();
   State95:
      const String StateDescription95 =
           "OptionalDescriptionAttribute(String description)= \"[\", Name(UnifiedString attributeIdentifier), ►\"(\", LexerString(UnifiedString descriptionString), \")\", \"]\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.GroupStart)
      {
         if (ErrorHandler(95, StateDescription95, ParserInput))
            goto State95;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.GroupStart);
      Lexer.AcceptSymbol();
   State96:
      const String StateDescription96 =
           "OptionalDescriptionAttribute(String description)= \"[\", Name(UnifiedString attributeIdentifier), \"(\", ►LexerString(UnifiedString descriptionString), \")\", \"]\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.LexerString)
      {
         if (ErrorHandler(96, StateDescription96, ParserInput))
            goto State96;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.LexerString);
      Lexer.AcceptSymbol();
   State97:
      const String StateDescription97 =
           "OptionalDescriptionAttribute(String description)= \"[\", Name(UnifiedString attributeIdentifier), \"(\", LexerString(UnifiedString descriptionString), ►\")\", \"]\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.GroupEnd)
      {
         if (ErrorHandler(97, StateDescription97, ParserInput))
            goto State97;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.GroupEnd);
      Lexer.AcceptSymbol();
   State98:
      const String StateDescription98 =
           "OptionalDescriptionAttribute(String description)= \"[\", Name(UnifiedString attributeIdentifier), \"(\", LexerString(UnifiedString descriptionString), \")\", ►\"]\";";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.OptionEnd)
      {
         if (ErrorHandler(98, StateDescription98, ParserInput))
            goto State98;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.OptionEnd);
      Lexer.AcceptSymbol();
      // Reduce93:
      /* aAdjust: -1
       * OptionalDescriptionAttribute(String description)= "[", Name(UnifiedString attributeIdentifier), "(", LexerString(UnifiedString descriptionString), ")", "]";◄ */

      DescriptionAttribute(
         description: out _a.PeekRef(-1)._String,
         attributeIdentifier: _a.PeekClear(-1)._UnifiedString,
         descriptionString: _a.PeekRef(0)._UnifiedString
         );

      _a.Remove(1);
      goto State91;

   State93:
      const String StateDescription93 =
           "OptionalEnumElementNumber(Int64 enumNumber)= \"=\", ►Number(Int64 enumNumber);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Number)
      {
         if (ErrorHandler(93, StateDescription93, ParserInput))
            goto State93;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Number);
      Lexer.AcceptSymbol();
      goto Reduce92;

   State100:
      const String StateDescription100 =
           "CSEnumMemberList= CSEnumMemberList, Comma, ►CSEnumMember;";
      _s.Push(1);
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Name)
         goto Reduce89;
      if (ParserInput != LexerResult.OptionStart)
      {
         if (ErrorHandler(100, StateDescription100, ParserInput))
         {
            _s.Remove(1);
            goto State100;
         };
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.OptionStart);
      goto AcceptState94;

   State101:
      const String StateDescription101 =
           "optionalBaseType= \":\", ►Name(UnifiedString Ignored);";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Name)
      {
         if (ErrorHandler(101, StateDescription101, ParserInput))
            goto State101;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name);
      Lexer.AcceptSymbol();
      // Reduce94:
      /* aAdjust: -1
       * optionalBaseType= ":", Name(UnifiedString Ignored);◄ */
      _a.Remove(1);
      goto State88;

   State103:
      const String StateDescription103 =
           "CSEnumProperties= Name(UnifiedString modifier1StringIndex), ►Name(UnifiedString modifier2StringIndex), Name(UnifiedString enumStringIndex), CSEnumName;\r\n"
         + "CSEnumProperties= Name(UnifiedString modifierStringIndex), ►Name(UnifiedString enumStringIndex), CSEnumName;\r\n"
         + "CSEnumProperties= Name(UnifiedString enumStringIndex), ►CSEnumName;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput != LexerResult.Name)
      {
         if (ErrorHandler(103, StateDescription103, ParserInput))
            goto State103;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Name);
      Lexer.AcceptSymbol();
   State104:
      const String StateDescription104 =
           "CSEnumProperties= Name(UnifiedString modifier1StringIndex), Name(UnifiedString modifier2StringIndex), ►Name(UnifiedString enumStringIndex), CSEnumName;\r\n"
         + "CSEnumProperties= Name(UnifiedString modifierStringIndex), Name(UnifiedString enumStringIndex), ►CSEnumName;\r\n"
         + "CSEnumName= Name(UnifiedString nameString)●;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Name)
      {
         Lexer.AcceptSymbol();
         goto State105;
      }
      if (ParserInput != LexerResult.Colon
         && ParserInput != LexerResult.RepeatStart)
      {
         if (ErrorHandler(104, StateDescription104, ParserInput))
            goto State104;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Colon
         || ParserInput == LexerResult.RepeatStart);
      // Reduce96:
      /* aAdjust: -2
       * CSEnumName= Name(UnifiedString nameString);◄
       * then: CSEnumProperties= Name(UnifiedString enumStringIndex), CSEnumName;◄ */

      EnumNameRecognized(
         nameString: _a.PeekRef(0)._UnifiedString
         );

      _a.Remove(2);
      goto State87;

   State105:
      const String StateDescription105 =
           "CSEnumProperties= Name(UnifiedString modifier1StringIndex), Name(UnifiedString modifier2StringIndex), Name(UnifiedString enumStringIndex), ►CSEnumName;\r\n"
         + "CSEnumName= Name(UnifiedString nameString)●;";
      ParserInput = Lexer.PeekSymbol();
      if (ParserInput == LexerResult.Name)
      {
         Lexer.AcceptSymbol();
         // Reduce99:
         /* aAdjust: -4
          * CSEnumName= Name(UnifiedString nameString);◄
          * then: CSEnumProperties= Name(UnifiedString modifier1StringIndex), Name(UnifiedString modifier2StringIndex), Name(UnifiedString enumStringIndex), CSEnumName;◄ */

         EnumNameRecognized(
            nameString: _a.PeekRef(0)._UnifiedString
            );

         _a.Remove(4);
         goto State87;
      }
      if (ParserInput != LexerResult.Colon
         && ParserInput != LexerResult.RepeatStart)
      {
         if (ErrorHandler(105, StateDescription105, ParserInput))
            goto State105;
         goto EndWithError;
      }
      Debug.Assert(ParserInput == LexerResult.Colon
         || ParserInput == LexerResult.RepeatStart);
      // Reduce98:
      /* aAdjust: -3
       * CSEnumName= Name(UnifiedString nameString);◄
       * then: CSEnumProperties= Name(UnifiedString modifierStringIndex), Name(UnifiedString enumStringIndex), CSEnumName;◄ */

      EnumNameRecognized(
         nameString: _a.PeekRef(0)._UnifiedString
         );

      _a.Remove(3);
      goto State87;

   Reduce24:
      /* sAdjust: -1
       * outerDefinitions= EndOfDefinition, "|", outerDefinitionList;◄ */
      _s.Remove(1);
      goto Branch5;

   Reduce28:
      /* sAdjust: -2
       * outerDefinitionList= SequenceOfElements, EndOfDefinition, "|", outerDefinitionList;◄ */
      _s.Remove(2);
      // Branch7:
      switch (_s.Peek())
      {
         case 2:
            goto Reduce13;
         case 3:
            goto Reduce24;
         case 4:
            goto Reduce28;
            /*case 11:
            default: break; */
      }
      goto Reduce73;

   Reduce44:
      /* sAdjust: -1
       * CSAttribute= "[", OptCSAttributeChars, "]";◄
       * or: CSAttribute= "[", OptCSAttributeChars, CSAttribute, OptCSAttributeChars, "]";◄ */
      _s.Remove(1);
      // Branch11:
      if (_s.Peek() == 1)
         goto State41;
      goto State24;

   HandleError10:
      if (ErrorHandler(10, StateDescription10, ParserInput))
      {
         _s.Remove(1);
         goto State10;
      };
      goto EndWithError;

   HandleError18:
      if (ErrorHandler(18, StateDescription18, ParserInput))
      {
         _s.Remove(1);
         goto State18;
      };
      goto EndWithError;

   EndWithError:
      // This point is reached after an input error has been found
      _s.Remove(_s.Count - _StateStackInitialCount);
      _a.Remove(_a.Count - _AttributeStackInitialCount);
   EndOfGeneratedCode:
      ;

      #endregion grammlator generated 13 Dez. 2020 (grammlator file version/date 2020.11.09.0/13 Dez. 2020)

   }
}
