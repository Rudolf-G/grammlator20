using GrammlatorRuntime;

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Text;

namespace grammlator {
#pragma warning disable CS1591 // Fehlender XML-Kommentar für öffentlich sichtbaren Typ oder Element
   public enum LexerResult {
      // Defines the output of the lexer, which is assigned to Symbol to be used by the parser
      // The elements of LexerResult are ordered such that grammlator can
      // generate efficient code for the conditions of the parsers actions
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
   /// Grammlator lexer (uses input classifier, is used by parser)
   /// </summary>
   internal class P1bLexer : GrammlatorInputApplication<LexerResult> {

      private readonly P1cInputClassifier inputClassifier;

      public P1bLexer(SpanReaderWithCharacterAndLineCounter sourceReader,
          StackOfMultiTypeElements attributeStack, Stack<Int32> stateStack)
          : base(attributeStack, stateStack)
      {
         Source = sourceReader.Source;
         inputClassifier = new P1cInputClassifier(sourceReader, attributeStack);
         LexerTextPos = 0;
         Accepted = true;
      }

      private readonly ReadOnlyMemory<Char> Source;

      /// <summary>
      /// Current Position
      /// </summary>
      public Int32 LexerTextPos {
         get; private set;
      }

      public Int32 StartOf1stGrammlatorGeneratedLine { get { return inputClassifier.StartOf1stGrammlatorGeneratedLine; } }

      /// <summary>
      /// Skips all input lines which are not grammar lines.
      /// The next symbol will be CSharpEnd.
      /// </summary>
      public void SkipToEndOfCSLines()
      {
         Accepted = true;
         inputClassifier.SkipToEndOfCSLines();
      }
     
      /// <summary>
      /// Makes the next symbol available for Phase1. If accepted==false does nothing (symbol is already available)
      /// Else computes the next Symbol, pushs its attributes to an internal stack and sets accepted to false.
      /// </summary>
      public override LexerResult PeekSymbol()
      { // Bestimmen des naechsten terminalen Symbols fuer lex2
         if (!Accepted)
            return Symbol;
         Accepted = false;
         Debug.Assert(AttributesOfSymbol.Count == 0); // must have been cleared by AcceptSymbol
         LexerTextPos = inputClassifier.InputPosition;
         // the position of the first character of the next symbol the lexer will find,
         //    might be incremented after skipping whitespace and comments

         try
         {
            Debug.Assert(AttributesOfSymbol.Count == 0); // TODO Check : after syntax errors?
            EvaluateInput(); // stores the attributes in AttributesOfSymbol
         }
         catch (ApplicationException)
         {
            throw;
         }
         return Symbol;
      }

      private Boolean ErrorHandler(Int32 LexerStateNumber, String stateDescription, ClassifierResult symbol)
      {

         var aCountBeforeAccept = _a.Count;
         LexerTextPos = inputClassifier.InputPosition;
         inputClassifier.AcceptSymbol(); // make the attribute (the character) available in the stack and to discard the classifiers result

         GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Error,
             "Lexical analysis error " + LexerStateNumber.ToString()
             + ": input character \'" + Source.Span[_a.PeekRef(0)._Int32]
             + "\' classified as \"" + symbol.MyToString() +
             "\" not allowed in state " + Environment.NewLine + stateDescription + Environment.NewLine
             , LexerTextPos
             ); // throws an exception

         _a.Remove(_a.Count - aCountBeforeAccept);  // discard the attributes of the discarded terminal symbol
         return true; // continue parsing
         ;
      }

      private readonly StringBuilder NameBuilder = new StringBuilder(30);

      private readonly LexerResult[] CharToLexerResult = new LexerResult[]
      {
            // sorted by character value
        LexerResult.Error,  // 0; U0000; Cc; NULL
        LexerResult.Error,  // 1; U0001; Cc; START OF HEADING
        LexerResult.Error,  // 2; U0002; Cc; START OF TEXT
        LexerResult.Error,  // 3; U0003; Cc; END OF TEXT
        LexerResult.Error,  // 4; U0004; Cc; END OF TRANSMISSION
        LexerResult.Error,  // 5; U0005; Cc; ENQUIRY
        LexerResult.Error,  // 6; U0006; Cc; ACKNOWLEDGE
        LexerResult.Error,  // 7; U0007; Cc; BELL
        LexerResult.Error,  // 8; U0008; Cc; BACKSPACE
        LexerResult.Error,  // 9; U0009; Cc; HorizontalTab
        LexerResult.Error,  // 10; U000A; Cc; LF
        LexerResult.Error,  // 11; U000B; Cc; VerticalTab
        LexerResult.Error,  // 12; U000C; Cc; FormFeed
        LexerResult.Error,  // 13; U000D; Cc; CR
        LexerResult.Error,  // 14; U000E; Cc; SHIFT OUT
        LexerResult.Error,  // 15; U000F; Cc; SHIFT IN
        LexerResult.Error,  // 16; U0010; Cc; DATA LINK ESCAPE
        LexerResult.Error,  // 17; U0011; Cc; DEVICE CONTROL ONE
        LexerResult.Error,  // 18; U0012; Cc; DEVICE CONTROL TWO
        LexerResult.Error,  // 19; U0013; Cc; DEVICE CONTROL THREE
        LexerResult.Error,  // 20; U0014; Cc; DEVICE CONTROL FOUR
        LexerResult.Error,  // 21; U0015; Cc; NEGATIVE ACKNOWLEDGE
        LexerResult.Error,  // 22; U0016; Cc; SYNCHRONOUS IDLE
        LexerResult.Error,  // 23; U0017; Cc; END OF TRANSMISSION BLOCK
        LexerResult.Error,  // 24; U0018; Cc; CANCEL
        LexerResult.Error,  // 25; U0019; Cc; END OF MEDIUM
        LexerResult.Error,  // 26; U001A; Cc; SUBSTITUTE
        LexerResult.Error,  // 27; U001B; Cc; ESCAPE
        LexerResult.Error,  // 28; U001C; Cc; INFORMATION SEPARATOR FOUR
        LexerResult.Error,  // 29; U001D; Cc; INFORMATION SEPARATOR THREE
        LexerResult.Error,  // 30; U001E; Cc; INFORMATION SEPARATOR TWO
        LexerResult.Error,  // 31; U001F; Cc; INFORMATION SEPARATOR ONE
        LexerResult.Error,  // 32; U0020; Zs; Space
        LexerResult.Error,  // 33; U0021; Po; !
        LexerResult.Error,  // 34; U0022; Po; "
        LexerResult.NumberSign,  // 35; U0023; Po; #
        LexerResult.Error,  // 36; U0024; Sc; $
        LexerResult.Percent,  // 37; U0025; Po; %
        LexerResult.Error,  // 38; U0026; Po; &
        LexerResult.Error,  // 39; U0027; Po; '
        LexerResult.GroupStart,  // 40; U0028; Ps; (
        LexerResult.GroupEnd,  // 41; U0029; Pe; )
        LexerResult.Asterisk,  // 42; U002A; Po; *
        LexerResult.Plus,  // 43; U002B; Sm; +
        LexerResult.Comma,  // 44; U002C; Po; ,
        LexerResult.Minus,  // 45; U002D; Pd; -
        LexerResult.Error,  // 46; U002E; Po; .
        LexerResult.Error,  // 47; U002F; Po; /
        LexerResult.Error,  // 48; U0030; Nd; 0
        LexerResult.Error,  // 49; U0031; Nd; 1
        LexerResult.Error,  // 50; U0032; Nd; 2
        LexerResult.Error,  // 51; U0033; Nd; 3
        LexerResult.Error,  // 52; U0034; Nd; 4
        LexerResult.Error,  // 53; U0035; Nd; 5
        LexerResult.Error,  // 54; U0036; Nd; 6
        LexerResult.Error,  // 55; U0037; Nd; 7
        LexerResult.Error,  // 56; U0038; Nd; 8
        LexerResult.Error,  // 57; U0039; Nd; 9
        LexerResult.Colon,  // 58; U003A; Po; :
        LexerResult.TerminatorSymbol,  // 59; U003B; Po; ;
        LexerResult.Error,  // 60; U003C; Sm; <
        LexerResult.DefiningSymbol,  // 61; U003D; Sm; =
        LexerResult.Error,  // 62; U003E; Sm; >
        LexerResult.Questionmark,  // 63; U003F; Po; ?
        LexerResult.Error,  // 64; U0040; Po; @
        LexerResult.Error,  // 65; U0041; Lu; A
        LexerResult.Error,  // 66; U0042; Lu; B
        LexerResult.Error,  // 67; U0043; Lu; C
        LexerResult.Error,  // 68; U0044; Lu; D
        LexerResult.Error,  // 69; U0045; Lu; E
        LexerResult.Error,  // 70; U0046; Lu; F
        LexerResult.Error,  // 71; U0047; Lu; G
        LexerResult.Error,  // 72; U0048; Lu; H
        LexerResult.Error,  // 73; U0049; Lu; I
        LexerResult.Error,  // 74; U004A; Lu; J
        LexerResult.Error,  // 75; U004B; Lu; K
        LexerResult.Error,  // 76; U004C; Lu; L
        LexerResult.Error,  // 77; U004D; Lu; M
        LexerResult.Error,  // 78; U004E; Lu; N
        LexerResult.Error,  // 79; U004F; Lu; O
        LexerResult.Error,  // 80; U0050; Lu; P
        LexerResult.Error,  // 81; U0051; Lu; Q
        LexerResult.Error,  // 82; U0052; Lu; R
        LexerResult.Error,  // 83; U0053; Lu; S
        LexerResult.Error,  // 84; U0054; Lu; T
        LexerResult.Error,  // 85; U0055; Lu; U
        LexerResult.Error,  // 86; U0056; Lu; V
        LexerResult.Error,  // 87; U0057; Lu; W
        LexerResult.Error,  // 88; U0058; Lu; X
        LexerResult.Error,  // 89; U0059; Lu; Y
        LexerResult.Error,  // 90; U005A; Lu; Z
        LexerResult.OptionStart,  // 91; U005B; Ps; [
        LexerResult.Error,  // 92; U005C; Po; \
        LexerResult.OptionEnd,  // 93; U005D; Pe; ]
        LexerResult.Error,  // 94; U005E; Sk; ^
        LexerResult.Error,  // 95; U005F; Pc; _
        LexerResult.Error,  // 96; U0060; Sk; `
        LexerResult.Error,  // 97; U0061; LI; a
        LexerResult.Error,  // 98; U0062; LI; b
        LexerResult.Error,  // 99; U0063; LI; c
        LexerResult.Error,  // 100; U0064; LI; d
        LexerResult.Error,  // 101; U0065; LI; e
        LexerResult.Error,  // 102; U0066; LI; f
        LexerResult.Error,  // 103; U0067; LI; g
        LexerResult.Error,  // 104; U0068; LI; h
        LexerResult.Error,  // 105; U0069; LI; i
        LexerResult.Error,  // 106; U006A; LI; j
        LexerResult.Error,  // 107; U006B; LI; k
        LexerResult.Error,  // 108; U006C; LI; l
        LexerResult.Error,  // 109; U006D; LI; m
        LexerResult.Error,  // 110; U006E; LI; n
        LexerResult.Error,  // 111; U006F; LI; o
        LexerResult.Error,  // 112; U0070; LI; p
        LexerResult.Error,  // 113; U0071; LI; q
        LexerResult.Error,  // 114; U0072; LI; r
        LexerResult.Error,  // 115; U0073; LI; s
        LexerResult.Error,  // 116; U0074; LI; t
        LexerResult.Error,  // 117; U0075; LI; u
        LexerResult.Error,  // 118; U0076; LI; v
        LexerResult.Error,  // 119; U0077; LI; w
        LexerResult.Error,  // 120; U0078; LI; x
        LexerResult.Error,  // 121; U0079; LI; y
        LexerResult.Error,  // 122; U007A; LI; z
        LexerResult.RepeatStart,  // 123; U007B; Ps; {
        LexerResult.DefinitionSeparatorSymbol,  // 124; U007C; Sm; |
        LexerResult.RepeatEnd,  // 125; U007D; Pe; }
        LexerResult.Error,  // 126; U007E; Sm; ~
        LexerResult.Error // 127; U007F; Cc; DELETE

      };

      #region grammar
      //| /* ---- Start of Lex1 grammar as control structure  ---- */
      //|
      //| // Grammlator settings
      //| TerminalSymbolEnum: "ClassifierResult";
      //| SymbolNameOrFunctionCall: "LexerInput";
      //| SymbolAssignInstruction: "LexerInput = inputClassifier.PeekSymbol();";
      //| SymbolAcceptInstruction: "inputClassifier.AcceptSymbol();";
      //| ErrorHandlerMethod: "ErrorHandler";
      //| StateDescriptionPrefix: "StateDescription";
      //| IfToSwitchBorder: "4";
      //| CompareToFlagTestBorder: 4;
      //| IsMethod: "_is";
      //|
      //| // Declaration of the lexers terminal symbols (the output of the classifier):
      public enum CopyOfClassifierResult {
         // The following characters will be passed on by the lexer to the parser
         [Description(@"DefiningSymbol(Int32 i) %7 ""="" ")]
         DefiningSymbol,
         [Description(@"Comma(Int32 i) %7 "","" ")]
         Comma,
         [Description(@"DefinitionSeparatorSymbol(Int32 i) %7 ""|"" ")]
         DefinitionSeparatorSymbol,

         [Description(@"TerminatorSymbol(Int32 i) %5 "";"" ")]
         TerminatorSymbol,
         [Description(@"Plus(Int32 i) %3 ""+"" ")]
         Plus,
         [Description(@"Colon(Int32 i) %3 "":"" ")]
         Colon,
         [Description(@"Percent(Int32 i) %3 ""%"" ")]
         Percent,

         [Description(@"GroupStart(Int32 i) 3x ""("" ")]
         GroupStart,
         [Description(@"OptionStart(Int32 i) 1x ""["" ")]
         OptionStart,
         [Description(@"RepeatStart(Int32 i) 1x ""{"" ")]
         RepeatStart,

         [Description(@"GroupEnd(Int32 i) %3 "")"" ")]
         GroupEnd,
         [Description(@"OptionEnd(Int32 i) %1 ""]"" ")]
         OptionEnd,
         [Description(@"RepeatEnd(Int32 i) %1 ""}"" ")]
         RepeatEnd,
         [Description(@"NumberSign(Int32 i) %1 ""#"" ")]
         NumberSign,

         // The following "virtual" symbols represent the change from grammlator lines to CSharp lines and vice versa
         // They will also be passed on by the lexer to the parser
         [Description(@"CSharpStart(Int32 i) %8 ""C#"" ")]
         CSharpStart,
         [Description(@"CSharpEnd(Int32 i) %1 ""//|"" ")]
         CSharpEnd,
         // The following characters are evaluated by the lexer and not passed on to the parser

         // The following symbols may be part of a combined symbol and are handled individually by the generated code "?" may be part of "??"
         // Nevertheless they can be copied
         [Description(@"Questionmark(Int32 i) %5 ""?"" ")]
         Questionmark, // part of "??"
         [Description(@"Asterisk(Int32 i) %1 ""*"" ")]
         Asterisk, // Part of "*="
         [Description(@"Minus(Int32 i) %1 ""-"" ")]
         Minus, // Part of "-="

         [Description(@"At(Int32 i) %1 ""@"" ")]
         At, // @ may be 1st character of a string
         [Description(@"WhiteSpace(Int32 i) %20 "" "" ")]
         WhiteSpace,     // used as delimiter, skipped by lexer
         [Description(@"Slash(Int32 i) %9 ""/"" ")]
         Slash,          // delimiter of comments
         [Description(@"OtherCharacter(Int32 i) %1 ")]
         OtherCharacter, // allowed only in comments
         [Description(@"Quotationmark(Int32 i) %5 ""''"" ")]
         Quotationmark,     // delimits string constants used as names

         [Description(@"Letter(Int32 i) %10 ""letter"" ")]
         Letter,
         [Description(@"Digit(Int32 i) %5 ""digit"" ")]
         Digit   // as part of names and numbers
      };

      //| // Declaration of the startsymbol: the attributes of the definitions are used as attributes of the generated symbols
      //| *= 
      //|     Gap, CharacterToPassOn
      //|   | Gap, Name(UnifiedString unifiedString) 
      private void AssignNameToSymbol() => Symbol = LexerResult.Name;
      //|   | Gap, StartsymbolNumber (Int64 value)
      //|   | Gap, StartsymbolString(UnifiedString lexerString)
      //|   | Gap, StartsymbolDoubleQuestionmark
      //|   | Gap, StartsymbolStarEqual
      //|   | Gap, StartsymbolMinusEqual
      //|   | Gap, StartsymbolCSharpStart
      //|   | Gap, StartsymbolCSharpEnd

      //| StartsymbolNumber(Int64 value)=
      //|    Number(Int64 value) ?? -1 ?? /* low priority makes this definition greedy */
      private void AssignNumberToSymbol() => Symbol = LexerResult.Number;

      //| StartsymbolString(UnifiedString lexerString)=
      //|    String(UnifiedString lexerString)
      private void AssignStringToStartsymbol() => Symbol = LexerResult.LexerString;

      //| StartsymbolStarEqual=
      //|    Asterisk(Int32 i1), DefiningSymbol(Int32 i2)
      private void AssignStarEqual() => Symbol = LexerResult.StarEqual;

      //| StartsymbolMinusEqual=
      //|    Minus(Int32 i1), DefiningSymbol(Int32 i2)
      private void AssignMinusEqual() => Symbol = LexerResult.MinusEqual;

      //| StartsymbolDoubleQuestionmark=
      //|    Questionmark(Int32 i1), Questionmark(Int32 i2)
      private void AssignDoubleQuestionmarkToSymbol() => Symbol = LexerResult.DoubleQuestionmark;

      //| Gap= GapString
      private void AdvanceTextPos() => LexerTextPos = inputClassifier.InputPosition; // TOCHECK inputClassifier.InputPosition will be obsolete

      //| GapString=
      //|      /*empty */
      //|    | Gap, WhiteSpace(Int32 i)
      //|    | Gap, Comment;
      //|
      //| CharacterToPassOn=
      //|    OneCharacterToPassOn(Int32 index)
      private void TranslateCharToLexerResult(Int32 index)
         => Symbol = CharToLexerResult[Source.Span[index]];

      //| OneCharacterToPassOn(Int32 i)=
      //|    DefiningSymbol(Int32 i)     | Comma(Int32 i)         | DefinitionSeparatorSymbol(Int32 i) | Questionmark(Int32 i) ??-202??
      //|    | TerminatorSymbol(Int32 i) | Minus(Int32 i)??-200?? | Plus(Int32 i)                      | Colon (Int32 i)
      //|    | Asterisk(Int32 i)??-201?? | GroupStart(Int32 i)    | OptionStart(Int32 i)               | RepeatStart(Int32 i)
      //|    | GroupEnd(Int32 i)         | OptionEnd(Int32 i)     | RepeatEnd(Int32 i)                 | NumberSign(Int32 i)
      //|    | Percent(Int32 i)

      //|    StartsymbolCSharpStart = CSharpStart(Int32 i) // c == NewLineCharacter !!!
      private void AssignCSharpStartToSymbol()
          => Symbol = LexerResult.CSharpStart;

      //|    StartsymbolCSharpEnd= CSharpEnd(Int32 i) // c == NewLineCharacter !!!
      private void AssignCSharpEndToSymbol()
          => Symbol = LexerResult.CSharpEnd;

      //| /* Define optional String representations of some terminal symbols for better readability of the following grammar */
      //| "/"(Int32 i) = Slash(Int32 i); "*"(Int32 i) = Asterisk(Int32 i)

      //| Comment=
      //|    "/"(Int32 i1), "*"(Int32 i2), CommentcharacterSequenceEndingWithAsterisk, "/"(Int32 iEnd)
      //|    | "/"(Int32 i1), "/"(Int32 i2) /* ignore rest of line */
      private void SlashSlashComment() => inputClassifier.IgnoreAllCharactersUntilEndOfLine();

      //| CommentcharacterSequenceEndingWithAsterisk=
      //|   "*"(Int32 i)
      //|    | CommentcharacterSequenceNotEndingWithAsterisk, "*"(Int32 i)
      //|    | CommentcharacterSequenceEndingWithAsterisk, "*"(Int32 i)

      //| CommentcharacterSequenceNotEndingWithAsterisk=
      //|   "anyCharacter-*-CSharpStart-CSharpEnd"(Int32 i)
      //|   | CommentcharacterSequenceNotEndingWithAsterisk, "anyCharacter-*-CSharpStart-CSharpEnd"(Int32 i)
      //|   | CommentcharacterSequenceEndingWithAsterisk, anyCharacterExceptAsteriskAndSlash(Int32 i)

      //| "anyCharacter-*-CSharpStart-CSharpEnd"(Int32 i)-= /* and except CSharpStart and CSharpEnd */
      //|    Asterisk | CSharpStart | CSharpEnd;
      //|
      //| anyCharacterExceptAsteriskAndSlash(Int32 i)-= /* and except CSharpStart and CSharpEnd */
      //|   Asterisk | Slash | CSharpStart | CSharpEnd;
      //|
      //| anyCharacterExceptQuotationmark(Int32 i)-= /* and except CSharpStart and CSharpEnd */
      //|   Quotationmark | CSharpStart | CSharpEnd;
      //|
      //| Name(UnifiedString unifiedString)=
      //|    SequenceOfLettersOrDigits ??-11?? /* low priority makes this definition greedy */
      private void GetNameFromSource(out UnifiedString unifiedString)
         => unifiedString = new UnifiedString(Source[Name1stIndex..(NameLastIndex + 1)]);

      //| SequenceOfLettersOrDigits=
      //|    Letter(Int32  index)
      private void Found1stLetterOfName(Int32 index)
      {
         Name1stIndex = index;
         NameLastIndex = index;
      }

      private Int32 Name1stIndex, NameLastIndex;

      //|    | SequenceOfLettersOrDigits, LetterOrDigit(Int32 index) ??-12??
      private void AddCharToName(Int32 index)
         => NameLastIndex = index;

      //| LetterOrDigit(Int32 i)=
      //|       Letter(Int32 i) | Digit(Int32 i)

      //| String(UnifiedString lexerString) =
      //|    At(Int32 i)?, Quotationmark(Int32 startIndex), StringCharacterSequence, Quotationmark(Int32 endIndex) ??-111??
      private void GetStringIndex(out UnifiedString lexerString)
      {
         lexerString = new UnifiedString(StringCharacterSequence.Append("\"").ToString());
         StringCharacterSequence.Clear();
      }

      //| // OptionalAt= /* empty */ | At(Int32 i) // will be ignored and not consiedered part of the string
      //|
      //| StringCharacterSequence=
      //|      /* empty */
      private void StringStart()
      {
         StringCharacterSequence.Clear().Append("\"");
         ;
      }
      readonly StringBuilder StringCharacterSequence = new StringBuilder(128);

      //|    | StringCharacterSequence, anyCharacterExceptQuotationmark(Int32 index)
      private void StringAppendCharacter(Int32 index)
      {
         StringCharacterSequence.Append(Source.Span[index]);
      }
      //|    | StringCharacterSequence, Quotationmark(Int32 i1), Quotationmark(Int32 i2) 
      private void StringAppendQuotationmark()
      {
         StringCharacterSequence.Append("\"");
      }

      //| Number(Int64 number)=
      //|    Digit(Int32 index)
      private void Digit(out Int64 number, Int32 index)
          => number = Source.Span[index] - '0';
      //|    | Number(Int64 number), Digit(Int32 index)
      private void DigitNumberNew(ref Int64 number, Int32 index)
          => number = (number * 10) + (Source.Span[index] - '0');
      #endregion grammar

#pragma warning disable CA1502 // Avoid excessive complexity
      private void EvaluateInput() // von lex1
#pragma warning restore CA1502 // Avoid excessive complexity
      {
         ClassifierResult LexerInput;

#pragma warning disable IDE0059 // Der Wert, der dem Symbol zugeordnet ist, wird niemals verwendet.
#region grammlator generated 11 Okt 2020 (grammlator file version/date 2020.10.10.0/11 Okt 2020)
  Int32 _AttributeStackInitialCount = _a.Count;
  const Int64 _fCSharpStart = 1L << (Int32)(ClassifierResult.CSharpStart);
  const Int64 _fCSharpEnd = 1L << (Int32)(ClassifierResult.CSharpEnd);
  const Int64 _fAsterisk = 1L << (Int32)(ClassifierResult.Asterisk);
  const Int64 _fSlash = 1L << (Int32)(ClassifierResult.Slash);
  const Int64 _fQuotationmark = 1L << (Int32)(ClassifierResult.Quotationmark);
  Boolean _is(Int64 flags) => (1L << (Int32)((LexerInput)) & flags) != 0;

Reduce1:
  /* Gap= GapString;◄ */

  AdvanceTextPos();

State2:
  const String StateDescription2 =
       "*Startsymbol= Gap, ►CharacterToPassOn;\r\n"
     + "*Startsymbol= Gap, ►Name(UnifiedString unifiedString);\r\n"
     + "*Startsymbol= Gap, ►StartsymbolNumber(Int64 value);\r\n"
     + "*Startsymbol= Gap, ►StartsymbolString(UnifiedString lexerString);\r\n"
     + "*Startsymbol= Gap, ►StartsymbolDoubleQuestionmark;\r\n"
     + "*Startsymbol= Gap, ►StartsymbolStarEqual;\r\n"
     + "*Startsymbol= Gap, ►StartsymbolMinusEqual;\r\n"
     + "*Startsymbol= Gap, ►StartsymbolCSharpStart;\r\n"
     + "*Startsymbol= Gap, ►StartsymbolCSharpEnd;\r\n"
     + "GapString= Gap, ►WhiteSpace(Int32 i);\r\n"
     + "GapString= Gap, ►Comment;";
  LexerInput = inputClassifier.PeekSymbol();
  switch (LexerInput)
  {
  // <= ClassifierResult.NumberSign: goto AcceptReduce2 // see end of switch
  case ClassifierResult.CSharpStart:
     {
     inputClassifier.AcceptSymbol();
     // Reduce3:
     /* aAdjust: -1
      * StartsymbolCSharpStart= CSharpStart(Int32 i);◄ */

     AssignCSharpStartToSymbol();

     _a.Remove();
     goto EndOfGeneratedCode;
     }
  case ClassifierResult.CSharpEnd:
     {
     inputClassifier.AcceptSymbol();
     // Reduce4:
     /* aAdjust: -1
      * StartsymbolCSharpEnd= CSharpEnd(Int32 i);◄ */

     AssignCSharpEndToSymbol();

     _a.Remove();
     goto EndOfGeneratedCode;
     }
  case ClassifierResult.Questionmark:
     {
     inputClassifier.AcceptSymbol();
     // State15:
     /* StartsymbolDoubleQuestionmark= Questionmark(Int32 i1), ►Questionmark(Int32 i2);
      * OneCharacterToPassOn(Int32 i)= Questionmark(Int32 i)●; */
     LexerInput = inputClassifier.PeekSymbol();
     if (LexerInput != ClassifierResult.Questionmark)
        goto Reduce2;
     Debug.Assert(LexerInput == ClassifierResult.Questionmark);
     inputClassifier.AcceptSymbol();
     // Reduce33:
     /* aAdjust: -2
      * StartsymbolDoubleQuestionmark= Questionmark(Int32 i1), Questionmark(Int32 i2);◄ */

     AssignDoubleQuestionmarkToSymbol();

     _a.Remove(2);
     goto EndOfGeneratedCode;
     }
  case ClassifierResult.Asterisk:
     {
     inputClassifier.AcceptSymbol();
     // State14:
     /* StartsymbolStarEqual= Asterisk(Int32 i1), ►DefiningSymbol(Int32 i2);
      * OneCharacterToPassOn(Int32 i)= Asterisk(Int32 i)●; */
     LexerInput = inputClassifier.PeekSymbol();
     if (LexerInput > ClassifierResult.DefiningSymbol)
        goto Reduce2;
     Debug.Assert(LexerInput <= ClassifierResult.DefiningSymbol);
     inputClassifier.AcceptSymbol();
     // Reduce32:
     /* aAdjust: -2
      * StartsymbolStarEqual= Asterisk(Int32 i1), DefiningSymbol(Int32 i2);◄ */

     AssignStarEqual();

     _a.Remove(2);
     goto EndOfGeneratedCode;
     }
  case ClassifierResult.Minus:
     {
     inputClassifier.AcceptSymbol();
     // State13:
     /* StartsymbolMinusEqual= Minus(Int32 i1), ►DefiningSymbol(Int32 i2);
      * OneCharacterToPassOn(Int32 i)= Minus(Int32 i)●; */
     LexerInput = inputClassifier.PeekSymbol();
     if (LexerInput > ClassifierResult.DefiningSymbol)
        goto Reduce2;
     Debug.Assert(LexerInput <= ClassifierResult.DefiningSymbol);
     inputClassifier.AcceptSymbol();
     // Reduce31:
     /* aAdjust: -2
      * StartsymbolMinusEqual= Minus(Int32 i1), DefiningSymbol(Int32 i2);◄ */

     AssignMinusEqual();

     _a.Remove(2);
     goto EndOfGeneratedCode;
     }
  case ClassifierResult.At:
     {
     inputClassifier.AcceptSymbol();
     // Reduce5:
     /* aAdjust: -1
      * At?= At(Int32 i);◄ */
     _a.Remove();
     goto State3;
     }
  case ClassifierResult.WhiteSpace:
     {
     inputClassifier.AcceptSymbol();
     // Reduce6:
     /* aAdjust: -1
      * GapString= Gap, WhiteSpace(Int32 i);◄ */
     _a.Remove();
     goto Reduce1;
     }
  case ClassifierResult.Slash:
     {
     inputClassifier.AcceptSymbol();
     goto State9;
     }
  case ClassifierResult.OtherCharacter:
     {
     if (ErrorHandler(2, StateDescription2, LexerInput))
        goto State2;
     goto EndWithError;
     }
  case ClassifierResult.Quotationmark:
     goto State3;
  case ClassifierResult.Letter:
     {
     inputClassifier.AcceptSymbol();
     // Reduce7:
     /* aAdjust: -1
      * SequenceOfLettersOrDigits= Letter(Int32 index);◄ */

     Found1stLetterOfName(
        index: _a.PeekRef(0)._Int32
        );

     _a.Remove();
     goto State7;
     }
  // >= ClassifierResult.Digit: goto AcceptReduce8 // see end of switch
  } // end of switch
  if (LexerInput <= ClassifierResult.NumberSign)
     {
     inputClassifier.AcceptSymbol();
     goto Reduce2;
     }
  Debug.Assert(LexerInput >= ClassifierResult.Digit);

  inputClassifier.AcceptSymbol();
  // Reduce8:
  /* Number(Int64 number)= Digit(Int32 index);◄ */

  Digit(
     number: out _a.PeekRef(0)._Int64,
     index: _a.PeekClear(0)._Int32
     );

State8:
  /* StartsymbolNumber(Int64 value)= Number(Int64 value)●;
   * Number(Int64 number)= Number(Int64 number), ►Digit(Int32 index); */
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput < ClassifierResult.Digit)
     // Reduce25:
     {
     /* StartsymbolNumber(Int64 value)= Number(Int64 value);◄
      * then: *Startsymbol= Gap, StartsymbolNumber(Int64 value);◄
      * or: *Startsymbol= Gap, StartsymbolString(UnifiedString lexerString);◄ */

     AssignNumberToSymbol();

     goto ApplyStartsymbolDefinition2;
     }
  Debug.Assert(LexerInput >= ClassifierResult.Digit);
  inputClassifier.AcceptSymbol();
  // Reduce26:
  /* aAdjust: -1
   * Number(Int64 number)= Number(Int64 number), Digit(Int32 index);◄ */

  DigitNumberNew(
     number: ref _a.PeekRef(-1)._Int64,
     index: _a.PeekRef(0)._Int32
     );

  _a.Remove();
  goto State8;

AcceptReduce28:
  inputClassifier.AcceptSymbol();
  // Reduce28:
  /* aAdjust: -1
   * CommentcharacterSequenceNotEndingWithAsterisk= "anyCharacter-*-CSharpStart-CSharpEnd"(Int32 i);◄
   * or: CommentcharacterSequenceNotEndingWithAsterisk= CommentcharacterSequenceNotEndingWithAsterisk, "anyCharacter-*-CSharpStart-CSharpEnd"(Int32 i);◄
   * or: CommentcharacterSequenceNotEndingWithAsterisk= CommentcharacterSequenceEndingWithAsterisk, anyCharacterExceptAsteriskAndSlash(Int32 i);◄ */
  _a.Remove();
State11:
  const String StateDescription11 =
       "CommentcharacterSequenceEndingWithAsterisk= CommentcharacterSequenceNotEndingWithAsterisk, ►\"*\"(Int32 i);\r\n"
     + "CommentcharacterSequenceNotEndingWithAsterisk= CommentcharacterSequenceNotEndingWithAsterisk, ►\"anyCharacter-*-CSharpStart-CSharpEnd\"(Int32 i);";
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput == ClassifierResult.Asterisk)
     goto AcceptReduce29;
  if (LexerInput == ClassifierResult.CSharpStart || LexerInput == ClassifierResult.CSharpEnd)
     {
     if (ErrorHandler(11, StateDescription11, LexerInput))
        goto State11;
     goto EndWithError;
     }
  Debug.Assert(!_is(_fCSharpStart | _fCSharpEnd | _fAsterisk));
  goto AcceptReduce28;

AcceptReduce29:
  inputClassifier.AcceptSymbol();
  // Reduce29:
  /* aAdjust: -1
   * CommentcharacterSequenceEndingWithAsterisk= "*"(Int32 i);◄
   * or: CommentcharacterSequenceEndingWithAsterisk= CommentcharacterSequenceNotEndingWithAsterisk, "*"(Int32 i);◄
   * or: CommentcharacterSequenceEndingWithAsterisk= CommentcharacterSequenceEndingWithAsterisk, "*"(Int32 i);◄ */
  _a.Remove();
State12:
  const String StateDescription12 =
       "Comment= \"/\"(Int32 i1), \"*\"(Int32 i2), CommentcharacterSequenceEndingWithAsterisk, ►\"/\"(Int32 iEnd);\r\n"
     + "CommentcharacterSequenceEndingWithAsterisk= CommentcharacterSequenceEndingWithAsterisk, ►\"*\"(Int32 i);\r\n"
     + "CommentcharacterSequenceNotEndingWithAsterisk= CommentcharacterSequenceEndingWithAsterisk, ►anyCharacterExceptAsteriskAndSlash(Int32 i);";
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput == ClassifierResult.Slash)
     {
     inputClassifier.AcceptSymbol();
     // Reduce30:
     /* aAdjust: -3
      * Comment= "/"(Int32 i1), "*"(Int32 i2), CommentcharacterSequenceEndingWithAsterisk, "/"(Int32 iEnd);◄ */
     _a.Remove(3);
     goto Reduce1;
     }
  if (LexerInput == ClassifierResult.Asterisk)
     goto AcceptReduce29;
  if (LexerInput == ClassifierResult.CSharpStart || LexerInput == ClassifierResult.CSharpEnd)
     {
     if (ErrorHandler(12, StateDescription12, LexerInput))
        goto State12;
     goto EndWithError;
     }
  Debug.Assert(!_is(_fCSharpStart | _fCSharpEnd | _fAsterisk | _fSlash));
  goto AcceptReduce28;

State3:
  const String StateDescription3 =
       "String(UnifiedString lexerString)= At?, ►Quotationmark(Int32 startIndex), StringCharacterSequence, Quotationmark(Int32 endIndex);";
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput != ClassifierResult.Quotationmark)
     {
     if (ErrorHandler(3, StateDescription3, LexerInput))
        goto State3;
     goto EndWithError;
     }
  Debug.Assert(LexerInput == ClassifierResult.Quotationmark);
  inputClassifier.AcceptSymbol();
  // Reduce19:
  /* StringCharacterSequence= ;◄ */

  StringStart();

State5:
  const String StateDescription5 =
       "String(UnifiedString lexerString)= At?, Quotationmark(Int32 startIndex), StringCharacterSequence, ►Quotationmark(Int32 endIndex);\r\n"
     + "StringCharacterSequence= StringCharacterSequence, ►anyCharacterExceptQuotationmark(Int32 index);\r\n"
     + "StringCharacterSequence= StringCharacterSequence, ►Quotationmark(Int32 i1), Quotationmark(Int32 i2);";
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput == ClassifierResult.Quotationmark)
     {
     inputClassifier.AcceptSymbol();
     // State6:
     /* String(UnifiedString lexerString)= At?, Quotationmark(Int32 startIndex), StringCharacterSequence, Quotationmark(Int32 endIndex)●;
      * StringCharacterSequence= StringCharacterSequence, Quotationmark(Int32 i1), ►Quotationmark(Int32 i2); */
     LexerInput = inputClassifier.PeekSymbol();
     if (LexerInput != ClassifierResult.Quotationmark)
        // Reduce21:
        {
        /* aAdjust: -1
         * String(UnifiedString lexerString)= At?, Quotationmark(Int32 startIndex), StringCharacterSequence, Quotationmark(Int32 endIndex);◄ */

        GetStringIndex(
           lexerString: out _a.PeekRefClear(-1)._UnifiedString
           );

        _a.Remove();
        // Reduce18:
        /* StartsymbolString(UnifiedString lexerString)= String(UnifiedString lexerString);◄
         * then: *Startsymbol= Gap, StartsymbolNumber(Int64 value);◄
         * or: *Startsymbol= Gap, StartsymbolString(UnifiedString lexerString);◄ */

        AssignStringToStartsymbol();

        goto ApplyStartsymbolDefinition2;
        }
     Debug.Assert(LexerInput == ClassifierResult.Quotationmark);
     inputClassifier.AcceptSymbol();
     // Reduce22:
     /* aAdjust: -2
      * StringCharacterSequence= StringCharacterSequence, Quotationmark(Int32 i1), Quotationmark(Int32 i2);◄ */

     StringAppendQuotationmark();

     _a.Remove(2);
     goto State5;
     }
  if (LexerInput == ClassifierResult.CSharpStart || LexerInput == ClassifierResult.CSharpEnd)
     {
     if (ErrorHandler(5, StateDescription5, LexerInput))
        goto State5;
     goto EndWithError;
     }
  Debug.Assert(!_is(_fCSharpStart | _fCSharpEnd | _fQuotationmark));
  inputClassifier.AcceptSymbol();
  // Reduce20:
  /* aAdjust: -1
   * StringCharacterSequence= StringCharacterSequence, anyCharacterExceptQuotationmark(Int32 index);◄ */

  StringAppendCharacter(
     index: _a.PeekRef(0)._Int32
     );

  _a.Remove();
  goto State5;

State7:
  /* Name(UnifiedString unifiedString)= SequenceOfLettersOrDigits●;
   * SequenceOfLettersOrDigits= SequenceOfLettersOrDigits, ►LetterOrDigit(Int32 index); */
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput <= ClassifierResult.Quotationmark)
     // Reduce23:
     {
     /* aAdjust: 1
      * Name(UnifiedString unifiedString)= SequenceOfLettersOrDigits;◄ */
     _a.Allocate();

     GetNameFromSource(
        unifiedString: out _a.PeekRef(0)._UnifiedString
        );

     // Reduce10:
     /* *Startsymbol= Gap, Name(UnifiedString unifiedString);◄ */

     AssignNameToSymbol();

     goto ApplyStartsymbolDefinition2;
     }
  Debug.Assert(LexerInput >= ClassifierResult.Letter);
  inputClassifier.AcceptSymbol();
  // Reduce24:
  /* aAdjust: -1
   * SequenceOfLettersOrDigits= SequenceOfLettersOrDigits, LetterOrDigit(Int32 index);◄ */

  AddCharToName(
     index: _a.PeekRef(0)._Int32
     );

  _a.Remove();
  goto State7;

State9:
  const String StateDescription9 =
       "Comment= \"/\"(Int32 i1), ►\"*\"(Int32 i2), CommentcharacterSequenceEndingWithAsterisk, \"/\"(Int32 iEnd);\r\n"
     + "Comment= \"/\"(Int32 i1), ►\"/\"(Int32 i2);";
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput == ClassifierResult.Slash)
     {
     inputClassifier.AcceptSymbol();
     // Reduce27:
     /* aAdjust: -2
      * Comment= "/"(Int32 i1), "/"(Int32 i2);◄ */

     SlashSlashComment();

     _a.Remove(2);
     goto Reduce1;
     }
  if (LexerInput != ClassifierResult.Asterisk)
     {
     if (ErrorHandler(9, StateDescription9, LexerInput))
        goto State9;
     goto EndWithError;
     }
  Debug.Assert(LexerInput == ClassifierResult.Asterisk);
  inputClassifier.AcceptSymbol();
State10:
  const String StateDescription10 =
       "Comment= \"/\"(Int32 i1), \"*\"(Int32 i2), ►CommentcharacterSequenceEndingWithAsterisk, \"/\"(Int32 iEnd);";
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput == ClassifierResult.Asterisk)
     goto AcceptReduce29;
  if (LexerInput == ClassifierResult.CSharpStart || LexerInput == ClassifierResult.CSharpEnd)
     {
     if (ErrorHandler(10, StateDescription10, LexerInput))
        goto State10;
     goto EndWithError;
     }
  Debug.Assert(!_is(_fCSharpStart | _fCSharpEnd | _fAsterisk));
  goto AcceptReduce28;

Reduce2:
  /* aAdjust: -1
   * CharacterToPassOn= OneCharacterToPassOn(Int32 index);◄ */

  TranslateCharToLexerResult(
     index: _a.PeekRef(0)._Int32
     );

  _a.Remove();
  goto EndOfGeneratedCode;

ApplyStartsymbolDefinition2:
  // Halt: a definition of the startsymbol with 1 attributes has been recognized.
AttributesOfSymbol.CopyAndRemoveFrom(_a, 1);
  goto EndOfGeneratedCode;

EndWithError:
  // This point is reached after an input error has been found
  _a.Remove(_a.Count - _AttributeStackInitialCount);

EndOfGeneratedCode:
  ;

#endregion grammlator generated 11 Okt 2020 (grammlator file version/date 2020.10.10.0/11 Okt 2020)
#pragma warning restore IDE0059 // Der Wert, der dem Symbol zugeordnet ist, wird niemals verwendet.
      }
   }
}
