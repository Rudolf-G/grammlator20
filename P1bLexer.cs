using GrammlatorRuntime;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace Grammlator {
#pragma warning disable CS1591 // Fehlender XML-Kommentar für öffentlich sichtbaren Typ oder Element
   public enum LexerResult { // Defines the output of the lexer, which is assigned to Symbol to be used by the parser
                             // The elements of LexerResult are order such that grammlator can
                             // generate efficient code for the conditions of the parsers actions
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

      GroupStart, OptionStart, RepeatStart, // these are the characters ( [ {

      DoubleQuestionmark,
      CSharpStart, // represents the change from grammlator lines to CSharp lines

      GroupEnd, RepeatEnd, OptionEnd,  // these are the characters ) } ] 

      Name /* Attribute (Int32 stringIndex) */, // try (Position, Length
      StringResult /* should be named String, but this is avoided, Attribute (Int32 stringIndex) */,

      DefinitionSeparatorSymbol, // |
      TerminatorSymbol // ;
      };

   public static class LexerResultExtensions {
      public static string MyToString(this LexerResult lr)
         {
         const string MyDisplay = "=:%xx-xxx?*+,#([{\u2047x)}]xx|;"; // \u2047 is "??" as one character

         Debug.Assert(lr != LexerResult.Error);
         if ((int)lr >= MyDisplay.Length)
            return lr.ToString();

         char result = MyDisplay[(int)lr];
         if (result != 'x')
            return result.ToString();
         else
         if (lr == LexerResult.MinusEqual)
            return "-=";
         else if (lr == LexerResult.DoubleQuestionmark)
            return "??";
         else if (lr == LexerResult.StarEqual)
            return "*=";
         else
            return lr.ToString();
         }
      }


   /// <summary>
   /// Grammlator lexer (uses input classifier, is used by parser)
   /// </summary>
   internal class P1bLexer: GrammlatorInputApplication<LexerResult> {

      private readonly P1cInputClassifier inputClassifier;

      public P1bLexer(SpanReaderWithCharacterAndLineCounter sourceReader,
          StackOfMultiTypeElements attributeStack, Stack<Int32> stateStack)
          : base(attributeStack, stateStack)
         {
         Source = sourceReader.Source;
         inputClassifier = new P1cInputClassifier(sourceReader, attributeStack);
         //this._a = attributeStack;
         //this._s = stateStack;

         LexerTextPos = new STextPosition { LineNumber = -1, ColumnNumber = 0, Position = 0 };
         Accepted = true;
         }

      private readonly ReadOnlyMemory<char> Source;

      /// <summary>
      /// Current Position
      /// </summary>
      public STextPosition LexerTextPos {
         get; private set;
         }

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
      /// Skips whitespace and comment lines and checks if the reached line starts with the given markers
      /// </summary>
      /// <param name="markers">true if input line contains the squence of markers (and optional whitespace)</param>
      /// <returns>true if the markers are found in the input line</returns>
      public Boolean MarkedLineFollows(params String[] markers)
         {
         ClassifierResult s;
         if (inputClassifier.Accepted)
            {
            do
               {
               // skip whitespace (may be multiple lines)
               inputClassifier.AcceptSymbol();
               s = inputClassifier.PeekSymbol();

               } while (s == ClassifierResult.WhiteSpace);
            }
         return inputClassifier.IsMarkedLine(markers);
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

      private bool ErrorHandler(Int32 LexerStateNumber, String stateDescription, ClassifierResult symbol)
         {
         LexerTextPos = inputClassifier.InputPosition;
         inputClassifier.AcceptSymbol(); // make the attribute (the character) available in the stack
         GlobalVariables.OutputPositionAndMessage(MessageTypeOrDestinationEnum.Abort,
             "Lexical analysis error " + LexerStateNumber.ToString() + ": input character \'" +
             _a.PeekRef(0)._Char.ToString() + "\' classified as \"" + symbol.MyToString() +
             "\" not allowed in state " + Environment.NewLine + stateDescription + Environment.NewLine
             , LexerTextPos
             ); // throws an exception
         Debug.Fail("Error in program: this debug-instruction should never be executed");
         return false;
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
      //| // Compiler settings
      //| IfToSwitchBorder: "4";
      //| Symbol: "LexerInput"
      //| AssignSymbol: "LexerInput = inputClassifier.PeekSymbol();"
      //| AcceptSymbol: "inputClassifier.AcceptSymbol();"
      //| TerminalSymbolEnum: "ClassifierResult"
      //| StateDescription: "StateDescription"
      //| ErrorHandlerMethod: "ErrorHandler"
      //|
      //| // Declaration of the lexers terminal symbols (the output of the classifier):
      //|      DefiningSymbol(int i)%7 | Comma(int i)%7 | DefinitionSeparatorSymbol(int i)%7 
      //|    | TerminatorSymbol(int i)%5 | Plus(int i)%3 | Colon(int i)%3 | Percent(int i)%3
      //|    | GroupStart(int i)%3 | OptionStart(int i)%1 | RepeatStart(int i)%1
      //|    | GroupEnd(int i)%3 | OptionEnd(int i)%1 | RepeatEnd(int i)%1 | NumberSign(int i)%1
      //|    | CSharpStart(int i)%8 | CSharpEnd(int i)%1 
      //|    | Questionmark(int i)%5 | Asterisk(int i)%1 | Minus(int i)%1 
      //|    | WhiteSpace(int i)%20  | Slash(int i)%9
      //|    | OtherCharacter(int i)%1| Apostrophe(int i)%5
      //|    | Letter(int i)%10 | Digit(int i)%5  

      public enum CopyOfClassifierResult {
         // The following characters will be passed on by the lexer to the parser
         DefiningSymbol, Comma, DefinitionSeparatorSymbol, // these are the characters = , |

         TerminatorSymbol, Plus, Colon, Percent,           // these are the characters ; - + : * %

         GroupStart, OptionStart, RepeatStart,             // these are the characters ( [ { 

         GroupEnd, OptionEnd, RepeatEnd, NumberSign,       // these are the characters ) ] } #

         // The following "virtual" symbols represent the change from grammlator lines to CSharp lines and vice versa
         // They will also bepassed on by the lexer to the parser
         CSharpStart, CSharpEnd,
         // The following characters are evaluated by the lexer and not passed on to the parser

         // The following symbols may be part of a combined symbol and are handled individually by the generated code "?" may be part of "??"
         // Nevertheless they can be copied
         Questionmark, // part of "??"
         Asterisk, // Part of "*="
         Minus, // Part of "-="

         WhiteSpace,     // used as delimiter, skipped by lexer
         Slash,          // delimiter of comments
         OtherCharacter, // allowed only in comments
         Apostrophe,     // delimits string constants used as names
         Letter, Digit   // as part of names and numbers
         };

      //| // Declaration of the startsymbol: the attributes of the definitions are used as attributes of the generated symbols
      //| *= 
      //|     Gap, CharacterToPassOn
      //|   | Gap, Name(Int32 stringIndex) 
      private void AssignNameToSymbol() => Symbol = LexerResult.Name;
      //|   | Gap, StartsymbolNumber (Int32 value)
      //|   | Gap, StartsymbolString(Int32 stringIndex)
      //|   | Gap, StartsymbolDoubleQuestionmark
      //|   | Gap, StartsymbolStarEqual
      //|   | Gap, StartsymbolMinusEqual
      //|   | Gap, StartsymbolCSharpStart
      //|   | Gap, StartsymbolCSharpEnd

      //| StartsymbolNumber(Int32 value)=
      //|    Number(Int32 value) ?? -1 ?? /* low priority makes this definition greedy */
      private void AssignNumberToSymbol() => Symbol = LexerResult.Number;

      //| StartsymbolString(Int32 stringIndex)=
      //|    String(Int32 stringIndex)
      private void AssignStringToStartsymbol() => Symbol = LexerResult.StringResult;

      //| StartsymbolStarEqual=
      //|    Asterisk(int i1), DefiningSymbol(int i2)
      private void AssignStarEqual() => Symbol = LexerResult.StarEqual;

      //| StartsymbolMinusEqual=
      //|    Minus(int i1), DefiningSymbol(int i2)
      private void AssignMinusEqual() => Symbol = LexerResult.MinusEqual;

      //| StartsymbolDoubleQuestionmark=
      //|    Questionmark(int i1), Questionmark(int i2)
      private void AssignDoubleQuestionmarkToSymbol() => Symbol = LexerResult.DoubleQuestionmark;

      //| Gap= GapString
      private void AdvanceTextPos() => LexerTextPos = inputClassifier.InputPosition; // CHECK inputClassifier.InputPosition will be obsolete

      //| GapString=
      //|      /*empty */
      //|    | Gap, WhiteSpace(int i)
      //|    | Gap, Comment;
      //|
      //| CharacterToPassOn=
      //|    OneCharacterToPassOn(int index)
      private void TranslateCharToLexerResult(int index)
         => Symbol = CharToLexerResult[Source.Span[index]];

      //| OneCharacterToPassOn(int i)=
      //|    DefiningSymbol(int i)     | Comma(int i)         | DefinitionSeparatorSymbol(int i) | Questionmark(int i) ??-202??
      //|    | TerminatorSymbol(int i) | Minus(int i)??-200?? | Plus(int i)                      | Colon (int i)
      //|    | Asterisk(int i)??-201?? | GroupStart(int i)    | OptionStart(int i)               | RepeatStart(int i)
      //|    | GroupEnd(int i)         | OptionEnd(int i)     | RepeatEnd(int i)                 | NumberSign(int i)
      //|    | Percent(int i)

      //|    StartsymbolCSharpStart = CSharpStart(int i) // c == NewLineCharacter !!!
      private void AssignCSharpStartToSymbol()
          => Symbol = LexerResult.CSharpStart;

      //|    StartsymbolCSharpEnd= CSharpEnd(int i) // c == NewLineCharacter !!!
      private void AssignCSharpEndToSymbol()
          => Symbol = LexerResult.CSharpEnd;

      //| /* Define optional String representations of some terminal symbols for better readability of the following grammar */
      //| "/"(int i) = Slash(int i); "*"(int i) = Asterisk(int i)

      //| Comment=
      //|    "/"(int i1), "*"(int i2), CommentcharacterSequenceEndingWithAsterisk, "/"(int iEnd)
      //|    | "/"(int i1), "/"(int i2) /* ignore rest of line */
      private void SlashSlashComment() => inputClassifier.IgnoreAllCharactersUntilEndOfLine();

      //| CommentcharacterSequenceEndingWithAsterisk=
      //|   "*"(int i)
      //|    | CommentcharacterSequenceNotEndingWithAsterisk, "*"(int i)
      //|    | CommentcharacterSequenceEndingWithAsterisk, "*"(int i)

      //| CommentcharacterSequenceNotEndingWithAsterisk=
      //|   "anyCharacter-*-CSharpStart-CSharpEnd"(int i)
      //|   | CommentcharacterSequenceNotEndingWithAsterisk, "anyCharacter-*-CSharpStart-CSharpEnd"(int i)
      //|   | CommentcharacterSequenceEndingWithAsterisk, anyCharacterExceptAsteriskAndSlash(int i)

      //| "anyCharacter-*-CSharpStart-CSharpEnd"(int i)-= /* and except CSharpStart and CSharpEnd */
      //|    Asterisk | CSharpStart | CSharpEnd;
      //|
      //| anyCharacterExceptAsteriskAndSlash(int i)-= /* and except CSharpStart and CSharpEnd */
      //|   Asterisk | Slash | CSharpStart | CSharpEnd;
      //|
      //| anyCharacterExceptApostrophe(int i)-= /* and except CSharpStart and CSharpEnd */
      //|   Apostrophe | CSharpStart | CSharpEnd;
      //|
      //| Name(Int32 stringIndex)=
      //|    SequenceOfLettersOrDigits ??-11?? /* low priority makes this definition greedy */
      private void GetNameFromSource(out Int32 stringIndex)
         => stringIndex = GlobalVariables.GetIndexOfString(Source[Name1stIndex..(NameLastIndex + 1)]);

      //| SequenceOfLettersOrDigits=
      //|    Letter(int  index)
      private void Found1stLetterOfName(int index)
         {
         Name1stIndex = index;
         NameLastIndex = index;
         }

      private int Name1stIndex, NameLastIndex;

      //|    | SequenceOfLettersOrDigits, LetterOrDigit(int index) ??-12??
      private void AddCharToName(int index)
         => NameLastIndex = index;

      //| LetterOrDigit(int i)=
      //|       Letter(int i) | Digit(int i)

      //| String(Int32 stringIndex) = Apostrophe(int startIndex), StringCharacterSequence, Apostrophe(int endIndex)
      private void GetStringIndex(int startIndex, int endIndex, out Int32 stringIndex)
         {
         stringIndex = GlobalVariables.GetIndexOfString(Source[startIndex..(endIndex + 1)]);
         }

      //| StringCharacterSequence=
      //|      /* empty */
      //|    | StringCharacterSequence, StringCharacter

      //| StringCharacter=
      //|   anyCharacterExceptApostrophe(int i)

      //| Number(Int32 number)=
      //|    Digit(int index)
      private void DigitNew(out Int32 number, int index)
          => number = Source.Span[index] - '0';
      //|    | Number(Int32 number), Digit(int index)
      private void DigitNumberNew(ref Int32 number, int index)
          => number = (number * 10) + (Source.Span[index] - '0');
      #endregion grammar

#pragma warning disable CA1502 // Avoid excessive complexity
      private void EvaluateInput() // von lex1
#pragma warning restore CA1502 // Avoid excessive complexity
         {
         ClassifierResult LexerInput;

#pragma warning disable IDE0059 // Der Wert, der dem Symbol zugeordnet ist, wird niemals verwendet.
#region grammlator generated Sun, 12 Apr 2020 16:38:30 GMT by Grammlator version 0:21 (build Sun, 12 Apr 2020 07:40:56 GMT)
  Int32 AttributeStackInitialCount = _a.Count;
  /* State 1
   * *Startsymbol= ►Gap, CharacterToPassOn;
   * *Startsymbol= ►Gap, Name(Int32 stringIndex);
   * *Startsymbol= ►Gap, StartsymbolNumber(Int32 value);
   * *Startsymbol= ►Gap, StartsymbolString(Int32 stringIndex);
   * *Startsymbol= ►Gap, StartsymbolDoubleQuestionmark;
   * *Startsymbol= ►Gap, StartsymbolStarEqual;
   * *Startsymbol= ►Gap, StartsymbolMinusEqual;
   * *Startsymbol= ►Gap, StartsymbolCSharpStart;
   * *Startsymbol= ►Gap, StartsymbolCSharpEnd;
   */
Reduce1:
  /* Reduction 1
   * Gap= GapString;◄ method: AdvanceTextPos
   */

  AdvanceTextPos();

State2:
  /* State 2 */
  const String StateDescription2 =
       "*Startsymbol= Gap, ►CharacterToPassOn;\r\n"
     + "*Startsymbol= Gap, ►Name(Int32 stringIndex);\r\n"
     + "*Startsymbol= Gap, ►StartsymbolNumber(Int32 value);\r\n"
     + "*Startsymbol= Gap, ►StartsymbolString(Int32 stringIndex);\r\n"
     + "*Startsymbol= Gap, ►StartsymbolDoubleQuestionmark;\r\n"
     + "*Startsymbol= Gap, ►StartsymbolStarEqual;\r\n"
     + "*Startsymbol= Gap, ►StartsymbolMinusEqual;\r\n"
     + "*Startsymbol= Gap, ►StartsymbolCSharpStart;\r\n"
     + "*Startsymbol= Gap, ►StartsymbolCSharpEnd;\r\n"
     + "GapString= Gap, ►WhiteSpace(int i);\r\n"
     + "GapString= Gap, ►Comment;";
  LexerInput = inputClassifier.PeekSymbol();
  switch (LexerInput)
  {
  // <= ClassifierResult.NumberSign: goto AcceptReduce2 // see end of switch
  case ClassifierResult.CSharpStart:
     {
     inputClassifier.AcceptSymbol();
     /* Reduction 3, aStack: -1
      * StartsymbolCSharpStart= CSharpStart(int i);◄ method: AssignCSharpStartToSymbol, aStack: -1
      * then: *Startsymbol= Gap, CharacterToPassOn;◄
      */

     AssignCSharpStartToSymbol();

     _a.Free();
     goto EndOfGeneratedCode1;
     }
  case ClassifierResult.CSharpEnd:
     {
     inputClassifier.AcceptSymbol();
     /* Reduction 4, aStack: -1
      * StartsymbolCSharpEnd= CSharpEnd(int i);◄ method: AssignCSharpEndToSymbol, aStack: -1
      * then: *Startsymbol= Gap, CharacterToPassOn;◄
      */

     AssignCSharpEndToSymbol();

     _a.Free();
     goto EndOfGeneratedCode1;
     }
  case ClassifierResult.Questionmark:
     {
     inputClassifier.AcceptSymbol();
     /* State 13
      * StartsymbolDoubleQuestionmark= Questionmark(int i1), ►Questionmark(int i2);
      * OneCharacterToPassOn(int i)= Questionmark(int i)●;
      */
     LexerInput = inputClassifier.PeekSymbol();
     if (LexerInput != ClassifierResult.Questionmark)
        goto Reduce12;
     Debug.Assert(LexerInput == ClassifierResult.Questionmark);
     inputClassifier.AcceptSymbol();
     /* Reduction 25, aStack: -2
      * StartsymbolDoubleQuestionmark= Questionmark(int i1), Questionmark(int i2);◄ method: AssignDoubleQuestionmarkToSymbol, aStack: -2
      * then: *Startsymbol= Gap, CharacterToPassOn;◄
      */

     AssignDoubleQuestionmarkToSymbol();

     _a.Free(2);
     goto EndOfGeneratedCode1;
     }
  case ClassifierResult.Asterisk:
     {
     inputClassifier.AcceptSymbol();
     /* State 12
      * StartsymbolStarEqual= Asterisk(int i1), ►DefiningSymbol(int i2);
      * OneCharacterToPassOn(int i)= Asterisk(int i)●;
      */
     LexerInput = inputClassifier.PeekSymbol();
     if (LexerInput > ClassifierResult.DefiningSymbol)
        goto Reduce12;
     Debug.Assert(LexerInput <= ClassifierResult.DefiningSymbol);
     inputClassifier.AcceptSymbol();
     /* Reduction 24, aStack: -2
      * StartsymbolStarEqual= Asterisk(int i1), DefiningSymbol(int i2);◄ method: AssignStarEqual, aStack: -2
      * then: *Startsymbol= Gap, CharacterToPassOn;◄
      */

     AssignStarEqual();

     _a.Free(2);
     goto EndOfGeneratedCode1;
     }
  case ClassifierResult.Minus:
     {
     inputClassifier.AcceptSymbol();
     /* State 11
      * StartsymbolMinusEqual= Minus(int i1), ►DefiningSymbol(int i2);
      * OneCharacterToPassOn(int i)= Minus(int i)●;
      */
     LexerInput = inputClassifier.PeekSymbol();
     if (LexerInput > ClassifierResult.DefiningSymbol)
        goto Reduce12;
     Debug.Assert(LexerInput <= ClassifierResult.DefiningSymbol);
     inputClassifier.AcceptSymbol();
     /* Reduction 23, aStack: -2
      * StartsymbolMinusEqual= Minus(int i1), DefiningSymbol(int i2);◄ method: AssignMinusEqual, aStack: -2
      * then: *Startsymbol= Gap, CharacterToPassOn;◄
      */

     AssignMinusEqual();

     _a.Free(2);
     goto EndOfGeneratedCode1;
     }
  case ClassifierResult.WhiteSpace:
     {
     inputClassifier.AcceptSymbol();
     /* Reduction 5, aStack: -1
      * GapString= Gap, WhiteSpace(int i);◄ aStack: -1
      */
     _a.Free();
     goto Reduce1;
     }
  case ClassifierResult.Slash:
     {
     inputClassifier.AcceptSymbol();
     goto State7;
     }
  case ClassifierResult.OtherCharacter:
     {
     if (ErrorHandler(2, StateDescription2, LexerInput))
        goto State2;
     goto EndWithError1;
     }
  case ClassifierResult.Apostrophe:
     {
     inputClassifier.AcceptSymbol();
     goto State6;
     }
  case ClassifierResult.Letter:
     {
     inputClassifier.AcceptSymbol();
     /* Reduction 6, aStack: -1
      * SequenceOfLettersOrDigits= Letter(int index);◄ method: Found1stLetterOfName, aStack: -1
      */

     Found1stLetterOfName(
        index: _a.PeekRef(0)._int
        );

     _a.Free();
     goto State3;
     }
  // >= ClassifierResult.Digit: goto AcceptReduce7 // see end of switch
  } // end of switch
  if (LexerInput <= ClassifierResult.NumberSign)
  {
     inputClassifier.AcceptSymbol();
     /* Reduction 2, aStack: -1
      * CharacterToPassOn= OneCharacterToPassOn(int index);◄ method: TranslateCharToLexerResult, aStack: -1
      * then: *Startsymbol= Gap, CharacterToPassOn;◄
      */

     TranslateCharToLexerResult(
        index: _a.PeekRef(0)._int
        );

     _a.Free();
     goto EndOfGeneratedCode1;
     }
  Debug.Assert(LexerInput >= ClassifierResult.Digit);

  inputClassifier.AcceptSymbol();
  /* Reduction 7
   * Number(Int32 number)= Digit(int index);◄ method: DigitNew
   */

  DigitNew(
     number: out _a.PeekRef(0)._Int32,
     index: _a.PeekClear(0)._int
     );

State4:
  /* State 4
   * StartsymbolNumber(Int32 value)= Number(Int32 value)●;
   * Number(Int32 number)= Number(Int32 number), ►Digit(int index);
   */
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput < ClassifierResult.Digit)
     {
     /* Reduction 15
      * StartsymbolNumber(Int32 value)= Number(Int32 value);◄ Priority: -1, method: AssignNumberToSymbol
      * then: *Startsymbol= Gap, StartsymbolNumber(Int32 value);◄ aStack: -1
      */

     AssignNumberToSymbol();

     goto ApplyStartsymbolDefinition2;
     }
  Debug.Assert(LexerInput >= ClassifierResult.Digit);
  inputClassifier.AcceptSymbol();
  /* Reduction 16, aStack: -1
   * Number(Int32 number)= Number(Int32 number), Digit(int index);◄ method: DigitNumberNew, aStack: -1
   */

  DigitNumberNew(
     number: ref _a.PeekRef(-1)._Int32,
     index: _a.PeekRef(0)._int
     );

  _a.Free();
  goto State4;

AcceptReduce20:
  inputClassifier.AcceptSymbol();
  /* Reduction 20, aStack: -1
   * CommentcharacterSequenceNotEndingWithAsterisk= "anyCharacter-*-CSharpStart-CSharpEnd"(int i);◄ aStack: -1
   */
  _a.Free();
State9:
  /* State 9 */
  const String StateDescription9 =
       "CommentcharacterSequenceEndingWithAsterisk= CommentcharacterSequenceNotEndingWithAsterisk, ►\"*\"(int i);\r\n"
     + "CommentcharacterSequenceNotEndingWithAsterisk= CommentcharacterSequenceNotEndingWithAsterisk, ►\"anyCharacter-*-CSharpStart-CSharpEnd\"(int i);";
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput == ClassifierResult.Asterisk)
     goto AcceptReduce21;
  if (LexerInput == ClassifierResult.CSharpStart || LexerInput == ClassifierResult.CSharpEnd)
     {
     if (ErrorHandler(9, StateDescription9, LexerInput))
        goto State9;
     goto EndWithError1;
     }
  Debug.Assert(LexerInput != ClassifierResult.CSharpStart && LexerInput != ClassifierResult.CSharpEnd
     && LexerInput != ClassifierResult.Asterisk);
  goto AcceptReduce20;

AcceptReduce21:
  inputClassifier.AcceptSymbol();
  /* Reduction 21, aStack: -1
   * CommentcharacterSequenceEndingWithAsterisk= "*"(int i);◄ aStack: -1
   */
  _a.Free();
State10:
  /* State 10 */
  const String StateDescription10 =
       "Comment= \"/\"(int i1), \"*\"(int i2), CommentcharacterSequenceEndingWithAsterisk, ►\"/\"(int iEnd);\r\n"
     + "CommentcharacterSequenceEndingWithAsterisk= CommentcharacterSequenceEndingWithAsterisk, ►\"*\"(int i);\r\n"
     + "CommentcharacterSequenceNotEndingWithAsterisk= CommentcharacterSequenceEndingWithAsterisk, ►anyCharacterExceptAsteriskAndSlash(int i);";
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput == ClassifierResult.Slash)
     {
     inputClassifier.AcceptSymbol();
     /* Reduction 22, aStack: -3
      * Comment= "/"(int i1), "*"(int i2), CommentcharacterSequenceEndingWithAsterisk, "/"(int iEnd);◄ aStack: -3
      */
     _a.Free(3);
     goto Reduce1;
     }
  if (LexerInput == ClassifierResult.Asterisk)
     goto AcceptReduce21;
  if (LexerInput == ClassifierResult.CSharpStart || LexerInput == ClassifierResult.CSharpEnd)
     {
     if (ErrorHandler(10, StateDescription10, LexerInput))
        goto State10;
     goto EndWithError1;
     }
  Debug.Assert(LexerInput != ClassifierResult.CSharpStart && LexerInput != ClassifierResult.CSharpEnd
     && LexerInput != ClassifierResult.Asterisk
     && LexerInput != ClassifierResult.Slash);
  goto AcceptReduce20;

State3:
  /* State 3
   * Name(Int32 stringIndex)= SequenceOfLettersOrDigits●;
   * SequenceOfLettersOrDigits= SequenceOfLettersOrDigits, ►LetterOrDigit(int index);
   */
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput <= ClassifierResult.Apostrophe)
     {
     /* Reduction 13, aStack: 1
      * Name(Int32 stringIndex)= SequenceOfLettersOrDigits;◄ Priority: -11, aStack: 1, method: GetNameFromSource
      */
     _a.Allocate();

     GetNameFromSource(
        stringIndex: out _a.PeekRef(0)._Int32
        );

     /* Reduction 9
      * *Startsymbol= Gap, Name(Int32 stringIndex);◄ method: AssignNameToSymbol, aStack: -1
      */

     AssignNameToSymbol();

     goto ApplyStartsymbolDefinition2;
     }
  Debug.Assert(LexerInput >= ClassifierResult.Letter);
  inputClassifier.AcceptSymbol();
  /* Reduction 14, aStack: -1
   * SequenceOfLettersOrDigits= SequenceOfLettersOrDigits, LetterOrDigit(int index);◄ Priority: -12, method: AddCharToName, aStack: -1
   */

  AddCharToName(
     index: _a.PeekRef(0)._int
     );

  _a.Free();
  goto State3;

State6:
  /* State 6 */
  const String StateDescription6 =
       "String(Int32 stringIndex)= Apostrophe(int startIndex), StringCharacterSequence, ►Apostrophe(int endIndex);\r\n"
     + "StringCharacterSequence= StringCharacterSequence, ►StringCharacter;";
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput == ClassifierResult.Apostrophe)
     {
     inputClassifier.AcceptSymbol();
     /* Reduction 18, aStack: -1
      * String(Int32 stringIndex)= Apostrophe(int startIndex), StringCharacterSequence, Apostrophe(int endIndex);◄ method: GetStringIndex, aStack: -1
      */

     GetStringIndex(
        startIndex: _a.PeekClear(-1)._int,
        endIndex: _a.PeekRef(0)._int,
        stringIndex: out _a.PeekRef(-1)._Int32
        );

     _a.Free();
     /* Reduction 11
      * StartsymbolString(Int32 stringIndex)= String(Int32 stringIndex);◄ method: AssignStringToStartsymbol
      * then: *Startsymbol= Gap, StartsymbolNumber(Int32 value);◄ aStack: -1
      */

     AssignStringToStartsymbol();

     goto ApplyStartsymbolDefinition2;
     }
  if (LexerInput == ClassifierResult.CSharpStart || LexerInput == ClassifierResult.CSharpEnd)
     {
     if (ErrorHandler(6, StateDescription6, LexerInput))
        goto State6;
     goto EndWithError1;
     }
  Debug.Assert(LexerInput != ClassifierResult.CSharpStart && LexerInput != ClassifierResult.CSharpEnd
     && LexerInput != ClassifierResult.Apostrophe);
  inputClassifier.AcceptSymbol();
  /* Reduction 17, aStack: -1
   * StringCharacter= anyCharacterExceptApostrophe(int i);◄ aStack: -1
   */
  _a.Free();
  goto State6;

State7:
  /* State 7 */
  const String StateDescription7 =
       "Comment= \"/\"(int i1), ►\"*\"(int i2), CommentcharacterSequenceEndingWithAsterisk, \"/\"(int iEnd);\r\n"
     + "Comment= \"/\"(int i1), ►\"/\"(int i2);";
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput == ClassifierResult.Slash)
     {
     inputClassifier.AcceptSymbol();
     /* Reduction 19, aStack: -2
      * Comment= "/"(int i1), "/"(int i2);◄ method: SlashSlashComment, aStack: -2
      */

     SlashSlashComment();

     _a.Free(2);
     goto Reduce1;
     }
  if (LexerInput != ClassifierResult.Asterisk)
     {
     if (ErrorHandler(7, StateDescription7, LexerInput))
        goto State7;
     goto EndWithError1;
     }
  Debug.Assert(LexerInput == ClassifierResult.Asterisk);
  inputClassifier.AcceptSymbol();
State8:
  /* State 8 */
  const String StateDescription8 =
       "Comment= \"/\"(int i1), \"*\"(int i2), ►CommentcharacterSequenceEndingWithAsterisk, \"/\"(int iEnd);";
  LexerInput = inputClassifier.PeekSymbol();
  if (LexerInput == ClassifierResult.Asterisk)
     goto AcceptReduce21;
  if (LexerInput == ClassifierResult.CSharpStart || LexerInput == ClassifierResult.CSharpEnd)
     {
     if (ErrorHandler(8, StateDescription8, LexerInput))
        goto State8;
     goto EndWithError1;
     }
  Debug.Assert(LexerInput != ClassifierResult.CSharpStart && LexerInput != ClassifierResult.CSharpEnd
     && LexerInput != ClassifierResult.Asterisk);
  goto AcceptReduce20;

Reduce12:
  /* Reduction 12, aStack: -1
   * CharacterToPassOn= OneCharacterToPassOn(int index);◄ method: TranslateCharToLexerResult, aStack: -1
   * then: *Startsymbol= Gap, CharacterToPassOn;◄
   */

  TranslateCharToLexerResult(
     index: _a.PeekRef(0)._int
     );

  _a.Free();
  goto EndOfGeneratedCode1;

ApplyStartsymbolDefinition2:
  // Halt: a definition of the startsymbol with 1 attributes has been recognized.
AttributesOfSymbol.CopyAndRemoveFrom(_a, 1);
  goto EndOfGeneratedCode1;

EndWithError1:
  // This point is reached after an input error has been found
  _a.Free(_a.Count - AttributeStackInitialCount);

EndOfGeneratedCode1:
  ;
#endregion grammlator generated Sun, 12 Apr 2020 16:38:30 GMT by Grammlator version 0:21 (build Sun, 12 Apr 2020 07:40:56 GMT)
#pragma warning restore IDE0059 // Der Wert, der dem Symbol zugeordnet ist, wird niemals verwendet.
         }
      }
   }
