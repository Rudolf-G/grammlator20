using GrammlatorRuntime;

using System;
using System.ComponentModel;
using System.Diagnostics;
using System.Text;
using System.Windows.Xps;

namespace grammlator {
   /// <summary>
   /// This enum defines the characters and character groups recognized by InputClassifier and used by Lexer.
   /// </summary>
   public enum ClassifierResult {
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
      At, // @

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

   public static class ClassifierResultExtensions {
      public static String MyToString(this ClassifierResult cr)
      {
         const String MyDisplay = "=,|;-+:%*([{)]}#xx?x/x\"xx";

         if ((Int32)cr >= MyDisplay.Length)
            return cr.ToString();

         Char result = MyDisplay[(Int32)cr];
         if (result != 'x')
            return result.ToString();
         else
            return cr.ToString();
      }
   }
#pragma warning restore CS1591 // Fehledes XML-Kommentar für öffentlich sichtbaren Typ oder Element

   /// <summary>
   /// Grammlator syntaxchecker Level 0: input for lexical analysis Lex1
   /// </summary>
   internal class P1cInputClassifier : IGrammlatorInput<ClassifierResult> { //GrammlatorInput<ClassifierResult> {

      // TODO use the Rune struct https://docs.microsoft.com/en-gb/dotnet/api/system.text.rune?view=netcore-3.1
      // and the EnumerateRunes Method
      // https://docs.microsoft.com/en-gb/dotnet/api/system.memoryextensions.enumeraterunes?view=netcore-3.1
      //  foreach (Rune r in s){}
      // bzw. explzitem Aufruf der enumerator-Methoden https://docs.microsoft.com/de-de/dotnet/csharp/iterators

      /// <summary>
      /// Constructor
      /// </summary>
      /// <param name="SbResult"></param>
      /// <param name="SourceReader">the LineReader provides "string ReadLine()" und "int LineNumber"</param>
      /// <param name="attributeStack">The <paramref name="attributeStack"/> is used to store the attributes of the recognized character</param>
      public P1cInputClassifier(
            SpanReaderWithCharacterAndLineCounter SourceReader,
            StackOfMultiTypeElements attributeStack)
      {
         this.SourceReader = SourceReader;
         this._a = attributeStack;
         Accepted = true;
         inputLine = ReadOnlyMemory<Char>.Empty;
         CurrentColumn = inputLine.Length + 1; // Alike end of line had been recognized and accepted

         // Grammlator parameterization
         GrammarlineMarker = GlobalSettings.GrammarLineMarker.Value; // = "//|"
         CSharpCommentlineMarker = GlobalSettings.CSharpCommentlineMarker.Value; // = "//"
         CSharpPragma = GlobalSettings.CSharpPragmaMarker.Value; // = "#pragma"
      }

      /// <summary>
      /// <para>When <see cref="Accepted"/> is false, then calls to <see cref="PeekSymbol"/> do nothing and </para>
      /// <para>calls to <see cref="AcceptSymbol"/> push all the attributes of Symbol from a local stack to the attribute stack and set accepted to true.</para>
      /// <para>Else calls to <see cref="AcceptSymbol"/> do nothing and calls to <see cref="PeekSymbol"/> retrieve the next symbol and set accepted to false. </para>
      /// </summary>
      public Boolean Accepted {
         get; // may be evaluated in semantic methods before accessing context
         protected set;
      }


      ///<summary>
      /// The attributeStack <see cref="_a"/> of <see cref="GrammlatorRuntime"/> 
      /// is used  by grammlator generated code 
      /// a) to return the attributes of output symbols (if any)
      /// b) to get the attributes of input symbols. 
      /// Caution: Access to the elements of the attribute stack is not type save.
      ///</summary>
#pragma warning disable IDE1006 // Benennungsstile
      private StackOfMultiTypeElements _a {
         get; set;
      }
#pragma warning restore IDE1006 // Benennungsstile

      /// <summary>
      /// After the first call of <see cref="PeekSymbol"/> <see cref="Symbol"/> will have a defined value.
      /// The value can only be changed by <see cref="PeekSymbol"/>.
      /// </summary>
      public ClassifierResult Symbol {
         get; protected set;
      }

      Int32 PositionOfSymbol;

      /// <summary>
      /// All lines starting (after trimming) with the <see cref="GrammarlineMarker"/> (e.g. "//|"),
      /// which contain some more characters,
      /// are interpreted as lines containing grammar rules.
      /// </summary>
      private readonly String GrammarlineMarker;

      /// <summary>
      /// All lines starting (after trimming) with the <see cref="CSharpCommentlineMarker"/> are not interpreted
      /// Is "//" in the standard version of grammlator.
      /// </summary>
      private readonly String CSharpCommentlineMarker;

      /// <summary>
      /// All lines starting (after trimming) with the <see cref="CSharpPragma"/> are not interpreted
      /// Is "#pragma" in the standard version of grammlator.
      /// </summary>
      private readonly String CSharpPragma;

      /// <summary>
      /// AcceptSymbol marks Symbol as accepted and pushs its attributes on the attribute stack.
      /// AcceptSymbol has no effect, if Symbol is already accepted.
      /// </summary>
      public void AcceptSymbol()
      {
         //if (Accepted)
         //   return;
         Accepted = true;

         // Push position to the stack of attributes
         _a.Push(new MultiTypeStruct() { _Int32 = PositionOfSymbol });
         // advance input to next character if Symbol was from inputline
         if (!(Symbol == ClassifierResult.CSharpStart || Symbol == ClassifierResult.CSharpEnd))
            CurrentColumn++;
      }

      /// <summary>
      /// Position of <see cref="Symbol"/> in the actual line. Is always &gt;=0. May be == inputLine.Length SourceReader.EoLLength, if end of line is reached. 
      /// May be inputLine.Length SourceReader.EoLLength + 1 if end of line is accepted.
      /// <para>Is the column of the last returned ClassifierResult as long as Accpted is false. AcceptSymbol increments the column</para>
      /// </summary>
      public Int32 CurrentColumn {
         get; private set;
      } // Initialized by constructor

      /// <summary>
      /// The position in SourceReader of the last peeked Symbol. Accepting the symbol by AcceptSymbol will increment the value by 1.
      /// </summary>
      public Int32 CurrentPosition {
         get { return SourceReader.Position - inputLine.Length + CurrentColumn; } // TOCHECK replace CurrentPosition by a local variable
      }

      /// <summary>
      /// Returns CurrentPosition
      /// </summary>
      public Int32 InputPosition // TOCHECK  replace InputPosition by CurrentPosition (without lineNumber and columnNumber)
          => CurrentPosition;

      /// <summary>
      /// Checks if actual line  starts with the sequence of strings optionally separated by withespace
      /// </summary>
      /// <param name="markers">markers = sequence of strings</param>
      /// <returns>treu if the line starts with the markers</returns>
      public Boolean IsMarkedLine(params String[] markers)
         => inputLine.Span.IndexBehindMarkers(0, markers) > -1;

      /// <summary>
      /// Makes the next not accepted symbol available in Symbol and its attributes in AttributesOfSymbol.
      /// Multiple calls to <see cref="PeekSymbol"/> without intermediate calls to <see cref="AcceptSymbol"/> do not have any effect.
      /// At the end of each input line a EndOfLine is added.
      /// </summary>
      public ClassifierResult PeekSymbol()
      {
         if (!Accepted)
            return Symbol;

         Int32 Index;
         Char c;

         // AcceptSymbol already incremented CurrentColumn
         Accepted = false;

         if (CurrentColumn < inputLine.Length - SourceReader.EoLLength)
         {
            c = inputLine.Span[CurrentColumn];
         }
         else if (CurrentColumn == inputLine.Length - SourceReader.EoLLength)
         {
            c = EndOfLineCharacter;
         }
         else
         {
            c = ReadAndPreprocessLineFromInput();

            if (wasGrammarLine != (TypeOfInputline == TypeOfInputlineEnum.Grammar))
            {
               // Changing from grammlator line(s) to CSharp line(s) or vice versa
               if (TypeOfInputline == TypeOfInputlineEnum.Grammar)
               {
                  // first grammar line after CSharp line(s), comment line(s) and grammar line marker skipped
                  Debug.Assert(CurrentColumn <= inputLine.Length - SourceReader.EoLLength);
                  Symbol = ClassifierResult.CSharpEnd;
               }
               else
               { // First CSharp line after grammar lines, comment line(s) and white space skipped
                 // Debug.Assert(CurrentColumn == 0);
                  Symbol = ClassifierResult.CSharpStart;
               }

               // c = EndOfLineCharacter;
               wasGrammarLine = (TypeOfInputline == TypeOfInputlineEnum.Grammar);


               // return Symbol == CharacterEnum.CSharpStart or == CharacterEnum.CSharpEnd with attribute EndOfLineCharacter
               PositionOfSymbol = CurrentPosition;
               return Symbol;
            }
         }

         PositionOfSymbol = CurrentPosition;

         if (Char.IsWhiteSpace(c))
            Symbol = ClassifierResult.WhiteSpace;
         else if (Char.IsLetter(c))
            Symbol = ClassifierResult.Letter;
         else if (Char.IsDigit(c))
            Symbol = ClassifierResult.Digit;
         else if ((Index = Array.IndexOf(SpecialCharacterList, c)) >= 0)
            Symbol = assignedSymbol[Index];
         else
            Symbol = ClassifierResult.OtherCharacter;

         PositionOfSymbol = CurrentPosition;
         return Symbol;
      }

      // local constants
      private const Char EndOfLineCharacter = '\r';

      // Set by constructor
      private readonly SpanReaderWithCharacterAndLineCounter SourceReader;
      // private cAttributeStack _a;

      // Local fields
      private ReadOnlyMemory<Char> inputLine;

      // The translation of input characters to ClassifierResults is supported by two arrays

      /// <summary>
      /// The <see cref="SpecialCharacterList"/> lists all special characters used in grammlator
      /// so that they can be mapped by the array <see cref="assignedSymbol"/> to elements of <see cref="ClassifierResult"/>.
      /// Whitespace, digits, standard letters and all other characters are handled separately.
      /// </summary>
      private readonly Char[] SpecialCharacterList =
          new Char[] { // the order of the characters listed here must correspond to assignedSymbol
               '=', ',', '|',
                ';', '+', ':', '%',
                '(', '[', '{',
               ')', ']', '}', '#',
                '?','*','-','@',
                '\"', '/',
               '\\', '_', '.'
          };

      /// <summary>
      /// This array maps each character in the list <see cref="SpecialCharacterList"/> to a symbol
      /// </summary>
      private readonly ClassifierResult[] assignedSymbol = new ClassifierResult[]
      { // the order of the enum elements listed here must correspond to SpecialCharacterList
            ClassifierResult.DefiningSymbol, // =
            ClassifierResult.Comma, // ,
            ClassifierResult.DefinitionSeparatorSymbol, // |

            ClassifierResult.TerminatorSymbol, // ;
            ClassifierResult.Plus, // +
            ClassifierResult.Colon, // *
            ClassifierResult.Percent, // %

            ClassifierResult.GroupStart, // (
            ClassifierResult.OptionStart, // [
            ClassifierResult.RepeatStart, // {

            ClassifierResult.GroupEnd, // )
            ClassifierResult.OptionEnd, // ]
            ClassifierResult.RepeatEnd, // }
            ClassifierResult.NumberSign, // #

            ClassifierResult.Questionmark, // ?
            ClassifierResult.Asterisk, // *
            ClassifierResult.Minus, // -
            ClassifierResult.At, // @

            ClassifierResult.Quotationmark, // "
            ClassifierResult.Slash, // /

            // The following characters are interpreted as letters in additon to all characters of the class char.IsLetter(c)
            ClassifierResult.Letter, // Backslash (escape symbol of unicode sequences \u006) // TODO translate unicode escape sequences to characters
            ClassifierResult.Letter, // Underline
            ClassifierResult.Letter  // Point
      };

      private Boolean wasGrammarLine = true; // the first grammar line  is not to be interpreted as change from method lines to grammar lines

      private enum TypeOfInputlineEnum { Grammar, CSharp };
      private TypeOfInputlineEnum TypeOfInputline;


      /// <summary>
      /// Read and analyze next line, set resulting TypeOfInputline. Set CurrentColumn to a column after skipping separators and the grammar line markers.
      /// Return ' ' for empty lines, comment lines and grammar lines with only separators else return character at CurrentColumn.
      /// </summary>
      /// <returns>the 1st character of t</returns>
      private Char ReadAndPreprocessLineFromInput()
      {


         // Get next line 
         inputLine = SourceReader.ReadLine();

         if (inputLine.IsEmpty)
         {
            // end of source: Output message and throw exception
            GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Abort,
                "The end of file has been reached prematurely by the lexical analyzer",
                 SourceReader.Position);

            Debug.Fail("This Debug line should be never executed, because ...Message(..Abort..) has been called");
         }

         Debug.Assert(inputLine.Length >= 1);

         CurrentColumn = 0;

         // Skip unicode separators
         CurrentColumn = inputLine.Span.IndexBehindSeparators(0);

         Int32 newPosition;

         if ((newPosition = inputLine.Span.IndexBehindMarkers(CurrentColumn, GrammarlineMarker)) > -1)
         {
            // found grammar line, start evaluation 
            TypeOfInputline = TypeOfInputlineEnum.Grammar;

            CurrentColumn = newPosition;
            // start interpreting after the GrammarlineMarker and white space
            CurrentColumn = inputLine.Span.IndexBehindSeparators(CurrentColumn);
            if (CurrentColumn >= inputLine.Length - SourceReader.EoLLength)
               return ' '; // do not ignore line so that switching between CSharp and Grammar lines can be handled 
            return inputLine.Span[CurrentColumn];
         }
         else if (CurrentColumn >= inputLine.Length - SourceReader.EoLLength)
         {
            // found empty line (only CR or LF or CR LF) or line consisting only of separators: 
            TypeOfInputline = TypeOfInputlineEnum.CSharp; // white space is interpreted as CSharp
            CurrentColumn = inputLine.Length - SourceReader.EoLLength; // is >=0 
            return ' ';
         }
         else if (inputLine.Span.IndexBehindMarkers(CurrentColumn, CSharpCommentlineMarker) > -1
                  || inputLine.Span.IndexBehindMarkers(CurrentColumn, CSharpPragma) > -1)
         {
            // C# comment lines and C# Pragmas
            // handle as CSharp line consisting of only one ' '
            TypeOfInputline = TypeOfInputlineEnum.CSharp;
            CurrentColumn = inputLine.Length - SourceReader.EoLLength; // is >=0 
            return ' ';
         }
         else if (inputLine.Span.IndexBehindMarkers(CurrentColumn, GlobalSettings.EndregionString.Value, GlobalSettings.GrammarString.Value) > -1)
         {
            // found "#endregion"
            // handle all lines (in the grammar region) starting with "endregion...grammar" as grammar lines so that the parser can
            // recognize the # as TerminatorAtEndOfGrammar
            TypeOfInputline = TypeOfInputlineEnum.Grammar;
            // start interpreting at the first character of endregion, which is at position CurrentColumn
            return inputLine.Span[CurrentColumn];
         }
         else
         {
            TypeOfInputline = TypeOfInputlineEnum.CSharp;
            return inputLine.Span[CurrentColumn];
         }

      } // ReadLineFromInput()

      /// <summary>
      /// Skips all input lines which are not grammar lines
      /// and sets the next noty yet accepted symbol to <see cref="ClassifierResult.CSharpEnd"/>
      /// </summary>
      public void SkipToEndOfCSLines()
      {
         Debug.Assert(TypeOfInputline != TypeOfInputlineEnum.Grammar);
         while (TypeOfInputline != TypeOfInputlineEnum.Grammar)
            _ = ReadAndPreprocessLineFromInput();
         Symbol = ClassifierResult.CSharpEnd;

         PositionOfSymbol = CurrentPosition;

         wasGrammarLine = true;
         Accepted = false;
      }

      public void IgnoreAllCharactersUntilEndOfLine()
      {
         Accepted = true;
         CurrentColumn = inputLine.Length + 1;
      }
   } // class Lex0
}
