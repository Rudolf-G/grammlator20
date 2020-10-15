using GrammlatorRuntime;

using System;
using System.ComponentModel;
using System.Diagnostics;
using System.Text;
using System.Windows.Xps;

namespace grammlator {

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
      /// <param name="SourceReader">the LineReader provides "string ReadLine()" and "int LineNumber"</param>
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
         StartOf1stGrammlatorGeneratedLine = -1;

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


#pragma warning disable IDE1006 // Benennungsstile
      ///<summary>
      /// The attributeStack <see cref="_a"/> of <see cref="GrammlatorRuntime"/> 
      /// is used  by grammlator generated code 
      /// a) to return the attributes of output symbols (if any)
      /// b) to get the attributes of input symbols. 
      /// Caution: Access to the elements of the attribute stack is not type save.
      ///</summary>
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
      /// Makes the next not accepted symbol available in Symbol and its attributes in AttributesOfSymbol.
      /// Multiple calls to <see cref="PeekSymbol"/> without intermediate calls to <see cref="AcceptSymbol"/> do not have any effect.
      /// At the end of each input line a EndOfLine is added.
      /// </summary>
      public ClassifierResult PeekSymbol()
      {
         if (!Accepted)
            return Symbol;

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

         Symbol = GetResultFromChar(c);
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

      private ClassifierResult GetResultFromChar(char c)
      {
         if (Char.IsWhiteSpace(c))
            return ClassifierResult.WhiteSpace;
         if (Char.IsLetter(c))
            return ClassifierResult.Letter;
         if (Char.IsDigit(c))
            return ClassifierResult.Digit;

         return c switch
         {
            '=' => ClassifierResult.DefiningSymbol,
            ',' => ClassifierResult.Comma, // ,
            '|' => ClassifierResult.DefinitionSeparatorSymbol, // |

            ';' => ClassifierResult.TerminatorSymbol, // ;
            '+' => ClassifierResult.Plus, // +
            ':' => ClassifierResult.Colon, // :
            '%' => ClassifierResult.Percent, // %

            '(' => ClassifierResult.GroupStart, // (
            '[' => ClassifierResult.OptionStart, // [
            '{' => ClassifierResult.RepeatStart, // {

            ')' => ClassifierResult.GroupEnd, // )
            ']' => ClassifierResult.OptionEnd, // ]
            '}' => ClassifierResult.RepeatEnd, // }
            '#' => ClassifierResult.NumberSign, // #

            '?' => ClassifierResult.Questionmark, // ?
            '*' => ClassifierResult.Asterisk, // *
            '-' => ClassifierResult.Minus, // -
            '@' => ClassifierResult.At, // @

            '"' => ClassifierResult.Quotationmark, // "
            '/' => ClassifierResult.Slash, // /

            // The following characters are interpreted as letters in additon to all characters of the class char.IsLetter(c)
            '\\' => ClassifierResult.Letter, // Backslash (escape symbol of unicode sequences \u006) // TODO translate unicode escape sequences to characters
            '_' => ClassifierResult.Letter, // Underline
            '.' => ClassifierResult.Letter,  // Point
            _ => ClassifierResult.OtherCharacter
         };
      }

      private Boolean wasGrammarLine = true; // the first grammar line  is not to be interpreted as change from method lines to grammar lines

      private enum TypeOfInputlineEnum { Grammar, CSharp };
      private TypeOfInputlineEnum TypeOfInputline;

      /// <summary>
      /// Index of the line containing "#region grammlator generated"
      /// </summary>
      public Int32 StartOf1stGrammlatorGeneratedLine { get; private set; }

      /// <summary>
      /// Read and analyze next line, set resulting TypeOfInputline. Set CurrentColumn to a column after skipping separators and the grammar line markers.
      /// Return ' ' for empty lines, comment lines and grammar lines with only separators else return character at CurrentColumn.
      /// </summary>
      /// <returns>the 1st character of t</returns>
      private Char ReadAndPreprocessLineFromInput()
      {


      ReadNextLine:
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
            // found grammar line (starting with "//|"), start evaluation 
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
         else if (inputLine.Span.IndexBehindMarkers(
            CurrentColumn, GlobalSettings.EndregionString.Value, GlobalSettings.GrammarString.Value) > -1
            )
         {
         // skip to "region grammar" or "#region grammlator generated", error message if end of file
         SkipToNextRegion:
            // a) skip all text to position behind "region"
            Int32 StartOfMarkedLine = SourceReader.ReadAndCopyUntilMarkedLineFound(null,
               copy: false, false, // do not copy, but skip marked line
               GlobalSettings.RegionString.Value);

            inputLine = SourceReader.Source[StartOfMarkedLine..SourceReader.Position];
            CurrentColumn = 0;

            if (StartOfMarkedLine < 0)
            {
               // Reached end of Input
               // end of source: Output message and throw exception
               GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Abort,
                   $"The end of file has been reached prematurely by the lexical analyzer while searching "
                   + $"{GlobalSettings.RegionString.Value}",
                   SourceReader.Position
                   );

               Debug.Fail("This Debug line should be never executed, because ...Message(..Abort..) has been called");

            }

            Int32 i = SourceReader.Source.Span.IndexBehindMarkers(StartOfMarkedLine, GlobalSettings.RegionString.Value);

            if (i < 0)
               Debug.Fail($"Has already been tested and is not again {GlobalSettings.RegionString.Value}");

            if (SourceReader.Source.Span.IndexBehindMarkers(i, GlobalSettings.GrammarString.Value) > 0)
            {
               // found "#region grammar": skip this line and continue parsing with next line
               goto ReadNextLine;

            }
            else if ((i = SourceReader.Source.Span.IndexBehindMarkers(i, GlobalSettings.GrammlatorString.Value)) > 0)
            {
               // found "#region grammlator", expect "generated": 
               i = SourceReader.Source.Span.IndexBehindMarkers(i, GlobalSettings.GeneratedString.Value);
               if (i < 0)
               {
                  // expected but didn't find "generated"
                  GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Abort,
                      $"Found \"{GlobalSettings.RegionString.Value} {GlobalSettings.GrammlatorString.Value}\""
                      + $"which is not followed by \"{GlobalSettings.GeneratedString.Value}\"",
                      SourceReader.Position
                      );
               }
               // found "#region grammlator generated"
               StartOf1stGrammlatorGeneratedLine = StartOfMarkedLine;
               // the parser must see the '#' to interpret it as end of grammar
               TypeOfInputline = TypeOfInputlineEnum.Grammar;
               // start interpreting at the '#', which is at position CurrentColumn (==0)
               return inputLine.Span[0];
            }
            else
            {
               // found some other "region xxx"
               goto SkipToNextRegion;
            }
            // Debug.Assert(false);
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
