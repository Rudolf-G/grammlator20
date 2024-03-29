using GrammlatorRuntime;

using System;
using System.ComponentModel;
using System.Diagnostics;
using System.Text;
using System.Windows.Xps;

namespace grammlator;

/* The input classifier uses a SpanReader as input and a grammlator StackOfMultiTypeElements as grammlator attribute stack.
 * It implements the methods PeekSymbol() and AcceptSymbol() fetching line spans from the input as needed.
 * It ignores
 *    lines which are not between "#region grammlator grammar" and "endregion grammar", 
 *    unicode-separators at the beginning of a line and following the grammar line markers "//|",
 *    empty lines, C# comment lines and C# pragma lines (special handling see below) and
 *    #region and #endregion lines (special handling of "#region grammlator .." and "#endregion grammlator ...". 
 *    
 * It synthesizes CSharpStart and CSharpEnd symbols between lines starting with grammar line markers and other lines.
 * In doing so it interpretes empty lines, C# comment lines and C# pragma lines as empty CS code lines.
 * 
 * The input classifier calls  GlobalVariables.OutputMessageAndPosition(...) when it detects a not expected 
 *    end of source or a not expected 
 *    "region grammlator" not followed by "generated". * 
 * 
 * */


/// <summary>
/// Grammlator syntaxchecker Level 0: input for lexical analysis Lex1
/// </summary>
internal sealed class P1cInputClassifier : IGrammlatorInput<ClassifierResult>
{ //GrammlatorInput<ClassifierResult> {

   // TODO use the Rune struct https://docs.microsoft.com/en-gb/dotnet/api/system.text.rune?view=netcore-3.1
   // and the EnumerateRunes Method
   // https://docs.microsoft.com/en-gb/dotnet/api/system.memoryextensions.enumeraterunes?view=netcore-3.1
   //  foreach (Rune r in s){}
   // or explicit call of iterators https://docs.microsoft.com/de-de/dotnet/csharp/iterators

   /// <summary>
   /// Constructor
   /// </summary>
   /// <param name="SourceReader">the reader provides "ReadOnlyMemory<Char> ReadLine()" and "int LineNumber", "int Position" "int EoLLength"</param>
   /// <param name="attributeStack">The <paramref name="attributeStack"/> is used to store the attributes of the recognized character</param>
   public P1cInputClassifier(
         SpanReaderWithCharacterAndLineCounter SourceReader,
         StackOfMultiTypeElements attributeStack)
   {
      this.SourceReader = SourceReader;
      this._a = attributeStack;
      Accepted = true;
      InputLine = ReadOnlyMemory<Char>.Empty;
      CurrentColumn = InputLine.Length + 1; // Alike end of line had been recognized and accepted
      StartOf1stGrammlatorGeneratedLine = -1;

      // Grammlator parameterization
      GrammarlineMarker = GlobalSettings.CSharpGrammarLineMarker.Value; // = "//|"
      CSharpCommentlineMarker = GlobalSettings.CSharpCommentlineMarker.Value; // = "//"
      CSharpPragma = GlobalSettings.CSharpPragmaMarker.Value; // = "#pragma"

      for (int i = 0; i < ClassifierTable.Length; i++)
         ClassifierTable[i] = GetResultFromChar((char)i);
   }

   /// <summary>
   /// <para>When <see cref="Accepted"/> is false, then calls to <see cref="PeekSymbol"/> do nothing and </para>
   /// <para>calls to <see cref="AcceptSymbol"/> push all the attributes of Symbol from a local stack to the attribute stack and set accepted to true.</para>
   /// <para>Else calls to <see cref="AcceptSymbol"/> do nothing and calls to <see cref="PeekSymbol"/> retrieve the next symbol and set accepted to false. </para>
   /// </summary>
   public Boolean Accepted
   {
      get; // may be evaluated in semantic methods before accessing context
      private set;
   }


#pragma warning disable IDE1006 // Benennungsstile
   ///<summary>
   /// The attributeStack <see cref="_a"/> of <see cref="GrammlatorRuntime"/> 
   /// is used  by grammlator generated code 
   /// a) to return the attributes of output symbols (if any)
   /// b) to get the attributes of input symbols. 
   /// Caution: Access to the elements of the attribute stack is not type save.
   ///</summary>
   private StackOfMultiTypeElements _a
   {
      get; set;
   }
#pragma warning restore IDE1006 // Benennungsstile

   /// <summary>
   /// After the first call of <see cref="PeekSymbol"/> <see cref="Symbol"/> will have a defined value.
   /// The value can only be changed by <see cref="PeekSymbol"/>.
   /// </summary>
   public ClassifierResult Symbol
   {
      get; private set;
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
   private Int32 CurrentColumn; // Initialized by constructor

   /// <summary>
   /// The position in SourceReader of the last peeked Symbol. Accepting the symbol by AcceptSymbol will increment the value by 1.
   /// </summary>
   public Int32 CurrentPosition
   {
      get { return SourceReader.Position - InputLine.Length + CurrentColumn; } // TOCHECK replace CurrentPosition by a local variable
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

      if (CurrentColumn < InputLineLengthWithoutEol)
      {
         c = InputLine.Span[CurrentColumn];
      }
      else if (CurrentColumn == InputLineLengthWithoutEol)
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
               Debug.Assert(CurrentColumn <= InputLineLengthWithoutEol);
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

      Symbol = GetResultFromTable(c);
      PositionOfSymbol = CurrentPosition;
      return Symbol;
   }

   // local constants
   private const Char EndOfLineCharacter = '\r';

   // Set by constructor
   private readonly SpanReaderWithCharacterAndLineCounter SourceReader;
   // private cAttributeStack _a;

   // Local fields
   private ReadOnlyMemory<Char> InputLine;
   private Int32 InputLineLengthWithoutEol; // = 0;

   // The translation of input characters to ClassifierResults is accelarated using an array
   const int TableLength = 128; // 128 is sufficient, no considerable benefit if including characters such as umlauts 
   private readonly ClassifierResult[] ClassifierTable = new ClassifierResult[TableLength];

   private ClassifierResult GetResultFromTable(char c)
   {
      if (c < TableLength)
         return ClassifierTable[c]; // This handles all ASCII characters !

      if (Char.IsLetter(c)) // resolves characters such as umlauts
         return ClassifierResult.Letter;
      if (Char.IsWhiteSpace(c))
         return ClassifierResult.WhiteSpace;
      if (Char.IsDigit(c))
         return ClassifierResult.Digit;

      return ClassifierResult.OtherCharacter;
   }

   /// <summary>
   /// This method is used to build the ClassifierTable
   /// </summary>
   /// <param name="c"></param>
   /// <returns></returns>
   private static ClassifierResult GetResultFromChar(char c)
   {
      if (Char.IsWhiteSpace(c))
         return ClassifierResult.WhiteSpace;
      if (Char.IsLetter(c))
         return ClassifierResult.Letter;
      if (c >= '0' && c <= '9')
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
         '\'' => ClassifierResult.Apostrophe,  // Apostrophe
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
      InputLine = SourceReader.ReadLine();
      InputLineLengthWithoutEol = InputLine.Length - SourceReader.EoLLength;
      ReadOnlySpan<char> InputSpan = InputLine.Span;


      if (InputLine.IsEmpty)
      {
         // end of source: Output message and throw exception
         GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Abort,
             "The end of file has been reached prematurely by the lexical analyzer",
              SourceReader.Position);

         Debug.Fail("This Debug line should be never executed, because ...Message(..Abort..) has been called");
      }

      Debug.Assert(InputLine.Length >= 1);

      CurrentColumn = 0;

      // Skip unicode separators
      CurrentColumn = InputSpan.IndexBehindSeparators(0);

      Int32 newPosition;

      if ((newPosition = InputSpan.IndexBehindMarkers(CurrentColumn, GrammarlineMarker)) > -1)
      {
         // found grammar line (starting with "//|"), start evaluation 
         TypeOfInputline = TypeOfInputlineEnum.Grammar;

         CurrentColumn = newPosition;
         // start interpreting after the GrammarlineMarker and white space
         CurrentColumn = InputLine.Span.IndexBehindSeparators(CurrentColumn);
         if (CurrentColumn >= InputLineLengthWithoutEol)
            return ' '; // do not ignore line so that switching between CSharp and Grammar lines can be handled 
         return InputSpan[CurrentColumn];
      }
      else if (CurrentColumn >= InputLineLengthWithoutEol)
      {
         // found empty line (only CR or LF or CR LF) or line consisting only of separators: 
         TypeOfInputline = TypeOfInputlineEnum.CSharp; // white space is interpreted as CSharp
         CurrentColumn = InputLineLengthWithoutEol; // is >=0 
         return ' ';
      }
      else if (InputSpan.IndexBehindMarkers(CurrentColumn, CSharpCommentlineMarker) > -1
               || InputSpan.IndexBehindMarkers(CurrentColumn, CSharpPragma) > -1)
      {
         // C# comment lines and C# Pragmas
         // handle as CSharp line consisting of only one ' '
         TypeOfInputline = TypeOfInputlineEnum.CSharp;
         CurrentColumn = InputLineLengthWithoutEol; // is >=0 
         return ' ';
      }
      else if (InputSpan.IndexBehindMarkers(
         CurrentColumn, GlobalSettings.RegionEnd.Value, GlobalSettings.RegionGrammarMarker.Value) > -1
         )
      {
      // found "endregion grammar .."
      // skip to "region grammar" or "#region grammlator generated",
      // output error message if end of file
      SkipToNextRegion:
         // a) skip all text to position behind "region"
         Int32 StartOfMarkedLine = SourceReader.ReadAndCopyUntilMarkedLineFound(null,
            copy: false, false, // do not copy, but skip marked line
            GlobalSettings.RegionBegin.Value);

         InputLine = SourceReader.Source[StartOfMarkedLine..SourceReader.Position];
         InputLineLengthWithoutEol = InputLine.Length - SourceReader.EoLLength;
         InputSpan = InputLine.Span;

         CurrentColumn = 0;

         if (StartOfMarkedLine < 0)
         {
            // Reached end of Input
            // end of source: Output message and throw exception
            GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Abort,
                $"The end of file has been reached prematurely by the lexical analyzer while searching "
                + $"{GlobalSettings.RegionBegin.Value}",
                SourceReader.Position
                );

            Debug.Fail("This Debug line should be never executed, because ...Message(..Abort..) has been called");

         }

         Int32 i = SourceReader.Source.Span.IndexBehindMarkers(StartOfMarkedLine, GlobalSettings.RegionBegin.Value);

         if (i < 0)
            Debug.Fail($"Has already been tested and is not again {GlobalSettings.RegionBegin.Value}");

         if (SourceReader.Source.Span.IndexBehindMarkers(i, GlobalSettings.RegionGrammarMarker.Value) > 0)
         {
            // found "#region grammar": skip this line and continue parsing with next line
            goto ReadNextLine;

         }
         else if ((i = SourceReader.Source.Span.IndexBehindMarkers(i, GlobalSettings.RegionGrammlatorMarker.Value)) > 0)
         {
            // found "#region grammlator", expect "generated": 
            i = SourceReader.Source.Span.IndexBehindMarkers(i, GlobalSettings.RegionGeneratedMarker.Value);
            if (i < 0)
            {
               // expected but didn't find "generated"
               GlobalVariables.OutputMessageAndPosition(MessageTypeOrDestinationEnum.Abort,
                   $"Found \"{GlobalSettings.RegionBegin.Value} {GlobalSettings.RegionGrammlatorMarker.Value}\""
                   + $"which is not followed by \"{GlobalSettings.RegionGeneratedMarker.Value}\"",
                   SourceReader.Position
                   );
            }
            // found "#region grammlator generated"
            StartOf1stGrammlatorGeneratedLine = StartOfMarkedLine;
            // the parser must see the '#' to interpret it as end of grammar
            TypeOfInputline = TypeOfInputlineEnum.Grammar;
            // start interpreting at the '#', which is at position CurrentColumn (==0)
            return InputSpan[0];
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
         return InputSpan[CurrentColumn];
      }

   } // ReadLineFromInput()

   /// <summary>
   /// Skips all input lines which are not grammar lines
   /// and sets the next not yet accepted symbol to <see cref="ClassifierResult.CSharpEnd"/>
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
      CurrentColumn = InputLine.Length + 1;
   }
} // class Lex0
