﻿using System;
using System.Diagnostics;
using System.Text;

namespace grammlator
{
   /// <summary>
   /// <see cref="GrammlatorMemoryReader"/> is a StringReader which counts the read lines and makes the actual <see cref="LineNumber"/> available
   /// </summary>
   /// <remarks>
   /// Contructor of the <see cref="System.IO.StringReader"/> <see cref="GrammlatorMemoryReader"/>, assigning a string to read from
   /// </remarks>
   /// <param name="input">The input of the reader</param>
   public class GrammlatorMemoryReader(String input) : System.IO.StringReader(input)
   {

      /// <summary>
      /// the number of the actual line (0 if no line has been read)
      /// </summary>
      public Int32 LineNumber
      {
         get; private set;
      } = -1;

      /// <summary>
      /// Increment <see cref="LineNumber"/> and <see cref="ReadLine"/>
      /// </summary>
      /// <returns>The next line from the current input (string) or null if the end of the input is reached</returns>
      public override String? ReadLine()
      {
         LineNumber++;
         return base.ReadLine();
      }

      /// <summary>
      /// Read and depending on <paramref name="copy"/> and <paramref name="copyFoundLine"/> copy
      /// all lines to <paramref name="sb"/> until a line starting with the <paramref name="markers"/>, optionally separated by whitespace, is reached.
      /// </summary>
      /// <param name="sb">Depending on <paramref name="copy"/> and <paramref name="copyFoundLine"/> 
      /// lines will be appended to <paramref name="sb"/> </param>
      /// <param name="copy">if true: lines will be copied to <paramref name="sb"/></param>
      /// <param name="copyFoundLine">if true, also the found line will be copied to <paramref name="sb"/> else not</param>
      /// <param name="markers"><paramref name="markers"/> is a sequence of strings</param>
      /// <exception cref="ErrorInSourcedataException">exception thrown if end of input reached</exception>
      public Boolean ReadAndCopyUntilMarkedLineFound(StringBuilder sb, Boolean copy, Boolean copyFoundLine, params String[] markers)
      {
         String inputLine;

         ArgumentNullException.ThrowIfNull(sb);

         ArgumentNullException.ThrowIfNull(markers);

         void OutputLine(String s)
         {
            if (sb.Length > 0)
               sb.AppendLine();
            sb.Append(s);
         }

         while (true)
         {
            String? line = ReadLine();
            if (line == null)
               return false;

            inputLine = line;
            if (inputLine.AsSpan().StartsWithKeywordSequence(markers))
            {
               if (copyFoundLine)
                  OutputLine(inputLine);
               break;
            }
            if (copy)
               OutputLine(inputLine);
         }

         return true;
      }
   }

   /// <summary>
   /// Contructor of the <see cref="System.IO.StringReader"/> <see cref="GrammlatorMemoryReader"/>,
   /// assigning a string to read from
   /// </summary>
   /// <param name="source">The input of the reader</param>
   public class SpanReaderWithCharacterAndLineCounter(ReadOnlyMemory<Char> source)
   {
      public ReadOnlyMemory<Char> Source { get; private set; } = source;

      /// <summary>
      /// Number of the last read line, initial value == -1
      /// </summary>
      public Int32 LineNumber { get; private set; } = -1;

      /// <summary>
      /// Number of the EOL character(s) at the end of the last read line: 0, 1 or 2
      /// </summary>
      public Int32 EoLLength { get; private set; } = -1;

      /// <summary>
      /// Position of the next character to read, initial value == 0
      /// </summary>
      public Int32 Position { get; private set; } //  = 0;

      /// <summary>
      /// Finds the end of the line
      /// </summary>
      /// <returns>Returns a slice of Source starting at Position and ending after the next '\r', '\n' or '\r''\n' or end of Source. Sets Position to the next character after the line. </returns>
      public ReadOnlyMemory<Char> ReadLine()
      {

         if (Position >= Source.Length)
            return ReadOnlyMemory<Char>.Empty;

         LineNumber++;

         ReadOnlySpan<Char> Remainder = Source.Span[Position..];

         // Find end of line
         Int32 eolIndex = Remainder.IndexOfAny<Char>('\r', '\n');
         ReadOnlyMemory<Char> result;
         // Cases: no end of line, '\r', "\r\n" or '\n' 
         EoLLength = 1; // default: /r or /n
         if (eolIndex < 0)
         { // no end of line
            EoLLength = 0;
            result = Source[Position..^0];
            Position = Source.Length; // last line of input
            return result;
         }
         // else
         if (Remainder[eolIndex] == '\r'
            && eolIndex + 1 < Remainder.Length
            && Remainder[eolIndex + 1] == '\n')
         {
            EoLLength = 2; // "\r\n"
         }

         result = Source[Position..(Position + eolIndex + EoLLength)];
         Position += result.Length;
         return result;
      }

      /// <summary>
      /// Read and depending on <paramref name="copy"/> and <paramref name="copyLineWithMarkers"/> copy
      /// all lines to <paramref name="sb"/> until a line starting with the <paramref name="markers"/>, optionally separated by whitespace, is reached.
      /// Returns the position of the 1st character of the 1st marker in the source or -1 if not found.
      /// </summary>
      /// <param name="sb">Depending on <paramref name="copy"/> and <paramref name="copyLineWithMarkers"/> 
      /// lines will be appended to <paramref name="sb"/> </param>
      /// <param name="copy">if true: lines will be copied to <paramref name="sb"/></param>
      /// <param name="copyLineWithMarkers">if true, also the found line will be copied to <paramref name="sb"/> else not</param>
      /// <param name="markers"><paramref name="markers"/> is a sequence of strings</param>
      /// <returns>Position of the 1st character of the 1st marker in the source</returns>
      public Int32 ReadAndCopyUntilMarkedLineFound(StringBuilder? sb, Boolean copy, Boolean copyLineWithMarkers, params String[] markers)
      {

         if (copy && sb == null)
            throw new ArgumentNullException(nameof(sb));

         ArgumentNullException.ThrowIfNull(markers);

         Int32 StartPosition = Position;
         Int32 StartOfLine;
         ReadOnlySpan<Char> lineSpan;

         while (true)
         {

            StartOfLine = Position;

            lineSpan = ReadLine().Span; // sets Position to next line

            if (lineSpan.IsEmpty)
               return -1; // end of Source: didn't find the markers

            if (lineSpan.IndexBehindMarkers(0, markers) > 0)
            {
               // found line starting with the markers
               if (copy)
               {
                  if (copyLineWithMarkers)
                     sb!.Append(Source[StartPosition..Position]);
                  else // copy only the lines up to the line with markers
                     sb!.Append(Source[StartPosition..StartOfLine]);
               } // else skip the lines
               break; // end of search
            }
            // continue searching
         }

         return StartOfLine; //  + lineSpan.IndexBehindSeparators(0);
      }

      public void CopyFromTo(StringBuilder sb, Int32 from, Int32 to)
         => sb.Append(Source[from..to]);

      public void ReadAndCopyToEnd(StringBuilder sb)
      {
         sb.Append(Source[Position..Source.Length]);
         Position = Source.Length;
      }


   }
}

