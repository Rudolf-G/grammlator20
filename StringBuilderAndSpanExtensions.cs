using System;
using System.Text;

namespace grammlator;

/// <summary>
/// Defines .TrimEnd (params Char[] TrimChars)
/// </summary>
internal static class StringbuilderExtensions
{
   internal static StringBuilder TrimEnd(this StringBuilder Sb, params Char[] TrimChars)
   {
      if (Sb.Length == 0)
         return Sb;
      Int32 trimCharIndex = -1;
      while (++trimCharIndex < TrimChars.Length)
      {
         if (Sb[^1] == TrimChars[trimCharIndex])
         {
            // found one of the TrimChars
            Sb.Length--; // trim
            if (Sb.Length == 0)
               return Sb; // return if Sb is empty
            trimCharIndex = -1; // continue with first trimChar
         }
      }
      return Sb; // no (more) match
   }
}

internal static class SpanExtensions
{

   /// <summary>
   /// Count the number of lines in the string
   /// </summary>
   /// <param name="thisSpan"></param>
   /// <returns>the number of newline-characters in the string + 1, 0 if the string is null or empty</returns>
   internal static Int32 CountLines(this ReadOnlySpan<char> thisSpan)
   {
      return thisSpan.IsEmpty ? 0 : thisSpan.Count(c => c == '\n') + 1;
   }

   /// <summary>
   /// Counts the elements of the Span for which the <paramref name="predicate"/> is true
   /// </summary>
   /// <param name="thisSpan"></param>
   /// <param name="predicate"></param>
   /// <returns>the number of elements of the span for which the <paramref name="predicate"/> is true</returns>
   public static int Count(this ReadOnlySpan<char> thisSpan, Func<char, bool> predicate)
   {
      int Counter = 0;
      foreach (char c in thisSpan)
         if (predicate(c))
            Counter++;
      return Counter;
   }

   /// <summary>
   /// Increments <paramref name="position"/> while <paramref name="thisSpan"/>[<paramref name="position"/>] IsSeparator (including space, line and paragraph separators 
   /// but not line feed ...)
   /// </summary>
   /// <param name="thisSpan">the String to check</param>
   /// <param name="position">returns new Position less or equal s.Length</param>
   /// <returns>string <paramref name="ThisString"/> to allow catenation of operations</returns>      
   internal static ReadOnlySpan<char> SkipSeparator(this ReadOnlySpan<char> thisSpan, ref Int32 position)
   {
      if (position < 0)
         position = 0;
      while (position < thisSpan.Length && Char.IsSeparator(thisSpan[position]))
         position++;
      return thisSpan;
   }

   /// <summary>
   /// Starting at ThisSpan[start ]finds the first char which is not char.IsSeparator and
   /// returns its index.
   /// IsSeparator is true for space, line and paragraph separators but not CR, LF and FF
   /// </summary>
   /// <param name="thisSpan">the Span to check</param>
   /// <param name="start">The index of the first char of ThisSpan to check</param>
   /// <returns>returns the index of the first char which is not a separator, less or equal s.Length</returns>
   internal static Int32 IndexBehindSeparators(this ReadOnlySpan<Char> thisSpan, Int32 start)
   {
      Int32 result = start;
      if (result < 0)
         result = 0;
      while (result < thisSpan.Length && Char.IsSeparator(thisSpan[result]))
         result++;
      return result;
   }

   /// <summary>
   /// Checks whether <paramref name="thisSpan"/> contains the <paramref name="pattern"/> at the <paramref name="position"/>
   /// and updates <paramref name="position"/> to point to the first character after the 
   /// <paramref name="pattern"/> - else lets <paramref name="position"/> unchanged
   /// </summary>
   /// <param name="thisSpan">the Span to check</param>
   /// <param name="position">the Position to start the comparision</param>
   /// <param name="pattern">the string to test for</param>
   /// <returns>returns true and Position after pattern if pattern found start of string, else false and Position unchanged</returns>      
   internal static Boolean StartsWithAndSkip(this ReadOnlySpan<char> thisSpan, ref Int32 position, String pattern)
   {
      if (thisSpan == null || position + pattern.Length > thisSpan.Length)
         return false;

      Int32 sPosition = position;     

      if (!thisSpan[position..].StartsWith(pattern))
         return false;
      position += pattern.Length;
      return true;
   }

   /// <summary>
   /// Checks whether the string starts with a sequence of strings ignoring white space in front of each marker.
   /// Does not allocate temporary string variables in the .NET heap.
   /// </summary>
   /// <param name="thisSpan">the Span to be checked</param>
   /// <param name="keywords">The strings to be checkd for</param>
   /// <returns>true if all markers have been found</returns>      
   internal static Boolean StartsWithKeywordSequence(this ReadOnlySpan<char> thisSpan, params String[] keywords)
   {
      Boolean foundAllKeywords = true;
      Int32 column = 0;
      foreach (String keyword in keywords)
      {
         if (!thisSpan.SkipSeparator(ref column)[column..].StartsWith(keyword))
         {
            foundAllKeywords = false;
            break;
         }

         column+=keyword.Length;
      }

      return foundAllKeywords;
   }

   /// <summary>
   /// Checks whether the Span starts with a sequence of strings ignoring unicode separator characters in front of each marker.
   /// If all markers have been found the index of the next character after the last marker is returned, else -1.
   /// </summary>
   /// <param name="thisSpan">the string to be checked</param>
   /// <param name="start">the index where to start checking</param>
   /// <param name="markers">the strings to be checked for</param>
   /// <returns>the index after the last marker if success else -1</returns>
   internal static Int32 IndexBehindMarkers(this ReadOnlySpan<Char> thisSpan, Int32 start, params String[] markers)
   {
      Int32 result = start;

      for (Int32 i = 0; i < markers.Length; i++)
      {
         result = thisSpan.IndexBehindSeparators(result);

         if (!thisSpan[result..].StartsWith(markers[i]))
            return -1;

         result += markers[i].Length;
      }

      return result;
   }

   internal static void SkipWhiteSpace(this ReadOnlySpan<Char> thisSpan, ref int position)
   {
      while (position < thisSpan.Length && char.IsWhiteSpace(thisSpan[position]))
         position++;
      return;
   }

   internal static Boolean TestAndSkipCharacter(this ReadOnlySpan<Char> thisSpan, ref int position, char c)
   {
      if (position >= thisSpan.Length || thisSpan[position] != c)
         return false;

      position++;
      return true;
   }

   internal static Boolean IsIdentifier(this ReadOnlySpan<Char> thisSpan, ref int position, out ReadOnlySpan<Char> identifier)
   {
      Int32 Position = position;
      identifier = "";
      int StartPosition = Position;

      while (Position < thisSpan.Length &&
            (char.IsLetter(thisSpan[Position]) || char.IsDigit(thisSpan[Position])))
         Position++;

      if (Position == StartPosition)
         return false;

      identifier = thisSpan[StartPosition..Position];
      position = Position;
      return true;
   }

   internal static Boolean IsInt64(this ReadOnlySpan<Char> ThisSpan, ref int Position, out Int64 Number)
   {
      Int32 checkPosition = Position;
      Number = 0;

      while (checkPosition < ThisSpan.Length && char.IsDigit(ThisSpan[checkPosition]))
         checkPosition++;

      return Int64.TryParse(ThisSpan[Position..checkPosition], out Number); // false if empty string or overflow
   }

}


