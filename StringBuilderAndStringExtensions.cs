using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;
using System.Text;

namespace grammlator {
   /// <summary>
   /// Defines .TrimEnd (params Char[] TrimChars)
   /// </summary>
   internal static class StringbuilderExtensions {
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

   internal static class StringExtensions {

      /// <summary>
      /// Count the number of lines in the string
      /// </summary>
      /// <param name="thisString"></param>
      /// <returns>the number of newline-characters in the string + 1, 0 if the string is null or empty</returns>
      internal static Int32 CountLines(this String thisString)
      {
         return String.IsNullOrEmpty(thisString)?0:thisString.Count(c => c == '\n') + 1;
      }

      /// <summary>
      /// increments <paramref name="Position"/> while <paramref name="ThisString"/>[<paramref name="Position"/>] IsSeparator (including space, line and paragraph separators) 
      /// but not line feed ...)
      /// </summary>
      /// <param name="ThisString">the String to check</param>
      /// <param name="Position">returns new Position less or equal s.Length</param>
      /// <returns>string <paramref name="ThisString"/> to allow catenation of operations</returns>
      internal static String SkipSeparator(this String ThisString, ref Int32 Position)
      {
         if (Position < 0)
            Position = 0;
         while (Position < ThisString.Length && Char.IsSeparator(ThisString[Position]))
            Position++;
         return ThisString;
      }

      /// <summary>
      /// Starting at ThisSpan[start ]finds the first char which is not char.IsSeparator and
      /// returns its index.
      /// IsSeparator is true for space, line and paragraph separators but not CR, LF and FF
      /// </summary>
      /// <param name="ThisSpan">the Span to check</param>
      /// <param name="start">The index of the first char of ThisSpan to check</param>
      /// <returns>returns the index of the first char which is not a separator, less or equal s.Length</returns>
      internal static Int32 IndexBehindSeparators(this ReadOnlySpan<Char> ThisSpan, Int32 start)
      {
         Int32 result = start;
         if (result < 0)
            result = 0;
         while (result < ThisSpan.Length && Char.IsSeparator(ThisSpan[result]))
            result++;
         return result;
      }
 
      /// <summary>
      /// Checks whether <paramref name="ThisString"/> contains the <paramref name="Pattern"/> at the <paramref name="Position"/>
      /// and updates <paramref name="Position"/> to point to the first character after the 
      /// <paramref name="Pattern"/> - else lets <paramref name="Position"/> unchanged
      /// </summary>
      /// <param name="ThisString">the String to check</param>
      /// <param name="Position">the Position to start the comparision</param>
      /// <param name="Pattern">the string to test for</param>
      /// <returns>returns true and Position after pattern if pattern found start of string, else false and Position unchanged</returns>
      internal static Boolean StartsWithAndSkip(this String ThisString, ref Int32 Position, String Pattern)
      {
         if (ThisString == null || Position + Pattern.Length > ThisString.Length)
            return false;

         Int32 sPosition = Position;

         for (Int32 pPosition = 0; pPosition < Pattern.Length; pPosition++)
         {
            if (ThisString[sPosition] != Pattern[pPosition])
               return false; // Position unchanged
            sPosition++;
         }

         Position = sPosition;
         return true;
      }
            
      /// <summary>
      /// Checks whether the string starts with a sequence of strings ignoring white space in front of each marker.
      /// Does not allocate temporary string variables in the .NET heap.
      /// </summary>
      /// <param name="ThisString">the string to be checked</param>
      /// <param name="markers">the strings to be checked for</param>
      /// <returns>true if all markers have been found</returns>
      internal static Boolean StartsWith(this String ThisString, params String[] markers)
      {
         Boolean foundAllMarkers = true;
         Int32 column = 0;
         foreach (String marker in markers)
         {
            if (!ThisString.SkipSeparator(ref column).StartsWithAndSkip(ref column, marker))
            {
               foundAllMarkers = false;
               break;
            }

            if (!foundAllMarkers)
               break;
         }

         return foundAllMarkers;
      }

      /// <summary>
      /// Checks whether the Span starts with a sequence of strings ignoring unicode separator characters in front of each marker.
      /// If all markers have been found the index of the next character after the last marker is returned, else -1.
      /// </summary>
      /// <param name="start">the index where to start checking</param>
      /// <param name="ThisSpan">the string to be checked</param>
      /// <param name="markers">the strings to be checked for</param>
      /// <returns>the index after the last marker if success else -1</returns>

      internal static Int32 IndexBehindMarkers(this ReadOnlySpan<Char> ThisSpan, Int32 start, params String[] markers)
      {

         Int32 result = start;

         for (Int32 i = 0; i < markers.Length; i++)
         {
            result = ThisSpan.IndexBehindSeparators(result);

            if (!ThisSpan.Slice(result).StartsWith(markers[i]))
               return -1;

            result += markers[i].Length;
         }

         return result;
      }

   }
}


