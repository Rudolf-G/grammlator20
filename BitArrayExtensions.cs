using System;
using System.Collections;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Grammlator {
   /// <summary>
   /// Adds the following extensions to BitArray: Assign, Subtract, Empty, All, IsEqualTo, BitsToStringBuilder, IndexOfFirstTrueElement,
   /// IndexOfLastTrueElement, IndexOfFirstAndLastTrueELement, FindTrue, FindFalse, PopulationCount
   /// </summary>
   public static class BitArrayExtensions {

      // https://de.wikipedia.org/wiki/Boolesche_Funktion#Zweistellige_Funktion 
      //

      private static readonly Int32[] mask = new Int32[] {
            0x1, 0x3, 0x7, 0xf,
            0x1f, 0x3f, 0x7f, 0xff,
            0x1ff, 0x3ff, 0x7ff, 0xfff,
            0x1fff, 0x3fff, 0x7fff, 0xffff,
            0x1ffff, 0x3ffff, 0x7ffff, 0xfffff,
            0x1fffff, 0x3fffff, 0x7fffff, 0xffffff,
            0x1ffffff, 0x3ffffff, 0x7ffffff, 0xfffffff,
            0x1fffffff, 0x3fffffff, 0x7fffffff, -1
            };

      /// <summary>
      /// Int32[64]=2048 Bits for use in empty and in all
      /// </summary>
      private static readonly Int32[] staticarray = new Int32[64];

      /// <summary>
      /// some tests
      /// </summary>
      public static void Test()
      {
         // tests functions of BitAray which use CopyTo and expect a special order of the returned bits
         var b = new BitArray(0);
         Debug.Assert(b.Empty());
         Debug.Assert(b.All());
         Debug.Assert(b.PopulationCount() == 0);
         b.Not();
         Debug.Assert(b.Empty());
         Debug.Assert(b.All());
         Debug.Assert(b.PopulationCount() == 0);

         b = new BitArray(1);
         var ia = new Int32[1];
         b.CopyTo(ia, 0);
         Debug.Assert(ia[0] == 0);
         Debug.Assert(b.Empty());
         Debug.Assert(b.PopulationCount() == 0);
         b.Set(0, true);
         b.CopyTo(ia, 0);
         Debug.Assert(ia[0] == 1);
         Debug.Assert(b.All());
         Debug.Assert(b.PopulationCount() == 1);
         b.Not();
         Debug.Assert(b.Empty());
         Debug.Assert(b.PopulationCount() == 0);
         b.SetAll(true);
         Debug.Assert(b.All());
         Debug.Assert(b.PopulationCount() == 1);

         b = new BitArray(33);
         Debug.Assert(b.Empty());
         Debug.Assert(b.PopulationCount() == 0);
         b.Not();
         Debug.Assert(b.All());
         Debug.Assert(b.PopulationCount() == 33);

         b.Not();
         Debug.Assert(b.Empty());
         Debug.Assert(b.PopulationCount() == 0);
         for (Int32 i = 0; i < 33; i++)
            b[i] = true;
         Debug.Assert(b.All());
         Debug.Assert(b.PopulationCount() == 33);
         b.Not();
         Debug.Assert(b.Empty());
         Debug.Assert(b.PopulationCount() == 0);
         b.Not();
         b[32] = false;
         Debug.Assert(!b.All());
         Debug.Assert(!b.Empty());
         Debug.Assert(b.PopulationCount() == 32);

         b.Not();
         Debug.Assert(!b.All());
         Debug.Assert(!b.Empty());
         Debug.Assert(b.PopulationCount() == 1);
      }

      /// <summary>
      /// Copies the source to the destination (by destination.SetAll(false); destination.Or(source);)
      /// </summary>
      /// <param name="destination">destination must have the same length as source</param>
      /// <param name="source"></param>
      /// <returns>An array containing the result of the bitwise OR operation, which is a reference to the current BitArray object.</returns>
      /// <exception cref="ArgumentNullException">Thrown if destination or source are null</exception>
      /// <exception cref="ArgumentException">Thrown if source and destination do not have the same length</exception>
      public static BitArray Assign(this BitArray destination, BitArray source)
      {
         if (destination == null)
            throw new ArgumentNullException(nameof(destination));
         if (ReferenceEquals(destination, source))
            return destination;
         destination.SetAll(false);
         destination.Or(source);
         return destination;
      }

      /// <summary>
      /// computes Or(subtrahend).Xor(subtrahend): all bits which are true in subtrahend will be set to false in the actual BitArray
      /// </summary>
      /// <param name="minuend"></param>
      /// <param name="subtrahend"></param>
      /// <returns>An array containing the result of the bitwise OR operation, which is a reference to the current BitArray object.</returns>
      /// <exception cref="ArgumentNullException"><paramref name="minuend"/> is <c>null</c>.</exception>
      public static BitArray ExceptWith(this BitArray minuend, BitArray subtrahend)
      {
         // The name "ExceptWith" conforms to ISet<T>, elsewhere this function is nameded "Inhibition", "Substraction"
         // Bit (0-1 = 0, 1-1 = 0, 0-0 = 0, 1-0 = 1), 
         if (minuend == null)
            throw new ArgumentNullException(nameof(minuend));

         minuend.Or(subtrahend).Xor(subtrahend);
         return minuend;
      }

      /// <summary>
      /// Checks if all bits are 0
      /// </summary>
      /// <param name="bits"></param>
      /// <returns>true if all Bits are true</returns>
      /// <exception cref="ArgumentNullException"><paramref name="bits"/> is <c>null</c>.</exception>
      public static Boolean Empty(this BitArray bits)
      {
         if (bits == null)
            throw new ArgumentNullException(nameof(bits));

         //for (int i = 0; i < b.Count; i++) {
         //    if (b[i]) return false;
         //    }
         //return true;

         // Optimized by Int-Operations
         if (bits.Count <= 0)
            return true;

         Int32 length = ((bits.Count - 1) / 32) + 1;
         Int32[] a = staticarray; // try to use staticarray (not thread save!)
         if (a.Length < length) // staticarray is too short, so accept overhead by new
            a = new Int32[length];

         bits.CopyTo(a, 0); // beware of undefined state of unused bits 
         a[length - 1] &= mask[(bits.Count - 1) % 32]; // set unused bits of last used element to 0

         for (int i = 0; i < length; i++)
            if (a[i] != 0)
               return false;

         return true;
      }

      /// <summary>
      /// Checks if all bits are 1
      /// </summary>
      /// <param name="bits"></param>
      /// <returns>returns true if all bits are set</returns>
      /// <exception cref="ArgumentNullException"><paramref name="bits"/> is <c>null</c>.</exception>
      public static Boolean All(this BitArray bits)
      {
         if (bits == null)
            throw new ArgumentNullException(nameof(bits));

         //for (int i = 0; i < b.Count; i++) {
         //    if (!b[i]) return false;
         //    }
         //return true;

         // Optimized by Int-Operations
         if (bits.Count <= 0)
            return true;
         Int32 length = ((bits.Count - 1) / 32) + 1;
         Int32[] a = staticarray; // try to use staticarray (not thread save!)
         if (a.Length < length)
            a = new Int32[length]; // staticarray is too short, so accept overhead by new

         bits.CopyTo(a, 0); // beware of undefined state of unused bits 
         a[length - 1] |= ~mask[(bits.Count - 1) % 32]; // set unused bits to 1

         for (int i = 0; i < length; i++)
            if (a[i] != -1)
               return false;

         return true;
      }

      /// <summary>
      /// Checks if all bits are equal
      /// </summary>
      /// <param name="leftArgument"></param>
      /// <param name="rightArgument"></param>
      /// <returns>true if equal</returns>
      /// <exception cref="ArgumentNullException"><paramref name="rightArgument"/> is <c>null</c>.</exception>
      public static Boolean IsEqualTo(this BitArray leftArgument, BitArray rightArgument)
      {
         if (rightArgument == null)
            throw new ArgumentNullException(nameof(rightArgument));
         if (leftArgument == null)
            throw new ArgumentNullException(nameof(leftArgument));

         if (leftArgument.Count != rightArgument.Count)
            return false;
         for (Int32 i = 0; i < leftArgument.Count; i++)
         {
            if (leftArgument[i] != rightArgument[i])
               return false;
         }
         return true;
      }

      /// <summary>
      /// Appends the <paramref name="BitNames"/> (or the indexes if no names) of all bits which are true to the 
      /// Stringbuilder <paramref name="Sb"/> using the <paramref name="Delimiter"/>.
      /// There may be given special texts for the case of all bits or no bits set.
      /// </summary>
      /// <param name="Bits">the Bitarray</param>
      /// <param name="Sb">the Stringbuilder to which the names of the set bits are appended</param>
      /// <param name="BitNames">defines the name of each bit i by BitNames[i].ToString(), may be null</param>
      /// <param name="Delimiter">the delimiter to be used to separate names, default</param>
      /// <param name="All">Text if all bits are set or null (default)</param>
      /// <param name="Empty">Text if no Bits are set or null (default)</param>
      /// <returns>the argument <paramref name="Sb"/></returns>
      /// <exception cref="ArgumentOutOfRangeException">if </exception>
      internal static StringBuilder BitsToStringbuilder(
              this BitArray Bits, StringBuilder Sb,
              Object[]? BitNames = null, String Delimiter = ", ", String? All = null, String? Empty = null)
      {
         if (BitNames != null && BitNames.Length < Bits.Length)
         {
            throw new ArgumentOutOfRangeException
               ($"Length of {nameof(BitNames)} == {BitNames.Length} is less than the number of bits == {Bits.Length} in call of {nameof(BitsToStringbuilder)}");
         }

         Boolean isFirst = true;
         if (All != null && Bits.All())
         {
            Sb.Append(All);
         }
         else
         {
            for (Int32 i = 0; i < Bits.Length; i++)
            {
               if (Bits[i])
               {
                  if (isFirst)
                     isFirst = false;
                  else
                     Sb.Append(Delimiter);
                  if (BitNames == null)
                     Sb.Append(i);
                  else
                     Sb.Append(BitNames[i].ToString());
               }
            }

            if (isFirst && Empty != null)
               Sb.Append(Empty);
         }
         return Sb;
      }

      /// <summary>
      /// The lowest index for which element[i]=true
      /// or -1 if no such element exists
      /// </summary>
      /// <param name="bits"></param>
      public static Int32 IndexOfFirstTrueElement(this BitArray bits)
      {
         if (bits == null)
            throw new ArgumentNullException(nameof(bits), $"Null-Argument in call of  {nameof(IndexOfFirstTrueElement)}");

         Int32 i;
         for (i = 0; i <= bits.Count; i++)
         {
            if (bits[i])
               return i;
         }
         return -1;
      }

      /// <summary>
      /// The highest index for which element[i]=true
      /// or -1 if no such element exists
      /// </summary>
      /// <param name="bits"></param>
      public static Int32 IndexOfLastTrueElement(this BitArray bits)
      {
         if (bits == null)
            throw new ArgumentNullException(nameof(bits), $"Null-Argument in call of  {nameof(IndexOfLastTrueElement)}");

         Int32 i;
         for (i = bits.Count - 1; i >= 0; i--)
         {
            if (bits[i])
               return i;
         }
         return -1;
      }

      /// <summary>
      /// Returns the Tuple (int IndexOfFirstTrueElement, int IndexOfLastTrueelement) or (-1,-1) if no such element
      /// </summary>
      /// <param name="bits"></param>
      public static (Int32 IndexOfFirstTrueElement, Int32 IndexOfLastTrueElement) IndexOfFirstAndLastTrueElement(this BitArray bits)
      {
         if (bits == null)
            throw new ArgumentNullException(nameof(bits), $"Null-Argument in call of  {nameof(IndexOfFirstAndLastTrueElement)}");

         Int32 iLast;
         for (iLast = bits.Count - 1; iLast >= 0; iLast--)
         {
            if (bits[iLast])
               break;
         }
         if (iLast == -1)
            return (-1, -1);

         // There is at least one element (the element iLast) with the value true
         // The while-loop will find the first
         Int32 iFirst = 0;
         while (!bits[iFirst])
            iFirst++;
         return (iFirst, iLast);
      }

      /// <summary>
      /// Find the next element which is true and return the index of this element or this.count if not found
      /// </summary>
      /// <param name="bits"></param>
      /// <param name="index">Index to start with, default is 0</param>
      /// <returns>Index of found element or this.count if not found</returns>
      public static Int32 FindNextTrue(this BitArray bits, Int32 index = -1)
      {
         if (bits == null)
            throw new ArgumentNullException(nameof(bits), $"Null-Argument in call of  {nameof(FindNextTrue)}");

         if (++index < 0)
            index = 0;

         while (index < bits.Count && !bits[index])
         {
            index++;
         }
         return index;
      }

      /// <summary>
      /// Find the next element which is false and return the index of this element or LastIndex+1 if not found
      /// </summary>
      /// <param name="bits">the BitArray</param>
      /// <param name="index">Index to start with, default is -1, if value is less than -1 then -1 is used</param>
      /// <param name="lastIndex">Index to end with, default is int.MaxValue, 
      /// if the value is greater or equal bits.Count then Bits.Count-1 is used</param>
      /// <returns>Index of found element or this.count if not found</returns>
      public static Int32 FindNextFalse(this BitArray bits, Int32 index = -1, Int32 lastIndex = int.MaxValue)
      {
         if (bits == null)
            throw new ArgumentNullException(nameof(bits), $"Null-Argument in call of  {nameof(FindNextFalse)}");

         if (++index < 0)
            index = 0;

         if (lastIndex >= bits.Count)
            lastIndex = bits.Count - 1;

         while (index <= lastIndex && bits[index])
         {
            index++;
         }
         return index;
      }

      /// <summary>
      /// Find the preceding element which is false and return the index of this element or -1 if not found
      /// </summary>
      /// <param name="bits">the BitArray</param>
      /// <param name="index">Index to start with, default is Int32.MaxValue, if value is greater or equal than bits.Count
      /// then bits.Count-1 is used</param>
      /// <returns></returns>
      public static Int32 FindPrecedingFalse(this BitArray bits, Int32 index = Int32.MaxValue)
      {
         if (bits == null)
            throw new ArgumentNullException(nameof(bits), $"Null-Argument in call of  {nameof(FindPrecedingFalse)}");

         if (--index >= bits.Count)
            index = bits.Count - 1;

         while (index >= 0 && bits[index])
         {
            index--;
         }

         return index;
      }

      /// <summary>
      /// Computes the number of Bits set in the Arg
      /// </summary>
      /// <param name="Arg">The argument which 1 bits are to be counted</param>
      /// <returns>the number of 1 bits</returns>
      private static Int32 PopulationCount(Int32 Arg)
      {
         UInt32 ArgPositive = unchecked((UInt32)Arg);

         // The following algorithm and comments have been found in: http://dflund.se/~john_e/gems/gem002d.html (no longer exists)
         // The algorithm is shown in https://stackoverflow.com/questions/6097635/checking-cpu-popcount-from-c-sharp

         // See also http://graphics.stanford.edu/~seander/bithacks.html#OperationCounting

         /* Partition the register into groups of 2 bits. Compute the population count for each 2-bit group 
            and store the result in the 2-bit group
            This calls for the following transformation to be performed for each 2-bit group: 00b -> 00b, 01b -> 01b, 10b -> 01b, 11b -> 10b 
            If the orginal value of a 2-bit group is x, then the new value will be x - (x >> 1).
            In order to handle all 2-bit groups simultaneously, we have to mask appropriately to prevent spilling from one group
            to the next lower group. Thus: n = n - ((n >> 1) & 0x55555555)               */
         UInt32 i1 = ArgPositive - ((ArgPositive >> 1) & 0x55555555u); // 16 sums of each 2 bits

         /* Now, add the population count of adjacent 2-bit groups 
            and store the sum to the 4-bit group resulting from merging these adjacent 2-bit groups. 
            To do this simultaneous to all groups, we mask out the odd numbered groups, mask out the even numbered groups, 
            and then add the odd numered groups to the even numbered groups:
            n = (n & 0x33333333) + ((n >> 2) & 0x33333333) 
            Each 4-bit field now has value 0000b, 0001b, 0010b, 0011b, or 0100b.               */
         UInt32 i2 = (i1 & 0x33333333u) + ((i1 >> 2) & 0x33333333u); // 8 sums of each 4 bits

         /* Now, for the first time, the value in each k-bit field is small enough that adding two k-bit fields results in a value
            that still fits in the k-bit field. Thus the following computation is performed:
            n = (n + (n >> 4)) & 0x0F0F0F0F
            The result is four 8-bit fields whose lower half has the correct sum and whose upper half contains junk that has to be masked out.
            Pictorially:
                 n      = 0aaa0bbb0ccc0ddd0eee0fff0ggg0hhh
                 n >> 4 = 00000aaa0bbb0ccc0ddd0eee0fff0ggg
                 sum    = 0aaaWWWWiiiiXXXXjjjjYYYYkkkkZZZZ
                 where WWWW, XXXX, YYYY, and ZZZZ are the interesting sums each at most 1000b, or 8 decimal.             */
         UInt32 i3 = (i2 + (i2 >> 4)) & 0x0F0F0F0Fu; // 4 sums of each 8 bits

         /* For the next step, adjacent 8-bit fields are added together and the sum is put into 16-bit fields
            created by merging the adjacent 8-bit fields.            */
         UInt32 i4 = (i3 + (i3 >> 8)); // 2 sums of each 16 Bits

         /* In the last step, the values of the two 16 - bit groups are added, 
             producing the final result in the six least significant bits.Any remaining junk is masked out.
               n = (n + (n >> 16)) & 0x3F
               n =       0000WWWW000ppppp000qqqqq000rrrrr
               n >> 16 = 00000000000000000000WWWW000ppppp
               sum =     0000WWWW000ppppp000sssss00tttttt
              where sssss is at most 11000b and tttttt at most 100000b(32 decimal).*/
         UInt32 i5 = (i4 + (i4 >> 16)) & 0x03Fu; // sum of 32 bits

         return (Int32)i5;

         /*   ...If the target machine has a fast integer multiply, we can use an IMUL to add the 4-bit fields like so:
                 0000pppp0000qqqq0000rrrr0000ssss * 00000001000000010000000100000001 =

                                             :0000pppp0000qqqq0000rrrr0000ssss
                                     0000pppp:0000qqqq0000rrrr0000ssss
                             0000pppp0000qqqq:0000rrrr0000ssss
                     0000pppp0000qqqq0000rrrr:0000ssss
                     ---------------------------------------------------------------
                     0000ppppxxxxxxxxwwwwwwww:vvvvvvvvuuuuuuuutttttttt0000ssss 
              here ppppp and rrrrr are the interesting sums (and each is at most 10000b or 16 decimal).
              The sum qqqqq is junk, but it is not necessary to discard it this time.                  
           i4 =   (i3 * 0x001010101) >> 24
           return (Int32)i4;
         */
      }

      /// <summary>
      /// Computes the number of set bits
      /// </summary>
      /// <param name="bits">the bitarray whose bits are to be counted</param>
      /// <returns>the number of bits with value 1 (true)</returns>
      public static Int32 PopulationCount(this BitArray bits)
      {
         if (bits == null)
            throw new ArgumentNullException(nameof(bits), $"Null-Argument in call of  {nameof(PopulationCount)}");

         if (bits.Count <= 0)
            return 0;

         Int32 length = ((bits.Count - 1) / 32) + 1;
         Int32[] a = staticarray; // try to use staticarray (not thread save!)
         if (a.Length < length) // staticarray is too short, so accept overhead by new
            a = new Int32[length];

         bits.CopyTo(a, 0); // beware of undefined state of unused bits 
         a[length - 1] &= mask[(bits.Count - 1) % 32]; // set unused bits to 0

         Int32 sum = 0;
         for (int i = 0; i < length; i++)
            sum += PopulationCount(a[i]);

         return sum;
      }
   }
}