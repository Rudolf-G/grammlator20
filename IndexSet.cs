using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Numerics;
using System.Text;

namespace IndexSetNamespace;

/// <summary>
/// <para>The <see cref="IndexSet"/> struct implements the collection and set operations defined in
/// <see cref="ICollection{T}"/> (including  <see cref="IEnumerable{T}"/>) and <see cref="ISet{T}"/>
/// with T == <see cref="int"/>. The set elements are restricted to the range 0 .. <see cref="Length"/>.
/// The struct is similar to the class <see cref="SortedSet{T}"/> with T == int, but with a very different implementation.
/// Most methods conform to methods of the <see cref="System.Collections.BitArray"/> class using bit parallel operations.
/// </para>
/// <para>
/// The struct references a sequence of bits, whereby a collection or a set contains an <see cref="int"/> value i
/// if and only if bit[i] is true. Typical methods modify the referenced bits and return a struct referencing the same bits.
/// </para>
/// <para>
/// The number of bits of the sequence is given by the readonly property <see cref="Length"/>.
/// Simple rules allow to combine <see cref="IndexSet"/> with different length in operations.
/// If bits outside the range are accessed, the value 0(false) is returned,
/// stating that the respective element is not contained in the set.
/// If a method tries to remove an element outside of the range (set a bit outside of the range to 0),
/// this does not throw an exception. An <see cref="ArgumentException"/> will be thrown only
/// if a method tries to add an element outside the range to a <see cref="IndexSet"/> struct
/// (set a bit outside of the range to 1).
/// </para>
/// <para>
/// Some collection resp. set methods, e.g. <see cref="IntersectWith(IndexSet)"/> are alias names of bit operations,
/// e.g. <see cref="And(IndexSet)"/>. 
/// </para>
/// </summary>
public readonly struct IndexSet :
   ICollection<int>, IEnumerable<int>, IComparable<IndexSet>, ISet<int>
{
   /// <summary>
   /// The <see cref="IndexSet"/> struct has the capacity to store elements with indexes in the range 0..&lt; <see cref="Length"/>.
   /// </summary>
   public readonly int Length; // 4 bytes. Is 0 if and only if BitArray is null. 

   /// <summary>
   /// <see cref="BitSequence"/> is allocated if <see cref="Length"/> is greater 0, else is null.
   /// If <see cref="BitSequence"/> is allocated, this allocates of 24 bytes for the class plus 8 bytes per 64 bits.
   /// The array contains (ArrayLength * 64 - Length) not used (excess) bits. These are always 0.
   /// </summary>
   private readonly UInt64[]? BitSequence; // 8 bytes


   private readonly int RequiredLength; // 4 bytes (uses allignment space otherwise unused). Is 0 if and only if BitArray is null.

   private const int BitsPerArrayElement = 64;
   private const int BitsPerBitIndex = 6; // ld(BitsPerArrayElement)
   private const int BitIndexMask = BitsPerArrayElement - 1; // 63

   private static int ComputeArrayLength(int length)
      => (length + (BitsPerArrayElement - 1)) >> BitsPerBitIndex;
   private static UInt64 ExceptWith(UInt64 a, UInt64 b)
      => (a | b) ^ b;

   private static bool IsGe0AndLT(int x, int length) //   => (x >= 0) & (x <= length);
        => unchecked((uint)x < (uint)length);
   private static int Maximum(int a, int b) => a <= b ? b : a;
   private static int Minimum(int a, int b) => a <= b ? a : b;

   /// <summary>
   /// Returns the mimum value, the maximum value and the count of elements of <paramref name="initialIndexes"/>.
   /// Tests if the values are sorted in increasing order.
   /// Returns (0, -1, 0, true) if <paramref name="initialIndexes"/> is null or empty.
   /// </summary>
   /// <returns>The tuple (int min, int max, int count).</returns>
   private static (int min, int max, int count, bool isSorted) GetMinMaxCountSorted(IEnumerable<int> initialIndexes)
   {

      int max = int.MinValue;
      int min = int.MaxValue;
      int count = 0;
      bool increasing = true;
      bool decreasing = true;

      void testIndex(int index)
      {
         if (index >= max)
         {
            if (index != max)
               count++;
            max = index;
         }
         else
         {
            increasing = false;
            count++;
         }
         if (index <= min)
         {
            if (index != min)
               count++;
            min = index;
         }
         else
         {
            decreasing = false;
            count++;
         }
      }


      switch (initialIndexes)
      {
         case null: break;
         case int[] a: // avoid iterator
            for (int i = 0; i < a.Length; i++)
               testIndex(a[i]);
            break;

         default:
            foreach (int index in initialIndexes)
               testIndex(index);

            break;
      }

      if (count == 0)
         return (0, -1, 0, true); // empty set

      return (min, max, count, increasing || decreasing);
   }

   /// <summary>
   /// This standard constructor returns a <see cref="IndexSet"/> struct with no bits.
   /// </summary>
   public IndexSet() => this = default;

   /// <summary>
   /// This constructor returns a <see cref="IndexSet"/> struct which is a deep copy of <paramref name="source"/>.
   /// </summary>
   /// <param name="source">The <see cref="IndexSet"/> which are copied into the new struct.</param>
   public IndexSet(IndexSet source)
   {
      this = new(source.Length);
      CopyFrom(source);
   }

   /// <summary>
   /// This constructor returns an <see cref="IndexSet"/> struct with the given <see cref="Length"/> and 
   /// allocates an Array, which can store this number of bits.
   /// </summary>
   /// <param name="length">The <see cref="Length"/> (number of bits) of the new struct. If the <paramref name="length"/> is &lt; 0 a struct with <see cref="Length"/> 0 is returned.</param>
   public IndexSet(int length)
   {
      if (length < 0)
         throw new ArgumentException($"The {nameof(IndexSet)} constructor was called with {nameof(length)}< 0");
      RequiredLength = (length + (BitsPerArrayElement - 1)) >> BitsPerBitIndex;

      Length = length;
      BitSequence = RequiredLength <= 0 ? null : new ulong[RequiredLength];
   }

   /// <summary>
   /// This constructor returns a new <see cref="IndexSet"/> struct which references a new sequence of bits
   /// with <paramref name="length"/> bits.
   /// This sequence of bits is initialized with the given <paramref name="initialValues"/>
   /// by <see cref="CopyFrom(IndexSet, bool)"/>. Additional bits remain 0 (false).
   /// </summary>
   /// <param name="length">The <see cref="Length"/> of the new struct.
   /// Each bit of the new struct can be indexed by an integer in the range 0..<paramref name="length"/>.</param>
   /// <param name="initialValues">All bits of <paramref name="initialValues"/> will be copied to the new set.
   /// Depending on <paramref name="ignoreExcessValues"/> bits of <paramref name="initialValues"/> 
   /// with value 1 (true) and with an index not in the range 0..<paramref name="length"/>
   /// are ignored or cause an ArgumentException.</param>
   /// <param name="ignoreExcessValues"></param>
   public IndexSet(int length, IndexSet initialValues, bool ignoreExcessValues = false)
      => this = new IndexSet(length).CopyFrom(initialValues, ignoreExcessValues);

   /// <summary>
   /// This constructor allocates an Array, which can store the given number of bits
   /// and for each given initial index sets the bit with this index to 1 (true).
   /// </summary>
   /// <param name="length">The <see cref="Length"/> of the new struct.
   /// Each bit of the new struct can be indexed by an integer in the range 0..<paramref name="length"/>.</param>
   /// <param name="initialIndexes">All elements of <paramref name="initialIndexes"/> will be added to the new set.
   /// Depending on <paramref name="ignoreExcessValues"/> values which are not within
   /// the range 0..<paramref name="length"/> are ignored or cause an ArgumentException.</param>
   /// <param name="ignoreExcessValues"></param>
   public IndexSet(int length, IEnumerable<int> initialIndexes, bool ignoreExcessValues = false)
   {
      this = new(length);
      if (initialIndexes is IndexSet bits)
      {
         CopyFrom(bits, ignoreExcessValues);
         return;
      }

      foreach (int initialValue in initialIndexes)
      {
         if (ignoreExcessValues && (initialValue < 0 || initialValue >= length))
            continue;

         Set(initialValue, true); // may throw argument exception
      }

      return;
   }

   /// <summary>
   ///  For Length == 0, 1, 2, 3, .., 64 the mask is -1, 1, 3, 7, ... 2**64-1 (== ~0ul == -1)
   /// </summary>
   private readonly UInt64 LastElementMask => (~0UL) >> (-Length); // ">>" uses only 6 Bits of "-Length"

   /// <summary>
   /// Creates a <see cref="IndexSet"/> struct with length = Minimum(max+1, this.Length)
   /// whereby max is the largest element in <paramref name="initialIndexes"/> and copies
   /// all values vom <paramref name="initialIndexes"/>, which are within the range of the new set.
   /// </summary>
   /// <param name="initialIndexes"></param>
   /// <returns>A new <see cref="IndexSet"/> struct.</returns>
   private IndexSet CopyToNewLimitedLengthBits(IEnumerable<int> initialIndexes)
      => new(length: Minimum(initialIndexes.Max() + 1, Length), initialIndexes, ignoreExcessValues: true);

   ///// <summary>
   ///// A static instance of <see cref="Bits"/> based on the empty set 0..&lt;0 as universal set. This set contains no elements.
   ///// No elements can be added.
   ///// </summary>
   //public static readonly Bits IndexSetRWithLength0 = new(Array.Empty<ulong>(), 0);

   /* ****** Properties ****** */

   /// <summary>
   /// Returns the number of bits with value 1 (true).
   /// <para>
   /// The indexes of the bits with value 1 can be taken as a set of &lt;<see cref="int"/>> numbers
   /// with cardinality <see cref="Count"/>.</para>
   /// <para>
   /// Implements ICollection&lt;int>.Count alike <see cref="SortedSet{T}.Count"/>,
   /// but different than <see cref="BitArray.Count"/> (which returns the <see cref="BitArray.Length"/>).</para>
   /// <para>This is an O(n) operation with 64 bit parallel execution.</para>
   /// </summary>
   public readonly int Count
   {
      get
      {
         int count = 0;
         for (int i = 0; i < RequiredLength; i++)
            count += System.Numerics.BitOperations.PopCount(BitSequence![i]); // O(1) !
         return count;
      }
   }

   /// <summary>
   /// Tests, if the set contains all indexes from 0 to Length-1 / 
   /// if all bits of the referenced sequence of bits are 1 (true).
   /// <para>If <see cref="Length"/> == 0: result is <see langword="false"/>, as if all bits are false.</para>
   /// </summary>
   public readonly bool IsComplete => AllBitsAre(true);

   /// <summary>
   /// Tests, if the set ist empty / 
   /// if all bits of the referenced sequence of bits are 0 (false).
   /// <para>If <see cref="Length"/> == 0: result is <see langword="true"/>, as if all bits are false.</para>
   /// </summary>
   public readonly bool IsEmpty => AllBitsAre(false);

   /// <summary>
   /// Is always false.
   /// </summary>
   public readonly bool IsReadOnly => false;  // ICollection<int>

   /// <summary>
   /// Tests if all bits have the given value.
   /// </summary>
   /// <param name="value">The value to test all bits for.</param>
   /// <returns><see langword="true"/>, if all bits have the given value, else <see langword="false"/>.
   /// <para>If <see cref="Length"/> == 0: result is (not <paramref name="value"/>), as if all bits are false.</para>
   /// </returns>
   public readonly bool AllBitsAre(bool value)
   {
      if (BitSequence is null)
         return !value;

      ulong ulongValue = value ? ~0UL : 0;
      for (int i = 0; i < RequiredLength - 1; i++)
         if (BitSequence[i] != ulongValue)
            return false;

      return BitSequence[^1] == (LastElementMask & ulongValue);
   }

   /// <summary>
   /// Todo ///
   /// </summary>
   public readonly bool IsSynchronized => Length == 0;  // BitArray: false

   /// <summary>
   /// Todo ///
   /// </summary>
   public object SyncRoot => Length > 0 ? BitSequence! : new object();

   /// <summary>
   /// Tests if <paramref name="index"/> is member of the set or adds  <paramref name="index"/> to the set /
   /// Gets or sets the value of the bit at position <paramref name="index"/> in <see cref="IndexSet"/>.
   /// If <paramref name="index"/> is out of the Range of the set, returns 0 resp. has no effect.
   /// </summary>
   /// <param name="index"></param>
   /// <returns>true, if <paramref name="index"/>" is a member of the set /
   /// if the bit at position index is 1 (true)</returns>
   /// <exception cref="ArgumentException"></exception>
   public readonly bool this[int index]
   {
      get { return Get(index); }
      set { Set(index, value); }
   }

   /* ****** Methods ****** */

   /// <summary>
   /// Sets the selected bit to <see langword="true"/>.
   /// </summary>
   /// <param name="index">The index of the selected bit. The index must be >=0 and &lt; <see cref="Length"/>
   /// </param>
   /// <exception cref="ArgumentException">
   /// An exception is thrown if <paramref name="index"/> is &lt; 0 or >= <see cref="Length"/>. 
   /// </exception>
   public void Add(int index) => Set(index, true); // ICollection<int>


   /// <summary>
   /// Returns <see langword="false"/> if the selected bit is already <see langword="true"/>
   /// else sets the selected bit to <see langword="true"/>.
   /// <para>ISet&lt;int>: Adds an int to the current set and returns a value to indicate if the int was successfully added.</para>
   /// </summary>
   /// <param name="index">The index of the selected bit. The index must be >=0 and &lt; <see cref="Length"/></param>
   /// <exception cref="ArgumentException"></exception>
   bool ISet<int>.Add(int index)
   {
      if (Get(index))
         return false;
      Set(index, true);
      return true;
   }

   /// <summary>
   /// Modifies the bits referenced by the current struct by performing a logical "and" operation between those 
   /// and the bits referenced by the <paramref name="other"/> struct.
   /// If the <see cref="Length"/> of the current struct exceeds the <see cref="Length"/>
   /// of the <paramref name="other"/> the excess bits referenced by the current struct are set to 0
   /// as if the other had been replenished with zeros.
   /// <para>This is an O(n) operation with 64 bit parallel execution.</para>
   /// <para>Same as <see cref="IntersectWith(IndexSet)"/>.</para>
   /// </summary>
   /// <param name="other">The <see cref="IndexSet"/> used as operand of the <see cref="And(IndexSet)"/> operation.</param>
   /// <returns>A struct which references the same bits as the current struct.</returns>
   public IndexSet And(IndexSet other) // BitArray
   {
      int smallerArrayLength = Minimum(RequiredLength, other.RequiredLength);
      if (smallerArrayLength > 0)
      {
         Debug.Assert(BitSequence is not null);
         Debug.Assert(other.BitSequence is not null);
         int i;
         for (i = 0; i < smallerArrayLength; i++)
            BitSequence[i] &= other.BitSequence[i];
         for (; i < BitSequence.Length; i++)
            BitSequence[i] = 0;
      }
      return this;
   }

   /// <summary>
   /// Removes all elements from the set / clears all bits.
   /// Same as SetAll(false).
   /// </summary>
   public IndexSet Clear() => SetAll(false); // alike ICollection<int>

   /// <summary>
   /// Removes all elements from the set / clears all bits.
   /// Same as SetAll(false).
   /// </summary>
   void ICollection<int>.Clear() => SetAll(false); // alike ICollection<int>

   /// <summary>
   /// Compares this with another instance of <see cref="IndexSet"/>.
   /// For comparision the sequence of bits of each <see cref="IndexSet"/> is interpreted as a long unsigned integer.
   /// If both instances have different <see cref="Length"/>, the shorter one is considered as beeing filled with 
   /// leading (more significant) 0 (true) bits.
   /// This is a O(n) operation with n~(Length+63)/64.
   /// </summary>
   /// <param name="other">The <see cref="IndexSet"/> to compare with</param>
   /// <returns>&lt;0: if this precedes other; 0: if this is at the same postion as other;
   /// >0: if this follows other</returns>
   public readonly int CompareTo(IndexSet other) // IComparable<IndexSetR>
   {
      int i;
      if (RequiredLength < other.RequiredLength)
      {
         // Compare excess array elements of other against 0
         for (i = other.RequiredLength - 1; i >= RequiredLength; i--)
            if (other.BitSequence![i] != 0)
               return 0ul.CompareTo(other.BitSequence[i]);
      }
      else
      {
         // Compare excess array elements of this against 0
         for (i = RequiredLength - 1; i >= other.RequiredLength; i--)
            if (BitSequence![i] != 0)
               return BitSequence[i].CompareTo(0ul);
      }
      // Compare array elements of this with array elements of other
      for (; i >= 0; i--)
         if (BitSequence![i] != other.BitSequence![i])
            return BitSequence[i].CompareTo(other.BitSequence[i]);

      return 0;
   }


   /// <summary>
   /// Inverts the bits referenced by the current struct: each bit which is 0 is set to 1 and 
   /// each bit which is 1 is set to 0.
   /// <para>This is an O(n) operation with 64 bit parallel execution.</para>
   /// <para>Same as <see cref="Not"/>.</para>
   /// </summary>
   /// <returns>A struct which references the same bits as the current struct.</returns>
   public IndexSet Complement() => Not();

   /// <summary>
   /// Determines whether the set contains the value specified in <paramref name="index"/> /
   /// if the bit at position <paramref name="index"/> is 1 resp. true. This is a O(1) operation.
   /// </summary>
   /// <param name="index"></param>
   /// <returns><see langword="true"/> if the set contains <paramref name="index"/> / the bit at position index is set;
   /// otherwise <see langword="false"/>.</returns>
   public readonly bool Contains(int index) => Get(index);  // ICollection<int>

   /// <summary>
   /// Copies the bits of <paramref name="other"/> into the bits of the current set as far as possible.
   /// If the <see cref="Length"/> of <paramref name="other"/>" exceeds the <see cref="Length"/>
   /// of the current set and some bit <paramref name="other"/>[i]
   /// with i >= current <see cref="Length"/> is 1 (true) an
   /// <see cref="ArgumentException"/> is thrown. If <paramref name="ignoreExcessValues"/>
   /// ist true, the excess bits are not checked and no exception will be thrown.
   /// </summary>
   /// <param name="other">The source to copy from.</param>
   /// <param name="ignoreExcessValues">Supresses the check of excess bits of <paramref name="other"/>.</param>
   /// <returns>The modified current set.</returns>
   public IndexSet CopyFrom(IndexSet other, bool ignoreExcessValues = false)
   {
      if (other.Length <= Length)
      {
         if (other.Length > 0)
            Array.Copy(other.BitSequence!, this.BitSequence!, other.RequiredLength);
         return this;
      }

      Array.Copy(other.BitSequence!, this.BitSequence!, this.RequiredLength);
      BitSequence![^1] &= LastElementMask; // clear excess bits
      if (!ignoreExcessValues)
      {
         bool error = (other.BitSequence![this.RequiredLength - 1] & ~this.LastElementMask) != 0;
         if (!error)
            for (int i = this.RequiredLength; i < other.RequiredLength; i++)
               if (other.BitSequence[i] != 0)
               {
                  error = true;
                  break;
               }
         if (error)
            throw new ArgumentException("");
      }

      return this;
   }

   /// <summary>
   /// Copies all <see cref="Count"/> elements of the set (the indexes of the bits with value 1)
   /// to the <paramref name="destination"/> array.
   /// <para>To copy the contents of another <see cref="IndexSet"/> struct into the current one use <see cref="CopyFrom"/></para>
   /// </summary>
   /// <param name="destination">The destinatione int Array.</param>
   /// <param name="startIndex">The 1st element will be copied to <paramref name="destination"/>[<paramref name="startIndex"/>].
   /// </param>
   /// <exception cref="ArgumentNullException"></exception>
   /// <exception cref="ArgumentOutOfRangeException">the exception will be thrown if <paramref name="destination"/> &lt;0 or &gt; <see cref="Length"/> </exception>
   public void CopyTo(int[] destination, int startIndex) // ICollection<int>
   {
      if (startIndex < 0 || startIndex > destination.Length - this.Count)
         throw new ArgumentOutOfRangeException(
            nameof(startIndex),
            $"The {nameof(startIndex)} argument of {nameof(CopyTo)} is {startIndex}but must be >= 0 and <= {nameof(destination)}.Length - cardinality of the set, which is {this.Count}"); ;

      int destinationIndex = startIndex;
      foreach (int element in this)
         destination[destinationIndex++] = element;
   }

   /// <summary>
   /// Returns the index of the first bit with the given value.
   /// Returns -1 if the set does not contain a bit with the given value.
   /// </summary>
   public int IndexOfFirstBit(bool value = true) // SortedSet<int>
      => IndexOfNextBit(-1, value);

   /// <summary>
   /// Returns the index of the last bit with the given value.
   /// Returns <see cref="Length"/> if the set does not contain a bit with the given value.
   /// </summary>
   public int IndexOfLastBit(bool value = true)  // SortedSet<int>
      => IndexOfPrecedingBit(int.MaxValue, value);

   /// <summary>
   /// Returns the first index greater than <paramref name="lastFound"/>
   /// with the given <paramref name="value"/>.
   /// </summary>
   /// <param name="lastFound">The search starts at index lastFound+1, but starts at 0, if lastFound is &lt; 0</param>
   /// <param name="value"></param> // Todo ///
   /// <returns>Index of found bit or <see cref="Length"/> if not found.</returns>

   public int IndexOfNextBit(int lastFound, bool value = true)
   {
      int start = lastFound + 1;
      if (start < 0)
         start = 0;
      if (start >= Length)
         return Length;
      Debug.Assert(BitSequence is not null);
      int startBitIndex = start & BitIndexMask, // 0 <= startBitIndex <= 63
         startArrayIndex = start >> BitsPerBitIndex; // 0 <= startUInt64Index < BitArray.Length         

      for (int i = startArrayIndex; i < BitSequence.Length; i++)
      {
         UInt64 actual = value ? BitSequence[i] : ~BitSequence[i];

         if (actual == 0)
            continue; // redundant shortcut

         // ignore (clear) the bits preceding startBitIndex in the start element 
         if (i == startArrayIndex)
            actual = (actual >> startBitIndex) << startBitIndex;

         if (actual == 0)
            continue;

         // found UInt64 with set bit (maybe one of the excess bits in the complemented last element);
         int bitPos = (i << BitsPerBitIndex) + BitOperations.TrailingZeroCount(actual);
         return bitPos > Length ? Length : bitPos; ;
      }

      return Length;
   }

   /// <summary>
   /// Returns the last index less than <paramref name="lastFound"/>
   /// with the given <paramref name="value"/>.
   /// </summary>
   /// <param name="lastFound">The search starts at index lastFound-1, but starts at <see cref="Length"/>-1, 
   /// if lastFound is >= <see cref="Length"/></param>
   /// <param name="value"></param>
   /// <returns>Index of found bit or -1 if not found.</returns>
   public int IndexOfPrecedingBit(int lastFound, bool value = true)
   {
      int start = lastFound - 1;
      if (start >= Length)
         start = Length - 1;
      if (start < 0)
         return -1;
      Debug.Assert(BitSequence is not null);

      int startUInt64Index = start >> BitsPerBitIndex; // // start / BitsPerBitIndex

      for (int i = startUInt64Index; i >= 0; i--)
      {
         UInt64 actual = value ? BitSequence[i] : ~BitSequence[i];

         if (actual == 0)
            continue; // redundant shortcut

         // ignore (clear) the bits following startBitIndex in the start element 
         // if startUInt64Index equals bits.Length-1 this also clears the excess bits
         if (i == startUInt64Index)
         {
            int leadingBits = BitsPerArrayElement - 1 - (start & BitIndexMask); // start % BitsPerElement
            actual = (actual << leadingBits) >> leadingBits;
         }

         if (actual == 0)
            continue;

         // found UInt64 with set bit, result >= 0
         return ((i + 1) << BitsPerBitIndex) - 1 - BitOperations.LeadingZeroCount(actual);
      }

      return -1;
   }

   /// <summary>
   /// Appends the <paramref name="ElementNames"/> (or the indexes if no names) of all bits which are true to the 
   /// Stringbuilder <paramref name="sb"/> using the <paramref name="delimiter"/>.
   /// There may be given special texts for the case of all bits or no bits set.
   /// </summary>
   /// <param name="sb">'The Stringbuilder to which the names (or indexes) of the bits with value 1 are appended.</param>
   /// <param name="ElementNames">Defines the name of each bit i 
   /// by <paramref name="ElementNames"/>"/>[i].ToString(), may be null.</param>
   /// <param name="delimiter">The delimiter to be used to separate names.</param>
   /// <param name="allString">Text if all bits are set or null (default).</param>
   /// <param name="emptyString">Text if no bits are set or null (default).</param>
   /// <returns>The argument <paramref name="sb"/></returns>
   /// <exception cref="ArgumentOutOfRangeException">if </exception>
   public StringBuilder ElementsToStringbuilder(
      StringBuilder sb,
      Object[]? ElementNames = null,
      String delimiter = ", ", String? allString = null, String? emptyString = null)
   {
      if (ElementNames != null && ElementNames.Length < Length)
      {
         throw new ArgumentOutOfRangeException
            ($"Length of {nameof(ElementNames)} == {ElementNames.Length} is less than the number of bits == {Length} in call of {nameof(ElementsToStringbuilder)}");
      }

      if (allString != null && AllBitsAre(true))
      {
         sb.Append(allString);
      }
      else
      {
         Boolean isFirst = true;
         foreach (int index in this)
         {
            if (isFirst)
               isFirst = false;
            else
               sb.Append(delimiter);

            sb.Append(
               ElementNames == null || index >= ElementNames.Length || ElementNames[index] == null
               ? index.ToString()
               : ElementNames[index].ToString()
               );
         }

         if (isFirst && emptyString != null)
            sb.Append(emptyString);
      }
      return sb;
   }

   /// <summary>
   /// Modifies the bits referenced by the current struct by performing an "except with" operation
   /// (equivalent to this.Or(<paramref name="other"/>).Xor(<paramref name="other"/>)) between those 
   /// and the bits referenced by the <paramref name="other"/> struct. (Each bit referenced by the current struct
   /// is cleared if the corresponding bit of the <paramref name="other"/> struct is true (1)).
   /// If the <see cref="Length"/> of the current struct is less than the <see cref="Length"/>
   /// of the <paramref name="other"/> struct the excess bits of the <paramref name="other"/> struct are ignored.
   /// <para>ISet&lt;int>: Removes all elements in the specified collection from the current set.</para>
   /// <para>This is an O(n) operation with 64 bit parallel execution.</para>
   /// <para>Same as <see cref="Subtract(IndexSet)"/>.</para>
   /// </summary>
   /// <param name="other">The <see cref="IndexSet"/> used as operand of the operation.</param>
   /// <returns>A struct which references the same bits as the current struct.</returns>
   public IndexSet ExceptWith(IndexSet other) // this.Or(other).Xor(other);
   {
      int smallerArrayLength = Minimum(RequiredLength, other.RequiredLength);

      for (int i = 0; i < smallerArrayLength; i++)
         BitSequence![i] = ExceptWith(BitSequence[i], other.BitSequence![i]);  // does not change excess bits

      return this;
   }


   /// <summary>
   /// ISet&lt;int>: Removes all elements in the specified collection from the current set.
   /// </summary>
   /// <param name="otherEnum"></param>
   public void ExceptWith(IEnumerable<int> otherEnum) // ISet<int>
   {
      foreach (int index in otherEnum)
      {
         if (IsGe0AndLT(index, Length))
            this[index] = false;
         // bits not contained in the set are considered to be false already
      }
      return;
   }

   /// <summary>
   /// Determines if  the selected bit is set.
   /// </summary>
   /// <param name="index">The index of the selected bit.</param>
   /// <returns>
   /// Returns <see langword="true"/> if the selected bit is set.
   /// Else or if <paramref name="index"/> is &lt; 0 or >= <see cref="Length"/>, returns <see langword="false"/>.
   /// </returns>
   public bool Get(int index) // BitArray
   {
      return IsGe0AndLT(index, Length) &&
         (BitSequence![index >> BitsPerBitIndex] & (1UL << index)) != 0;
   }

   /// <summary>
   /// Modifies the bits referenced by the current struct by performing a logical "and" operation between those 
   /// and the bits referenced by the <paramref name="other"/> struct.
   /// If the <see cref="Length"/> of the current struct exceeds the <see cref="Length"/>
   /// of the <paramref name="other"/> the excess bits referenced by the current struct are set to 0
   /// as if the other had been replenished with zeros.
   /// <para>This is an O(n) operation with 64 bit parallel execution.</para>
   /// <para>Same as <see cref="And(IndexSet)"/>.</para>
   /// </summary>
   /// <param name="other">The <see cref="IndexSet"/> used as operand of the <see cref="And(IndexSet)"/> operation.</param>
   /// <returns>A struct which references the same bits as the current struct.</returns>
   public IndexSet IntersectWith(IndexSet other) // alike ISet<int>
      => And(other);

   /// <summary>
   /// ISet&lt;int>: Modifies the current set so that it contains only int values that are also in a specified collection.
   /// </summary>
   /// <param name="otherEnum"></param>
   public void IntersectWith(IEnumerable<int> otherEnum) // ISet<int>
      => And(CopyToNewLimitedLengthBits(otherEnum));

   /// <summary>
   /// Determines whether the bits referenced by the current struct are a proper subset of the 
   /// bits referenced by the <paramref name="other"/> struct. They are a proper subset if 
   /// there is no pair of bits (this[i], other[i]) which is (1,0) and there is at least one pair (0,1).
   /// <para>If the <see cref="Length"/> of this and <paramref name="other"/> differ the
   /// shorter one is asssumed to be filled with zeros.</para>
   /// <para>This is an O(n) operation with 64 bit parallel execution.</para>
   /// </summary>
   /// <param name="other"></param>
   /// <returns> <see langword="true"/> if the bits referenced by the current struct are a proper subset of the 
   /// bits referenced by the <paramref name="other"/> struct, else false. </returns>
   public bool IsProperSubsetOf(IndexSet other) // alike ISet<int>
      => IsSubsetOrSuperset(this.BitSequence, other.BitSequence) == SubsetResult.SubSet; // and not Superset!

   /// <summary>
   /// ISet&lt;int>: Determines whether the current set is a proper (strict) subset of a specified collection.
   /// </summary>
   /// <param name="otherEnum"></param>
   /// <returns>true if the current set is a proper subset of other; otherwise, false.</returns>
   public bool IsProperSubsetOf(IEnumerable<int> otherEnum) // ISet<int>
      => IsProperSubsetOf(
         new(length: Minimum(otherEnum.Max() + 1, Length),
            otherEnum,
            ignoreExcessValues: false));

   /// <summary>
   /// Determines whether the bits referenced by the current struct are a proper superset of the 
   /// bits referenced by the <paramref name="other"/> struct. They are a proper superset if 
   /// there is no pair of bits (this[i], other[i]) which is (0, 1) and there is at least one pair (1, 0).
   /// <para>If the <see cref="Length"/> of this and <paramref name="other"/> differ the
   /// shorter one is asssumed to be filled with zeros.</para>
   /// <para>This is an O(n) operation with 64 bit parallel execution.</para>
   /// </summary>
   /// <param name="other"></param>
   /// <returns> <see langword="true"/> if the bits referenced by the current struct are a proper subset of the 
   /// bits referenced by the <paramref name="other"/> struct, else false. </returns>
   public bool IsProperSupersetOf(IndexSet other) // alike ISet<int>
      => IsSubsetOrSuperset(this.BitSequence, other.BitSequence) == SubsetResult.Superset; // and not Subset!

   /// <summary>
   /// ISet&lt;int>: Determines whether the current set is a proper (strict) superset of a specified collection.
   /// </summary>
   /// <param name="otherEnum">The collection to compare to the current set.</param>
   /// <returns>true if the current set is a proper superset of other; otherwise, false.</returns>
   public bool IsProperSupersetOf(IEnumerable<int> otherEnum)
      => IsSupersetOf(otherEnum) && !SetEquals(otherEnum); // ISet<int> // Todo  optimize & excess bits? 

   /// <summary>
   /// Determines whether the bits referenced by the current struct are a subset of the 
   /// bits referenced by the <paramref name="other"/> struct. They are a subset if 
   /// there is no pair of bits (this[i], other[i]) which (1,0).
   /// <para>If the <see cref="Length"/> of this and <paramref name="other"/> differ the
   /// shorter one is asssumed to be filled with zeros.</para>
   /// <para>This is an O(n) operation with 64 bit parallel execution.</para>
   /// </summary>
   /// <param name="other"></param>
   /// <returns> <see langword="true"/> if the bits referenced by the current struct are a subset of the 
   /// bits referenced by the <paramref name="other"/> struct, else false. </returns>
   public bool IsSubsetOf(IndexSet other)
   {
      int smallerArrayLength = Minimum(RequiredLength, other.RequiredLength);
      int i;
      for (i = 0; i < smallerArrayLength; i++)
         if (ExceptWith(BitSequence![i], other.BitSequence![i]) != 0)
            return false;
      for (; i < RequiredLength; i++)
         if (BitSequence![i] != 0)
            return false;

      return true;
   }

   /// <summary>
   /// ISet&lt;int>: Determines whether a set is a subset of a specified collection.
   /// </summary>
   /// <param name="otherEnum">The collection to compare to the current set.</param>
   /// <returns>true if the current set is a subset of other; otherwise, false.</returns>
   public bool IsSubsetOf(IEnumerable<int> otherEnum) // ISet<int>
   {
      if (IsEmpty)
         return true;

      if (otherEnum is null)
         return IsEmpty;

      IndexSet other = CopyToNewLimitedLengthBits(otherEnum); // ignore additional elements in other
      return IsSubsetOf(other);
   }

   [Flags]
   private enum SubsetResult { NotRelated = 0, SubSet = 1, Superset = 2, Equal = 3 }

   private static SubsetResult IsSubsetOrSuperset(ReadOnlySpan<ulong> a, ReadOnlySpan<ulong> b)
   {
      bool subset = true, superset = true;

      int i;
      for (i = 0; i < Minimum(a.Length, b.Length); i++)
      {
         ulong aXORb = a[i] ^ b[i];
         superset &= (aXORb & b[i]) == 0;
         subset &= (aXORb & a[i]) == 0;

         if (!(subset | superset))
            return SubsetResult.NotRelated;
      }
      for (; i < a.Length; i++) // b[i] assumed to be 0;
      {
         if (a[i] != 0) // subset=false;
            return superset ? SubsetResult.Superset : SubsetResult.NotRelated;
      }
      for (; i < b.Length; i++) // a[i] assumed to be 0;
      {
         if (b[i] != 0) // Superset=false;
            return subset ? SubsetResult.SubSet : SubsetResult.NotRelated;
      }

      return ((subset ? SubsetResult.SubSet : 0) | (superset ? SubsetResult.Superset : 0));
   }

   /// <summary>
   /// Determines whether the bits referenced by the current struct are a superset of the 
   /// bits referenced by the <paramref name="other"/> struct. They are a superset if 
   /// there is no pair of bits (this[i], other[i]) which is (0, 1).
   /// <para>If the <see cref="Length"/> of this and <paramref name="other"/> differ the
   /// shorter one is asssumed to be filled with zeros.</para>
   /// <para>This is an O(n) operation with 64 bit parallel execution.</para>
   /// </summary>
   /// <param name="other"></param>
   /// <returns> <see langword="true"/> if the bits referenced by the current struct are a subset of the 
   /// bits referenced by the <paramref name="other"/> struct, else false. </returns>
   public bool IsSupersetOf(IndexSet other) => other.IsSubsetOf(this);

   /// <summary>
   /// ISet&lt;int>: Determines whether the current set is a superset of a specified collection.
   /// </summary>
   /// <param name="otherEnum">The collection to compare to the current set.</param>
   /// <returns>If other contains the same elements as the current set, the current set is still considered a superset of other.
   /// <para></para></returns>
   public bool IsSupersetOf(IEnumerable<int> otherEnum) // ISet<int>
   {
      foreach (int element in otherEnum)
      {
         if (!IsGe0AndLT(element, Length) || !Get(element))
            return false;
      }
      return true;
   }

   /// <summary>
   /// Determines the indexes of the first and of the last bit with value 1 (true).
   /// </summary>
   /// <returns>Returns the tuple (IndexOfFirstBit(true), IndexOfLastBit(true)), an O(n) operation.
   /// If all bits are false IndexOfFirstBit(true) is <see cref="Length"/> and
   /// -1 is returned als value of Max without additional computation.
   /// </returns>
   public readonly (int indexOfFirstTrue, int indexOfLastTrue) MinAndMax()
   {
      int firstTrue = IndexOfFirstBit(true);
      return (
         firstTrue,
         firstTrue >= Length ? -1 : IndexOfLastBit(true)
         );
   }

   /// <summary>
   /// Inverts the bit values referenced by the current struct: each bit which is 0 is set to 1 and 
   /// each bit which is 1 is set to 0.
   /// <para>This is an O(n) operation with 64 bit parallel execution.</para>
   /// <para>Same as <see cref="Complement"/>.</para>
   /// </summary>
   /// <returns>A struct which references the same bits as the current struct.</returns>
   public IndexSet Not() // alike public System.Collections.BitArray Not ();
   {
      if (RequiredLength > 0)
      {
         int i;
         for (i = 0; i < RequiredLength; i++)
            BitSequence![i] = ~BitSequence[i]; // May set excess bits

         BitSequence![^1] &= LastElementMask; // Clear excess bits
      }
      return this;
   }

   /// <summary>
   /// Modifies the bits referenced by the current struct by performing a logical "or" operation between those 
   /// and the bits referenced by the <paramref name="other"/> struct.
   /// If the <see cref="Length"/> of the current struct is less than the <see cref="Length"/>
   /// of the <paramref name="other"/> struct and at least one of the excess bits referenced by the other struct is 1
   /// an <see cref="ArgumentException"/> is thrown.
   /// <para>This is an O(n) operation with 64 bit parallel execution.</para>
   /// <para>Same as <see cref="UnionWith(IndexSet)"/>.</para>
   /// </summary>
   /// <param name="other">The <see cref="IndexSet"/> used as operand of the <see cref="Or(IndexSet)"/> operation.</param>
   /// <returns>A struct which references the same bits as the current struct.</returns>

   public IndexSet Or(IndexSet other) // alike System.Collections.BitArray Or (System.Collections.BitArray value);
   {

      int smallerArrayLength = Minimum(RequiredLength, other.RequiredLength);
      for (int i = 0; i < smallerArrayLength; i++)
         BitSequence![i] |= other.BitSequence![i];

      if (Length < other.Length)
      {
         Debug.Assert(other.BitSequence is not null);
         bool error = false;
         if (RequiredLength > 0 && ((other.BitSequence[RequiredLength - 1] & ~LastElementMask) != 0))
            error = true; // other set on of the excess bits of this
         else
            for (int i = RequiredLength; i < other.BitSequence.Length; i++)
               if (other.BitSequence[i] != 0)
               { error = true; break; }; // other can not set a bit beyond this.BitArray.Length

         if (error)
            throw new ArgumentException
               ($"{nameof(other)} contains a 1 at an index whích  >= {nameof(Length)} {Length}");
      }

      return this;
   }

   /// <summary>
   /// ISet&lt;int>: Determines whether the current set overlaps with the specified collection.
   /// </summary>
   /// <param name="other">The collection to compare to the current set.</param>
   /// <returns>true if the current set and other share at least one common element; otherwise, false.</returns>
   public bool Overlaps(IndexSet other)
   {
      int smallerArrayLength = Minimum(RequiredLength, other.RequiredLength);

      for (int i = 0; i < smallerArrayLength; i++)
      {
         Debug.Assert(BitSequence is not null);
         Debug.Assert(other.BitSequence is not null);

         if ((BitSequence[i] & other.BitSequence[i]) != 0)
            return true;
      }

      return false;
   }

   /// <summary>
   /// ISet&lt;int>: Determines whether the current set overlaps with the specified collection.
   /// </summary>
   /// <param name="otherEnum">The collection to compare to the current set.</param>
   /// <returns>true if the current set and other share at least one common element; otherwise, false.</returns>
   public bool Overlaps(IEnumerable<int> otherEnum) // ISet<int>
   {
      foreach (int element in otherEnum)
      {
         if (this[element])
            return true; // found common element
      }
      return false;
   }

   /// <summary>
   /// Tests, if the set contains an element and removes this element /
   /// tests if a bit ist set and clears this bit.
   /// </summary>
   /// <param name="index">the element to remove / the bit to clear</param>
   /// <returns>true if the set did contain the element <paramref name="index"/> / the bit had been set;
   /// otherwise (or if <paramref name="index"/> is &lt;0 or >= Length) false.
   /// </returns>
   public bool Remove(int index) // ICollection<int>
   {
      if (Get(index))
      {
         Set(index, false);
         return true;
      };
      return false;
   }

   /// <summary>
   /// Assigns the given <paramref name="value"/> to bit[<paramref name="index"/>].
   /// If <paramref name="index"/> is &lt; 0 or >= <see cref="Length"/> then
   /// if <paramref name="value"/> is 0 <see cref="Set"/> has no effect,
   /// else throws an exception.
   /// </summary>
   /// <param name="index">The index of the bit, which is set.</param>
   /// <param name="value">The value to assign.</param>
   /// <exception cref="ArgumentException">An exception is thrown if 
   /// <paramref name="value"/> is true and <paramref name="index"/> is &lt; 0 or is > <see cref="Length"/>. 
   /// </exception>
   /// <returns>The modified current set.</returns>
   public IndexSet Set(int index, bool value) // BitArray
   {
      if (!IsGe0AndLT(index, Length))
      {
         if (value)
            throw new ArgumentException
               ($"Bit can not be set to true: index is <0 or >= {nameof(Length)} {Length}.");
         return this;
      }

      int arrayIndex = index >> BitsPerBitIndex;
      UInt64 selectedBit = 1UL << index; // C# masks index in << operation

      if (value)
         BitSequence![arrayIndex] |= selectedBit;
      else
         BitSequence![arrayIndex] &= ~selectedBit;

      return this;
   }


   /// <summary>
   /// Sets all bits referenced by the current struct to the given value.
   /// </summary>
   /// <param name="value">All bits are set to this value: false (0) or true(1).</param>
   /// <returns>A struct which references the same bits as the current struct.</returns>
   public IndexSet SetAll(bool value) // alike BitArray public void SetAll (bool value);
   {
      if (BitSequence is null)
         return this;

      if (value)
      {
         Array.Fill<UInt64>(BitSequence, ~0UL);
         BitSequence![^1] &= LastElementMask;
      }
      else
      {
         Array.Clear(BitSequence);
      }
      return this;
   }

   /// <summary>
   /// Determines whether the indexes of the bits with value 1 of the current <see cref="IndexSet"/>
   /// and of the specified <see cref="IndexSet"/> are equal.  
   /// </summary>
   /// <param name="other"></param>
   /// <returns><see langword="true"/> if the given <see cref="IndexSet"/> is equal to <paramref name="other"/>
   /// (if the <see cref="Length"/> is different, the shorter one is assumed to be filled with 0); 
   /// otherwise <see langword="false"/></returns>
   public bool SetEquals(IndexSet other) // alike ISet<int>
      => CompareTo(other) == 0;

   /// <summary>
   /// Determines whether the current <see cref="IndexSet"/> and the specified <see cref="IndexSet"/> have the same sequence of bits.
   /// If the <see cref="Length"/> differ, the shorter one is considered as extended with zeros.
   /// </summary>
   /// <param name="otherEnum">The collection to compare to the current set.</param>
   /// <returns>true if the current set is equal to other; otherwise, false.</returns>
   public bool SetEquals(IEnumerable<int> otherEnum) // ISet<int>.SetEquals(System.Collections.Generic.IEnumerable<T> other)
   {
      (int otherMin, int otherMax, int otherCount, bool isSorted) = GetMinMaxCountSorted(otherEnum); // O(otherEnum.Length)
      // Simple tests ( O(this.Length) before allocation of new Bits (..., otherEnum)
      if (otherMin != IndexOfFirstBit(true) || otherMax != IndexOfLastBit(true)
         || (isSorted ? otherCount != Count : otherCount < Count))
         // < because other may contain counted duplicates if not sorted
         return false;
      // Todo  avoid new(...)  if other is sorted
      // 
      return CompareTo(new(otherMax + 1, otherEnum, ignoreExcessValues: true)) == 0;
   }

   /// <summary>
   /// Modifies the bits referenced by the current struct by performing an "except with" operation
   /// (equivalent to this.Or(<paramref name="other"/>).Xor(<paramref name="other"/>)) between those 
   /// and the bits referenced by the <paramref name="other"/> struct. (Each bit referenced by the current struct
   /// is cleared if the corresponding bit of the <paramref name="other"/> struct is true (1)).
   /// If the <see cref="Length"/> of the current struct is less than the <see cref="Length"/>
   /// of the <paramref name="other"/> struct the excess bits of the <paramref name="other"/> struct are ignored.
   /// <para>This is an O(n) operation with 64 bit parallel execution.</para>
   /// <para>Same as <see cref="ExceptWith(IndexSet)"/>.</para>
   /// </summary>
   /// <param name="other">The <see cref="IndexSet"/> used as operand of the operation.</param>
   /// <returns>A struct which references the same bits as the current struct.</returns>
   public IndexSet Subtract(IndexSet other) => ExceptWith(other);

   /// <summary>
   ///  Toggles all bits referenced by the current struct which are true (1) in
   ///  the bits referenced by the <paramref name="other"/> struct ("xor").
   /// </summary>
   /// <param name="other">The <paramref name="other"/> set.</param>
   ///<returns>A struct which references the same bits as the current struct.</returns>
   public IndexSet SymmetricExceptWith(IndexSet other) => Xor(other);

   /// <summary>
   ///  ISet&lt;int>: Toggles all bits of the current set with indexes given by <paramref name="otherEnum"/>
   ///  so that it contains only bits that are present either in the
   ///  current set or in the specified <paramref name="otherEnum"/> but not in both (xor).
   /// </summary>
   /// <param name="otherEnum">The collection to compare to the current set.</param>
   /// <exception cref="System.ArgumentException"></exception>
   void ISet<int>.SymmetricExceptWith(IEnumerable<int> otherEnum)
   {
      foreach (int other in otherEnum)
         if (IsGe0AndLT(other, Length))
            this[other] = !this[other];
         else
            throw new ArgumentException
               ($"{nameof(otherEnum)} contains the index {other} whích is &lt; 0 or >= {nameof(Length)} {Length}");
      return;
   }

   /// <summary>
   /// Returns a string that represents the elements contained in the set.
   /// </summary>
   /// <returns>A comma separated list of all indexes.</returns>
   public override string ToString()
   {
      return ElementsToStringbuilder(new StringBuilder(Length)).ToString();
   }

   /// <summary>
   /// Adds all elements of the specified set, which are less than Length of the current set, to the current set.
   /// The same as <see cref="Or(IndexSet)"/>.
   /// </summary>
   /// <param name="other">The specified set containig the elements to be added.</param>
   /// <returns>The modified current set.</returns>
   public IndexSet UnionWith(IndexSet other) => Or(other); // alike ISet<int>.UnionWith

   /// <summary>
   /// ISet&lt;int>: Modifies the current set so that it contains all elements that are present
   /// in the current set, in the specified collection, or in both.
   /// </summary>
   /// <param name="otherEnum">The collection to compare to the current set.</param>
   /// <exception cref="ArgumentException">if <paramref name="otherEnum"/> contains an index
   /// less than 0 or ><see cref="Length"/></exception>
   public void UnionWith(IEnumerable<int> otherEnum) // ISet<int>.
   {
      foreach (int element in otherEnum)
         this[element] = true; // this[] handles element out of bounds
   }

   /// <summary>
   /// Modifies the bits referenced by the current struct by performing a logical "xor" operation between those 
   /// and the bits referenced by the <paramref name="other"/> struct.
   /// If the <see cref="Length"/> of the current struct is less than the <see cref="Length"/>
   /// of the <paramref name="other"/> struct and at least one of the excess bits referenced by the other struct is 1
   /// an <see cref="ArgumentException"/> is thrown.
   /// <para>This is an O(n) operation with 64 bit parallel execution.</para>
   /// <para>Same as <see cref="UnionWith(IndexSet)"/>.</para>
   /// </summary>
   /// <param name="other">The <see cref="IndexSet"/> used as operand of the <see cref="And(IndexSet)"/> operation.</param>
   /// <returns>A struct which references the same bits as the current struct.</returns>

   public IndexSet Xor(IndexSet other) // alike public System.Collections.BitArray Xor (System.Collections.BitArray value);
   {

      int smallerArrayLength = Minimum(RequiredLength, other.RequiredLength);
      for (int i = 0; i < smallerArrayLength; i++)
         BitSequence![i] ^= other.BitSequence![i];

      if (Length < other.Length)
      {
         // did other set one of the excess bits? 
         Debug.Assert(other.BitSequence is not null);
         bool error = false;
         if (RequiredLength > 0 && ((other.BitSequence[RequiredLength - 1] & ~LastElementMask) != 0))
            error = true; // other set one of the excess bits
         else
            for (int i = RequiredLength; i < other.BitSequence.Length; i++)
               if (other.BitSequence[i] != 0)
               { error = true; break; }; // other can not set the bit beyond this.BitArray.Length

         if (error)
            throw new ArgumentException
               ($"{nameof(other)} contains a 1 at an index whích  >= {nameof(Length)} {Length}");
      }

      return this;
   }

   /* IEnumerable and IEnumerable<T> methods*/

   /* --------------------------------------------------------------- */

   /// <summary>
   /// Bits: This enumerator iterates through the indexes of all bits with the given <paramref name="value"/>.
   /// <para>Set: ... iterates through all numbers of the set, if <paramref name="value"/> == true,
   /// else throught all numbers of the of the complement of the set.</para>
   /// </summary>
   /// <returns></returns>
   public IEnumerable<int> IndexesOfBitsWithValue(Boolean value)
   {
      for (int i = 0; i < Length; i++)
         if (Get(i) == value)
            yield return i;
      yield break;
   }


   /// <summary> Returns an enumerator that iterates through the indexes of the bits which are 1.
   /// </summary>
   /// <returns>An enumerator that can be used to iterate through the set.</returns>
   IEnumerator<int> IEnumerable<int>.GetEnumerator()
   {
      return new GenericBitsEnumerator(this);
   }

   /// <summary>
   /// This IEnumerator&lt;int> iterates through all elements of the set
   /// == iterates through the indices of the bits which are 1
   /// </summary>
   private class GenericBitsEnumerator : IEnumerator<int>
   {
      private readonly IndexSet set;
      private int currentIndex;

      internal GenericBitsEnumerator(IndexSet thisSet)
      {
         this.set = thisSet;
         this.currentIndex = -1;
      }

      public virtual bool MoveNext()
         => (currentIndex = set.IndexOfNextBit(currentIndex)) < set.Length;

      int IEnumerator<int>.Current
      {
         get
         {
            if (currentIndex == -1)
               throw new InvalidOperationException("The Current property is undefined before first call of MoveNext()");
            if (currentIndex >= set.Length)
               throw new InvalidOperationException("The Current property is undefined because the end of the set has been reached");
            return currentIndex;
         }
      }

      object IEnumerator.Current
      {
         get
         {
            if (currentIndex == -1)
               throw new InvalidOperationException("The Current property is undefined before first call of MoveNext()");
            if (currentIndex >= set.Length)
               throw new InvalidOperationException("The Current property is undefined because the end of the set has been reached");
            return currentIndex;
         }
      }

      public void Reset() => currentIndex = -1;

      void IDisposable.Dispose() { }
   }


   /* ------------------------------------------------------------------ */
   /// <summary>
   /// Returns an enumerator which iterates through all indexes of bits with value 1 (true).
   /// </summary>
   /// <returns></returns>
   IEnumerator IEnumerable.GetEnumerator()
   {
      return new BitsEnumerator(this);
   }

   /// <summary>
   /// This IEnumerator iterates through all elements of the set
   /// == iterates through the indices of the bits which are 1
   /// </summary>
   private class BitsEnumerator : IEnumerator
   {
      private readonly IndexSet set;
      private int currentIndex;

      internal BitsEnumerator(IndexSet thisSet)
      {
         this.set = thisSet;
         this.currentIndex = -1;
      }

      public virtual bool MoveNext()
         => (currentIndex = set.IndexOfNextBit(currentIndex)) < set.Length;

      public virtual Object Current
      {
         get
         {
            if (IsGe0AndLT(currentIndex, set.Length))
               return currentIndex;

            if (currentIndex == -1)
               throw new InvalidOperationException("The Current property is undefined before first call of MoveNext()");
            throw new InvalidOperationException("The Current property is undefined because the end of the set has been reeached");
         }
      }

      public void Reset() => currentIndex = -1;
   }

}