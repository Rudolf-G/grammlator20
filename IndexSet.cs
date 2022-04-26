using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Numerics;
using System.Text;

namespace IndexSetNamespace
{
   /// <summary>
   /// This class implements sets of numbers (within a specific range 0 .. Length for each set) with methods
   /// alike the methods of <see cref="SortedSet{T}"/> with T == int, but with a very different implementation by arrays of bits.
   /// Most methods are implemented using 64 bit operations.
   /// This class can be used as a limited replacement of <see cref="BitArray"/> with less restrictions,
   /// additonal methods and, especially if Length is &lt;= 64, better performance. The semantics of some methods are different.
   /// <para>
   /// Length is assigned for each set when the constructor ist called. The Length of a set can not be changed afterwards.</para>
   /// <para>A set is created with a static method e.g. <see cref="Create(int)"/>,
   /// defining its <see cref="Length"/>. Depending on the given length <see cref="Create(int)"/>
   /// creates a set of type <see cref="SmallIndexSet"/> or <see cref="LargeIndexSet"/> or
   /// returns a singleton of type <see cref="ZeroLengthIndexSet"/>.</para> 
   /// <para>Set methods alike a.UnionWith(b) are not only defined for sets a and b based on
   /// universal sets with the same <see cref="Length"/>. But if b.Length is greater and b contains
   /// indexes >= a.Length, those elements are ignored: only the subset of b which fits into a is used. </para>
   /// <para>The names of most of the set methods are the same as in <see cref="SortedSet{T}"/>.
   /// Some methods, e.g. <see cref="IntersectWith(IndexSet)"/>, have alternate names, e.g. <see cref="And(IndexSet)"/>,
   /// to be compatible with <see cref="BitArray"/>.</para>
   /// </summary>
   public sealed class IndexSet : ICollection<int>, IEnumerable<int>, IComparable<IndexSet>, ISet<int>
   //, IReadOnlyCollection<int>
   // ISet<int>, IReadOnlySet<int>, IReadOnlyCollection<T>, IDeserializationCallback, ISerializable 
   {
      /// <summary>
      /// The number of elements of the universal set (the number of bits in internal memory).
      /// The <see cref="IndexSet"/> may contain elements in the range 0..&lt;<see cref="Length"/>
      /// </summary>
      public int Length { get; init; }

      /// <summary>
      /// The first 64 bits of the <see cref="SmallIndexSet"/>, excess bits are always be set to 0!
      /// </summary>
      private UInt64 FirstBits = 0UL; // to avoid 40 bytes memory allocation for Memory and for Array

      /// <summary>
      /// <see cref="AdditionalBits"/> are only allocated if Length is greater 64, else points to the singleton Array.Empty with Length==0. 
      /// If an array is allocated, this causes an overhead of 24 bytes (IndexSets with up to 64 bits allocate 40 bytes,
      /// an IndexSet with 65 bits allocates 72 bytes (24 bytes + 8 bytes to stores additional bits).
      /// </summary>
      readonly UInt64[] AdditionalBits; // 4 bytes (pointer to Array.Empty) not used for small IndexSets

      internal const int BitsPerArrayElement = 64;
      internal const int BitsPerBitIndex = 6; // ld(BitsPerArrayElement)
      internal const int BitIndexMask = ~0 >> (BitsPerArrayElement - BitsPerBitIndex); // >> 58

      /// <summary>
      /// This private constructor sets the <see cref="Length"/> of the <see cref="IndexSet"/>. It is intended for internal use only. 
      /// Public access to constructors is provided by the static methods <see cref="Create(int)"/>, <see cref="Create(IndexSet)"/> etc.
      /// </summary>
      /// <param name="length"></param>
      private IndexSet(int length)
      {
         if (length < 0)
            throw new ArgumentException($"The {nameof(IndexSet)} constructor was called with {nameof(length)}< 0");
         Length = length;
         if (length <= 64)
         {
            AdditionalBits = Array.Empty<UInt64>(); // a singleton
         }
         else
         {
            int arrayLength = (length + (BitsPerArrayElement - 1)) >> BitsPerBitIndex;
            AdditionalBits = new UInt64[arrayLength - 1]; // use FirstBits
         }
      }

      /// <summary>
      /// Including Firstbits: AdditionalBits.Length + 1;
      /// </summary>
      private int ArrayLength => AdditionalBits.Length + 1;
      // 1; 1, 1, ...1;
      // different from => (Length + (BitsPerArrayElement - 1)) >> BitsPerBitIndex; // 0; 1, 1, ....

      /// <summary>
      ///  For Length == 0, 1, 2, 3, .. the mask is -1, 1, 3, 7, ...
      /// </summary>
      private UInt64 LastElementMask => (~0UL) >> ((ArrayLength << BitsPerBitIndex) - Length); // -1, 1, 3, 7, ...

      /// <summary>
      /// Returns the mimum value, the maximum value and the count of elements of <paramref name="initialIndexes"/>.
      /// Returns (0, -1, 0) if <paramref name="initialIndexes"/> is null or empty.
      /// </summary>
      /// <returns>The tuple (int min, int max, int count).</returns>
      private static (int min, int max, int count) GetMinMaxAndCount(IEnumerable<int> initialIndexes)
      {

         int max = int.MinValue;
         int min = int.MaxValue;
         int count = 0;

         switch (initialIndexes)
         {
            case null: break;
            case int[] a: // avoid iterator
               for (int i = 0; i < a.Length; i++)
               {
                  int index = a[i];
                  if (index > max)
                     max = index;
                  if (index < min)
                     min = index;
               }
               count = a.Length;
               break;

            default:
               foreach (int index in initialIndexes)
               {
                  if (index > max)
                     max = index;
                  if (index < min)
                     min = index;
                  count++;
               }
               break;
         }

         if (count == 0)
            return (0, -1, 0); // empty set

         return (min, max, count);
      }

      private bool HasExcessElements(IEnumerable<int> otherEnum)
      {
         foreach (int element in otherEnum)
            if (element < 0 || element >= Length)
               return true;
         return false;
      }

      /// <summary>
      /// For internal use: 
      /// Simulates a single array composed of FirstBits and AdditionalBits and leading and trailing zeros.
      /// </summary>
      /// <param name="i">The zero based index</param>
      /// <returns>0, if <paramref name="index"/> is >= <see cref="Length"/> or &lt;0,
      /// <see cref="FirstBits"/> if , if <paramref name="index"/> is 0,
      /// else <see cref="AdditionalBits"/>[<paramref name="i"/>-1].
      /// </returns>
      private UInt64 GetUInt64(int i)
      {
         switch (i)
         {
            case 0:
               return FirstBits;
            case > 0:
               if (i <= AdditionalBits.Length)
                  return AdditionalBits[i - 1];
               break;
         }

         return 0;
      }

      private void SetUInt64(int i, UInt64 value)
      {
         switch (i)
         {
            case 0:
               FirstBits = value;
               return;
            case > 0 when (i <= AdditionalBits.Length):
               AdditionalBits[i - 1] = value;
               return;
         }
         if (value == 0)
            return; // correct because the simulated excess bits are 0

         ThrowAssignmentException();
      }

      private static void ThrowAssignmentException() =>
         throw new ArgumentException("Value can not be assigned: index out of range of IndexSet and value !=0 ");

      private void ClearExcessBits()
      {
         switch (Length)
         {
            case 0:
               FirstBits = 0;
               return;
            case <= BitsPerArrayElement:
               FirstBits &= LastElementMask;
               return;
            default:
               AdditionalBits[^1] &= LastElementMask;
               return;
         }
      }

      /// <summary>
      /// This static method constructs an IndexSet with the same length as the source and copies the source into the new IndexSet.
      /// </summary>
      /// <param name="source">IndexSet which will be copied into the new IndexSet</param>
      /// <returns>new IndexSet</returns>
      public static IndexSet Create(IndexSet source) => Create(source.Length).CopyFrom(source);

      /// <summary>
      /// Depending on <paramref name="maxCardinality"/> this static method selects a type derived from <see cref="IndexSet"/>
      /// and returns a new instance of this type.
      /// </summary>
      /// <param name="maxCardinality">The <see cref="Length"/> of the new <see cref="IndexSet"/>.
      /// If &lt;= 0 then 0 is used.</param>
      /// <returns>A new empty IndexSet of type <see cref="SmallIndexSet"/> or <see cref="LargeIndexSet"/>
      /// or the singleton <see cref="IndexSetWithLength0"/> of type <see cref="ZeroLengthIndexSet"/>.</returns>
      public static IndexSet Create(int maxCardinality)
      {
         return maxCardinality switch
         {
            <= 0 => IndexSetWithLength0,
            _ => new IndexSet(maxCardinality),
         };
      }

      /// <summary>
      /// Depending on <paramref name="maxCardinality"/> this static method selects a type derived from <see cref="IndexSet"/>
      /// and returns a new instance of this type initialized with the <paramref name="initialIndexes"/>.
      /// </summary>
      /// <param name="maxCardinality">All elements of the new set must be in the range 0..<paramref name="maxCardinality"/>.</param>
      /// <param name="initialIndexes">All elements of <paramref name="initialIndexes"/> will be added to the new set.
      /// Values which are not within the Range of the set are ignored.</param>
      /// <returns>The new IndexSet of type <see cref="SmallIndexSet"/> or <see cref="LargeIndexSet"/>
      /// or the singleton <see cref="IndexSetWithLength0"/> of type <see cref="ZeroLengthIndexSet"/>.</returns>
      public static IndexSet Create(int maxCardinality, IEnumerable<int> initialIndexes)
      {
         IndexSet newSet = Create(maxCardinality);

         foreach (int initialValue in initialIndexes)//  i initialValue in initialIndexes)
         {
            if (initialValue < 0 || initialValue >= maxCardinality)
               continue;
            newSet.Set(initialValue, true);
         }
         return newSet;
      }

      /// <summary>
      /// Creates an <see cref="IndexSet"/> with length = Mimimum(max+1, this.Length)
      /// whereby max is the largest element in <paramref name="initialIndexes"/> and copies
      /// all values vom <paramref name="initialIndexes"/>, which are within the range of the new set.
      /// </summary>
      /// <param name="initialIndexes"></param>
      /// <returns></returns>
      public IndexSet CreateLimited(IEnumerable<int> initialIndexes)
      {
         (_, int max, _) = GetMinMaxAndCount(initialIndexes);
         int length = Minimum(max + 1, Length);
         return Create(length, initialIndexes);
      }

      /// <summary>
      /// A static instance of <see cref="IndexSet"/> based on the empty set 0..&lt;0 as universal set. This set contains no elements.
      /// No elements can be added.
      /// </summary>
      public static readonly IndexSet IndexSetWithLength0 = new(0);

      private static int Minimum(int a, int b) => a <= b ? a : b;
      private static int Maximum(int a, int b) => a <= b ? b : a;
      private static bool IsGe0AndLT(int x, int length) //   => (x >= 0) & (x <= length);
           => unchecked((uint)x < (uint)length);
      private static UInt64 ExceptWith(UInt64 a, UInt64 b) => (a | b) ^ b; // minuend.Or(subtrahend).Xor(subtrahend)

      /* ****** Properties ****** */

      /// <summary>
      /// Returns the number of elements in the set (greater or equal 0 and less than Length). It is a fast implemented O(n) operation.
      /// Implements ICollection<int>.Count alike SortedSet.Count, but has different semantics than BitArray.Count.
      /// </summary>
      public int Count
      {
         get
         {
            int count = 0;
            for (int i = 0; i < ArrayLength; i++)
               count += System.Numerics.BitOperations.PopCount(GetUInt64(i));
            return count;
         }
      }

      /// <summary>
      /// Tests, if the set contains all indexes from 0 to Length-1 / if all relevant bits are set.
      /// Will return <see langword="false"/>, if Length == 0.
      /// </summary>
      public bool IsComplete
      {
         get
         {
            int i;
            for (i = 0; i <= ArrayLength - 2; i++)
               if (GetUInt64(i) != ~0UL)
                  return false;
            return GetUInt64(i) == LastElementMask; // ZeroLength will return false because mask =-1 !
         }

      }

      /// <summary>
      /// Tests, if the set ist empty / all relevant bits are 0 (false).
      /// Will return <see langword="true"/>, if Length == 0.
      /// </summary>
      public bool IsEmpty
      {
         get
         {
            for (int i = 0; i < ArrayLength; i++)
               if (GetUInt64(i) != 0)
                  return false;
            return true;  // ZeroLength will return true
         }
      }

      /// <summary>
      /// Is always false.
      /// </summary>
      public bool IsReadOnly { get { return false; } } // ICollection<int>

      /// <summary>
      /// Returns the largest value in the set. Returns -1 if the set is empty.
      /// </summary>
      public int Max  // SortedSet<int>
      {
         get
         {
            for (int i = ArrayLength - 1; i >= 0; i--)
            {
               UInt64 actual = GetUInt64(i);
               if (actual == 0)
                  continue; // fast O(n) operation

               // found UInt64 with set bit
               // O(1) operation
               return ((i + 1) << BitsPerBitIndex) - 1 - BitOperations.LeadingZeroCount(actual);
            }

            return -1;
         }
      }

      /// <summary>
      /// Returns the smallest value in the set. Returns Length if the set is empty.
      /// </summary>
      public int Min // SortedSet<int>
      {
         get
         {
            for (int i = 0; i < ArrayLength; i++)
            {
               UInt64 actual = GetUInt64(i);
               if (actual == 0)
                  continue;

               // found UInt64 with set bit
               return (i << BitsPerBitIndex) + BitOperations.TrailingZeroCount(actual);
            }

            return Length;
         }

      }


      /// <summary>
      /// Tests if <paramref name="index"/> is member of the set or adds  <paramref name="index"/> to the set /
      /// Gets or sets the value of the bit at the position <paramref name="index"/> in the <see cref="IndexSet"/>.
      /// If <paramref name="index"/> is out of the Range of the set, returns 0 resp. has no effect.
      /// </summary>
      /// <param name="index"></param>
      /// <returns>true, if <paramref name="index"/>"/> is a member of the set /
      /// if the bit as position index is set</returns>
      public bool this[int index]
      {
         get { return Get(index); }
         set { Set(index, value); }
      }

      /* ****** Methods ****** */

      /// <summary>
      /// Adds the index to the set / sets the bit [index] to 1 (true).
      /// </summary>
      /// <param name="index"></param>
      public void Add(int index) => Set(index, true); // ICollection<int>

      /// <summary>
      /// Removes all indexes from the current set which are not in the specified set /
      /// Clears all bits in the current set which are 0 in the specified set.
      /// Same as <see cref="IntersectWith(IndexSet)"/>.
      /// </summary>
      /// <param name="other"></param>
      /// <returns>The modified current set.</returns>

      bool ISet<int>.Add(int index)
      {
         if (Get(index))
            return false;
         Set(index, true);
         return true;
      }

      public IndexSet And(IndexSet other) // BitArray
      {
         int smallerArrayLength = Minimum(ArrayLength, other.ArrayLength);

         int i;
         for (i = 0; i < smallerArrayLength; i++)
            SetUInt64(i, GetUInt64(i) & other.GetUInt64(i));
         for (; i < ArrayLength; i++)
            SetUInt64(i, 0);

         return this;
      }

      /// <summary>
      /// Removes all elements from the set / clears all bits.
      /// Same as SetAll(false).
      /// </summary>
      public void Clear() => SetAll(false); // ICollection<int>

      /// <summary>
      /// Compares this with another instance of <see cref="IndexSet"/> with the same <see cref="Length"/>.
      /// For comparision the sequence of bits of each <see cref="IndexSet"/> is interpreted as a long unsigned integer.
      /// This is a O(n) operation with n~(Length+63)/64.
      /// </summary>
      /// <param name="other">The <see cref="IndexSet"/> to compare with</param>
      /// <returns>&lt;0: if this precedes other; 0: if this is at the same postion as other;
      /// >0: if this follows other</returns>
      public int CompareTo(IndexSet? other) // IComparable<IndexSet>
      {
         if (other is null)
            return IsEmpty ? 0 : +1;

         int largerArrayLength = Maximum(ArrayLength, other.ArrayLength);

         int i;
         for (i = largerArrayLength - 1; i >= 0; i--)
            if (GetUInt64(i) != other.GetUInt64(i))
               return GetUInt64(i).CompareTo(other.GetUInt64(i));

         return 0;
      }


      /// <summary>
      /// Replaces the content of the current set by its complement / inverts all bits from 0 (false) to 1 (true) and from 1 (true) to 0 (false).
      /// </summary>
      /// <returns>The modified current set.</returns>
      public IndexSet Complement() => Not();  // complement U - A

      /// <summary>
      /// Determines whether the set contains the value spcified in <paramref name="index"/> /
      /// if the bit at position <paramref name="index"/> is 1 resp. true. This is a O(1) operation.
      /// </summary>
      /// <param name="index"></param>
      /// <returns>true if the set contains <paramref name="index"/> / the bit at position index is set; otherwise false</returns>
      public bool Contains(int index) => Get(index);  // ICollection<int>

      /// <summary>
      /// Replaces the content of the current set by the content of the source set.
      /// If the Length of the source exceeds the Length of the current <see cref="IndexSet"/>,
      /// excess elements / bits of the source are ignored.
      /// </summary>
      /// <param name="source">The source to copy from.</param>
      /// <returns>The modified current set.</returns>
      public IndexSet CopyFrom(IndexSet other)
      {

         for (int i = 0; i < ArrayLength; i++)
            SetUInt64(i, other.GetUInt64(i));

         ClearExcessBits();

         return this;
      }

      /// <summary>
      /// Copies all <see cref="Count"/> elements of the set (the indices of the bits with value 1) to the <paramref name="destination"/> array.
      /// <para>To copy one <see cref="IndexSet"/> from another use <see cref="CopyFrom"/></para>
      /// </summary>
      /// <param name="destination">The destinatione int Array</param>
      /// <param name="startIndex">the 1st element will be copied to <paramref name="destination"/>[<paramref name="startIndex"/>]</param>
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

      // Todo ///
      public bool SetEquals(IndexSet other) // alike ISet<int>
         => CompareTo(other) == 0;

      // Todo ///
      public bool SetEquals(IEnumerable<int> otherEnum) // ISet<int>
      {
         (int otherMin, int otherMax, int otherCount) = GetMinMaxAndCount(otherEnum);
         if (otherMin != Min || otherMax != Max || otherCount < Count) // other may contain duplicates ! 
            return false;
         IndexSet otherSet = Create(otherMax + 1, otherEnum); // remove duplicates
                                                              // Todo  simpler solution if sorted
         return CompareTo(Create(otherMax + 1, otherSet)) == 0;
      }

      /// <summary>
      /// Appends the <paramref name="ElementNames"/> (or the indexes if no names) of all bits which are true to the 
      /// Stringbuilder <paramref name="sb"/> using the <paramref name="delimiter"/>.
      /// There may be given special texts for the case of all bits or no bits set.
      /// </summary>
      /// <param name="Bits">the Bitarray</param>
      /// <param name="sb">the Stringbuilder to which the names of the set bits are appended</param>
      /// <param name="ElementNames">defines the name of each bit i by BitNames[i].ToString(), may be null</param>
      /// <param name="delimiter">the delimiter to be used to separate names, default</param>
      /// <param name="allString">Text if all bits are set or null (default)</param>
      /// <param name="emptyString">Text if no bits are set or null (default)</param>
      /// <returns>the argument <paramref name="sb"/></returns>
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

         if (allString != null && IsComplete)
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
                  ElementNames == null || ElementNames[index] == null
                  ? index.ToString()
                  : ElementNames[index].ToString()
                  );
            }

            if (isFirst && emptyString != null)
               sb.Append(emptyString);
         }
         return sb;
      }

      // Todo ///
      public IndexSet ExceptWith(IndexSet other) // minuend.Or(subtrahend).Xor(subtrahend);
      {
         int smallerArrayLength = Minimum(ArrayLength, other.ArrayLength);

         for (int i = 0; i < smallerArrayLength; i++)
            SetUInt64(i, ExceptWith(GetUInt64(i), other.GetUInt64(i)));  // does not change excess

         return this;
      }


      // Todo ///
      public void ExceptWith(IEnumerable<int> otherEnum) // ISet<int>
      {
         switch (otherEnum)
         {
            case null: return; // nothing to subtract
            case int[] a:
               for (int i = 0; i < a.Length; i++)
               {
                  int index = a[i];
                  if (IsGe0AndLT(index, Length))
                     this[index] = false;
                  // ignore indexes not contained in the set
               }
               return;
            default:
               foreach (int index in otherEnum)
               {
                  if (IsGe0AndLT(index, Length))
                     this[index] = false;
                  // ignore indexes not contained in the set
               }
               return;
         }
      }

      /// <summary>
      /// Determines if an certain element is member of the set / if a certain bit is set.
      /// </summary>
      /// <param name="index">The element / bit to test.</param>
      /// <returns>
      /// Returns <see langword="true"/> if set contains the element <paramref name="index"/> / if the bit index is set.
      /// Else or if <paramref name="index"/> is not within the range of the set, returns <see langword="false"/>;
      /// </returns>
      public bool Get(int index) // BitArray
            => IsGe0AndLT(index, Length) &&
            ((GetUInt64(index >> BitsPerBitIndex) & (1UL << index)) != 0);

      // Todo ///
      public IndexSet IntersectWith(IndexSet other)
         => And(other);

      // Todo ///
      public void IntersectWith(IEnumerable<int> otherEnum) // ISet<int>
         => And(CreateLimited(otherEnum));

      // Todo ///
      public bool IsProperSubsetOf(IndexSet other)
         => IsSubsetOf(other) & !SetEquals(other);

      // Todo ///
      public bool IsProperSubsetOf(IEnumerable<int> otherEnum) // ISet<int>
         => IsProperSubsetOf(CreateLimited(otherEnum));

      // Todo ///
      public bool IsProperSupersetOf(IndexSet other)
         => other.IsProperSubsetOf(this);

      // Todo ///
      public bool IsProperSupersetOf(IEnumerable<int> otherEnum)
         => IsSupersetOf(otherEnum) && !EqualsSet(otherEnum); // ISet<int> 

      // Todo ///
      public bool IsSubsetOf(IndexSet other)
      {
         if (other is null)
            return IsEmpty;

         int smallerArrayLength = Minimum(ArrayLength, other.ArrayLength);
         int i;
         for (i = 0; i < smallerArrayLength; i++)
            if (ExceptWith(GetUInt64(i), other.GetUInt64(i)) != 0)
               return false;
         for (; i < ArrayLength; i++)
            if (GetUInt64(i) != 0)
               return false;

         return true;
      }

      // Todo ///
      public bool IsSubsetOf(IEnumerable<int> otherEnum) // ISet<int>
      {
         if (IsEmpty)
            return true;

         if (otherEnum is null)
            return IsEmpty;

         IndexSet other = CreateLimited(otherEnum); // ignore additional elements in other
         return IsSubsetOf(other);
      }

      // Todo ///
      public bool IsSupersetOf(IndexSet other) => other.IsSubsetOf(this);

      // Todo ///
      public bool IsSupersetOf(IEnumerable<int> otherEnum) // ISet<int>
      {
         foreach (int element in otherEnum)
         {
            if (element < 0 || element >= Length)
               continue; // ignore elements out of range
            if (!this[element])
               return false;
         }
         return true;
      }

      /// <summary>
      /// Returns the tuple (Min, Max).
      /// </summary>
      /// <returns></returns>
      public (int min, int max) MinAndMax() => (Min, Max);

      /// <summary>
      /// Find the next element and return the index of this element or this.count if not found
      /// </summary>
      /// <param name="lastFound">starts at lastFound+1, starts at 0, if lastFound is -1</param>
      /// <param name="complement">search in complemented set</param>
      /// <returns>Index of found element or this.Length if not found</returns>
      public int Next(int lastFound, bool complement = false)
      {
         int start = lastFound + 1;
         if (start < 0)
            start = 0;
         if (start >= Length)
            return Length;
         int startBitIndex = start & BitIndexMask, // 0 <= startBitIndex <= 63
            startUInt64Index = start >> BitsPerBitIndex; // 0 <= startUInt64Index < ArrayLength         

         for (int i = startUInt64Index; i < ArrayLength; i++)
         {
            UInt64 actual = complement ? ~GetUInt64(i) : GetUInt64(i);

            if (actual == 0)
               continue; // redundant shortcut

            // ignore (clear) the bits preceding startBitIndex in the start element 
            if (i == startUInt64Index)
               actual = (actual >> startBitIndex) << startBitIndex;

            if (actual == 0)
               continue;

            // found UInt64 with set bit (maybe one of the excess bits in the complemented last element);
            int bitPos = (i << BitsPerBitIndex) + BitOperations.TrailingZeroCount(actual);
            return bitPos > Length ? Length : bitPos; ;
         }

         return Length;
      }


      // Todo ///
      public IndexSet Not() // BitArray
      {
         int i;
         for (i = 0; i <= ArrayLength - 2; i++)
            SetUInt64(i, ~GetUInt64(i));

         SetUInt64(i, ~GetUInt64(i) & LastElementMask);
         return this;
      }

      // Todo ///
      public IndexSet Or(IndexSet other) // BitArray
      {
         int smallerArrayLength = Minimum(ArrayLength, other.ArrayLength);

         int i;
         for (i = 0; i < smallerArrayLength; i++)
            SetUInt64(i, GetUInt64(i) | other.GetUInt64(i));  // Or: may set excess bits

         ClearExcessBits();

         return this;
      }



      // Todo ///
      public bool Overlaps(IndexSet other)
      {
         int smallerArrayLength = Minimum(ArrayLength, other.ArrayLength);

         for (int i = 0; i < smallerArrayLength; i++)
            if ((GetUInt64(i) & other.GetUInt64(i)) != 0)
               return true;

         return false;
      }



      // Todo ///
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
      /// Find the preceding element and return the index of this element or -1 if not found
      /// </summary>
      /// <param name="lastFound"></param>
      /// <param name="complement"></param>
      /// <returns></returns>
      public int Preceding(int lastFound, bool complement = false)
      {
         int start = lastFound - 1;
         if (start >= Length)
            start = Length - 1;
         if (start < 0)
            return -1;

         int startUInt64Index = start >> BitsPerBitIndex; // // start / BitsPerBitIndex

         for (int i = startUInt64Index; i >= 0; i--)
         {
            UInt64 actual = complement ? ~GetUInt64(i) : GetUInt64(i);

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
      /// Assigns the given value to bit[index]. Does nothing if index is not within the range 0..Length.
      /// </summary>
      /// <param name="index">The index of the bit, which is set.</param>
      /// <param name="value">The value to assign.</param>
      /// <returns>The modified current set</returns>
      public IndexSet Set(int index, bool value) // BitArray
      {
         if (!IsGe0AndLT(index, Length))
         {
            if (value)
               throw new ArgumentException("Value can not be assigned: index out of range of IndexSet and value !=0 ");
            return this;
         }

         int arrayIndex = index >> BitsPerBitIndex;
         UInt64 selectedBit = 1UL << index; // C# masks index in << operation

         if (value)
            SetUInt64(arrayIndex, GetUInt64(arrayIndex) | selectedBit);
         else
            SetUInt64(arrayIndex, GetUInt64(arrayIndex) & ~selectedBit);

         return this;
      }


      // Todo ///
      public IndexSet SetAll(bool value) // BitArray
      {
         if (value)
         {
            FirstBits = ~0UL;
            Array.Fill<UInt64>(AdditionalBits, ~0UL);
            ClearExcessBits();
         }
         else
         {
            FirstBits = 0;
            Array.Clear(AdditionalBits);
         }
         return this;
      }

      // Todo ///
      public bool EqualsSet(IEnumerable<int> otherEnum) // ISet<int>
      {
         if (HasExcessElements(otherEnum))
            return false;

         IndexSet other = CreateLimited(otherEnum);
         return SetEquals(other);
      }

      // Todo ///
      public IndexSet Subtract(IndexSet other) => ExceptWith(other);

      // Todo ///
      public IndexSet SymmetricExceptWith(IndexSet other) => Xor(other);

      // Todo ///
      void ISet<int>.SymmetricExceptWith(IEnumerable<int> otherEnum)
      {
         IndexSet other = IndexSet.Create(Length, otherEnum);
         SymmetricExceptWith(other);
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
      public IndexSet UnionWith(IndexSet other) => Or(other);
      void ISet<int>.UnionWith(IEnumerable<int> otherEnum)
      {
         foreach (int element in otherEnum)
            this[element] = true; // this[] handles element out of bounds
      }

      // Todo ///
      public IndexSet Xor(IndexSet other)  // BitArray
      {
         int smallerArrayLength = Minimum(ArrayLength, other.ArrayLength);

         for (int i = 0; i < smallerArrayLength; i++)
            SetUInt64(i, GetUInt64(i) ^ other.GetUInt64(i));  // Xor: may toggle excess bits in bits[^1]

         ClearExcessBits();

         return this;
      }



      /* IEnumerable<T> iterator methods: IEnumerable.GetEnumerator*/

      /// <summary>
      /// This enumerator iterates through all elements of the complement of the set
      /// == iterates through the indices of the bits which are 0
      /// </summary>
      /// <returns></returns>
      public IEnumerable<int> ComplementedSet()
      {
         for (int i = 0; i < Length; i++)
            if (!Get(i))
               yield return i;
         yield break;
      }

      /* IEnumerable and IEnumerable<T> methods*/

      /* --------------------------------------------------------------- */

      /// <summary> Returns an enumerator that iterates through the elements (indexes) of the <see cref="IndexSet"/> /
      /// iterates through the indexes of the bits which are 1.
      /// </summary>
      /// <returns>An enumerator that can be used to iterate through the set.</returns>
      IEnumerator<int> IEnumerable<int>.GetEnumerator()
      {
         return new GenericIndexSetEnumerator(this);
      }

      /// <summary>
      /// This IEnumerator<int> iterates through all elements of the set
      /// == iterates through the indices of the bits which are 1
      /// </summary>
      private class GenericIndexSetEnumerator : IEnumerator<int>
      {
         private readonly IndexSet indexSet;
         private int currentIndex;

         internal GenericIndexSetEnumerator(IndexSet indexSet)
         {
            this.indexSet = indexSet;
            this.currentIndex = -1;
         }

         public virtual bool MoveNext()
            => (currentIndex = indexSet.Next(currentIndex)) < indexSet.Length;

         int IEnumerator<int>.Current
         {
            get
            {
               if (currentIndex == -1)
                  throw new InvalidOperationException("The Current property is undefined before first call of MoveNext()");
               if (currentIndex >= indexSet.Length)
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
               if (currentIndex >= indexSet.Length)
                  throw new InvalidOperationException("The Current property is undefined because the end of the set has been reached");
               return currentIndex;
            }
         }

         public void Reset() => currentIndex = -1;

         void IDisposable.Dispose() { }
      }


      /* ------------------------------------------------------------------ */
      IEnumerator IEnumerable.GetEnumerator() // BitArray
      {
         return new IndexSetEnumerator(this);
      }

      /// <summary>
      /// This IEnumerator iterates through all elements of the set
      /// == iterates through the indices of the bits which are 1
      /// </summary>
      private class IndexSetEnumerator : IEnumerator
      {
         private readonly IndexSet indexSet;
         private int currentIndex;
         // private int version;

         internal IndexSetEnumerator(IndexSet indexSet)
         {
            this.indexSet = indexSet;
            this.currentIndex = -1;
            // version = indexSet._version;
         }

         public virtual bool MoveNext()
            => (currentIndex = indexSet.Next(currentIndex)) < indexSet.Length;

         public virtual Object Current
         {
            get
            {
               if (currentIndex == -1)
                  throw new InvalidOperationException("The Current property is undefined before first call of MoveNext()");
               if (currentIndex >= indexSet.Length)
                  throw new InvalidOperationException("The Current property is undefined because the end of the set has been reeached");
               return currentIndex;
            }
         }

         public void Reset() => currentIndex = -1;
      }
   }
}