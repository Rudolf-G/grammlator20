using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Numerics;
using System.Text;

namespace IndexSetNamespace
{

   /// <summary>
   /// This class implements sets of numbers within a range 0 .. Length by compact arrays of bits..
   /// Length is set by the constructor and can not be changed.
   /// <para>Sets are created with a static method <see cref="New(int, bool)"/>,
   /// defining the <see cref="Length"/> and the initial values of the universal set to be used. </para> 
   /// <para>Set methods alike <see cref="IntersectWith(IndexSet)"/> are defined (only) for sets a and b based on
   /// universal sets with the same <see cref="Length"/>. Most of these methods are implemented using 64 bit operations.
   /// The names of most of the set methods are the same as in <see cref="SortedSet{T}"/>.
   /// Some methods, e.g. <see cref="IntersectWith(IndexSet)"/>, have alternate names, e.g. <see cref="And(IndexSet)"/>,
   /// which are the same as in <see cref="IndexSet"/> .</para>
   /// </summary>
   public abstract class IndexSet : ICollection<int>, IEquatable<IndexSet>
   {
      /// <summary>
      /// The number of elements of the universal set (the number of bits in internal memory).
      /// The <see cref="IndexSet"/> may contain elements in the range 0..<see cref="Length"/>
      /// </summary>
      public int Length { get; init; }

      /// <summary>
      /// This constructor sets the value of <see cref="Length"/>. It is intended for internal use only. 
      /// Constructors are called by the static method New.
      /// </summary>
      /// <param name="length"></param>
      private protected IndexSet(int length) => Length = length;

      /// <summary>
      /// This static method constructs an IndexSet with the same length as the source and copies the source into the new IndexSet.
      /// </summary>
      /// <param name="source">IndexSet which will be copied into the new IndexSet</param>
      /// <returns>new IndexSet</returns>
      public static IndexSet New(IndexSet source) => New(source.Length).CopyFrom(source);

      /// <summary>
      /// Depending on <paramref name="maxCardinality"/> this static method selects a type derived from <see cref="IndexSet"/>
      /// and returns a new instance of this type. If <paramref name="addAll"/> is true
      /// then all elements in the range 0..<paramref name="maxCardinality"/> (the universal set used for the new set) will be added, else the new set will be empty.
      /// </summary>
      /// <param name="maxCardinality"></param>
      /// <param name="addAll">if true all elements of the universal set 0..<paramref name="maxCardinality"/> will be added </param>
      /// <returns></returns>
      /// <exception cref="ArgumentOutOfRangeException"></exception>
      public static IndexSet New(int maxCardinality, bool addAll = false)
      {
         switch (maxCardinality)
         {
            case < 0:
               throw new ArgumentOutOfRangeException(nameof(maxCardinality), "Argument must be >= 0");

            case 0:
               return IndexSetWithLength0;

            case <= 64:
               return new SmallIndexSet(maxCardinality, addAll);

            default:
               return new LargeIndexSet(maxCardinality, addAll);
         }
      }

      /// <summary>
      /// A static instance of <see cref="IndexSet"/> based on the empty set 0..0 as universal set. This set contains no elements.
      /// No elements can be added.
      /// </summary>
      public static readonly IndexSet IndexSetWithLength0 = new ZeroLengthIndexSet(0);

      // [Conditional("Debug")]
      internal void CheckArgumentLength(int length)
      {
         Debug.Assert(length == Length, $"{nameof(IndexSet)}.{nameof(Length)} == {Length}, the arguments length == {length} is different. This will throw an exception.");
         if (length != Length)
            throw new ArgumentException("The right argument of a binary operation must have the same Length as the left argument");
      }

      // [Conditional("Debug")]
      internal void CheckIndex(int index)
      {
         Debug.Assert(index >= 0 && index < Length,
            $"The argument is {index}. This is not within the range 0..<{nameof(IndexSet)}.{nameof(Length)} == {Length}.This will throw an exception.");
         if (index < 0 || index >= Length)
            throw new ArgumentOutOfRangeException(nameof(index),
               $"The index must be >=0 and < Length {Length} but is {index}");
      }

      /* ****** Abstract Nethods and renamings ****** */

      public abstract IndexSet CopyFrom(IndexSet source);
      public abstract IndexSet Not();
      public IndexSet Complement() => Not();  // complement U - A
      public abstract IndexSet And(IndexSet other);
      public IndexSet IntersectWith(IndexSet other) => And(other);
      public abstract IndexSet Or(IndexSet other);
      public IndexSet UnionWith(IndexSet other) => Or(other);
      public abstract IndexSet Xor(IndexSet other); // anticoincidence
      public IndexSet SymmetricExceptWith(IndexSet other) => Xor(other);
      public abstract IndexSet ExceptWith(IndexSet other); // minuend.Or(subtrahend).Xor(subtrahend);
      public IndexSet Subtract(IndexSet other) => ExceptWith(other);
      public abstract IndexSet SetAll(bool value);

      public abstract IndexSet Set(int index, bool value);
      public void Add(int index) => Set(index, true);
      public void Clear() => SetAll(false);

      /// <summary>
      /// Tests, if the set contains an element and removes this element /
      /// tests if a bit ist set and clears this bit.
      /// </summary>
      /// <param name="index">the element to remove / the bit to set to 0</param>
      /// <returns>false if the set did not contain the element <paramref name="index"/> / the bit has already been zero</returns>
      public bool Remove(int index)
      {
         if (Get(index))
         {
            Set(index, false);
            return true;
         };
         return false;
      }

      /// <summary>
      /// Tests if <paramref name="index"/> is member of the set or adds  <paramref name="index"/> to the set /
      /// Gets or sets the value of the bit at the position <paramref name="index"/> in the <see cref="IndexSet"/>.
      /// </summary>
      /// <param name="index"></param>
      /// <returns>true, if <paramref name="index"/>"/> is a member of the set /
      /// if the bit as position index is set</returns>
      public bool this[int index]
      {
         get { return Get(index); }
         set { Set(index, value); }
      }

      /// <summary>
      /// Appends the <paramref name="ElementNames"/> (or the indexes if no names) of all bits which are true to the 
      /// Stringbuilder <paramref name="sb"/> using the <paramref name="delimiter"/>.
      /// There may be given special texts for the case of all bits or no bits set.
      /// </summary>
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

      /* methods and operators required to implement IEquatable<IndexSet> */
      /// <summary>
      /// Compares two <see cref="IndexSet"/>s.
      /// </summary>
      /// <param name="obj"></param>
      /// <returns>true, if both sets contain the same elements / the same bits</returns>
      public override bool Equals(object? obj) => Equals(obj as IndexSet);

      public abstract override int GetHashCode();

      /// <summary>
      /// Determines whether the specified object instances are equal.
      /// </summary>
      /// <param name="a">The first argument to compare</param>
      /// <param name="b">The second argument to compare</param>
      /// <returns>true if both arguments contain the same elements / bits or if both are null,
      /// otherwise false.</returns>
      public static bool operator ==(IndexSet? a, IndexSet? b)
      {
         if (ReferenceEquals(a, b))
            return true; ;
         if (ReferenceEquals(a, null) || ReferenceEquals(b, null))
            return false;

         return a.Equals(b);
      }

      /// <summary>
      /// Determines whether the specified object instances are different.
      /// </summary>
      /// <param name="a">The first argument to compare</param>
      /// <param name="b">The second argument to compare</param>
      /// <returns>true if the arguments do not contain the same elements / if at least one bit is different and if both arguments are not null,
      /// otherwise false</returns>
      public static bool operator !=(IndexSet a, IndexSet b)
      {
         return !(a==b);
      }

      public abstract bool Equals(IndexSet? other);

      public abstract bool Get(int index);
      public abstract bool IsEmpty { get; }
      public abstract bool IsComplete { get; }
      public bool All() => IsComplete; // ToDo replace All with IsComplete

      public abstract bool IsSubsetOf(IndexSet other);
      public bool IsProperSubsetOf(IndexSet other) => IsSubsetOf(other) & !Equals(other);
      public bool IsProperSupersetOf(IndexSet other) => other.IsProperSubsetOf(this);
      public bool IsSupersetOf(IndexSet other) => other.IsSubsetOf(this);
      public abstract bool Overlaps(IndexSet other);

      /* Properties */
      /// <summary>
      /// Returns the smallest value in the set. Returns Length if the set is empty.
      /// </summary>
      public abstract int Min { get; }
      public int First() => Min; // Todo Replace First() and Last with Min and Max
      public int Last() => Max; // Todo Replace First() and Last with Min and Max

      /// <summary>
      /// Returns the largest value in the set. Returns -1 if the set is empty.
      /// </summary>
      public abstract int Max { get; }

      public virtual (int min, int max) MinAndMax() => (Min, Max);

      /// <summary>
      /// Find the next element and return the index of this element or this.count if not found
      /// </summary>
      /// <param name="lastFound">starts at lastFound+1, starts at 0, if lastFound is -1</param>
      /// <param name="complement">search in complemented set</param>
      /// <returns>Index of found element or this.Length if not found</returns>
      public abstract int Next(int lastFound, bool complement = false);

      /// <summary>
      /// Find the preceding element and return the index of this element or -1 if not found
      /// </summary>
      /// <param name="lastFound"></param>
      /// <param name="complement"></param>
      /// <returns></returns>
      public abstract int Preceding(int lastFound, bool complement = false);

      /* ICollection<T> properties and methods */
      /// <summary>
      /// Returns the number of elements in the set (greater or equal 0 and less than Length) - an O(n) operation.
      /// </summary>
      public abstract int Count { get; }

      /// <summary>
      /// Is always false.
      /// </summary>
      public bool IsReadOnly { get { return false; } }

      /// <summary>
      /// Is true if the set contains <paramref name="index"/> /
      /// if the bit at position <paramref name="index"/> is 1 resp. true
      /// </summary>
      /// <param name="index"></param>
      /// <returns></returns>
      public bool Contains(int index) => Get(index);

      /// <summary>
      /// Copies all <see cref="Count"/> elements of the set (the indices of the bits with value 1) to the <paramref name="destination"/> array.
      /// <para>To copy one <see cref="IndexSet"/> from another use <see cref="CopyFrom"/></para>
      /// </summary>
      /// <param name="destination">The destinatione int Array</param>
      /// <param name="startIndex">the 1st element will be copied to <paramref name="destination"/>[<paramref name="startIndex"/>]</param>
      /// <exception cref="ArgumentNullException"></exception>
      /// <exception cref="ArgumentOutOfRangeException">the exception will be thrown if <paramref name="destination"/> &lt;0 or &gt; <see cref="Length"/> </exception>
      public void CopyTo(int[] destination, int startIndex)
      {
         if (destination is null)
            throw new ArgumentNullException(nameof(destination),
               $"The {nameof(destination)} argument of {nameof(CopyTo)} must not be null");

         if (startIndex < 0 || startIndex > destination.Length - this.Count)
            throw new ArgumentOutOfRangeException(
               nameof(startIndex),
               $"The {nameof(startIndex)} argument of {nameof(CopyTo)} is {startIndex}but must be >= 0 and <= {nameof(destination)}.Length - cardinality of the set, which is {this.Count}"); ;

         int destinationIndex = startIndex;
         foreach (int element in this)
            destination[destinationIndex++] = element;
      }

      /* IEnumerable<T> methods*/

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

      /// <summary>
      /// The IEnumerator<int> iterates through all elements of the set
      /// == iterates through the indices of the bits which are 1
      /// </summary>
      /// <returns></returns>
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
                  throw new InvalidOperationException("The Current property is undefined because the end of the set has been reeached");
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
                  throw new InvalidOperationException("The Current property is undefined because the end of the set has been reeached");
               return currentIndex;
            }
         }

         public void Reset() => currentIndex = -1;

         void IDisposable.Dispose() { }
      }


      /* ------------------------------------------------------------------ */
      IEnumerator IEnumerable.GetEnumerator()
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

      /* ------------------------------------------------------------------ */


   }

   /* ****** ZeroLengthIndexSet ****** */
   /// <summary>
   /// An <see cref="IndexSet"/> with no elements.
   /// </summary>
   internal sealed class ZeroLengthIndexSet : IndexSet
   {
      /// <summary>
      /// new <see cref="IndexSet"/> with no elements. 
      /// </summary>
      /// <param name="length"></param>
      internal ZeroLengthIndexSet(int length) : base(length) { }

      public override IndexSet CopyFrom(IndexSet source) // NoBits
      {
         CheckArgumentLength(source.Length);
         return this;
      }

      public override IndexSet Not() // NoBits
      {
         return this;
      }

      public override IndexSet And(IndexSet other) // NoBits
      {
         CheckArgumentLength(other.Length);
         return this;
      }

      public override IndexSet Or(IndexSet other) // NoBits
      {
         CheckArgumentLength(other.Length);
         return this;
      }

      public override IndexSet Xor(IndexSet other) // NoBits
      {
         CheckArgumentLength(other.Length);
         return this;
      }

      public override IndexSet ExceptWith(IndexSet other) // NoBits
      {
         CheckArgumentLength(other.Length);
         return this;
      }

      public override IndexSet SetAll(bool value) // NoBits
      {
         return this;
      }

      public override IndexSet Set(int index, bool value) // NoBits
      {
         CheckIndex(index); // always throws an exception, because there is no element, which can be indexed
         return this;
      }

      public override int GetHashCode() // NoBits
      {
         var hash = new HashCode();
         hash.Add(0);
         return hash.ToHashCode();
      }

      public override bool Equals(IndexSet? other) // NoBits
      {
         if ((other is null) || (other.Length != Length)) return false;

         return true;
      }

      public override bool IsSubsetOf(IndexSet other) // NoBits
      {
         CheckArgumentLength(other.Length);
         return true;
      }

      public override bool Overlaps(IndexSet other) // NoBits
      {
         CheckArgumentLength(other.Length);
         return false;
      }

      public override bool Get(int index) // NoBits
      {
         CheckIndex(index); // always throws an exception, because there is no element, which can be indexed
         return false;
      }

      public override bool IsEmpty => true; // NoBits

      public override bool IsComplete => true; // NoBits

      public override int Min => 0; // NoBits

      public override int Max => -1; // NoBits

      public override int Next(Int32 lastFound, bool complement = false) => 0; // NoBits

      public override int Preceding(Int32 lastFound, bool complement = false) => -1; // NoBits

      public override int Count => 0; // NoBits

   }

   /* ****** SmallIndexSet with up to 64 bits ****** */
   internal sealed class SmallIndexSet : IndexSet
   {
      internal UInt64 bits;

      const int BitsPerElement = 64;
      const int BitsPerBitIndex = 6;
      const UInt64 BitIndexMask = (~0UL >> (BitsPerElement - BitsPerBitIndex));

      private UInt64 maskLengthBits => ~0UL >> (BitsPerElement - Length);

      internal SmallIndexSet(int length, bool addAll) : base(length)
      {
         Debug.Assert(length > 0 && length <= 64);
         bits = addAll ? ~0UL : 0UL;
      }

      public override IndexSet CopyFrom(IndexSet source) // UpTo64Bits
      {
         CheckArgumentLength(source.Length);
         bits = (source as SmallIndexSet)!.bits; // Copy UInt64
         return this;
      }

      public override IndexSet Not() // UpTo64Bits
      {
         bits = ~bits;
         return this;
      }

      public override IndexSet And(IndexSet right) // UpTo64Bits
      {
         CheckArgumentLength(right.Length);
         bits &= (right as SmallIndexSet)!.bits;
         return this;
      }

      public override IndexSet Or(IndexSet right) // UpTo64Bits
      {
         CheckArgumentLength(right.Length);
         bits |= (right as SmallIndexSet)!.bits;
         return this;
      }

      public override IndexSet Xor(IndexSet right) // UpTo64Bits
      {
         CheckArgumentLength(right.Length);
         bits ^= (right as SmallIndexSet)!.bits;
         return this;
      }

      public override IndexSet ExceptWith(IndexSet subtrahend) // UpTo64Bits
      {
         CheckArgumentLength(subtrahend.Length);
         UInt64 s = (subtrahend as SmallIndexSet)!.bits;
         bits = (bits | s) ^ s;// minuend.Or(subtrahend).Xor(subtrahend)
         return this;
      }

      public override IndexSet SetAll(bool value) // UpTo64Bits
      {
         bits = value ? ~0UL : 0UL;
         return this;
      }

      public override IndexSet Set(int index, bool value) // UpTo64Bits
      {
         CheckIndex(index);
         UInt64 selectedBit = (UInt64)1 << index;

         if (value)
            bits |= 1UL << index; // index is masked by 125
         else
            bits &= ~(1UL << index);// index is masked by 125

         return this;
      }

      public override int GetHashCode() // UpTo64Bits
      {
         var hash = new HashCode();
         hash.Add(bits & maskLengthBits);
         return hash.ToHashCode();
      }

      public override bool Equals(IndexSet? other) // UpTo64Bits
      {
         if (other is null || other.Length != Length) return false;

         return ((bits ^ (other as SmallIndexSet)!.bits) & maskLengthBits) == 0;
      }

      public override bool IsSubsetOf(IndexSet other) // UpTo64Bits
      {
         CheckArgumentLength(other.Length);
         UInt64 otherBits = (other as SmallIndexSet)!.bits;
         return (((bits | otherBits) ^ otherBits) & maskLengthBits) == 0UL; // this.ExceptWith(other).IsEmpty
      }

      public override bool Overlaps(IndexSet other) // UpTo64Bits
      {
         CheckArgumentLength(other.Length);
         UInt64 otherBits = (other as SmallIndexSet)!.bits;
         return ((bits & otherBits) & maskLengthBits) != 0UL; // !(this.And(other).IsEmpty)
      }

      public override bool Get(int index) // UpTo64Bits
      {
         CheckIndex(index);
         return (bits & (1UL << index)) != 0;
      }

      public override bool IsEmpty // UpTo64Bits
         => (bits & maskLengthBits) == 0;

      public override bool IsComplete // UpTo64Bits
      {
         get
         {
            UInt64 mask = (~0UL) >> (BitsPerElement - Length);
            return (bits & mask) == mask;
         }
      }

      public override int Min  // UpTo64Bits
         => BitOperations.TrailingZeroCount(bits | ~maskLengthBits); // Next(-1, false); 

      public override int Max // UpTo64Bits
         => BitsPerElement - 1 - BitOperations.LeadingZeroCount(bits & maskLengthBits);

      public override Int32 Next(Int32 lastFound, bool complement = false) // UpTo64Bits
      {
         if (lastFound >= Length - 1)
            return Length;

         int start = lastFound < 0 ? 0 : lastFound + 1; // 0 <= start < Length
         int result = BitOperations.TrailingZeroCount(((complement ? ~bits : bits) >> start) << start);
         return result <= Length ? result : Length; // ignore unused bits
      }

      public override int Preceding(int lastFound, bool complement = false) // UpTo64Bits
      {
         if (lastFound <= 0)
            return -1;
         int start = lastFound >= Length ? Length - 1 : lastFound - 1; // 0 <= start <= Length-1
         UInt64 actual = complement ? ~bits : bits;
         int leadingBits = BitsPerElement - 1 - start;
         actual = (actual << leadingBits) >> leadingBits;
         return BitsPerElement - 1 - BitOperations.LeadingZeroCount(actual);
      }

      public override int Count // UpTo64Bits
      {
         get =>
            System.Numerics.BitOperations.PopCount(bits & maskLengthBits);
      }

   }

   /* ****** LargeIndexSet with more than 64 Bits ****** */
   internal sealed class LargeIndexSet : IndexSet
   {
      private Memory<UInt64> bits;
      const int BitsPerElement = 64;
      const int BitsPerBitIndex = 6;
      const int BitIndexMask = (~0 >> (BitsPerElement - BitsPerBitIndex));
      private UInt64 lastElementMask => (~0UL) >> ((bits.Span.Length << BitsPerBitIndex) - Length);

      internal LargeIndexSet(int length, bool addAll) : base(length)
      {
         Debug.Assert(length > 0);

         int SpanLength = (Length - 1) / BitsPerElement + 1;
         bits = new UInt64[SpanLength]; // initial value of each element is 0
         if (addAll)
            bits.Span.Fill(~0UL);
      }

      public override IndexSet CopyFrom(IndexSet source)
      {
         CheckArgumentLength(source.Length);
         (source as LargeIndexSet)!.bits.Span.CopyTo(bits.Span);
         return this;
      }

      public override IndexSet Not()
      {
         for (int memoryIndex = 0; memoryIndex < bits.Length; memoryIndex++)
            bits.Span[memoryIndex] = ~bits.Span[memoryIndex];
         return this;
      }

      public override IndexSet And(IndexSet right)
      {
         CheckArgumentLength(right.Length);
         Span<UInt64> rightSpan = (right as LargeIndexSet)!.bits.Span;

         for (int i = 0; i < bits.Span.Length; i++)
            bits.Span[i] &= rightSpan[i];

         return this;
      }

      public override IndexSet Or(IndexSet right)
      {
         CheckArgumentLength(right.Length);
         Span<UInt64> rightSpan = (right as LargeIndexSet)!.bits.Span;

         for (int memoryIndex = 0; memoryIndex < bits.Span.Length; memoryIndex++)
            bits.Span[memoryIndex] |= rightSpan[memoryIndex];

         return this;
      }

      public override IndexSet Xor(IndexSet right)
      {
         CheckArgumentLength(right.Length);
         Span<UInt64> rightSpan = (right as LargeIndexSet)!.bits.Span;

         for (int memoryIndex = 0; memoryIndex < bits.Span.Length; memoryIndex++)
            bits.Span[memoryIndex] ^= rightSpan[memoryIndex];

         return this;
      }

      public override IndexSet ExceptWith(IndexSet subtrahend) // minuend.Or(subtrahend).Xor(subtrahend);
      {
         CheckArgumentLength(subtrahend.Length);
         Span<UInt64> subtrahendSpan = (subtrahend as LargeIndexSet)!.bits.Span;

         for (int memoryIndex = 0; memoryIndex < bits.Span.Length; memoryIndex++)
         {
            UInt64 s = subtrahendSpan[memoryIndex];
            bits.Span[memoryIndex] = (bits.Span[memoryIndex] | s) ^ s;
         }

         return this;
      }

      public override IndexSet SetAll(bool value)
      {
         UInt64 elementValue = value ? ~0UL : 0UL;
         for (int memoryIndex = 0; memoryIndex < bits.Span.Length; memoryIndex++)
            bits.Span[memoryIndex] = elementValue;

         return this;
      }

      public override IndexSet Set(int index, bool value)
      {
         CheckIndex(index);

         if (value)
            bits.Span[index >> BitsPerBitIndex] |= 1UL << index; // C# masks index in << operation
         else
            bits.Span[index >> BitsPerBitIndex] &= ~(1UL << index); // C# masks index << operation

         return this;
      }

      public override int GetHashCode()
      {
         var hash = new HashCode();
         for (int memoryIndex = 0; memoryIndex < bits.Span.Length - 1; memoryIndex++)
            hash.Add(bits.Span[memoryIndex]);

         hash.Add((bits.Span[bits.Span.Length - 1]) & lastElementMask);

         return hash.ToHashCode();
      }

      public override bool Equals(IndexSet? other)
      {
         if (other is null || other.Length != Length) return false;
         Span<UInt64> otherSpan = (other as LargeIndexSet)!.bits.Span;

         for (int memoryIndex = 0; memoryIndex < bits.Span.Length - 1; memoryIndex++)
            if ((bits.Span[memoryIndex] ^ otherSpan[memoryIndex]) != 0)
               return false;

         return
            ((bits.Span[bits.Span.Length - 1] ^ otherSpan[bits.Span.Length - 1]) & lastElementMask) == 0;
      }

      public override bool IsSubsetOf(IndexSet other)
      {
         CheckArgumentLength(other.Length);
         Span<UInt64> subtrahendSpan = (other as LargeIndexSet)!.bits.Span;

         UInt64 s;

         for (int memoryIndex = 0; memoryIndex < bits.Span.Length - 1; memoryIndex++)
         {
            s = subtrahendSpan[memoryIndex];
            if (((bits.Span[memoryIndex] | s) ^ s) != 0)
               return false; // !this.ExceptWith(other).IsEmpty
         }

         s = subtrahendSpan[subtrahendSpan.Length - 1];

         return (((bits.Span[bits.Span.Length - 1] | s) ^ s) & lastElementMask) == 0UL; // this.ExceptWith(other).IsEmpty
      }

      public override bool Overlaps(IndexSet other)
      {
         CheckArgumentLength(other.Length);
         Span<UInt64> otherSpan = (other as LargeIndexSet)!.bits.Span;

         for (int memoryIndex = 0; memoryIndex < bits.Span.Length - 1; memoryIndex++)
         {
            if ((bits.Span[memoryIndex] & otherSpan[memoryIndex]) != 0UL)
               return true; // !(this.And(other).IsEmpty)
         }

         // return !(this.And(other).IsEmpty) :
         return (bits.Span[bits.Span.Length - 1] & otherSpan[otherSpan.Length - 1] & lastElementMask) != 0UL;
      }

      public override bool Get(int index)
      {
         CheckIndex(index);
         return (bits.Span[index >> BitsPerBitIndex] & (1UL << index)) != 0;
      }

      public override bool IsEmpty
      {
         get
         {
            return Min == Length;
         }
      }

      public override bool IsComplete
      {
         get
         {
            // test all except last element
            for (int spanIndex = 0; spanIndex < bits.Span.Length - 1; spanIndex++)
               if (bits.Span[spanIndex] != ~0UL)
                  return false;
            UInt64 mask = lastElementMask;
            return
               (bits.Span[bits.Span.Length - 1] & mask) == mask;
         }
      }

      public override int Min
      {
         get
         {
            for (int spanIndex = 0; spanIndex < bits.Span.Length; spanIndex++)
            {
               UInt64 actual = bits.Span[spanIndex];
               if (actual == 0)
                  continue;

               // found UInt64 with set bit (maybe one of the unused bits in the last element of the span);
               int bitPos = (spanIndex << BitsPerBitIndex) + BitOperations.TrailingZeroCount(actual);
               return bitPos <= Length ? bitPos : Length;
            }

            return Length;
         }
      }

      public override int Next(Int32 lastFound, bool complement = false)
      {
         if (lastFound >= Length - 1)
            return Length;

         int start = lastFound < 0 ? 0 : lastFound + 1; // 0<= start <= Length - 1
         int startBitIndex = start & BitIndexMask, // 0 <= startBitIndex <= 63
            startSpanIndex = start >> BitsPerBitIndex; // 0 <= startSpanIndex < Span.Length         

         for (int spanIndex = startSpanIndex; spanIndex < bits.Span.Length; spanIndex++)
         {
            UInt64 actual = complement ? ~bits.Span[spanIndex] : bits.Span[spanIndex];

            if (actual == 0)
               continue; // redundant shortcut

            // ignore (clear) the bits preceding startBitIndex in the start element 
            if (spanIndex == startSpanIndex)
               actual = (actual >> startBitIndex) << startBitIndex;

            if (actual == 0)
               continue;

            // found UInt64 with set bit (maybe one of the unused bits in the last element of the span);
            int bitPos = (spanIndex << BitsPerBitIndex) + BitOperations.TrailingZeroCount(actual);
            return bitPos > Length ? Length : bitPos; ;
         }

         return Length;
      }

      public override int Max
      {
         get
         {
            for (int spanIndex = bits.Span.Length - 1; spanIndex >= 0; spanIndex--)
            {
               UInt64 actual = bits.Span[spanIndex];
               if (actual == 0)
                  continue; // redundant shortcut O(n)

               // O(1)
               // ignore (clear) unused bits of last element
               if (spanIndex == bits.Span.Length - 1)
               {
                  int unusedBitCount = BitsPerElement - 1 - (Length - 1) & BitIndexMask;
                  actual = (actual << unusedBitCount) >> unusedBitCount;
               }

               if (actual == 0)
                  continue;

               // found UInt64 with set bit, result >= 1
               return ((spanIndex + 1) << BitsPerBitIndex) - 1 - BitOperations.LeadingZeroCount(actual);
            }

            return -1;
         }
      }

      public override int Preceding(Int32 lastFound, bool complement = false)
      {
         if (lastFound <= 0)
            return -1;
         int start = lastFound >= Length ? Length - 1 : lastFound - 1; // 0 <= start <= Length-1
         int startSpanIndex = start >> BitsPerBitIndex; // // start / BitsPerBitIndex

         for (int spanIndex = startSpanIndex; spanIndex >= 0; spanIndex--)
         {
            UInt64 actual = complement ? ~bits.Span[spanIndex] : bits.Span[spanIndex];

            if (actual == 0)
               continue; // redundant shortcut

            // ignore (clear) the bits following startBitIndex in the start element 
            // if spanIndex == bits.Span.Length-1 this also clears the unused bits
            if (spanIndex == startSpanIndex)
            {
               int leadingBits = BitsPerElement - 1 - (start & BitIndexMask); // start % BitsPerElement
               actual = (actual << leadingBits) >> leadingBits;
            }

            if (actual == 0)
               continue;

            // found UInt64 with set bit, result >= 0
            return ((spanIndex + 1) << BitsPerBitIndex) - 1 - BitOperations.LeadingZeroCount(actual);
         }

         return -1;
      }

      public override int Count
      {
         get
         {
            int count = 0;
            for (int memoryIndex = 0; memoryIndex < bits.Span.Length - 1; memoryIndex++)
               count += System.Numerics.BitOperations.PopCount(bits.Span[memoryIndex]);

            return count +
               System.Numerics.BitOperations.PopCount(bits.Span[bits.Span.Length - 1] & lastElementMask);
         }
      }

   }

}