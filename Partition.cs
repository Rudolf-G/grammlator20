using System;
using System.Diagnostics;
using System.Collections.Generic;

namespace Grammlator {
   /*  A partition of a set M is a set P. The elements of P are (nonempty) subsets (classes) of M,
    *  where each element of M is contained exactly in one class.
    *   
    *  Classes of a partition can be combined to construct a new partition.
    */

   /// <summary>
   /// <see cref="PartitionInfoArrayInt"/> implements a partition of integer indexes from 0 to MaxIndex 
   /// by an array of elementdescriptors (index of next element and index of representative of same class)
   /// </summary>
   public class PartitionInfoArrayInt {
      /// <summary>
      /// This constructor initializes a partion with one class for each element
      /// </summary>
      /// <param name="maxIndex">A tuple containing the maximal index</param>
      protected PartitionInfoArrayInt(Int32 maxIndex)
         {
         Info = new ElementDescriptor[maxIndex];

         for (Int32 i = 0; i < Info.Length; i++)
            {
            Info[i].NextElementID = i;    // one element partition class i, element i pointing to itself
            Info[i].RepresentativeID = i; // and element i being its own representative
            }
         }

      /// <summary>
      /// This private standard constructor should never be used explicitly
      /// </summary>
      private PartitionInfoArrayInt() { }

      /// <summary>
      /// The <see cref="ElementDescriptor"/> consists of the index of the next element in the same class
      /// and the index of an element which represents the class.
      /// </summary>
      [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1815:OverrideEqualsAndOperatorEqualsOnValueTypes")]
      protected struct ElementDescriptor {
         internal Int32 NextElementID;    // index of next element 
         internal Int32 RepresentativeID; // index of representative element of the class
         }

      private readonly ElementDescriptor[] Info;

      /// <summary>
      /// Returns the length of the array Info
      /// </summary>
      public Int32 InfoLength => Info.Length;

      /// <summary>
      /// scans all elements of the partition class to find the preceding element
      /// </summary>
      /// <param name="indexOfElement"></param>
      /// <returns>returns the index of the preceding element in the same class (may be the same element)</returns>
      public Int32 PrecedingElement(Int32 indexOfElement)
         {
         Int32 i = indexOfElement;

         while (Info[i].NextElementID != indexOfElement)
            i = Info[i].NextElementID;

         Debug.Assert(Info[i].NextElementID == indexOfElement);
         return i;
         }

      /// <summary>
      /// Returns the index of the next element
      /// </summary>
      /// <param name="indexOfElement"></param>
      /// <returns>returns the ID of the preceding element (may be the same element)</returns>
      public Int32 NextElementsId(Int32 indexOfElement)
          => Info[indexOfElement].NextElementID;

      /// <summary>
      /// Checks if the element is the representative of its partition class
      /// </summary>
      /// <param name="memberId"></param>
      /// <returns>returns true if the element is the representative of its own class</returns>
      public Boolean IsClassRepresentative(Int32 memberId)
          => Info[memberId].RepresentativeID == memberId;

      /// <summary>
      /// Combines the pertition classes of the two members, no effect, if the members are in the same class
      /// </summary>
      /// <param name="memberId1"></param>
      /// <param name="memberId2"></param>
      public void CombineClasses(Int32 memberId1, Int32 memberId2)
         {
         Int32 representative1 = Info[memberId1].RepresentativeID;
         Int32 representative2 = Info[memberId2].RepresentativeID;
         if (representative1 == representative2)
            return; // MemberID1 and MemberID2 already belong to the same class

         // The class with the larger RepresentativeID ist combined into the other class
         // ClassindexA denotes the class, the other class (ClassIndex2) schould be combined to

         Int32 representativeA = representative1, representativeB = representative2;
         if (representativeA > representativeB)
            {
            representativeA = representative2;
            representativeB = representative1;
            }

         // Set representative of all elements of class B to representative of class A
         Int32 iB = representativeB;
         do
            {
            Info[iB].RepresentativeID = representativeA;
            iB = Info[iB].NextElementID;

            } while (iB != representativeB);

         // Combine the classes: 
         // Cut the cycles between ClassIndexA and NextA and between ClassIndexB and NextB 
         Int32 NextA = Info[representativeA].NextElementID;
         // Link the chains NextA...ClassIndexA and NextB...ClassIndexB together
         Info[representativeA].NextElementID = Info[representativeB].NextElementID;
         // and close to a  cycle
         Info[representativeB].NextElementID = NextA;
         }
      }

   /// <summary>
   /// Jede C#-Class vom Typ T, die Elemente dieser Partitionsverfahren implementiert, muss dieses Interface implementieren
   /// </summary>
   public interface IELementOfPartition {
      /// <summary>
      /// A number unique to each element of the partition
      /// </summary>
      Int32 IdNumber { get; }
      }

   /// <summary>
   /// <see cref="PartitionInfoArray{T}"/> implements a partition. 
   /// Used in grammlator to implement the neighbors partition of parserstates.
   /// </summary>
   /// <typeparam name="T">The class type of the elements of classes of the partition.
   /// In grammlator this ist the type ParserState.
   /// This class must implement the interface <see cref="IELementOfPartition"/>
   /// </typeparam>
   public class PartitionInfoArray<T>:
      PartitionInfoArrayInt where T : class, IELementOfPartition {
      private static Int32 MaxOfIdNumber(IEnumerable<T> elementlist)
         {
         Int32 count = 0, max = Int32.MinValue, min = Int32.MaxValue;
         if (elementlist == null)
            throw new ArgumentNullException(nameof(elementlist));

         foreach (T Element in elementlist)
            {
            count++;
            if (Element.IdNumber > max)
               max = Element.IdNumber;
            if (Element.IdNumber < min)
               min = Element.IdNumber;
            }

         if (min < 0)
            throw new ErrorInGrammlatorProgramException("Argument Error: ID-Numbers of states must not be negative ");

         return max;
         }

      ///<summary>
      /// This constructor creates an internal IList used to access the elements of elementlist by index (0 &lt;= index &lt;= max(index))
      ///</summary>
      ///<param name="elementlist"></param>
      public PartitionInfoArray(IEnumerable<T> elementlist) : base(MaxOfIdNumber(elementlist))
         {
         Elementlist = new T[InfoLength]; // mapping of elements number to element

         foreach (T Element in elementlist)
            {
            if (Elementlist[Element.IdNumber] != null)
               throw new ErrorInGrammlatorProgramException(
                  $"Constructor {nameof(PartitionInfoArray<T>)}: Argument Error, ID-Numbers are not unique "
                  );
            Elementlist[Element.IdNumber] = Element;
            }
         }

      /// <summary>
      /// This constructor takes an elementlist to be used for access to the elements by index
      /// </summary>
      /// <param name="elementlist"></param>
      public PartitionInfoArray(IList<T> elementlist) : base(elementlist?.Count ?? 1)
         {
         Elementlist = elementlist ?? throw new ArgumentNullException(nameof(elementlist));

#if DEBUG

         for (Int32 i = 0; i < elementlist.Count; i++)
            {
            if (elementlist[i].IdNumber != i)
               throw new ArgumentException($"{nameof(PartitionInfoArray<T>)}.elementlist[{i}].IDNumber != {i}");
            }
         foreach (T Element in elementlist)
            {
            if (Elementlist[Element.IdNumber] != Element)
               throw new ErrorInGrammlatorProgramException("Argument Error: ID-Numbers are not unique ");
            }
#endif

         }

      private readonly IList<T> Elementlist; // used to map element indizes i to elements e by e=ElementList[i]

      /// <summary>
      /// Returns the Count of the Elementlist, which maps element indexes to elements
      /// </summary>
      public Int32 Count => Elementlist.Count;

      /// <summary>
      /// Returns the ID of the next element 
      /// </summary>
      /// <param name="element"></param>
      /// <returns>returns the Id of the next element in the same class (may be the element)</returns>
      public Int32 NextElementsId(T element)
          => NextElementsId(
              (element ?? throw new ArgumentNullException(nameof(element)))
              .IdNumber
              );

      /// <summary>
      /// Returns the next element in the same partition class
      /// </summary>
      /// <param name="element"></param>
      /// <returns>returns the the next element in the same class (may be the element)</returns>
      public T NextElement(T element) => Elementlist[NextElementsId(element)];

      /// <summary>
      /// Combines the partition classes of two elements; no effect, if the elements are already in the same class
      /// </summary>
      /// <param name="memberOfClass1"></param>
      /// <param name="memberOfClass2"></param>
      public void CombineClasses(T memberOfClass1, T memberOfClass2)
          => CombineClasses(
              (memberOfClass1 ?? throw new ArgumentNullException(nameof(memberOfClass1))).IdNumber,
              (memberOfClass2 ?? throw new ArgumentNullException(nameof(memberOfClass2))).IdNumber
              );

      /// <summary>
      /// Tests if an element is the representative of its partition class
      /// </summary>
      /// <param name="memberOfClass"></param>
      /// <returns>returns true if the element is the representative of the class it belongs to</returns>
      public Boolean IsClassRepresentative(T memberOfClass)
          => IsClassRepresentative(
              (memberOfClass ?? throw new ArgumentNullException(nameof(memberOfClass))).IdNumber
              );

      public IEnumerable<T> ClassRepresentatives {
         get {
            foreach (T classRepresentative in Elementlist)
               {
               if (IsClassRepresentative(classRepresentative))
                  yield return classRepresentative;
               }
            }
         }

      public IEnumerable<T> ElementsOfClass(T element)
         {
         T result = element;
         do
            {
            yield return result;
            result = NextElement(result);
            }
         while (result != element);
         }

      }
   }
