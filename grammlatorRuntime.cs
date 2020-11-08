﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.Contracts;
using System.Runtime.InteropServices; // to overlay fields of the elements of the attribute array
using System.Text;

using grammlator;

namespace GrammlatorRuntime {
   /// <summary>
   /// The code generated by grammlator gets its input by the methods specified by this interface
   /// <see cref="IGrammlatorInput{TTypeOfOutputSymbols}"/>.
   /// This interface may be used to check that a method defines all methods that
   /// grammlator generated applications need for symbol input.
   /// You will find more information on the interface methods in
   /// <see cref="GrammlatorInput{TTypeOfOutputSymbols}"/> given in
   /// <see cref="GrammlatorRuntime"/>,
   /// which may be used as base class instead of using the interfaces.
   /// </summary>
   /// <typeparam name="TTypeOfOutputSymbols">Type of the attributes of the "Symbol"
   /// the instance makes available</typeparam>
   public interface IGrammlatorInput<TTypeOfOutputSymbols> where TTypeOfOutputSymbols : IComparable /* enum */
       {
      /// <summary>
      /// Do nothing, if accepted==true.
      /// Else set accepted=true and copy the AttributesOfSymbol to the attribute stack. 
      /// </summary>
      void AcceptSymbol();

      /// <summary>
      /// if accepted==true compute the next Symbol, push its attributes to AttributesOfSymbol
      /// and set accepted to false, else do nothing
      /// </summary>
      TTypeOfOutputSymbols PeekSymbol();
   }

   /* Base classes
    *   GrammlatorApplication
    *   GrammlatorInput
    *   GrammlatorInputApplication
    */

   /// <summary>
   /// Abstract base class for classes that provide input for grammlator generated code
   /// </summary>
   /// <typeparam name="TTypeOfOutputSymbols">the type of Symbol</typeparam>
   public abstract class GrammlatorInput<TTypeOfOutputSymbols>
       : IGrammlatorInput<TTypeOfOutputSymbols> where TTypeOfOutputSymbols : notnull, IComparable  /* enum */
       {
      ///<summary>
      /// The attributeStack <see cref="_a"/> of <see cref="GrammlatorRuntime"/> 
      /// is used  by grammlator generated code 
      /// a) to return the attributes of output symbols (if any)
      /// b) to get the attributes of input symbols. 
      /// Caution: Access to the elements of the attribute stack is not type save.
      ///</summary>
#pragma warning disable IDE1006 // Benennungsstile
      public StackOfMultiTypeElements _a {
         get; protected set;
      }
#pragma warning restore IDE1006 // Benennungsstile

      /// <summary>
      /// Constructor of cGrammlatorInputApplication
      /// </summary>
      /// <param name="attributeStack">grammlator uses the attributeStack a) in grammlator generated code 
      /// b) to return the attributes of output symbol (if any) and c) to get the attribute of input symbols
      /// </param>
      protected GrammlatorInput(StackOfMultiTypeElements attributeStack)
      {
         _a = attributeStack;
         Accepted = true;
         Symbol = default!;
      }

      /// <summary>
      /// After the first call of <see cref="PeekSymbol"/> <see cref="Symbol"/> will have a defined value.
      /// The value can only be changed by <see cref="PeekSymbol"/>.
      /// </summary>
      public TTypeOfOutputSymbols Symbol {
         get; protected set;
      }

      /// <summary>
      /// <para>When <see cref="Accepted"/> is false, then calls to <see cref="PeekSymbol"/> return the last symbol
      /// and do nothing else and </para>
      /// <para>calls to <see cref="AcceptSymbol"/> push all the attributes of Symbol from a local stack to the attribute stack and set accepted to true.</para>
      /// <para>Else calls to <see cref="AcceptSymbol"/> move the input one element ahead and
      /// do nothing else and calls to <see cref="PeekSymbol"/> retrieve the next symbol and set accepted to false. </para>
      /// </summary>
      public Boolean Accepted {
         get; // may be evaluated in semantic methods before accessing context
         protected set;
      }

      /// <summary>
      /// Returns a local stack with the attributes of Symbol (if any). It is empty if accepted == true.
      /// </summary>
      protected StackOfMultiTypeElements _AttributesOfSymbol {
         get;
         // private set{ _AttributesOfSymbol = value; }
      } = new StackOfMultiTypeElements(10);

      /// <summary>
      /// Sets <see cref="Accepted"/>=true, copies the <see cref="_AttributesOfSymbol"/> to the attribute stack
      /// and then clears <see cref="_AttributesOfSymbol"/>. 
      /// <para>It may happen that  AcceptSymbol() is called without any PeekSymbol() after the last call
      /// of AcceptSymbol(). Because this second AcceptSymbol() must accept the next symbol it calls
      /// the missing PeekSymbol() first.</para>
      /// </summary>
      public virtual void AcceptSymbol()
      {
         if (Accepted) // skip one symbol
            PeekSymbol(); // move to the next the symbol and set accepted to false
         Accepted = true;
         _a.CopyAndRemoveFrom(_AttributesOfSymbol);
      }

      /// <summary>
      /// if <see cref="Accepted"/>==true computes the next Symbol,
      /// pushs its attributes to <see cref="_AttributesOfSymbol"/>
      /// and sets <see cref="Accepted"/> to false,
      /// else does nothing
      /// </summary>
      public abstract TTypeOfOutputSymbols PeekSymbol();
   }

   /// <summary>
   /// Abstract base class for classes that use grammlator generated code
   /// </summary>
   public abstract class GrammlatorApplication {

      /// <summary>
      /// grammlator uses the <see cref="StackOfMultiTypeElements"/>
      /// <para>a) in grammlator generated code to store attributes;</para>
      /// <para>b) to return the attributes of the start symbol (if any);</para>
      /// <para>c) to get the attributes of terminal symbols.</para>
      /// Beware: access to its elements is not type save.
      /// </summary>
#pragma warning disable IDE1006 // Benennungsstile
      public StackOfMultiTypeElements _a {
         get; protected set;
      }

      /// <summary>
      /// <para>The state stack is used by grammlator generated code to push integer values assigned to states.
      /// When a definition is recognized, processing continues at a state (label) depending on the contents
      /// of the state stack.</para>
      /// <para>By optimization the same number may be assigned to different states.</para>
      /// <para>There may be states with no assigned number.
      /// Each parser / lexer may have its own state stack.
      /// But also different lexers / parsers may share the same state stack.</para>
      /// </summary>
      protected Stack<Int32> _s {
         get;
      }
#pragma warning restore IDE1006 // Benennungsstile

      /// <summary>
      /// This constructor of GrammlatorApplication declares an attribute stack _a and a state stack _s
      /// </summary>
      /// <param name="initialSizeOfAttributeStack">grammlator uses the attributeStack a) in grammlator generated code 
      /// b) to return the attributes of output symbol (if any) and c) to get the attribute of input symbols
      /// </param>
      /// <param name="initialSizeOfStateStack">the code generated by grammlator may need a state stack, which can be shared. 
      /// </param>
      protected GrammlatorApplication(Int32 initialSizeOfAttributeStack = 10, Int32 initialSizeOfStateStack = 10)
      {
         _a = new StackOfMultiTypeElements(initialSizeOfAttributeStack);
         _s = new Stack<Int32>(initialSizeOfStateStack);
      }
   }

   /// <summary>
   /// Abstract base class for classes that provide input for grammlator generated code
   /// and that use grammlator generated code for their owm implementation
   /// </summary>
   /// <typeparam name="TTypeOfOutputSymbols"></typeparam>
   public abstract class GrammlatorInputApplication<TTypeOfOutputSymbols>
       : GrammlatorInput<TTypeOfOutputSymbols> where TTypeOfOutputSymbols : IComparable  /* enum */
       {
      /// <summary>
      /// The state stack is used by grammlator generated code. Each class may have its own state stack. Different classes may share the same state stack.
      /// </summary>
#pragma warning disable IDE1006 // Benennungsstile
      protected Stack<Int32> _s {
         get;
      }
#pragma warning restore IDE1006 // Benennungsstile

      /// <summary>
      /// Constructor of cGrammlatorInputApplication
      /// </summary>
      /// <param name="attributeStack">grammlator uses the attributeStack a) in grammlator generated code 
      /// b) to return the attributes of output symbol (if any) and c) to get the attribute of input symbols
      /// </param>
      /// <param name="stateStack">the code generated by grammlator may need a state stack, which can be shared. 
      /// </param>
      protected GrammlatorInputApplication(StackOfMultiTypeElements attributeStack, Stack<Int32> stateStack)
          : base(attributeStack) => _s = stateStack;
   }

   /*          c S t a t e S t a c k          */

   /// <summary>
   /// Extension Remove(Int32 i) to <see cref="System.Collections.Generic.Stack{T}"/>
   /// </summary>
   public static class StackExtensions {
      ///<summary>
      /// "Remove(1);" is eqivalent to "_=Pop();".  "Remove(2);" is equivalent to "_=Pop(); _=Pop();" and so on.
      /// Remove(i) is executed, when a reduction goes back over n states, with i of them having assigned values.
      /// </summary>
      /// <param name="stack">the base stack</param>
      /// <param name="count">number of elements to remove from the stack</param>
      /// <returns>last element removed</returns>
      public static void Remove<T>(this Stack<T> stack, Int32 count)
      {
         Debug.Assert(stack != null && stack.Count>=count);

         for (Int32 i = 0; i < count; i++)
            stack.Pop();
         return;
      }
   }

   //    /// <summary>
   ///// The state stack is used to push integer values assigned to states. When a production is recognized,
   ///// processing continues depending on the contents of the state stack. By optimization
   ///// the same number may be assigned to different states. There may be states with no assigned number.
   ///// </summary>
   //public class StateStack : Stack<Int32>
   //{
   //    /// <summary>
   //    /// constructor
   //    /// </summary>
   //    /// <param name="capacity"></param>
   //    public StateStack(Int32 capacity) : base(capacity) { }

   //    /// <summary>
   //    /// constructor
   //    /// </summary>
   //    public StateStack() { }

   //    public Int32 Pop(Int32 count)
   //    {
   //        for (Int32 i = 1; i < count; i++) // count-1 Pop
   //            Pop();
   //        return Pop(); // one additional Pop
   //    }
   //}

   /*          s A t t r i b u t e  and  c A t t r i b u t e S t a c k          */

   /* Semantic actions assigned to grammar definitions are C# methods with formal parameters (out-, ref- or value-parameters).
    * Grammlator generates code which uses an attribute stack to store and access the values of attributes.
    * 
    * The generated calls of the methods refer to the stack by PeekRef(i) and by PeekClear(i) with i <=0 as actual parameters.
    * PeekRef(i) returns a reference to the element -i below the top of the stack,
    * PeakClear(i) returns the value of of the element -i below the top of the stack and clears this element.
    * New elements (with the default value) are pushed on the stack with Reserve(n), where n is the number of elements to push.
    * Not longer needed elements are removed by calls of Remove(n), which sets n elements at the top
    * of the stack to the default value and then decrements the number of elements in the stack by n.
    * 
    * To implement the different types of attributes each element of the stack is a struct containing a field 
    * for each possible attribute type, e.g. a field "_Int32" for type "Int32", a field "_Object" for type "Object".
    * 
    * Different fields of the struct can be overlayed to save memory space with one restriction in managed code:
    * reference and value fields must not be overlayed.
    * 
    * The implementation of the attribute stack and the code generated by grammlator must take care
    * not to keep orphan references in the attribute stack. This might happen, if an element of the stack is fírst
    * used to store an object reference (e.g in the field "_Object"), and later to store a value
    * (e.g.) in the field "_Int32". 
    * 
    * This is not trivial to solve, because attributes of the left side of a definition (the attributes of the nonterminal symbol)
    * are assigned to the same elements of the stack as attributes of the right side.
    * 
    * Example:
    * //| nt(Int32 i, Object c) = t1(Object o1), t2(Object c), t2(Object o2) .... 
    *       void method1(out Int32 i, ref Object c, Object o1, Object o2){...}
    * 
    * Position:    -2        /   -1      /    -0
    * Left side:   Int32 i   / object c
    * Right side:  object o1 / object c / object o2
    * 
    * The generated code may look like
    *    method1(
    *       i: out _a.PeekRef(-2)._Int32, 
    *       o: ref _a.PeekRef(-1)._object, 
    *       o1: _a.PeekClear(-2)._object, 
    *       o2: _a.PeekClear(0));
    *    _a.Remove(1);
    *    
    * The element o1 is cleared by PeekClear(-2), the element o2 by PeekClear (0) and also by Remove(1).     
    * The element c is reused and must not be cleared.
    * The method uses a copy of o1 because it is a value parameter and because it is copied by PeekClear.
    * The method uses a copy of o2 because it is a value parameter. 
    * It is cleared by Remove, so PeekRef instead of PeekClear might also be ok.
    * 
    * The case becomes more complicated, if the method does not use o1, e.g.
    *       void method2(out Int32 i, out Object c){...}
    * The generated code must explicitly reset o1:
    *    _a.PeekClear(-2);
    *    method2(i: out _a.PeekRef(-2), o: out _a.PeekRef(-2));
    *    _a.Remove(1);
    * 
    */

   /// <summary>
   /// Each element of the attribute stack stores one attribute of the actually processed productions of the grammar.
   /// Only one of the different fields of each stackelement is used at the same time.
   /// The other fields contain old or undefined values, may overlap on anonother and be overwritten by random binary patterns.
   /// The standard grammlator runtime provides all C# standard types. Additional types can be added.
   /// </summary>
   [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1815:OverrideEqualsAndOperatorEqualsOnValueTypes")]
   [StructLayout(LayoutKind.Explicit)]
#pragma warning disable CA1708 // Identifiers should differ by more than case
   public partial struct MultiTypeStruct // may be extended by "partial" declarations
#pragma warning restore CA1708 // Identifiers should differ by more than case
    {
      /* It is possible, to overlap fields with different object types.
       * Not all errors caused by different overlapping object-types are recognized by the C# compiler or the C# runtime system.
       * Storing an object in one field and accessing the object by an other typed object field 
       * will result in very hard to recognize errors in the behaviour of the program.
       */
#pragma warning disable CA1051 // Do not declare visible instance fields
#pragma warning disable CS1591 // Fehledes XML-Kommentar für öffentlich sichtbaren Typ oder Element
      [FieldOffset(0)]
      public Object _object; // object types

      [FieldOffset(0)]
      public String _string;

      [FieldOffset(8)]       // value fields must not overlap object fields
      public Boolean _bool;

      [FieldOffset(8)]
      public Byte _byte;

      [FieldOffset(8)]
      public SByte _sbyte;

      [FieldOffset(8)]
      public Char _char;

      [FieldOffset(8)]
      public Decimal _decimal;

      [FieldOffset(8)]
      public Double _double;

      [FieldOffset(8)]
      public Single _float;

      [FieldOffset(8)]
      public Int32 _int;

      [FieldOffset(8)]
      public UInt32 _uint;

      [FieldOffset(8)]
      public Int64 _long;

      [FieldOffset(8)]
      public UInt64 _ulong;

      [FieldOffset(8)]
      public Int16 _short;

      [FieldOffset(8)]
      public UInt16 _ushort;

      [FieldOffset(8)]
      public Char _Char;

      [FieldOffset(8)]
      public Int32 _Int32;

      [FieldOffset(8)]
      public UInt32 _Uint32;

      [FieldOffset(8)]
      public Int64 _Int64;

      [FieldOffset(8)]
      public UInt64 _UInt64;

      [FieldOffset(8)]
      public Int16 _Int16;

      [FieldOffset(8)]
      public UInt16 _UInt16;

#pragma warning restore CS1591 // Fehledes XML-Kommentar für öffentlich sichtbaren Typ oder Element
#pragma warning restore CA1051 // Do not declare visible instance fields
   }

   /// <summary>
   /// The <see cref="StackOfMultiTypeElements"/> is used to store the attributes of grammar symbols during the (recursive) analyzing process.
   /// </summary>
   public class StackOfMultiTypeElements {
      /// <summary>
      /// Constructor
      /// </summary>
      /// <param name="initialCapacity">must be >= 0. Ff not specified, an implemenation specific value will be used</param>
      [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1026:DefaultParametersShouldNotBeUsed")]
      public StackOfMultiTypeElements(Int32 initialCapacity)
      {
         if (initialCapacity < 0)
            throw new ArgumentOutOfRangeException(nameof(initialCapacity));
         a = new MultiTypeStruct[initialCapacity];
         TopIndex = -1; // empty stack, no top element
      }

      /// <summary>
      /// Constructs an attribute stack with 100 elements initial capacity
      /// </summary>
      public StackOfMultiTypeElements() : this(100)
      {
      }

      private static readonly MultiTypeStruct MultiTypeStructDefault = new MultiTypeStruct();

      // For each type identifier used in the grammar there must be a corresponding field in the MultiTypeStruct.
      // The identifier of the corresponding field of a type starts with the character '_' followed by the identifier of the type.

      // By using System.Runtime.InteropServices [StructLayout(LayoutKind.Explicit)] it is possible 
      // to overlap different value-fields and different class-fields in the stack, so that aditional types do not use additional space.
      // This option is demonstrated in the following comments.

      // The aGaC runtime library does not contain predefined types.
      // All types used in the grammar are to be specified in a local partial definition.

      /// <summary>
      /// This array implements the stack of attributes.
      /// </summary>
      private MultiTypeStruct[] a;

      /// <summary>
      /// <see cref="TopIndex"/> is the index of the element on top of the stack or -1 if the stack is empty
      /// </summary>
      // // [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "a")]
      internal Int32 TopIndex {
         get; set;
      }

      /// <summary>
      /// Count returns the number of elements the stack contains (x+1)
      /// </summary>
      public Int32 Count => TopIndex + 1;

      /// <summary>
      /// returns and clears element a[top+offset] of the attribute stack
      /// </summary>
      /// <param name="offset">offset from top of stack, must be less or equal 0</param>
      /// <returns>a copy of the element a[top+offset] of the attribute stack</returns>
      public MultiTypeStruct PeekClear(Int32 offset)
      {
         Debug.Assert(offset <= 0 && offset >= -TopIndex, $"Argument of {nameof(PeekClear)} must be <=0");
         MultiTypeStruct result = a[TopIndex + offset];
         a[TopIndex + offset] = MultiTypeStructDefault; // clear all references contained in the element
         return result;
      }

      /// <summary>
      /// clears element a[top+offset] of the attribute stack and returns a ref to this element
      /// </summary>
      /// <param name="offset">offset from top of stack, must be less or equal 0</param>
      /// <returns>a ref to the element a[top+offset] of the attribute stack</returns>
      public ref MultiTypeStruct PeekRefClear(Int32 offset)
      {
         Debug.Assert(offset <= 0 && offset >= -TopIndex, $"Argument of {nameof(PeekClear)} must be <=0");
         ref MultiTypeStruct result = ref a[TopIndex + offset];
         result = MultiTypeStructDefault; // same as "a[TopIndex + offset] = MultiTypeStructDefault;"
         return ref result;
      }

      /// <summary>
      /// returns a reference to element a[top+i] of the attribute stack
      /// </summary>
      /// <param name="offset">offset from top of stack, must be less or equal 0</param>
      /// <returns>a reference to element a[top+i] of the attribute stack</returns>
      public ref MultiTypeStruct PeekRef(Int32 offset) => ref a[TopIndex + offset];

      /// <summary>
      /// Increment the stack pointer without modifying elements.
      /// Increment the capacity of the stack if required.
      /// </summary>
      /// <param name="increment">number by which the stack count is incremented</param>
      public void Allocate(Int32 increment)
      {
         Debug.Assert(increment >= 0, "Argument of Reserve has to be >=0");
         TopIndex += increment;

         // increment the stack if it is not large enough (by a factor of 2)
         Int32 newLength = a.Length;
         while (newLength <= TopIndex)
            newLength *= 2;

         if (newLength > a.Length)
            Array.Resize(ref a, newLength);
      }      

      /// <summary>
      /// Push one element on the stack
      /// </summary>
      /// <param name="elementToPush"></param>
      public void Push(MultiTypeStruct elementToPush)
      {
         Allocate(1);
         a[TopIndex] = elementToPush;
      }

      /// <summary>
      /// Remove <paramref name="count"/> elements from the stack: clear the elements and then decrement <see cref="TopIndex"/>
      /// </summary>
      /// <param name="count">the number of elements to remove </param>
      public void Remove(Int32 count)
      {
         Debug.Assert(count >= 0, $"Argument of {nameof(Remove)} has to be >=0 !");
         Debug.Assert(count <= Count, $"Argument of {nameof(Remove)} has to be <= Count !");

         // Clear discarded elements to avoid orphan references
         for (Int32 i = TopIndex; i >= TopIndex + 1 - count; i--)
         {
            a[i] = MultiTypeStructDefault;
         }
         TopIndex -= count;
         return;
      }

      /// <summary>
      /// Copy <paramref name="count"/> elements from <paramref name="sourceStack"/> and remove them from the source
      /// </summary>
      /// <param name="sourceStack">stack to copy from</param>
      /// <param name="count">number of elements to remove &gt;= 0</param>
      public void CopyAndRemoveFrom(StackOfMultiTypeElements sourceStack, Int32 count)
      {
         if (sourceStack == null)
            return;
         Allocate(count);
         for (Int32 i = 0; i < count; i++)
         {
            a[TopIndex - i] = sourceStack.a[sourceStack.TopIndex - i];
         }
         sourceStack.Remove(count);
      }

      /// <summary>
      /// Copy and remove all elements from <paramref name="sourceStack"/>
      /// </summary>
      /// <param name="sourceStack">stack to copy from</param>
      public void CopyAndRemoveFrom(StackOfMultiTypeElements sourceStack)
      {
         if (sourceStack == null)
            return;
         CopyAndRemoveFrom(sourceStack, sourceStack.Count);
      }            
   }

   public static class Methods {

      /// <summary>
      /// Finds the index of the first occurence of the maximum value. Returns 0 if no argument.
      /// Calls of <see cref="IndexOfMaximum(int[])"/> are generated if conflicts have to be solved by dynamic priorities
      /// </summary>
      /// <param name="values"></param>
      /// <returns></returns>
      public static Int32 IndexOfMaximum(params Int32[] values)
      {
         Int32 Maximum = Int32.MinValue;
         Int32 Index = 0; // result is 0 if all values are Int32.MinValue;
         for (Int32 i = 0; i < values.Length; i++)
            if (values[i] > Maximum)
            {
               Maximum = values[i];
               Index = i;
            }
         return Index;
      }
   }
}