using System;
using System.Collections.Generic;
using System.Text;

namespace grammlator {


   public abstract class Setting {
      public enum SettingType {
         StringType, BooleanType, Int64Type
      }

      public String Name { get; }
      public String NameToLower { get; }
      public SettingType HasType { get; }
      public String InitialValueAsString { get; }
      public String Description { get; }

      public override String? ToString() => base.ToString();

      public virtual String ValueAsString { get { return ""; } }

      protected Setting(String name, SettingType hasType, String initialValueAsString, List<Setting> settingList, String description)
      {
         Name = name;
         NameToLower = name.ToLower();
         HasType = hasType;
         InitialValueAsString = initialValueAsString;
         Description = description;

         settingList.Add(this);
      }

      public virtual void Reset() { }
   }

   public class Int64Setting : Setting {

      public Int64 InitialValue { get; }
      public Int64 Value { get; set; }
      public override String ValueAsString { get { return Value.ToString(); } }

      public Int64Setting(String name, Int64 initialValue, List<Setting> settingList, String description)
         : base(name, SettingType.Int64Type, initialValue.ToString(), settingList, description)
      {
         InitialValue = initialValue;
         Value = initialValue;
      }

      public override String ToString() => Value.ToString();

      public override void Reset() => Value = InitialValue;
   }

   public class BooleanSetting : Setting {

      public Boolean InitialValue { get; }
      public Boolean Value { get; set; }
      public override String ValueAsString { get { return Value ? "true" : "false"; } }

      public BooleanSetting(String name, Boolean initialValue, List<Setting> settingList, String description)
         : base(name, SettingType.BooleanType, initialValue ? "true" : "false", settingList, description)
      {
         InitialValue = initialValue;
         Value = initialValue;
      }

      public override String ToString() => Value ? "true" : "false";

      public override void Reset() => Value = InitialValue;
   }

   public class StringSetting : Setting {

      public String InitialValue { get; }
      public String Value { get; set; }
      public override String ValueAsString { get { return Value; } }

      public StringSetting(String name, String initialValue, List<Setting> settingList, String description)
         : base(name, SettingType.StringType, initialValue.ToString(), settingList, description)
      {
         InitialValue = initialValue;
         Value = initialValue;
      }

      public override String ToString() => Value;

      public override void Reset() => Value = InitialValue;
   }

   public static class GlobalSettings {

      public static void Reset()
      {
         foreach (Setting s in VisibleSettings)
            s.Reset();
         foreach (Setting s in InternalSettings)
            s.Reset();
      }
      /* Grammlator settings */

      /// <summary>
      /// To all elements of the list <see cref="VisibleSettings"/> values can be assigned by instructions at the beginning of the 
      /// grammar part in the input file.
      /// </summary>
      internal static readonly List<Setting> VisibleSettings = new List<Setting>(25);

      /// <summary>
      /// The values of the list <see cref="InternalSettings"/> can not be changed by instructions in the input file.
      /// These settings allow to modify the grammlator program by the programmer.
      /// </summary>
      internal static readonly List<Setting> InternalSettings = new List<Setting>(15);

      /// <summary>
      /// e.g. "//|"
      /// </summary>
      internal static readonly StringSetting GrammarLineMarker
         = new StringSetting("GrammarLineMarker", "//|", InternalSettings,
            @"This string is used to mark grammar lines: ""//|""");

      /// <summary>
      /// e.g. "//"
      /// </summary>
      internal static readonly StringSetting CSharpCommentlineMarker
         = new StringSetting("CSharpCommentlineMarker", "//", InternalSettings,
            @"This string is used to mark comments in the grammar: ""//""");

      /// <summary>
      /// e.g. "#pragma"
      /// </summary>
      internal static readonly StringSetting CSharpPragmaMarker
         = new StringSetting("CSharpPragmaMarker", "#pragma", InternalSettings,
            @"This string is used to mark pragmas in C#: ""#pragma""");

      /// <summary>
      /// e.g. "#region" (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting RegionString
         = new StringSetting("RegionString", "#region", InternalSettings,
            @"The string starting a region: typically ""#region""");

      /// <summary>
      /// e.g. "#endregion"  (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting EndregionString
         = new StringSetting("EndregionString", "#endregion", InternalSettings,
            @"The string ending a region: typically ""#endregion""");

      /// <summary>
      /// e.g. "grammar"  (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting GrammarString
         = new StringSetting("GrammarString", "grammar", InternalSettings,
            @"The name of the region which contains the grammar, typically ""grammar""");

      /// <summary>
      /// e.g. "grammlator"  (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting GrammlatorString
         = new StringSetting("GrammlatorString", "grammlator", InternalSettings,
            @"The 1st part of the name of the region which contains the grammlator generated code, typically ""grammlator""");

      /// <summary>
      /// e.g. "generated"  (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting GeneratedString
         = new StringSetting("GeneratedString", "generated", InternalSettings,
            @"The second part of the name of the region which contains the grammlator generated code, typically ""generated""");

      /// <summary>
      /// <see cref="NewLineWithEscapes"/> is defined by <see cref="Settings.NewLineConstant"/>
      /// and will typically be "\\r\\n"
      /// (unlike <see cref="System.Environment.NewLine"/> typically "\r\n").
      /// </summary>
      internal static readonly StringSetting NewLineWithEscapes
         = new StringSetting("NewLineWithEscapes", "\\r\\n", InternalSettings,
            @"The string representing NewLine in printable form, typically ""\\r\\n""");

      internal static readonly Int64Setting ErrorLimit
         = new Int64Setting("ErrorLimit", 6, InternalSettings,
            @"Grammlator will abort translation if ErrorLimit errormessages are found.");

      /*** Settings which can be modified by the user ***/

      /// <summary>
      /// <see cref="TerminalSymbolEnum.Value"/> (e.g. "LexerResult") is used to generated code
      ///  (e.g. "if (Symbol != LexerResult.Name)...;"
      /// </summary>
      internal static StringSetting TerminalSymbolEnum
         = new StringSetting("TerminalSymbolEnum", "", VisibleSettings,
@"Typically this is the name of the C# enum which defines the terminal symbols.
This name will be used in the generated code in combination with the names of the terminal symbols.
Only in very special applications there is no explicit enum and TerminalSymbolEnum is """".
If an explicit enum is giben and this setting is """", then the name of the explicit enum is used.
Example: """" in very simple applications or if an explicit enum is give else ""MyEnum""");

      /// <summary>
      /// <see cref="SymbolNameOrFunctionCall"/> is used as variable name or method call in generated code
      /// </summary>
      internal static StringSetting SymbolNameOrFunctionCall
         = new StringSetting("SymbolNameOrFunctionCall", "PeekSymbol()", VisibleSettings,
@"This function call will be used in the generated code to peek the input terminal.
Instead of a function call it may be a variable name. This has to be used in the SymbolAssignInstruction.
Grammlator does not generated code which declares this variable or this function. This
has to be done by the application programmer in the context.
Examples: ""Symbol"" or ""PeekSymbol()"" 
      Somewhere in the context there must be a declaration
      ""MyEnum Symbol"" or ""MyEnum PeekSymbol(){....}""");

      /// <summary>
      /// <see cref="SymbolAssignInstruction"/> is used to generate code
      /// </summary>
      internal static StringSetting SymbolAssignInstruction
         = new StringSetting("SymbolAssignInstruction", "", VisibleSettings,
@"This instruction is inserted in the generated code to assign the value of the next
input symbol to a variable (see SymbolNameOrMethodCall).
Examples: """" (if SymbolNameOrMethodCall is a method call)
          or ""Symbol = MyReader.Peek();"" (if SymbolNameOrMethodCall is ""Symbol"")");

      /// <summary>
      /// <see cref="SymbolAcceptInstruction"/> is used to generate code
      /// </summary>
      internal static StringSetting SymbolAcceptInstruction
         = new StringSetting("SymbolAcceptInstruction", "AcceptSymbol();", VisibleSettings,
@"This instruction will be inserted in the generated code at all places where a
terminal symbol has to be accepted.
Example: ""AcceptSymbol();""  or ""_=MyReader.Read();""
         This void method has to be declared somewhere in the context. 
");

      /// <summary>
      /// <see cref="ErrorHandlerMethod"/> is used to generate code
      /// </summary>
      internal static StringSetting ErrorHandlerMethod
         = new StringSetting("ErrorHandlerMethod", "", VisibleSettings,
@"This defines the name of the optional error handler method. The calls of the
error handler are generated as part of states, in which the errors are detected.
Examples: """" or ""ErrorHandler""
   The ErroHandlerMethod has to be declared somewhere in the context.
   Boolean ErrorHandler(Int32 stateNumber, String stateDescription, MyEnum symbol)");

      /// <summary>
      /// <see cref="ErrorHaltInstruction"/> is used to generate code
      /// </summary>
      internal static StringSetting ErrorHaltInstruction
         = new StringSetting("ErrorHaltInstruction", "", VisibleSettings,
@"This optional instruction will be inserted in the generated code so that it is executed
when the generated parser detects an error in its input data.
It is executed after the optional ErrorHandler has been called
just before the jump to the end of generated code.
Examples: """" or ""return false;""");

      internal static StringSetting StateStack
         = new StringSetting("StateStack", "_s", VisibleSettings,
@"This name is used in the generated code as the name of the state stack.
A typical value is ""_s"", which is defined in grammlatorRuntime.cs.");

      internal static StringSetting AttributeStack
         = new StringSetting("AttributeStack", "_a", VisibleSettings,
@"This name is used in the generated code as the name of the attribute stack.
A typical value is ""_a"", which is defined in grammlatorRuntime.cs.");

      internal static Int64Setting TerminalDefaultWeight
         = new Int64Setting("TerminalDefaultWeight", 20, VisibleSettings,
@"The default weight assigned to a terminal symbol. Terminals with a high weight
tend to be checked earlier in generated conditions.");

      internal static StringSetting TerminalDefaultAttributes
         = new StringSetting("TerminalDefaultAttributes", "", InternalSettings,
@"These attributes are provided for terminal declarations with no specification of attributes.
They are specified in an explict terminal declaration by using a ':'. Example: ""a(char c):""");

      internal static BooleanSetting OptimizeStateStackNumbers
        = new BooleanSetting("OptimizeStateStackNumbers", true, VisibleSettings,
@"If this option is set to false, states push their own number on the stack. This improves
readability of the generated code but may cause less performing switch statements.
It may effect other optimizations performed by grammlator.");

      internal static Int64Setting NestingLevelLimit
        = new Int64Setting("NestingLevelLimit", 5, VisibleSettings,
@"This limits the nesting in the generated code.
If this limit is reached a goto is generated instead of an inlined sequence of code.
A typical value is ""5""");

      internal static Int64Setting LineLengthLimit
         = new Int64Setting("LineLengthLimit", 120, VisibleSettings,
 @"This limits the length of lines in the generated code.
A typical value is ""120""");

      /// <summary>
      /// <see cref="ConditionalAction"/>s with complexity &lt;= <see cref="IfToSwitchBorder"/> are generated as if instruction sequence, others as switch statement
      /// </summary>
      internal static Int64Setting IfToSwitchBorder
         = new Int64Setting("IfToSwitchBorder", 5, settingList: VisibleSettings,
@"A sequence of conditional actions is generated as a sequence of if-statements, 
if its complexity (estimated number of logical operations) is less or equal
than this number (typically 5), else a switch statement will be generated.");

      /// <summary>
      /// <see cref="ConditionalAction"/>s with complexity &lt;= <see cref="IfToSwitchBorder"/> are generated as if instruction sequence, others as switch statement
      /// </summary>
      internal static Int64Setting CompareToFlagTestBorder
         = new Int64Setting("CompareToFlagTestBorder", 3, settingList: VisibleSettings,
@"The condition in If-statements can be generated as a sequence of < or > or = comparisions
connected by && and ||. Or it can be generated as a test of flags (if no terminal
symbol has a value >63). If the estmated complexity of the sequence of comparisions
is greater than the CompareToFlagTestBorder then a test of flags will be generated.
A typical value is ""3""");

      /// <summary>
      /// e.g. "_Is(#)". Will be set to "" if an enum is found and the maximum value of an element is &gt;63
      /// </summary>
      internal static StringSetting FlagTestMethodName
         = new StringSetting("IsMethod", "_is", settingList: VisibleSettings,
@"The name of the boolean method which implements the flag test.
If this name is """" then grammlator will not generate and use
this method and the flag constants representing terminal symbols.
It should only be a string value, e.g. the preset value ""_is"", if
the values of the terminal symbols are flags (1, 2, 4, ...) or the
diffence between the largest and the smallest value of a terminal symbol is <=63 and
if the input can be only values explicitely defined in the terminal enum
but no additional values of the enum base type.
Grammlator will reset this pattern to """" if it recognizes that the above condition
is broken.");

      /// <summary>
      /// e.g. "_f"
      /// </summary>
      internal static readonly StringSetting FlagsPrefix
         = new StringSetting("FlagsPrefix", "_f", settingList: VisibleSettings,
@"This string is used as prefix to the names of terminals to declare flag constants.
The initial value ""_f"" will avoid conflicts with other names.");

      /// <summary>
      /// <see cref="StateDescriptionPrefix"/> is used as Prefix when generating names of constants
      /// </summary>
      internal static StringSetting StateDescriptionPrefix
         = new StringSetting("StateDescriptionPrefix", "", settingList: VisibleSettings,
@"For each generated state, for which a call of ErrorHandler is
generated, grammlator combines this string and the number of the state
to the name of a constant with the description of the state.
This constant is used as argument of the generated error handler.
If the prefix is the empty string, then no constant is generated
and the empty string is used as argument in the call of the ErrorHandler.");

      internal static StringSetting StateStackInitialCountVariable
         = new StringSetting("StateStackInitialCountVariable", "_StateStackInitialCount", VisibleSettings,
@"This variable is declared and used in the generated code to store the initial size of
the state stack (if a state stack is used in the generated code).
A typical value is ""StateStackInitialCount"".");

      internal static StringSetting AttributeStackInitialCountVariable
         = new StringSetting("AttributeStackInitialCountVariable", "_AttributeStackInitialCount", VisibleSettings,
@"This  variable is declared and used in the generated code to store the initial size of the
attribute stack (if an attribute stack is used in the generated code).
A typical value is ""_AttributeStackInitialCount"".");

      internal static StringSetting MethodIndexOfMaximum
   = new StringSetting("MethodIndexOfMaximum", "Methods.IndexOfMaximum", VisibleSettings,
@"This is the name of a method which accepts integer arguments and returns
the index of the largest value. A call of this method is generated, if there are
conflicts solved by dynamic priorities and 3 or more priorities have to be compared.
A typical value is ""Methods.IndexOfMaximum"", the name of a method in grammlatorRuntime.cs.");

      internal static StringSetting DebugAssertMethod
         = new StringSetting("DebugAssertMethod", "Debug.Assert", VisibleSettings,
@"If defined then ""Debug.Assert(...)"" instructions will be generated.");

      internal static BooleanSetting GenerateComments
         = new BooleanSetting("GenerateComments", true, VisibleSettings,
@"If true then for each state which does not include a constant declaration with its 
state description a comment with this description will be generated.");
   }
}
