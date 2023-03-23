using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;

namespace grammlator
{


   public abstract class Setting
   {
      public enum SettingType
      {
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
         NameToLower = name.ToLower(CultureInfo.InvariantCulture);
         HasType = hasType;
         InitialValueAsString = initialValueAsString;
         Description = description;

         settingList.Add(this);
      }

      public virtual void Reset() { }
   }

   public class Int64Setting : Setting
   {

      public Int64 InitialValue { get; }
      public Int64 Value { get; set; }
      public override String ValueAsString { get { return Value.ToString(CultureInfo.InvariantCulture); } }

      public Int64Setting(String name, Int64 initialValue, List<Setting> settingList, String description)
         : base(name, SettingType.Int64Type, initialValue.ToString(CultureInfo.InvariantCulture), settingList, description)
      {
         InitialValue = initialValue;
         Value = initialValue;
      }

      public override String ToString() => Value.ToString(CultureInfo.InvariantCulture);

      public override void Reset() => Value = InitialValue;
   }

   public class BooleanSetting : Setting
   {

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

   public class StringSetting : Setting
   {

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

   public static class GlobalSettings
   {

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
      internal static readonly List<Setting> VisibleSettings = new(25);

      /// <summary>
      /// The values of the list <see cref="InternalSettings"/> can not be changed by instructions in the input file.
      /// These settings allow to modify the grammlator program by the programmer.
      /// </summary>
      internal static readonly List<Setting> InternalSettings = new(15);

      /* Grammar Line Markers and Region Settings*/

      /// <summary>
      /// e.g. "//|"
      /// </summary>
      internal static readonly StringSetting CSharpGrammarLineMarker
         = new("CSharpGrammarLineMarker", "//|", InternalSettings,
            @"This string is used to mark grammar lines: ""//|""");

      /// <summary>
      /// e.g. "//"
      /// </summary>
      internal static readonly StringSetting CSharpCommentlineMarker
         = new("CSharpCommentlineMarker", "//", InternalSettings,
            @"This string is used to mark comments in the grammar: ""//""");

      /// <summary>
      /// e.g. "#pragma"
      /// </summary>
      internal static readonly StringSetting CSharpPragmaMarker
         = new("CSharpPragmaMarker", "#pragma", InternalSettings,
            @"This string is used to mark pragmas in C#: ""#pragma""");

      /// <summary>
      /// e.g. "#region" (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting RegionBegin
         = new("RegionBegin", "#region", InternalSettings,
            @"The string starting a region: typically ""#region""");

      /// <summary>
      /// e.g. "#endregion"  (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting RegionEnd
         = new("RegionEnd", "#endregion", InternalSettings,
            @"The string ending a region: typically ""#endregion""");

      /// <summary>
      /// e.g. "grammar"  (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting RegionGrammarMarker
         = new("RegionGrammarMarker", "grammar", InternalSettings,
            @"The name of the region which contains the grammar, typically ""grammar""");

      /// <summary>
      /// e.g. "grammlator"  (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting RegionGrammlatorMarker
         = new("GrammlatorString", "grammlator", InternalSettings,
            @"The 1st part of the name of the region which contains the grammlator generated code, typically ""grammlator""");

      /// <summary>
      /// e.g. "generated"  (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting RegionGeneratedMarker
         = new("RegionGeneratedMarker", "generated", InternalSettings,
            @"The second part of the name of the region which contains the grammlator generated code, typically ""generated""");

      /* */

      /// <summary>
      /// <see cref="StringNewLineWithEscapes"/> is defined by <see cref="Settings.NewLineConstant"/>
      /// and will typically be "\\r\\n"
      /// (unlike <see cref="System.Environment.NewLine"/> typically "\r\n").
      /// </summary>
      internal static readonly StringSetting StringNewLineWithEscapes
         = new("StringNewLineWithEscapes", "\\r\\n", InternalSettings,
            @"The string representing NewLine in printable form, typically ""\\r\\n""");

      internal static readonly Int64Setting GrammlatorErrorLimit
         = new("GrammlatorErrorLimit", 6, InternalSettings,
            @"Grammlator will abort translation if ErrorLimit errormessages are found.");

      internal static readonly StringSetting TerminalSymbolDefaultName
         = new("TerminalSymbolDefaultName", "*default terminal symbol*", InternalSettings,
            @"Grammlator will create a terminal symbol with this name if the source does not declare any terminal symbol");

      // TODO expect '.' to be part of TerminalSymbolEnum !!!

      /*** Terminal and Input Settings ***/

      /// <summary>
      /// <see cref="InputExpression"/> is used as variable name or method call in generated code
      /// </summary>
      internal static StringSetting InputExpression
         = new("InputExpression", "PeekSymbol()", VisibleSettings,
@"This expression (a peek function call, a variable name,...) will be used in the generated comparisions.
If it is a variable name it has to be assigned in the InputAssignInstruction.
Note: Grammlator does not generate code which declares this variable or this function.
This has to be done by the application programmer in the context.
Examples: ""Symbol"" or ""MyReader.Peek()"" or ""(MyEnum)line[i]""
      Somewhere in the context there must be a declaration
      ""MyEnum Symbol"" or ""MyEnum MyReader.Peek(){....}"" or ...");

      internal static BooleanSetting InputPeekChecksBounds
         = new("InputPeekChecksBounds", true, InternalSettings, // TODO implement and then VisibleSettings
@"If false then grammlator generates more complex code which handles values smaller than the
1st terminal symbols value or larger then the last terminals value as error conditions.
If true then grammlator may generate shortened conditions that will behave undefined if the
value to be tested is not the value of a terminal symbol.
This is preset to ""true"".
Note: ""false"" is an experimental not completely implemented option of grammlator.");

      /// <summary>
      /// <see cref="InputAssignInstruction"/> is used to generate code
      /// </summary>
      internal static StringSetting InputAssignInstruction
         = new("InputAssignInstruction", "", VisibleSettings,
@"This instruction is inserted in the generated code to assign the value of the next
input symbol to a variable (see InputComparisionArgument).
Examples: """" (if InputComparisionArgument is a method call)
          or ""Symbol = MyReader.Peek();"" (if InputComparisionArgument is ""Symbol"")");

      /// <summary>
      /// <see cref="InputAcceptInstruction"/> is used to generate code
      /// </summary>
      internal static StringSetting InputAcceptInstruction
         = new("InputAcceptInstruction", "AcceptSymbol();", VisibleSettings,
@"This instruction will be inserted in the generated code at all places where a
terminal symbol (the value of InputComparisionArgument) has to be accepted.
Example: ""AcceptSymbol();""  or ""_ = MyReader.Read();""
         This method has to be declared somewhere in the context. 
");

      internal static Int64Setting TerminalDefaultWeight
         = new("TerminalDefaultWeight", 20, VisibleSettings,
@"The default weight assigned to a terminal symbol. Terminals with a high weight
tend to be checked earlier in generated sequences of conditions.");

      internal static StringSetting TerminalDefaultAttributes
         = new("TerminalDefaultAttributes", "", InternalSettings,
@"These attributes are provided for terminal declarations with no specification of attributes.
Example: ""(char c)""");

      // TODO implement TerminalDefaultAttributes

      internal const string TerminalSymbolUndefinedValue = "\uFFFF";

      /// <summary>
      /// <see cref="TerminalSymbolEnum.Value"/> (e.g. "LexerResult") is used to generated code
      ///  (e.g. "if (Symbol != LexerResult.Name)...;"
      /// </summary>
      internal static StringSetting TerminalSymbolEnum
         = new("TerminalSymbolEnum", TerminalSymbolUndefinedValue, VisibleSettings,
@"The initial value is a special character which denotes that this value is undefined.
If defined and not equal """", this name will be combined in the generated code with the names
of the terminal symbols using the dot syntax ""enum.member"".
In special cases it will be used as the type of the enum elements.
Only in very special applications there is no explicit enum and TerminalSymbolEnum is """".
If an explicit enum is used to define the terminals and this setting is the special initial value,
then TerminalSymbolEnum is used.
Typical usage: ""MyClass.MyEnum"" if the enum defining the terminal symbols is defined in another file
   and a copy with a modified name is used to define the terminal symbols.");

      /// <summary>
      /// <see cref="NameOfErrorHandlerMethod"/> is used to generate code
      /// </summary>
      internal static StringSetting NameOfErrorHandlerMethod
         = new("NameOfErrorHandlerMethod", "", VisibleSettings,
@"This defines the name of the optional error handler method. The calls of the
error handler are generated as part of states, in which the errors are detected.
Examples: """" or ""ErrorHandler""
   The ErroHandlerMethod has to be declared somewhere in the context.
   Boolean ErrorHandler(Int32 stateNumber, String stateDescription, MyEnum symbol)");

      /// <summary>
      /// <see cref="ErrorHaltInstruction"/> is generated code only once 
      /// </summary>
      internal static StringSetting ErrorHaltInstruction
         = new("ErrorHaltInstruction", "", VisibleSettings,
@"This optional instruction will be inserted in the generated code so that it is executed
when the generated parser detects an error in its input data.
It is executed after the optional NameOfTheErrorHandlerMethod has been called
 nd returned false (or if there is no NameOfTheErrorHandlerMethod defined)
just before the jump to the end of generated code.
Examples: """" or ""return false;""");

      internal static BooleanSetting GenerateSmallStateStackNumbers
        = new("GenerateSmallStateStackNumbers", true, VisibleSettings,
@"If this option is set to false, states push their own number on the stack. This improves
readability of the generated code but may cause less performing switch statements.
It may affect other optimizations performed by grammlator.");

      internal static Int64Setting OutputNestingLevelLimit
        = new("NestingLevelLimit", 5, VisibleSettings,
@"This limits the nesting in the generated code.
If this limit is exceeded a goto is generated instead of an inlined sequence of code.
A typical value is ""5""");

      internal static Int64Setting OutputLineLengthLimit
         = new("LineLengthLimit", 120, VisibleSettings,
 @"This limits the length of lines in the generated code.
If this limit is exceeded a line break will be inserted in the generated code.
A typical value is ""120""");

      /// <summary>
      /// <see cref="ConditionalAction"/>s with complexity &lt;= <see cref="GenerateSwitchStartingLevel"/> are generated as if instruction sequence, others as switch statement
      /// </summary>
      internal static Int64Setting GenerateSwitchStartingLevel
         = new("GenerateSwitchStartingLevel", 5, settingList: VisibleSettings,
@"A sequence of conditional actions is generated as a sequence of if statements, 
if its complexity (estimated number of logical operations) is less or equal
than this number (typically 5), else a switch statement will be generated.");

      /// <summary>
      /// Conditions of if-instructions are generated as flag tests if the complexity of comparisions is >= <see cref="GenerateFlagTestStartingLevel"/>
      /// </summary>
      internal static Int64Setting GenerateFlagTestStartingLevel
         = new("GenerateFlagTestStartingLevel", 3, settingList: VisibleSettings,
@"The comparision in If-statements can be generated as a sequence of
< or > or == comparisions together with && and || operations.
Or it can be generated as a test of flags if the span of the values
of terminal symbols is less or equal 63 or if the values are flags.
If the estimated complexity of the sequence of comparisions is greater than the
CompareToFlagTestBorder then a test of flags will be generated.
A typical value is ""3"".");

      /// <summary>
      /// e.g. "_Is(#)". Will be set to "" if the span of the values of the terminal symbols is &gt;63
      /// or if the values of the terminal symbols can be used as flags directly
      /// </summary>
      internal static StringSetting NameOfFlagTestMethod
         = new("NameOfFlagTestMethod", "_is", settingList: VisibleSettings,
@"The name of the boolean method which implements the flag test if the values of the 
terminals symbols are not flags (2,4, 8, ...).
If this name is """" then grammlator will not generate and use
this method and the flag constants representing terminal symbols.
It should only be a string value, e.g. the preset value ""_is"", if
the difference between the largest and the smallest value of a terminal symbol is <=63.
Grammlator will reset this pattern to """" if it recognizes that the above condition
is broken.");

      /// <summary>
      /// e.g. "_f"
      /// </summary>
      internal static readonly StringSetting PrefixOfFlagConstants
         = new("PrefixOfFlagConstants", "_f", settingList: VisibleSettings,
@"This string is used as prefix to the names of terminals to declare flag constants.
The initial value ""_f"" will avoid conflicts with other names.");

      internal static StringSetting NameOfAssertMethod
         = new("NameOfAssertMethod", "Debug.Assert", VisibleSettings,
@"If defined then ""Debug.Assert(...)"" instructions will be generated.
These assertions are less intended to debug the code but primarily to comment the code.");

      internal static BooleanSetting GenerateComments
         = new("GenerateComments", true, VisibleSettings,
@"If true then comments are generated to make the generated code understandable.");

      /// <summary>
      /// <see cref="PrefixOfStateDescriptionConstant"/> is used as Prefix when generating names of constants
      /// </summary>
      internal static StringSetting PrefixOfStateDescriptionConstant
         = new("PrefixOfStateDescriptionConstant", "", settingList: VisibleSettings,
@"For each generated state, for which a call of ErrorHandler is
generated, grammlator combines this string and the number of the state
to the name of a constant with the description of the state.
This constant is used as argument of the generated error handler.
If the prefix is the empty string, then no constant is generated
and the empty string is used as argument in the call of the ErrorHandler.
Then instead (if GenerateComments is true) the description of the state is generated as a comment.
");

      internal static StringSetting PriorityDynamicSwitchAndStartOfMathExpression
   = new("PriorityDynamicSwitchPart1", "switch(Methods.IndexOfMaximum(", VisibleSettings,
@"This switch instruction uses a method which accepts integer arguments and returns
the index of the largest value. A call of this instruction is generated, if there are
conflicts solved by dynamic priorities and 3 or more priorities have to be compared.
A typical value is ""switch(Methods.IndexOfMaximum("", the name of a method in grammlatorRuntime.cs.");

      internal static StringSetting PriorityDynamicSwitchEndOfMatchExpression
   = new("PriorityDynamicSwitchEndOfMatchExpression", "))", VisibleSettings,
@"A typical value is ""))""");

      internal static StringSetting PriorityDynamicSwitchCaseLabelFormat
   = new("PriorityDynamicSwitchCaseLabelFormat", "case {0}: ", VisibleSettings,
@"This format is used to generate the case labels.
A typical value is ""case {0}: """);

      /*** State and Attribute Stack settings ***/

      internal static StringSetting StateStack
         = new("StateStack", "_s", VisibleSettings,
@"This name is used in the generated code as the name of the state stack.
A typical value is ""_s"", which is defined in grammlatorRuntime.cs.");

      internal static readonly StringSetting StateStackPeekMethodFormat
         = new("StateStackPeekMethodFormat", "{0}.Peek()", InternalSettings,
             @"to be used with 1 argument: the name of the state stack");

      internal static readonly StringSetting StateStackIfPeekEqualMethodFormat
         = new("StateStackIfPeekEqualMethodFormat", "if ({0}.Peek() == {1})", InternalSettings,
             @"to be used with 2 arguments: the name of the state stack and the condition");

      internal static readonly StringSetting StateStackIfPeekNotEqualMethodFormat
         = new("StateStackIfPeekNotEqualMethodFormat", "if ({0}.Peek() != {1})", InternalSettings,
             @"to be used with 2 arguments: the name of the state stack and the condition");

      internal static readonly StringSetting StateStackPushInstructionFormat
         = new("StateStackPushInstructionFormat", "{0}.Push({1});", InternalSettings,
            @"to be used with 2 argument: the name of the state stack and the value to push");

      internal static readonly StringSetting StateStackRemoveInstructionFormat
         = new("StateStackRemoveInstructionFormat", "{0}.Remove({1});", InternalSettings,
            @"to be used with 2 arguments: the name of the state stack and the number of elements to remove.");

      internal static StringSetting StateStackNameOfInitialCountVariable
   = new("StateStackNameOfInitialCountVariable", "_StateStackInitialCount", InternalSettings,
@"This variable is declared and used in the generated code to store the initial size of
the state stack (if a state stack is used in the generated code).
A typical value is ""StateStackInitialCount"".");

      internal static readonly StringSetting StateStackSaveCountInstructionFormat
         = new("StateStackSaveCountInstructionFormat", "Int32 {0} = {1}.Count;", InternalSettings,
@"This instruction is generated at most once.
The format uses 2 arguments:
(0) the NameOfStateStackInitialCountVariable
(1) the StateStack");

      internal static readonly StringSetting StateStackResetInstructionFormat
         = new("StateStackResetInstructionFormat", "{0}.Remove({0}.Count - {1});",
            InternalSettings,
            @"to be used with 2 arguments: the name of the state stack and the NameOfStateStackInitialCountVariable.");

      internal static StringSetting AttributeStack
   = new("AttributeStack", "_a", VisibleSettings,
@"This name is used in the generated code as the name of the attribute stack.
A typical value is ""_a"", which is defined in grammlatorRuntime.cs.");

      internal static StringSetting AttributesOfSymbolStack
   = new("AttributesOfSymbolStack", "_AttributesOfSymbol", VisibleSettings,
@"This name is used in the generated code as the name of a small attribute stack,
which stores the attributes of the last recognized symbol while
reductions may modify the stack.
A typical value is ""_a"", which is defined in grammlatorRuntime.cs.");

      internal static StringSetting AttributesCopyAndRemoveInstructionFormat
         = new("AttributesCopyAndRemoveInstructionFormat",
            "{0}.CopyAndRemoveFrom({1}, {2});", VisibleSettings,
@"This instruction is used to move n attribute values from one stack
to another stack. This format is used with 3 arguments:
(0) the destination stack, (1) the source stack, (2) the number of arguments");

      internal static readonly StringSetting AttributeStackAllocateInstructionFormat
= new("AttributeStackAllocateInstructionFormat", "{0}.Allocate({1});", InternalSettings,
@"to be used with 2 arguments: the name of the attribute stack and the number of elements to allocate.");

      internal static readonly StringSetting AttributeStackRemoveInstructionFormat
   = new("AttributeStackRemoveInstructionFormat", "{0}.Remove({1});", InternalSettings,
@"to be used with 2 arguments: the name of the attribute stack and the number of elements to remove.");

      internal static StringSetting AttributeStackNameOfInitialCountVariable
         = new("AttributeStackNameOfInitialCountVariable", "_AttributeStackInitialCount", VisibleSettings,
@"This  variable is declared and used in the generated code to store the initial size of the
attribute stack (if an attribute stack is used in the generated code).
A typical value is ""_AttributeStackInitialCount"".");

      internal static readonly StringSetting AttributeStackSaveCountInstructionFormat
   = new("AttributeStackSaveCountInstructionFormat", "Int32 {0} = {1}.Count;", InternalSettings,
@"This instruction is generated at most once.
The format uses 2 arguments:
(0) the NameOfAttributeStackInitialCountVariable
(1) the AttributeStack");

      internal static readonly StringSetting AttributeStackResetInstructionFormat
   = new("AttributeStackResetInstructionFormat", "{0}.Remove({0}.Count - {1});",
      InternalSettings,
      @"to be used with 2 arguments: the name of the attribute stack and the NameOfAttributeStackInitialCountVariable.");

      internal static readonly StringSetting AttributePeekRefExpressionFormat
   = new("AttributePeekRefExpressionFormat", "{0}.PeekRef({1})._{2}",
      InternalSettings,
@"This C# expression is used as actual parameter of semantic method calls.
It is used with 2 arguments:
(0) the name of the attribute stack
(1) the offset (<=0)
(2) the access type");

      internal static readonly StringSetting AttributePeekRefClearExpressionFormat
   = new("AttributePeekRefClearExpressionFormat", "{0}.PeekRefClear({1})._{2}",
      InternalSettings,
@"This C# expression is used as actual parameter of semantic method calls.
It is used with 2 arguments:
(0) the name of the attribute stack
(1) the offset (<=0)
(2) the access type");

      internal static readonly StringSetting AttributePeekClearExpressionFormat
   = new("AttributePeekClearExpressionFormat", "{0}.PeekClear({1})._{2}",
      InternalSettings,
@"This C# expression is used as actual parameter of semantic method calls.
It is used with 2 arguments:
(0) the name of the attribute stack
(1) the offset (<=0)
(2) the access type");

   }
}
