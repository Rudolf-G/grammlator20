using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Text;

namespace Grammlator {

   public class Setting {
      public enum SettingType {
         StringType, BooleanType, Int32Type
      }

      public String Name { get; }
      public String NameToLower { get; }
      public SettingType HasType { get; }
      public String InitialValueAsString { get; }
      public String Description { get; }

      public override String? ToString() => base.ToString();

      public virtual String ValueAsString { get { return ""; } }

      protected Setting(String name, SettingType hasType, String initialValueAsString, String description, List<Setting> settingList)
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

   public class Int32Setting : Setting {

      public Int32 InitialValue { get; }
      public Int32 Value { get; set; }
      public override String ValueAsString { get { return Value.ToString(); } }

      public Int32Setting(String name, Int32 initialValue, String description, List<Setting> settingList)
         : base(name, SettingType.Int32Type, initialValue.ToString(), description, settingList)
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
      public override String ValueAsString { get { return Value.ToString(); } }

      public BooleanSetting(String name, Boolean initialValue, String description, List<Setting> settingList)
         : base(name, SettingType.BooleanType, initialValue.ToString(), description, settingList)
      {
         InitialValue = initialValue;
         Value = initialValue;
      }

      public override String ToString() => Value.ToString();

      public override void Reset() => Value = InitialValue;
   }

   public class StringSetting : Setting {

      public String InitialValue { get; }
      public String Value { get; set; }
      public override String ValueAsString { get { return Value; } }

      public StringSetting(String name, String initialValue, String description, List<Setting> settingList)
         : base(name, SettingType.StringType, initialValue.ToString(), description, settingList)
      {
         InitialValue = initialValue;
         Value = initialValue;
      }

      public override String ToString() => Value;

      public override void Reset() => Value = InitialValue;
   }

   class MemoryComparer : IEqualityComparer<ReadOnlyMemory<Char>> {
      public Boolean Equals(ReadOnlyMemory<Char> rom1, ReadOnlyMemory<Char> rom2)
         => rom1.Span.SequenceEqual(rom2.Span);

      public Int32 GetHashCode(ReadOnlyMemory<Char> rom)
         => String.GetHashCode(rom.Span);
   }


   internal static class GlobalVariables {

      static GlobalVariables()
      {
         Startsymbol = new NonterminalSymbol("*Startsymbol",
            position: 0,
            symbolNumber: 0,
            attributetypeStringIndexList: Array.Empty<Int32>(),
            attributenameStringIndexList: Array.Empty<Int32>()
            );
         TerminalSymbolByIndex = Array.Empty<TerminalSymbol>();
         OutputMessage = OutputToNirwana;
         OutputMessageAndPosition = OutputToNirwana;
         Startaction = new DeletedParserAction();
      }
      private static String GetVersioninfo {
         get {

            Assembly ThisAssembly = typeof(GlobalVariables).Assembly;
            AssemblyName AssemblyName = ThisAssembly.GetName();
            String AssemblyFullPath = ThisAssembly.Location;
            FileVersionInfo fvi = FileVersionInfo.GetVersionInfo(AssemblyFullPath);
            String FileVersion = fvi.FileVersion;
            String FileWrittenDate = System.IO.File.GetLastWriteTime(AssemblyFullPath).ToString();

            /* FileVersion is used instead of Version because Version generally should  be changed only to reflect major changes.
             */

            /*
           DateTimeOffset dto = (new DateTime(2000, 1, 1, 0, 0, 0, DateTimeKind.Local));
           dto = (dto.AddDays(AVersion.Build).AddSeconds(AVersion.Revision * 2));
           String buildTime = dto.ToUniversalTime().ToString("r");
           // ToUniversalTime().ToString();

           // https://msdn.microsoft.com/de-de/library/system.reflection.assemblyversionattribute%28v=vs.110%29.aspx

           // File version: Die Fileversion wird in Visual Studio (an gleicher Stelle) gesondert festgelegt

           // In Visual Studio kann unter Projekt / ... Eigenschaften // Veröffentlichen 
           // die Produktversion eingestellt werden, optional so, dass die Revision bei jeder Veröffentlichung erhöht wird
           // Info dazu bietet die Hilfeseite zur Eingabe in "Veröffentlichen"
           // if (ApplicationDeployment.IsNetworkDeployed) {
           //    string Version_Label = "Version " + ApplicationDeployment.CurrentDeployment.CurrentVersion.ToString() +
           //        " (Click-Once-Installation)" + buildTime;
           //    }

           String VersionInfo = new StringBuilder(100)
              .Append(" by ")
              .Append(AssemblyName.Name)
              .Append(" version ")
              .Append(AVersion.Major.ToString())
              .Append(':')
              .Append(AVersion.Minor.ToString())
              .Append(" (build ")
              .Append(buildTime)
              .Append(')')
              .ToString();
           return VersionInfo;
           */
            return new StringBuilder(100)
               .Append(" (")
               .Append(AssemblyName.Name)
               .Append(", File version ")
               .Append(FileVersion)
               .Append(" ")
               .Append(FileWrittenDate)
               .Append(")")
               .ToString();
         }
      }

      private static readonly String VersionInfo = GetVersioninfo;
      internal static String TranslationInfo => DateTime.Now.ToString("r") + VersionInfo;
      internal const Int32 InitialCapacityOfListOfAllStates = 1000;
      internal const Int32 InitialCapacityOfListOfAllReductions = 400;
      internal const Int32 InitialCapacityOfListOfAllBranchActions = 200;
      internal const Int32 InitialCapacityOfListOfAllHaltActions = 20;
      internal const Int32 InitialCapacityOfListOfAllPushStateActions = 20;
      internal const Int32 InitialCapacityOfListOfAllPrioritySelectActions = 20;
      internal const Int32 InitialCapacityOfListOfAllPriorityBranchActions
         = InitialCapacityOfListOfAllPrioritySelectActions;

      static readonly Dictionary<ReadOnlyMemory<Char>, Int32> MemoryToIndexDictionary
         = new Dictionary<ReadOnlyMemory<Char>, Int32>(1000, new MemoryComparer());

      static readonly List<String> IndexToString = new List<String>(1000);

      static public Int32 GetIndexOfString(ReadOnlyMemory<Char> MemorySpan)
      {
         if (MemoryToIndexDictionary.TryGetValue(MemorySpan, out Int32 result))
            return result;

         String s = MemorySpan.ToString(); // allocation of string which usually will be used for output later
         ReadOnlyMemory<Char> newSpan = s.AsMemory(); // keep bytes referenced from dictionary together:  (don't use reference to source)

         // Discussion: This solution allocates all strings as soon as they are known and avoids multiple instances of those strings.
         // An alternate method would be, to access the MemorySpans (scattered around the source) and to generate strings not before they are used.
         // A third method would be to allocate copies of the Spans in a large array to keep them together
         //   so that dictionary searches would have good local access behaviour.
         IndexToString.Add(s); // allocation of string which usually will be used later for output
         MemoryToIndexDictionary.Add(newSpan, IndexToString.Count - 1);
         return IndexToString.Count - 1;
      }

      static public Int32 GetIndexOfString(String s)
      {
         ReadOnlyMemory<Char> m = s.AsMemory();
         if (MemoryToIndexDictionary.TryGetValue(m, out Int32 result))
            return result;

         IndexToString.Add(s); // string is alrady available
         MemoryToIndexDictionary.Add(m, IndexToString.Count - 1);
         return IndexToString.Count - 1;
      }

      static public Int32 GetIndexOfEmptyString()
      {
         return GetIndexOfString(ReadOnlyMemory<Char>.Empty);
      }

      static public String GetStringOfIndex(Int32 i) => IndexToString[i];

      public static void ResetGlobalVariables(
                Action<MessageTypeOrDestinationEnum, String> OutputMessage,
                Action<MessageTypeOrDestinationEnum, String, Int32> outputMessageAndPosition)
      {
         // Reset all settings
         foreach (Setting s in VisibleSettings)
            s.Reset();
         foreach (Setting s in InternalSettings)
            s.Reset();

         // Reset all static variables 
         GlobalVariables.OutputMessage = OutputMessage;
         GlobalVariables.OutputMessageAndPosition = outputMessageAndPosition;
         NumberOfTerminalSymbols = 0;
         NumberOfNonterminalSymbols = 0;
         Startsymbol = new NonterminalSymbol("*Startsymbol",
            position: 0,
            symbolNumber: 0,
            attributetypeStringIndexList: Array.Empty<Int32>(),
            attributenameStringIndexList: Array.Empty<Int32>()
            // trivalDefinitionsArray: Array.Empty<Symbol>(), // the startsymbol will not have any trival definitions: default
            // nontrivalDefinitionsList: ... // will be set after the startsymbol: default (empty list)
            );

         MemoryToIndexDictionary.Clear();
         IndexToString.Clear();

         ListOfAllHaltActions.Clear();
         ListOfAllHaltActions.Capacity = InitialCapacityOfListOfAllHaltActions;
         ListOfAllHaltActions.Add(new HaltAction(IdNumber: 0, AttributestackAdjustement: 0));

         TerminalSymbolByIndex = Array.Empty<TerminalSymbol>();
         AllTerminalSymbols = EmptyBitarray;

         ListOfAllStates.Clear();
         ListOfAllStates.Capacity = InitialCapacityOfListOfAllStates;

         ListOfAllErrorhandlingActions.Clear();
         ListOfAllErrorhandlingActions.Capacity = InitialCapacityOfListOfAllStates;

         ListOfAllReductions.Clear();
         ListOfAllReductions.Capacity = InitialCapacityOfListOfAllReductions;

         ListOfAllBranchActions.Clear();
         ListOfAllBranchActions.Capacity = InitialCapacityOfListOfAllBranchActions;

         ListOfAllPushStateActions.Clear();
         ListOfAllPushStateActions.Capacity = InitialCapacityOfListOfAllPushStateActions;

         ListOfAllPrioritySelectActions.Clear();
         ListOfAllPrioritySelectActions.Capacity = InitialCapacityOfListOfAllPrioritySelectActions;

         ListOfAllPriorityBranchActions.Clear();
         ListOfAllPriorityBranchActions.Capacity = InitialCapacityOfListOfAllPriorityBranchActions;
      }
      /* Options:
       * */

      /* Grammlator settings */

      internal static readonly List<Setting> VisibleSettings = new List<Setting>(25); // >=17
      internal static readonly List<Setting> InternalSettings = new List<Setting>(15); // >=9

      /// <summary>
      /// e.g. "//" (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting GrammarLineMarker
         = new StringSetting("GrammarLineMarker", "//|",
         "This string is used to mark grammar lines: \"//|\"",
         settingList: InternalSettings);

      /// <summary>
      /// e.g. "//" (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting CSharpCommentlineMarker
         = new StringSetting("CSharpCommentlineMarker", "//",
         "This string is used to mark comments in the grammar: \"//\"",
         settingList: InternalSettings);

      /// <summary>
      /// e.g. "#pragma" (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting CSharpPragmaMarker
         = new StringSetting("CSharpPragmaMarker", "#pragma",
         "This string is used to mark pragmas in C#: \"#pragma\"",
         settingList: InternalSettings);

      /// <summary>
      /// e.g. "#pragma" (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting FlagsPrefix
         = new StringSetting("FlagsPrefix", "_f",
         "This string is used as prefix to the names of terminals when flags are declared or used: \"_f\"",
         settingList: InternalSettings);


      /// <summary>
      /// e.g. "#region" (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting RegionString
         = new StringSetting("RegionString", "#region",
         "The string starting a region: typically \"#region\"",
         settingList: InternalSettings);

      /// <summary>
      /// e.g. "#endregion"  (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting EndregionString
         = new StringSetting("EndregionString", "#endregion",
         "The string ending a region: typically \"#endregion\"",
         settingList: InternalSettings);

      /// <summary>
      /// e.g. "grammar"  (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting GrammarString
         = new StringSetting("GrammarString", "grammar",
         "The name of the region which contains the grammar, typically \"grammar\"",
         settingList: InternalSettings);

      /// <summary>
      /// e.g. "grammlator"  (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting GrammlatorString
         = new StringSetting("GrammlatorString", "grammlator",
         "The 1st part of the name of the region which contains the grammlator generated code, typically \"grammlator\"",
         settingList: InternalSettings);

      /// <summary>
      /// e.g. "generated"  (not including the apostrophes)
      /// </summary>
      internal static readonly StringSetting GeneratedString
         = new StringSetting("GeneratedString", "generated",
         "The second part of the name of the region which contains the grammlator generated code, typically \"generated\"",
         settingList: InternalSettings);

      /// <summary>
      /// e.g. "_1In(#)". Will be set to "" if an enum is found and the maximum value of an element is &gt;63
      /// </summary>
      internal static StringSetting IsInMethod
         = new StringSetting("IsInMethod", "_IsIn(#)",
         "The pattern defining the IsIn-Method, typically \"_IsIn(#)\"",
         settingList: VisibleSettings);

      /// <summary>
      /// <see cref="NewLineWithEscapes"/> is defined by <see cref="Settings.NewLineConstant"/>
      /// and will typically be "\\r\\n"
      /// (unlike <see cref="System.Environment.NewLine"/> typically "\r\n").
      /// </summary>
      internal static readonly StringSetting NewLineWithEscapes
         = new StringSetting("NewLineWithEscapes", "\\r\\n",
         "The string representing NewLine in printabel form, typically \"\\r\\n\"",
         settingList: InternalSettings);

      /// <summary>
      /// <see cref="ConditionalAction"/>s with complexity &lt;= <see cref="IfToSwitchBorder"/> are generated as if instruction sequence, others as switch statement
      /// </summary>
      internal static Int32Setting IfToSwitchBorder
         = new Int32Setting("IfToSwitchBorder", 5,
         "A sequence of conditional actions is generated as  sequence of if-statements, "
         + "if its complexity is less or equal than this number (typically 5), "
         + "else a switch statement will be generated.",
         settingList: VisibleSettings);

      /// <summary>
      /// <see cref="StateDescriptionPrefix"/> is used as Prefix when generating names of constants
      /// </summary>
      internal static StringSetting StateDescriptionPrefix
         = new StringSetting("StateDescriptionPrefix", "",
         "if not empty grammlator will use this string "
         + "as prefix of names of constants which get the description of the actual state "
         + "and which are used in case of errors, e.g. \"StateDescription\"",
         settingList: VisibleSettings);

      /// <summary>
      /// <see cref="VariableNameSymbol"/> is used as variable name or method call in generated code
      /// </summary>
      internal static StringSetting VariableNameSymbol
         = new StringSetting("VariableNameSymbol", "PeekSymbol()",
         "This variable name or method call will be used in the generated code "
         + "to get the last peeked input terminal "
         + "typically \"Symbol\" or e.g. \"PeekTerminal()\"",
         settingList: VisibleSettings);

      /// <summary>
      /// <see cref="TerminalSymbolEnum.Value"/> (e.g. "LexerResult") is used to generated code
      ///  (e.g. "if (Symbol != LexerResult.Name)...;"
      /// </summary>
      internal static StringSetting TerminalSymbolEnum
         = new StringSetting("TerminalSymbolEnum", "",
         "This variable name or method call will be used in the generated code "
         + "in combination with the names of the terminal symbols "
         + " which are defined as elements of this enum, "
         + "typically empty or the name of an enum.",
         settingList: VisibleSettings);

      /// <summary>
      /// <see cref="InstructionAssignSymbol"/> is used to generate code
      /// </summary>
      internal static StringSetting InstructionAssignSymbol
         = new StringSetting("AssignSymbolInstruction", "",
         "This string will be used in the generated code "
         + "to assign a peeked terminal to a variable, "
         + "typically empty or e.g. \"Symbol = Lexer.PeekSymbol();\"",
         settingList: VisibleSettings);

      /// <summary>
      /// <see cref="InstructionAcceptSymbol"/> is used to generate code
      /// </summary>
      internal static StringSetting InstructionAcceptSymbol
         = new StringSetting("AcceptSymbolInstruction", "AcceptSymbol();",
         "This instruction will be inserted in the generated code "
         + "where a terminal symbol has to be accepted, "
         + "typically e.g. \"AcceptSymbol();\"",
         settingList: VisibleSettings);

      /// <summary>
      /// <see cref="InstructionErrorHalt"/> is used to generate code
      /// </summary>
      internal static StringSetting InstructionErrorHalt
         = new StringSetting("InstructionErrorHalt", "",
         "This instruction will be inserted in the generated code "
         + "where a error halt is handled (after return from the errorhandler), "
         + "typically empty or e.g. \"return false;\"",
         settingList: VisibleSettings);

      internal static Int32Setting NestingLevelLimit
         = new Int32Setting("NestingLevelLimit", 5,
         "This limits the nesting in the generated code. "
         + "If this limit is reached a goto is generated instead of a inlined sequence of code. "
         + "A typical value is \"5\"",
         settingList: VisibleSettings);

      internal static Int32Setting LineLengthLimit
         = new Int32Setting("LineLengthLimit", 120,
         "This limits the length of lines in the generated code. "
         + "A typical value is \"120\"",
         settingList: VisibleSettings);

      /// <summary>
      /// <see cref="ErrorHandlerMethod"/> is used to generate code
      /// </summary>
      internal static StringSetting ErrorHandlerMethod
         = new StringSetting("ErrorHandlerMethod", "",
         "This defines the name of the error handler method. "
         + "If it is empty a goto ErrorHalt is generated. "
         + " Typically it is empty or e.g. \"ErrorHandler\"",
         settingList: VisibleSettings);

      /// <summary>
      /// ErrorHandlerIsDefined => !string.IsNullOrEmpty(ErrorHandlerMethod);
      /// </summary>
      internal static Boolean ErrorHandlerIsDefined => !String.IsNullOrEmpty(ErrorHandlerMethod.Value);

      internal static StringSetting MethodIndexOfMaximum
         = new StringSetting("MethodIndexOfMaximum", "Methods.IndexOfMaximum",
         "This is the name of a method which accepts integer arguments and returns "
         + "the index of the largest value. A call of this method is generated, if there are "
         + " conflicts solved by dynamic priorities and 3 or more priorities have to be compared."
         + " The initial value is \"Methods.IndexOfMaximum\" is the name of a method in grammlatorRuntime.cs.",
         settingList: VisibleSettings);

      internal static StringSetting StateStackInitialCountVariable
         = new StringSetting("StateStackInitialCountVariable", "_StateStackInitialCount",
         "This  variable is used in the generated code to store the initial size of the "
         + "state stack - if a state stack is used."
         + " The initial value is \"StateStackInitialCount\".",
         settingList: VisibleSettings);

      internal static StringSetting StateStack
         = new StringSetting("StateStack", "_s",
         "This name is used in the generated code as the name of the state stack."
         + " The initial value is \"_s\"., which is defined in grammlatorRuntime.cs.",
         settingList: VisibleSettings);

      internal static StringSetting AttributeStackInitialCountVariable
         = new StringSetting("AttributeStackInitialCountVariable", "_AttributeStackInitialCount",
         "This  variable is used in the generated code to store the initial size of the "
         + "attribute stack - if an attribute stack is needed."
         + " The initial value is \"_AttributeStackInitialCount\".",
         settingList: VisibleSettings);

      internal static StringSetting AttributeStack
         = new StringSetting("AttributeStack", "_a",
         "This name is used in the generated code as the name of the attribute stack."
         + " The initial value is \"_a\"., which is defined in grammlatorRuntime.cs.",
            settingList: VisibleSettings);

      internal static BooleanSetting OptimizeStateStackNumbers
         = new BooleanSetting("OptimizeStateStackNumbers", true,
            "If this option is set to false, states push their own number on the stack. This improves "
            + " readability of the generated code but may cause less performing switch statements. "
            + " it may prohibit other optimizations performed by grammlator.",
            settingList: VisibleSettings);
            
      internal static Action<MessageTypeOrDestinationEnum, String> OutputMessage {
         get; private set;
      }

      private static void OutputToNirwana(MessageTypeOrDestinationEnum a, String s)
      {
      }
      private static void OutputToNirwana(MessageTypeOrDestinationEnum a, String s, Int32 i)
      {
      }

      internal static Action<MessageTypeOrDestinationEnum, String, Int32> OutputMessageAndPosition {
         get; private set;
      }

      /// <summary>
      ///  The number of terminal symbols is defined in phase1. It may be zero.
      /// </summary>
      internal static Int32 NumberOfTerminalSymbols {
         get; set;
      } // wird beim Erkennen des ersten nichtterminalen Symbols bestimmt

      /// <summary>
      /// The number of nonterminal symbols ist defined at the end of phase1.
      /// </summary>
      internal static Int32 NumberOfNonterminalSymbols {
         get; set;
      }

      /// <summary>
      /// The <see cref="Startsymbol"/> "*Startsymbol" with all its definitions and used symbols is the result of phase1.
      /// None of its definitions will be considered as trivial definition.
      /// Starting with it phase2 computes all states.
      /// </summary>
      internal static NonterminalSymbol Startsymbol {
         get; private set;
      }

      /// <summary>
      /// Used to get terminal symbols by its indexes
      /// </summary>
      internal static TerminalSymbol[] TerminalSymbolByIndex;

      /// <summary>
      /// Called once after phase1 is finished.
      /// Computes the array TerminalSymbolByIndex = new cTerminalesSymbol[NumberOfTerminalSymbols], 
      /// which is used to get the instance of a terminal symbol by its index (SymbolNumber+1).
      /// </summary>
      /// <param name="symbolDictionary"></param>
      internal static void DefineArrayTerminalSymbolByIndex(Dictionary<Int32, Symbol> symbolDictionary)
      {
         TerminalSymbolByIndex = new TerminalSymbol[NumberOfTerminalSymbols];
         foreach (KeyValuePair<Int32, Symbol> pair in symbolDictionary)
         {
            if (pair.Value is TerminalSymbol terminal)
               TerminalSymbolByIndex[terminal.SymbolNumber] = terminal;
         }
      }

      /// <summary>
      /// Gets a terminal symbol by its index, which is the symbols number
      /// </summary>
      /// <param name="index">Index of the terminal symbol(Symbol.Nummer-1)</param>
      /// <returns>instance of the terminal symbol</returns>
      internal static TerminalSymbol GetTerminalSymbolByIndex(Int32 index) => TerminalSymbolByIndex[index];

      /// <summary>
      /// List of all parser states, each one defined by its core items 
      /// (only the first state contains items with element number 0).
      /// </summary>
      internal static readonly List<ParserState> ListOfAllStates = new List<ParserState>(InitialCapacityOfListOfAllStates);

      internal readonly static ParserAction DefaultAction = new HaltAction(IdNumber: 0, AttributestackAdjustement: 0);
      internal readonly static BitArray EmptyBitarray = new BitArray(0);

      /// <summary>
      /// is defined after ListOfAllStates[0]; is assigned in 
      /// <see cref="P4ReplaceNonterminalsAndOptimize.MakeInstanceAndExecute"/>
      ///  and is used as root of all actions in P4 to count usage and in P5 generate the code
      /// </summary>
      internal static ParserAction Startaction {
         get; set;
      }

      /// <summary>
      /// Referenced by actions of type <see cref="ErrorhandlingAction"/>.
      /// Assigned to actions in phase 4, used in phase 5
      /// </summary>
      internal static readonly ErrorHaltAction TheOnlyOneErrorHaltAction = new ErrorHaltAction();

      /// <summary>
      /// Referenced by actions of type <see cref="HaltAction"/> and <see cref="ErrorHaltAction"/>.
      /// Assigned to actions in phase 4, used in phase 5
      /// </summary>
      internal static readonly EndOfGeneratedCodeAction TheEndOfGeneratedCodeAction = new EndOfGeneratedCodeAction();

      /// <summary>
      /// A set of terminal symbols containing all terminal symbols
      /// </summary>
      internal static BitArray AllTerminalSymbols = EmptyBitarray; // assigned in Phases1to5Controller

      /// <summary>
      /// Defined an used in phase 4, used in phase 5
      /// </summary>
      internal static readonly List<ReduceAction> ListOfAllReductions = new List<ReduceAction>(InitialCapacityOfListOfAllReductions);

      /// <summary>
      /// Defined and used in phase 4, used in phase 5
      /// </summary>
      internal static readonly List<BranchAction> ListOfAllBranchActions = new List<BranchAction>(InitialCapacityOfListOfAllBranchActions);

      /// <summary>
      /// Defined and used in phase 4, used in phase 5
      /// </summary>
      internal static readonly List<PrioritySelectAction> ListOfAllPrioritySelectActions
         = new List<PrioritySelectAction>(InitialCapacityOfListOfAllPrioritySelectActions);

      /// <summary>
      /// Defined and used in phase 4, used in phase 5
      /// </summary>
      internal static readonly List<PriorityBranchAction> ListOfAllPriorityBranchActions
         = new List<PriorityBranchAction>(InitialCapacityOfListOfAllPriorityBranchActions);

      /// <summary>
      /// Defined in phase 3, used in phase 5
      /// </summary>
      internal static readonly List<ErrorhandlingAction> ListOfAllErrorhandlingActions = new List<ErrorhandlingAction>(InitialCapacityOfListOfAllStates);

      /// <summary>
      /// <see cref="ListOfAllHaltActions"/>[0] is defined for use as initial <see cref="HaltAction"/>,
      /// all other entries are defined and used in phase 4 and used in phase 5.
      /// </summary>
      internal static readonly List<HaltAction> ListOfAllHaltActions
         = new List<HaltAction>(InitialCapacityOfListOfAllHaltActions);

      internal static readonly List<PushStateAction> ListOfAllPushStateActions
         = new List<PushStateAction>(InitialCapacityOfListOfAllPushStateActions);

      /// <summary>
      /// Defined in Phase4, used in Phase5
      /// </summary>
      internal static Int32 CountOfStatesWithStateStackNumber;

      /// <summary>
      /// used in <see cref="P5GenerateCode"/> and <see cref="HaltAction.Generate(P5CodegenCS, out Boolean)"/>
      /// </summary>
      internal static Boolean reductionsModifyAttributStack;
   } // class GlobalVariables
}
