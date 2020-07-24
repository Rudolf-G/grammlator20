using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Text;

namespace Grammlator {
   internal static class InitialSettings {
      static readonly Dictionary<string, string> InitialValues = new Dictionary<string, string>(40);
      static InitialSettings()
         {
         InitialValues.Add("AttributeStack", "_a");
         InitialValues.Add("CSharpCommentlineMarker", "//");
         InitialValues.Add("CSharpPragmaMarker", "#pragma");
         InitialValues.Add("EndregionString", "#endregion");
         InitialValues.Add("ErrorHandlerMethod", "");
         InitialValues.Add("GeneratedString", "generated");
         InitialValues.Add("GrammarlineMarker", "//|");
         InitialValues.Add("GrammarString", "grammar");
         InitialValues.Add("GrammlatorString", "grammlator");
         InitialValues.Add("IfToSwitchBorder", "5");
         InitialValues.Add("InstructionAcceptSymbol", "AcceptSymbol();");
         InitialValues.Add("InstructionAssignSymbol", "");
         InitialValues.Add("InstructionErrorHalt", "");
         InitialValues.Add("NestingLevelLimit", "5");
         InitialValues.Add("LineLengthLimit", "120");
         InitialValues.Add("NewLineConstant", "\\r\\n");
         InitialValues.Add("PrefixStateDescription", "");
         InitialValues.Add("RegionString", "#region");
         InitialValues.Add("StateStack", "_s");
         InitialValues.Add("TerminalSymbolEnum", "");
         InitialValues.Add("VariableAttributeStackInitialCount", "AttributeStackInitialCount");
         InitialValues.Add("VariableErrorStateNumber", "ErrorStateNumber");
         InitialValues.Add("VariableSymbol", "PeekSymbol()");
         InitialValues.Add("VariableStateStackInitialCount", "StateStackInitialCount");
         InitialValues.Add("MethodIndexOfMaximum", "Method.IndexOfMaximum");
         }

      public static String GetString(String name) => InitialValues[name]; // TODO check not found
      public static Int32 GetInt(String name)
         {
         if (Int32.TryParse(GetString(name), out Int32 result))
            return result; // TODO check conversion error
         throw new ErrorInSourcedataException($"{name} is not a number");
         }
      }

   class MemoryComparer: IEqualityComparer<ReadOnlyMemory<char>> {
      public bool Equals(ReadOnlyMemory<char> rom1, ReadOnlyMemory<char> rom2)
         => rom1.Span.SequenceEqual(rom2.Span);

      public int GetHashCode(ReadOnlyMemory<char> rom)
         => string.GetHashCode(rom.Span);
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
         }
      private static String GetVersioninfo {
         get {

            Assembly ThisAssembly = typeof(GlobalVariables).Assembly;
            AssemblyName AssemblyName = ThisAssembly.GetName();
            String AssemblyFullPath = ThisAssembly.Location;
            FileVersionInfo fvi = FileVersionInfo.GetVersionInfo(AssemblyFullPath);
            string FileVersion = fvi.FileVersion;
            string FileWrittenDate = System.IO.File.GetLastWriteTime(AssemblyFullPath).ToString();

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

      static readonly Dictionary<ReadOnlyMemory<char>, Int32> MemoryToIndexDictionary
         = new Dictionary<ReadOnlyMemory<char>, Int32>(1000, new MemoryComparer());

      static readonly List<string> IndexToString = new List<string>(1000);

      static public Int32 GetIndexOfString(ReadOnlyMemory<char> MemorySpan)
         {
         if (MemoryToIndexDictionary.TryGetValue(MemorySpan, out int result))
            return result;

         String s = MemorySpan.ToString(); // allocation of string which usually will be used for output later
         ReadOnlyMemory<char> newSpan = s.AsMemory(); // keep bytes referenced from dictionary together:  (don't use reference to source)

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
         ReadOnlyMemory<char> m = s.AsMemory();
         if (MemoryToIndexDictionary.TryGetValue(m, out int result))
            return result;

         IndexToString.Add(s); // string is alrady available
         MemoryToIndexDictionary.Add(m, IndexToString.Count - 1);
         return IndexToString.Count - 1;
         }

      static public Int32 GetIndexOfEmptyString()
         {
         return GetIndexOfString(ReadOnlyMemory<char>.Empty);
         }

      static public string GetStringOfIndex(Int32 i) => IndexToString[i];

      public static void ResetGlobalVariables(
                Action<MessageTypeOrDestinationEnum, String> OutputMessage,
                Action<MessageTypeOrDestinationEnum, String, Int32> outputMessageAndPosition)
         {
         // Reset all static variables 

         /* Reset initial values of all variables which can be modified by grammlator settings in P1Parser */
         // TODO there remain global variables which the user should be allowed to set
         AttributeStack = InitialSettings.GetString("AttributeStack");
         // "CSharpCommentlineMarker"
         // "CSharpPragmaMarker"
         // "EndregionString"
         ErrorHandlerMethod = InitialSettings.GetString("ErrorHandlerMethod");
         // "GeneratedString"
         // "GrammarlineMarker"
         // "GrammarString"
         // "GrammlatorString"
         IfToSwitchBorder = InitialSettings.GetInt("IfToSwitchBorder");
         InstructionAcceptSymbol = InitialSettings.GetString("InstructionAcceptSymbol");
         InstructionAssignSymbol = InitialSettings.GetString("InstructionAssignSymbol");
         InstructionErrorHalt = InitialSettings.GetString("InstructionErrorHalt");
         // "NewLineConstant"
         LineLengthLimit = InitialSettings.GetInt("LineLengthLimit");
         NestingLevelLimit = InitialSettings.GetInt("NestingLevelLimit");
         VariableNameStateDescription = InitialSettings.GetString("PrefixStateDescription");
         // "RegionString"
         StateStack = InitialSettings.GetString("StateStack");
         TerminalSymbolEnum = InitialSettings.GetString("TerminalSymbolEnum");
         AttributeStackInitialCountVariable = InitialSettings.GetString("VariableAttributeStackInitialCount");
         // "VariableErrorStateNumber"
         VariableNameSymbol = InitialSettings.GetString("VariableSymbol");
         StateStackInitialCountVariable = InitialSettings.GetString("VariableStateStackInitialCount");
         MethodIndexOfMaximum = InitialSettings.GetString("MethodIndexOfMaximum");

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

         Startaction = ListOfAllHaltActions[0]; // 
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

      /* Grammlator parametrization */

      /// <summary>
      /// e.g. "#region" (not including the apostrophes)
      /// </summary>
      internal static readonly String RegionString = InitialSettings.GetString("RegionString");

      /// <summary>
      /// e.g. "#endregion"  (not including the apostrophes)
      /// </summary>
      internal static readonly String EndregionString = InitialSettings.GetString("EndregionString");

      /// <summary>
      /// e.g. "grammar"  (not including the apostrophes)
      /// </summary>
      internal static readonly String GrammarString = InitialSettings.GetString("GrammarString");

      /// <summary>
      /// e.g. "grammlator"  (not including the apostrophes)
      /// </summary>
      internal static readonly String GrammlatorString = InitialSettings.GetString("GrammlatorString");

      /// <summary>
      /// e.g. "generated"  (not including the apostrophes)
      /// </summary>
      internal static readonly String GeneratedString = InitialSettings.GetString("GeneratedString");

      /// <summary>
      /// <see cref="NewLineWithEscapes"/> is defined by <see cref="Settings.NewLineConstant"/>
      /// and will typically be "\\r\\n"
      /// (unlike <see cref="System.Environment.NewLine"/> typically "\r\n").
      /// </summary>
      internal static readonly string NewLineWithEscapes = InitialSettings.GetString("NewLineConstant");

      /// <summary>
      /// <see cref="ConditionalAction"/>s with complexity &lt;= <see cref="IfToSwitchBorder"/> are generated as if instruction sequence, others as switch statement
      /// </summary>
      internal static Int32 IfToSwitchBorder = InitialSettings.GetInt("IfToSwitchBorder");

      /// <summary>
      /// <see cref="VariableNameStateDescription"/> is used to generate code
      /// </summary>
      internal static string VariableNameStateDescription = InitialSettings.GetString("PrefixStateDescription");

      /// <summary>
      /// <see cref="VariableNameSymbol"/> is used to generate code
      /// </summary>
      internal static string VariableNameSymbol = InitialSettings.GetString("VariableSymbol");

      /// <summary>
      /// <see cref="TerminalSymbolEnum"/> (e.g. "LexerResult") is used to generated code
      ///  (e.g. "if (Symbol != LexerResult.Name)...;"
      /// </summary>
      internal static string TerminalSymbolEnum = InitialSettings.GetString("TerminalSymbolEnum");

      /// <summary>
      /// <see cref="InstructionAssignSymbol"/> is used to generate code
      /// </summary>
      internal static string InstructionAssignSymbol = InitialSettings.GetString("InstructionAssignSymbol");

      /// <summary>
      /// <see cref="InstructionAcceptSymbol"/> is used to generate code
      /// </summary>
      internal static string InstructionAcceptSymbol = InitialSettings.GetString("InstructionAcceptSymbol");

      /// <summary>
      /// <see cref="InstructionErrorHalt"/> is used to generate code
      /// </summary>
      internal static string InstructionErrorHalt = InitialSettings.GetString("InstructionErrorHalt");

      internal static Int32 NestingLevelLimit = InitialSettings.GetInt("NestingLevelLimit");
      internal static Int32 LineLengthLimit = InitialSettings.GetInt("LineLengthLimit");

      /// <summary>
      /// <see cref="ErrorHandlerMethod"/> is used to generate code
      /// </summary>
      internal static string ErrorHandlerMethod = InitialSettings.GetString("ErrorHandlerMethod");

      /// <summary>
      /// ErrorHandlerIsDefined => !string.IsNullOrEmpty(ErrorHandlerMethod);
      /// </summary>
      internal static bool ErrorHandlerIsDefined => !string.IsNullOrEmpty(ErrorHandlerMethod);

      internal static string MethodIndexOfMaximum = InitialSettings.GetString("MethodIndexOfMaximum");

      internal static string StateStackInitialCountVariable = InitialSettings.GetString("VariableStateStackInitialCount");

      internal static string StateStack = InitialSettings.GetString("StateStack");

      internal static string AttributeStackInitialCountVariable = InitialSettings.GetString("VariableAttributeStackInitialCount");

      internal static string AttributeStack = InitialSettings.GetString("AttributeStack");

      internal static Action<MessageTypeOrDestinationEnum, String> OutputMessage {
         get; private set;
         }

      private static void  OutputToNirwana(MessageTypeOrDestinationEnum a, String s) { }
      private static void OutputToNirwana(MessageTypeOrDestinationEnum a, String s, Int32 i) { }

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
      /// is assigned in phase 4 and used as root of all actions in phase5 to count usage and generate the code 
      /// </summary>
      internal static ParserAction Startaction = DefaultAction;

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
      } // class GlobalVariables
   }
