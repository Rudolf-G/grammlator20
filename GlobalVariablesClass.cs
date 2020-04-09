﻿using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Text;

namespace Grammlator {
   internal static class InitialSettings {
      static readonly Dictionary<string, string> InitialValues = new Dictionary<string, string>();
      static InitialSettings() {
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
         InitialValues.Add("InstructionAcceptSymbol", "AcceptSymbol");
         InitialValues.Add("InstructionAssignSymbol", "");
         InitialValues.Add("InstructionErrorHalt", "");
         InitialValues.Add("NewLineConstant", "\\r\\n");
         InitialValues.Add("PrefixStateDescription", "");
         InitialValues.Add("RegionString", "#region");
         InitialValues.Add("StateStack", "_s");
         InitialValues.Add("TerminalSymbolEnum", "");
         InitialValues.Add("VariableAttributeStackInitialCount", "AttributeStackInitialCount");
         InitialValues.Add("VariableErrorStateNumber", "ErrorStateNumber");
         InitialValues.Add("VariableSymbol", "PeekSymbol()");
         InitialValues.Add("VariableStateStackInitialCount", "StateStackInitialCount");
      }

      public static String GetString(String name) => InitialValues[name]; // TODO check not dound
      public static Int32 GetInt(String name) => Int32.Parse(GetString(name)); // TODO check conversion error
   }

   internal static class GlobalVariables {
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

      public static void ResetGlobalVariables(
                Action<MessageTypeOrDestinationEnum, String> OutputMessage,
                Action<MessageTypeOrDestinationEnum, String, STextPosition> OutputPositionAndMessage) {
         // Reset all static variables 
         /* Reset initial values of all variables which can be modified by grammlator settings in P1Parser */

         // TODO Settings To core ???
         //IfToSwitchBorder = InitialSettings.GetInt("IfToSwitchBorder");
         //VariableNameStateDescription = InitialSettings.GetString("VariablePrefixStateDescription");
         //VariableNameSymbol = InitialSettings.GetString("VariableNameSymbol");
         //TerminalSymbolEnum = InitialSettings.GetString("TerminalSymbolEnum");
         //InstructionAssignSymbol = InitialSettings.GetString("InstructionAssignSymbol");
         //InstructionAcceptSymbol = InitialSettings.GetString("InstructionAcceptSymbol");
         //InstructionErrorHalt = InitialSettings.GetString("InstructionErrorHalt");
         //ErrorHandlerMethod = InitialSettings.GetString("ErrorHandlerMethod");
         //StateStackInitialCountVariable = InitialSettings.GetString("VariableStateStackInitialCount");
         //StateStack = InitialSettings.GetString("StateStack");
         //AttributeStack = InitialSettings.GetString("AttributeStack");
         //AttributeStackInitialCountVariable = InitialSettings.GetString("VariableAttributeStackInitialCount");

         GlobalVariables.OutputMessage = OutputMessage;
         GlobalVariables.OutputPositionAndMessage = OutputPositionAndMessage;
         NumberOfTerminalSymbols = 0;
         NumberOfNonterminalSymbols = 0;
         Startsymbol = new NonterminalSymbol("*Startsymbol") {
            SymbolNumber = 0,
            TrivalDefinitionsArray = Array.Empty<Symbol>(), // the startsymbol will not have any trival definitions
            NontrivalDefinitionsList = null, // will be set by after the startsymbol is recognized in the source
            AttributetypeList = Array.Empty<String>(),
            AttributenameList = Array.Empty<String>()
         };

         Startaction = null;
         TerminalSymbolByIndex = null;
         AllTerminalSymbols = null;
         ListOfAllStates.Clear();
         ListOfAllStates.Capacity = InitialCapacityOfListOfAllStates;
         ListOfAllErrorhandlingActions.Clear();
         ListOfAllErrorhandlingActions.Capacity = InitialCapacityOfListOfAllStates;
         ListOfAllReductions.Clear();
         ListOfAllReductions.Capacity = InitialCapacityOfListOfAllReductions;
         ListOfAllBranchActions.Clear();
         ListOfAllBranchActions.Capacity = InitialCapacityOfListOfAllBranchActions;
         ListOfAllHaltActions.Clear();
         ListOfAllHaltActions.Capacity = InitialCapacityOfListOfAllHaltActions;
         ListOfAllHaltActions.Add(new HaltAction(IdNumber: 0, AttributestackAdjustement: 0));
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

      /// <summary>
      /// <see cref="ErrorHandlerMethod"/> is used to generate code
      /// </summary>
      internal static string ErrorHandlerMethod = InitialSettings.GetString("ErrorHandlerMethod");

      /// <summary>
      /// ErrorHandlerIsDefined => !string.IsNullOrEmpty(ErrorHandlerMethod);
      /// </summary>
      internal static bool ErrorHandlerIsDefined => !string.IsNullOrEmpty(ErrorHandlerMethod);

      internal static string StateStackInitialCountVariable = InitialSettings.GetString("VariableStateStackInitialCount");

      internal static string StateStack = InitialSettings.GetString("StateStack");

      internal static string AttributeStackInitialCountVariable = InitialSettings.GetString("VariableAttributeStackInitialCount");

      internal static string AttributeStack = InitialSettings.GetString("AttributeStack");

      /// <summary>
      /// These strings are used to construct labels in the generated program.
      /// They are indexed by <see cref="ParserActionEnum"/>.
      /// Some of these will never occur in labels. They are provided for future modifications.
      /// </summary>
      internal static readonly string[] LabelPrefixes = new string[]{
            "ApplyDefinition", "State", "LookAhead", "Reduce",
            "ApplyStartsymbolDefinition", "EndWithError",
            "TerminalTransition", "Accept", "NonterminalTransition",
            "Branch", "PrioritySelect", "HandleError", "Deleted",
            "EndOfGeneratedCode",
            "Label"  // Unknown
            };

      internal static Action<MessageTypeOrDestinationEnum, String> OutputMessage {
         get; private set;
      }

      internal static Action<MessageTypeOrDestinationEnum, String, STextPosition> OutputPositionAndMessage {
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
      internal static void DefineArrayTerminalSymbolByIndex(Dictionary<String, Symbol> symbolDictionary) {
         TerminalSymbolByIndex = new TerminalSymbol[NumberOfTerminalSymbols];
         foreach (KeyValuePair<String, Symbol> pair in symbolDictionary) {
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

      /// <summary>
      /// is assigned in phase 4 and used as root of all actions in phase5 to count usage and generate the code 
      /// </summary>
      internal static ParserAction Startaction;

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
      internal static BitArray AllTerminalSymbols; // assigned in Phases1to5Controller

      /// <summary>
      /// Defined in phase 3, used in phase 5
      /// </summary>
      internal static readonly List<ErrorhandlingAction> ListOfAllErrorhandlingActions = new List<ErrorhandlingAction>(InitialCapacityOfListOfAllStates);

      /// <summary>
      /// Defined an used in phase 4, used in phase 5
      /// </summary>
      internal static readonly List<ReduceAction> ListOfAllReductions = new List<ReduceAction>(InitialCapacityOfListOfAllReductions);

      /// <summary>
      /// Defined and used in phase 4, used in phase 5
      /// </summary>
      internal static readonly List<BranchAction> ListOfAllBranchActions = new List<BranchAction>(InitialCapacityOfListOfAllBranchActions);

      /// <summary>
      /// <see cref="ListOfAllHaltActions"/>[0] is defined for use as standard <see cref="HaltAction"/>,
      /// all other entries are defined and used in phase 4 and used in phase 5.
      /// </summary>
      internal static readonly List<HaltAction> ListOfAllHaltActions = new List<HaltAction>(InitialCapacityOfListOfAllHaltActions);

      /// <summary>
      /// berechnet in Phase4, verwendet in Phase5
      /// </summary>
      internal static Int32 CountOfStatesWithStateStackNumber;
   } // class GlobalDeclarations
}
