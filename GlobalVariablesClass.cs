using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using System.Text;

using Microsoft.Extensions.ObjectPool;

namespace grammlator {

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
            attributetypeStringList: Array.Empty<UnifiedString>(),
            attributenameStringList: Array.Empty<UnifiedString>()
            );
         TerminalSymbols = Array.Empty<TerminalSymbol>();
         OutputMessage = OutputToNirwana;
         OutputMessageAndPosition = OutputToNirwana;
         Startaction = new DeletedParserAction();
      }

      static public String AssemblyFullPath { get; private set; } = "";
      internal static String GetVersioninfo {
         get {

            Assembly ThisAssembly = typeof(GlobalVariables).Assembly;
            AssemblyName AssemblyName = ThisAssembly.GetName();
            AssemblyFullPath = ThisAssembly.Location;
            FileVersionInfo fvi = FileVersionInfo.GetVersionInfo(AssemblyFullPath)!;
            String FileVersion = fvi.FileVersion!;
            String FileWrittenDate =
               System.IO.File.GetLastWriteTime(AssemblyFullPath)
               .ToString("d MMM yyyy");

            /* Concept:
             * FileVersion is incremented using year / month / day
             * Version is incremented only to year / version to reflect major changes. */

            return new StringBuilder(100)
               .Append(" (")
               .Append(AssemblyName.Name)
               .Append(" file version/date ")
               .Append(FileVersion)
               .Append("/")
               .Append(FileWrittenDate)
               .Append(")")
               .ToString();
         }
      }

      private static readonly String VersionInfo = GetVersioninfo;
      internal static String TranslationInfo => DateTime.Now.ToString("d MMM yyyy") + VersionInfo;

      internal const Int32 InitialCapacityOfListOfAllStates = 1000;
      internal const Int32 InitialCapacityOfListOfAllReductions = 400;
      internal const Int32 InitialCapacityOfListOfAllBranchActions = 200;
      internal const Int32 InitialCapacityOfListOfAllHaltActions = 20;
      internal const Int32 InitialCapacityOfListOfAllPushStateActions = 20;
      internal const Int32 InitialCapacityOfListOfAllPrioritySelectActions = 20;
      internal const Int32 InitialCapacityOfListOfAllPriorityBranchActions
         = InitialCapacityOfListOfAllPrioritySelectActions;

      public static void ResetGlobalVariables(
                Action<MessageTypeOrDestinationEnum, String> outputMessage,
                Action<MessageTypeOrDestinationEnum, String, Int32> outputMessageAndPosition)
      {
         // Reset all settings
         GlobalSettings.Reset();

         UnifiedString.Reset();

         // Reset all static variables 
         NumberOfActions = 0;
         OutputMessage = outputMessage;
         OutputMessageAndPosition = outputMessageAndPosition;
         NumberOfTerminalSymbols = 0;
         NumberOfNonterminalSymbols = 0;
         Startsymbol = new NonterminalSymbol("*Startsymbol",
            position: 0,
            symbolNumber: 0,
            attributetypeStringList: Array.Empty<UnifiedString>(),
            attributenameStringList: Array.Empty<UnifiedString>()
            // trivalDefinitionsArray: Array.Empty<Symbol>(), // the startsymbol will not have any trival definitions: default
            // nontrivalDefinitionsList: ... // will be set after the startsymbol: default (empty list)
            );


         ListOfAllHaltActions.Clear();
         ListOfAllHaltActions.Capacity = InitialCapacityOfListOfAllHaltActions;
         ListOfAllHaltActions.Add(new HaltAction(IdNumber: 0, AttributestackAdjustement: 0));

         TerminalSymbols = Array.Empty<TerminalSymbol>();
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

      // Number of all terminal and nonterminal transitions and lookahead actions generated in P2 and in P4
      internal static Int32 NumberOfActions;

      /// <summary>
      /// Defined in Phase4, used in Phase5
      /// </summary>
      internal static Int32 NumberOfStatesWithStateStackNumber;

      /// <summary>
      /// The <see cref="Startsymbol"/> "*Startsymbol" with all its definitions and used symbols is the result of phase1.
      /// None of its definitions will be considered as trivial definition.
      /// Starting with it phase2 computes all states.
      /// </summary>
      internal static NonterminalSymbol Startsymbol {
         get; private set;
      }

      /// <summary>
      /// Used to get terminal symbols by its SymbolNumber
      /// </summary>
      internal static TerminalSymbol[] TerminalSymbols;

      /// <summary>
      /// Assigned and used in P5. Is set if the terminal symbols can be used as flags else is reset.
      /// </summary>
      internal static Boolean TerminalSymbolsAreFlags = false;

      /// <summary>
      /// Called once after phase1 is finished.
      /// Computes the array TerminalSymbolByIndex = new cTerminalesSymbol[NumberOfTerminalSymbols], 
      /// which is used to get the instance of a terminal symbol by its index (SymbolNumber+1).
      /// </summary>
      /// <param name="symbolDictionary"></param>
      internal static void DefineArrayTerminalSymbolByIndex(Dictionary<UnifiedString, Symbol> symbolDictionary)
      {
         TerminalSymbols = new TerminalSymbol[NumberOfTerminalSymbols];
         Debug.Assert(symbolDictionary.Count == NumberOfTerminalSymbols);
         foreach (KeyValuePair<UnifiedString, Symbol> pair in symbolDictionary)
         {
            if (pair.Value is TerminalSymbol terminal)
               TerminalSymbols[terminal.SymbolNumber] = terminal;
         }
#if DEBUG
         for (int i=1; i<TerminalSymbols.Length; i++)
         {
            Debug.Assert(TerminalSymbols[i].EnumValue > TerminalSymbols[i - 1].EnumValue);
         }
#endif
      }

      /// <summary>
      /// Gets a terminal symbol by its index, which is the symbols number
      /// </summary>
      /// <param name="index">Index of the terminal symbol(Symbol.Nummer-1)</param>
      /// <returns>instance of the terminal symbol</returns>
      internal static TerminalSymbol GetTerminalSymbolByIndex(Int32 index) => TerminalSymbols[index];

      internal readonly static BitArray EmptyBitarray = new BitArray(0);

      /// <summary>
      /// A set of terminal symbols containing all terminal symbols
      /// </summary>
      internal static BitArray AllTerminalSymbols = EmptyBitarray; // assigned in Phases1to5Controller

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
      /// List of all parser states, each one defined by its core items 
      /// (only the first state contains items with element number 0).
      /// </summary>
      internal static readonly List<ParserState> ListOfAllStates = new List<ParserState>(InitialCapacityOfListOfAllStates);

      /// <summary>
      /// Defined and used in phase 4, used in phase 5
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
      /// used in <see cref="P5GenerateCode"/> and <see cref="HaltAction.Generate(P5CodegenCS, out Boolean)"/>
      /// </summary>
      internal static Boolean reductionsModifyAttributStack;

      static readonly DefaultObjectPoolProvider objectPoolProvider = new DefaultObjectPoolProvider();
      internal static ObjectPool<StringBuilder> stringBuilderPool = objectPoolProvider.CreateStringBuilderPool();
   } // class GlobalVariables
}
