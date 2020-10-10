using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;

using static grammlator.GlobalSettings;
using static grammlator.GlobalVariables;

namespace grammlator {
   internal static class Phases1to5 {
      /// <summary>
      /// Translates the source using <paramref name="Resultbuilder"/> to output the result, 
      /// <paramref name="outputMessage"/> and <paramref name="outputMessageAndPosition"/>
      /// to output additional information.
      /// </summary>
      /// <param name="Resultbuilder"></param>
      /// <param name="SourceReader">The source</param>
      /// <param name="outputMessage">A method to output messages </param>
      /// <param name="outputMessageAndPosition">A method to output messages with an associated poition in the source</param>
      public static void Execute(
         StringBuilder Resultbuilder,
         SpanReaderWithCharacterAndLineCounter SourceReader,
         Action<MessageTypeOrDestinationEnum, String> outputMessage,
         Action<MessageTypeOrDestinationEnum, String, Int32> outputMessageAndPosition)
          => Go(Resultbuilder, SourceReader, outputMessage, outputMessageAndPosition);

      /* TODO Grammlator:
       *
       * Compare with other implementations: https://en.wikipedia.org/wiki/Comparison_of_parser_generators
       * 
       * Allow multile grammar parts in the same file
       * 
       * Test dynamic priorities and provide access to context attributes
       * 
       * Use names for labels instead of numbers so that small grammar changes do not cause lots of changed labels
       * 
       * 
       * */

      private static void Go(
         StringBuilder Resultbuilder,
         SpanReaderWithCharacterAndLineCounter SourceReader,
         Action<MessageTypeOrDestinationEnum, String> outputMessage,
         Action<MessageTypeOrDestinationEnum, String, Int32> outputMessageAndPos)
      {
         // ----- Set initial values
         ResetGlobalVariables(outputMessage, outputMessageAndPos);
         var SymbolDictionary = new Dictionary<UnifiedString, Symbol>(1000);

         // ----- Copy input up to and including line starting with "#region" GrammarString
         Int32 StartOfMarkedLine =
            SourceReader.ReadAndCopyUntilMarkedLineFound(Resultbuilder, true, true, RegionString.Value, GrammarString.Value);
         if (StartOfMarkedLine >= 0)
         {
            OutputMessageAndPosition(MessageTypeOrDestinationEnum.Status,
                $"Found \"{RegionString} {GrammarString}\", grammlator will start translation at next line.",
                StartOfMarkedLine);
         }
         else
         {
            OutputMessageAndPosition(
               MessageTypeOrDestinationEnum.Abort,
               $"Missing \"{RegionString} {GrammarString}\".",
               SourceReader.Position);
         }

         Int32 grammarPosition = SourceReader.Position;

         // ----- Do phase 1
#if DEBUG
            outputMessage(MessageTypeOrDestinationEnum.Information, "Start of phase 1: analyse the source and check usage of symbols.");
#endif
         SymbolDictionary.Clear();
         P1aParser.MakeInstanceAndExecute(SourceReader, SymbolDictionary);

         outputMessage(MessageTypeOrDestinationEnum.AbortIfErrors, "Error(s) in phase 1: translation abandoned.");

         // Copy grammar to Resultbuilder
         SourceReader.CopyFromTo(Resultbuilder, grammarPosition, SourceReader.Position);

         // Copy part between grammar and generated string to Resultbuilder
         StartOfMarkedLine = SourceReader.ReadAndCopyUntilMarkedLineFound(Resultbuilder, true, false,
            RegionString.Value, GrammlatorString.Value, GeneratedString.Value);
         if (StartOfMarkedLine >= 0)
         {
            OutputMessageAndPosition(MessageTypeOrDestinationEnum.Status,
                 $"Found \"{RegionString} {GrammlatorString} {GeneratedString}\", the first line of generated code, which will be replaced.",
                StartOfMarkedLine);
         }
         else
         {
            OutputMessageAndPosition(MessageTypeOrDestinationEnum.Abort,
               $"Missing \"{RegionString} {GrammlatorString}\".",
               SourceReader.Position);
         }

         // Skip generated part
         StartOfMarkedLine = SourceReader.ReadAndCopyUntilMarkedLineFound(Resultbuilder, false, false,
            EndregionString.Value, GrammlatorString.Value, GeneratedString.Value);
         if (StartOfMarkedLine >= 0)
         {
            OutputMessageAndPosition(MessageTypeOrDestinationEnum.Status,
                 $"Found \"{EndregionString} {GrammlatorString} {GeneratedString}\", the last line of replaced code.",
                StartOfMarkedLine);
         }
         else
         {
            OutputMessageAndPosition(MessageTypeOrDestinationEnum.Abort,
               $"Missing \"{EndregionString} {GrammlatorString} {GeneratedString}\".",
               SourceReader.Position);
         }

         // ----- Do phase 2
#if DEBUG
         outputMessage(MessageTypeOrDestinationEnum.Information, "Start of phase 2: compute the states");
#endif

         // Prepare handling of terminal symbols             
         AllTerminalSymbols = new BitArray(NumberOfTerminalSymbols, true);
         P2ComputeLR0States.MakeInstanceAndExecute();

         // ----- Do phase 3
#if DEBUG
         outputMessage(MessageTypeOrDestinationEnum.Information, "Start of phase 3: compute follow symbols, solve conflicts, write conflicts protocol");
#endif
         P3ComputeLALR1.MakeInstanceAndExecute();
         // ----- Write protocol of symbols and states before optimizations
#if DEBUG
         outputMessage(MessageTypeOrDestinationEnum.Information, "                  write protocol of symbols and states before optimizations");
#endif
         var Protocol = new StringBuilder(5000);
         Protocol.AppendLine("List of all symbols:")
             .AppendLine();
         ProtocolSymbols(Protocol, SymbolDictionary);
         outputMessage(MessageTypeOrDestinationEnum.SymbolProtocol, Protocol.ToString());
         Protocol.Clear()
           .AppendLine("List of all states before optimizations:")
           .AppendLine();
         DisplayStates(Protocol);
         outputMessage(MessageTypeOrDestinationEnum.StateProtocol1, Protocol.ToString());
         Protocol.Clear();

         ParserState? ErrorState = ContainsStateWithoutActions();
         if (ErrorState != null)
            outputMessage(MessageTypeOrDestinationEnum.Abort,
               $"Check your grammar (endless loop?). State {ErrorState.IdNumber + 1} has no actions caused by terminal symbols."
               );

         // ----- Do phase 4
#if DEBUG

         outputMessage(MessageTypeOrDestinationEnum.Information, "Start of phase 4: optimizations");
#endif
         P4ReplaceNonterminalsAndOptimize.MakeInstanceAndExecute();

         // ----- Write protocol of states after optimizations
#if DEBUG
         outputMessage(MessageTypeOrDestinationEnum.Information,
            "                  write protocol of states after optimizations");
#endif
         Protocol.AppendLine("List of all states after optimizations:")
           .AppendLine();
         DisplayStates(Protocol); // dazu evtl. die Items der Zustände erweitern ???
         outputMessage(MessageTypeOrDestinationEnum.StateProtocol2, Protocol.ToString());
         Protocol.Clear();

         ErrorState = ContainsStateWithoutActions();
         if (ErrorState != null)
            outputMessage(MessageTypeOrDestinationEnum.Abort,
               $"Check your grammar (endless loop?). State {ErrorState.IdNumber + 1} has no actions caused by terminal symbols."
               );
         Protocol.Capacity = 0;

         // ----- Do phase 5 with a specific code generator
#if DEBUG
         outputMessage(MessageTypeOrDestinationEnum.Information, "Start of phase 5: generate result");
#endif

         // Construct codegen and generate code
         P5GenerateCode.MakeInstanceAndExecute(new P5CodegenCS(Resultbuilder));

         // ----- Copy trailing lines of source to result
#if DEBUG
         outputMessage(MessageTypeOrDestinationEnum.Information,
            "                  copy trailing lines of source to result");
#endif
         SourceReader.ReadAndCopyToEnd(Resultbuilder);

         // ----- Translation finished
         outputMessage(MessageTypeOrDestinationEnum.Information,
             "Done. Don't forget to check the messages and the protocol of the conflicts.");
      }

      public static void ProtocolSymbols(StringBuilder sb, Dictionary<UnifiedString, Symbol> symbolDictionary)
      {
         foreach (KeyValuePair<UnifiedString, Symbol> p in symbolDictionary)
         {
            Symbol Symbol = p.Value;

            if (Symbol != null)
               Symbol.Append(sb);
            else
               sb.Append(p.Key.ToString()).Append(" is not a symbol");
            sb.AppendLine();

            if (Symbol != null && Symbol.SymbolNumber == NumberOfTerminalSymbols - 1)
               sb.AppendLine("---"); // delimiting line between terminal symbols and nonterminal symbols
         }
      }

      public static Boolean DisplayStates(StringBuilder sb)
      {
         Boolean foundError = false;
         foreach (ParserState state in ListOfAllStates)
         {
            if (state.Actions.Count == 0)
               foundError = true;
            state.AppendToSB(sb);
         }
         return foundError;
      }

      public static ParserState? ContainsStateWithoutActions()
      {
         foreach (ParserState state in ListOfAllStates)
         {
            if (state.Actions.Count == 0)
               return state;
         }
         return null;
      }
   }
}
