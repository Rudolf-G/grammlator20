using BitsNamespace;

using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;

using static grammlator.GlobalSettings;
using static grammlator.GlobalVariables;

namespace grammlator;

internal static class Phases1to5
{
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
      Action<MessageTypeOrDestinationEnum, String, Int32> outputMessageAndPosition,
      out Int32 sumOfConflictsNotSolvedByExplicitPriority)
       => Go(Resultbuilder, SourceReader, outputMessage, outputMessageAndPosition,
             out sumOfConflictsNotSolvedByExplicitPriority);

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
      Action<MessageTypeOrDestinationEnum, String, Int32> outputMessageAndPos,
      out Int32 sumOfConflictsNotSolvedByExplicitPriority
      )
   {
      // ----- Set initial values
      ResetGlobalVariables(outputMessage, outputMessageAndPos);
      var SymbolDictionary = new Dictionary<UnifiedString, Symbol>(1000);

      // ----- Copy input up to and including line starting with "#region grammar"
      Int32 StartOfGrammlatorLines =
         SourceReader.ReadAndCopyUntilMarkedLineFound(Resultbuilder, copy: true, copyLineWithMarkers: true,
            RegionBegin.Value, RegionGrammarMarker.Value);
      if (StartOfGrammlatorLines >= 0)
      {
         Int32 StartOfRegion = StartOfGrammlatorLines
            + SourceReader.Source.Span[StartOfGrammlatorLines..].IndexOf('#');
         OutputMessageAndPosition(MessageTypeOrDestinationEnum.Status,
             $"Found \"{RegionBegin} {RegionGrammarMarker}\"",
             StartOfRegion + 1);
      }
      else
      {
         OutputMessageAndPosition(
            MessageTypeOrDestinationEnum.Abort,
            $"Missing \"{RegionBegin} {RegionGrammarMarker}\"",
            SourceReader.Position);
      }

      Int32 grammarPosition = SourceReader.Position;

      // ----- Do phase 1
#if DEBUG
      outputMessage(MessageTypeOrDestinationEnum.Information, "Start of phase 1: analyse the source and check usage of symbols.");
#endif
      SymbolDictionary.Clear();

      /*************** Call the parser ****************/
      Int32 StartOfGeneratedCode =
         P1aParser.MakeInstanceAndExecute(SourceReader, SymbolDictionary);

      // The parser outputs the message "Found #region grammlator generated"
      // before checking the symbols (which may cause additional messages)

      outputMessage(MessageTypeOrDestinationEnum.AbortIfErrors, "Error(s) in phase 1: translation abandoned.");

      if (StartOfGeneratedCode < 0)
         OutputMessageAndPosition(MessageTypeOrDestinationEnum.Abort,
            $"Missing \"{RegionBegin} {RegionGrammlatorMarker}\"",
            StartOfGeneratedCode);

      // Copy grammar to Resultbuilder
      SourceReader.CopyFromTo(Resultbuilder,
         from: grammarPosition,
         to: StartOfGeneratedCode
         );

      // Skip generated part
      Int32 EndOfGeneratedCode = SourceReader.ReadAndCopyUntilMarkedLineFound(
         Resultbuilder, copy: false, copyLineWithMarkers: false, RegionEnd.Value, RegionGrammlatorMarker.Value,
         RegionGeneratedMarker.Value);
      if (EndOfGeneratedCode >= 0)
      {
         Int32 StartOfEndRegion = EndOfGeneratedCode
            + SourceReader.Source.Span[EndOfGeneratedCode..].IndexOf('#');

         OutputMessageAndPosition(MessageTypeOrDestinationEnum.Status,
              $"Found \"{RegionEnd} {RegionGrammlatorMarker} {RegionGeneratedMarker}\"",
             StartOfEndRegion + 1);
      }
      else
      {
         OutputMessageAndPosition(MessageTypeOrDestinationEnum.Abort,
            $"Missing \"{RegionEnd} {RegionGrammlatorMarker} {RegionGeneratedMarker}\"",
            SourceReader.Position);
      }

      // ----- Do phase 2
#if DEBUG
      outputMessage(MessageTypeOrDestinationEnum.Information, "Start of phase 2: compute the states");
#endif

      // Prepare handling of terminal symbols             
      AllTerminalSymbols = new Bits(NumberOfTerminalSymbols).SetAll(true);
      P2ComputeLR0States.MakeInstanceAndExecute();

      // ----- Do phase 3
#if DEBUG
      outputMessage(MessageTypeOrDestinationEnum.Information,
         "Start of phase 3: compute follow symbols, solve conflicts, write conflicts protocol");
#endif
      P3ComputeLALR1.MakeInstanceAndExecute(out sumOfConflictsNotSolvedByExplicitPriority);

      // ----- Write protocol of symbols and states before optimizations
#if DEBUG
      outputMessage(MessageTypeOrDestinationEnum.Information,
         "                  write protocol of symbols and states before optimizations");
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
