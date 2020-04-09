using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using static Grammlator.GlobalVariables;

namespace Grammlator {
   internal static class Phases1to5 {
      /// <summary>
      /// Translates the source using outputMethod to output the result, 
      /// outputMessage and outputMessageAndPos to output additional information.
      /// Optional trace instructions may be inserted in the result.
      /// </summary>
      /// <param name="Resultbuilder"></param>
      /// <param name="SourceReader">The source</param>
      /// <param name="outputMessage">A method to output messages </param>
      /// <param name="outputPositionAndMessage">A method to output messages with an associated poition in the source</param>
      public static void Execute(
         StringBuilder Resultbuilder,
         SpanReaderWithCharacterAndLineCounter SourceReader,
         Action<MessageTypeOrDestinationEnum, String> outputMessage,
         Action<MessageTypeOrDestinationEnum, String, STextPosition> outputPositionAndMessage)
          => Go(Resultbuilder, SourceReader, outputMessage, outputPositionAndMessage);

      /* TODO bei Grammlator:
       *
       * compare with other implementations: https://en.wikipedia.org/wiki/Comparison_of_parser_generators
       * statische Lösung von Konflikten: vorher evtl. einfache Optimierungen?
       * 
       * dynamisch bedingte Prioritäten klären: 
       *   - int Methode vorsehen, Kontextabfrage klären (vorerst händisch einprogrammiert)
       *   - in statischer Lösung von Konflikten berücksichtigen, Prüfen auf "unbedingte" default-Aktion
       *   - Kontextabfrage klären (vorerst händisch einprogrammiert)
       *   - Codegenerierung klären (switch maxindex(...,...,...,..)
       * empty:. definieren in STEP1 (zu besseren Lesbarkeit) . Konsequenzen?
       *     oder simpleEmptyAlternative besonders behandeln ???
       * Konzept für Präfixe klären
       * Terminale Symbole und enum abgleichen
       * Startsymbol definieren durch Auflistung von Symbolen mit Attributen + enum
       *    Attributbehandlung bei den Startsymbolen klären
       *        
       * Konzept für mehrere Gruppen terminaler Symbole klären 
       *    (verschiedene Herkunft: interne Numerierung ab ...? zulässige Eingabesymbole bei Konflikterkennung und If-Generierung jeweils begrenzt)
       * 
       * */

      private static void Go(
         StringBuilder Resultbuilder,
         SpanReaderWithCharacterAndLineCounter SourceReader,
         Action<MessageTypeOrDestinationEnum, String> outputMessage,
         Action<MessageTypeOrDestinationEnum, String, STextPosition> outputMessageAndPos) {

         // ----- Set initial values
         ResetGlobalVariables(outputMessage, outputMessageAndPos);
         var SymbolDictionary = new Dictionary<String, Symbol>(1000);

         // ----- Copy input up to and including line starting with "#region" GrammarString
         Int32 StartOfMarkedLine = SourceReader.ReadAndCopyUntilMarkedLineFound(Resultbuilder, true, true, RegionString, GrammarString);
         if (StartOfMarkedLine >= 0) {
            OutputPositionAndMessage(MessageTypeOrDestinationEnum.Status,
                $"Found \"{RegionString} {GrammarString}\", grammlator will start translation at next line.",
                new STextPosition(lineNumber: SourceReader.LineNumber, columnNumber: 0, position: StartOfMarkedLine));
            ;
            ;
         }
         else {
            OutputPositionAndMessage(
               MessageTypeOrDestinationEnum.Abort,
               $"Missing \"{RegionString} {GrammarString}\".",
               new STextPosition(SourceReader.LineNumber, columnNumber: 0, SourceReader.Position));
            ;
         }

         int grammarPosition = SourceReader.Position;

         // ----- Do phase 1
         outputMessage(MessageTypeOrDestinationEnum.Information, "Start of phase 1: analyse the source and check usage of symbols.");

         P1aParser.MakeInstanceAndExecute(SourceReader, SymbolDictionary);

         outputMessage(MessageTypeOrDestinationEnum.AbortIfErrors, "Error(s) in phase 1: translation abandoned.");

         // Copy grammar to Resultbuilder
         SourceReader.CopyFromTo(Resultbuilder, grammarPosition, SourceReader.Position);

         // Copy part between grammar and generated string to Resultbuilder
         StartOfMarkedLine = SourceReader.ReadAndCopyUntilMarkedLineFound(Resultbuilder, true, false, RegionString, GrammlatorString, GeneratedString);
         if (StartOfMarkedLine >= 0) {
            OutputPositionAndMessage(MessageTypeOrDestinationEnum.Status,
                 $"Found \"{RegionString} {GrammlatorString} {GeneratedString}\", the first line of generated code, which will be replaced.",
                new STextPosition(lineNumber: SourceReader.LineNumber, columnNumber: 0, position: StartOfMarkedLine));
         }
         else {
            OutputPositionAndMessage(MessageTypeOrDestinationEnum.Abort,
               $"Missing \"{RegionString} {GrammlatorString}\".",
                new STextPosition(lineNumber: SourceReader.LineNumber, columnNumber: 0, position: SourceReader.Position));
         }

         // Skip generated part
         StartOfMarkedLine = SourceReader.ReadAndCopyUntilMarkedLineFound(Resultbuilder, false, false, EndregionString, GrammlatorString, GeneratedString);
         if (StartOfMarkedLine>=0) {
            OutputPositionAndMessage(MessageTypeOrDestinationEnum.Status,
                 $"Found \"{EndregionString} {GrammlatorString} {GeneratedString}\", the last line of replaced code.",
                new STextPosition(lineNumber: SourceReader.LineNumber, columnNumber: 0, position: StartOfMarkedLine));
         }
         else {
            OutputPositionAndMessage(MessageTypeOrDestinationEnum.Abort,
               $"Missing \"{EndregionString} {GrammlatorString} {GeneratedString}\".",
               new STextPosition(lineNumber: SourceReader.LineNumber, columnNumber: 0, position: SourceReader.Position));
         }

         // ----- Do phase 2
         outputMessage(MessageTypeOrDestinationEnum.Information, "Start of phase 2: compute the states");
         // Verwaltung für Mengen terminaler Symbole vorbereiten:            
         AllTerminalSymbols = new BitArray(NumberOfTerminalSymbols, true);
         P2ComputeLR0States.MakeInstanceAndExecute();

         // ----- Do phase 3
         outputMessage(MessageTypeOrDestinationEnum.Information, "Start of phase 3: compute follow symbols, solve conflicts, write conflicts protocol");
         P3ComputeLALR1.MakeInstanceAndExecute();

         // ----- Write protocol of symbols and states before optimizations
         outputMessage(MessageTypeOrDestinationEnum.Information, "                  write protocol of symbols and states before optimizations");

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

         // ----- Do phase 4
         outputMessage(MessageTypeOrDestinationEnum.Information, "Start of phase 4: optimizations");
         P4ReplaceNonterminalsAndOptimize.MakeInstanceAndExecute();

         // ----- Write protocol of states after optimizations
         outputMessage(MessageTypeOrDestinationEnum.Information, "                  write protocol of states after optimizations");
         Protocol.AppendLine("List of all states after optimizations:")
           .AppendLine();
         DisplayStates(Protocol); // dazu evtl. die Items der Zustände erweitern ???
         outputMessage(MessageTypeOrDestinationEnum.StateProtocol2, Protocol.ToString());
#pragma warning disable IDE0059 // Der Wert, der dem Symbol zugeordnet ist, wird niemals verwendet.
         Protocol.Clear();
         Protocol.Capacity = 0;
         Protocol = null; // free ressources (optional)
#pragma warning restore IDE0059 // Der Wert, der dem Symbol zugeordnet ist, wird niemals verwendet.

         // ----- Do phase 5 with a specific code generator
         outputMessage(MessageTypeOrDestinationEnum.Information, "Start of phase 5: generate result");
         P5GenerateCode.MakeInstanceAndExecute(new P5CodegenCS(Resultbuilder));

         // ----- Copy rest of source to result
         outputMessage(MessageTypeOrDestinationEnum.Information, "                  copy rest of source to result");

         SourceReader.ReadAndCopyToEnd(Resultbuilder);

         // ----- Translation finished
         outputMessage(MessageTypeOrDestinationEnum.Information,
             "Done. Don't forget to check the messages and the protocol of the conflicts.");
      }

      public static void ProtocolSymbols(StringBuilder sb, Dictionary<String, Symbol> symbolDictionary) {
         foreach (KeyValuePair<String, Symbol> p in symbolDictionary) {
            Symbol Symbol = p.Value;

            if (Symbol != null)
               Symbol.ToStringbuilder(sb);
            else
               sb.Append(p.Key).Append(" is not a symbol");
            sb.AppendLine();

            if (Symbol != null && Symbol.SymbolNumber == NumberOfTerminalSymbols - 1)
               sb.AppendLine("---"); // delimiting line between terminal symbols and nonterminal symbols
         }
      }

      public static void DisplayStates(StringBuilder sb) {
         foreach (ParserState state in ListOfAllStates) {
            state.ToStringbuilder(sb);
         }
      }
   }
}
