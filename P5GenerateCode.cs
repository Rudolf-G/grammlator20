using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Text;

namespace Grammlator
{
   internal partial class P5GenerateCode
   {
      public static void MakeInstanceAndExecute(P5CodegenCS Codegen) => new P5GenerateCode(Codegen).Execute();

      /// <summary>
      /// Generate code
      /// </summary>
      /// <param name="CodeGen"></param>
      private P5GenerateCode(P5CodegenCS CodeGen) => this.codegen = CodeGen;

      private readonly P5CodegenCS codegen;

      /*
       *    TODO Der Praefix für xx.a ist aus dem Vorgängerzustand zu bestimmen,
       *    die xx.a-Aktionen sind also grundsätzlich anders zu erzeugen !!!
       *    (derzeit wird pauschal ein globaler Präfix verwendet)
       *    
       *    -- TODO  Diverse Optimierungen in Phase 5:
       *    -- Es gibt Zustände, die nur nach l1.s ohne l1.a erreicht werden
       *    --   (alle Zustände mit Akzept_Aufrufe=0? Startzustand?).
       *    --   Diese Zustände werden nur
       *    --   nach Abprüfen erlaubter Symbole erreicht. Alle anderen Symbole
       *    --   können als nicht relevant vorgemerkt werden! Damit entfällt u.U.
       *    --   die Fehleraktion und somit eine (evtl. die einzige) IF-Abfrage!
       *    --
       *    --   l1.a; l1.s   zusamenfassen zu  l1.as;
       *    --
       *    -- gleiche Aktionen eines Zustandes zusammenfassen zu einer Aktion!
       *    --
       *    -- evtl. nach solchen Optimierungen gleich gewordene Zustände zusammenfassen.
       *    --
       *    -- Aktionstyp ist_Fehleraktion: ordentliche Fehlermeldungen erzeugen!
       *    
       *    -- Aktionen sortieren, bevor IF-Code erzeugt wird
       *    --    Fehleraktion moeglichst nicht ans Ende eines Zustandes
       *    --    Aktionen mit Marke ans Ende kommen (da Marken nicht innerhalb von Bloecken
       *    --    erzeugt werden koennen)
       *    --    Status: teilweise implementiert(erzeugte Aktionen nicht ans Ende)
       *    
       *    
       *    -- Erzeugen von CASE-Anweisungen fuer terminale Aktionen
       *    -- Sequenzen in CASE-Anweisungen fuer Verzweigungen
       *    
       *    -- Struktur des erzeugten Codes ueberpruefen:
       *    --      in den semantischen Aktionen sollen moeglichst keine Namen
       *    --      fuer interne Zwecke vorbehalten sein  (z.B. Aktionxxx)
       *    
       *    -- kleinere Optimierungen:
       *    
       *    -- Sonstiges:
       *    --    Fehler melden, wenn es Stellen im Code gibt, die nicht zu einer Halt-Aktion führen
       *    --
       *    --    Zustände aufteilen in verschiedene Teile (insbesondere bedingte Aktionen) und
       *    --       gleiche Teile (bedingte Aktionen) verschiedener Zustände zusammenfassen}
       *    */

      private const Int32 nestingLevelLimit = 5; // TODO allow user to set NestingLevelLimit
      private Boolean reductionsModifyAttributStack;

      private void Execute()
      {
         GlobalVariables.Startaction.CountUsage(Accept: false);
         /* doppelte Aktionen in gleichen Zuständen sollten bereits zusammengefasst sein,
            um die Zahl der Aufrufe der entsprechenden Folgeaktion gering zu halten!}
         */

         // Determine whether the generated code will modify the attribute stack
         reductionsModifyAttributStack = false;
         foreach (ReduceAction reduction in GlobalVariables.ListOfAllReductions)
         {
            if (reduction.Calls > 0 && reduction.AttributeStackAdjustment != 0)
            {
               reductionsModifyAttributStack = true;
               break;
            }
         }

         codegen.GenerateStartOfCode(
             GenerateStateStackInitialCountVariable: GlobalVariables.CountOfStatesWithStateStackNumber > 0,
             GenerateAttributeStackInitialCountVariable: reductionsModifyAttributStack && GlobalVariables.TheOnlyOneErrorHaltAction.Calls > 0
             // The variable AttributeStackInitialCount is used by ErrorHaltActions, if the attributestack is used
             );

         //// verhindern, dass die Halt-Aktion und die Fehlerhaltaktion inline erzeugt werden,
         //// um sie am Ende des Codes erzeugen zu können

         //g.HaltAction.Aufrufe = -g.HaltAction.Aufrufe; // g.Haltaktion.Codenummer = 1;
         GlobalVariables.TheEndOfGeneratedCodeAction.Calls = -GlobalVariables.TheEndOfGeneratedCodeAction.Calls;
         GlobalVariables.TheOnlyOneErrorHaltAction.Calls = -GlobalVariables.TheOnlyOneErrorHaltAction.Calls;

         // Generate the code for the parsers first action and the sequence of actions reached from this action without goto
         GenerateCodeSequence(
             GlobalVariables.Startaction,
             accept: false,
             labelMustBeGenerated: false,
             nestingLevel: 0);

         // Generate all the actions which are reached by goto and which are not yet generated
         // 1. generate code for all actions which are called more than once and therefore need a label
         //    and for all actions which can be reached from those actions without goto
         GenerateCodeWithLabels(2);
         // 2. generate code for all other actions
         GenerateCodeWithLabels(1);

         // Generate the ErrorHaltAction and the EndOfGeneratedCodeAction (=Label)
         GlobalVariables.TheOnlyOneErrorHaltAction.Calls = -GlobalVariables.TheOnlyOneErrorHaltAction.Calls; // war g.Fehlerhaltaktion.Codenummer = 0;
         GlobalVariables.TheEndOfGeneratedCodeAction.Calls = -GlobalVariables.TheEndOfGeneratedCodeAction.Calls;
         if (GlobalVariables.TheOnlyOneErrorHaltAction.Calls > 0)
         {
            GenerateCodeSequence(GlobalVariables.TheOnlyOneErrorHaltAction, GlobalVariables.TheOnlyOneErrorHaltAction.AcceptCalls > 0,
                labelMustBeGenerated: true, nestingLevel: 0);
         }

         // Generate the 
         if (GlobalVariables.TheEndOfGeneratedCodeAction.Calls > 0)
         {
            GenerateCodeSequence(GlobalVariables.TheEndOfGeneratedCodeAction, GlobalVariables.TheEndOfGeneratedCodeAction.AcceptCalls > 0,
                labelMustBeGenerated: true, nestingLevel: 0);
         }

         codegen.GenerateEndOfCode(); // und den eventuell in cg zwischengespeicherten Code ausgeben
      }

      private static Boolean GeneratesGoto(Boolean generateAccept, ParserAction action, Int32 nestingLevel)
      {
         if (generateAccept)
         {
            if (action.AcceptCalls <= 0
                || (action.AcceptCalls >= 2 && nestingLevel > 0))
            {
               return true;
            }
         }

         return action.Calls <= 0
             || (action.Calls > 1 && nestingLevel > 0)
             || nestingLevel >= nestingLevelLimit;
      }

      /// <summary>
      /// Erzeugt einen Sprung oder eine Marke und gibt zurück, ob ein Sprung erzeugt wurde 
      /// (bzw. auch true wenn nichts zu erzeugen ist weil .AcceptCalls bzw. .Calls &lt;=0)
      /// </summary>
      /// <param name="canNotBeReachedWithoutLabel">Gibt an, ob der vorhergehende Code mit einem Sprung endet, nestingLevel muss 0 sein </param>
      /// <param name="generateAccept">Wählt aus, ob die Aktion mit oder ohne Akzeptieren erzeugt werden soll</param>
      /// <param name="parserAction">Die Aktion, zu der ein Sprung oder für die eine Marke (sofern benötigt) erzeugt werden soll</param>
      /// <param name="beginOfBlockHasBeenGenerated">Gibt an, ob eine öffnende Klammer erzeugt wurde</param>
      /// <param name="nestingLevel">Falls >0 wird keine Marke erzeugt</param>
      /// <returns>true, if goto has been generated</returns>
      private Boolean GenerateOptionalLabelOptionalAcceptOrGoto(
          Boolean canNotBeReachedWithoutLabel, // because goto has been generated
          Boolean generateAccept,
          ParserAction parserAction,
          ref Boolean beginOfBlockHasBeenGenerated,
          Int32 nestingLevel)
      {
         /*  --- neue Version vom 20.05.2016 ---
          *  
          *  Abhängig von "akzeptieren" bezieht sich die Methode auf die Akzeptieren-Aktion (mit der darauf folgenden Aktion)
          *  oder nur auf die darauf folgende Aktion.
          *  
          *  Wenn ein Sprung erzeugt wird, hat die Methode ihren Zweck erfüllt (return true).
          *  
          *  Wenn SprungErzeugt==true, dann handelt es sich um ein alleine stehendes Codefragment. 
          *  Verschachtelungstiefe muss 0 sein.
          *     Wenn der Aufrufzähler<=0 ist, dann muss nichts erzeugt werden
          *        (return true, da nachfolgender Code ohne Marke nicht erreichbar wäre) dürfte nicht vorkommen ????? !!!
          *     Wenn der Aufrufzähler>0 ist, muss eine Marke erzeugt werden,
          *     danach siehe "eventuell Akzeptieren-Aktion erzeugen".
          *    
          *  Andernfalls, wenn !SprungErzeugt, 
          *    schließt der nun erzeugte Code an vorhergehend erzeugten Code an. Das bedeutet:
          *    Wenn der Aufrufzähler für den Code <= 0 ist, ist ein Sprung zu erzeugen
          *        (0: Code ist erzeugt, <0: Codeerzeugung ist unterdrückt) Anwendung / Kontext ????
          *    wenn die Zahl der Aufrufe>1 und die Verschachtelungstiefe>0 ist, ist ein Sprung zu erzeugen (return true).
          *    Wenn die Zahl der Aufrufe>1 und die Verschachtelungstiefe==0 ist, ist eine Marke zu erzeugen,
          *    bei exakt einem in der Statistik festgestellten "Aufruf" ist weder eine Marke noch ein Sprung zu erzeugen,
          *    jeweils ist "eventuell eine Akzeptieren-Aktion zu erzeugen" (siehe unten).
          *     
          * Nachdem eine Marke erzeugt wurde, ist "eventuell eine Akzeptieren-Aktion zu erzeugen":
          *   genau dann, wenn "akzeptieren"  übergeben wurde, ansonsten ist alles erledigt (return false).
          *   Nach dem Erzeugen der Akzeptieren-Aktion wird der Aufrufzähler auf 0 gesetzt, um zu markieren, dass die Aktion nicht mehr zu erzeugen ist
          *   und dann die Methode für die "Aktion" rekursiv aufzurufen
          *   (return MarkeOderSprungErzeugen(SprungErzeugt:false, ref bool BeginErzeugt, cAktion Aktion, akzeptieren: false, int Verschachtelungstiefe)
          */

         if (generateAccept)
         {
            // Part 1: provide the accept code
            if (canNotBeReachedWithoutLabel)
            {
               // if any code is generated now it needs a label 

               Debug.Assert(nestingLevel == 0);
               if (parserAction.AcceptCalls <= 0)
               {
                  // no code has to be generated  now
                  // because it has already been generated elsewhere (calls == 0)
                  // or generating this code ist blocked (calls < 0)
                  return true;
               }

               // generate the label
               codegen.GenerateLabel(parserAction, accept: true);
            }
            else
            {
               // generate a goto or a label in special cases

               // generate goto, if code for the parsing action must not yet be generated (calls <0)
               //       or has already been generated (calls == 0)
               // or if the code will be referenced more than once (calls > 1)
               //       and therefore a label must be generated but ist not allowed to
               //       because the generated code will be within a block (nesting level >0)
               // or if the resulting nesting level would exceed the nesting level limit
               if (parserAction.AcceptCalls <= 0
                   || (parserAction.AcceptCalls > 1 && nestingLevel > 0)
                   || nestingLevel >= nestingLevelLimit)
               {
                  codegen.GenerateGoto(parserAction, accept: true, nestingLevel: nestingLevel);
                  return true; // goto has been generated
               }

               // It is necessary to generate a label if the code is referenced more than once (calls > 1) 
               if (parserAction.AcceptCalls > 1)
               {
                  codegen.GenerateLabel(parserAction, accept: true);
               }

               // If the code which can be reached without label
               // is referenced exactly once (calls ==1) no label is generated !

            }

            // if this part of the code is executed, a goto had not to be generated, 
            // a label has only been generated, if necessary

            // Generate the code for accepting a symbol
            GenerateBeginIfNestedAndGenerateAcceptInstruction(ref beginOfBlockHasBeenGenerated, nestingLevel);
            parserAction.AcceptCalls = 0; // mark that the accept code has been generated

            // Accept has been generated. 
            // Now generate a goto parser action or 
            // the optional label of the parser action and the parser action 

            // this might be done by a recursive call and then return
            // return GenerateOptionalLabelOptionalAcceptOrGoto(canNotBeReachedWithoutLabel: false, parserAction: parserAction,
            //    generateAccept: false,
            //    beginOfBlockHasBeenGenerated: ref beginOfBlockHasBeenGenerated,
            //    nestingLevel: nestingLevel);

            // this is done by modifying the local variables (formal parameters) 
            canNotBeReachedWithoutLabel = false;
            generateAccept = false;
         }

         // Part 2: provide the code of the parser action
         Debug.Assert(!generateAccept);

         // generate a goto parser action or 
         // the optional label of the parser action

         if (canNotBeReachedWithoutLabel)
         { // alleine stehendes Codefragment mit Marke erzeugen
            Debug.Assert(nestingLevel == 0);
            if (parserAction.Calls <= 0)
            {
               // no code has to be generated  now
               // because it has already been generated elsewhere (calls == 0)
               // or generating this code ist blocked (calls < 0)
               return true; // wurde bereits erzeugt oder das Erzeugen ist geblockt
            }

            // generate the label
            codegen.GenerateLabel(parserAction, accept: false);
         }
         else
         {
            // generate a goto or a label in special cases

            // generate goto, if code for the parsing action must not yet be generated (calls <0)
            //       or has already been generated (calls == 0)
            // or if the code will be referenced more than once (calls > 1)
            //       and therefore a label must be generated but ist not allowed to
            //       because the generated code will be within a block (nesting level >0)
            // or if the resulting nesting level would exceed the nesting level limit
            if (parserAction.Calls <= 0
                || (parserAction.Calls > 1 && nestingLevel > 0)
                || nestingLevel >= nestingLevelLimit)
            {
               codegen.GenerateGoto(parserAction, false, nestingLevel);
               return true;
            }

            // It is necessary to generate a label if the code is referenced more than once (calls > 1) 
            if (parserAction.Calls > 1)
            {
               codegen.GenerateLabel(parserAction, false);
            }

            // If the code which can be reached without label
            // is referenced exactly once (calls ==1) no label is generated !

         }

         // if this part of the code is executed, a goto had not to be generated, 
         // a label has only been generated, if necessary

         parserAction.Calls = 0;  // mark that begin of the parser code has been generated
                                  // the parser code will be generated by the calling method immediately
         return false;
      } // GenerateLabelOrGoto (...)

      private void GenerateBeginIfNestedAndGenerateAcceptInstruction(ref Boolean BeginHasBeenGenerated, Int32 nestingLevel)
      {
         if (!BeginHasBeenGenerated && nestingLevel > 0)
         {
            codegen.GenerateBeginOfBlock();
            BeginHasBeenGenerated = true;
         }
         codegen.IndentExactly(nestingLevel);
         codegen.GenerateAcceptSymbolInstruction();
      }

      ///// <summary>
      ///// Estimates how many if conditions might be generated to test if a symbol is one of the allowed symbols.
      ///// 0 &lt;= <see cref="ComplexityOfBitsequence"/> &lt;= allowed symbols. Returns 0 if all or none.
      ///// </summary>
      ///// <param name="SymbolErlaubt"></param>
      ///// <returns></returns>
      //private static Int32 ComplexityOfBitsequence(BitArray SymbolErlaubt)
      //{
      //    /* Bestimmt die Zahl der Abfragen, die Bedingung_erzeugen generieren würde,
      //       wenn alle Symbole relevant wären  
      //    */
      //    Int32 result = 0;
      //    Boolean aktuellerWert = SymbolErlaubt[0];
      //    Boolean Basiswert = aktuellerWert;
      //    Boolean NächsterWert;

      //    for (Int32 i = 1; i < SymbolErlaubt.Count; i++)
      //    {
      //        NächsterWert = SymbolErlaubt[i];
      //        // es liegt vor: eine bereits analysierte Folge von Werten == Basiswert
      //        // danach der aktuelle Wert,
      //        // danach der nächste Wert.
      //        // Wenn alle Werte gleich sind, ist kein Vergleich notwendig !
      //        if (aktuellerWert != Basiswert)
      //        {
      //            // Der aktuelle Wert ist anders als der Basiswert, also ist ein Vergleich zu generieren
      //            result++; // = oder >= Abfrage notwendig bzw. <> oder <=
      //            if (NächsterWert != Basiswert)
      //            {
      //                // Der nächste Wert ist gleich zum aktuellen Wert, also genügt eine >= bzw. <= Abfrage
      //                Basiswert = NächsterWert; // erneute Abfragen sind erst nötig, wenn der Wert wieder wechselt
      //            }
      //            // sonst ist der Basiswert bereits richtig (gleich dem nächsten Wert)
      //            // es genügt ebenfalls eine Abfrage, nämlich auf Gleichheit (bzw. Ungleichheit) mit dem aktuellen Wert
      //        }
      //        aktuellerWert = NächsterWert;
      //    }
      //    // letzten Wert noch berücksichtigen
      //    if (aktuellerWert != Basiswert)
      //        result++;
      //    return result;
      //}

      public struct ActionAndCounter
      {
         public Int32 Counter;
         public ParserAction Action;

         public ActionAndCounter(Int32 Counter, ParserAction Action)
         {
            this.Counter = Counter;
            this.Action = Action;
         }
      }

      /// <summary>
      /// In der BranchToGenerate.ListOfCases kann als Folge der Optimierungen die gleiche Aktion mit verschiedenen Kennungen vorkommen.
      /// In der ActionCounterList kommt jede Aktion daraus genau einmal vor. Der Zähler gibt an, wie oft sie in der ListOfCases vorkommt.
      /// </summary>
      public class ActionCounterList : List<ActionAndCounter>
      {
         private ActionCounterList(Int32 capacity) : base(capacity) { }

         private ActionCounterList()
         {
         }

         /// <summary>
         /// Constructs an ActionCounterList with the same length as the ListOfCases of the BranchToGenerate,
         /// adds all different branchcases counting duplicates, finally trims the list
         /// </summary>
         /// <param name="BranchToGenerate"></param>
         public ActionCounterList(BranchAction BranchToGenerate) : this(BranchToGenerate.ListOfCases.Count)
         {
            foreach (BranchcaseStruct branchcase in BranchToGenerate.ListOfCases)
            {
               // Die Aktion in der Liste suchen:
               Int32 FoundIndex = FindIndex(x => x.Action == branchcase.BranchcaseAction);

               if (FoundIndex == -1)
               { // falls nicht gefunden: die Aktion mit Zähler 1 einfügen
                  Add(new ActionAndCounter(Counter: 1, Action: branchcase.BranchcaseAction));
               }
               else
               { // falls gefunden:  den Zähler erhöhen
                  ActionAndCounter actionAndCounter = this[FoundIndex];
                  actionAndCounter.Counter++; // bzw. den Zähler erhöhen
                  this[FoundIndex] = actionAndCounter;
               }
            }
            TrimExcess();
         }
      }

      private ParserAction GenerateCondionalActionsOfBranch(BranchAction BranchToGenerate, out Boolean Accept, Int32 NestingLevel)
      {
         // Create a CounterList which is ready to use preset with one entry and counter for each different action in BranchtoGenerate  
         var CounterList = new ActionCounterList(BranchToGenerate);

         // if there is only one action: return it to be generated without condition
         if (CounterList.Count == 1)
         {
            Accept = false;
            return CounterList[0].Action;
         }

         // Select a default action
         // There are different criteria to select the default action:
         //   select the most frequent action to reduce the conditions;
         //   avoid goto actions to move them in the conditional part;
         //   avoid other simple actions to put them in the conditional part

         Int32 priority = Int32.MinValue;
         ParserAction defaultAction = null;

         foreach (ActionAndCounter actionAndCounter in CounterList)
         {
            // Select the most frequent action
            Int32 thisPriority = actionAndCounter.Counter;

            if (
                // but not actions generated as goto
                GeneratesGoto(generateAccept: false, actionAndCounter.Action, NestingLevel)
                // or actions which will loop back 
                || (actionAndCounter.Action as ReduceAction)?.NextAction == BranchToGenerate
                // or indirect or direct HaltActions
                || (actionAndCounter.Action as ReduceAction)?.NextAction is HaltAction
                || actionAndCounter.Action is HaltAction
                )
            {
               thisPriority -= 2;
            }

            if (thisPriority > priority)
            {
               priority = thisPriority;
               defaultAction = actionAndCounter.Action;
            }
         }

         // It is possible to generate an IF condition with one test for equality (or unequality)
         // if there are only 2 different actions one of which has only one condition (Counter==1) to check for
         // If both actiosn have a counter of 1, 
         if (CounterList.Count == 2 && (CounterList[0].Counter == 1 || CounterList[1].Counter == 1))
         {
            GenerateBranchAsIFInstruction(BranchToGenerate, CounterList, defaultAction, NestingLevel);
            Accept = false;
            return defaultAction;
         }

         return GenerateBranchAsSwitch(BranchToGenerate, out Accept, NestingLevel, CounterList, defaultAction);
      }

      private ParserAction GenerateBranchAsSwitch(
          BranchAction BranchToGenerate, out Boolean Accept, Int32 NestingLevel, ActionCounterList CounterList, ParserAction defaultAction)
      {
         // Generate a switch statement
         // Für jede Aktion ungleich Defaultaktion in der Zählliste alle gleichen Aktionen in der Falliste der Verzweigung suchen
         // und eine oder mehrere Case-Anweisungen erzeugen sowie einmal die Codefolge für die Aktion

         codegen.IndentExactly(NestingLevel)
            .Append("switch (")
            .Append(GlobalVariables.StateStack)
            .Append(".Peek())")
            .IndentExactly()
            .Append("{");

         foreach (ActionAndCounter ElementOFCounterList in CounterList)
         {
            if (ElementOFCounterList.Action != defaultAction)
            {
               // Alle gleichen Aktionen suchen
               foreach (BranchcaseStruct f in BranchToGenerate.ListOfCases)
               {
                  if (f.BranchcaseAction == ElementOFCounterList.Action)
                     codegen.IndentExactly()
                        .Append("case ")
                        .Append(f.BranchcaseCondition.ToString())
                        .Append(": ");

                  // codegen.AppendInstruction("case ", f.BranchcaseCondition.ToString(), ": ");
               }

               // Die zugehörige Codefolge erzeugen
               GenerateCodeSequence(ElementOFCounterList.Action, false, false, NestingLevel + 1);
               codegen.IndentExactly(NestingLevel);
               // Die Codefolge endet immer mit einem GOTO - auch bei Halt !
            }
         }

         // Für die Defaultaktion analog

         // Alle gleichen Aktionen suchen und die case-Anweisungen als Kommentar erzeugen
         // Falls mehrere Fälle zusammengefast werden, ist die Zahl der Aufrufe der default-Aktion entsprechend zu verringern
         codegen.Append("/*");
         Int32 countOfDefaultCases = 0;
         foreach (BranchcaseStruct f in BranchToGenerate.ListOfCases)
         {
            if (f.BranchcaseAction == defaultAction)
            {
               countOfDefaultCases++;
               codegen.AppendWithOptionalLinebreak("case ", f.BranchcaseCondition.ToString(), ": ");
            }
         }

         if (countOfDefaultCases > 1)
            defaultAction.Calls -= countOfDefaultCases - 1;
         // Die auskommentierte Default-Codefolge  und das Kommentarende erzeugen
         codegen.IndentAndAppendLine("default: break;")
         .IndentAndAppendLine("*/")
         .IndentExactly(NestingLevel)
         .AppendLine("}");
         Accept = false;
         return defaultAction;
      }

      /// <summary>
      /// generate an if instruction so that not the default action will be executed conditionally 
      /// and the default action will remain to be generated in the codesequence
      /// </summary>
      /// <param name="BranchToGenerate"></param>
      /// <param name="CounterList"></param>
      /// <param name="defaultAction"></param>
      /// <param name="NestingLevel"></param>
      private void GenerateBranchAsIFInstruction(BranchAction BranchToGenerate, ActionCounterList CounterList, ParserAction defaultAction, Int32 NestingLevel)
      {
         Int32 simpleCaseIndex = 0, complexCaseIndex = 1; // or vice versa

         if (CounterList[simpleCaseIndex].Counter == 1 && CounterList[complexCaseIndex].Counter == 1)
         {
            // if each of both action occurs only once use the default-Action as complexCase
            if (CounterList[simpleCaseIndex].Action == defaultAction)
            {
               // vice versa
               simpleCaseIndex = 1;
               complexCaseIndex = 0;
            }
         }
         else if (CounterList[simpleCaseIndex].Counter != 1)
         {
            // vice versa
            simpleCaseIndex = 1;
            complexCaseIndex = 0;
         }

         ParserAction simpleCase = CounterList[simpleCaseIndex].Action;
         ParserAction complexCase = CounterList[complexCaseIndex].Action;
         Int32 complexCaseCounter = CounterList[complexCaseIndex].Counter;

         // simpleCaseCounter == 1   !

         // Find simpleCase in the ListOfCases - where it occurs only once - to get its condition
         Int32 simpleCaseCondition = BranchToGenerate.ListOfCases.Find(x => x.BranchcaseAction == simpleCase).BranchcaseCondition;

         if (complexCase == defaultAction)
         {
            codegen.GenerateIfSPeek(NestingLevel, false, simpleCaseCondition.ToString());
            // generate simpleCase
            GenerateCodeSequence(simpleCase, false, false, NestingLevel + 1);
            codegen.IndentExactly(0); // force new line
                                      // return default (complexCase) to be generated in sequence
            complexCase.Calls -= complexCaseCounter - 1; // Die Zahl der Aufrufe korrigieren, da eventuell mehrere Aufrufe zusammengefasst sind
         }
         else
         { // use the complement of the condition of simpleCase 
            codegen.GenerateIfSPeek(NestingLevel, true, simpleCaseCondition.ToString());
            // generate complexCase
            complexCase.Calls -= complexCaseCounter - 1; // Adjust the number of calls because some actions are handled together
            GenerateCodeSequence(complexCase, false, false, NestingLevel + 1);
            codegen.IndentExactly(0); // force new line
                                      // return default (simpleCase) to be generated in sequence
         }
      }

      /// <summary>
      /// Compare a1 and a2 such that sort will place
      /// the action with the lower complexity ahead of the other one.
      /// If both have the same complexity the action with the higher
      /// sum of weights of its symbols will be placed ahead of the other.
      /// The sum of weights of ErorHandlingActions is ignored.
      /// </summary>
      /// <param name="a1"></param>
      /// <param name="a2"></param>
      /// <returns></returns>
      private static Int32 CompareWeightandConditionComplexity(ParserAction a1, ParserAction a2)
      {
         // a1 < a2 => <0
         // a1 == a2 => 0
         // a1 > a2 => >0 
         if (a1 as ConditionalAction == null)
            return (a2 == null) ? 0 : 1;
         if (!(a2 is ConditionalAction ActionB))
            return -1;

         /* prefer symbols with highest weight, so that the user can influence 
          * the order of the generated if-tests (put the most frequent group of terminals symbols
          * at the beginning or the end of the list of terminals symbols)
          */

         /* Conflicting purposes:
          * - order actions by increasing complexity, because preceding checks may reduce complexity of actions
          * - order actions with decreasing sum of weights (probability) so that symbols
          *   which occur with higher probability are checked first
          * - give error actions lower priority
          * 
          * Actions with same complexity should be ordered by decreasing weight
          *  
          */

         Single PositionA = a1 as ConditionalAction is ErrorhandlingAction
             ? (a1 as ConditionalAction).Complexity * 100_000
             : ((a1 as ConditionalAction).Complexity * 100_000) - (a1 as ConditionalAction).SumOfWeights;

         Single PositionB = ActionB is ErrorhandlingAction
             ? ActionB.Complexity * 100_000
             : (ActionB.Complexity * 100_000) - ActionB.SumOfWeights;

         if (PositionA != PositionB)
            return (Int32)(PositionA - PositionB);

         return (a1 as ConditionalAction).IdNumber - ActionB.IdNumber;
      }

      /// <summary>
      /// Compares ActionA.TerminalSymbols.IndexOfFirstTrueElement() with ActionB...
      /// </summary>
      /// <param name="a">Action A</param>
      /// <param name="b">Action B</param>
      /// <returns></returns>
      private static Int32 CompareIndexOfFirstTrueElement(ParserAction a, ParserAction b)
      {
         if (!(a is ConditionalAction ActionA))
            return (b == null) ? 0 : 1;
         if (!(b is ConditionalAction ActionB))
            return -1;

         Int32 First1 = ActionA.TerminalSymbols.IndexOfFirstTrueElement();
         Int32 First2 = ActionB.TerminalSymbols.IndexOfFirstTrueElement();

         if (First1 != First2)
            return First1 - First2;

         return ActionA.IdNumber - ActionB.IdNumber;
      }

      /// <summary>
      /// Generates nothing if .Calls (resp. .AccepCalls) &lt;=0.
      /// Generates goto xxx, if the code has been generated or can not be generated at the actual nesting level.
      /// Generates accept if the action to be generated is a <see cref="TerminalTransition"/>.
      /// Sets .Calls (resp. .AcceptCalls) to 0.
      /// </summary>
      /// <param name="actionToGenerate"></param>
      /// <param name="labelMustBeGenerated"></param>
      /// <param name="nestingLevel"></param>
      private void GenerateCodeSequence(ParserActionWithNextAction actionToGenerate, Boolean labelMustBeGenerated, Int32 nestingLevel)
      {
         GenerateCodeSequence(
             actionToGenerate:
                actionToGenerate is ErrorhandlingAction && GlobalVariables.ErrorHandlerIsDefined
                   ? actionToGenerate
                   : actionToGenerate.NextAction, // LookaheadAction | TerminalTransition
             accept: actionToGenerate is TerminalTransition,
             labelMustBeGenerated,
             nestingLevel
             );
      }

      readonly StringBuilder CodeSequenceBuilder = new StringBuilder(4000);

      /// <summary>
      /// Generates nothing if .Calls (resp. .AccepCalls) &lt;=0.
      /// Generates goto xxx, if the code has been generated or can not be generated at the actual nesting leve.
      /// Sets .Calls (resp. .AcceptCalls) to 0.
      /// </summary>
      /// <param name="actionToGenerate"></param>
      /// <param name="accept"></param>
      /// <param name="labelMustBeGenerated"></param>
      /// <param name="nestingLevel"></param>
      [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1804:RemoveUnusedLocals", MessageId = "e")]
      private void GenerateCodeSequence(ParserAction actionToGenerate, Boolean accept, Boolean labelMustBeGenerated, Int32 nestingLevel)
      {
         ParserAction ActionToGenerate = actionToGenerate;
         Boolean Accept = accept;
         Boolean GotoHasBeenGenerated = labelMustBeGenerated;

         Debug.Assert(CodeSequenceBuilder.Length == 0, $"sbCodeSequence should be cleared at end of {nameof(GenerateCodeSequence)}");

         Boolean BeginOfBlockHasBeenGenerated = false;

         while (ActionToGenerate != null)
         {
            GotoHasBeenGenerated =
                GenerateOptionalLabelOptionalAcceptOrGoto(
                    canNotBeReachedWithoutLabel: GotoHasBeenGenerated,
                    generateAccept: Accept,
                    parserAction: ActionToGenerate,
                    ref BeginOfBlockHasBeenGenerated,
                    nestingLevel);

            if (GotoHasBeenGenerated)
               break;

            // gegebenenfalls Blockanfang erzeugen
            if (!BeginOfBlockHasBeenGenerated && nestingLevel > 0)
            {
               codegen.GenerateBeginOfBlock();
               BeginOfBlockHasBeenGenerated = true;
            }

            switch (ActionToGenerate)
            {
               case ParserState state:
               {
                  ActionToGenerate = GenerateState(out Accept, state, CodeSequenceBuilder, nestingLevel);
                  break;
               }

               case ReduceAction reduce:
               {
                  ActionToGenerate = GenerateReduce(out Accept, reduce, nestingLevel);
                  break;
               }

               case BranchAction branch:
               {
                  ActionToGenerate = GenerateBranch(out Accept, branch, nestingLevel);
                  break;
               }

               case ErrorhandlingAction errorhandlingAction:
               {
                  ActionToGenerate = GenerateErrorHandling(out Accept, errorhandlingAction, nestingLevel);

                  break;
               }

#pragma warning disable IDE0059 // Der Wert, der dem Symbol zugeordnet ist, wird niemals verwendet.
               case ErrorHaltAction e: // aus Fehlerhalt wird in der Regel ein Sprung generiert (siehe MarkeOderSprungErzeugen)
               {
                  ActionToGenerate = GenerateErrorHalt();
                  break;
               }

               case HaltAction haltaction:
               {
                  ActionToGenerate = GenerateHalt(nestingLevel, haltaction);
                  break;
               }

               case EndOfGeneratedCodeAction endaction:
               {
                  ActionToGenerate = endaction.Generate(codegen, out Accept);
                  break;
               }
#pragma warning restore IDE0059 // Der Wert, der dem Symbol zugeordnet ist, wird niemals verwendet.

               default:
               {
                  throw new ErrorInGrammlatorProgramException
                     ($"illegal or unknown type {ActionToGenerate} in {nameof(P5GenerateCode.GenerateCodeSequence)}");
               }
            } // switch
         } // while (ActionToGenerate != null)

         if (BeginOfBlockHasBeenGenerated)
         {
            codegen.IndentExactly(nestingLevel);
            codegen.GenerateEndOfBlock("");
         }

         // No action to generate => if topmost level the next code can only be reached by a goto.
         // Generate an empty line preceding the label that will be generated.
         if (nestingLevel == 0)
            codegen.AppendLine(' ');

         this.CodeSequenceBuilder.Clear();
      } // private ... CodefolgeErzeugen(...)

      private ParserAction GenerateHalt(Int32 nestingLevel, HaltAction haltaction)
      {
         codegen.IndentExactly(nestingLevel);
         Int32 numberOfAttributesToStore = haltaction.AttributestackAdjustment;
         Debug.Assert(numberOfAttributesToStore >= 0);

         codegen.IndentAndAppendLine("// Halt: a definition of the startsymbol with " + numberOfAttributesToStore.ToString() + " attributes has been recognized.");

         if (GlobalVariables.ListOfAllStates[0].StateStackNumber >= 0)
            codegen.IndentExactly()
               .Append(GlobalVariables.StateStack)
               .Append(".Pop();");

         if (numberOfAttributesToStore != 0)
         {
            codegen.Append("AttributesOfSymbol.CopyAndRemoveFrom(")
               .Append(GlobalVariables.AttributeStack)
               .Append(", ")
               .Append(numberOfAttributesToStore)
               .AppendLine(");");
         }

         return haltaction.NextAction;
      }

      private ParserAction GenerateErrorHalt()
      {
         codegen.IndentAndAppendLine(
             "// This point is reached after an input error has been found");

         // generate _s.Pop(x)
         if (GlobalVariables.CountOfStatesWithStateStackNumber > 0)
            codegen.IndentExactly()
               .Append(GlobalVariables.StateStack)
               .Append(".Discard(")
               .Append(GlobalVariables.StateStack)
               .Append(".Count - ")
               .Append(GlobalVariables.StateStackInitialCountVariable)
               .AppendLine(");");
         // codegen.IndentAndAppendLine("_s.Discard(_s.Count - StateStackInitialCount);");  // TODO als Ressource definieren

         // generate _a.Free(x)
         if (reductionsModifyAttributStack)
            codegen.IndentExactly()
               .Append(GlobalVariables.AttributeStack)
               .Append(".Free(")
               .Append(GlobalVariables.AttributeStack)
               .Append(".Count - ")
               .Append(GlobalVariables.AttributeStackInitialCountVariable)
               .AppendLine(");");

         // generate additional instruction
         if (!String.IsNullOrEmpty(GlobalVariables.InstructionErrorHalt))
            codegen.IndentAndAppendLine(GlobalVariables.InstructionErrorHalt);

         return GlobalVariables.TheOnlyOneErrorHaltAction.NextAction;
      }

      /// <summary>
      /// Generates the assignment to StateNumber and the call of ErrorHandler dependent on the global variables or
      /// if there is no error handling method defined, generates only the next action (error halt)
      /// </summary>
      /// <param name="Accept"></param>
      /// <param name="errorhandlingAction"></param>
      /// <param name="nestingLevel"></param>
      /// <returns></returns>
      private ParserAction GenerateErrorHandling(out Boolean Accept, ErrorhandlingAction errorhandlingAction, Int32 nestingLevel)
      {
         if (!string.IsNullOrEmpty(GlobalVariables.ErrorHandlerMethod))
         {
            codegen.IndentExactly(nestingLevel);

            if (!string.IsNullOrEmpty(GlobalVariables.ErrorHandlerMethod))
            {   // generate ErrorHandlerCall   ErrorHandler(ErrorStateNumber, StateDescription, ParserInput);
               codegen.Append("if (")
               .Append(GlobalVariables.ErrorHandlerMethod)
               .Append('(')
               .Append(errorhandlingAction.IdNumber + 1)
               .Append(", ")
               .Append(GlobalVariables.VariableNameStateDescription)
               .Append(errorhandlingAction.State.IdNumber + 1)
               .Append(", ")
               .Append(GlobalVariables.VariableNameSymbol)
               .AppendLine("))");

               if (errorhandlingAction.State.StateStackNumber >= 0)
                  { // generate {_s.POP(..); goto state...;}
                  codegen
                     .Indent(nestingLevel + 1)
                     .AppendLine("{")
                     .IndentAndAppend(GlobalVariables.StateStack)
                     .AppendLine(".Pop(); ")
                     .GenerateGoto(errorhandlingAction.State, accept: false, nestingLevel + 1)
                     .IndentAndAppendLine("};");
                  }
               else
                  { // generate goto state...;
                  codegen.GenerateGoto(errorhandlingAction.State, accept: false, nestingLevel + 1);
                  }
               }
         }

         Accept = false;
         return errorhandlingAction.NextAction; // errorhandlingAction.NextAction == ErrorHalt  
      }

      private ParserAction GenerateBranch(
          out Boolean Accept, BranchAction BranchactionToGenerate, Int32 nestingLevel)
      {
         // Generate comment
         codegen.IndentExactly(nestingLevel)
         .AppendWithOptionalLinebreak("/* Branch ")
         .AppendWithOptionalLinebreak(BranchactionToGenerate.IdNumber + 1)
         .AppendLineWithOptionalLinebreak("*/");

         return GenerateCondionalActionsOfBranch(BranchactionToGenerate, out Accept, nestingLevel);
      }

      private ParserAction GenerateReduce(out Boolean Accept, ReduceAction ReduceActionToGenerate, Int32 nestingLevel)
      {
         //if (!BeginOfBlockHasBeenGenerated & NestingLevel > 0) {
         //    cg.BlockanfangErzeugen(); BeginOfBlockHasBeenGenerated = true;
         //    }

         // Generate description
         codegen.IndentExactly(nestingLevel);
         codegen.Append("/* Reduction ");
         codegen.Append(ReduceActionToGenerate.IdNumber + 1);
         if (ReduceActionToGenerate.StateStackAdjustment != 0)
         {
            codegen.Append(", sStack: ");
            codegen.Append(-ReduceActionToGenerate.StateStackAdjustment);
         }
         if (ReduceActionToGenerate.AttributeStackAdjustment != 0)
         {
            codegen.Append(", aStack: ");
            codegen.Append(ReduceActionToGenerate.AttributeStackAdjustment);
         }
         codegen.OutputandClearLine();
         codegen.IndentAndAppendLines(ReduceActionToGenerate.Description, " * ");
         codegen.IndentAndAppendLine(" */");

         // Generate instructions to handle the state stack
         if (ReduceActionToGenerate.StateStackAdjustment != 0)
         {
            if (ReduceActionToGenerate.StateStackAdjustment == 1)
            {
               codegen.IndentAndAppend(GlobalVariables.StateStack)
                  .AppendLine(".Pop(); ");
            }
            else
            {
               codegen.IndentAndAppend(GlobalVariables.StateStack)
                  .Append(".Discard(")
                  .Append(ReduceActionToGenerate.StateStackAdjustment)
                  .Append("); ");
            }
         }

         // Generate instructions to handle the attribute stack and to call the method

         if (ReduceActionToGenerate.AttributeStackAdjustment != 0 && ReduceActionToGenerate.FirstAdjustAttributeStackThenCallMethod)
            codegen.GenerateAttributeStackAdjustment(ReduceActionToGenerate.AttributeStackAdjustment);

         if (ReduceActionToGenerate.SemanticMethod != null)
            codegen.GenerateSemanticMethodCall(ReduceActionToGenerate.SemanticMethod);

         if (ReduceActionToGenerate.AttributeStackAdjustment != 0 && !ReduceActionToGenerate.FirstAdjustAttributeStackThenCallMethod)
         {
            codegen.Indent();
            codegen.GenerateAttributeStackAdjustment(ReduceActionToGenerate.AttributeStackAdjustment);
         }

         // Gegebenenfalls die Zeile beenden
         if (codegen.LineLength > 0)
            codegen.OutputandClearLine();

         // return next action
         Accept = false;
         Debug.Assert(
            ReduceActionToGenerate.NextAction != null,
            $"Error in Phase 5: {nameof(ReduceActionToGenerate.NextAction)} == null"
            );

         return ReduceActionToGenerate.NextAction;
      }

      /// <summary>
      /// generates all actions of the state including nested actions of some other states
      /// </summary>
      /// <param name="Accept"></param>
      /// <param name="State"></param>
      /// <param name="sbTemp"></param>
      /// <param name="NestingLevel"></param>
      /// <returns>action which has to be generated as next action</returns>
      private ParserAction GenerateState(out Boolean Accept, ParserState State, StringBuilder sbTemp, Int32 NestingLevel)
      {
         // Generate description
         State.CoreItems.ToStringbuilder(sbTemp);

         codegen.IndentExactly(NestingLevel);
         codegen.AppendWithOptionalLinebreak("/* State ");
         codegen.AppendWithOptionalLinebreak(State.IdNumber + 1);
         codegen.AppendWithOptionalLinebreak(' ');
         if (State.StateStackNumber >= 0)
         { // if it is a stacking state display the number it puts on the stack
            codegen.AppendWithOptionalLinebreak("(");
            codegen.AppendWithOptionalLinebreak((State.StateStackNumber).ToString());
            codegen.AppendWithOptionalLinebreak(")");
         }

         if (State.ContainsErrorHandlerCall && !string.IsNullOrEmpty(GlobalVariables.VariableNameStateDescription))
         {
            // Generate 1st state description line "// State xxx (yyy) with xxx=State.IdNumber + 1 
            // and yyy=State.StateStackNumber (if >= 0)
            codegen.AppendLine("*/"); // end comment

            // Generate assignment to VariableNameStateDescription (if defined)
            codegen.Indent();
            codegen.Append("const String ");
            codegen.Append(GlobalVariables.VariableNameStateDescription);
            codegen.Append(State.IdNumber + 1);
            codegen.Append(" =");
            codegen.OutputandClearLine();

            // Generate the item descriptions to be assigned to the variable
            sbTemp.Replace("\\", "\\\\").Replace("\"", "\\\""); // escape the symbols which are not allowed in strings

            codegen.IndentAndAppendLinesWithSeparator(
                linesToAppend: sbTemp.ToString() // the strings describing the items of the state
                , stringPrecedingFirstLine: "     \""  // indentation and character """" in front of each string
                , separatorAtEndofLine: GlobalVariables.NewLineWithEscapes + "\""  // at end of each string except the last
                , separatorAtNewLine: "   + \"" // before each string except the first
                , stringAtEndOfLastLine: "\";" // after the last string
                );
            sbTemp.Clear();
         }
         else
         {
            codegen.OutputandClearLine(); // continue comment

            sbTemp.Replace("*/", "* /"); // escape the symbols which are not allowed in comment

            codegen.IndentAndAppendLinesWithSeparator(
                linesToAppend: sbTemp.ToString() // the strings describing the items of the state
                , stringPrecedingFirstLine: " * "  // indentation and character """" in front of each string
                , separatorAtEndofLine: ""  // at end of each string except the last
                , separatorAtNewLine: " * " // before each string except the first
                , stringAtEndOfLastLine: "" // after the last string
                );
            codegen.AppendInstruction(" */");
            sbTemp.Clear();
         }

         codegen.Indent();

         // generate push to the state stack if necessary 
         if (State.StateStackNumber >= 0)
            codegen.GenerateStateStackPushWithOptionalLinebreak(State.StateStackNumber);

         // The call of "FetchSymbol();" must be generated only if the state contains actions, which check the input symbol.
         // This is prepared  by shortening chains in phase 4 und implemented by the following.
         // CHECK 05 (low priority) Not yet implemented: "FetchSymbol();" needs not to be generated if all pathes leading to the state
         //   contain a "FetchSymbol();" and then not a "Accept(...)"

         if (State.Actions.Count == 1 && !(State.Actions[0] is TerminalTransition))
         {
            // generate unconditional action
         }
         else if (State.Actions.Count >= 1)
         {
            // If the state contains more than one action or one action, which is a terminal transition (all symbols allowed),
            // "FetchSymbol()" must be generated
            Debug.Assert(State.Actions.Count > 1
                || State.Actions[0] is LookaheadAction
                || State.Actions[0] is TerminalTransition
                || State.Actions[0] is ErrorhandlingAction
                );

            codegen.IndentExactly();

            codegen.AppendWithOptionalLinebreak(GlobalVariables.InstructionAssignSymbol);
         }
         else // State.Actions.Count = 0
         {
            // This shouldn't happen 'cause each state should at least contain one action (may be error action) 

            // Check 5 (low priority) ist die Behandlung von Zuständen ohne terminale Aktion so ok?
            // Sind Zustände, die keine erlaubte Aktion enthalten (resultierend aus z.B *=A; A=B; B=A;), entsprechend berücksichtigt
            GlobalVariables.OutputMessage(
                MessageTypeOrDestinationEnum.Error, "Check your grammar: no actions in state " + (State.IdNumber + 1).ToString());

            ParserAction NextActionToGenerate = new ErrorhandlingAction(
                GlobalVariables.AllTerminalSymbols,
                idNumber: State.IdNumber,
                State);
            NextActionToGenerate.Calls++;
            GlobalVariables.TheOnlyOneErrorHaltAction.Calls++;
            Accept = false;
            return NextActionToGenerate;
         }

         return SortAndGenerateConditionalActionsOfState(State, out Accept, NestingLevel); // => Folgaktion oder null
      }

      /// <summary>
      /// Sort actions depending on the weight of the terminals symbols and an estimation of the complexity
      /// of the if conditions to be generated and then generate "if (...){...}" code
      /// </summary>
      /// <param name="state"></param>
      /// <param name="accept"></param>
      /// <param name="nestingLevel"></param>
      /// <returns>Unconditional action which has to be generated</returns>
      /// <exception cref="ErrorInGrammlatorProgramException"></exception>
      private ParserAction SortAndGenerateConditionalActionsOfState(ParserState state, out Boolean accept, Int32 nestingLevel)
      {
         // vorher : Aktion zeigt auf Zustand
         // nachher: result == Folgeaktion == null oder 
         //          result == Folgeaktion und akzeptieren geben die Folgeaktion an    

         /* nur eine unbedingte Aktion => als Folgeaktion zurückgeben
            wenige Aktionen            => IF-Abfrage 
            ansonsten SWITCH (nicht implementiert)
         */

         // Test if the state contains an unconditional action
         ParserAction unconditionalAction = TestForUnconditionalAction(state, out Int32 NumberOfConditionalActions);
         if (unconditionalAction != null && NumberOfConditionalActions <= 0)
         {
            accept = false;
            return unconditionalAction;
         }

         Debug.Assert(
            NumberOfConditionalActions == state.Actions.Count,
            "Error in phase 5: unconditional and other actions must not occur together in the same state"
            );

         if (state.IfComplexity > GlobalVariables.IfToSwitchBorder)
            return GenerateSwitchWithActionsOfState(state, out accept, nestingLevel);

         // Sortieren der Folgeaktionen nach steigender Komplexität der if Bedingung

         if (GlobalVariables.NumberOfTerminalSymbols > 0)
            state.Actions.Sort(CompareWeightandConditionComplexity);

         // Eine Fehleraktion am Ende möglichst mit der vorletzten Aktion tauschen,
         // da eine Fehleraktion immer die Sequenz unterbricht
         if (state.Actions.Count > 1 && state.Actions[^1] is ErrorhandlingAction)
         {
            ParserAction temp = state.Actions[^2];
            state.Actions[^2] = state.Actions[^1];
            state.Actions[^1] = temp;
         }

         // Test the state if it contains an unconditional action
         // und eine geeignete Folgeaktion heraussuchen, die als letzte Aktion unbedingt
         // erzeugt werden kann (möglichst keine Fehleraktion) - prüfen und die Intention  genau dokumentieren !!!!!
         // TODO diese Optimierung überarbeiten !!! keine Aktionen, die eine Marke erzeugen als Folgeaktion bevorzugen !!! 

         return GenerateConditionalActionsOfState(state, out accept, nestingLevel);
      }

      private static void Swap<T>(ref T a, ref T b)
      {
         T temp = a;
         a = b;
         b = temp;
      }

      /// <summary>
      /// Sort actions depending on the terminal symbols
      /// then generate "switch(...)case ...:" code
      /// </summary>
      /// <param name="state"></param>
      /// <param name="accept"></param>
      /// <param name="nestingLevel"></param>
      /// <returns>Unconditional action which has to be generated</returns>
      /// <exception cref="ErrorInGrammlatorProgramException"></exception>
      private ParserAction GenerateSwitchWithActionsOfState(ParserState state, out Boolean accept, Int32 nestingLevel)
      {
         // Sort the actions by their first symbol to improve readability of the generated code
         if (GlobalVariables.NumberOfTerminalSymbols > 0)
            state.Actions.Sort(CompareIndexOfFirstTrueElement);

         codegen.IndentExactly(nestingLevel);
         codegen.AppendWithOptionalLinebreak("switch (");
         codegen.Append(GlobalVariables.VariableNameSymbol);
         codegen.Append(")");
         codegen.GenerateBeginOfBlock();

         Int32 LeadingCount = 0, TrailingCount = 0, TerminalsCount = 0;
         ConditionalAction LeadingAction = null, TrailingAction = null;

         for (int i = 0; i < state.Actions.Count; i++)
         {
            var ThisAction = (ConditionalAction)state.Actions[i];
            BitArray Terminals = ThisAction.TerminalSymbols;
            TerminalsCount = Terminals.Count;
            Int32 TerminalIndex = Terminals.FindNextTrue(-1); // not (LeadingCount - 1) because we want to find bugs
            Boolean IsDefaultAction = false;

            // Special case if Terminals contains the first terminal
            if (TerminalIndex == 0)
            {
               // remember this action and the count of leading terminals
               Debug.Assert(LeadingCount == 0, "Phase5: LeadingCount already != 0");
               LeadingAction = ThisAction;
               LeadingCount = Terminals.FindNextFalse(0);
               TerminalIndex = Terminals.FindNextTrue(LeadingCount);
               IsDefaultAction = true;

               // generate first part of comment
               codegen.IndentExactly(nestingLevel);
               codegen.Append("// <= ");
               codegen.AppendWithPrefix(
                   GlobalVariables.TerminalSymbolEnum,
                   GlobalVariables.TerminalSymbolByIndex[LeadingCount - 1].Identifier
                   );
            }

            // Special case if Terminals contains the last terminal
            if (Terminals[TerminalsCount - 1])
            {
               // remember this action and the count of trailing terminals
               Debug.Assert(TrailingCount == 0, "Phase5: TrailingCount already != 0");
               TrailingAction = ThisAction;
               TrailingCount = TerminalsCount - Terminals.FindPrecedingFalse(Terminals.Count) - 1;
               IsDefaultAction = true;

               // generate first part of comment
               codegen.IndentExactly(nestingLevel);
               codegen.Append("// >= ");
               codegen.AppendWithPrefix(
                   GlobalVariables.TerminalSymbolEnum,
                   GlobalVariables.TerminalSymbolByIndex[TerminalsCount - TrailingCount].Identifier
                   );
            }

            if (IsDefaultAction)
            {
               // generate second part of comment
               codegen.Append(": ");
               codegen.Append("goto ");
               codegen.Append(P5CodegenCS.GotoLabel(ThisAction));
               codegen.Append(" // see end of switch");
            }

            // if remaining terminal symbols, for each 
            bool RemainingSymbols = false;
            while (TerminalIndex < TerminalsCount - TrailingCount)
            {
               RemainingSymbols = true;
               // generate "case TerminalSymbol: " 
               codegen.IndentExactly(nestingLevel);
               codegen.Append("case ");
               codegen.AppendWithPrefix(
                   GlobalVariables.TerminalSymbolEnum,
                   GlobalVariables.TerminalSymbolByIndex[TerminalIndex].Identifier
                   );
               codegen.Append(":");

               TerminalIndex = Terminals.FindNextTrue(TerminalIndex);
            }

            if (RemainingSymbols)
            {
               codegen.IncrementIndentationLevel();
               //codegen.Indent(nestingLevel + 1);

               if (IsDefaultAction)
               {
                  // adjust the statistics because here a "goto label" will be generated
                  // in addition to the code generated in the default action.
                  // If the statistics are not adjusted those code may be generated without label.
                  if (ThisAction is TerminalTransition)
                     ThisAction.NextAction.AcceptCalls++;
                  else
                     ((ThisAction is ErrorhandlingAction && GlobalVariables.ErrorHandlerIsDefined) ? ThisAction : ThisAction.NextAction).Calls++;
                  // generate goto
                  codegen.GenerateGoto(ThisAction, nestingLevel + 1);
               }
               else
                  GenerateCodeSequence(ThisAction, labelMustBeGenerated: false, nestingLevel + 1);

               codegen.DecrementIndentationLevel();
            }
         }

         Debug.Assert(LeadingAction != null && TrailingAction != null, "Leading or Trailing Action is null");

         // generate end of switch statement
         codegen.GenerateEndOfBlock("end of switch");

         // implement "default:" as fall through

         ConditionalAction Action1 = LeadingAction, Action2 = TrailingAction;
         ParserAction Action1Generate = (Action1 is ErrorhandlingAction && GlobalVariables.ErrorHandlerIsDefined) ? Action1 : Action1.NextAction;
         ParserAction Action2Generate = (Action2 is ErrorhandlingAction && GlobalVariables.ErrorHandlerIsDefined) ? Action2 : Action2.NextAction;

         Boolean ActionsSwapped = false;

         // If there are two different default actions
         if (Action1Generate != Action2Generate)
         {
            // yes: generate if
            codegen.IndentExactly(nestingLevel);
            codegen.Append("if (");
            codegen.Append(GlobalVariables.VariableNameSymbol);

            // prefer action which generates goto as first action
            // TODO else prefer action which does not generate a label
            if (GeneratesGoto(generateAccept: Action2 is TerminalTransition, Action2Generate, nestingLevel) &&
                !GeneratesGoto(generateAccept: Action1 is TerminalTransition, Action1Generate, nestingLevel))
            {
               Swap(ref Action1, ref Action2);
               Swap(ref Action1Generate, ref Action2Generate);
               ActionsSwapped = true;

               codegen.Append(" >= ")
               .AppendWithPrefix(
                   GlobalVariables.TerminalSymbolEnum,
                   GlobalVariables.TerminalSymbolByIndex[TerminalsCount - TrailingCount].Identifier
                   );
            }
            else
            {
               codegen.Append(" <= ");
               codegen.AppendWithPrefix(
                   GlobalVariables.TerminalSymbolEnum,
                   GlobalVariables.TerminalSymbolByIndex[LeadingCount - 1].Identifier
                   );
            }

            codegen.Append(") ");

            GenerateCodeSequence(Action1, labelMustBeGenerated: false, nestingLevel + 1);
         }

         // generate comment as Debug.Assert
         codegen.IndentExactly(nestingLevel);
         codegen.Append("Debug.Assert(");
         if (Action1Generate == Action2Generate)
         {
            codegen.Append(GlobalVariables.VariableNameSymbol);
            codegen.Append(" <= ");
            codegen.AppendWithPrefix(
                GlobalVariables.TerminalSymbolEnum,
                GlobalVariables.TerminalSymbolByIndex[LeadingCount - 1].Identifier
                );
            codegen.Append(" || ");
            codegen.Append(GlobalVariables.VariableNameSymbol);
            codegen.Append(" >= ");
            codegen.AppendWithPrefix(
                GlobalVariables.TerminalSymbolEnum,
                GlobalVariables.TerminalSymbolByIndex[TerminalsCount - TrailingCount].Identifier
                );
         }
         else if (ActionsSwapped)
         {
            codegen.Append(GlobalVariables.VariableNameSymbol);
            codegen.Append(" <= ");
            codegen.AppendWithPrefix(
                GlobalVariables.TerminalSymbolEnum,
                GlobalVariables.TerminalSymbolByIndex[LeadingCount - 1].Identifier
                );
         }
         else
         {
            codegen.Append(GlobalVariables.VariableNameSymbol);
            codegen.Append(" >= ");
            codegen.AppendWithPrefix(
                GlobalVariables.TerminalSymbolEnum,
                GlobalVariables.TerminalSymbolByIndex[TerminalsCount - TrailingCount].Identifier
                );
         }
         codegen.AppendLine(");");
         codegen.AppendLine("");

         // generate Action2
         accept = Action2 is TerminalTransition; // CHECK try to avoid the out parameter "accept"?
         return Action2Generate; // Check: try to avoid the special handling of ErrorhandlingAction?
      }

      /// <summary>
      /// The state contains no unconditional action.
      /// Generate the conditional actions of the state and return an action (if any) to generate next.
      /// </summary>
      /// <param name="State"></param>
      /// <param name="Accept"></param>
      /// <param name="NestingLevel"></param>
      /// <returns>next <see cref="ParserAction"/> to generate</returns>
      private ParserAction GenerateConditionalActionsOfState(
          ParserState State,
          out Boolean Accept,
          Int32 NestingLevel)
      {
         // all terminal symbols which are condition of one action can be ignored
         // in the conditions of all following actions (are not relevant)
         var relevantSymbols = new BitArray(GlobalVariables.AllTerminalSymbols);

         for (int i = 0; i < State.Actions.Count - 1; i++)
         {
            var a = (ConditionalAction)State.Actions[i];
            GenerateOneConditionalAction(a, relevantSymbols, NestingLevel); // Modifies relevantSymbols
         }

         var LastAction = (ConditionalAction)State.Actions[^1];

         ParserAction nextAction;
         if (LastAction is ErrorhandlingAction)
            nextAction = LastAction;
         else // TerminalTransition || LookaheadAction
            nextAction = LastAction.NextAction;

         BitArray suppressedCondition = LastAction.TerminalSymbols;
         if (nextAction != null && suppressedCondition != null)
         {
            GenerateConditionAsComment(suppressedCondition);
         }

         Accept = LastAction is TerminalTransition;
         return nextAction;
      }

      /// <summary>
      /// Test the state if it contains an unconditional action and return it (or null) and the number of conditional actions.
      /// </summary>
      /// <param name="state">The state which actions are to be tested</param>
      /// <param name="NumberOfConditionalActions">The number of conditional actions of the state</param>
      /// <returns>The last unconditional action of the state or null</returns>
      private static ParserAction TestForUnconditionalAction(ParserState state, out Int32 NumberOfConditionalActions)
      {
         NumberOfConditionalActions = 0;

         // If there is only one action which is a lookahead action (this may be the result of optimization)
         // look ahead must not be generated. The next action has to be generated without condition.
         if (state.Actions.Count == 1 && state.Actions[0] is LookaheadAction la)
            return la.NextAction;

         ParserAction unconditionalAction = null;
         foreach (ParserAction a in state.Actions)
         {
            switch (a)
            {
               case ParserState s:
               case ReduceAction r:
               case HaltAction h:
               case BranchAction b:
               {
                  if (unconditionalAction != null)
                  {
                     throw new ErrorInGrammlatorProgramException
                        ($"There must not be a second unconditional action in state {state}");
                  }

                  unconditionalAction = a;
                  break;
               }

               case TerminalTransition t:
               {
                  NumberOfConditionalActions++;
                  // even if all terminal symbols are allowed a terminal transition can not be skipped because 
                  // accept has to be generated
                  break;
               }

               case ErrorhandlingAction e:
               case LookaheadAction l:
               {
                  NumberOfConditionalActions++;
                  break;
               }

               case Definition d:
                  throw new ErrorInGrammlatorProgramException("Programmfehler: Aktion cAktionalternative darf in Phase 5 nicht mehr vorkommen");
               case DeletedParserAction d:
               case NonterminalTransition n:
                  break;
               default:
                  throw new ErrorInGrammlatorProgramException("Programmfehler: unbekannte Aktionsart in Phase 5");
            }
         }

         return unconditionalAction;
      }

      /// <summary>
      /// Erzeugt Codefolgen für Aktionen mit der geforderten Mindestaufrufanzahl
      /// </summary>
      /// <param name="MinimumOfCalls">Mindestzahl von Aufrufen</param>
      private void GenerateCodeWithLabels(Int32 MinimumOfCalls)
      {
         /* Each action a with a.xxxCalls == 1 has a good chance to be generated as part of another action
          * (if not prohibited by the nesting level limit). The generation of these actions is delayed.
          * 
          * Each action a with a.xxxCalls > 1 needs a label and can not be generated
          * in nested instructions (but following another action as next action).
          * 
          * The following code is a simple heuristic (!) to get a sequence where 
          * actions are generated earlier than actions they contain.
          * 
          * Another perhaps better to optimize solution would be to sort the actions in advance.
          */

         /* Generate ReduceActions early if
          * * their next action has not yet been generated
          * * their next action has more than one call
          * Typically the next action of ReduceActions are States.
          */
         foreach (ReduceAction r in GlobalVariables.ListOfAllReductions)
         {
            if (r.NextAction.Calls > 1)
            {
               if (r.AcceptCalls >= MinimumOfCalls)
                  GenerateCodeSequence(r, accept: true, labelMustBeGenerated: true, nestingLevel: 0);
               if (r.Calls >= MinimumOfCalls)
                  GenerateCodeSequence(r, accept: false, labelMustBeGenerated: true, nestingLevel: 0);
            }
         }

         /* Generate ParserStates early because they often include other actions or have a next action 
          */
         foreach (ParserState state in GlobalVariables.ListOfAllStates)
         {
            if (state.AcceptCalls >= MinimumOfCalls)
               GenerateCodeSequence(state, accept: true, labelMustBeGenerated: true, nestingLevel: 0);
            if (state.Calls >= MinimumOfCalls)
               GenerateCodeSequence(state, accept: false, labelMustBeGenerated: true, nestingLevel: 0);
         }

         /* Generate the other ReduceActions 
          */
         foreach (ReduceAction r in GlobalVariables.ListOfAllReductions)
         {
            if (r.AcceptCalls >= MinimumOfCalls)
               GenerateCodeSequence(r, accept: true, labelMustBeGenerated: true, nestingLevel: 0);
            if (r.Calls >= MinimumOfCalls)
               GenerateCodeSequence(r, accept: false, labelMustBeGenerated: true, nestingLevel: 0);
         }

         /* Typically branches are next actions of ReduceActions. So generate them after those 
          */
         foreach (BranchAction b in GlobalVariables.ListOfAllBranchActions)
         {
            if (b.AcceptCalls >= MinimumOfCalls)
               GenerateCodeSequence(b, accept: true, labelMustBeGenerated: true, nestingLevel: 0);
            if (b.Calls >= MinimumOfCalls)
               GenerateCodeSequence(b, accept: false, labelMustBeGenerated: true, nestingLevel: 0);
         }

         foreach (ErrorhandlingAction e in GlobalVariables.ListOfAllErrorhandlingActions)
         {
            if (e.AcceptCalls >= MinimumOfCalls)
               GenerateCodeSequence(e, accept: true, labelMustBeGenerated: true, nestingLevel: 0);
            if (e.Calls >= MinimumOfCalls)
               GenerateCodeSequence(e, accept: false, labelMustBeGenerated: true, nestingLevel: 0);
         }

         foreach (HaltAction h in GlobalVariables.ListOfAllHaltActions)
         {
            if (h.AcceptCalls >= MinimumOfCalls)
               GenerateCodeSequence(h, accept: true, labelMustBeGenerated: true, nestingLevel: 0);
            if (h.Calls >= MinimumOfCalls)
               GenerateCodeSequence(h, accept: false, labelMustBeGenerated: true, nestingLevel: 0);
         }
      } // void FehlendenCodeErzeugen
   } // class Phase5
} // namespace ...




