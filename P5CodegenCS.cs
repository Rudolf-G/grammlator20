using System;
using System.Diagnostics;
using System.Text;

namespace Grammlator
{
   interface ICodegen
   {
      void GenerateStartOfCode(
          Boolean GenerateStateStackInitialCountVariable,
          Boolean GenerateAttributeStackInitialCountVariable);

      void GenerateEndOfCode();

      ParserAction GenerateEndOfCodeAction();
   }

   /// <summary>
   /// Low level methods for code generation (C#) to be used by phase 5
   /// </summary>
   internal class P5CodegenCS: ICodegen
   {
      public P5CodegenCS(StringBuilder Resultbuilder)
          => this.resultbuilder = Resultbuilder;// fCode = new List<string>(100);

      private readonly StringBuilder resultbuilder;

      /// <summary>
      /// a StringBuilder to store parts of the generated code until it is output 
      /// </summary>
      private readonly StringBuilder CodeLine = new StringBuilder();

      private const Int32 LineLengthLimit = 120;
      // TODO allow user to define LimitForLineLength

      /// <summary>
      /// number of spaces used per indentation level
      /// </summary>
      private const Int32 IndentationWidth = 3;

      private void OutputAndClearCodeLine()
      {
         //if (resultbuilder.Length > 0)
         //   resultbuilder.AppendLine();
         resultbuilder.Append(CodeLine.TrimEnd(' '));
         resultbuilder.AppendLine();
         CodeLine.Clear();
      }

      public void GenerateStartOfCode(
          Boolean GenerateStateStackInitialCountVariable,
          Boolean GenerateAttributeStackInitialCountVariable)
      {
         Append(GlobalVariables.RegionString).
            Append(' ').
            Append(GlobalVariables.GrammlatorString).
            Append(' ').
            Append(GlobalVariables.GeneratedString).
            Append(' ').
            Append(GlobalVariables.TranslationInfo);

         if (GenerateStateStackInitialCountVariable)
         {
            IndentExactly()
               .Append("Int32 ")
               .Append(GlobalVariables.StateStackInitialCountVariable)
               .Append(" = ")
               .Append(GlobalVariables.StateStack)
               .AppendLine(".Count; ");
         }

         if (GenerateAttributeStackInitialCountVariable)
         {
            IndentExactly()
               .Append("Int32 ")
               .Append(GlobalVariables.AttributeStackInitialCountVariable)
               .Append(" = ")
               .Append(GlobalVariables.AttributeStack)
               .AppendLine(".Count; ");
         }
      }

      public void GenerateEndOfCode()
      {
         Append(GlobalVariables.EndregionString).
            Append(' ').
            Append(GlobalVariables.GrammlatorString).
            Append(' ').
            Append(GlobalVariables.GeneratedString).
            Append(' ').
            AppendLine(GlobalVariables.TranslationInfo);
      }

      public ParserAction GenerateEndOfCodeAction()
      {
         // The label (if needed) has already been generated
        AppendLineAndIndent()
         .Append(';');
         return null;
      }

      public void GenerateStateStackPushWithOptionalLinebreak(Int32 valueToPush)
      {
         IndentExactly();
         Append(GlobalVariables.StateStack);
         AppendWithOptionalLinebreak(".Push(");
         Append(valueToPush);
         Append("); ");
      }

      /// <summary>
      /// AppendLine() if Linelength>column, then fills the line with LineLength-column spaces
      /// </summary>
      /// <param name="column"></param>
      private void SetCol(Int32 column)
      {
         // new line if length is already beyond column
         if (LineLength > column)
            OutputandClearLine();
         // fill up to column with space 
         CodeLine.Insert(CodeLine.Length, " ", column - LineLength);
      }

      // public int AktuelleZeilennummer() { return fCode.Count; } // TODO die Position in sb für die Labelanweisung bestimmen

      // TODO  Der Zeilenumbruch kann die Kommentarzeilen massiv zerstören. außerdem müsste er einrücken !!!

      public Int32 LineLength => CodeLine.Length;

      private readonly String[] lineSeparators = new String[] { Environment.NewLine };

      /// <summary>
      /// split <paramref name="sToAppend"/> into lines and append separately
      /// </summary>
      /// <param name="sToAppend">String to break into lines</param>
      /// <param name="Prefix">String to write after indentation before sToAppend</param>
      public void IndentAndAppendLines(String sToAppend, String Prefix)
      {
         foreach (String s in sToAppend.Split(lineSeparators, StringSplitOptions.None))
         {
            Indent();
            Append(Prefix);
            AppendLine(s);
         }
      }

      /// <summary>
      /// split <paramref name="linesToAppend"/> into lines and append special strings
      /// </summary>
      /// <param name="linesToAppend">The string consisting of one or more lines, which are to be appended to the generated code</param>
      /// <param name="stringPrecedingFirstLine">for example 'stateDescription="'</param>
      /// <param name="separatorAtEndofLine">for example '"'</param>
      /// <param name="separatorAtNewLine">for example '+"'</param>
      /// <param name="stringAtEndOfLastLine">for example ';'</param>
      public void IndentAndAppendLinesWithSeparator(
          String linesToAppend, String stringPrecedingFirstLine, String separatorAtEndofLine, String separatorAtNewLine, String stringAtEndOfLastLine
          )
      {
         Int32 count = 0;
         foreach (String s in linesToAppend.Split(lineSeparators, StringSplitOptions.None))
         {
            if (count++ == 0)
            {
               Indent();
               AppendWithOptionalLinebreak(stringPrecedingFirstLine);
            }
            else
            {
               AppendLine(separatorAtEndofLine);
               IndentAndAppend(separatorAtNewLine);
            }
            Append(s);
         }
         IndentAndAppend(stringAtEndOfLastLine);
         OutputandClearLine();
      }

      public P5CodegenCS Append(Char c)
      {
         CodeLine.Append(c);
         return this;
      }

      public void AppendLine(Char c)
      {
         CodeLine.Append(c);
         OutputandClearLine();
      }

      public void AppendWithOptionalLinebreak(Char c)
      {
         if (LineLength > 10 && (LineLength + 1) >= LineLengthLimit)
         {
            OutputandClearLine();
            Indent();
         }
         CodeLine.Append(c);
      }

      /// <summary>
      /// Indents exactly before appending the strings
      /// </summary>
      /// <param name="stringsToAppend"></param>
      public void AppendInstruction(params String[] stringsToAppend)
      {
         IndentExactly();
         AppendWithOptionalLinebreak(stringsToAppend);
      }

      public P5CodegenCS Append(String s)
      {
         CodeLine.Append(s);
         return this;
      }

      public P5CodegenCS IndentAndAppend(String s)
      {
         Indent();
         CodeLine.Append(s);
         return this;
      }

      public void Append(String s1, Int32 i, String s2)
          => CodeLine.Append(s1)
              .Append(i)
              .Append(s2);

      /// <summary>
      /// Indent() and then for each string AppendLineAndIndent
      /// </summary>
      /// <param name="stringsToAppend"></param>
      public P5CodegenCS AppendWithOptionalLinebreak(params String[] stringsToAppend)
      {
         Indent();
         foreach (String s in stringsToAppend)
         {
            if (LineLength > 10 && (LineLength + s.Length) >= LineLengthLimit) // CHECK 10
               AppendLineAndIndent();
            CodeLine.Append(s);
         }
         return this;
      }

      public P5CodegenCS Append(Int32 i)
      {
         CodeLine.Append(i);
         return this;
      }

      public P5CodegenCS AppendWithOptionalLinebreak(Int32 i)
      {
         //AppendWithOptionalLinebreak(i.ToString());
         Indent();
         if (LineLength > 10 && (LineLength + 5) >= LineLengthLimit) // CHECK 10 and 5
            AppendLineAndIndent();
         CodeLine.Append(i);
         return this;
      }

      /// <summary>
      /// append line and then indent exactly
      /// </summary>
      public P5CodegenCS AppendLineAndIndent()
      {
         OutputandClearLine();
         IndentExactly();
         return this;
      }

      /// <summary>
      /// IndentExactly() and then AppendLine()
      /// </summary>
      /// <param name="s"></param>
      public P5CodegenCS IndentAndAppendLine(String s)
      {
         IndentExactly();
         AppendLine(s);
         return this;
      }

      /// <summary>
      /// append s and new line and then indent
      /// </summary>
      /// <param name="s"></param>
      public P5CodegenCS AppendLine(String s)
      {
         CodeLine.Append(s);
         OutputandClearLine();
         return this;
      }

      /// <summary>
      /// Indent() and then for each string AppendLineAndIndent, then append line
      /// </summary>
      /// <param name="stringsToAppend"></param>
      public void AppendLineWithOptionalLinebreak(params String[] stringsToAppend)
      {
         AppendWithOptionalLinebreak(stringsToAppend);
         OutputandClearLine();
      }

      /// <summary>
      /// Output the code line, if not empty, and clear the code line
      /// </summary>
      public void OutputandClearLine() =>
          OutputAndClearCodeLine();

      public void AppendWithPrefix(String Prefix, String s)
      {
         if (!String.IsNullOrEmpty(Prefix))
         {
            Append(Prefix);
            Append('.');
            Append(s);
         }
         else
         {
            Append(s);
         }
      }

      private Int32 IndentationLevel = 0;

      /// <summary>
      /// Set indentation level and indent to new indentation position if actual position is smaller,
      /// else start new line and indent. The indentation is IndentationLevel*IndentationWidth + 2.
      /// 2 is added, so that only labels appear without any indentation.
      /// </summary>
      /// <param name="newIndentation"></param>
      public P5CodegenCS IndentExactly(Int32 newIndentation)
      {
         IndentationLevel = newIndentation;
         SetCol((IndentationLevel * IndentationWidth) + 2);
         return this;
      }

      /// <summary>
      /// Indent to position given by the indentation level, if actual position is smaller,
      /// else start new line and indent
      /// </summary>
      public P5CodegenCS IndentExactly()
      {
         IndentExactly(IndentationLevel);
         return this;
      }

      /// <summary>
      /// Indent to actual indentation position, if actual position is smaller, else do nothing
      /// </summary>
      public void Indent()
      {
         if (LineLength < (IndentationLevel * IndentationWidth) + 2)
            SetCol((IndentationLevel * IndentationWidth) + 2);
      }

      /// <summary>
      /// Set indentation level and indent to actual indentation position only, if actual position is smaller
      /// </summary>
      /// <param name="newIndentation">new indentation level</param>
      public P5CodegenCS Indent(Int32 newIndentation)
      {
         IndentationLevel = newIndentation;
         if (LineLength < (IndentationLevel * IndentationWidth) + 2)
            SetCol((IndentationLevel * IndentationWidth) + 2);
         return this;
      }

      public void IncrementIndentationLevel()
         => IndentationLevel++;

      public void DecrementIndentationLevel()
      {
         IndentationLevel--;
         if (IndentationLevel < 0)
            IndentationLevel = 0;
            // throw new ArgumentException("IndentationLevel<0");
      }

      public void GenerateBeginOfBlock()
         {
         IndentExactly();
         AppendLine('{');
         IncrementIndentationLevel();
         //IndentationLevel = NewIndentation;
         //IndentAndAppend('{');
         //AppendLine(); // eine neue Zeile anfangen
      }

      public void GenerateEndOfBlock(String comment)
      {
         IndentExactly();
         DecrementIndentationLevel();
         CodeLine.Append("}");
         if (!string.IsNullOrEmpty(comment))
            Append(" // ").
               Append(comment);
         OutputandClearLine();
      }

      // keine neue Zeile anfangen

      public void GenerateAcceptSymbolInstruction()
          => AppendWithOptionalLinebreak(GlobalVariables.InstructionAcceptSymbol);

      /// <summary>
      /// returns a label built from an action specific string and the actions IdNumber+1
      /// </summary>
      /// <param name="action"></param>
      /// <param name="accept"></param>
      /// <returns>The label assigned to the (accept) action</returns>
      private static String GotoLabel(ParserAction action, Boolean accept)
      {
         string MapActiontypeToString = GlobalVariables.LabelPrefixes[(int)action.ParserActionType];
         if (action.ParserActionType==ParserActionEnum.isErrorhaltAction) // There is only one ErrorHaltAction
            return (accept ? "Accept" : "") + MapActiontypeToString.ToString();
         return (accept ? "Accept" : "") + MapActiontypeToString + (action.IdNumber + 1).ToString();
      }

      public static String GotoLabel(ParserActionWithNextAction action)
      {
         return GotoLabel(
             action is ErrorhandlingAction
             ? action
             : action.NextAction,
             accept: action is TerminalTransition);
      }

      public void GenerateLabel(ParserAction action, Boolean accept)
          => GenerateLabel(GotoLabel(action, accept));

      private void GenerateLabel(String s)
      {
         SetCol(0);
         CodeLine.Append(s)
             .Append(": ");
      }

      public void GenerateGoto(ParserActionWithNextAction action, Int32 nestingLevel)
      {
         GenerateGoto(
             action is ErrorhandlingAction ? action : action.NextAction,
             action is TerminalTransition,
             nestingLevel);
      }

      public P5CodegenCS GenerateGoto(ParserAction action, Boolean accept, Int32 nestingLevel)
      {
         IndentExactly(nestingLevel);
         CodeLine.Append("goto ")
             .Append(GotoLabel(action, accept));
         AppendLine("; ");
         return this;
      }

      public void GenerateIfSPeek(Int32 NewIndentationLevel, Boolean inverse, String condition)
      {
         IndentExactly(NewIndentationLevel);
         if (inverse)
            Append("if (")
               .Append(GlobalVariables.StateStack)
               .Append(".Peek() != ");
         //            AppendWithOptionalLinebreak("if (_s.Peek() != ");
         else
            Append("if (")
               .Append(GlobalVariables.StateStack)
               .Append(".Peek() == ");
         //   AppendWithOptionalLinebreak("if (_s.Peek() == ");
         AppendWithOptionalLinebreak(condition);
         Append(") ");
      }

      public void GenerateSemanticMethodCall(VoidMethodClass semantischeAktion)
      {
         IndentExactly();
         OutputandClearLine(); // empty line preceding method call
         IndentAndAppend(semantischeAktion.MethodName);
         Append("(");

         // Generate actual parameters for method call
         IndentationLevel++;

         Int32 count = 0;
         foreach (MethodParameterStruct Parameter in semantischeAktion.MethodParameters)
         {
            string ParameterTypeString=GlobalVariables.GetStringOfIndex(Parameter.TypeStringIndex);

            OutputandClearLine();
            IndentAndAppend(GlobalVariables.GetStringOfIndex(Parameter.NameStringIndex));
            Append(": ");
            switch (Parameter.Implementation)
            {
               case ParameterImplementation.OutCall:
               {
                  Append("out ");
                  AppendAttributePeekRef(Parameter.Offset, ParameterTypeString);
                  break;
               }

               case ParameterImplementation.RefCall:
               {
                  Append("ref ");
                  AppendAttributePeekRef(Parameter.Offset, ParameterTypeString);
                  break;
               }

               case ParameterImplementation.ValueOrInCall:
               {
                  AppendAttributePeekRef(Parameter.Offset, ParameterTypeString);
                  break;
               }

               case ParameterImplementation.OutClearCall:
               {
                  Append("out ");
                  AppendAttributePeekRefClear(Parameter.Offset, ParameterTypeString);
                  break;
               }

               case ParameterImplementation.ValueOrInClearCall:
               {
                  AppendAttributePeekClear(Parameter.Offset, ParameterTypeString);
                  break;
               }

               case ParameterImplementation.NotAssigned:
               {
                  Debug.Fail("Programm error in PSCodeGen: ParameterImplementation.NotAssigned");
                  throw new ErrorInGrammlatorProgramException
                      ("Programm error in PSCodeGen: ParameterImplementation.NotAssigned");
               }
            }
            count++;
            if (count < semantischeAktion.MethodParameters.Length)
               Append(", ");
         }

         if (semantischeAktion.MethodParameters.Length > 0)
            IndentExactly();
         AppendLine("); ");
         IndentationLevel--;

         OutputandClearLine(); // empty line following method call
      }

      public void GenerateAttributeStackAdjustment(Int32 AttributkellerKorrektur)
      {
         if (AttributkellerKorrektur == 1)
         {
            IndentAndAppend(AttributeAccessPrefix);
            Append("Allocate(); ");
         }
         else if (AttributkellerKorrektur >= 1)
         {
            IndentAndAppend(AttributeAccessPrefix);
            Append("Allocate(", AttributkellerKorrektur, "); ");
         }
         else if (AttributkellerKorrektur == -1)
         {
            IndentAndAppend(AttributeAccessPrefix);
            Append("Free(); ");
         }
         else
         {
            IndentAndAppend(AttributeAccessPrefix);
            Append("Free(", -AttributkellerKorrektur, "); ");
         }
      }

      /// <summary>
      /// for example "_a."
      /// </summary>
      public readonly String AttributeAccessPrefix = GlobalVariables.AttributeStack + ".";

      /// <summary>
      /// for example "_a.x-"
      /// </summary>
      public readonly String AttributeIndexPrefix = GlobalVariables.AttributeStack + ".x";

      /// <summary>
      /// for example "_", used to modify the type of the methods formal parameter
      /// to a fieldname of the elements of the attribute stack (e.g. modifies "Int32" to "_Int32")
      /// </summary>
      public const Char AttributeTypePrefix = '_';

      /// <summary>
      /// for example "_a.useup("
      /// </summary>
      public readonly String AttributePeekClearPart1 = GlobalVariables.AttributeStack + ".PeekClear(";

      /// <summary>
      /// for example ")."
      /// </summary>
      public const string AttributePeekClearPart2 = ").";

      /// <summary>
      /// for example "_a.useup("
      /// </summary>
      public readonly String AttributePeekRefPart1 = GlobalVariables.AttributeStack + ".PeekRef(";

      /// <summary>
      /// for example "_a.useup("
      /// </summary>
      public readonly String AttributePeekRefClearPart1 = GlobalVariables.AttributeStack + ".PeekRefClear(";

      /// <summary>
      /// for example ")."
      /// </summary>
      public const string AttributePeekRefPart2 = ").";

      /// <summary>
      /// Append attribute get reference expression e.g. "_a.PeekRef(-1)._String"
      /// </summary>
      /// <param name="offset">offset of attribute from top of attribute stack, must be &lt;0</param>
      /// <param name="AccessType">the type of the methods formal parameter</param>
      public void AppendAttributePeekRef(Int32 offset, String AccessType)
      {
         Debug.Assert(offset <= 0);
         Append(AttributePeekRefPart1);
         Append(offset); // Appends the sign '-' because here offset < 0
         Append(AttributePeekRefPart2);
         Append(AttributeTypePrefix);
         Append(AccessType);
      }

      /// <summary>
      /// Append attribute get reference expression e.g. "_a.PeekRef(-1)._String"
      /// </summary>
      /// <param name="offset">offset of attribute from top of attribute stack, must be &lt;0</param>
      /// <param name="AccessType">the type of the methods formal parameter</param>
      public void AppendAttributePeekRefClear(Int32 offset, String AccessType)
      {
         Debug.Assert(offset <= 0);
         Append(AttributePeekRefClearPart1);
         Append(offset); // Appends the sign '-' because here offset < 0
         Append(AttributePeekRefPart2);
         Append(AttributeTypePrefix);
         Append(AccessType);
      }

      /// <summary>
      /// Append attribute copy and clear method call e.g. "_a.PeekClear(-1)._String"
      /// </summary>
      /// <param name="offset">offset of attribute from top of attribute stack, must be &lt;0</param>
      /// <param name="AccessType">the type of the methods formal parameter</param>
      public void AppendAttributePeekClear(Int32 offset, String AccessType)
      {
         Debug.Assert(offset <= 0);
         Append(AttributePeekClearPart1);
         Append(offset);
         Append(AttributePeekClearPart2);
         Append(AttributeTypePrefix);
         Append(AccessType);
      }
   }
}
