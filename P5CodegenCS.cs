using System;
using System.Diagnostics;
using System.Dynamic;
using System.Text;

namespace grammlator {

   /// <summary>
   /// Low level methods for code generation (C#) to be used by phase 5
   /// </summary>
   internal class P5CodegenCS {
      public P5CodegenCS(StringBuilder resultbuilder)
      {
         ResultBuilder = resultbuilder;
         // Code is constructed in Codebuilder
         // then definitions are appended to ResultBuilder
         // then CodeBuilder is appended to Resultbuilder
         CodeBuilder = new StringBuilder(20000);
      }

      private readonly StringBuilder ResultBuilder;
      private StringBuilder CodeBuilder;

      /// <summary>
      /// a StringBuilder to store parts of the generated code until it is output 
      /// </summary>
      private readonly StringBuilder CodeLine = new StringBuilder();

      private readonly Int32 LineLengthLimit = GlobalVariables.LineLengthLimit.Value;
      // TOCHECK can more generated lines be shortened to LineLengthLimit?

      /// <summary>
      /// number of spaces used per indentation level
      /// </summary>
      private const Int32 IndentationWidth = 3;

      public Int32 IndentationLevel {
         get; private set;
      } = 0;

      private Int32 IndentationPosition() => IndentationLevel * IndentationWidth + 2;

      private void OutputAndClearCodeLine()
      {
         CodeBuilder.Append(CodeLine.TrimEnd(' '));
         CodeBuilder.AppendLine();
         CodeLine.Clear();
      }

      public void GenerateStartOfCodeAndCopyCodeToResultBuilder(
         Boolean useTerminalValuesAsFlags,
         Boolean GenerateStateStackInitialCountVariable,
         Boolean GenerateAttributeStackInitialCountVariable)
      {

         StringBuilder ResultPart2 = CodeBuilder;
         CodeBuilder = ResultBuilder;
         Append(GlobalVariables.RegionString.Value).
            Append(' ').
            Append(GlobalVariables.GrammlatorString.Value).
            Append(' ').
            Append(GlobalVariables.GeneratedString.Value).
            Append(' ').
            Append(GlobalVariables.TranslationInfo);

         if (GenerateStateStackInitialCountVariable)
         {
            IndentExactly()
               .Append("Int32 ")
               .Append(GlobalVariables.StateStackInitialCountVariable.Value)
               .Append(" = ")
               .Append(GlobalVariables.StateStack.Value)
               .AppendLine(".Count; ");
         }

         if (GenerateAttributeStackInitialCountVariable)
         {
            IndentExactly()
               .Append("Int32 ")
               .Append(GlobalVariables.AttributeStackInitialCountVariable.Value)
               .Append(" = ")
               .Append(GlobalVariables.AttributeStack.Value)
               .AppendLine(".Count; ");
         }

         // Define constants and method for Flag-Tests

         Int64 MinValue = GlobalVariables.TerminalSymbols[0].EnumValue;
         Int64 MaxValue = GlobalVariables.TerminalSymbols[^1].EnumValue;

         if (GlobalVariables.FlagTestMethodName.Value != "")
         {
            Int64 Offset = useTerminalValuesAsFlags 
               ? 0  // not used
               : MaxValue <= 63 ? 0 : MinValue;
            Boolean IsInFunctionHastoBeGenerated = false;

            foreach (TerminalSymbol t in GlobalVariables.TerminalSymbols)
            {
               if (t.IsUsedInIsIn)
               {
                  IsInFunctionHastoBeGenerated = true; // 
                  String Identifier = t.Identifier;
                  IndentExactly();
                  if (useTerminalValuesAsFlags)
                  {
                     // generate shorter aliases
                     // generate e.g. "const ThreeLetters _fb = ThreeLetters.b;"
                     Append("const ")
                        .Append(GlobalVariables.TerminalSymbolEnum.Value) // "ThreeLetters"
                        .Append(' ') // ' '
                        .Append(GlobalVariables.FlagsPrefix.Value) // "_f"
                        .Append(Identifier) // "b"
                        .Append(" = ") // " = "
                        .AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, Identifier); // "ThreeLetters.b"
                     AppendLine(';'); // ';'
                  }
                  else
                  {
                     // generate e.g. "const Int64 _fb = 1L << (Int32)(LexerResult.CSharpEnd-12);"
                     Append("const Int64 ")
                        .Append(GlobalVariables.FlagsPrefix.Value) //  "_f"
                        .Append(Identifier) // "b"
                        .Append(" = 1L << (Int32)(")
                        .AppendWithPrefix(GlobalVariables.TerminalSymbolEnum.Value, Identifier); // LexerResult.CSharpEnd
                     if (Offset != 0)
                        Append('-')
                        .Append(Offset.ToString());
                     AppendLine(");"); // ");"
                  }
               }
            }

            if (IsInFunctionHastoBeGenerated)
            {
               // generate e.g. "Boolean _IsIn"
               IndentExactly().
               Append("Boolean ");
               Append(GlobalVariables.FlagTestMethodName.Value);

               if (useTerminalValuesAsFlags)
               {
                  // generate e.g. "(ThreeLetters flags) => ((PeekSymbol()) & flags) != 0;"
                  Append("(")
                     .Append(GlobalVariables.TerminalSymbolEnum.Value)
                     .Append(" flags) => ((")
                     .Append(GlobalVariables.SymbolNameOrFunctionCall.Value) // "PeekSymbol()"
                     .AppendLine(") & flags) != 0;");
               }
               else
               {
                  // generate e.g. "(Int64 flags) => (1L << (Int32)((PeekSymbol()) - 0) & flags) != 0;
                  Append("(Int64 flags) => (1L << (Int32)((")
                  .Append(GlobalVariables.SymbolNameOrFunctionCall.Value) // "PeekSymbol()"
                  .Append(')');
                  if (Offset != 0)
                     Append('-')
                     .Append(Offset.ToString());
                  AppendLine(") & flags) != 0;");
               }
            }
         }

         AppendLine(); // write codeline to StringBuilder!

         ResultBuilder.Append(ResultPart2);
         return;
      }

      public void GenerateEndOfRegion()
      {
         Append(GlobalVariables.EndregionString.Value).
            Append(' ').
            Append(GlobalVariables.GrammlatorString.Value).
            Append(' ').
            Append(GlobalVariables.GeneratedString.Value).
            Append(' ').
            AppendLine(GlobalVariables.TranslationInfo);
      }

      public ParserAction? GenerateEndOfCodeAction()
      {
         // The label (if needed) has already been generated
         Indent().AppendLine(';');
         return null;
      }

      public void GenerateStateStackPushWithOptionalLinebreak(Int32 valueToPush)
      {
         IndentExactly();
         Append(GlobalVariables.StateStack.Value);
         AppendWithOptionalLinebreak(".Push(");
         Append(valueToPush);
         Append("); ");
      }

      /// <summary>
      /// AppendLine() if Linelength>column, then fill the line with spaces up to column (no spaces if column==0)
      /// </summary>
      /// <param name="column"></param>
      private void SetCol(Int32 column)
      {
         // new line if length is already beyond column
         if (LineLength > column)
            AppendLine();
         // fill up to column with space 
         CodeLine.Insert(CodeLine.Length, " ", column - LineLength);
      }

      public Int32 LineLength => CodeLine.Length;

      private readonly String[] lineSeparators = new String[] { Environment.NewLine };

      /// <summary>
      /// split <paramref name="sToAppend"/> into lines and append separately
      /// </summary>
      /// <param name="sToAppend">String to break into lines</param>
      /// <param name="Prefix">String to write after indentation before sToAppend</param>
      public void IndentAndAppendLines(String sToAppend, String Prefix)
      {
         Boolean firstLine = true;
         foreach (String s in sToAppend.Split(lineSeparators, StringSplitOptions.None))
         {
            if (!firstLine)
            {
               AppendLineAndIndent();
               Append(Prefix);
            }
            firstLine = false;
            Append(s);
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
      }

      public P5CodegenCS Append(Char c)
      {
         CodeLine.Append(c);
         return this;
      }

      public void AppendLine(Char c)
      {
         CodeLine.Append(c);
         AppendLine();
      }

      public P5CodegenCS AppendWithOptionalLinebreak(Char c)
      {
         if (LineLength > 10 && (LineLength + 1) >= LineLengthLimit)
         {
            AppendLine();
            Indent();
         }
         CodeLine.Append(c);
         return this;
      }

      /// <summary>
      /// Indents exactly before appending the strings
      /// </summary>
      /// <param name="stringsToAppend"></param>
      public P5CodegenCS AppendInstruction(params String[] stringsToAppend)
      {
         IndentExactly();
         AppendWithOptionalLinebreak(stringsToAppend);
         return this;
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
            if (LineLength > 10 && (LineLength + s.Length) >= LineLengthLimit) // TOCHECK LineLength > 10 why 10?
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
         if (LineLength > 10 && (LineLength + 5) >= LineLengthLimit) // TOCHECK (LineLength > 10 && (LineLength + 5) >= LineLengthLimit
            AppendLineAndIndent();
         CodeLine.Append(i);
         return this;
      }

      /// <summary>
      /// append line and then indent exactly
      /// </summary>
      public P5CodegenCS AppendLineAndIndent()
      {
         AppendLine();
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
         AppendLine();
         return this;
      }

      /// <summary>
      /// Indent() and then for each string AppendLineAndIndent, then append line
      /// </summary>
      /// <param name="stringsToAppend"></param>
      public void AppendLineWithOptionalLinebreak(params String[] stringsToAppend)
      {
         AppendWithOptionalLinebreak(stringsToAppend);
         AppendLine();
      }

      /// <summary>
      /// Trim and output the code line, even if empty, and clear the code line
      /// </summary>
      public P5CodegenCS AppendLine()
      {
         OutputAndClearCodeLine();
         return this;
      }

      public P5CodegenCS AppendWithPrefix(String Prefix, String s)
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
         return this;
      }

      /// <summary>
      /// Indent to position given by the indentation level, if actual position is smaller,
      /// else start new line and indent
      /// </summary>
      public P5CodegenCS IndentExactly()
      {
         SetCol(IndentationPosition());
         return this;
      }

      /// <summary>
      /// Set indentation level and indent to maximum of actual lineposition and new indentation position
      /// </summary>
      /// <param name="newIndentation">new indentation level</param>
      public P5CodegenCS Indent(Int32 newIndentation)
      {
         IndentationLevel = newIndentation;
         Indent();
         return this;
      }

      /// <summary>
      /// Indent to maximum of actual lineposition and new indentation position
      /// </summary>
      public P5CodegenCS Indent()
      {
         if (LineLength < IndentationPosition())
            SetCol(IndentationPosition());
         return this;
      }

      public P5CodegenCS IncrementIndentationLevel()
      {
         IndentationLevel++;
         return this;
      }

      public P5CodegenCS DecrementIndentationLevel()
      {
         IndentationLevel--;
         if (IndentationLevel < 0)
            IndentationLevel = 0;
         // throw new ArgumentException("IndentationLevel<0");
         return this;
      }

      public void GenerateBeginOfBlock()
      {
         IndentExactly();
         AppendLine('{');
         // IncrementIndentationLevel();
         //IndentationLevel = NewIndentation;
         //IndentAndAppend('{');
         //AppendLine(); // eine neue Zeile anfangen
      }

      public void GenerateEndOfBlock(String comment)
      {
         // DecrementIndentationLevel();
         IndentExactly();
         CodeLine.Append("}");
         if (!String.IsNullOrEmpty(comment))
            Append(" // ").
               Append(comment);
         AppendLine();
      }

      // keine neue Zeile anfangen

      public void GenerateAcceptInstruction()
          => AppendWithOptionalLinebreak(GlobalVariables.SymbolAcceptInstruction.Value);

      /// <summary>
      /// returns a label built from an action specific string and the actions IdNumber+1
      /// </summary>
      /// <param name="action"></param>
      /// <param name="accept"></param>
      /// <returns>The label assigned to the (accept) action</returns>
      public static String GotoLabel(ParserAction action, Boolean accept)
      {
         String LabelPrefix = ParserEnumExtension.LabelPrefix(action.ParserActionType);
         if (action.ParserActionType == ParserActionEnum.isErrorhaltAction ||
            action.ParserActionType == ParserActionEnum.isEndOfGeneratedCode)
            // There is only one ErrorHaltAction, one end of generated code
            return (accept ? "Accept" : "") + LabelPrefix;
         return (accept ? "Accept" : "") + LabelPrefix + (action.IdNumber + 1).ToString();
      }

      /// <summary>
      /// If <paramref name="action"/> is not <see cref="ErrorhandlingAction"/> then  generate label of NextAction (else of given action).
      /// If <paramref name="action"/> is <see cref="TerminalTransition"/> then generate accept label.
      /// </summary>
      /// <param name="action"></param>
      /// <returns></returns>
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

      public void GenerateLabel(String s)
      {
         SetCol(0);
         CodeLine.Append(s)
             .Append(": ");
         AppendLine();
      }


      public P5CodegenCS GenerateGoto(String label)
      {
         IndentExactly();
         CodeLine.Append("goto ")
             .Append(label);
         AppendLine("; ");
         return this;
      }

      public P5CodegenCS GenerateGoto(ParserAction action, Boolean accept)
      {
         return GenerateGoto(GotoLabel(action, accept));
      }

      public void GenerateIfSPeek(Boolean inverse, Int32 condition)
      {
         IndentExactly();
         if (inverse)
            Append("if (")
               .Append(GlobalVariables.StateStack.Value)
               .Append(".Peek() != ");
         //            AppendWithOptionalLinebreak("if (_s.Peek() != ");
         else
            Append("if (")
               .Append(GlobalVariables.StateStack.Value)
               .Append(".Peek() == ");
         //   AppendWithOptionalLinebreak("if (_s.Peek() == ");
         AppendWithOptionalLinebreak(condition);
         Append(") ");
      }

      public P5CodegenCS GenerateSemanticMethodCall(MethodClass semanticMethod)
      {
         IndentAndAppend(semanticMethod.MethodName);
         Append("(");

         // Generate actual parameters for method call
         IndentationLevel++;

         Int32 count = 0;
         foreach (MethodParameterStruct Parameter in semanticMethod.MethodParameters)
         {
            String ParameterTypeString = GlobalVariables.GetStringOfIndex(Parameter.TypeStringIndex);

            AppendLine();
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
            if (count < semanticMethod.MethodParameters.Length)
               Append(", ");
         }

         if (semanticMethod.MethodParameters.Length > 0)
            IndentExactly();
         Append(")");

         IndentationLevel--;

         return this;
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
            Append("Remove(); ");
         }
         else
         {
            IndentAndAppend(AttributeAccessPrefix);
            Append("Remove(", -AttributkellerKorrektur, "); ");
         }
      }

      /// <summary>
      /// for example "_a."
      /// </summary>
      public readonly String AttributeAccessPrefix = GlobalVariables.AttributeStack.Value + ".";

      /// <summary>
      /// for example "_a.x-"
      /// </summary>
      public readonly String AttributeIndexPrefix = GlobalVariables.AttributeStack.Value + ".x";

      /// <summary>
      /// for example "_", used to modify the type of the methods formal parameter
      /// to a fieldname of the elements of the attribute stack (e.g. modifies "Int32" to "_Int32")
      /// </summary>
      public const Char AttributeTypePrefix = '_';

      /// <summary>
      /// for example "_a.useup("
      /// </summary>
      public readonly String AttributePeekClearPart1 = GlobalVariables.AttributeStack.Value + ".PeekClear(";

      /// <summary>
      /// for example ")."
      /// </summary>
      public const String AttributePeekClearPart2 = ").";

      /// <summary>
      /// for example "_a.useup("
      /// </summary>
      public readonly String AttributePeekRefPart1 = GlobalVariables.AttributeStack.Value + ".PeekRef(";

      /// <summary>
      /// for example "_a.useup("
      /// </summary>
      public readonly String AttributePeekRefClearPart1 = GlobalVariables.AttributeStack.Value + ".PeekRefClear(";

      /// <summary>
      /// for example ")."
      /// </summary>
      public const String AttributePeekRefPart2 = ").";

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
