﻿using Grammlator;

using Microsoft.Win32;

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace grammlator {
   /// <summary>
   /// Interaction logic for MainWindow.xaml
   /// </summary>
   public partial class MainWindow : Window {
      public MainWindow()
      {
         InitializeComponent();
         Title = "grammlator - no file -";
         MenuItemReloadAndTranslate.IsEnabled = false;
         FocusTextBox += HandleFocusTextBox;
         OnFocusTextBox(new FocusTextBoxEventArgs(SourceTextBox));
      }

      const String fileFilter = "cs files (*.cs)|*.cs|All files (*.*)|*.*";
      readonly OpenFileDialog OpenSourceFileDialog = new OpenFileDialog() {
         AddExtension = false,
         ReadOnlyChecked = true,
         FilterIndex = 2,
         Filter = fileFilter
      };

      String
         SourceFilename = String.Empty,
         ResultFilename = String.Empty;

      private readonly StringBuilder
         Resultbuilder = new StringBuilder(100_000),
         Log = new StringBuilder(2_000);

      private const Int32 errorLimit = 15; // TODO allow user to set the errorLimit
      private Int32 warnings, errors, firstErrorIndex;
      private Boolean aborted;

      readonly Stopwatch Watch = new Stopwatch();

      private void ClearAllResults()
      {
         RemoveErrorBoxes();
         SymbolsTextBox.Clear();
         ConflictsTextBox.Clear();
         States1TextBox.Clear();
         States2TextBox.Clear();

         ResultFilename = "no_result"; // ResultFilename will be set after successfull translation

         Resultbuilder.Clear();
         LogTextBox.Clear();
         Log.Clear();
         warnings = 0;
         errors = 0;
         firstErrorIndex = -1;
         aborted = false;
      }

      private Boolean ClearResultsAndReadFileToSourceTextbox()
      {
         Boolean success = true;
         try
         {
            ClearAllResults();

            SourceTextBox.Text = File.ReadAllText(OpenSourceFileDialog.FileName);

            // show source file with cursor at first position
            SetCursorTo(0, SourceTextBox, 0, 0);
            GrammlatorTabControl.SelectedIndex = 0;
            OnFocusTextBox(new FocusTextBoxEventArgs(SourceTextBox));

            EnableMenuItemsStoreFileName(OpenSourceFileDialog.FileName);
         }
         catch (Exception ex)
         {
            success = false;
            MessageBox.Show($"Error: Could not read from file. Original error: {ex.Message}");
         }

         return success;
      }

      private void EnableMenuItemsStoreFileName(String fileName)
      {
         // Save name of the source file for later use
         SourceFilename = fileName;
         // Show  name of source file in caption of main window
         this.Title = SourceFilename;
         // enable "reload source ... " toolstrip item
         MenuItemTranslateStandard.IsEnabled = true;
         MenuItemReloadAndTranslate.IsEnabled = true;
      }

      internal void SetCursorTo(Int32 tabIndex, TextBox box, Int32 position, Int32 length = 3)
      {
         if (position >= box.Text.Length - length)
            position = box.Text.Length - length - 1;
         if (position < 0)
            position = 0;

         GrammlatorTabControl.SelectedIndex = tabIndex;
         box.Select(position, length);
         Int32 lineIndex = box.GetLineIndexFromCharacterIndex(position);
         if (lineIndex < 0)
            return;
         box.ScrollToLine(lineIndex);
      }

      private void Translate()
      {
         ClearAllResults();

         ResultFilename = "Errors_in_Source"; // default, is replaced after successfull translation

         try
         {
            var source = new ReadOnlyMemory<Char>(SourceTextBox.Text.ToCharArray());
            var SourceReader = new SpanReaderWithCharacterAndLineCounter(source);
            // Translate   ----- HERE WE GO -----
            Watch.Restart();
            Phases1to5.Execute(Resultbuilder, SourceReader, BufferMessage, BufferPositionAndMessage);

            if (errors > 0)
            {
               AppendLine(Log, "<<<< Error break:   ", "translation finished with {errors} errors in source");
            }
            else
            {
               // Set ResultFilename only after successfull translation
               ResultFilename =
                   Path.GetDirectoryName(SourceFilename) + "\\" +
                   Path.GetFileNameWithoutExtension(SourceFilename) + "-generated" +
                   Path.GetExtension(SourceFilename);
            }
         }
         catch (ErrorInSourcedataException e)
         {
            // Show message in log
            AppendLine(Log, "<<<< Error break:   ", ReferenceToBox(e.Message));
            // show message in new Errorbox
            if (errors++ == 0)
               firstErrorIndex = ErrorPositions.Count;
            AddErrorBox(e.Message, e.Position);
         }
         catch (Exception ex)
         {
            AppendLine(Log, "<<<< translation aborted, reason: ", ex.Message);
         }
         finally
         {
            Watch.Stop();
         }
         // Copy the result buffer to the ResultTextBox and clear the buffer
         String ResultText = Resultbuilder.ToString();
         Resultbuilder.Clear();

         ResultTextBox.Text = ResultText;

         // Display statistics
         BufferMessage(MessageTypeOrDestinationEnum.Information,
             $"Input  contains {SourceTextBox.Text.Length,7} characters, {SourceTextBox.Text.CountLines(),7} lines");
         BufferMessage(MessageTypeOrDestinationEnum.Information,
             $"Result contains {ResultText.Length,7} characters, {ResultText.CountLines(),7} lines");

         // If there is an error move the cursor in SourceTextBox to the position of the error
         if (firstErrorIndex >= 0 && firstErrorIndex < ErrorPositions.Count)
         {
            // move the cursor in SourceTextBox to the position of the first error
            SetCursorTo(0, SourceTextBox, ErrorPositions[firstErrorIndex], length: 5);
         }
         else if (ErrorPositions.Count > 0)
            SetCursorTo(0, SourceTextBox, ErrorPositions[0], length: 5);
         else
            SetCursorTo(0, SourceTextBox, SourceTextBox.Text.Length - 5, length: 5);

         BufferMessage(MessageTypeOrDestinationEnum.Information,
             $"There have been {errors} error{(errors == 1 ? "" : "s")} and {warnings} warning{(warnings == 1 ? "" : "s")}.");

         BufferMessage(MessageTypeOrDestinationEnum.Information,
             $"Execution time (without I/O):  {Watch.ElapsedMilliseconds} msec.");

         if (aborted)
            AppendLine(Log, "<<<< translation not completed (exception) ", "");
         else if (errors > 0)
            AppendLine(Log, "<<<< translation not completed (errors)  ", "");

         // Show the buffered messages in the MessagesTextBox and clear the buffer
         LogTextBox.Text = Log.ToString();

         Log.Clear();
         SetCursorTo(1, LogTextBox, 0, 0);

         // Show source end select first one if there is a box with error message,  else show log
         if (firstErrorIndex >= 0)
         {
            GrammlatorListBox.SelectedIndex = 0;
            GrammlatorListBox.SelectedIndex = firstErrorIndex;
            GrammlatorListBox.ScrollIntoView(GrammlatorListBox.SelectedItem);
            GrammlatorTabControl.SelectedIndex = 0; // Source
         }
         else
            GrammlatorTabControl.SelectedIndex = 1; // Log
      }

      private readonly List<Int32> ErrorPositions = new List<Int32>();

      private void RemoveErrorBoxes()
      {
         ItemCollection ic = GrammlatorListBox.Items;
         ic.Clear();
         ErrorPositions.Clear();
      }

      readonly FontFamily StandardFont = new FontFamily("Consolas");
      private void AddErrorBox(String text, Int32 position)
      {
         ErrorPositions.Add(position);

         var tb = new TextBox {
            Name = 'E' + GrammlatorListBox.Items.Count.ToString(),
            Text = text,
            Width = GrammlatorListBox.ActualWidth - ListboxDistanceAtRight,
            IsReadOnly = true,
            IsEnabled = true,
            AcceptsReturn = true,
            AcceptsTab = true,
            UseLayoutRounding = true,
            Padding = new Thickness(5, 5, 5, 5),
            FontFamily = StandardFont,
            TextWrapping = TextWrapping.Wrap,
            HorizontalScrollBarVisibility = ScrollBarVisibility.Auto,
            VerticalScrollBarVisibility = ScrollBarVisibility.Disabled,
            HorizontalAlignment = HorizontalAlignment.Stretch,
            VerticalAlignment = VerticalAlignment.Top,
            HorizontalContentAlignment = HorizontalAlignment.Left,
            VerticalContentAlignment = VerticalAlignment.Top
         };
         tb.GotFocus += ErrorBox_GotFocus;
         tb.MouseDoubleClick += ErrorBox_DoubleClick;

         GrammlatorListBox.ClipToBounds = true;
         GrammlatorListBox.Items.Add(tb);

      }

      private void ErrorBox_DoubleClick(Object sender, RoutedEventArgs e)
      {
         OnFocusTextBox(new FocusTextBoxEventArgs(SourceTextBox));
      }

      private void ErrorBox_GotFocus(Object sender, RoutedEventArgs e)
      {
         if (sender is TextBox tb)
         {
            Int32 Index = Int32.Parse(tb.Name.AsSpan(1));
            if (Index < ErrorPositions.Count)
               SetCursorOfSourceTextbox(ErrorPositions[Index]);
         }
      }

      private void SetCursorOfSourceTextbox(Int32 position)
      {
         GrammlatorTabControl.SelectedIndex = 0;
         if (position >= 0)
            SetCursorTo(0, SourceTextBox, position, length: 5);
         else
            SetCursorTo(0, SourceTextBox, 0);
      }


      const Int32 ListboxDistanceAtRight = 35; // used to avoid horizontal scrollbar in Listbox

      private void ListBox_SizeChanged(Object sender, SizeChangedEventArgs _)
      { // Adjust Width of contained TextBox (may be contained in a ListBoxItem)
         if (!(sender is ListBox lb))
            return;

         foreach (Object? o in lb.Items)
            if (o is TextBox tb)
               tb.Width = lb.ActualWidth - ListboxDistanceAtRight; // try to avoid horizontal scrollbar in Listbox
            else if (o is ListBoxItem li && li.Content is TextBox containedTb)
            {
               // li.Width is OK !
               containedTb.Width = li.ActualWidth - ListboxDistanceAtRight;
            }
      }

      private static void AppendLine(StringBuilder sb, String s1, String s2)
         => sb.Append(s1).AppendLine(s2);

      /// <summary>
      /// returns shortened message with reference to not yet created next messagebox
      /// </summary>
      /// <param name="m"></param>
      /// <returns></returns>
      String ReferenceToBox(String m)
       => m.AsSpan(0, m.Length > 35 ? 35 : m.Length).ToString()
          + $"...   see message box {ErrorPositions.Count + 1}";

      /// <summary>
      /// Write message into <see langword="abstract"/>buffer which will be displayed later in the UI.
      /// If possible BufferPositionAndMessage(...) should be used!
      /// </summary>
      /// <param name="f"></param>
      /// <param name="message"></param>
      private void BufferMessage(MessageTypeOrDestinationEnum f, String message)
         => BufferPositionAndMessage(f, message, 0);

      /// <summary>
      /// Store messagetype, message and text position for later display in UI.
      /// Throw exception if MessagetypeEnum.Abort or AbortIfErrors (if errors>0).
      /// </summary>
      /// <param name="messageType">noMessageType, Status, Information, Warning, Error or Abort</param>
      /// <param name="message"></param>
      /// <param name="pos">Position of the input file, where the error occured</param>
      /// <exception cref="ErrorInSourcedataException">will be thrown if Abort</exception>
      private void BufferPositionAndMessage(MessageTypeOrDestinationEnum messageType, String message, Int32 pos)
      {
         Int32 LineNumber, ColumnNumber;
         String MessageIncludingPosition = message;

         if (pos >= 0)
         {
            GrammlatorTabControl.SelectedIndex = 0; // without correct selection of the TabControl with the SourceTextBox the program will crash
            if (pos >= SourceTextBox.Text.Length)
               pos = SourceTextBox.Text.Length - 1;
            LineNumber = SourceTextBox.GetLineIndexFromCharacterIndex(pos);
            ColumnNumber = pos - SourceTextBox.GetCharacterIndexFromLineIndex(LineNumber);
            if (ColumnNumber < 0)
               ColumnNumber = 0; // repair strange results of GetCharacterIndexFromLineIndex
            MessageIncludingPosition = $"Line {LineNumber + 1,5} column {ColumnNumber + 1,3} {message}";
         }
         else
            pos = 0;

         // local method to format the message type for output
         String messageHeader()
            => messageType == MessageTypeOrDestinationEnum.Error
            ? $"--<< {messageType.ToString() + ":",-15}"
            : $"---- {messageType.ToString() + ":",-15}";

         switch (messageType)
         {

         // Messages directed to special TextBoxes
         case MessageTypeOrDestinationEnum.SymbolProtocol:
            SymbolsTextBox.Text = message;
            return;

         case MessageTypeOrDestinationEnum.ConflictProtocol:
            ConflictsTextBox.Text = message;
            return;

         case MessageTypeOrDestinationEnum.StateProtocol1:
            States1TextBox.Text = message;
            return;

         case MessageTypeOrDestinationEnum.StateProtocol2:
            States2TextBox.Text = message;
            return;

         // Status ("found... grammlator line"), Error and Abort messages
         case MessageTypeOrDestinationEnum.Error:
            if (errors++ == 0)
               firstErrorIndex = ErrorPositions.Count;
            if (errors >= errorLimit)
               throw new ErrorInSourcedataException($"More than {errorLimit} messages");
            goto case MessageTypeOrDestinationEnum.Status;

         case MessageTypeOrDestinationEnum.Status:
            // display shortened message in log and complete message in ErrorList
            AppendLine(Log, messageHeader(), ReferenceToBox(MessageIncludingPosition));
            AddErrorBox(MessageIncludingPosition, pos);
            return;

         case MessageTypeOrDestinationEnum.AbortIfErrors:
            if (errors <= 0)
               return;
            goto case MessageTypeOrDestinationEnum.Abort;

         case MessageTypeOrDestinationEnum.Abort:
            // The exception handler will display the message
            throw new ErrorInSourcedataException(pos, MessageIncludingPosition);

         // Information, Warning, noMessageTyp (noMessageTyp should not be used)
         default:
            // show complete message in log but not in ErrorList
            AppendLine(Log, messageHeader(), MessageIncludingPosition);
            return;
         }
      }

      private static Boolean CompareSourceAndResultSpan(ReadOnlySpan<Char> s1, ReadOnlySpan<Char> s2,
         out ReadOnlySpan<Char> s1Line, out ReadOnlySpan<Char> s2Line,
         out Int32 s1Index, out Int32 s2Index)
      {
         var s1Remain = s1;
         var s2Remain = s2;

         s1Line = ReadOnlySpan<Char>.Empty;
         s2Line = ReadOnlySpan<Char>.Empty;

         s1Index = 0;
         s2Index = 0;

         Int32 eol1Index, eol2Index;

         while (s1Remain.Length > 0 && s2Remain.Length > 0)
         {

            // determine index of line in the respective Span
            s1Index = s1.Length - s1Remain.Length;
            s2Index = s2.Length - s2Remain.Length;

            // compare the next lines and remove them from the remainder spans
            // assume that '\r' or '\r''\n' are line spearators (but not '\n')

            // find end of line
            eol1Index = s1Remain.IndexOf('\r');
            eol2Index = s2Remain.IndexOf('\r');

            if (eol1Index >= 0)
            {
               s1Line = s1Remain.Slice(0, eol1Index); // without trailing '\r'
               s1Remain = s1Remain[(s1Line.Length + 1)..]; // skip line and trailing '\r'
            }
            else
            {
               s1Line = s1Remain;
               s1Remain = ReadOnlySpan<Char>.Empty;
            }

            if (eol2Index >= 0)
            {
               s2Line = s2Remain.Slice(0, eol2Index); // without trailing '\r'
               s2Remain = s2Remain[(s2Line.Length + 1)..]; // skip line and trailing '\r'
            }
            else
            {
               s2Line = s2Remain;
               s2Remain = ReadOnlySpan<Char>.Empty;
            }


            // In remainders if present skip leading '\n' as part of preceding line separator (ignore diffences of line separators)
            if (s1Remain.Length > 0 && s1Remain[0] == '\n')
               s1Remain = s1Remain[1..];
            if (s2Remain.Length > 0 && s2Remain[0] == '\n')
               s2Remain = s2Remain[1..];

            // Trim lines to ignore leading and trailing whitespace
            var s1LineTrimmed = s1Line.Trim();
            var s2LineTrimmed = s2Line.Trim();

            // Compare lines
            if (s1LineTrimmed.SequenceEqual(s2LineTrimmed))
               continue; // lines are equal

            // Test if special lines and allow them to differ after the keywords
            if (s1LineTrimmed.StartsWith(GlobalVariables.RegionString.Value)
                && s2LineTrimmed.StartsWith(GlobalVariables.RegionString.Value))
            {
               continue;
            }

            if (s1LineTrimmed.StartsWith(GlobalVariables.EndregionString.Value)
                && s2LineTrimmed.StartsWith(GlobalVariables.EndregionString.Value))
            {
               continue;
            }

            return false; // the lines differ
         }
         return s1Remain.Length == 0 && s2Remain.Length == 0;
      }

   }
}
