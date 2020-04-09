using Grammlator;
using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace grammlator {
   /// <summary>
   /// Interaction logic for MainWindow.xaml
   /// </summary>
   public partial class MainWindow: Window {
      public MainWindow()
         {
         InitializeComponent();
         Title="grammlator - no file -";
         MenuItemTranslateStandard.IsEnabled=false;
         MenuItemReloadAndTranslate.IsEnabled=false;
         }

      const string fileFilter = "cs files (*.cs)|*.cs|All files (*.*)|*.*";
      readonly OpenFileDialog OpenSourceFileDialog = new OpenFileDialog() {
         AddExtension=false, ReadOnlyChecked=true,
         FilterIndex=2, Filter=fileFilter
         };
      readonly SaveFileDialog SaveSourceOrResultDialog = new SaveFileDialog() {
         AddExtension=false,
         FilterIndex=2, Filter=fileFilter
         };

      String
         SourceFilename = string.Empty,
         ResultFilename = string.Empty;

      private readonly StringBuilder
         Resultbuilder = new StringBuilder(100_000),
         Log = new StringBuilder(2_000);

      private const Int32 errorLimit = 15; // TODO allow user to set the errorLimit
      private Int32 warnings, errors, firstErrorIndex;
      private Boolean aborted;

      private void ClearAllResults()
         {
         RemoveErrorBoxes();
         SymbolsTextBox.Clear();
         ConflictsTextBox.Clear();
         States1TextBox.Clear();
         States2TextBox.Clear();

         ResultFilename="no_result"; // ResultFilename will be set after successfull translation

         Resultbuilder.Clear();
         LogTextBox.Clear();
         Log.Clear();
         warnings=0;
         errors=0;
         firstErrorIndex=-1;
         aborted=false;
         }

      private void MenuItemSaveResult_Click(Object sender, RoutedEventArgs e)
         {
         SaveSourceOrResultDialog.Title="Save result";
         if (String.IsNullOrEmpty(ResultFilename))
            {
            // no source filename known
            SaveSourceOrResultDialog.FileName="";
            }
         else
            {
            // use the ResultFilename , which was set on last translate
            SaveSourceOrResultDialog.InitialDirectory=Path.GetDirectoryName(ResultFilename);
            SaveSourceOrResultDialog.FileName=Path.GetFileName(ResultFilename);
            }

         bool? r = SaveSourceOrResultDialog.ShowDialog();

         if (r!=true)
            {
            return;
            }

         try
            {
            File.WriteAllText(SaveSourceOrResultDialog.FileName, ResultTextBox.Text);
            ResultFilename=SaveSourceOrResultDialog.FileName;
            }
         catch (Exception ex)
            {
            MessageBox.Show($"Error: Could not write to file. Original error: {ex.Message}");
            }

         }

      private void MenuItemSaveSource_Click(Object sender, RoutedEventArgs e)
         {

         SaveSourceOrResultDialog.Title="Save source file";
         String saveFileFilename = SourceFilename; // may be ""

         if (String.IsNullOrEmpty(saveFileFilename))
            {
            // no source filename known
            SaveSourceOrResultDialog.FileName="";
            }
         else
            {
            // use the ResultFilename , which was set on last translate
            SaveSourceOrResultDialog.InitialDirectory=Path.GetDirectoryName(saveFileFilename);
            SaveSourceOrResultDialog.FileName=Path.GetFileName(saveFileFilename);
            }

         bool? r = SaveSourceOrResultDialog.ShowDialog();

         if (r!=true)
            {
            return;
            }

         try
            {
            File.WriteAllText(SaveSourceOrResultDialog.FileName, SourceTextBox.Text);
            UseFilenameInDialogs(SaveSourceOrResultDialog.FileName);
            }
         catch (Exception ex)
            {
            MessageBox.Show($"Error: Could not write to file. Original error: {ex.Message}");
            }
         }

      private void MenuItemTranslateStandard_Click(Object sender, RoutedEventArgs e)
         => Translate();

      private void MenuItemLoadSourceFile_Click(Object sender, RoutedEventArgs e)
         {
         OpenSourceFileDialog.Title="Load source file";
         if (String.IsNullOrEmpty(SourceFilename))
            {
            OpenSourceFileDialog.FileName="";
            }
         else
            {
            OpenSourceFileDialog.InitialDirectory=Path.GetDirectoryName(SourceFilename);
            OpenSourceFileDialog.FileName=Path.GetFileName(SourceFilename);
            }

         bool? result = OpenSourceFileDialog.ShowDialog();
         if (result!=true)
            {
            return;
            }

         ClearResultsAndReadFileToSourceTextbox();

         }

      private Boolean ClearResultsAndReadFileToSourceTextbox()
         {
         Boolean success = true;
         try
            {
            ClearAllResults();

            SourceTextBox.Text=File.ReadAllText(OpenSourceFileDialog.FileName);
            SetCursorTo(0, SourceTextBox, 0, 0); // show source file with cursor at first position

            UseFilenameInDialogs(OpenSourceFileDialog.FileName);
            }
         catch (Exception ex)
            {
            success=false;
            MessageBox.Show($"Error: Could not read from file. Original error: {ex.Message}");
            }

         return success;
         }

      private void UseFilenameInDialogs(String fileName)
         {
         // Save name of the source file for later use
         SourceFilename=fileName;
         // Show  name of source file in caption of main window
         this.Title=SourceFilename;
         // enable "reload source ... " toolstrip item
         MenuItemTranslateStandard.IsEnabled=true;
         MenuItemReloadAndTranslate.IsEnabled=true;
         }


      internal void SetCursorTo(Int32 tabIndex, TextBox box, Int32 position, Int32 length = 3)
         {
         if (position>=box.Text.Length-length)
            position=box.Text.Length-length-1;
         if (position<0)
            position=0;
         GrammlatorTabControl.SelectedIndex=tabIndex; // Select destination to enable ScrollToCaret();
         // Box. .Select(); // give focus to the box (and select the contents)
         box.Select(position, length);
         Int32 line = box.GetLineIndexFromCharacterIndex(position);
         box.ScrollToLine(line);
         }

      private void Translate()
         {
         Int64 TranslateStart = DateTime.Now.Ticks; // will be reset after "using(var SourceReader...)
         Int64 TranslateEnd = DateTime.Now.Ticks; // will be reset in finally clause
         ClearAllResults();

         ResultFilename="Errors_in_Source"; // default, is replaced after successfull translation

         try
            {
            var source = new ReadOnlyMemory<char>(SourceTextBox.Text.ToCharArray());
            var SourceReader = new SpanReaderWithCharacterAndLineCounter(source);
            // Translate   ----- HERE WE GO -----
            TranslateStart=DateTime.Now.Ticks;
            Phases1to5.Execute(Resultbuilder, SourceReader, BufferMessage, BufferPositionAndMessage);

            if (errors>0)
               {
               AppendLine(Log, "<<<< Error break:   ", "translation finished with {errors} errors in source");
               }
            else
               {
               // Set ResultFilename only after successfull translation
               ResultFilename=
                   Path.GetDirectoryName(SourceFilename)+"\\"+
                   Path.GetFileNameWithoutExtension(SourceFilename)+"-generated"+
                   Path.GetExtension(SourceFilename);
               }
            }
         catch (ErrorInSourcedataException e)
            {
            // Show message in log
            AppendLine(Log, "<<<< Error break:   ", referenceToBox(e.Message));
            // show message in new Errorbox
            if (errors++==0)
               firstErrorIndex=ErrorPositions.Count;
            AddErrorBox(e.Message, e.ErrorPosition.Position);
            }
         catch (Exception ex)
            {
            AppendLine(Log, "<<<< translation aborted, reason: ", ex.Message);
            }
         finally
            {
            TranslateEnd=DateTime.Now.Ticks;
            }
         // Copy the result buffer to the ResultTextBox and clear the buffer
         String ResultText = Resultbuilder.ToString();
         Resultbuilder.Clear();

         ResultTextBox.Text=ResultText;

         // TODO LbItem1.Content = xxx. firstErrorMessage; ???

         // Display statistics
         BufferMessage(MessageTypeOrDestinationEnum.Information,
             $"Input  contains {SourceTextBox.Text.Length,7} characters"); // in {SourceTextBox.GetLineFromCharIndex(Int32.MaxValue),5} lines.");
         BufferMessage(MessageTypeOrDestinationEnum.Information,
             $"Result contains {ResultText.Length,7} characters"); // {ResultTextBox.GetLineFromCharIndex(Int32.MaxValue),5} lines.");


         // If there is an error move the cursor in SourceTextBox to the position of the error
         if (firstErrorIndex>=0&&firstErrorIndex<ErrorPositions.Count)
            {
            // move the cursor in SourceTextBox to the position of the first error
            SetCursorTo(0, SourceTextBox, ErrorPositions[firstErrorIndex], length: 5);
            }
         else if (ErrorPositions.Count>0)
            SetCursorTo(0, SourceTextBox, ErrorPositions[0], length: 5);
         else
            SetCursorTo(0, SourceTextBox, SourceTextBox.Text.Length-5, length: 5);

         BufferMessage(MessageTypeOrDestinationEnum.Information,
             $"There have been {errors} error{(errors==1 ? "" : "s")} and {warnings} warning{(warnings==1 ? "" : "s")}.");

         BufferMessage(MessageTypeOrDestinationEnum.Information,
             $"Execution time (without I/O):  {(TranslateEnd-TranslateStart)/10000} msec.");

         if (aborted)
            AppendLine(Log, "<<<< translation not completed (exception) ", "");
         else if (errors>0)
            AppendLine(Log, "<<<< translation not completed (errors)  ", "");

         // Show the buffered messages in the MessagesTextBox and clear the buffer
         LogTextBox.Text=Log.ToString();

         Log.Clear();
         SetCursorTo(1, LogTextBox, 0, 0);

         // Show source end select first one if there is a box with error message,  else show log
         if (firstErrorIndex>=0)
            {
            lb.SelectedIndex=0;
            lb.SelectedIndex=firstErrorIndex;
            lb.ScrollIntoView(lb.SelectedItem);
            GrammlatorTabControl.SelectedIndex=0; // Source
            }
         else
            GrammlatorTabControl.SelectedIndex= 1; // Log
         }

      private void MenuItemReloadAndTranslate_Click(Object sender, RoutedEventArgs e)
         {
         if (String.IsNullOrEmpty(SourceFilename))
            {
            MessageBox.Show("unknown filename");
            }
         else if (ClearResultsAndReadFileToSourceTextbox())
            {
            Translate();
            }
         }

      private readonly List<Int32> ErrorPositions = new List<int>();

      private void RemoveErrorBoxes()
         {
         ItemCollection ic = lb.Items;
         ic.Clear();
         ErrorPositions.Clear();
         }

      FontFamily StandardFont = new FontFamily("Consolas");
      private void AddErrorBox(String text, Int32 position)
         {
         ErrorPositions.Add(position);

         var tb = new TextBox {
            Name='E'+lb.Items.Count.ToString(),
            Text=text,
            Width=lb.ActualWidth,
            IsReadOnly=true,
            IsEnabled=true,
            AcceptsReturn=true,
            AcceptsTab=true,
            UseLayoutRounding=true,
            Padding=new Thickness(5, 5, 5, 5),
            FontFamily=StandardFont,
            TextWrapping=TextWrapping.Wrap
            };
         tb.GotFocus+=TextBox_GotFocus;

         lb.Items.Add(tb);
         }

      private void TextBox_GotFocus(Object sender, RoutedEventArgs e)
         {
         if (sender is TextBox tb)
            {
            Int32 Index = Int32.Parse(tb.Name.AsSpan(1));
            if (Index<ErrorPositions.Count)
               SetCursorOfSourceTextbox(ErrorPositions[Index]);
            }
         else
            MessageBox.Show("sender is not a textbox ");
         }

      private void SetCursorOfSourceTextbox(Int32 position)
         {
         GrammlatorTabControl.SelectedIndex=0;
         if (position>=0)
            SetCursorTo(0, SourceTextBox, position, length: 5);
         else
            SetCursorTo(0, SourceTextBox, 0);
         }

      private void SourceTextBox_MouseEnter(Object sender, System.Windows.Input.MouseEventArgs e)
         {
         SourceTextBox.Focus();
         }

      private void CompareIgnoringSeparators_Click(Object sender, RoutedEventArgs e)
         {
         /* Compare result and source 
          * and set both cursors to the first lines which are different.
          * Thereby ignore text after #region and after #endregion up to the end of the line
          * because this may contain the time of the grammlator translation which is not relevant
         */

         CompareSourceAndResult(SourceTextBox.Text, ResultTextBox.Text,
             out SubStringStruct differingSourceLine, out SubStringStruct differingResultLine);

         SetCursorTo(0, SourceTextBox, differingSourceLine.StartIndex, differingSourceLine.Length<3 ? 3 : differingSourceLine.Length);
         SetCursorTo(6, ResultTextBox, differingResultLine.StartIndex, differingResultLine.Length<3 ? 3 : differingResultLine.Length);
         }

      private void lb_SizeChanged(Object sender, SizeChangedEventArgs e)
         { // Adjust Width of contained TextBox (may be contained in a ListBoxItem)
         ListBox lb = sender as ListBox;
         if (lb==null)
            return;
         foreach (object o in lb.Items)
            if (o is TextBox tb)
               tb.Width=lb.ActualWidth;
            else if (o is ListBoxItem li&&li.Content is TextBox containedTb)
               {
               // li.Width is OK !
               containedTb.Width=li.ActualWidth;
               }
         // Is too large if there is a scrollbar
         }

      private static void AppendLine(StringBuilder sb, String s1, String s2)
         => sb.Append(s1).AppendLine(s2);

      /// <summary>
      /// form shortened message with reference to not yet created messagebox
      /// </summary>
      /// <param name="m"></param>
      /// <returns></returns>
      string referenceToBox(String m)
         => m.AsSpan(0, 35).ToString()
            +$"...   see error messages {ErrorPositions.Count+1}";



      /// <summary>
      /// Write message into <see langword="abstract"/>buffer which will be displayed later in the UI.
      /// If possible BufferPositionAndMessage(...) should be used!
      /// </summary>
      /// <param name="f"></param>
      /// <param name="message"></param>
      private void BufferMessage(MessageTypeOrDestinationEnum f, String message)
         => BufferPositionAndMessage(f, message, new STextPosition(-1, -1, 0));

      /// <summary>
      /// Store messagetype, message and text position for later display in UI.
      /// Throw exception if MessagetypeEnum.Abort or AbortIfErrors (if errors>0).
      /// </summary>
      /// <param name="messageType">noMessageType, Status, Information, Warning, Error or Abort</param>
      /// <param name="message"></param>
      /// <param name="pos">Position of the input file, where the error occured</param>
      /// <exception cref="ErrorInSourcedataException">will be thrown if Abort</exception>
      private void BufferPositionAndMessage(MessageTypeOrDestinationEnum messageType, String message, STextPosition pos)
         {

         // local method to add line and column number to message
         String MessageIncludingPosition()
            => pos.LineNumber<0 ?
            message :
            $"Line {pos.LineNumber+1,5} column {pos.ColumnNumber+1,3} {message}";

         // local method to format the message type for output
         string messageHeader()
            => $"---- {messageType.ToString()+":",-15}";

         switch (messageType)
            {

         // Messages directed to special TextBoxes
         case MessageTypeOrDestinationEnum.SymbolProtocol:
            SymbolsTextBox.Text=message;
            return;

         case MessageTypeOrDestinationEnum.ConflictProtocol:
            ConflictsTextBox.Text=message;
            return;

         case MessageTypeOrDestinationEnum.StateProtocol1:
            States1TextBox.Text=message;
            return;

         case MessageTypeOrDestinationEnum.StateProtocol2:
            States2TextBox.Text=message;
            return;

         // Status ("found... grammlator line"), Error and Abort messages
         case MessageTypeOrDestinationEnum.Error:
            if (errors++==0)
               firstErrorIndex=ErrorPositions.Count;
            if (errors>=errorLimit)
               throw new ErrorInSourcedataException($"More than {errorLimit} messages");
            goto case MessageTypeOrDestinationEnum.Status;

         case MessageTypeOrDestinationEnum.Status:
            // display shortened message in log and complete message in ErrorList
            AppendLine(Log, messageHeader(), referenceToBox(MessageIncludingPosition()));
            AddErrorBox(MessageIncludingPosition(), pos.Position);
            return;

         case MessageTypeOrDestinationEnum.AbortIfErrors:
            if (errors<=0)
               return;
            goto case MessageTypeOrDestinationEnum.Abort;

         case MessageTypeOrDestinationEnum.Abort:
            // The exception handler will display the message
            throw new ErrorInSourcedataException(pos, MessageIncludingPosition());

         // Information, Warning, noMessageTyp (noMessageTyp should not be used)
         default:
            // show complete message in log but not in ErrorList
            AppendLine(Log, messageHeader(), MessageIncludingPosition());
            return;
            }
         }

      /// <summary>
      /// Compares the source and the result texts ignoring separator characters at start and at end of lines
      /// and ignoring text the rest of lines starting with #region or #endregion.
      /// </summary>
      /// <param name="s1">the 1st string to be compared with the other</param>
      /// <param name="s2">the 2nd string to be compared with the other</param>
      /// <param name="s1Line">The line (subString) of s1 which is different or at end of s1</param>
      /// <param name="s2Line">The line (subString) of s2 which is different or at end of s1</param>
      /// <returns><see langword="true"/> if s1 and s1 are equal (ignoring separators and special lines)</returns>
      private static Boolean CompareSourceAndResult(String s1, String s2,
          out SubStringStruct s1Line, out SubStringStruct s2Line)
         {
         var s1Remain = new SubStringStruct(s1);
         var s2Remain = new SubStringStruct(s2);

         s1Line=s1Remain.SelectUpTo('\r'); // initial values for case s1="" or s2='""
         s2Line=s2Remain.SelectUpTo('\r');

         while (s1Remain.Length>0&&s2Remain.Length>0)
            {
            // Get (next) lines = part 
            s1Remain=s1Remain.IgnoreLeadingChar('\n');
            s2Remain=s2Remain.IgnoreLeadingChar('\n');
            s1Line=s1Remain.SelectUpTo('\r');
            s2Line=s2Remain.SelectUpTo('\r');
            // Skip these lines in the origins and skip next '\n' if present
            s1Remain=s1Remain.Skip(s1Line.Length);
            s2Remain=s2Remain.Skip(s2Line.Length);
            // Skip '\n' if present and trim actual lines
            s1Line=s1Line.IgnoreLeadingSeparators().IgnoreTrailingChar('\r').IgnoreTrailingSeparators();
            s2Line=s2Line.IgnoreLeadingSeparators().IgnoreTrailingChar('\r').IgnoreTrailingSeparators();
            // Compare lines
            if (s1Line==s2Line)
               continue; // lines are equal
                         // Test if special lines and allow them to differ after the keywords
            if (s1Line.StartsWith(GlobalVariables.RegionString)
                &&s2Line.StartsWith(GlobalVariables.RegionString))
               {
               continue;
               }

            if (s1Line.StartsWith(GlobalVariables.EndregionString)
                &&s2Line.StartsWith(GlobalVariables.EndregionString))
               {
               continue;
               }

            return false; // the lines differ
            }
         return true;
         }

      }
   }
