using Microsoft.Win32;

using System;
using System.Diagnostics;
using System.IO;
using System.Security.Policy;
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

         ActualStatus = new StatusStruct(StatusChanged); // Initial State: no flags are set
         ActualStatus.SetAction(StatusChanged);

         Title = "grammlator - no file -";
         FocusTextBox += HandleFocusTextBox;
         OnFocusTextBox(new FocusTextBoxEventArgs(SourceTextBox));

         string SaveDirectory = SaveSourceOrResultDialog.InitialDirectory;
         // do not offer the directory containing the assembly as default initial directory
         if (SaveDirectory == GlobalVariables.AssemblyFullPath)
            SaveSourceOrResultDialog.InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);

         string OpenSourceDirectory = OpenSourceFileDialog.InitialDirectory;
         // do not offer the directory containing the assembly as default initial directory
         if (OpenSourceDirectory == GlobalVariables.AssemblyFullPath)
            OpenSourceFileDialog.InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
      }

      const String fileFilter = "cs files (*.cs)|*.cs|All files (*.*)|*.*";

      readonly OpenFileDialog OpenSourceFileDialog = new OpenFileDialog() {
         AddExtension = false,
         ReadOnlyChecked = true,
         FilterIndex = 2,
         Filter = fileFilter
      };

      readonly SaveFileDialog SaveSourceOrResultDialog = new SaveFileDialog() {
         AddExtension = false,
         FilterIndex = 2,
         Filter = fileFilter
      };


      private bool SaveFile(string lastFileName, string title, string text, bool allowSourceAsDestination)
      {
         Debug.Assert(!String.IsNullOrEmpty(title));

         if (lastFileName != "")
         {
            SaveSourceOrResultDialog.InitialDirectory = Path.GetDirectoryName(lastFileName);
            SaveSourceOrResultDialog.FileName = Path.GetFileName(lastFileName);
         }

         SaveSourceOrResultDialog.Title = title;
         Boolean? r = SaveSourceOrResultDialog.ShowDialog();
         if (r != true)
            return false; // canceled by user

         // Did the user select the source file?
         bool SourceFileReplace = SourceFilename != "" && SourceFilename == SaveSourceOrResultDialog.FileName;

         if (!allowSourceAsDestination && SourceFileReplace)
         {
            // ask user if source textbox contens and source file should be replaced by result
            if (MessageBoxResult.Cancel == MessageBox.Show("Copy the result to the source text and save to source file?",
               "Save result to source?", MessageBoxButton.OKCancel)
               )
               return false;
            // Copy result to source to stay consistent
            SourceTextBox.Text = ResultTextBox.Text;
         }


         try
         {
            File.WriteAllText(SaveSourceOrResultDialog.FileName, text);
         }
         catch (Exception ex)
         {
            MessageBox.Show($"Error: Could not write to file. Original error: {ex.Message}");
            return false;
         }

         if (SourceFileReplace)
         {
            SourceFilename = SaveSourceOrResultDialog.FileName;
            ActualStatus.SetFlags(StatusFlag.SourceHasFilename, true);
            ActualStatus.SetFlags(StatusFlag.SourceTextChanged, false); // has just been saved
         }
         return true;
      }

      private void MenuItemSaveSource_Click(Object sender, RoutedEventArgs e)
      {
         SaveSource();
      }

      private void MenuItemSaveResultToSource_Click(Object sender, RoutedEventArgs e)
      {
         if (!ActualStatus.TestFlags(StatusFlag.ResultAvailable))
         {
            // this should not occur because the "SaveResultToSource" should be disabled
            MessageBox.Show("No result available");
            return;
         }

         SourceTextBox.Text = ResultTextBox.Text;
         SaveSource();
      }

      private void SaveSource()
      {
         if (!SaveFile(SourceFilename, "Save source file", SourceTextBox.Text, true))
            return;

         // Source has been saved (may be with a different filename)

         // TODO mark sourcefile as unchanged (has been saved)

         SourceFilename = SaveSourceOrResultDialog.FileName;
         ActualStatus.SetFlags(StatusFlag.SourceHasFilename, true);
         ActualStatus.SetFlags(StatusFlag.SourceTextChanged, false);
      }

      private void MenuItemSaveResult_Click(Object sender, RoutedEventArgs e)
      {
         if (!ActualStatus.TestFlags(StatusFlag.ResultAvailable))
         {
            // this should not occur because the "SaveResult" should be disabled
            MessageBox.Show("No result available");
            return;
         };

         // Preset ResultFilename with SourceFilename+"-generated"
         String ResultFilename = "";
         if (SourceFilename != "")
            ResultFilename =
                Path.GetDirectoryName(SourceFilename) + "\\" +
                Path.GetFileNameWithoutExtension(SourceFilename) + "-generated" +
                Path.GetExtension(SourceFilename);


         if (!SaveFile(ResultFilename, "Save result", ResultTextBox.Text, false))
            return;

         // Result has been saved

      }


      private void MenuItemTranslateStandard_Click(Object _1, RoutedEventArgs _2)
         => Translate();

      private void MenuItemLoadSourceFile_Click(Object sender, RoutedEventArgs e)
      {
         LoadSourcefile();
      }

      private Boolean LoadSourcefile()
      {

         OpenSourceFileDialog.FileName = "";

         if (!String.IsNullOrEmpty(SourceFilename))
         {
            OpenSourceFileDialog.InitialDirectory = Path.GetDirectoryName(SourceFilename);
         }

         OpenSourceFileDialog.Title = "Load source file";
         Boolean? result = OpenSourceFileDialog.ShowDialog();
         if (result != true)
            return false; // canceled by user

         return ClearResultsAndReadFileToSourceTextbox(); // and set ActualStatus
      }

      private void MenuItemReloadAndTranslate_Click(Object sender, RoutedEventArgs e)
      {
         if (String.IsNullOrEmpty(SourceFilename))
         {
            MessageBox.Show("unknown filename");
            return;
         }

         if (LoadSourcefile())
            Translate();
      }

      private void CompareIgnoringSeparators_Click(Object _1, RoutedEventArgs _2)
      {
         /* Compare result and source 
          * and set both cursors to the first lines which are different.
          * Thereby ignore text after #region and after #endregion up to the end of the line
          * because this may contain the time of the grammlator translation which is not relevant
         */

         CompareSourceAndResultSpan(SourceTextBox.Text, ResultTextBox.Text,
             out ReadOnlySpan<Char> differingSourceLine, out ReadOnlySpan<Char> differingResultLine,
             out Int32 sourceDiffIndex, out Int32 resultDiffIndex);

         SetCursorTo(0, SourceTextBox, sourceDiffIndex, differingSourceLine.Length < 3 ? 3 : differingSourceLine.Length);
         SetCursorTo(6, ResultTextBox, resultDiffIndex, differingResultLine.Length < 3 ? 3 : differingResultLine.Length);
         GrammlatorTabControl.SelectedIndex = 0;
         OnFocusTextBox(new FocusTextBoxEventArgs(SourceTextBox));
      }

      private void DisplayExample_Click(Object sender, RoutedEventArgs e)
      {
         String FileFullPath = AppContext.BaseDirectory + "GrammlatorConsoleExample.txt";
         bool exists = File.Exists(FileFullPath);
         if (exists)
         {
            using var reader = new StreamReader(FileFullPath);
            InfoTextBox.Text = reader.ReadToEnd();
         }
         else
            InfoTextBox.Text = "File not found: " + FileFullPath;
         GrammlatorTabControl.SelectedIndex = 7;
         OnFocusTextBox(new FocusTextBoxEventArgs(InfoTextBox));
      }

      private void DisplaySettings_Click(Object sender, RoutedEventArgs e)
      {
         StringBuilder InfoBuilder = new StringBuilder(1000);
         String Delimiter;

         InfoBuilder.AppendLine("The grammlator settings are:").AppendLine();

         foreach (Setting s in GlobalSettings.VisibleSettings)
         {
            if (s.HasType == Setting.SettingType.StringType)
               Delimiter = "\"";
            else
               Delimiter = "";

            InfoBuilder.Append(s.Name).Append(": ")
               .Append(Delimiter).Append(s.InitialValueAsString).Append(Delimiter)
               .AppendLine(";")
               .Append("/* ").Append(s.Description).AppendLine(" */")
               .AppendLine();
         }
         InfoTextBox.Text = InfoBuilder.ToString();
         GrammlatorTabControl.SelectedIndex = 7;
         OnFocusTextBox(new FocusTextBoxEventArgs(InfoTextBox));
      }

      private void SourceTextBox_MouseEnter(Object sender, System.Windows.Input.MouseEventArgs e)
      {
         SourceTextBox.Focus();
      }

      private void ResultTextBox_MouseEnter(Object sender, System.Windows.Input.MouseEventArgs e)
      {
         ResultTextBox.Focus();
      }

      public class FocusTextBoxEventArgs : EventArgs {
         public FocusTextBoxEventArgs(TextBox? box)
         {
            Box = box;
         }
         public TextBox? Box;
      }

      public event EventHandler<FocusTextBoxEventArgs> FocusTextBox;

      /// <summary>
      /// While handling a click on a TextBox it is not possible 
      /// to give focus to another textbox by "AnotherBox.Focus()".
      /// This problem is solved by <see cref="OnFocusTextBox(FocusTextBoxEventArgs)"/>,
      /// which raises an event. This event is handled by the MainWindow.
      /// </summary>
      /// <param name="e"></param>
      protected virtual void OnFocusTextBox(FocusTextBoxEventArgs e)
      {
         FocusTextBox?.Invoke(this, e);
      }

      void HandleFocusTextBox(object? sender, FocusTextBoxEventArgs e)
      {
         e.Box?.Focus();
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

      private void SourceTextBox_TextChanged(object sender, TextChangedEventArgs args)
      {
         ActualStatus.SetFlags(StatusFlag.SourceTextChanged, true);
         ActualStatus.SetFlags(StatusFlag.SourceNotEmpty, (sender as TextBox)?.LineCount > 10);
      }
   }
}
