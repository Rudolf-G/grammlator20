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

         MainUserInterfaceMethods.SetExampleMenus(MenuItemDisplayExample, this, "GrammlatorExamples.zip");
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


      private void MenuItemSaveSource_Click(Object sender, RoutedEventArgs e)
      {
         SaveSource();
      }

      private void MenuItemLoadSourceFile_Click(Object sender, RoutedEventArgs e)
      {
         if (!AllowReplaceSourceBoxTextIfChanged(StatusFlags.SourceTextChanged, "The source textbox has been edited and not yet saved!"))
            return;

         LoadSourcefile();
      }

      private void MenuItemClearSourceTextbox_Click(Object sender, RoutedEventArgs e)
      {
         if (!AllowReplaceSourceBoxTextIfChanged(StatusFlags.SourceTextChanged, "The source textbox has been edited and not yet saved!"))
            return;

         SourceTextBox.Clear();
         SourceFilename = "";
         ActualStatus.SetFlags(
            StatusFlags.SourceHasFilename | StatusFlags.SourceNotEmpty
            | StatusFlags.SourceTextChanged | StatusFlags.SourceTextChangedSinceLastTranslate
            , false);
         ClearAllResultsAndErrorBoxes();
      }

      private void SaveSource()
      {
         if (!SaveFile(SourceFilename, "Save source file", SourceTextBox.Text, true))
            return;

         // Source has been saved (may be with a different filename)

         // TODO mark sourcefile as unchanged (has been saved)

         SourceFilename = SaveSourceOrResultDialog.FileName;
         ActualStatus.SetFlags(StatusFlags.SourceHasFilename, true);
         ActualStatus.SetFlags(
            StatusFlags.SourceTextChanged | StatusFlags.SourceTextChangedSinceLastTranslate,
            false);
      }

      private void MenuItemSaveResult_Click(Object sender, RoutedEventArgs e)
      {
         if (!ActualStatus.TestFlags(StatusFlags.ResultAvailable))
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

         // SaveFile will display warnings if saving to source
         if (!SaveFile(ResultFilename, "Save result", ResultTextBox.Text, false))
            return;

         // Result has been saved

      }

      private bool SaveFile(string lastFileName, string title, string text, bool saveOfSource)
      {
         Debug.Assert(!String.IsNullOrEmpty(title));

         if (lastFileName != "")
         {
            SaveSourceOrResultDialog.InitialDirectory = Path.GetDirectoryName(lastFileName);
            SaveSourceOrResultDialog.FileName = Path.GetFileName(lastFileName);
         }

         SaveSourceOrResultDialog.Title = title;

         /*** Show SaveFileDialog ***/
         Boolean? r = SaveSourceOrResultDialog.ShowDialog();
         if (r != true)
            return false; // canceled by user

         // Did the user select the source file?
         bool SourceFileReplace = SourceFilename != "" && SourceFilename == SaveSourceOrResultDialog.FileName;

         if (!saveOfSource && SourceFileReplace)
         {

            // ask user if source textbox content and source file should be replaced by result
            // a) least problem: no change of textbox since last load
            if (!ActualStatus.TestFlags(StatusFlags.SourceTextChanged))
            {
               if (MessageBoxResult.Cancel == MessageBox.Show("Copy the result to the source text and save to source file? (source has not been edited)",
                  "Save result to source?", MessageBoxButton.OKCancel)
                  )
                  return false;
            }
            // b) minor problem: no change of textbox since last translate (will be included in the result)
            else if (!ActualStatus.TestFlags(StatusFlags.SourceTextChangedSinceLastTranslate))
            {
               if (MessageBoxResult.Cancel == MessageBox.Show("Copy the result to the source text and save it to the source file? (source has been edited and then translated)",
                  "Save result to source?", MessageBoxButton.OKCancel)
                  )
                  return false;
            }
            // c) be careful: change of textbox since last successful translate (will be lost)
            else
            {
               if (MessageBoxResult.Cancel == MessageBox.Show("Copy the result to the source text and save to source file? (source has been edited since last successful translation !)",
                  "Save result to source?", MessageBoxButton.OKCancel)
                  )
                  return false;
            }

            // "Replacing the source file with the result of the last translation will also replace the contents of the source textbox. The textbox has been edited since the last successfull translation!"
            if (MessageBoxResult.Cancel == MessageBox.Show("Copy the result to the source file and then to the source text?",
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
            ActualStatus.SetFlags(StatusFlags.SourceHasFilename, true);
            ActualStatus.SetFlags(
               StatusFlags.SourceTextChanged | StatusFlags.SourceTextChangedSinceLastTranslate,
               false); // has just been saved
         }
         return true;
      }

      private void MenuItemSaveResultToSource_Click(Object sender, RoutedEventArgs e)
      {
         if (!ActualStatus.TestFlags(StatusFlags.ResultAvailable))
         {
            // this should not occur because the "SaveResultToSource" should be disabled
            MessageBox.Show("No result available");
            return;
         }

         if (!AllowReplaceSourceBoxTextIfChanged(StatusFlags.SourceTextChangedSinceLastTranslate,
            "The source textbox has been edited since last successful translation and not yet saved!")
   )
            return;


         SourceTextBox.Text = ResultTextBox.Text;
         SaveSource();
      }

      private void MenuItemTranslateStandard_Click(Object _1, RoutedEventArgs _2)
         => Translate();

      private void MenuItemReloadAndTranslate_Click(Object sender, RoutedEventArgs e)
      {
         if (String.IsNullOrEmpty(SourceFilename))
         {
            MessageBox.Show("unknown filename");
            return;
         }

         if (!AllowReplaceSourceBoxTextIfChanged(StatusFlags.SourceTextChanged, "The source textbox has been edited and not yet saved!"))
            return;

         if (LoadSourcefile())
            Translate();
      }

      private Boolean LoadSourcefile()
      {
         OpenSourceFileDialog.FileName = "";

         if (!String.IsNullOrEmpty(SourceFilename))
         {
            OpenSourceFileDialog.InitialDirectory = Path.GetDirectoryName(SourceFilename);
            OpenSourceFileDialog.FileName = Path.GetFileName(SourceFilename);
         }

         OpenSourceFileDialog.Title = "Load source file";
         Boolean? result = OpenSourceFileDialog.ShowDialog();
         if (result != true)
            return false; // canceled by user

         return ClearResultsAndReadFileToSourceTextbox(); // and set ActualStatus
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

      internal void MenuItemDisplayExample_Click(Object sender, RoutedEventArgs e)
      {
         if (sender is MenuItem menuItem
            && menuItem.HasHeader
            && menuItem.Header is String header)
         {
            // use header as name of zip-file element
            MainUserInterfaceMethods
               .CopyExampleToInfoTextBox(header, InfoTextBox);

            GrammlatorTabControl.SelectedIndex = 7;
            OnFocusTextBox(new FocusTextBoxEventArgs(InfoTextBox));
            return;
         }
         // error !!!
         return;
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

      private StringBuilder InfoBuilder = new StringBuilder(1000);
      private void MenuItemDisplaySettingsCompact_Click(Object sender, RoutedEventArgs e)
      {
         String Delimiter;

         InfoBuilder
            .Clear()
            .AppendLine("The grammlator settings are:")
            .AppendLine();

         foreach (Setting s in GlobalSettings.VisibleSettings)
         {
            if (s.HasType == Setting.SettingType.StringType)
               Delimiter = "\"";
            else
               Delimiter = "";

            InfoBuilder
               .Append ("//| ")
               .Append(s.Name).Append(": ")
               .Append(Delimiter).Append(s.InitialValueAsString).Append(Delimiter)
               .AppendLine(";");
         }
         InfoTextBox.Text = InfoBuilder.ToString();
         GrammlatorTabControl.SelectedIndex = 7;
         OnFocusTextBox(new FocusTextBoxEventArgs(InfoTextBox));
      }

      private void MenuItemDisplayVersion_Click(Object sender, RoutedEventArgs e)
      {
         InfoBuilder
            .Clear()
            .AppendLine("Grammlator version info:")
            .AppendLine()
            .AppendLine(GlobalVariables.GetVersioninfo);

         InfoTextBox.Text = InfoBuilder.ToString();
         GrammlatorTabControl.SelectedIndex = 7;
         OnFocusTextBox(new FocusTextBoxEventArgs(InfoTextBox));

      }



      private void SourceTextBox_TextChanged(object sender, TextChangedEventArgs args)
      {
         ActualStatus.SetFlags(
            StatusFlags.SourceTextChanged | StatusFlags.SourceTextChangedSinceLastTranslate, true
            );
         TextBox tb = (TextBox)sender;
         ActualStatus.SetFlags(StatusFlags.SourceNotEmpty, tb.LineCount >= 5);
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
      /// While handling a double click on an error TextBox a call of
      /// SourceTextBox.Focus() has no effect.
      /// This problem is solved by <see cref="OnFocusTextBox(FocusTextBoxEventArgs)"/>,
      /// which raises an event. This event is handled by the MainWindow.
      /// </summary>
      /// <param name="e">typically the source textbox</param>
      protected virtual void OnFocusTextBox(FocusTextBoxEventArgs e)
      {
         FocusTextBox?.Invoke(this, e); // invokes HandleFocusTextBox
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
         // Set cursor of the source textbox
         if (sender is TextBox tb)
         {
            // The name of the error textbox contains its number, which is used to index ErrorPositions
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

   }
}
