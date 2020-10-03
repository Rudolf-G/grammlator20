using Microsoft.Win32;
using System;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Windows;
using System.Windows.Controls;

namespace grammlator {
   /// <summary>
   /// Interaction logic for MainWindow.xaml
   /// </summary>
   public partial class MainWindow : Window {
      public MainWindow()
      {
         InitializeComponent();
         Title = "grammlator - no file -";
         SetInitiaStatus();
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

      readonly SaveFileDialog SaveSourceOrResultDialog = new SaveFileDialog() {
         AddExtension = false,
         FilterIndex = 2,
         Filter = fileFilter
      };

      
      private bool SaveFile(string fileName, string title, string text)
      {
         Debug.Assert(!String.IsNullOrEmpty(title));

         if (fileName != "")
         {
            SaveSourceOrResultDialog.InitialDirectory = Path.GetDirectoryName(fileName);
            SaveSourceOrResultDialog.FileName = Path.GetFileName(fileName);
         }

         SaveSourceOrResultDialog.Title = title;
         Boolean? r = SaveSourceOrResultDialog.ShowDialog();
         if (r != true)
            return false; // canceled by user
         try
         {
            File.WriteAllText(SaveSourceOrResultDialog.FileName, text);
            SetStatusHavingSourceFilename(SaveSourceOrResultDialog.FileName);
         }
         catch (Exception ex)
         {
            MessageBox.Show($"Error: Could not write to file. Original error: {ex.Message}");
            return false;
         }

         return true;
      }

      private void MenuItemSaveSource_Click(Object sender, RoutedEventArgs e)
      {
         SaveFile(SourceFilename, "Save source file", SourceTextBox.Text);
         // SaveFile does: SetStatusHavingSourceFilename(SaveSourceOrResultDialog.FileName);
      }

      private void MenuItemSaveResult_Click(Object sender, RoutedEventArgs e)
      {
         if (!TranslationSucceeded)
            return; // Click should have been disabled, but be shure not to save incomplete result

         String ResultFilename = "";
         if (!String.IsNullOrEmpty(SourceFilename))
            ResultFilename =
                Path.GetDirectoryName(SourceFilename) + "\\" +
                Path.GetFileNameWithoutExtension(SourceFilename) + "-generated" +
                Path.GetExtension(SourceFilename);

         SaveFile(ResultFilename, "Save result", ResultTextBox.Text); // TOCHECK: Has source been replaced?

      }

      private void MenuItemSaveResultToSource_Click(Object sender, RoutedEventArgs e)
      {
         if (!TranslationSucceeded)
            return; // Click should have been disabled, but be shure not to save incomplete result

         SourceTextBox.Text = ResultTextBox.Text;
         MenuItemSaveSource_Click(sender, e); // 
      }

      private void MenuItemTranslateStandard_Click(Object _1, RoutedEventArgs _2)
         => Translate();

      private void MenuItemLoadSourceFile_Click(Object sender, RoutedEventArgs e)
      {
         if (String.IsNullOrEmpty(SourceFilename))
         {
            OpenSourceFileDialog.FileName = "";
         }
         else
         {
            OpenSourceFileDialog.InitialDirectory = Path.GetDirectoryName(SourceFilename);
            OpenSourceFileDialog.FileName = Path.GetFileName(SourceFilename);
         }

         OpenSourceFileDialog.Title = "Load source file";
         Boolean? result = OpenSourceFileDialog.ShowDialog();
         if (result != true)
            return; // canceled by user

         ClearResultsAndReadFileToSourceTextbox();
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

         foreach (Setting s in GlobalVariables.VisibleSettings)
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
         if (MenuItemTranslateStandard == null)
            return;

         if ((sender as TextBox)?.LineCount > 10)
            MenuItemTranslateStandard.IsEnabled = true;
         else
            MenuItemTranslateStandard.IsEnabled = false;
      }
   }
}
