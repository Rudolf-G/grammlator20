using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Windows;
using System.Windows.Controls;

using Grammlator;

using Microsoft.Win32;

namespace grammlator {
   public partial class MainWindow {

      readonly SaveFileDialog SaveSourceOrResultDialog = new SaveFileDialog() {
         AddExtension = false,
         FilterIndex = 2,
         Filter = fileFilter
      };

      private void SourceTextBox_TextChanged(object sender, TextChangedEventArgs args)
      {
         if (MenuItemTranslateStandard == null)
            return;

         if ((sender as TextBox)?.LineCount > 10)
            MenuItemTranslateStandard.IsEnabled = true;
         else
            MenuItemTranslateStandard.IsEnabled = false;
      }

      private void MenuItemSaveResult_Click(Object sender, RoutedEventArgs e)
      {
         SaveSourceOrResultDialog.Title = "Save result";
         if (String.IsNullOrEmpty(ResultFilename))
         {
            // no source filename known
            SaveSourceOrResultDialog.FileName = "";
         }
         else
         {
            // use the ResultFilename , which was set on last translate
            SaveSourceOrResultDialog.InitialDirectory = Path.GetDirectoryName(ResultFilename);
            SaveSourceOrResultDialog.FileName = Path.GetFileName(ResultFilename);
         }

         Boolean? r = SaveSourceOrResultDialog.ShowDialog();

         if (r != true)
         {
            return;
         }

         try
         {
            File.WriteAllText(SaveSourceOrResultDialog.FileName, ResultTextBox.Text);
            ResultFilename = SaveSourceOrResultDialog.FileName;
         }
         catch (Exception ex)
         {
            MessageBox.Show($"Error: Could not write to file. Original error: {ex.Message}");
         }

      }

      private void MenuItemSaveSource_Click(Object sender, RoutedEventArgs e)
      {

         SaveSourceOrResultDialog.Title = "Save source file";
         String saveFileFilename = SourceFilename; // may be ""

         if (String.IsNullOrEmpty(saveFileFilename))
         {
            // no source filename known
            SaveSourceOrResultDialog.FileName = "";
         }
         else
         {
            // use the ResultFilename , which was set on last translate
            SaveSourceOrResultDialog.InitialDirectory = Path.GetDirectoryName(saveFileFilename);
            SaveSourceOrResultDialog.FileName = Path.GetFileName(saveFileFilename);
         }

         Boolean? r = SaveSourceOrResultDialog.ShowDialog();

         if (r != true)
         {
            return;
         }

         try
         {
            File.WriteAllText(SaveSourceOrResultDialog.FileName, SourceTextBox.Text);
            EnableMenuItemsStoreFileName(SaveSourceOrResultDialog.FileName);
         }
         catch (Exception ex)
         {
            MessageBox.Show($"Error: Could not write to file. Original error: {ex.Message}");
         }
      }

      private void MenuItemTranslateStandard_Click(Object _1, RoutedEventArgs _2)
         => Translate();

      private void MenuItemLoadSourceFile_Click(Object sender, RoutedEventArgs e)
      {
         OpenSourceFileDialog.Title = "Load source file";
         if (String.IsNullOrEmpty(SourceFilename))
         {
            OpenSourceFileDialog.FileName = "";
         }
         else
         {
            OpenSourceFileDialog.InitialDirectory = Path.GetDirectoryName(SourceFilename);
            OpenSourceFileDialog.FileName = Path.GetFileName(SourceFilename);
         }

         Boolean? result = OpenSourceFileDialog.ShowDialog();
         if (result != true)
         {
            return;
         }

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

      private void ShowSettings_Click(Object sender, RoutedEventArgs e)
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
   }
}
