using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Text;
using System.Windows.Controls;

namespace grammlator {
   static class MainUserInterfaceMethods {

      /// <summary>
      /// Replaces all submenus of the topMenu. The new submenus are the names of the
      /// files in the zip archive. This archive must be in the base directory of grammlator.
      /// The click event of each submenu is routed to mainWindow.MenuItemDisplayExample_Click
      /// </summary>
      /// <param name="topMenu">The top menu</param>
      /// <param name="mainWindow">The main window of grammlator</param>
      internal static void SetExampleMenus(MenuItem topMenu, MainWindow mainWindow, String zipFilename)
      {
         String ZipPath = AppContext.BaseDirectory + zipFilename;
         if (!File.Exists(ZipPath))
            return;

         using ZipArchive archive = ZipFile.OpenRead(ZipPath);

         topMenu.Items.Clear();

         foreach (ZipArchiveEntry entry in archive.Entries)
         {
            String entryName = entry.Name;
            var newSubMenu = new MenuItem();
            newSubMenu.Header = entryName;
            newSubMenu.Click += new System.Windows.RoutedEventHandler(mainWindow.MenuItemDisplayExample_Click);
            topMenu.Items.Add(newSubMenu);
         }
      }

      internal static void CopyExampleToInfoTextBox(String entryName, TextBox box)
      {
         try
         {
            String ZipPath = AppContext.BaseDirectory + "GrammlatorExamples.zip";
            if (!File.Exists(ZipPath))
               return;
            using ZipArchive archive = ZipFile.OpenRead(ZipPath)!;
            ZipArchiveEntry entry = archive.GetEntry(entryName)!;
            using var reader = new StreamReader(entry.Open());
            box.Text = reader.ReadToEnd();
         }
         catch { }
      }
   }
}
