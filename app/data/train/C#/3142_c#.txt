using ExifLibrary;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Windows.Forms;

namespace ImageSort {
    enum DirOpt {
        NONE,
        DAY,
        MONTH,
        YEAR
    }

    enum FileRule {
        ORIGINAL,
        INCREMENT
    }

    struct userSelection {
        public string source;
        public string destination;
        public bool move;
        public string directoryFormat;
        public FileRule fileRule;
        public bool recursive;
        public bool log;
        public bool remove;
        public bool pause;
    }

    struct workerState {
        public string progressLabel;
        public int numFiles;
        public int filesProcessed;
    }

    public partial class MainForm : Form {
        /// <summary>
        /// Class constructor.
        /// Directory format and filename rule comboboxes are initialised.
        /// </summary>
        public MainForm() {
            InitializeComponent();

            filenameRule.SelectedIndex = 0;
            directoryFormat1.SelectedIndex = 0;
            directoryFormat2.SelectedIndex = 0;
            directoryFormat3.SelectedIndex = 0;
        }

        /// <summary>
        /// Asynchronous worker thread. This method uses the Worker class along with the
        /// StreamWriter class to loop through the discovered files and write a log file
        /// if the user requested one.
        /// </summary>
        /// <param name="sender">The worker thread object</param>
        /// <param name="e">Arguments passed to the worker thread</param>
        private void workerThread_DoWork(object sender, DoWorkEventArgs e) {
            int files = 0;      // Number of files discovered (also used for the loop)
            int progress = 0;   // Progress through the files (expected to increment to files)
            int processed = 0;  // Number of files processed (`COPY` or `MOVE` flag, not incremented with `NONE`)
            string log = "";    // The file log string. Newlines must be replaced before being written
            bool success = true;// True if the current file processed successfully
            userSelection user = (userSelection)e.Argument;

            using (Worker worker = new Worker(sender, e)) {
                for (files = 0; files < worker.state.numFiles; files++, processed++) {
                    // Process the current file in the enumerated list
                    success = worker.ProcessFile((worker.user.move ? 0 : files), ++progress);

                    if (!success) {
                        if (processed != 0) {
                            processed--;
                        }
                        continue;
                    }

                    // Add to the file operation log
                    log += String.Format("{0:00}:{1:00}:{2:00} {3} `{4}` -> `{5}`\n", DateTime.Now.Hour, DateTime.Now.Minute,
                        DateTime.Now.Second, success ? (user.move ? "[MOVE]" : "[COPY]") : ("[NONE]"), worker.currentSource, worker.currentDest);
                }
            }

            if (user.log) {
                // Using the StreamWriter class, write a log of the operations completed to a new
                // file, in the format `YYYY-MM:DD HH-MM-SS.log`. This file is created in the
                // Application startup directory.
                using (StreamWriter streamWriter = new StreamWriter(String.Format(@"{0}\{1:0000}-{2:00}-{3:00} {4:00}-{5:00}-{6:00}.log",
                    Application.StartupPath, DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day, DateTime.Now.Hour, DateTime.Now.Minute,
                    DateTime.Now.Second))) {
                    streamWriter.WriteLine(String.Format("Number of files discovered: {0}", files));
                    streamWriter.WriteLine(String.Format("Files {0}: {1}\n", user.move ? "moved" : "copied", processed));
                    streamWriter.Write(log.Replace("\n", Environment.NewLine));
                }
            }
        }

        /// <summary>
        /// Updates the progress bar and progress label when the background thread
        /// reports that the progress has changed.
        /// </summary>
        private void workerThread_ProgressChanged(object sender, System.ComponentModel.ProgressChangedEventArgs e) {
            workerState progress = (workerState)e.UserState;

            progressLabel.Text = progress.progressLabel;
            if (progress.numFiles != progressBar.Maximum) {
                progressBar.Maximum = progress.numFiles;
            }
            progressBar.Value = progress.filesProcessed;
        }

        /// <summary>
        /// Returns the controls to their previous enabled state
        /// upon background thread completion.
        /// </summary>
        private void workerThread_RunWorkerCompleted(object sender, System.ComponentModel.RunWorkerCompletedEventArgs e) {
            ChangeControlState(true);
        }

        /// <summary>
        /// Configures the folderBrowser object to request a source directory.
        /// Upon closure the selected path is inserted into the sourceDirectory textbox.
        /// </summary>
        private void sourceBrowseButton_Click(object sender, System.EventArgs e) {
            folderBrowser.Description = "Select the direcotry where your assorted images are stored.";
            folderBrowser.ShowNewFolderButton = false;
            folderBrowser.ShowDialog();
            sourceDirectory.Text = folderBrowser.SelectedPath;
        }

        /// <summary>
        /// Configures the folderBrowser object to request a destination directory.
        /// Upon closure the selected path is inserted into the destination textbox.
        /// </summary>
        private void destinationButton_Click(object sender, System.EventArgs e) {
            folderBrowser.Description = "Select the directory where the images should be moved/copied to.";
            folderBrowser.ShowNewFolderButton = true;
            folderBrowser.ShowDialog();
            destination.Text = folderBrowser.SelectedPath;
        }

        /// <summary>
        /// Changes the state of the pause option depending on the check state
        /// of the remove option.
        /// </summary>
        private void optionRemoveDuplicates_CheckedChanged(object sender, System.EventArgs e) {
            if (optionRemoveDuplicates.Checked) {
                optionPauseOnDuplicate.Enabled = false;
            } else {
                optionPauseOnDuplicate.Enabled = true;
            }
        }

        /// <summary>
        /// Changes the control states when the selected filename rule changes
        /// in order to enable and disable duplicate options.
        /// </summary>
        private void filenameRule_SelectedIndexChanged(object sender, EventArgs e) {
            ChangeControlState(true);
        }

        /// <summary>
        /// This method handles the validation and preparation of the main form
        /// when the main control button is clicked by the user. This button
        /// causes the background thread to begin.
        /// </summary>
        private void controlButton_Click(object sender, System.EventArgs e) {
            switch (controlButton.Text) {
                case "Start":
                    string dirFormat = "";
                    string sourceDir = "";
                    string destDir = "";

                    // Validate directory entries
                    if (!Directory.Exists(sourceDirectory.Text)) {
                        MessageBox.Show("The source directory could not be found.",
                            "Unable to find directory", MessageBoxButtons.OK, MessageBoxIcon.Error);
                        return;
                    } else if (!Directory.Exists(destination.Text)) {
                        MessageBox.Show("The destination directory could not be found.",
                            "Unable to find directory", MessageBoxButtons.OK, MessageBoxIcon.Error);
                        return;
                    } else if (sourceDirectory.Text == destination.Text && (directoryFormat1.SelectedIndex == 0 && directoryFormat2.SelectedIndex == 0 && directoryFormat3.SelectedIndex == 0)) {
                        MessageBox.Show("Please select source and destination directories that differ.",
                            "Identical directories", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                        return;
                    }

                    // Destination directory formatting
                    if ((DirOpt)directoryFormat1.SelectedIndex != DirOpt.NONE)
                        dirFormat += String.Format(@"{0}\", (DirOpt)directoryFormat1.SelectedIndex);
                    if ((DirOpt)directoryFormat2.SelectedIndex != DirOpt.NONE)
                        dirFormat += String.Format(@"{0}\", (DirOpt)directoryFormat2.SelectedIndex);
                    if ((DirOpt)directoryFormat3.SelectedIndex != DirOpt.NONE)
                        dirFormat += String.Format(@"{0}\", (DirOpt)directoryFormat3.SelectedIndex);

                    // Insert trailing slashes if not found
                    sourceDir = sourceDirectory.Text.EndsWith(@"\") ? sourceDirectory.Text : sourceDirectory.Text + @"\";
                    destDir = destination.Text.EndsWith(@"\") ? destination.Text : destination.Text + @"\";

                    // Prepare the environment for start
                    Directory.CreateDirectory(destDir);
                    ChangeControlState(false);

                    userSelection user = new userSelection() {
                        source = sourceDir,
                        destination = destDir,
                        move = optionMove.Checked ? true : false,
                        directoryFormat = dirFormat,
                        fileRule = (FileRule)filenameRule.SelectedIndex,
                        recursive = optionRecursive.Checked ? true : false,
                        log = optionLog.Checked ? true : false,
                        remove = optionRemoveDuplicates.Checked ? true : false,
                        pause = optionPauseOnDuplicate.Checked ? true : false
                    };

                    workerThread.RunWorkerAsync(user);
                    break;
                case "Stop":
                    // TODO: Cancel Async operation
                    break;
            };

        }

        /// <summary>
        /// Changes the state of all the user-interactable controls on the
        /// form to the specified value.
        /// </summary>
        /// <param name="value">True if all the controls are to be enabled.</param>
        private void ChangeControlState(bool value) {
            sourceDirectory.Enabled = value;
            destination.Enabled = value;
            optionMove.Enabled = value;
            optionCopy.Enabled = value;
            sourceBrowseButton.Enabled = value;
            destinationButton.Enabled = value;
            directoryFormat1.Enabled = value;
            directoryFormat2.Enabled = value;
            directoryFormat3.Enabled = value;
            filenameRule.Enabled = value;
            optionRecursive.Enabled = value;
            optionLog.Enabled = value;
            controlButton.Text = value ? "Start" : "Stop";

            if (filenameRule.SelectedIndex == 1) {
                optionRemoveDuplicates.Enabled = true;
                optionPauseOnDuplicate.Enabled = true;
            } else {
                optionRemoveDuplicates.Enabled = false;
                optionPauseOnDuplicate.Enabled = false;
            }
        }
    }
}
