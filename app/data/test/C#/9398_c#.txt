namespace GoogleAppEngineLauncher
{
    partial class FmMain
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FmMain));
            this.listApp = new System.Windows.Forms.ListView();
            this.chStatus = new System.Windows.Forms.ColumnHeader();
            this.chAppName = new System.Windows.Forms.ColumnHeader();
            this.chPath = new System.Windows.Forms.ColumnHeader();
            this.chPort = new System.Windows.Forms.ColumnHeader();
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.FileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.newApplicationToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addExistingApplicationToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.removeApplicationToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.terminalToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openApplicationDirectoryToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.editApplicationSettingsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
            this.preferencesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.quitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.controlCToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.runToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.runStrictToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.stopToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.browseToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.logsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.sDKConsoleToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator4 = new System.Windows.Forms.ToolStripSeparator();
            this.deployToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.dashboardToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.helpHToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.aboutToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.checkForUpdatesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.btnDashboard = new System.Windows.Forms.Button();
            this.btnDeploy = new System.Windows.Forms.Button();
            this.btnEdit = new System.Windows.Forms.Button();
            this.btnBrowse = new System.Windows.Forms.Button();
            this.btnRemove = new System.Windows.Forms.Button();
            this.btnAdd = new System.Windows.Forms.Button();
            this.btnStop = new System.Windows.Forms.Button();
            this.btnStart = new System.Windows.Forms.Button();
            this.btnConsole = new System.Windows.Forms.Button();
            this.rollbackToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.browseAppspotToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.menuStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // listApp
            // 
            this.listApp.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.listApp.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.chStatus,
            this.chAppName,
            this.chPath,
            this.chPort});
            this.listApp.HideSelection = false;
            this.listApp.Location = new System.Drawing.Point(13, 85);
            this.listApp.MultiSelect = false;
            this.listApp.Name = "listApp";
            this.listApp.Size = new System.Drawing.Size(694, 287);
            this.listApp.TabIndex = 0;
            this.listApp.UseCompatibleStateImageBehavior = false;
            this.listApp.View = System.Windows.Forms.View.Details;
            // 
            // chStatus
            // 
            this.chStatus.Text = "";
            this.chStatus.Width = 20;
            // 
            // chAppName
            // 
            this.chAppName.Text = "Name";
            this.chAppName.Width = 100;
            // 
            // chPath
            // 
            this.chPath.Text = "Path";
            this.chPath.Width = 400;
            // 
            // chPort
            // 
            this.chPort.Text = "Port";
            // 
            // menuStrip1
            // 
            this.menuStrip1.Font = new System.Drawing.Font("MS UI Gothic", 9F);
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.FileToolStripMenuItem,
            this.controlCToolStripMenuItem,
            this.helpHToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(719, 24);
            this.menuStrip1.TabIndex = 10;
            this.menuStrip1.Text = "menuStrip1";
            // 
            // FileToolStripMenuItem
            // 
            this.FileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.newApplicationToolStripMenuItem,
            this.addExistingApplicationToolStripMenuItem,
            this.removeApplicationToolStripMenuItem,
            this.toolStripSeparator1,
            this.terminalToolStripMenuItem,
            this.openApplicationDirectoryToolStripMenuItem,
            this.editApplicationSettingsToolStripMenuItem,
            this.toolStripSeparator3,
            this.preferencesToolStripMenuItem,
            this.toolStripSeparator2,
            this.quitToolStripMenuItem});
            this.FileToolStripMenuItem.Font = new System.Drawing.Font("MS UI Gothic", 9F);
            this.FileToolStripMenuItem.Name = "FileToolStripMenuItem";
            this.FileToolStripMenuItem.Size = new System.Drawing.Size(51, 20);
            this.FileToolStripMenuItem.Text = "File(&F)";
            // 
            // newApplicationToolStripMenuItem
            // 
            this.newApplicationToolStripMenuItem.Name = "newApplicationToolStripMenuItem";
            this.newApplicationToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.N)));
            this.newApplicationToolStripMenuItem.Size = new System.Drawing.Size(270, 22);
            this.newApplicationToolStripMenuItem.Text = "New Application...";
            this.newApplicationToolStripMenuItem.Click += new System.EventHandler(this.btnAdd_Click);
            // 
            // addExistingApplicationToolStripMenuItem
            // 
            this.addExistingApplicationToolStripMenuItem.Enabled = false;
            this.addExistingApplicationToolStripMenuItem.Name = "addExistingApplicationToolStripMenuItem";
            this.addExistingApplicationToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)(((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Shift)
                        | System.Windows.Forms.Keys.N)));
            this.addExistingApplicationToolStripMenuItem.Size = new System.Drawing.Size(270, 22);
            this.addExistingApplicationToolStripMenuItem.Text = "Add Existing Application...";
            // 
            // removeApplicationToolStripMenuItem
            // 
            this.removeApplicationToolStripMenuItem.Name = "removeApplicationToolStripMenuItem";
            this.removeApplicationToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Delete)));
            this.removeApplicationToolStripMenuItem.Size = new System.Drawing.Size(270, 22);
            this.removeApplicationToolStripMenuItem.Text = "Remove Application...";
            this.removeApplicationToolStripMenuItem.Click += new System.EventHandler(this.btnRemove_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(267, 6);
            // 
            // terminalToolStripMenuItem
            // 
            this.terminalToolStripMenuItem.Name = "terminalToolStripMenuItem";
            this.terminalToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.T)));
            this.terminalToolStripMenuItem.Size = new System.Drawing.Size(270, 22);
            this.terminalToolStripMenuItem.Text = "Terminal";
            this.terminalToolStripMenuItem.Click += new System.EventHandler(this.terminalToolStripMenuItem_Click);
            // 
            // openApplicationDirectoryToolStripMenuItem
            // 
            this.openApplicationDirectoryToolStripMenuItem.Name = "openApplicationDirectoryToolStripMenuItem";
            this.openApplicationDirectoryToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.O)));
            this.openApplicationDirectoryToolStripMenuItem.Size = new System.Drawing.Size(270, 22);
            this.openApplicationDirectoryToolStripMenuItem.Text = "Open Application Directory";
            this.openApplicationDirectoryToolStripMenuItem.Click += new System.EventHandler(this.openApplicationDirectoryToolStripMenuItem_Click);
            // 
            // editApplicationSettingsToolStripMenuItem
            // 
            this.editApplicationSettingsToolStripMenuItem.Name = "editApplicationSettingsToolStripMenuItem";
            this.editApplicationSettingsToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.I)));
            this.editApplicationSettingsToolStripMenuItem.Size = new System.Drawing.Size(270, 22);
            this.editApplicationSettingsToolStripMenuItem.Text = "Edit Application Settings...";
            this.editApplicationSettingsToolStripMenuItem.Click += new System.EventHandler(this.btnEdit_Click);
            // 
            // toolStripSeparator3
            // 
            this.toolStripSeparator3.Name = "toolStripSeparator3";
            this.toolStripSeparator3.Size = new System.Drawing.Size(267, 6);
            // 
            // preferencesToolStripMenuItem
            // 
            this.preferencesToolStripMenuItem.Name = "preferencesToolStripMenuItem";
            this.preferencesToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.P)));
            this.preferencesToolStripMenuItem.Size = new System.Drawing.Size(270, 22);
            this.preferencesToolStripMenuItem.Text = "Preferences...";
            this.preferencesToolStripMenuItem.Click += new System.EventHandler(this.btnPref_Click);
            // 
            // toolStripSeparator2
            // 
            this.toolStripSeparator2.Name = "toolStripSeparator2";
            this.toolStripSeparator2.Size = new System.Drawing.Size(267, 6);
            // 
            // quitToolStripMenuItem
            // 
            this.quitToolStripMenuItem.Name = "quitToolStripMenuItem";
            this.quitToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Q)));
            this.quitToolStripMenuItem.Size = new System.Drawing.Size(270, 22);
            this.quitToolStripMenuItem.Text = "Quit";
            this.quitToolStripMenuItem.Click += new System.EventHandler(this.quitToolStripMenuItem_Click);
            // 
            // controlCToolStripMenuItem
            // 
            this.controlCToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.runToolStripMenuItem,
            this.runStrictToolStripMenuItem,
            this.stopToolStripMenuItem,
            this.browseToolStripMenuItem,
            this.browseAppspotToolStripMenuItem,
            this.logsToolStripMenuItem,
            this.sDKConsoleToolStripMenuItem,
            this.toolStripSeparator4,
            this.deployToolStripMenuItem,
            this.rollbackToolStripMenuItem,
            this.dashboardToolStripMenuItem});
            this.controlCToolStripMenuItem.Name = "controlCToolStripMenuItem";
            this.controlCToolStripMenuItem.Size = new System.Drawing.Size(70, 20);
            this.controlCToolStripMenuItem.Text = "Control(&C)";
            // 
            // runToolStripMenuItem
            // 
            this.runToolStripMenuItem.Name = "runToolStripMenuItem";
            this.runToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.R)));
            this.runToolStripMenuItem.Size = new System.Drawing.Size(192, 22);
            this.runToolStripMenuItem.Text = "Run";
            this.runToolStripMenuItem.Click += new System.EventHandler(this.btnStart_Click);
            // 
            // runStrictToolStripMenuItem
            // 
            this.runStrictToolStripMenuItem.Enabled = false;
            this.runStrictToolStripMenuItem.Name = "runStrictToolStripMenuItem";
            this.runStrictToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)(((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Shift)
                        | System.Windows.Forms.Keys.R)));
            this.runStrictToolStripMenuItem.Size = new System.Drawing.Size(192, 22);
            this.runStrictToolStripMenuItem.Text = "Run Strict";
            // 
            // stopToolStripMenuItem
            // 
            this.stopToolStripMenuItem.Name = "stopToolStripMenuItem";
            this.stopToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.S)));
            this.stopToolStripMenuItem.Size = new System.Drawing.Size(192, 22);
            this.stopToolStripMenuItem.Text = "Stop";
            this.stopToolStripMenuItem.Click += new System.EventHandler(this.btnStop_Click);
            // 
            // browseToolStripMenuItem
            // 
            this.browseToolStripMenuItem.Name = "browseToolStripMenuItem";
            this.browseToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.B)));
            this.browseToolStripMenuItem.Size = new System.Drawing.Size(192, 22);
            this.browseToolStripMenuItem.Text = "Browse";
            this.browseToolStripMenuItem.Click += new System.EventHandler(this.btnBrowse_Click);
            // 
            // logsToolStripMenuItem
            // 
            this.logsToolStripMenuItem.Enabled = false;
            this.logsToolStripMenuItem.Name = "logsToolStripMenuItem";
            this.logsToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.L)));
            this.logsToolStripMenuItem.Size = new System.Drawing.Size(192, 22);
            this.logsToolStripMenuItem.Text = "Logs";
            // 
            // sDKConsoleToolStripMenuItem
            // 
            this.sDKConsoleToolStripMenuItem.Name = "sDKConsoleToolStripMenuItem";
            this.sDKConsoleToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.K)));
            this.sDKConsoleToolStripMenuItem.Size = new System.Drawing.Size(192, 22);
            this.sDKConsoleToolStripMenuItem.Text = "SDK Console";
            this.sDKConsoleToolStripMenuItem.Click += new System.EventHandler(this.sDKConsoleToolStripMenuItem_Click);
            // 
            // toolStripSeparator4
            // 
            this.toolStripSeparator4.Name = "toolStripSeparator4";
            this.toolStripSeparator4.Size = new System.Drawing.Size(189, 6);
            // 
            // deployToolStripMenuItem
            // 
            this.deployToolStripMenuItem.Name = "deployToolStripMenuItem";
            this.deployToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.D)));
            this.deployToolStripMenuItem.Size = new System.Drawing.Size(192, 22);
            this.deployToolStripMenuItem.Text = "Deploy...";
            // 
            // dashboardToolStripMenuItem
            // 
            this.dashboardToolStripMenuItem.Name = "dashboardToolStripMenuItem";
            this.dashboardToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)(((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Shift)
                        | System.Windows.Forms.Keys.D)));
            this.dashboardToolStripMenuItem.Size = new System.Drawing.Size(192, 22);
            this.dashboardToolStripMenuItem.Text = "Dashboard";
            // 
            // helpHToolStripMenuItem
            // 
            this.helpHToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.aboutToolStripMenuItem,
            this.checkForUpdatesToolStripMenuItem});
            this.helpHToolStripMenuItem.Name = "helpHToolStripMenuItem";
            this.helpHToolStripMenuItem.Size = new System.Drawing.Size(56, 20);
            this.helpHToolStripMenuItem.Text = "Help(&H)";
            // 
            // aboutToolStripMenuItem
            // 
            this.aboutToolStripMenuItem.Name = "aboutToolStripMenuItem";
            this.aboutToolStripMenuItem.Size = new System.Drawing.Size(305, 22);
            this.aboutToolStripMenuItem.Text = "About GoogleAppEngineLauncher for Windows";
            this.aboutToolStripMenuItem.Click += new System.EventHandler(this.btnAbout_Click);
            // 
            // checkForUpdatesToolStripMenuItem
            // 
            this.checkForUpdatesToolStripMenuItem.Name = "checkForUpdatesToolStripMenuItem";
            this.checkForUpdatesToolStripMenuItem.Size = new System.Drawing.Size(305, 22);
            this.checkForUpdatesToolStripMenuItem.Text = "Check for Updates...";
            this.checkForUpdatesToolStripMenuItem.Click += new System.EventHandler(this.checkForUpdatesToolStripMenuItem_Click);
            // 
            // btnDashboard
            // 
            this.btnDashboard.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnDashboard.Image = ((System.Drawing.Image)(resources.GetObject("btnDashboard.Image")));
            this.btnDashboard.Location = new System.Drawing.Point(632, 27);
            this.btnDashboard.Name = "btnDashboard";
            this.btnDashboard.Size = new System.Drawing.Size(75, 52);
            this.btnDashboard.TabIndex = 9;
            this.btnDashboard.Text = "Dashboard";
            this.btnDashboard.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.btnDashboard.UseVisualStyleBackColor = true;
            this.btnDashboard.Click += new System.EventHandler(this.btnDashboard_Click);
            // 
            // btnDeploy
            // 
            this.btnDeploy.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnDeploy.Image = ((System.Drawing.Image)(resources.GetObject("btnDeploy.Image")));
            this.btnDeploy.Location = new System.Drawing.Point(551, 27);
            this.btnDeploy.Name = "btnDeploy";
            this.btnDeploy.Size = new System.Drawing.Size(75, 52);
            this.btnDeploy.TabIndex = 8;
            this.btnDeploy.Text = "Deploy";
            this.btnDeploy.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.btnDeploy.UseVisualStyleBackColor = true;
            this.btnDeploy.Click += new System.EventHandler(this.btnDeploy_Click);
            // 
            // btnEdit
            // 
            this.btnEdit.Image = ((System.Drawing.Image)(resources.GetObject("btnEdit.Image")));
            this.btnEdit.Location = new System.Drawing.Point(382, 27);
            this.btnEdit.Name = "btnEdit";
            this.btnEdit.Size = new System.Drawing.Size(75, 52);
            this.btnEdit.TabIndex = 6;
            this.btnEdit.Text = "Edit";
            this.btnEdit.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.btnEdit.UseVisualStyleBackColor = true;
            this.btnEdit.Click += new System.EventHandler(this.btnEdit_Click);
            // 
            // btnBrowse
            // 
            this.btnBrowse.Image = ((System.Drawing.Image)(resources.GetObject("btnBrowse.Image")));
            this.btnBrowse.Location = new System.Drawing.Point(175, 27);
            this.btnBrowse.Name = "btnBrowse";
            this.btnBrowse.Size = new System.Drawing.Size(75, 52);
            this.btnBrowse.TabIndex = 5;
            this.btnBrowse.Text = "Browse";
            this.btnBrowse.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.btnBrowse.UseVisualStyleBackColor = true;
            this.btnBrowse.Click += new System.EventHandler(this.btnBrowse_Click);
            // 
            // btnRemove
            // 
            this.btnRemove.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.btnRemove.Image = ((System.Drawing.Image)(resources.GetObject("btnRemove.Image")));
            this.btnRemove.Location = new System.Drawing.Point(52, 378);
            this.btnRemove.Name = "btnRemove";
            this.btnRemove.Size = new System.Drawing.Size(33, 23);
            this.btnRemove.TabIndex = 4;
            this.btnRemove.UseVisualStyleBackColor = true;
            this.btnRemove.Click += new System.EventHandler(this.btnRemove_Click);
            // 
            // btnAdd
            // 
            this.btnAdd.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.btnAdd.Image = ((System.Drawing.Image)(resources.GetObject("btnAdd.Image")));
            this.btnAdd.Location = new System.Drawing.Point(13, 378);
            this.btnAdd.Name = "btnAdd";
            this.btnAdd.Size = new System.Drawing.Size(33, 23);
            this.btnAdd.TabIndex = 3;
            this.btnAdd.UseVisualStyleBackColor = true;
            this.btnAdd.Click += new System.EventHandler(this.btnAdd_Click);
            // 
            // btnStop
            // 
            this.btnStop.Image = ((System.Drawing.Image)(resources.GetObject("btnStop.Image")));
            this.btnStop.Location = new System.Drawing.Point(94, 27);
            this.btnStop.Name = "btnStop";
            this.btnStop.Size = new System.Drawing.Size(75, 52);
            this.btnStop.TabIndex = 2;
            this.btnStop.Text = "Stop";
            this.btnStop.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.btnStop.UseVisualStyleBackColor = true;
            this.btnStop.Click += new System.EventHandler(this.btnStop_Click);
            // 
            // btnStart
            // 
            this.btnStart.Image = ((System.Drawing.Image)(resources.GetObject("btnStart.Image")));
            this.btnStart.Location = new System.Drawing.Point(13, 27);
            this.btnStart.Name = "btnStart";
            this.btnStart.Size = new System.Drawing.Size(75, 52);
            this.btnStart.TabIndex = 1;
            this.btnStart.Text = "Run";
            this.btnStart.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.btnStart.UseVisualStyleBackColor = true;
            this.btnStart.Click += new System.EventHandler(this.btnStart_Click);
            // 
            // btnConsole
            // 
            this.btnConsole.Image = global::GoogleAppEngineLauncher.Properties.Resources.SDK_Console;
            this.btnConsole.Location = new System.Drawing.Point(256, 27);
            this.btnConsole.Name = "btnConsole";
            this.btnConsole.Size = new System.Drawing.Size(94, 52);
            this.btnConsole.TabIndex = 11;
            this.btnConsole.Text = "SDK Console";
            this.btnConsole.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.btnConsole.UseVisualStyleBackColor = true;
            this.btnConsole.Click += new System.EventHandler(this.btnConsole_Click);
            // 
            // rollbackToolStripMenuItem
            // 
            this.rollbackToolStripMenuItem.Name = "rollbackToolStripMenuItem";
            this.rollbackToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.O)));
            this.rollbackToolStripMenuItem.Size = new System.Drawing.Size(192, 22);
            this.rollbackToolStripMenuItem.Text = "Rollback...";
            this.rollbackToolStripMenuItem.Click += new System.EventHandler(this.rollbackToolStripMenuItem_Click);
            // 
            // browseAppspotToolStripMenuItem
            // 
            this.browseAppspotToolStripMenuItem.Name = "browseAppspotToolStripMenuItem";
            this.browseAppspotToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)(((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Shift)
                        | System.Windows.Forms.Keys.B)));
            this.browseAppspotToolStripMenuItem.Size = new System.Drawing.Size(220, 22);
            this.browseAppspotToolStripMenuItem.Text = "Browse appspot";
            this.browseAppspotToolStripMenuItem.Click += new System.EventHandler(this.browseAppspotToolStripMenuItem_Click);
            // 
            // FmMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 12F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(719, 413);
            this.Controls.Add(this.btnConsole);
            this.Controls.Add(this.btnDashboard);
            this.Controls.Add(this.btnDeploy);
            this.Controls.Add(this.btnEdit);
            this.Controls.Add(this.btnBrowse);
            this.Controls.Add(this.btnRemove);
            this.Controls.Add(this.btnAdd);
            this.Controls.Add(this.btnStop);
            this.Controls.Add(this.btnStart);
            this.Controls.Add(this.listApp);
            this.Controls.Add(this.menuStrip1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MainMenuStrip = this.menuStrip1;
            this.Name = "FmMain";
            this.Text = "GoogleAppEngineLauncher for Windows";
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.ListView listApp;
        private System.Windows.Forms.ColumnHeader chAppName;
        private System.Windows.Forms.ColumnHeader chPath;
        private System.Windows.Forms.ColumnHeader chPort;
        private System.Windows.Forms.Button btnStart;
        private System.Windows.Forms.Button btnStop;
        private System.Windows.Forms.Button btnAdd;
        private System.Windows.Forms.Button btnRemove;
        private System.Windows.Forms.ColumnHeader chStatus;
        private System.Windows.Forms.Button btnBrowse;
        private System.Windows.Forms.Button btnEdit;
        private System.Windows.Forms.Button btnDeploy;
        private System.Windows.Forms.Button btnDashboard;
        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.ToolStripMenuItem FileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem newApplicationToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addExistingApplicationToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem removeApplicationToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem quitToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripMenuItem preferencesToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
        private System.Windows.Forms.ToolStripMenuItem helpHToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem aboutToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem editApplicationSettingsToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator3;
        private System.Windows.Forms.ToolStripMenuItem controlCToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem runToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem runStrictToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem stopToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem browseToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem deployToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem dashboardToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem checkForUpdatesToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem openApplicationDirectoryToolStripMenuItem;
        private System.Windows.Forms.Button btnConsole;
        private System.Windows.Forms.ToolStripMenuItem logsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem sDKConsoleToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator4;
        private System.Windows.Forms.ToolStripMenuItem terminalToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem rollbackToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem browseAppspotToolStripMenuItem;
    }
}

