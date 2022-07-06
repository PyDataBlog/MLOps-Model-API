<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class MainWindow
    Inherits MetroFramework.Forms.MetroForm

    'Form remplace la méthode Dispose pour nettoyer la liste des composants.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Requise par le Concepteur Windows Form
    Private components As System.ComponentModel.IContainer

    'REMARQUE : la procédure suivante est requise par le Concepteur Windows Form
    'Elle peut être modifiée à l'aide du Concepteur Windows Form.  
    'Ne la modifiez pas à l'aide de l'éditeur de code.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
		Me.mainPanel = New MetroFramework.Controls.MetroPanel()
		Me.tlpContent = New System.Windows.Forms.TableLayoutPanel()
		Me.dgvSounds = New System.Windows.Forms.DataGridView()
		Me.colSoundLink = New IlshiPlay.SoundColumn()
		Me.colPlayButton = New IlshiPlay.KeyColumn()
		Me.cbMicrophone = New MetroFramework.Controls.MetroComboBox()
		Me.tlpButtons = New System.Windows.Forms.TableLayoutPanel()
		Me.btnSave = New MetroFramework.Controls.MetroButton()
		Me.btnImport = New MetroFramework.Controls.MetroButton()
		Me.TableLayoutPanel1 = New System.Windows.Forms.TableLayoutPanel()
		Me.tbVolume = New MetroFramework.Controls.MetroTrackBar()
		Me.lblVolume = New MetroFramework.Controls.MetroLabel()
		Me.TableLayoutPanel2 = New System.Windows.Forms.TableLayoutPanel()
		Me.btnToggleKey = New MetroFramework.Controls.MetroLabel()
		Me.cbUserOutput = New MetroFramework.Controls.MetroCheckBox()
		Me.linkAbout = New MetroFramework.Controls.MetroLink()
		Me.bgwPlayer = New System.ComponentModel.BackgroundWorker()
		Me.lblPlayingSong = New MetroFramework.Controls.MetroLabel()
		Me.KeyColumn1 = New IlshiPlay.KeyColumn()
		Me.KeyComboBoxColumn1 = New IlshiPlay.KeyColumn()
		Me.mainPanel.SuspendLayout()
		Me.tlpContent.SuspendLayout()
		CType(Me.dgvSounds, System.ComponentModel.ISupportInitialize).BeginInit()
		Me.tlpButtons.SuspendLayout()
		Me.TableLayoutPanel1.SuspendLayout()
		Me.TableLayoutPanel2.SuspendLayout()
		Me.SuspendLayout()
		'
		'mainPanel
		'
		Me.mainPanel.Controls.Add(Me.tlpContent)
		Me.mainPanel.Dock = System.Windows.Forms.DockStyle.Fill
		Me.mainPanel.HorizontalScrollbarBarColor = True
		Me.mainPanel.HorizontalScrollbarHighlightOnWheel = False
		Me.mainPanel.HorizontalScrollbarSize = 8
		Me.mainPanel.Location = New System.Drawing.Point(15, 60)
		Me.mainPanel.Margin = New System.Windows.Forms.Padding(2)
		Me.mainPanel.Name = "mainPanel"
		Me.mainPanel.Size = New System.Drawing.Size(270, 265)
		Me.mainPanel.TabIndex = 0
		Me.mainPanel.VerticalScrollbarBarColor = True
		Me.mainPanel.VerticalScrollbarHighlightOnWheel = False
		Me.mainPanel.VerticalScrollbarSize = 8
		'
		'tlpContent
		'
		Me.tlpContent.ColumnCount = 1
		Me.tlpContent.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
		Me.tlpContent.Controls.Add(Me.dgvSounds, 0, 3)
		Me.tlpContent.Controls.Add(Me.cbMicrophone, 0, 0)
		Me.tlpContent.Controls.Add(Me.tlpButtons, 0, 4)
		Me.tlpContent.Controls.Add(Me.TableLayoutPanel1, 0, 1)
		Me.tlpContent.Controls.Add(Me.TableLayoutPanel2, 0, 2)
		Me.tlpContent.Dock = System.Windows.Forms.DockStyle.Fill
		Me.tlpContent.Location = New System.Drawing.Point(0, 0)
		Me.tlpContent.Margin = New System.Windows.Forms.Padding(2)
		Me.tlpContent.Name = "tlpContent"
		Me.tlpContent.RowCount = 5
		Me.tlpContent.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 29.0!))
		Me.tlpContent.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 29.0!))
		Me.tlpContent.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 29.0!))
		Me.tlpContent.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
		Me.tlpContent.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 29.0!))
		Me.tlpContent.Size = New System.Drawing.Size(270, 265)
		Me.tlpContent.TabIndex = 2
		'
		'dgvSounds
		'
		Me.dgvSounds.BackgroundColor = System.Drawing.Color.Azure
		Me.dgvSounds.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
		Me.dgvSounds.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.colSoundLink, Me.colPlayButton})
		Me.dgvSounds.Dock = System.Windows.Forms.DockStyle.Fill
		Me.dgvSounds.Location = New System.Drawing.Point(2, 89)
		Me.dgvSounds.Margin = New System.Windows.Forms.Padding(2)
		Me.dgvSounds.Name = "dgvSounds"
		Me.dgvSounds.RowTemplate.Height = 24
		Me.dgvSounds.Size = New System.Drawing.Size(266, 145)
		Me.dgvSounds.TabIndex = 1
		'
		'colSoundLink
		'
		Me.colSoundLink.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill
		Me.colSoundLink.HeaderText = "Path"
		Me.colSoundLink.Name = "colSoundLink"
		Me.colSoundLink.ReadOnly = True
		Me.colSoundLink.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
		'
		'colPlayButton
		'
		Me.colPlayButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat
		Me.colPlayButton.HeaderText = "Key"
		Me.colPlayButton.Items.AddRange(New Object() {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"})
		Me.colPlayButton.Name = "colPlayButton"
		Me.colPlayButton.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
		Me.colPlayButton.Width = 50
		'
		'cbMicrophone
		'
		Me.cbMicrophone.Dock = System.Windows.Forms.DockStyle.Fill
		Me.cbMicrophone.FormattingEnabled = True
		Me.cbMicrophone.ItemHeight = 23
		Me.cbMicrophone.Location = New System.Drawing.Point(2, 2)
		Me.cbMicrophone.Margin = New System.Windows.Forms.Padding(2)
		Me.cbMicrophone.Name = "cbMicrophone"
		Me.cbMicrophone.Size = New System.Drawing.Size(266, 29)
		Me.cbMicrophone.TabIndex = 2
		Me.cbMicrophone.UseSelectable = True
		'
		'tlpButtons
		'
		Me.tlpButtons.ColumnCount = 2
		Me.tlpButtons.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
		Me.tlpButtons.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
		Me.tlpButtons.Controls.Add(Me.btnSave, 1, 0)
		Me.tlpButtons.Controls.Add(Me.btnImport, 0, 0)
		Me.tlpButtons.Dock = System.Windows.Forms.DockStyle.Fill
		Me.tlpButtons.Location = New System.Drawing.Point(2, 238)
		Me.tlpButtons.Margin = New System.Windows.Forms.Padding(2)
		Me.tlpButtons.Name = "tlpButtons"
		Me.tlpButtons.RowCount = 1
		Me.tlpButtons.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
		Me.tlpButtons.Size = New System.Drawing.Size(266, 25)
		Me.tlpButtons.TabIndex = 3
		'
		'btnSave
		'
		Me.btnSave.Dock = System.Windows.Forms.DockStyle.Fill
		Me.btnSave.Location = New System.Drawing.Point(135, 2)
		Me.btnSave.Margin = New System.Windows.Forms.Padding(2)
		Me.btnSave.Name = "btnSave"
		Me.btnSave.Size = New System.Drawing.Size(129, 21)
		Me.btnSave.TabIndex = 3
		Me.btnSave.Text = "Save ..."
		Me.btnSave.UseSelectable = True
		'
		'btnImport
		'
		Me.btnImport.Dock = System.Windows.Forms.DockStyle.Fill
		Me.btnImport.Location = New System.Drawing.Point(2, 2)
		Me.btnImport.Margin = New System.Windows.Forms.Padding(2)
		Me.btnImport.Name = "btnImport"
		Me.btnImport.Size = New System.Drawing.Size(129, 21)
		Me.btnImport.TabIndex = 0
		Me.btnImport.Text = "Import ..."
		Me.btnImport.UseSelectable = True
		'
		'TableLayoutPanel1
		'
		Me.TableLayoutPanel1.ColumnCount = 2
		Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 85.0!))
		Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 15.0!))
		Me.TableLayoutPanel1.Controls.Add(Me.tbVolume, 0, 0)
		Me.TableLayoutPanel1.Controls.Add(Me.lblVolume, 1, 0)
		Me.TableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill
		Me.TableLayoutPanel1.Location = New System.Drawing.Point(2, 31)
		Me.TableLayoutPanel1.Margin = New System.Windows.Forms.Padding(2)
		Me.TableLayoutPanel1.Name = "TableLayoutPanel1"
		Me.TableLayoutPanel1.RowCount = 1
		Me.TableLayoutPanel1.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
		Me.TableLayoutPanel1.Size = New System.Drawing.Size(266, 25)
		Me.TableLayoutPanel1.TabIndex = 5
		'
		'tbVolume
		'
		Me.tbVolume.BackColor = System.Drawing.Color.Transparent
		Me.tbVolume.Dock = System.Windows.Forms.DockStyle.Fill
		Me.tbVolume.Location = New System.Drawing.Point(2, 2)
		Me.tbVolume.Margin = New System.Windows.Forms.Padding(2)
		Me.tbVolume.Name = "tbVolume"
		Me.tbVolume.Size = New System.Drawing.Size(222, 21)
		Me.tbVolume.Style = MetroFramework.MetroColorStyle.Blue
		Me.tbVolume.TabIndex = 5
		'
		'lblVolume
		'
		Me.lblVolume.Dock = System.Windows.Forms.DockStyle.Fill
		Me.lblVolume.Location = New System.Drawing.Point(228, 0)
		Me.lblVolume.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
		Me.lblVolume.Name = "lblVolume"
		Me.lblVolume.Size = New System.Drawing.Size(36, 25)
		Me.lblVolume.TabIndex = 6
		Me.lblVolume.Text = "0.0"
		Me.lblVolume.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
		'
		'TableLayoutPanel2
		'
		Me.TableLayoutPanel2.ColumnCount = 2
		Me.TableLayoutPanel2.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 69.14498!))
		Me.TableLayoutPanel2.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 30.85502!))
		Me.TableLayoutPanel2.Controls.Add(Me.btnToggleKey, 1, 0)
		Me.TableLayoutPanel2.Controls.Add(Me.cbUserOutput, 0, 0)
		Me.TableLayoutPanel2.Dock = System.Windows.Forms.DockStyle.Fill
		Me.TableLayoutPanel2.Location = New System.Drawing.Point(2, 60)
		Me.TableLayoutPanel2.Margin = New System.Windows.Forms.Padding(2)
		Me.TableLayoutPanel2.Name = "TableLayoutPanel2"
		Me.TableLayoutPanel2.RowCount = 1
		Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
		Me.TableLayoutPanel2.Size = New System.Drawing.Size(266, 25)
		Me.TableLayoutPanel2.TabIndex = 6
		'
		'btnToggleKey
		'
		Me.btnToggleKey.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
		Me.btnToggleKey.Dock = System.Windows.Forms.DockStyle.Fill
		Me.btnToggleKey.Location = New System.Drawing.Point(186, 0)
		Me.btnToggleKey.Name = "btnToggleKey"
		Me.btnToggleKey.Size = New System.Drawing.Size(77, 25)
		Me.btnToggleKey.Style = MetroFramework.MetroColorStyle.Black
		Me.btnToggleKey.TabIndex = 3
		Me.btnToggleKey.Text = "<toggle>"
		Me.btnToggleKey.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
		Me.btnToggleKey.UseStyleColors = True
		'
		'cbUserOutput
		'
		Me.cbUserOutput.Dock = System.Windows.Forms.DockStyle.Fill
		Me.cbUserOutput.Location = New System.Drawing.Point(2, 2)
		Me.cbUserOutput.Margin = New System.Windows.Forms.Padding(2)
		Me.cbUserOutput.Name = "cbUserOutput"
		Me.cbUserOutput.Size = New System.Drawing.Size(179, 21)
		Me.cbUserOutput.TabIndex = 4
		Me.cbUserOutput.Text = "Hear on main headphones"
		Me.cbUserOutput.UseSelectable = True
		'
		'linkAbout
		'
		Me.linkAbout.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
		Me.linkAbout.Location = New System.Drawing.Point(227, 37)
		Me.linkAbout.Margin = New System.Windows.Forms.Padding(2)
		Me.linkAbout.Name = "linkAbout"
		Me.linkAbout.Size = New System.Drawing.Size(56, 19)
		Me.linkAbout.TabIndex = 1
		Me.linkAbout.Text = "About ..."
		Me.linkAbout.TextAlign = System.Drawing.ContentAlignment.MiddleRight
		Me.linkAbout.UseSelectable = True
		'
		'bgwPlayer
		'
		'
		'lblPlayingSong
		'
		Me.lblPlayingSong.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
		Me.lblPlayingSong.AutoSize = True
		Me.lblPlayingSong.Location = New System.Drawing.Point(17, 325)
		Me.lblPlayingSong.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
		Me.lblPlayingSong.Name = "lblPlayingSong"
		Me.lblPlayingSong.Size = New System.Drawing.Size(64, 19)
		Me.lblPlayingSong.TabIndex = 2
		Me.lblPlayingSong.Text = "Playing ..."
		Me.lblPlayingSong.Visible = False
		'
		'KeyColumn1
		'
		Me.KeyColumn1.FlatStyle = System.Windows.Forms.FlatStyle.Flat
		Me.KeyColumn1.HeaderText = ""
		Me.KeyColumn1.Items.AddRange(New Object() {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"})
		Me.KeyColumn1.Name = "KeyColumn1"
		Me.KeyColumn1.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
		Me.KeyColumn1.Width = 50
		'
		'KeyComboBoxColumn1
		'
		Me.KeyComboBoxColumn1.FlatStyle = System.Windows.Forms.FlatStyle.Flat
		Me.KeyComboBoxColumn1.HeaderText = ""
		Me.KeyComboBoxColumn1.Items.AddRange(New Object() {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"})
		Me.KeyComboBoxColumn1.Name = "KeyComboBoxColumn1"
		Me.KeyComboBoxColumn1.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
		Me.KeyComboBoxColumn1.Width = 50
		'
		'MainWindow
		'
		Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
		Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
		Me.ClientSize = New System.Drawing.Size(300, 341)
		Me.Controls.Add(Me.lblPlayingSong)
		Me.Controls.Add(Me.linkAbout)
		Me.Controls.Add(Me.mainPanel)
		Me.Margin = New System.Windows.Forms.Padding(2)
		Me.MaximizeBox = False
		Me.MaximumSize = New System.Drawing.Size(300, 650)
		Me.MinimumSize = New System.Drawing.Size(300, 341)
		Me.Name = "MainWindow"
		Me.Padding = New System.Windows.Forms.Padding(15, 60, 15, 16)
		Me.Text = "IlshiPlay"
		Me.mainPanel.ResumeLayout(False)
		Me.tlpContent.ResumeLayout(False)
		CType(Me.dgvSounds, System.ComponentModel.ISupportInitialize).EndInit()
		Me.tlpButtons.ResumeLayout(False)
		Me.TableLayoutPanel1.ResumeLayout(False)
		Me.TableLayoutPanel2.ResumeLayout(False)
		Me.ResumeLayout(False)
		Me.PerformLayout()

	End Sub
	Private WithEvents bgwPlayer As System.ComponentModel.BackgroundWorker
	Private WithEvents lblPlayingSong As MetroFramework.Controls.MetroLabel
	Private WithEvents mainPanel As MetroFramework.Controls.MetroPanel
	Private WithEvents tlpContent As System.Windows.Forms.TableLayoutPanel
	Private WithEvents btnImport As MetroFramework.Controls.MetroButton
	Private WithEvents linkAbout As MetroFramework.Controls.MetroLink
	Private WithEvents dgvSounds As System.Windows.Forms.DataGridView
	Private WithEvents cbMicrophone As MetroFramework.Controls.MetroComboBox
	Private WithEvents btnSave As MetroFramework.Controls.MetroButton
	Private WithEvents KeyComboBoxColumn1 As IlshiPlay.KeyColumn
	Private WithEvents tlpButtons As System.Windows.Forms.TableLayoutPanel
	Private WithEvents cbUserOutput As MetroFramework.Controls.MetroCheckBox
	Private WithEvents KeyColumn1 As IlshiPlay.KeyColumn
	Private WithEvents colSoundLink As IlshiPlay.SoundColumn
	Private WithEvents colPlayButton As IlshiPlay.KeyColumn
	Private WithEvents tbVolume As MetroFramework.Controls.MetroTrackBar
	Private WithEvents TableLayoutPanel1 As System.Windows.Forms.TableLayoutPanel
	Private WithEvents lblVolume As MetroFramework.Controls.MetroLabel
	Private WithEvents TableLayoutPanel2 As System.Windows.Forms.TableLayoutPanel
	Friend WithEvents btnToggleKey As MetroFramework.Controls.MetroLabel

End Class
