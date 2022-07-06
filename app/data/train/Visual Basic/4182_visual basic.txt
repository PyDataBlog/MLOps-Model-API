<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class notepadxp
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
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

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(notepadxp))
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.FileToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.NewCtrlNToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.OpenCtrlOToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveCtrlSToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveAsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PageSetupToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PrintCtrlPToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExitToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.EditToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.UndoCtrlZToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CutCtrlXToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CopyCtrlCToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PasteCtrlVToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DeleteDelToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.FindCtrlFToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.FindNextF3ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ReplaceCtrlHToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.GoToCtrlGToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SelectAllCtrlAToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.TimeDateF5ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.FormatToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.WordWrapToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.FontToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ViewToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.StatusBarToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.HelpToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.HelptTopicsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.AboutNotepadToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.pullbs = New System.Windows.Forms.Timer(Me.components)
        Me.pullbottom = New System.Windows.Forms.Timer(Me.components)
        Me.pullside = New System.Windows.Forms.Timer(Me.components)
        Me.program = New System.Windows.Forms.Panel()
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.bottomleftcorner = New System.Windows.Forms.Panel()
        Me.toprightcorner = New System.Windows.Forms.Panel()
        Me.bottomrightcorner = New System.Windows.Forms.Panel()
        Me.topleftcorner = New System.Windows.Forms.Panel()
        Me.bottom = New System.Windows.Forms.Panel()
        Me.top = New System.Windows.Forms.Panel()
        Me.maximizebutton = New System.Windows.Forms.PictureBox()
        Me.minimizebutton = New System.Windows.Forms.PictureBox()
        Me.programname = New System.Windows.Forms.Label()
        Me.closebutton = New System.Windows.Forms.PictureBox()
        Me.right = New System.Windows.Forms.Panel()
        Me.left = New System.Windows.Forms.Panel()
        Me.look = New System.Windows.Forms.Timer(Me.components)
        Me.MenuStrip1.SuspendLayout()
        Me.program.SuspendLayout()
        Me.top.SuspendLayout()
        CType(Me.maximizebutton, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.minimizebutton, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.closebutton, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'MenuStrip1
        '
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FileToolStripMenuItem, Me.EditToolStripMenuItem, Me.FormatToolStripMenuItem, Me.ViewToolStripMenuItem, Me.HelpToolStripMenuItem})
        Me.MenuStrip1.Location = New System.Drawing.Point(4, 30)
        Me.MenuStrip1.Name = "MenuStrip1"
        Me.MenuStrip1.Size = New System.Drawing.Size(444, 24)
        Me.MenuStrip1.TabIndex = 6
        Me.MenuStrip1.Text = "MenuStrip1"
        '
        'FileToolStripMenuItem
        '
        Me.FileToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.NewCtrlNToolStripMenuItem, Me.OpenCtrlOToolStripMenuItem, Me.SaveCtrlSToolStripMenuItem, Me.SaveAsToolStripMenuItem, Me.PageSetupToolStripMenuItem, Me.PrintCtrlPToolStripMenuItem, Me.ExitToolStripMenuItem})
        Me.FileToolStripMenuItem.Name = "FileToolStripMenuItem"
        Me.FileToolStripMenuItem.Size = New System.Drawing.Size(37, 20)
        Me.FileToolStripMenuItem.Text = "File"
        '
        'NewCtrlNToolStripMenuItem
        '
        Me.NewCtrlNToolStripMenuItem.Name = "NewCtrlNToolStripMenuItem"
        Me.NewCtrlNToolStripMenuItem.Size = New System.Drawing.Size(176, 22)
        Me.NewCtrlNToolStripMenuItem.Text = "New              Ctrl+N"
        '
        'OpenCtrlOToolStripMenuItem
        '
        Me.OpenCtrlOToolStripMenuItem.Name = "OpenCtrlOToolStripMenuItem"
        Me.OpenCtrlOToolStripMenuItem.Size = New System.Drawing.Size(176, 22)
        Me.OpenCtrlOToolStripMenuItem.Text = "Open...         Ctrl+O"
        '
        'SaveCtrlSToolStripMenuItem
        '
        Me.SaveCtrlSToolStripMenuItem.Name = "SaveCtrlSToolStripMenuItem"
        Me.SaveCtrlSToolStripMenuItem.Size = New System.Drawing.Size(176, 22)
        Me.SaveCtrlSToolStripMenuItem.Text = "Save              Ctrl+S"
        '
        'SaveAsToolStripMenuItem
        '
        Me.SaveAsToolStripMenuItem.Name = "SaveAsToolStripMenuItem"
        Me.SaveAsToolStripMenuItem.Size = New System.Drawing.Size(176, 22)
        Me.SaveAsToolStripMenuItem.Text = "Save As..."
        '
        'PageSetupToolStripMenuItem
        '
        Me.PageSetupToolStripMenuItem.Name = "PageSetupToolStripMenuItem"
        Me.PageSetupToolStripMenuItem.Size = New System.Drawing.Size(176, 22)
        Me.PageSetupToolStripMenuItem.Text = "Page Setup..."
        '
        'PrintCtrlPToolStripMenuItem
        '
        Me.PrintCtrlPToolStripMenuItem.Name = "PrintCtrlPToolStripMenuItem"
        Me.PrintCtrlPToolStripMenuItem.Size = New System.Drawing.Size(176, 22)
        Me.PrintCtrlPToolStripMenuItem.Text = "Print...          Ctrl+P"
        '
        'ExitToolStripMenuItem
        '
        Me.ExitToolStripMenuItem.Name = "ExitToolStripMenuItem"
        Me.ExitToolStripMenuItem.Size = New System.Drawing.Size(176, 22)
        Me.ExitToolStripMenuItem.Text = "Exit"
        '
        'EditToolStripMenuItem
        '
        Me.EditToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.UndoCtrlZToolStripMenuItem, Me.CutCtrlXToolStripMenuItem, Me.CopyCtrlCToolStripMenuItem, Me.PasteCtrlVToolStripMenuItem, Me.DeleteDelToolStripMenuItem, Me.FindCtrlFToolStripMenuItem, Me.FindNextF3ToolStripMenuItem, Me.ReplaceCtrlHToolStripMenuItem, Me.GoToCtrlGToolStripMenuItem, Me.SelectAllCtrlAToolStripMenuItem, Me.TimeDateF5ToolStripMenuItem})
        Me.EditToolStripMenuItem.Name = "EditToolStripMenuItem"
        Me.EditToolStripMenuItem.Size = New System.Drawing.Size(39, 20)
        Me.EditToolStripMenuItem.Text = "Edit"
        '
        'UndoCtrlZToolStripMenuItem
        '
        Me.UndoCtrlZToolStripMenuItem.Name = "UndoCtrlZToolStripMenuItem"
        Me.UndoCtrlZToolStripMenuItem.Size = New System.Drawing.Size(172, 22)
        Me.UndoCtrlZToolStripMenuItem.Text = "Undo           Ctrl+Z"
        '
        'CutCtrlXToolStripMenuItem
        '
        Me.CutCtrlXToolStripMenuItem.Name = "CutCtrlXToolStripMenuItem"
        Me.CutCtrlXToolStripMenuItem.Size = New System.Drawing.Size(172, 22)
        Me.CutCtrlXToolStripMenuItem.Text = "Cut              Ctrl+X"
        '
        'CopyCtrlCToolStripMenuItem
        '
        Me.CopyCtrlCToolStripMenuItem.Name = "CopyCtrlCToolStripMenuItem"
        Me.CopyCtrlCToolStripMenuItem.Size = New System.Drawing.Size(172, 22)
        Me.CopyCtrlCToolStripMenuItem.Text = "Copy           Ctrl+C"
        '
        'PasteCtrlVToolStripMenuItem
        '
        Me.PasteCtrlVToolStripMenuItem.Name = "PasteCtrlVToolStripMenuItem"
        Me.PasteCtrlVToolStripMenuItem.Size = New System.Drawing.Size(172, 22)
        Me.PasteCtrlVToolStripMenuItem.Text = "Paste           Ctrl+V"
        '
        'DeleteDelToolStripMenuItem
        '
        Me.DeleteDelToolStripMenuItem.Name = "DeleteDelToolStripMenuItem"
        Me.DeleteDelToolStripMenuItem.Size = New System.Drawing.Size(172, 22)
        Me.DeleteDelToolStripMenuItem.Text = "Delete          Del"
        '
        'FindCtrlFToolStripMenuItem
        '
        Me.FindCtrlFToolStripMenuItem.Name = "FindCtrlFToolStripMenuItem"
        Me.FindCtrlFToolStripMenuItem.Size = New System.Drawing.Size(172, 22)
        Me.FindCtrlFToolStripMenuItem.Text = "Find...          Ctrl+F"
        '
        'FindNextF3ToolStripMenuItem
        '
        Me.FindNextF3ToolStripMenuItem.Name = "FindNextF3ToolStripMenuItem"
        Me.FindNextF3ToolStripMenuItem.Size = New System.Drawing.Size(172, 22)
        Me.FindNextF3ToolStripMenuItem.Text = "Find Next     F3"
        '
        'ReplaceCtrlHToolStripMenuItem
        '
        Me.ReplaceCtrlHToolStripMenuItem.Name = "ReplaceCtrlHToolStripMenuItem"
        Me.ReplaceCtrlHToolStripMenuItem.Size = New System.Drawing.Size(172, 22)
        Me.ReplaceCtrlHToolStripMenuItem.Text = "Replace...    Ctrl+H"
        '
        'GoToCtrlGToolStripMenuItem
        '
        Me.GoToCtrlGToolStripMenuItem.Name = "GoToCtrlGToolStripMenuItem"
        Me.GoToCtrlGToolStripMenuItem.Size = New System.Drawing.Size(172, 22)
        Me.GoToCtrlGToolStripMenuItem.Text = "Go To...       Ctrl+G"
        '
        'SelectAllCtrlAToolStripMenuItem
        '
        Me.SelectAllCtrlAToolStripMenuItem.Name = "SelectAllCtrlAToolStripMenuItem"
        Me.SelectAllCtrlAToolStripMenuItem.Size = New System.Drawing.Size(172, 22)
        Me.SelectAllCtrlAToolStripMenuItem.Text = "Select All     Ctrl+A"
        '
        'TimeDateF5ToolStripMenuItem
        '
        Me.TimeDateF5ToolStripMenuItem.Name = "TimeDateF5ToolStripMenuItem"
        Me.TimeDateF5ToolStripMenuItem.Size = New System.Drawing.Size(172, 22)
        Me.TimeDateF5ToolStripMenuItem.Text = "Time/Date    F5"
        '
        'FormatToolStripMenuItem
        '
        Me.FormatToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.WordWrapToolStripMenuItem, Me.FontToolStripMenuItem})
        Me.FormatToolStripMenuItem.Name = "FormatToolStripMenuItem"
        Me.FormatToolStripMenuItem.Size = New System.Drawing.Size(57, 20)
        Me.FormatToolStripMenuItem.Text = "Format"
        '
        'WordWrapToolStripMenuItem
        '
        Me.WordWrapToolStripMenuItem.Name = "WordWrapToolStripMenuItem"
        Me.WordWrapToolStripMenuItem.Size = New System.Drawing.Size(134, 22)
        Me.WordWrapToolStripMenuItem.Text = "Word Wrap"
        '
        'FontToolStripMenuItem
        '
        Me.FontToolStripMenuItem.Name = "FontToolStripMenuItem"
        Me.FontToolStripMenuItem.Size = New System.Drawing.Size(134, 22)
        Me.FontToolStripMenuItem.Text = "Font..."
        '
        'ViewToolStripMenuItem
        '
        Me.ViewToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.StatusBarToolStripMenuItem})
        Me.ViewToolStripMenuItem.Name = "ViewToolStripMenuItem"
        Me.ViewToolStripMenuItem.Size = New System.Drawing.Size(44, 20)
        Me.ViewToolStripMenuItem.Text = "View"
        '
        'StatusBarToolStripMenuItem
        '
        Me.StatusBarToolStripMenuItem.Name = "StatusBarToolStripMenuItem"
        Me.StatusBarToolStripMenuItem.Size = New System.Drawing.Size(126, 22)
        Me.StatusBarToolStripMenuItem.Text = "Status Bar"
        '
        'HelpToolStripMenuItem
        '
        Me.HelpToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.HelptTopicsToolStripMenuItem, Me.AboutNotepadToolStripMenuItem})
        Me.HelpToolStripMenuItem.Name = "HelpToolStripMenuItem"
        Me.HelpToolStripMenuItem.Size = New System.Drawing.Size(44, 20)
        Me.HelpToolStripMenuItem.Text = "Help"
        '
        'HelptTopicsToolStripMenuItem
        '
        Me.HelptTopicsToolStripMenuItem.Name = "HelptTopicsToolStripMenuItem"
        Me.HelptTopicsToolStripMenuItem.Size = New System.Drawing.Size(156, 22)
        Me.HelptTopicsToolStripMenuItem.Text = "Help Topics"
        '
        'AboutNotepadToolStripMenuItem
        '
        Me.AboutNotepadToolStripMenuItem.Name = "AboutNotepadToolStripMenuItem"
        Me.AboutNotepadToolStripMenuItem.Size = New System.Drawing.Size(156, 22)
        Me.AboutNotepadToolStripMenuItem.Text = "About Notepad"
        '
        'pullbs
        '
        Me.pullbs.Interval = 1
        '
        'pullbottom
        '
        Me.pullbottom.Interval = 1
        '
        'pullside
        '
        Me.pullside.Interval = 1
        '
        'program
        '
        Me.program.BackColor = System.Drawing.Color.OldLace
        Me.program.Controls.Add(Me.TextBox1)
        Me.program.Controls.Add(Me.MenuStrip1)
        Me.program.Controls.Add(Me.bottomleftcorner)
        Me.program.Controls.Add(Me.toprightcorner)
        Me.program.Controls.Add(Me.bottomrightcorner)
        Me.program.Controls.Add(Me.topleftcorner)
        Me.program.Controls.Add(Me.bottom)
        Me.program.Controls.Add(Me.top)
        Me.program.Controls.Add(Me.right)
        Me.program.Controls.Add(Me.left)
        Me.program.Dock = System.Windows.Forms.DockStyle.Fill
        Me.program.Location = New System.Drawing.Point(0, 0)
        Me.program.Name = "program"
        Me.program.Size = New System.Drawing.Size(452, 366)
        Me.program.TabIndex = 9
        '
        'TextBox1
        '
        Me.TextBox1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TextBox1.Location = New System.Drawing.Point(4, 54)
        Me.TextBox1.Multiline = True
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.ScrollBars = System.Windows.Forms.ScrollBars.Both
        Me.TextBox1.Size = New System.Drawing.Size(444, 308)
        Me.TextBox1.TabIndex = 11
        '
        'bottomleftcorner
        '
        Me.bottomleftcorner.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.bottomleftcorner.BackgroundImage = Global.Histacom.My.Resources.Resources.windowsxpbottomleftcorner
        Me.bottomleftcorner.Location = New System.Drawing.Point(0, 362)
        Me.bottomleftcorner.Name = "bottomleftcorner"
        Me.bottomleftcorner.Size = New System.Drawing.Size(5, 4)
        Me.bottomleftcorner.TabIndex = 10
        '
        'toprightcorner
        '
        Me.toprightcorner.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.toprightcorner.BackColor = System.Drawing.Color.Magenta
        Me.toprightcorner.BackgroundImage = CType(resources.GetObject("toprightcorner.BackgroundImage"), System.Drawing.Image)
        Me.toprightcorner.Location = New System.Drawing.Point(446, 0)
        Me.toprightcorner.Name = "toprightcorner"
        Me.toprightcorner.Size = New System.Drawing.Size(6, 30)
        Me.toprightcorner.TabIndex = 9
        '
        'bottomrightcorner
        '
        Me.bottomrightcorner.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.bottomrightcorner.BackgroundImage = Global.Histacom.My.Resources.Resources.windowsxpbottomcorner
        Me.bottomrightcorner.Cursor = System.Windows.Forms.Cursors.SizeNWSE
        Me.bottomrightcorner.Location = New System.Drawing.Point(448, 362)
        Me.bottomrightcorner.Name = "bottomrightcorner"
        Me.bottomrightcorner.Size = New System.Drawing.Size(4, 4)
        Me.bottomrightcorner.TabIndex = 4
        '
        'topleftcorner
        '
        Me.topleftcorner.BackColor = System.Drawing.Color.Magenta
        Me.topleftcorner.BackgroundImage = CType(resources.GetObject("topleftcorner.BackgroundImage"), System.Drawing.Image)
        Me.topleftcorner.BackgroundImageLayout = System.Windows.Forms.ImageLayout.None
        Me.topleftcorner.Location = New System.Drawing.Point(0, 0)
        Me.topleftcorner.Name = "topleftcorner"
        Me.topleftcorner.Size = New System.Drawing.Size(7, 30)
        Me.topleftcorner.TabIndex = 8
        '
        'bottom
        '
        Me.bottom.BackgroundImage = Global.Histacom.My.Resources.Resources.windowsxpbottom
        Me.bottom.Cursor = System.Windows.Forms.Cursors.SizeNS
        Me.bottom.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.bottom.Location = New System.Drawing.Point(4, 362)
        Me.bottom.Name = "bottom"
        Me.bottom.Size = New System.Drawing.Size(444, 4)
        Me.bottom.TabIndex = 3
        '
        'top
        '
        Me.top.BackColor = System.Drawing.Color.Transparent
        Me.top.BackgroundImage = Global.Histacom.My.Resources.Resources.windowsxptopbarmiddle
        Me.top.Controls.Add(Me.maximizebutton)
        Me.top.Controls.Add(Me.minimizebutton)
        Me.top.Controls.Add(Me.programname)
        Me.top.Controls.Add(Me.closebutton)
        Me.top.Dock = System.Windows.Forms.DockStyle.Top
        Me.top.Location = New System.Drawing.Point(4, 0)
        Me.top.Name = "top"
        Me.top.Size = New System.Drawing.Size(444, 30)
        Me.top.TabIndex = 0
        '
        'maximizebutton
        '
        Me.maximizebutton.Anchor = System.Windows.Forms.AnchorStyles.Right
        Me.maximizebutton.Image = Global.Histacom.My.Resources.Resources.windowsxpmaximizebutton
        Me.maximizebutton.Location = New System.Drawing.Point(396, 5)
        Me.maximizebutton.Name = "maximizebutton"
        Me.maximizebutton.Size = New System.Drawing.Size(21, 21)
        Me.maximizebutton.TabIndex = 6
        Me.maximizebutton.TabStop = False
        '
        'minimizebutton
        '
        Me.minimizebutton.Anchor = System.Windows.Forms.AnchorStyles.Right
        Me.minimizebutton.Image = Global.Histacom.My.Resources.Resources.windowsxpminimizebutton
        Me.minimizebutton.Location = New System.Drawing.Point(373, 5)
        Me.minimizebutton.Name = "minimizebutton"
        Me.minimizebutton.Size = New System.Drawing.Size(21, 21)
        Me.minimizebutton.TabIndex = 5
        Me.minimizebutton.TabStop = False
        '
        'programname
        '
        Me.programname.AutoSize = True
        Me.programname.BackColor = System.Drawing.Color.Transparent
        Me.programname.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.programname.ForeColor = System.Drawing.Color.White
        Me.programname.Location = New System.Drawing.Point(5, 8)
        Me.programname.Name = "programname"
        Me.programname.Size = New System.Drawing.Size(55, 13)
        Me.programname.TabIndex = 3
        Me.programname.Text = "Notepad"
        '
        'closebutton
        '
        Me.closebutton.Anchor = System.Windows.Forms.AnchorStyles.Right
        Me.closebutton.Image = Global.Histacom.My.Resources.Resources.windowsxpclosebutton
        Me.closebutton.Location = New System.Drawing.Point(419, 5)
        Me.closebutton.Name = "closebutton"
        Me.closebutton.Size = New System.Drawing.Size(21, 21)
        Me.closebutton.TabIndex = 4
        Me.closebutton.TabStop = False
        '
        'right
        '
        Me.right.BackgroundImage = Global.Histacom.My.Resources.Resources.windowsxprightside
        Me.right.Cursor = System.Windows.Forms.Cursors.SizeWE
        Me.right.Dock = System.Windows.Forms.DockStyle.Right
        Me.right.Location = New System.Drawing.Point(448, 0)
        Me.right.Name = "right"
        Me.right.Size = New System.Drawing.Size(4, 366)
        Me.right.TabIndex = 2
        '
        'left
        '
        Me.left.BackgroundImage = Global.Histacom.My.Resources.Resources.windowsxpleftside
        Me.left.Dock = System.Windows.Forms.DockStyle.Left
        Me.left.Location = New System.Drawing.Point(0, 0)
        Me.left.Name = "left"
        Me.left.Size = New System.Drawing.Size(4, 366)
        Me.left.TabIndex = 1
        '
        'look
        '
        '
        'notepadxp
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(452, 366)
        Me.Controls.Add(Me.program)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
        Me.MainMenuStrip = Me.MenuStrip1
        Me.Name = "notepadxp"
        Me.Text = "notepadxp"
        Me.TopMost = True
        Me.TransparencyKey = System.Drawing.Color.Magenta
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.program.ResumeLayout(False)
        Me.program.PerformLayout()
        Me.top.ResumeLayout(False)
        Me.top.PerformLayout()
        CType(Me.maximizebutton, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.minimizebutton, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.closebutton, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents MenuStrip1 As System.Windows.Forms.MenuStrip
    Friend WithEvents FileToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents NewCtrlNToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents OpenCtrlOToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SaveCtrlSToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SaveAsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PageSetupToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PrintCtrlPToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ExitToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents EditToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents UndoCtrlZToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CutCtrlXToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CopyCtrlCToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PasteCtrlVToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DeleteDelToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FindCtrlFToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FindNextF3ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ReplaceCtrlHToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents GoToCtrlGToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SelectAllCtrlAToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents TimeDateF5ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FormatToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents WordWrapToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FontToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ViewToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents StatusBarToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents HelpToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents HelptTopicsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents AboutNotepadToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents pullbs As System.Windows.Forms.Timer
    Friend WithEvents pullbottom As System.Windows.Forms.Timer
    Friend WithEvents pullside As System.Windows.Forms.Timer
    Friend WithEvents program As System.Windows.Forms.Panel
    Friend WithEvents bottomleftcorner As System.Windows.Forms.Panel
    Friend WithEvents toprightcorner As System.Windows.Forms.Panel
    Friend WithEvents bottomrightcorner As System.Windows.Forms.Panel
    Friend WithEvents bottom As System.Windows.Forms.Panel
    Friend WithEvents right As System.Windows.Forms.Panel
    Friend WithEvents topleftcorner As System.Windows.Forms.Panel
    Friend WithEvents left As System.Windows.Forms.Panel
    Friend WithEvents top As System.Windows.Forms.Panel
    Friend WithEvents maximizebutton As System.Windows.Forms.PictureBox
    Friend WithEvents minimizebutton As System.Windows.Forms.PictureBox
    Friend WithEvents programname As System.Windows.Forms.Label
    Friend WithEvents closebutton As System.Windows.Forms.PictureBox
    Friend WithEvents TextBox1 As System.Windows.Forms.TextBox
    Friend WithEvents look As System.Windows.Forms.Timer
End Class
