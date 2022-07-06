<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class formnotepad
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
        Me.notepadtextbox = New System.Windows.Forms.TextBox()
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.FileToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.EditToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.SearchToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.FormatToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.FileToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.NewToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.OpenToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveAsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PageSetupToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PrintToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExitToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.EditToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.UndoCtrlZToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CutCtrlXToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CopyCtrlCToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PasteCtrlVToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DeleteDelToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SelectAllToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.TimeDateF5ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.WordWrapToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SearchToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.FindToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.FindNextF3ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.HelpToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.HelpTopicsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.AboutNotepadToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.Timer1 = New System.Windows.Forms.Timer(Me.components)
        Me.look = New System.Windows.Forms.Timer(Me.components)
        Me.program = New System.Windows.Forms.Panel()
        Me.programtopbar = New System.Windows.Forms.Panel()
        Me.maximizebutton = New System.Windows.Forms.PictureBox()
        Me.minimizebutton = New System.Windows.Forms.PictureBox()
        Me.programname = New System.Windows.Forms.Label()
        Me.closebutton = New System.Windows.Forms.PictureBox()
        Me.toprightcorner = New System.Windows.Forms.Panel()
        Me.bottomrightcorner = New System.Windows.Forms.Panel()
        Me.bottomleftcorner = New System.Windows.Forms.Panel()
        Me.topleftcorner = New System.Windows.Forms.Panel()
        Me.left = New System.Windows.Forms.Panel()
        Me.bottom = New System.Windows.Forms.Panel()
        Me.right = New System.Windows.Forms.Panel()
        Me.top = New System.Windows.Forms.Panel()
        Me.pullside = New System.Windows.Forms.Timer(Me.components)
        Me.pullbottom = New System.Windows.Forms.Timer(Me.components)
        Me.pullbs = New System.Windows.Forms.Timer(Me.components)
        Me.MenuStrip1.SuspendLayout()
        Me.program.SuspendLayout()
        Me.programtopbar.SuspendLayout()
        CType(Me.maximizebutton, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.minimizebutton, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.closebutton, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'notepadtextbox
        '
        Me.notepadtextbox.Cursor = System.Windows.Forms.Cursors.Arrow
        Me.notepadtextbox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.notepadtextbox.Location = New System.Drawing.Point(4, 46)
        Me.notepadtextbox.Multiline = True
        Me.notepadtextbox.Name = "notepadtextbox"
        Me.notepadtextbox.ScrollBars = System.Windows.Forms.ScrollBars.Both
        Me.notepadtextbox.Size = New System.Drawing.Size(532, 374)
        Me.notepadtextbox.TabIndex = 1
        '
        'MenuStrip1
        '
        Me.MenuStrip1.BackColor = System.Drawing.Color.Silver
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FileToolStripMenuItem1, Me.EditToolStripMenuItem1, Me.SearchToolStripMenuItem1, Me.FormatToolStripMenuItem})
        Me.MenuStrip1.Location = New System.Drawing.Point(4, 22)
        Me.MenuStrip1.Name = "MenuStrip1"
        Me.MenuStrip1.Size = New System.Drawing.Size(532, 24)
        Me.MenuStrip1.TabIndex = 2
        Me.MenuStrip1.Text = "MenuStrip1"
        '
        'FileToolStripMenuItem1
        '
        Me.FileToolStripMenuItem1.Name = "FileToolStripMenuItem1"
        Me.FileToolStripMenuItem1.Size = New System.Drawing.Size(37, 20)
        Me.FileToolStripMenuItem1.Text = "File"
        '
        'EditToolStripMenuItem1
        '
        Me.EditToolStripMenuItem1.Name = "EditToolStripMenuItem1"
        Me.EditToolStripMenuItem1.Size = New System.Drawing.Size(39, 20)
        Me.EditToolStripMenuItem1.Text = "Edit"
        '
        'SearchToolStripMenuItem1
        '
        Me.SearchToolStripMenuItem1.Name = "SearchToolStripMenuItem1"
        Me.SearchToolStripMenuItem1.Size = New System.Drawing.Size(54, 20)
        Me.SearchToolStripMenuItem1.Text = "Search"
        '
        'FormatToolStripMenuItem
        '
        Me.FormatToolStripMenuItem.Name = "FormatToolStripMenuItem"
        Me.FormatToolStripMenuItem.Size = New System.Drawing.Size(44, 20)
        Me.FormatToolStripMenuItem.Text = "Help"
        '
        'FileToolStripMenuItem
        '
        Me.FileToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.FileToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.NewToolStripMenuItem, Me.OpenToolStripMenuItem, Me.SaveToolStripMenuItem, Me.SaveAsToolStripMenuItem, Me.PageSetupToolStripMenuItem, Me.PrintToolStripMenuItem, Me.ExitToolStripMenuItem})
        Me.FileToolStripMenuItem.Name = "FileToolStripMenuItem"
        Me.FileToolStripMenuItem.Size = New System.Drawing.Size(37, 20)
        Me.FileToolStripMenuItem.Text = "File"
        '
        'NewToolStripMenuItem
        '
        Me.NewToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.NewToolStripMenuItem.Name = "NewToolStripMenuItem"
        Me.NewToolStripMenuItem.Size = New System.Drawing.Size(142, 22)
        Me.NewToolStripMenuItem.Text = "New"
        '
        'OpenToolStripMenuItem
        '
        Me.OpenToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.OpenToolStripMenuItem.Name = "OpenToolStripMenuItem"
        Me.OpenToolStripMenuItem.Size = New System.Drawing.Size(142, 22)
        Me.OpenToolStripMenuItem.Text = "Open..."
        '
        'SaveToolStripMenuItem
        '
        Me.SaveToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.SaveToolStripMenuItem.Name = "SaveToolStripMenuItem"
        Me.SaveToolStripMenuItem.Size = New System.Drawing.Size(142, 22)
        Me.SaveToolStripMenuItem.Text = "Save"
        '
        'SaveAsToolStripMenuItem
        '
        Me.SaveAsToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.SaveAsToolStripMenuItem.Name = "SaveAsToolStripMenuItem"
        Me.SaveAsToolStripMenuItem.Size = New System.Drawing.Size(142, 22)
        Me.SaveAsToolStripMenuItem.Text = "Save As..."
        '
        'PageSetupToolStripMenuItem
        '
        Me.PageSetupToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.PageSetupToolStripMenuItem.Name = "PageSetupToolStripMenuItem"
        Me.PageSetupToolStripMenuItem.Size = New System.Drawing.Size(142, 22)
        Me.PageSetupToolStripMenuItem.Text = "Page Setup..."
        '
        'PrintToolStripMenuItem
        '
        Me.PrintToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.PrintToolStripMenuItem.Name = "PrintToolStripMenuItem"
        Me.PrintToolStripMenuItem.Size = New System.Drawing.Size(142, 22)
        Me.PrintToolStripMenuItem.Text = "Print"
        '
        'ExitToolStripMenuItem
        '
        Me.ExitToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.ExitToolStripMenuItem.Name = "ExitToolStripMenuItem"
        Me.ExitToolStripMenuItem.Size = New System.Drawing.Size(142, 22)
        Me.ExitToolStripMenuItem.Text = "Exit"
        '
        'EditToolStripMenuItem
        '
        Me.EditToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.EditToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.UndoCtrlZToolStripMenuItem, Me.CutCtrlXToolStripMenuItem, Me.CopyCtrlCToolStripMenuItem, Me.PasteCtrlVToolStripMenuItem, Me.DeleteDelToolStripMenuItem, Me.SelectAllToolStripMenuItem, Me.TimeDateF5ToolStripMenuItem, Me.WordWrapToolStripMenuItem})
        Me.EditToolStripMenuItem.Name = "EditToolStripMenuItem"
        Me.EditToolStripMenuItem.Size = New System.Drawing.Size(39, 20)
        Me.EditToolStripMenuItem.Text = "Edit"
        '
        'UndoCtrlZToolStripMenuItem
        '
        Me.UndoCtrlZToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.UndoCtrlZToolStripMenuItem.Name = "UndoCtrlZToolStripMenuItem"
        Me.UndoCtrlZToolStripMenuItem.Size = New System.Drawing.Size(167, 22)
        Me.UndoCtrlZToolStripMenuItem.Text = "Undo          Ctrl+Z"
        '
        'CutCtrlXToolStripMenuItem
        '
        Me.CutCtrlXToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.CutCtrlXToolStripMenuItem.Name = "CutCtrlXToolStripMenuItem"
        Me.CutCtrlXToolStripMenuItem.Size = New System.Drawing.Size(167, 22)
        Me.CutCtrlXToolStripMenuItem.Text = "Cut             Ctrl+X"
        '
        'CopyCtrlCToolStripMenuItem
        '
        Me.CopyCtrlCToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.CopyCtrlCToolStripMenuItem.Name = "CopyCtrlCToolStripMenuItem"
        Me.CopyCtrlCToolStripMenuItem.Size = New System.Drawing.Size(167, 22)
        Me.CopyCtrlCToolStripMenuItem.Text = "Copy          Ctrl+C"
        '
        'PasteCtrlVToolStripMenuItem
        '
        Me.PasteCtrlVToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.PasteCtrlVToolStripMenuItem.Name = "PasteCtrlVToolStripMenuItem"
        Me.PasteCtrlVToolStripMenuItem.Size = New System.Drawing.Size(167, 22)
        Me.PasteCtrlVToolStripMenuItem.Text = "Paste          Ctrl+V"
        '
        'DeleteDelToolStripMenuItem
        '
        Me.DeleteDelToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.DeleteDelToolStripMenuItem.Name = "DeleteDelToolStripMenuItem"
        Me.DeleteDelToolStripMenuItem.Size = New System.Drawing.Size(167, 22)
        Me.DeleteDelToolStripMenuItem.Text = "Delete         Del"
        '
        'SelectAllToolStripMenuItem
        '
        Me.SelectAllToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.SelectAllToolStripMenuItem.Name = "SelectAllToolStripMenuItem"
        Me.SelectAllToolStripMenuItem.Size = New System.Drawing.Size(167, 22)
        Me.SelectAllToolStripMenuItem.Text = "Select All"
        '
        'TimeDateF5ToolStripMenuItem
        '
        Me.TimeDateF5ToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.TimeDateF5ToolStripMenuItem.Name = "TimeDateF5ToolStripMenuItem"
        Me.TimeDateF5ToolStripMenuItem.Size = New System.Drawing.Size(167, 22)
        Me.TimeDateF5ToolStripMenuItem.Text = "Time/Date F5"
        '
        'WordWrapToolStripMenuItem
        '
        Me.WordWrapToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.WordWrapToolStripMenuItem.Name = "WordWrapToolStripMenuItem"
        Me.WordWrapToolStripMenuItem.Size = New System.Drawing.Size(167, 22)
        Me.WordWrapToolStripMenuItem.Text = "Word Wrap"
        '
        'SearchToolStripMenuItem
        '
        Me.SearchToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.SearchToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FindToolStripMenuItem1, Me.FindNextF3ToolStripMenuItem})
        Me.SearchToolStripMenuItem.Name = "SearchToolStripMenuItem"
        Me.SearchToolStripMenuItem.Size = New System.Drawing.Size(54, 20)
        Me.SearchToolStripMenuItem.Text = "Search"
        '
        'FindToolStripMenuItem1
        '
        Me.FindToolStripMenuItem1.BackColor = System.Drawing.Color.Silver
        Me.FindToolStripMenuItem1.Name = "FindToolStripMenuItem1"
        Me.FindToolStripMenuItem1.Size = New System.Drawing.Size(151, 22)
        Me.FindToolStripMenuItem1.Text = "Find..."
        '
        'FindNextF3ToolStripMenuItem
        '
        Me.FindNextF3ToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.FindNextF3ToolStripMenuItem.Name = "FindNextF3ToolStripMenuItem"
        Me.FindNextF3ToolStripMenuItem.Size = New System.Drawing.Size(151, 22)
        Me.FindNextF3ToolStripMenuItem.Text = "Find Next     F3"
        '
        'HelpToolStripMenuItem1
        '
        Me.HelpToolStripMenuItem1.BackColor = System.Drawing.Color.Silver
        Me.HelpToolStripMenuItem1.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.HelpTopicsToolStripMenuItem, Me.AboutNotepadToolStripMenuItem})
        Me.HelpToolStripMenuItem1.Name = "HelpToolStripMenuItem1"
        Me.HelpToolStripMenuItem1.Size = New System.Drawing.Size(44, 20)
        Me.HelpToolStripMenuItem1.Text = "Help"
        '
        'HelpTopicsToolStripMenuItem
        '
        Me.HelpTopicsToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.HelpTopicsToolStripMenuItem.Name = "HelpTopicsToolStripMenuItem"
        Me.HelpTopicsToolStripMenuItem.Size = New System.Drawing.Size(156, 22)
        Me.HelpTopicsToolStripMenuItem.Text = "Help Topics"
        '
        'AboutNotepadToolStripMenuItem
        '
        Me.AboutNotepadToolStripMenuItem.BackColor = System.Drawing.Color.Silver
        Me.AboutNotepadToolStripMenuItem.Name = "AboutNotepadToolStripMenuItem"
        Me.AboutNotepadToolStripMenuItem.Size = New System.Drawing.Size(156, 22)
        Me.AboutNotepadToolStripMenuItem.Text = "About Notepad"
        '
        'look
        '
        '
        'program
        '
        Me.program.BackColor = System.Drawing.Color.Silver
        Me.program.Controls.Add(Me.notepadtextbox)
        Me.program.Controls.Add(Me.MenuStrip1)
        Me.program.Controls.Add(Me.programtopbar)
        Me.program.Controls.Add(Me.toprightcorner)
        Me.program.Controls.Add(Me.bottomrightcorner)
        Me.program.Controls.Add(Me.bottomleftcorner)
        Me.program.Controls.Add(Me.topleftcorner)
        Me.program.Controls.Add(Me.left)
        Me.program.Controls.Add(Me.bottom)
        Me.program.Controls.Add(Me.right)
        Me.program.Controls.Add(Me.top)
        Me.program.Dock = System.Windows.Forms.DockStyle.Fill
        Me.program.Location = New System.Drawing.Point(0, 0)
        Me.program.Name = "program"
        Me.program.Size = New System.Drawing.Size(540, 424)
        Me.program.TabIndex = 4
        '
        'programtopbar
        '
        Me.programtopbar.BackColor = System.Drawing.Color.DarkBlue
        Me.programtopbar.Controls.Add(Me.maximizebutton)
        Me.programtopbar.Controls.Add(Me.minimizebutton)
        Me.programtopbar.Controls.Add(Me.programname)
        Me.programtopbar.Controls.Add(Me.closebutton)
        Me.programtopbar.Dock = System.Windows.Forms.DockStyle.Top
        Me.programtopbar.Location = New System.Drawing.Point(4, 4)
        Me.programtopbar.Name = "programtopbar"
        Me.programtopbar.Size = New System.Drawing.Size(532, 18)
        Me.programtopbar.TabIndex = 0
        '
        'maximizebutton
        '
        Me.maximizebutton.Anchor = System.Windows.Forms.AnchorStyles.Right
        Me.maximizebutton.Image = Global.Histacom.My.Resources.Resources.Maximize
        Me.maximizebutton.Location = New System.Drawing.Point(499, 2)
        Me.maximizebutton.Name = "maximizebutton"
        Me.maximizebutton.Size = New System.Drawing.Size(16, 14)
        Me.maximizebutton.TabIndex = 6
        Me.maximizebutton.TabStop = False
        '
        'minimizebutton
        '
        Me.minimizebutton.Anchor = System.Windows.Forms.AnchorStyles.Right
        Me.minimizebutton.Image = Global.Histacom.My.Resources.Resources.minimize
        Me.minimizebutton.Location = New System.Drawing.Point(483, 2)
        Me.minimizebutton.Name = "minimizebutton"
        Me.minimizebutton.Size = New System.Drawing.Size(16, 14)
        Me.minimizebutton.TabIndex = 5
        Me.minimizebutton.TabStop = False
        '
        'programname
        '
        Me.programname.AutoSize = True
        Me.programname.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.programname.ForeColor = System.Drawing.Color.White
        Me.programname.Location = New System.Drawing.Point(3, 2)
        Me.programname.Name = "programname"
        Me.programname.Size = New System.Drawing.Size(55, 13)
        Me.programname.TabIndex = 3
        Me.programname.Text = "Notepad"
        '
        'closebutton
        '
        Me.closebutton.Anchor = System.Windows.Forms.AnchorStyles.Right
        Me.closebutton.Image = Global.Histacom.My.Resources.Resources.close
        Me.closebutton.Location = New System.Drawing.Point(515, 2)
        Me.closebutton.Name = "closebutton"
        Me.closebutton.Size = New System.Drawing.Size(16, 14)
        Me.closebutton.TabIndex = 4
        Me.closebutton.TabStop = False
        '
        'toprightcorner
        '
        Me.toprightcorner.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.toprightcorner.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95toprightcorner
        Me.toprightcorner.Location = New System.Drawing.Point(536, 0)
        Me.toprightcorner.Name = "toprightcorner"
        Me.toprightcorner.Size = New System.Drawing.Size(4, 4)
        Me.toprightcorner.TabIndex = 6
        '
        'bottomrightcorner
        '
        Me.bottomrightcorner.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.bottomrightcorner.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95bottomrightcorner
        Me.bottomrightcorner.Cursor = System.Windows.Forms.Cursors.SizeNWSE
        Me.bottomrightcorner.Location = New System.Drawing.Point(536, 420)
        Me.bottomrightcorner.Name = "bottomrightcorner"
        Me.bottomrightcorner.Size = New System.Drawing.Size(4, 4)
        Me.bottomrightcorner.TabIndex = 4
        '
        'bottomleftcorner
        '
        Me.bottomleftcorner.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.bottomleftcorner.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95bottomleftcorner
        Me.bottomleftcorner.Location = New System.Drawing.Point(0, 420)
        Me.bottomleftcorner.Name = "bottomleftcorner"
        Me.bottomleftcorner.Size = New System.Drawing.Size(4, 4)
        Me.bottomleftcorner.TabIndex = 2
        '
        'topleftcorner
        '
        Me.topleftcorner.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95topleftcorner
        Me.topleftcorner.Location = New System.Drawing.Point(0, 0)
        Me.topleftcorner.Name = "topleftcorner"
        Me.topleftcorner.Size = New System.Drawing.Size(4, 4)
        Me.topleftcorner.TabIndex = 1
        '
        'left
        '
        Me.left.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95leftside
        Me.left.Dock = System.Windows.Forms.DockStyle.Left
        Me.left.Location = New System.Drawing.Point(0, 4)
        Me.left.Name = "left"
        Me.left.Size = New System.Drawing.Size(4, 416)
        Me.left.TabIndex = 3
        '
        'bottom
        '
        Me.bottom.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95bottom
        Me.bottom.Cursor = System.Windows.Forms.Cursors.SizeNS
        Me.bottom.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.bottom.Location = New System.Drawing.Point(0, 420)
        Me.bottom.Name = "bottom"
        Me.bottom.Size = New System.Drawing.Size(536, 4)
        Me.bottom.TabIndex = 5
        '
        'right
        '
        Me.right.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95rightside
        Me.right.Cursor = System.Windows.Forms.Cursors.SizeWE
        Me.right.Dock = System.Windows.Forms.DockStyle.Right
        Me.right.Location = New System.Drawing.Point(536, 4)
        Me.right.Name = "right"
        Me.right.Size = New System.Drawing.Size(4, 420)
        Me.right.TabIndex = 7
        '
        'top
        '
        Me.top.BackgroundImage = Global.Histacom.My.Resources.Resources.windows95top
        Me.top.Dock = System.Windows.Forms.DockStyle.Top
        Me.top.Location = New System.Drawing.Point(0, 0)
        Me.top.Name = "top"
        Me.top.Size = New System.Drawing.Size(540, 4)
        Me.top.TabIndex = 8
        '
        'pullside
        '
        Me.pullside.Interval = 1
        '
        'pullbottom
        '
        Me.pullbottom.Interval = 1
        '
        'pullbs
        '
        Me.pullbs.Interval = 1
        '
        'formnotepad
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(540, 424)
        Me.Controls.Add(Me.program)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
        Me.Name = "formnotepad"
        Me.Text = "notepad"
        Me.TopMost = True
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.program.ResumeLayout(False)
        Me.program.PerformLayout()
        Me.programtopbar.ResumeLayout(False)
        Me.programtopbar.PerformLayout()
        CType(Me.maximizebutton, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.minimizebutton, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.closebutton, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents notepadtextbox As System.Windows.Forms.TextBox
    Friend WithEvents MenuStrip1 As System.Windows.Forms.MenuStrip
    Friend WithEvents FileToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents NewToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents OpenToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SaveToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SaveAsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PageSetupToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PrintToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ExitToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents EditToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents UndoCtrlZToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CutCtrlXToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CopyCtrlCToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PasteCtrlVToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DeleteDelToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SelectAllToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents TimeDateF5ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents WordWrapToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SearchToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FindToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FindNextF3ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents HelpToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents HelpTopicsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents AboutNotepadToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents Timer1 As System.Windows.Forms.Timer
    Friend WithEvents look As System.Windows.Forms.Timer
    Friend WithEvents program As System.Windows.Forms.Panel
    Friend WithEvents toprightcorner As System.Windows.Forms.Panel
    Friend WithEvents bottomrightcorner As System.Windows.Forms.Panel
    Friend WithEvents bottomleftcorner As System.Windows.Forms.Panel
    Friend WithEvents topleftcorner As System.Windows.Forms.Panel
    Friend WithEvents programtopbar As System.Windows.Forms.Panel
    Friend WithEvents maximizebutton As System.Windows.Forms.PictureBox
    Friend WithEvents minimizebutton As System.Windows.Forms.PictureBox
    Friend WithEvents programname As System.Windows.Forms.Label
    Friend WithEvents closebutton As System.Windows.Forms.PictureBox
    Friend WithEvents left As System.Windows.Forms.Panel
    Friend WithEvents bottom As System.Windows.Forms.Panel
    Friend WithEvents right As System.Windows.Forms.Panel
    Friend WithEvents top As System.Windows.Forms.Panel
    Friend WithEvents pullside As System.Windows.Forms.Timer
    Friend WithEvents pullbottom As System.Windows.Forms.Timer
    Friend WithEvents pullbs As System.Windows.Forms.Timer
    Friend WithEvents FileToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents EditToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SearchToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FormatToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
End Class
