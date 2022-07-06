<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class MainForm
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(MainForm))
        Me.MenuStrip = New System.Windows.Forms.MenuStrip()
        Me.FileToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.OpenFolderToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExitToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.FolderBrowserDialog = New System.Windows.Forms.FolderBrowserDialog()
        Me.clstWeaponScripts = New System.Windows.Forms.CheckedListBox()
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.ChangeCrosshairsToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.MenuStrip.SuspendLayout()
        Me.ToolStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'MenuStrip
        '
        Me.MenuStrip.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FileToolStripMenuItem})
        Me.MenuStrip.Location = New System.Drawing.Point(0, 0)
        Me.MenuStrip.Name = "MenuStrip"
        Me.MenuStrip.Size = New System.Drawing.Size(811, 28)
        Me.MenuStrip.TabIndex = 0
        Me.MenuStrip.Text = "MenuStrip1"
        '
        'FileToolStripMenuItem
        '
        Me.FileToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.OpenFolderToolStripMenuItem, Me.ExitToolStripMenuItem})
        Me.FileToolStripMenuItem.Name = "FileToolStripMenuItem"
        Me.FileToolStripMenuItem.Size = New System.Drawing.Size(44, 24)
        Me.FileToolStripMenuItem.Text = "File"
        '
        'OpenFolderToolStripMenuItem
        '
        Me.OpenFolderToolStripMenuItem.Name = "OpenFolderToolStripMenuItem"
        Me.OpenFolderToolStripMenuItem.Size = New System.Drawing.Size(160, 24)
        Me.OpenFolderToolStripMenuItem.Text = "Open Folder"
        '
        'ExitToolStripMenuItem
        '
        Me.ExitToolStripMenuItem.Name = "ExitToolStripMenuItem"
        Me.ExitToolStripMenuItem.Size = New System.Drawing.Size(160, 24)
        Me.ExitToolStripMenuItem.Text = "Exit"
        '
        'clstWeaponScripts
        '
        Me.clstWeaponScripts.Dock = System.Windows.Forms.DockStyle.Fill
        Me.clstWeaponScripts.Font = New System.Drawing.Font("Segoe UI", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.clstWeaponScripts.FormattingEnabled = True
        Me.clstWeaponScripts.Location = New System.Drawing.Point(0, 55)
        Me.clstWeaponScripts.Name = "clstWeaponScripts"
        Me.clstWeaponScripts.Size = New System.Drawing.Size(811, 396)
        Me.clstWeaponScripts.TabIndex = 2
        '
        'ToolStrip1
        '
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ChangeCrosshairsToolStripButton})
        Me.ToolStrip1.Location = New System.Drawing.Point(0, 28)
        Me.ToolStrip1.Name = "ToolStrip1"
        Me.ToolStrip1.Size = New System.Drawing.Size(811, 27)
        Me.ToolStrip1.TabIndex = 3
        Me.ToolStrip1.Text = "ToolStrip1"
        '
        'ChangeCrosshairsToolStripButton
        '
        Me.ChangeCrosshairsToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        Me.ChangeCrosshairsToolStripButton.Image = CType(resources.GetObject("ChangeCrosshairsToolStripButton.Image"), System.Drawing.Image)
        Me.ChangeCrosshairsToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ChangeCrosshairsToolStripButton.Name = "ChangeCrosshairsToolStripButton"
        Me.ChangeCrosshairsToolStripButton.Size = New System.Drawing.Size(133, 24)
        Me.ChangeCrosshairsToolStripButton.Text = "Change Crosshairs"
        '
        'Main
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(8.0!, 16.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(811, 451)
        Me.Controls.Add(Me.clstWeaponScripts)
        Me.Controls.Add(Me.ToolStrip1)
        Me.Controls.Add(Me.MenuStrip)
        Me.MainMenuStrip = Me.MenuStrip
        Me.Name = "Main"
        Me.MenuStrip.ResumeLayout(False)
        Me.MenuStrip.PerformLayout()
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents MenuStrip As System.Windows.Forms.MenuStrip
    Friend WithEvents FileToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents OpenFolderToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ExitToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FolderBrowserDialog As System.Windows.Forms.FolderBrowserDialog
    Friend WithEvents clstWeaponScripts As System.Windows.Forms.CheckedListBox
    Friend WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Friend WithEvents ChangeCrosshairsToolStripButton As System.Windows.Forms.ToolStripButton

End Class
