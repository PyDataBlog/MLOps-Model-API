<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormMain
    Inherits System.Windows.Forms.Form

    'Das Formular überschreibt den Löschvorgang, um die Komponentenliste zu bereinigen.
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

    'Wird vom Windows Form-Designer benötigt.
    Private components As System.ComponentModel.IContainer

    'Hinweis: Die folgende Prozedur ist für den Windows Form-Designer erforderlich.
    'Das Bearbeiten ist mit dem Windows Form-Designer möglich.  
    'Das Bearbeiten mit dem Code-Editor ist nicht möglich.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormMain))
        Me.MenuStripMain = New System.Windows.Forms.MenuStrip
        Me.FileToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.SaveDatabaseToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.RefreshDatabaseToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.ViewRawDatabaseToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator
        Me.PrintSelectedItemsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator
        Me.QuitToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.EditToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.MediaDatabaseInExcelToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.ToolStripSeparator3 = New System.Windows.Forms.ToolStripSeparator
        Me.EditSelectedItemToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.DeleteSelectedItemsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.ToolStripSeparator4 = New System.Windows.Forms.ToolStripSeparator
        Me.AddItemToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.ToolStripSeparator5 = New System.Windows.Forms.ToolStripSeparator
        Me.PreferencesToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.ViewToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.FindToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.HelpToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.AboutToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.DataGridViewMedia = New System.Windows.Forms.DataGridView
        Me.StatusStripMain = New System.Windows.Forms.StatusStrip
        Me.ToolStripStatusLabelVersion = New System.Windows.Forms.ToolStripStatusLabel
        Me.ToolStripStatusLabelTotalItems = New System.Windows.Forms.ToolStripStatusLabel
        Me.ToolStripStatusLabelLastUpdated = New System.Windows.Forms.ToolStripStatusLabel
        Me.GroupBoxFind = New System.Windows.Forms.GroupBox
        Me.TextBoxFind = New System.Windows.Forms.TextBox
        Me.ComboBoxField = New System.Windows.Forms.ComboBox
        Me.ButtonFind = New System.Windows.Forms.Button
        Me.ButtonFindClear = New System.Windows.Forms.Button
        Me.ButtonFindHide = New System.Windows.Forms.Button
        Me.MenuStripMain.SuspendLayout()
        CType(Me.DataGridViewMedia, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.StatusStripMain.SuspendLayout()
        Me.GroupBoxFind.SuspendLayout()
        Me.SuspendLayout()
        '
        'MenuStripMain
        '
        Me.MenuStripMain.AccessibleDescription = Nothing
        Me.MenuStripMain.AccessibleName = Nothing
        resources.ApplyResources(Me.MenuStripMain, "MenuStripMain")
        Me.MenuStripMain.BackgroundImage = Nothing
        Me.MenuStripMain.Font = Nothing
        Me.MenuStripMain.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FileToolStripMenuItem, Me.EditToolStripMenuItem, Me.ViewToolStripMenuItem, Me.HelpToolStripMenuItem})
        Me.MenuStripMain.Name = "MenuStripMain"
        '
        'FileToolStripMenuItem
        '
        Me.FileToolStripMenuItem.AccessibleDescription = Nothing
        Me.FileToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.FileToolStripMenuItem, "FileToolStripMenuItem")
        Me.FileToolStripMenuItem.BackgroundImage = Nothing
        Me.FileToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.SaveDatabaseToolStripMenuItem, Me.RefreshDatabaseToolStripMenuItem, Me.ViewRawDatabaseToolStripMenuItem, Me.ToolStripSeparator2, Me.PrintSelectedItemsToolStripMenuItem, Me.ToolStripSeparator1, Me.QuitToolStripMenuItem})
        Me.FileToolStripMenuItem.Name = "FileToolStripMenuItem"
        Me.FileToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'SaveDatabaseToolStripMenuItem
        '
        Me.SaveDatabaseToolStripMenuItem.AccessibleDescription = Nothing
        Me.SaveDatabaseToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.SaveDatabaseToolStripMenuItem, "SaveDatabaseToolStripMenuItem")
        Me.SaveDatabaseToolStripMenuItem.BackgroundImage = Nothing
        Me.SaveDatabaseToolStripMenuItem.Name = "SaveDatabaseToolStripMenuItem"
        Me.SaveDatabaseToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'RefreshDatabaseToolStripMenuItem
        '
        Me.RefreshDatabaseToolStripMenuItem.AccessibleDescription = Nothing
        Me.RefreshDatabaseToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.RefreshDatabaseToolStripMenuItem, "RefreshDatabaseToolStripMenuItem")
        Me.RefreshDatabaseToolStripMenuItem.BackgroundImage = Nothing
        Me.RefreshDatabaseToolStripMenuItem.Name = "RefreshDatabaseToolStripMenuItem"
        Me.RefreshDatabaseToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'ViewRawDatabaseToolStripMenuItem
        '
        Me.ViewRawDatabaseToolStripMenuItem.AccessibleDescription = Nothing
        Me.ViewRawDatabaseToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.ViewRawDatabaseToolStripMenuItem, "ViewRawDatabaseToolStripMenuItem")
        Me.ViewRawDatabaseToolStripMenuItem.BackgroundImage = Nothing
        Me.ViewRawDatabaseToolStripMenuItem.Name = "ViewRawDatabaseToolStripMenuItem"
        Me.ViewRawDatabaseToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.AccessibleDescription = Nothing
        Me.ToolStripSeparator2.AccessibleName = Nothing
        resources.ApplyResources(Me.ToolStripSeparator2, "ToolStripSeparator2")
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        '
        'PrintSelectedItemsToolStripMenuItem
        '
        Me.PrintSelectedItemsToolStripMenuItem.AccessibleDescription = Nothing
        Me.PrintSelectedItemsToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.PrintSelectedItemsToolStripMenuItem, "PrintSelectedItemsToolStripMenuItem")
        Me.PrintSelectedItemsToolStripMenuItem.BackgroundImage = Nothing
        Me.PrintSelectedItemsToolStripMenuItem.Name = "PrintSelectedItemsToolStripMenuItem"
        Me.PrintSelectedItemsToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.AccessibleDescription = Nothing
        Me.ToolStripSeparator1.AccessibleName = Nothing
        resources.ApplyResources(Me.ToolStripSeparator1, "ToolStripSeparator1")
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        '
        'QuitToolStripMenuItem
        '
        Me.QuitToolStripMenuItem.AccessibleDescription = Nothing
        Me.QuitToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.QuitToolStripMenuItem, "QuitToolStripMenuItem")
        Me.QuitToolStripMenuItem.BackgroundImage = Nothing
        Me.QuitToolStripMenuItem.Name = "QuitToolStripMenuItem"
        Me.QuitToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'EditToolStripMenuItem
        '
        Me.EditToolStripMenuItem.AccessibleDescription = Nothing
        Me.EditToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.EditToolStripMenuItem, "EditToolStripMenuItem")
        Me.EditToolStripMenuItem.BackgroundImage = Nothing
        Me.EditToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.MediaDatabaseInExcelToolStripMenuItem, Me.ToolStripSeparator3, Me.EditSelectedItemToolStripMenuItem, Me.DeleteSelectedItemsToolStripMenuItem, Me.ToolStripSeparator4, Me.AddItemToolStripMenuItem, Me.ToolStripSeparator5, Me.PreferencesToolStripMenuItem})
        Me.EditToolStripMenuItem.Name = "EditToolStripMenuItem"
        Me.EditToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'MediaDatabaseInExcelToolStripMenuItem
        '
        Me.MediaDatabaseInExcelToolStripMenuItem.AccessibleDescription = Nothing
        Me.MediaDatabaseInExcelToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.MediaDatabaseInExcelToolStripMenuItem, "MediaDatabaseInExcelToolStripMenuItem")
        Me.MediaDatabaseInExcelToolStripMenuItem.BackgroundImage = Nothing
        Me.MediaDatabaseInExcelToolStripMenuItem.Name = "MediaDatabaseInExcelToolStripMenuItem"
        Me.MediaDatabaseInExcelToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'ToolStripSeparator3
        '
        Me.ToolStripSeparator3.AccessibleDescription = Nothing
        Me.ToolStripSeparator3.AccessibleName = Nothing
        resources.ApplyResources(Me.ToolStripSeparator3, "ToolStripSeparator3")
        Me.ToolStripSeparator3.Name = "ToolStripSeparator3"
        '
        'EditSelectedItemToolStripMenuItem
        '
        Me.EditSelectedItemToolStripMenuItem.AccessibleDescription = Nothing
        Me.EditSelectedItemToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.EditSelectedItemToolStripMenuItem, "EditSelectedItemToolStripMenuItem")
        Me.EditSelectedItemToolStripMenuItem.BackgroundImage = Nothing
        Me.EditSelectedItemToolStripMenuItem.Name = "EditSelectedItemToolStripMenuItem"
        Me.EditSelectedItemToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'DeleteSelectedItemsToolStripMenuItem
        '
        Me.DeleteSelectedItemsToolStripMenuItem.AccessibleDescription = Nothing
        Me.DeleteSelectedItemsToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.DeleteSelectedItemsToolStripMenuItem, "DeleteSelectedItemsToolStripMenuItem")
        Me.DeleteSelectedItemsToolStripMenuItem.BackgroundImage = Nothing
        Me.DeleteSelectedItemsToolStripMenuItem.Name = "DeleteSelectedItemsToolStripMenuItem"
        Me.DeleteSelectedItemsToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'ToolStripSeparator4
        '
        Me.ToolStripSeparator4.AccessibleDescription = Nothing
        Me.ToolStripSeparator4.AccessibleName = Nothing
        resources.ApplyResources(Me.ToolStripSeparator4, "ToolStripSeparator4")
        Me.ToolStripSeparator4.Name = "ToolStripSeparator4"
        '
        'AddItemToolStripMenuItem
        '
        Me.AddItemToolStripMenuItem.AccessibleDescription = Nothing
        Me.AddItemToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.AddItemToolStripMenuItem, "AddItemToolStripMenuItem")
        Me.AddItemToolStripMenuItem.BackgroundImage = Nothing
        Me.AddItemToolStripMenuItem.Name = "AddItemToolStripMenuItem"
        Me.AddItemToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'ToolStripSeparator5
        '
        Me.ToolStripSeparator5.AccessibleDescription = Nothing
        Me.ToolStripSeparator5.AccessibleName = Nothing
        resources.ApplyResources(Me.ToolStripSeparator5, "ToolStripSeparator5")
        Me.ToolStripSeparator5.Name = "ToolStripSeparator5"
        '
        'PreferencesToolStripMenuItem
        '
        Me.PreferencesToolStripMenuItem.AccessibleDescription = Nothing
        Me.PreferencesToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.PreferencesToolStripMenuItem, "PreferencesToolStripMenuItem")
        Me.PreferencesToolStripMenuItem.BackgroundImage = Nothing
        Me.PreferencesToolStripMenuItem.Name = "PreferencesToolStripMenuItem"
        Me.PreferencesToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'ViewToolStripMenuItem
        '
        Me.ViewToolStripMenuItem.AccessibleDescription = Nothing
        Me.ViewToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.ViewToolStripMenuItem, "ViewToolStripMenuItem")
        Me.ViewToolStripMenuItem.BackgroundImage = Nothing
        Me.ViewToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FindToolStripMenuItem})
        Me.ViewToolStripMenuItem.Name = "ViewToolStripMenuItem"
        Me.ViewToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'FindToolStripMenuItem
        '
        Me.FindToolStripMenuItem.AccessibleDescription = Nothing
        Me.FindToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.FindToolStripMenuItem, "FindToolStripMenuItem")
        Me.FindToolStripMenuItem.BackgroundImage = Nothing
        Me.FindToolStripMenuItem.Name = "FindToolStripMenuItem"
        Me.FindToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'HelpToolStripMenuItem
        '
        Me.HelpToolStripMenuItem.AccessibleDescription = Nothing
        Me.HelpToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.HelpToolStripMenuItem, "HelpToolStripMenuItem")
        Me.HelpToolStripMenuItem.BackgroundImage = Nothing
        Me.HelpToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.AboutToolStripMenuItem})
        Me.HelpToolStripMenuItem.Name = "HelpToolStripMenuItem"
        Me.HelpToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'AboutToolStripMenuItem
        '
        Me.AboutToolStripMenuItem.AccessibleDescription = Nothing
        Me.AboutToolStripMenuItem.AccessibleName = Nothing
        resources.ApplyResources(Me.AboutToolStripMenuItem, "AboutToolStripMenuItem")
        Me.AboutToolStripMenuItem.BackgroundImage = Nothing
        Me.AboutToolStripMenuItem.Name = "AboutToolStripMenuItem"
        Me.AboutToolStripMenuItem.ShortcutKeyDisplayString = Nothing
        '
        'DataGridViewMedia
        '
        Me.DataGridViewMedia.AccessibleDescription = Nothing
        Me.DataGridViewMedia.AccessibleName = Nothing
        Me.DataGridViewMedia.AllowUserToAddRows = False
        Me.DataGridViewMedia.AllowUserToDeleteRows = False
        Me.DataGridViewMedia.AllowUserToOrderColumns = True
        resources.ApplyResources(Me.DataGridViewMedia, "DataGridViewMedia")
        Me.DataGridViewMedia.BackgroundImage = Nothing
        Me.DataGridViewMedia.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DataGridViewMedia.Font = Nothing
        Me.DataGridViewMedia.Name = "DataGridViewMedia"
        Me.DataGridViewMedia.ReadOnly = True
        Me.DataGridViewMedia.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        '
        'StatusStripMain
        '
        Me.StatusStripMain.AccessibleDescription = Nothing
        Me.StatusStripMain.AccessibleName = Nothing
        resources.ApplyResources(Me.StatusStripMain, "StatusStripMain")
        Me.StatusStripMain.BackgroundImage = Nothing
        Me.StatusStripMain.Font = Nothing
        Me.StatusStripMain.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripStatusLabelVersion, Me.ToolStripStatusLabelTotalItems, Me.ToolStripStatusLabelLastUpdated})
        Me.StatusStripMain.Name = "StatusStripMain"
        '
        'ToolStripStatusLabelVersion
        '
        Me.ToolStripStatusLabelVersion.AccessibleDescription = Nothing
        Me.ToolStripStatusLabelVersion.AccessibleName = Nothing
        resources.ApplyResources(Me.ToolStripStatusLabelVersion, "ToolStripStatusLabelVersion")
        Me.ToolStripStatusLabelVersion.BackgroundImage = Nothing
        Me.ToolStripStatusLabelVersion.BorderSides = System.Windows.Forms.ToolStripStatusLabelBorderSides.Right
        Me.ToolStripStatusLabelVersion.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        Me.ToolStripStatusLabelVersion.Name = "ToolStripStatusLabelVersion"
        '
        'ToolStripStatusLabelTotalItems
        '
        Me.ToolStripStatusLabelTotalItems.AccessibleDescription = Nothing
        Me.ToolStripStatusLabelTotalItems.AccessibleName = Nothing
        resources.ApplyResources(Me.ToolStripStatusLabelTotalItems, "ToolStripStatusLabelTotalItems")
        Me.ToolStripStatusLabelTotalItems.BackgroundImage = Nothing
        Me.ToolStripStatusLabelTotalItems.BorderSides = System.Windows.Forms.ToolStripStatusLabelBorderSides.Right
        Me.ToolStripStatusLabelTotalItems.Name = "ToolStripStatusLabelTotalItems"
        '
        'ToolStripStatusLabelLastUpdated
        '
        Me.ToolStripStatusLabelLastUpdated.AccessibleDescription = Nothing
        Me.ToolStripStatusLabelLastUpdated.AccessibleName = Nothing
        resources.ApplyResources(Me.ToolStripStatusLabelLastUpdated, "ToolStripStatusLabelLastUpdated")
        Me.ToolStripStatusLabelLastUpdated.BackgroundImage = Nothing
        Me.ToolStripStatusLabelLastUpdated.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        Me.ToolStripStatusLabelLastUpdated.Name = "ToolStripStatusLabelLastUpdated"
        '
        'GroupBoxFind
        '
        Me.GroupBoxFind.AccessibleDescription = Nothing
        Me.GroupBoxFind.AccessibleName = Nothing
        resources.ApplyResources(Me.GroupBoxFind, "GroupBoxFind")
        Me.GroupBoxFind.BackgroundImage = Nothing
        Me.GroupBoxFind.Controls.Add(Me.TextBoxFind)
        Me.GroupBoxFind.Controls.Add(Me.ComboBoxField)
        Me.GroupBoxFind.Controls.Add(Me.ButtonFind)
        Me.GroupBoxFind.Controls.Add(Me.ButtonFindClear)
        Me.GroupBoxFind.Controls.Add(Me.ButtonFindHide)
        Me.GroupBoxFind.Font = Nothing
        Me.GroupBoxFind.Name = "GroupBoxFind"
        Me.GroupBoxFind.TabStop = False
        '
        'TextBoxFind
        '
        Me.TextBoxFind.AccessibleDescription = Nothing
        Me.TextBoxFind.AccessibleName = Nothing
        resources.ApplyResources(Me.TextBoxFind, "TextBoxFind")
        Me.TextBoxFind.BackgroundImage = Nothing
        Me.TextBoxFind.Font = Nothing
        Me.TextBoxFind.Name = "TextBoxFind"
        '
        'ComboBoxField
        '
        Me.ComboBoxField.AccessibleDescription = Nothing
        Me.ComboBoxField.AccessibleName = Nothing
        resources.ApplyResources(Me.ComboBoxField, "ComboBoxField")
        Me.ComboBoxField.BackgroundImage = Nothing
        Me.ComboBoxField.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBoxField.Font = Nothing
        Me.ComboBoxField.FormattingEnabled = True
        Me.ComboBoxField.Name = "ComboBoxField"
        '
        'ButtonFind
        '
        Me.ButtonFind.AccessibleDescription = Nothing
        Me.ButtonFind.AccessibleName = Nothing
        resources.ApplyResources(Me.ButtonFind, "ButtonFind")
        Me.ButtonFind.BackgroundImage = Nothing
        Me.ButtonFind.Font = Nothing
        Me.ButtonFind.Name = "ButtonFind"
        Me.ButtonFind.UseVisualStyleBackColor = True
        '
        'ButtonFindClear
        '
        Me.ButtonFindClear.AccessibleDescription = Nothing
        Me.ButtonFindClear.AccessibleName = Nothing
        resources.ApplyResources(Me.ButtonFindClear, "ButtonFindClear")
        Me.ButtonFindClear.BackgroundImage = Nothing
        Me.ButtonFindClear.Font = Nothing
        Me.ButtonFindClear.Name = "ButtonFindClear"
        Me.ButtonFindClear.UseVisualStyleBackColor = True
        '
        'ButtonFindHide
        '
        Me.ButtonFindHide.AccessibleDescription = Nothing
        Me.ButtonFindHide.AccessibleName = Nothing
        resources.ApplyResources(Me.ButtonFindHide, "ButtonFindHide")
        Me.ButtonFindHide.BackgroundImage = Nothing
        Me.ButtonFindHide.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.ButtonFindHide.Font = Nothing
        Me.ButtonFindHide.Name = "ButtonFindHide"
        Me.ButtonFindHide.UseVisualStyleBackColor = True
        '
        'FormMain
        '
        Me.AcceptButton = Me.ButtonFind
        Me.AccessibleDescription = Nothing
        Me.AccessibleName = Nothing
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackgroundImage = Nothing
        Me.CancelButton = Me.ButtonFindHide
        Me.Controls.Add(Me.DataGridViewMedia)
        Me.Controls.Add(Me.StatusStripMain)
        Me.Controls.Add(Me.GroupBoxFind)
        Me.Controls.Add(Me.MenuStripMain)
        Me.Font = Nothing
        Me.Icon = Nothing
        Me.MainMenuStrip = Me.MenuStripMain
        Me.Name = "FormMain"
        Me.MenuStripMain.ResumeLayout(False)
        Me.MenuStripMain.PerformLayout()
        CType(Me.DataGridViewMedia, System.ComponentModel.ISupportInitialize).EndInit()
        Me.StatusStripMain.ResumeLayout(False)
        Me.StatusStripMain.PerformLayout()
        Me.GroupBoxFind.ResumeLayout(False)
        Me.GroupBoxFind.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents MenuStripMain As System.Windows.Forms.MenuStrip
    Friend WithEvents FileToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents QuitToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DataGridViewMedia As System.Windows.Forms.DataGridView
    Friend WithEvents EditToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents MediaDatabaseInExcelToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PrintSelectedItemsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents SaveDatabaseToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents RefreshDatabaseToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents ViewRawDatabaseToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents StatusStripMain As System.Windows.Forms.StatusStrip
    Friend WithEvents ToolStripStatusLabelVersion As System.Windows.Forms.ToolStripStatusLabel
    Friend WithEvents ToolStripStatusLabelLastUpdated As System.Windows.Forms.ToolStripStatusLabel
    Friend WithEvents ToolStripStatusLabelTotalItems As System.Windows.Forms.ToolStripStatusLabel
    Friend WithEvents GroupBoxFind As System.Windows.Forms.GroupBox
    Friend WithEvents ViewToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FindToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents TextBoxFind As System.Windows.Forms.TextBox
    Friend WithEvents ComboBoxField As System.Windows.Forms.ComboBox
    Friend WithEvents ButtonFind As System.Windows.Forms.Button
    Friend WithEvents ButtonFindClear As System.Windows.Forms.Button
    Friend WithEvents ButtonFindHide As System.Windows.Forms.Button
    Friend WithEvents ToolStripSeparator3 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents DeleteSelectedItemsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents EditSelectedItemToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator4 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents AddItemToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator5 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents PreferencesToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents HelpToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents AboutToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem

End Class
