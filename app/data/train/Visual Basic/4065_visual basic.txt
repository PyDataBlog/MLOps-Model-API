<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class InsertSnippetForm
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(InsertSnippetForm))
        Me.CopyToClipboardBt = New System.Windows.Forms.Button()
        Me.InsertAtCursorBt = New System.Windows.Forms.Button()
        Me.ContentsTxt = New System.Windows.Forms.TextBox()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.CancelBt = New System.Windows.Forms.Button()
        Me.GroupBox1.SuspendLayout()
        Me.SuspendLayout()
        '
        'CopyToClipboardBt
        '
        Me.CopyToClipboardBt.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.CopyToClipboardBt.Location = New System.Drawing.Point(261, 239)
        Me.CopyToClipboardBt.Name = "CopyToClipboardBt"
        Me.CopyToClipboardBt.Size = New System.Drawing.Size(107, 23)
        Me.CopyToClipboardBt.TabIndex = 4
        Me.CopyToClipboardBt.Text = "Copy to Clipboard"
        Me.CopyToClipboardBt.UseVisualStyleBackColor = True
        '
        'InsertAtCursorBt
        '
        Me.InsertAtCursorBt.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.InsertAtCursorBt.Location = New System.Drawing.Point(148, 239)
        Me.InsertAtCursorBt.Name = "InsertAtCursorBt"
        Me.InsertAtCursorBt.Size = New System.Drawing.Size(107, 23)
        Me.InsertAtCursorBt.TabIndex = 3
        Me.InsertAtCursorBt.Text = "Insert at Cursor"
        Me.InsertAtCursorBt.UseVisualStyleBackColor = True
        '
        'ContentsTxt
        '
        Me.ContentsTxt.AcceptsReturn = True
        Me.ContentsTxt.AcceptsTab = True
        Me.ContentsTxt.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ContentsTxt.Location = New System.Drawing.Point(6, 19)
        Me.ContentsTxt.Multiline = True
        Me.ContentsTxt.Name = "ContentsTxt"
        Me.ContentsTxt.ScrollBars = System.Windows.Forms.ScrollBars.Both
        Me.ContentsTxt.Size = New System.Drawing.Size(457, 187)
        Me.ContentsTxt.TabIndex = 6
        '
        'GroupBox1
        '
        Me.GroupBox1.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox1.Controls.Add(Me.ContentsTxt)
        Me.GroupBox1.Location = New System.Drawing.Point(12, 12)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(469, 221)
        Me.GroupBox1.TabIndex = 7
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Enter Text"
        '
        'CancelBt
        '
        Me.CancelBt.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.CancelBt.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.CancelBt.Location = New System.Drawing.Point(374, 239)
        Me.CancelBt.Name = "CancelBt"
        Me.CancelBt.Size = New System.Drawing.Size(107, 23)
        Me.CancelBt.TabIndex = 8
        Me.CancelBt.Text = "Cancel"
        Me.CancelBt.UseVisualStyleBackColor = True
        '
        'InsertSnippetForm
        '
        Me.AcceptButton = Me.InsertAtCursorBt
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.CancelButton = Me.CancelBt
        Me.ClientSize = New System.Drawing.Size(493, 272)
        Me.Controls.Add(Me.CancelBt)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.CopyToClipboardBt)
        Me.Controls.Add(Me.InsertAtCursorBt)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "InsertSnippetForm"
        Me.Text = "InsertSnippetForm"
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents CopyToClipboardBt As Windows.Forms.Button
    Friend WithEvents InsertAtCursorBt As Windows.Forms.Button
    Friend WithEvents ContentsTxt As Windows.Forms.TextBox
    Friend WithEvents GroupBox1 As Windows.Forms.GroupBox
    Friend WithEvents CancelBt As Windows.Forms.Button
End Class
