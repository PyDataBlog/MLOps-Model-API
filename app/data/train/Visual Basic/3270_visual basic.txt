<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmSpellCheck
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
        Me.tabSpellCheck = New System.Windows.Forms.TabControl()
        Me.SuspendLayout()
        '
        'tabSpellCheck
        '
        Me.tabSpellCheck.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tabSpellCheck.Location = New System.Drawing.Point(1, 0)
        Me.tabSpellCheck.Name = "tabSpellCheck"
        Me.tabSpellCheck.SelectedIndex = 0
        Me.tabSpellCheck.Size = New System.Drawing.Size(620, 399)
        Me.tabSpellCheck.TabIndex = 0
        '
        'frmSpellCheck
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(620, 399)
        Me.Controls.Add(Me.tabSpellCheck)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "frmSpellCheck"
        Me.Text = "Spelling Checker Suggestions"
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents tabSpellCheck As System.Windows.Forms.TabControl
End Class
