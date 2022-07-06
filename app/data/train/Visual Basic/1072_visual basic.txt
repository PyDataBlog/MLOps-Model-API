<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormHexEncode
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
        Me.Label1 = New System.Windows.Forms.Label()
        Me.tbStart = New System.Windows.Forms.TextBox()
        Me.tbSrc = New System.Windows.Forms.RichTextBox()
        Me.wbDst = New System.Windows.Forms.WebBrowser()
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(12, 28)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(73, 13)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Start Address:"
        '
        'tbStart
        '
        Me.tbStart.Location = New System.Drawing.Point(91, 25)
        Me.tbStart.Name = "tbStart"
        Me.tbStart.Size = New System.Drawing.Size(83, 20)
        Me.tbStart.TabIndex = 1
        '
        'tbSrc
        '
        Me.tbSrc.Location = New System.Drawing.Point(12, 51)
        Me.tbSrc.Name = "tbSrc"
        Me.tbSrc.Size = New System.Drawing.Size(538, 141)
        Me.tbSrc.TabIndex = 2
        Me.tbSrc.Text = ""
        '
        'wbDst
        '
        Me.wbDst.IsWebBrowserContextMenuEnabled = False
        Me.wbDst.Location = New System.Drawing.Point(15, 204)
        Me.wbDst.MinimumSize = New System.Drawing.Size(20, 20)
        Me.wbDst.Name = "wbDst"
        Me.wbDst.Size = New System.Drawing.Size(534, 209)
        Me.wbDst.TabIndex = 3
        '
        'FormHexEncode
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(562, 425)
        Me.Controls.Add(Me.wbDst)
        Me.Controls.Add(Me.tbSrc)
        Me.Controls.Add(Me.tbStart)
        Me.Controls.Add(Me.Label1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.MaximizeBox = False
        Me.Name = "FormHexEncode"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Hex Encoder"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents tbStart As System.Windows.Forms.TextBox
    Friend WithEvents tbSrc As System.Windows.Forms.RichTextBox
    Friend WithEvents wbDst As System.Windows.Forms.WebBrowser
End Class
