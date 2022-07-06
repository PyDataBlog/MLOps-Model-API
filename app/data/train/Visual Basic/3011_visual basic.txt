<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FindReplace
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
        Me.TXTsearch = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.BTNnext = New System.Windows.Forms.Button()
        Me.BTNcancel = New System.Windows.Forms.Button()
        Me.CHKcase = New System.Windows.Forms.CheckBox()
        Me.BTNprev = New System.Windows.Forms.Button()
        Me.TXTreplace = New System.Windows.Forms.TextBox()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.BTNreplaceAll = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'TXTsearch
        '
        Me.TXTsearch.Location = New System.Drawing.Point(88, 11)
        Me.TXTsearch.Name = "TXTsearch"
        Me.TXTsearch.Size = New System.Drawing.Size(153, 20)
        Me.TXTsearch.TabIndex = 3
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(12, 14)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(56, 13)
        Me.Label1.TabIndex = 1
        Me.Label1.Text = "Find what:"
        '
        'BTNnext
        '
        Me.BTNnext.Location = New System.Drawing.Point(256, 38)
        Me.BTNnext.Name = "BTNnext"
        Me.BTNnext.Size = New System.Drawing.Size(79, 23)
        Me.BTNnext.TabIndex = 1
        Me.BTNnext.Text = "Find Next"
        Me.BTNnext.UseVisualStyleBackColor = True
        '
        'BTNcancel
        '
        Me.BTNcancel.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.BTNcancel.Location = New System.Drawing.Point(256, 125)
        Me.BTNcancel.Name = "BTNcancel"
        Me.BTNcancel.Size = New System.Drawing.Size(79, 23)
        Me.BTNcancel.TabIndex = 2
        Me.BTNcancel.Text = "Cancel"
        Me.BTNcancel.UseVisualStyleBackColor = True
        '
        'CHKcase
        '
        Me.CHKcase.AutoSize = True
        Me.CHKcase.Location = New System.Drawing.Point(12, 84)
        Me.CHKcase.Name = "CHKcase"
        Me.CHKcase.Size = New System.Drawing.Size(94, 17)
        Me.CHKcase.TabIndex = 4
        Me.CHKcase.Text = "Case sensitive"
        Me.CHKcase.UseVisualStyleBackColor = True
        '
        'BTNprev
        '
        Me.BTNprev.Location = New System.Drawing.Point(256, 9)
        Me.BTNprev.Name = "BTNprev"
        Me.BTNprev.Size = New System.Drawing.Size(79, 23)
        Me.BTNprev.TabIndex = 4
        Me.BTNprev.Text = "Find Previous"
        Me.BTNprev.UseVisualStyleBackColor = True
        '
        'TXTreplace
        '
        Me.TXTreplace.Location = New System.Drawing.Point(88, 40)
        Me.TXTreplace.Name = "TXTreplace"
        Me.TXTreplace.Size = New System.Drawing.Size(153, 20)
        Me.TXTreplace.TabIndex = 6
        '
        'Button1
        '
        Me.Button1.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.Button1.Location = New System.Drawing.Point(256, 67)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(79, 23)
        Me.Button1.TabIndex = 8
        Me.Button1.Text = "Replace"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(12, 43)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(72, 13)
        Me.Label2.TabIndex = 9
        Me.Label2.Text = "Replace with:"
        '
        'BTNreplaceAll
        '
        Me.BTNreplaceAll.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.BTNreplaceAll.Location = New System.Drawing.Point(256, 96)
        Me.BTNreplaceAll.Name = "BTNreplaceAll"
        Me.BTNreplaceAll.Size = New System.Drawing.Size(79, 23)
        Me.BTNreplaceAll.TabIndex = 10
        Me.BTNreplaceAll.Text = "Replace All"
        Me.BTNreplaceAll.UseVisualStyleBackColor = True
        '
        'FindReplace
        '
        Me.AcceptButton = Me.BTNnext
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.CancelButton = Me.BTNcancel
        Me.ClientSize = New System.Drawing.Size(347, 159)
        Me.Controls.Add(Me.BTNreplaceAll)
        Me.Controls.Add(Me.TXTreplace)
        Me.Controls.Add(Me.TXTsearch)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.BTNprev)
        Me.Controls.Add(Me.CHKcase)
        Me.Controls.Add(Me.BTNcancel)
        Me.Controls.Add(Me.BTNnext)
        Me.Controls.Add(Me.Label1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "FindReplace"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Find"
        Me.TopMost = True
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents TXTsearch As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents BTNnext As System.Windows.Forms.Button
    Friend WithEvents BTNcancel As System.Windows.Forms.Button
    Friend WithEvents CHKcase As System.Windows.Forms.CheckBox
    Friend WithEvents BTNprev As System.Windows.Forms.Button
    Friend WithEvents TXTreplace As System.Windows.Forms.TextBox
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents BTNreplaceAll As System.Windows.Forms.Button
End Class
