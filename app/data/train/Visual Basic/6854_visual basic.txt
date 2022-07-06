<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form1
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
        Me.btnExit = New System.Windows.Forms.Button()
        Me.lblFirstName = New System.Windows.Forms.Label()
        Me.lblLastName = New System.Windows.Forms.Label()
        Me.lblAge = New System.Windows.Forms.Label()
        Me.txtFirst = New System.Windows.Forms.TextBox()
        Me.txtLast = New System.Windows.Forms.TextBox()
        Me.txtAge = New System.Windows.Forms.TextBox()
        Me.btnClick = New System.Windows.Forms.Button()
        Me.lblDisplay = New System.Windows.Forms.TextBox()
        Me.SuspendLayout()
        '
        'btnExit
        '
        Me.btnExit.BackColor = System.Drawing.Color.Red
        Me.btnExit.Location = New System.Drawing.Point(289, 272)
        Me.btnExit.Name = "btnExit"
        Me.btnExit.Size = New System.Drawing.Size(75, 23)
        Me.btnExit.TabIndex = 0
        Me.btnExit.Text = "Close"
        Me.btnExit.UseVisualStyleBackColor = False
        '
        'lblFirstName
        '
        Me.lblFirstName.AutoSize = True
        Me.lblFirstName.BackColor = System.Drawing.Color.Red
        Me.lblFirstName.ForeColor = System.Drawing.SystemColors.ActiveCaptionText
        Me.lblFirstName.Location = New System.Drawing.Point(41, 14)
        Me.lblFirstName.Name = "lblFirstName"
        Me.lblFirstName.Size = New System.Drawing.Size(85, 13)
        Me.lblFirstName.TabIndex = 1
        Me.lblFirstName.Text = "Enter First Name"
        '
        'lblLastName
        '
        Me.lblLastName.AutoSize = True
        Me.lblLastName.BackColor = System.Drawing.Color.Red
        Me.lblLastName.ForeColor = System.Drawing.SystemColors.ActiveCaptionText
        Me.lblLastName.Location = New System.Drawing.Point(41, 49)
        Me.lblLastName.Name = "lblLastName"
        Me.lblLastName.Size = New System.Drawing.Size(86, 13)
        Me.lblLastName.TabIndex = 2
        Me.lblLastName.Text = "Enter Last Name"
        '
        'lblAge
        '
        Me.lblAge.AutoSize = True
        Me.lblAge.BackColor = System.Drawing.Color.Red
        Me.lblAge.ForeColor = System.Drawing.SystemColors.ActiveCaptionText
        Me.lblAge.Location = New System.Drawing.Point(40, 94)
        Me.lblAge.Name = "lblAge"
        Me.lblAge.Size = New System.Drawing.Size(79, 13)
        Me.lblAge.TabIndex = 3
        Me.lblAge.Text = "Enter Your Age"
        '
        'txtFirst
        '
        Me.txtFirst.Location = New System.Drawing.Point(162, 11)
        Me.txtFirst.Name = "txtFirst"
        Me.txtFirst.Size = New System.Drawing.Size(151, 20)
        Me.txtFirst.TabIndex = 4
        '
        'txtLast
        '
        Me.txtLast.Location = New System.Drawing.Point(162, 46)
        Me.txtLast.Name = "txtLast"
        Me.txtLast.Size = New System.Drawing.Size(151, 20)
        Me.txtLast.TabIndex = 5
        '
        'txtAge
        '
        Me.txtAge.Location = New System.Drawing.Point(162, 87)
        Me.txtAge.Name = "txtAge"
        Me.txtAge.Size = New System.Drawing.Size(151, 20)
        Me.txtAge.TabIndex = 6
        '
        'btnClick
        '
        Me.btnClick.Location = New System.Drawing.Point(43, 123)
        Me.btnClick.Name = "btnClick"
        Me.btnClick.Size = New System.Drawing.Size(270, 42)
        Me.btnClick.TabIndex = 7
        Me.btnClick.Text = "Click Here When Done"
        Me.btnClick.UseVisualStyleBackColor = True
        '
        'lblDisplay
        '
        Me.lblDisplay.Location = New System.Drawing.Point(43, 183)
        Me.lblDisplay.Multiline = True
        Me.lblDisplay.Name = "lblDisplay"
        Me.lblDisplay.Size = New System.Drawing.Size(270, 74)
        Me.lblDisplay.TabIndex = 8
        Me.lblDisplay.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.Coral
        Me.ClientSize = New System.Drawing.Size(376, 307)
        Me.Controls.Add(Me.lblDisplay)
        Me.Controls.Add(Me.btnClick)
        Me.Controls.Add(Me.txtAge)
        Me.Controls.Add(Me.txtLast)
        Me.Controls.Add(Me.txtFirst)
        Me.Controls.Add(Me.lblAge)
        Me.Controls.Add(Me.lblLastName)
        Me.Controls.Add(Me.lblFirstName)
        Me.Controls.Add(Me.btnExit)
        Me.ForeColor = System.Drawing.SystemColors.ActiveCaptionText
        Me.Name = "Form1"
        Me.Text = "Input & Text Boxes"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents btnExit As System.Windows.Forms.Button
    Friend WithEvents lblFirstName As System.Windows.Forms.Label
    Friend WithEvents lblLastName As System.Windows.Forms.Label
    Friend WithEvents lblAge As System.Windows.Forms.Label
    Friend WithEvents txtFirst As System.Windows.Forms.TextBox
    Friend WithEvents txtLast As System.Windows.Forms.TextBox
    Friend WithEvents txtAge As System.Windows.Forms.TextBox
    Friend WithEvents btnClick As System.Windows.Forms.Button
    Friend WithEvents lblDisplay As System.Windows.Forms.TextBox

End Class
