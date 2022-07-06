<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmLettGrSlect
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
        Me.lblCourseName = New System.Windows.Forms.Label()
        Me.txtCourse = New System.Windows.Forms.TextBox()
        Me.lblLetterGrade = New System.Windows.Forms.Label()
        Me.txtGrade = New System.Windows.Forms.TextBox()
        Me.btnCmd = New System.Windows.Forms.Button()
        Me.btnCmdClear = New System.Windows.Forms.Button()
        Me.txtDisplay = New System.Windows.Forms.TextBox()
        Me.btnExit = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(32, 19)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(341, 24)
        Me.Label1.TabIndex = 6
        Me.Label1.Text = "Welcome to Letter Grade Calculator"
        '
        'lblCourseName
        '
        Me.lblCourseName.AutoSize = True
        Me.lblCourseName.Font = New System.Drawing.Font("Microsoft Sans Serif", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblCourseName.Location = New System.Drawing.Point(32, 77)
        Me.lblCourseName.Name = "lblCourseName"
        Me.lblCourseName.Size = New System.Drawing.Size(194, 24)
        Me.lblCourseName.TabIndex = 9
        Me.lblCourseName.Text = "&Enter Course Name"
        '
        'txtCourse
        '
        Me.txtCourse.Location = New System.Drawing.Point(242, 82)
        Me.txtCourse.Multiline = True
        Me.txtCourse.Name = "txtCourse"
        Me.txtCourse.Size = New System.Drawing.Size(162, 24)
        Me.txtCourse.TabIndex = 10
        '
        'lblLetterGrade
        '
        Me.lblLetterGrade.AutoSize = True
        Me.lblLetterGrade.Font = New System.Drawing.Font("Microsoft Sans Serif", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblLetterGrade.Location = New System.Drawing.Point(32, 143)
        Me.lblLetterGrade.Name = "lblLetterGrade"
        Me.lblLetterGrade.Size = New System.Drawing.Size(204, 24)
        Me.lblLetterGrade.TabIndex = 11
        Me.lblLetterGrade.Text = "Enter Grade Number"
        '
        'txtGrade
        '
        Me.txtGrade.Location = New System.Drawing.Point(242, 143)
        Me.txtGrade.Multiline = True
        Me.txtGrade.Name = "txtGrade"
        Me.txtGrade.Size = New System.Drawing.Size(162, 24)
        Me.txtGrade.TabIndex = 12
        '
        'btnCmd
        '
        Me.btnCmd.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnCmd.Location = New System.Drawing.Point(41, 191)
        Me.btnCmd.Name = "btnCmd"
        Me.btnCmd.Size = New System.Drawing.Size(195, 35)
        Me.btnCmd.TabIndex = 15
        Me.btnCmd.Text = "Calculate Letter"
        Me.btnCmd.UseVisualStyleBackColor = True
        '
        'btnCmdClear
        '
        Me.btnCmdClear.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnCmdClear.Location = New System.Drawing.Point(242, 191)
        Me.btnCmdClear.Name = "btnCmdClear"
        Me.btnCmdClear.Size = New System.Drawing.Size(107, 35)
        Me.btnCmdClear.TabIndex = 16
        Me.btnCmdClear.Text = "Clear"
        Me.btnCmdClear.UseVisualStyleBackColor = True
        '
        'txtDisplay
        '
        Me.txtDisplay.Location = New System.Drawing.Point(36, 232)
        Me.txtDisplay.Multiline = True
        Me.txtDisplay.Name = "txtDisplay"
        Me.txtDisplay.Size = New System.Drawing.Size(323, 242)
        Me.txtDisplay.TabIndex = 17
        Me.txtDisplay.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'btnExit
        '
        Me.btnExit.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnExit.Location = New System.Drawing.Point(365, 442)
        Me.btnExit.Name = "btnExit"
        Me.btnExit.Size = New System.Drawing.Size(58, 32)
        Me.btnExit.TabIndex = 18
        Me.btnExit.Text = "Exit"
        Me.btnExit.UseVisualStyleBackColor = True
        '
        'frmLettGrSlect
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.SteelBlue
        Me.ClientSize = New System.Drawing.Size(424, 499)
        Me.Controls.Add(Me.btnExit)
        Me.Controls.Add(Me.txtDisplay)
        Me.Controls.Add(Me.btnCmdClear)
        Me.Controls.Add(Me.btnCmd)
        Me.Controls.Add(Me.txtGrade)
        Me.Controls.Add(Me.lblLetterGrade)
        Me.Controls.Add(Me.txtCourse)
        Me.Controls.Add(Me.lblCourseName)
        Me.Controls.Add(Me.Label1)
        Me.Name = "frmLettGrSlect"
        Me.Text = "Letter Grade Select Case"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents lblCourseName As System.Windows.Forms.Label
    Friend WithEvents txtCourse As System.Windows.Forms.TextBox
    Friend WithEvents lblLetterGrade As System.Windows.Forms.Label
    Friend WithEvents txtGrade As System.Windows.Forms.TextBox
    Friend WithEvents btnCmd As System.Windows.Forms.Button
    Friend WithEvents btnCmdClear As System.Windows.Forms.Button
    Friend WithEvents txtDisplay As System.Windows.Forms.TextBox
    Friend WithEvents btnExit As System.Windows.Forms.Button

End Class
