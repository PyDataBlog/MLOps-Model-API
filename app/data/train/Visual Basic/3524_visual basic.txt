<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmAverageGrade
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
        Me.lblGrade = New System.Windows.Forms.Label()
        Me.txtGrade = New System.Windows.Forms.TextBox()
        Me.btnRecord = New System.Windows.Forms.Button()
        Me.btnDisplay = New System.Windows.Forms.Button()
        Me.lstGradeData = New System.Windows.Forms.ListBox()
        Me.SuspendLayout()
        '
        'lblGrade
        '
        Me.lblGrade.AutoSize = True
        Me.lblGrade.Location = New System.Drawing.Point(12, 30)
        Me.lblGrade.Name = "lblGrade"
        Me.lblGrade.Size = New System.Drawing.Size(39, 13)
        Me.lblGrade.TabIndex = 0
        Me.lblGrade.Text = "Grade:"
        '
        'txtGrade
        '
        Me.txtGrade.Location = New System.Drawing.Point(57, 27)
        Me.txtGrade.Name = "txtGrade"
        Me.txtGrade.Size = New System.Drawing.Size(50, 20)
        Me.txtGrade.TabIndex = 1
        '
        'btnRecord
        '
        Me.btnRecord.Location = New System.Drawing.Point(132, 12)
        Me.btnRecord.Name = "btnRecord"
        Me.btnRecord.Size = New System.Drawing.Size(100, 35)
        Me.btnRecord.TabIndex = 2
        Me.btnRecord.Text = "Record Grade"
        Me.btnRecord.UseVisualStyleBackColor = True
        '
        'btnDisplay
        '
        Me.btnDisplay.Location = New System.Drawing.Point(12, 53)
        Me.btnDisplay.Name = "btnDisplay"
        Me.btnDisplay.Size = New System.Drawing.Size(220, 50)
        Me.btnDisplay.TabIndex = 3
        Me.btnDisplay.Text = "Display Average Grade and Number" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "of Above-Average Grades"
        Me.btnDisplay.UseVisualStyleBackColor = True
        '
        'lstGradeData
        '
        Me.lstGradeData.FormattingEnabled = True
        Me.lstGradeData.Location = New System.Drawing.Point(12, 110)
        Me.lstGradeData.Name = "lstGradeData"
        Me.lstGradeData.Size = New System.Drawing.Size(220, 30)
        Me.lstGradeData.TabIndex = 4
        '
        'frmAverageGrade
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(244, 162)
        Me.Controls.Add(Me.lstGradeData)
        Me.Controls.Add(Me.btnDisplay)
        Me.Controls.Add(Me.btnRecord)
        Me.Controls.Add(Me.txtGrade)
        Me.Controls.Add(Me.lblGrade)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.MaximizeBox = False
        Me.Name = "frmAverageGrade"
        Me.Text = "Grades"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents lblGrade As System.Windows.Forms.Label
    Friend WithEvents txtGrade As System.Windows.Forms.TextBox
    Friend WithEvents btnRecord As System.Windows.Forms.Button
    Friend WithEvents btnDisplay As System.Windows.Forms.Button
    Friend WithEvents lstGradeData As System.Windows.Forms.ListBox

End Class
