<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmGuessGame
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
        Me.lblInstructions = New System.Windows.Forms.Label()
        Me.lblInput = New System.Windows.Forms.Label()
        Me.btnCMD = New System.Windows.Forms.Button()
        Me.lblDisplay = New System.Windows.Forms.Label()
        Me.btnExit = New System.Windows.Forms.Button()
        Me.List1 = New System.Windows.Forms.ListBox()
        Me.lblGuess = New System.Windows.Forms.Label()
        Me.lblCounter = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'lblInstructions
        '
        Me.lblInstructions.AutoSize = True
        Me.lblInstructions.Font = New System.Drawing.Font("Franklin Gothic Medium", 15.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblInstructions.ForeColor = System.Drawing.SystemColors.WindowText
        Me.lblInstructions.Location = New System.Drawing.Point(12, 20)
        Me.lblInstructions.Name = "lblInstructions"
        Me.lblInstructions.Size = New System.Drawing.Size(323, 26)
        Me.lblInstructions.TabIndex = 0
        Me.lblInstructions.Text = "Welcome to the guessing game"
        '
        'lblInput
        '
        Me.lblInput.AutoSize = True
        Me.lblInput.Font = New System.Drawing.Font("Lucida Sans Unicode", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblInput.ForeColor = System.Drawing.SystemColors.WindowText
        Me.lblInput.Location = New System.Drawing.Point(39, 57)
        Me.lblInput.Name = "lblInput"
        Me.lblInput.Size = New System.Drawing.Size(274, 20)
        Me.lblInput.TabIndex = 1
        Me.lblInput.Text = "Enter a number from 1 to 100"
        '
        'btnCMD
        '
        Me.btnCMD.BackColor = System.Drawing.Color.White
        Me.btnCMD.Font = New System.Drawing.Font("Tahoma", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnCMD.Location = New System.Drawing.Point(77, 94)
        Me.btnCMD.Name = "btnCMD"
        Me.btnCMD.Size = New System.Drawing.Size(182, 93)
        Me.btnCMD.TabIndex = 3
        Me.btnCMD.Text = "Start Guessing"
        Me.btnCMD.UseVisualStyleBackColor = False
        '
        'lblDisplay
        '
        Me.lblDisplay.AutoSize = True
        Me.lblDisplay.Font = New System.Drawing.Font("Lucida Sans Unicode", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblDisplay.Location = New System.Drawing.Point(12, 250)
        Me.lblDisplay.Name = "lblDisplay"
        Me.lblDisplay.Size = New System.Drawing.Size(0, 20)
        Me.lblDisplay.TabIndex = 4
        '
        'btnExit
        '
        Me.btnExit.BackColor = System.Drawing.Color.White
        Me.btnExit.Font = New System.Drawing.Font("Tahoma", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnExit.Location = New System.Drawing.Point(285, 435)
        Me.btnExit.Name = "btnExit"
        Me.btnExit.Size = New System.Drawing.Size(60, 38)
        Me.btnExit.TabIndex = 5
        Me.btnExit.Text = "Exit"
        Me.btnExit.UseVisualStyleBackColor = False
        '
        'List1
        '
        Me.List1.FormattingEnabled = True
        Me.List1.ItemHeight = 16
        Me.List1.Location = New System.Drawing.Point(12, 265)
        Me.List1.Name = "List1"
        Me.List1.Size = New System.Drawing.Size(317, 164)
        Me.List1.TabIndex = 6
        '
        'lblGuess
        '
        Me.lblGuess.Location = New System.Drawing.Point(12, 227)
        Me.lblGuess.Name = "lblGuess"
        Me.lblGuess.Size = New System.Drawing.Size(316, 35)
        Me.lblGuess.TabIndex = 7
        Me.lblGuess.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'lblCounter
        '
        Me.lblCounter.Location = New System.Drawing.Point(14, 190)
        Me.lblCounter.Name = "lblCounter"
        Me.lblCounter.Size = New System.Drawing.Size(316, 35)
        Me.lblCounter.TabIndex = 8
        Me.lblCounter.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'frmGuessGame
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(7.0!, 16.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.Silver
        Me.ClientSize = New System.Drawing.Size(347, 475)
        Me.Controls.Add(Me.lblCounter)
        Me.Controls.Add(Me.lblGuess)
        Me.Controls.Add(Me.List1)
        Me.Controls.Add(Me.btnExit)
        Me.Controls.Add(Me.lblDisplay)
        Me.Controls.Add(Me.btnCMD)
        Me.Controls.Add(Me.lblInput)
        Me.Controls.Add(Me.lblInstructions)
        Me.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Margin = New System.Windows.Forms.Padding(3, 4, 3, 4)
        Me.Name = "frmGuessGame"
        Me.Text = "Guessing The Number"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents lblInstructions As System.Windows.Forms.Label
    Friend WithEvents lblInput As System.Windows.Forms.Label
    Friend WithEvents btnCMD As System.Windows.Forms.Button
    Friend WithEvents lblDisplay As System.Windows.Forms.Label
    Friend WithEvents btnExit As System.Windows.Forms.Button
    Friend WithEvents List1 As System.Windows.Forms.ListBox
    Friend WithEvents lblGuess As System.Windows.Forms.Label
    Friend WithEvents lblCounter As System.Windows.Forms.Label

End Class
