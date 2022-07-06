<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmPhysicsCalculator
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
        Me.lblHeading = New System.Windows.Forms.Label
        Me.lblInstructions = New System.Windows.Forms.Label
        Me.radAcceleration = New System.Windows.Forms.RadioButton
        Me.radDisplacement = New System.Windows.Forms.RadioButton
        Me.lblValues = New System.Windows.Forms.Label
        Me.lblDistance = New System.Windows.Forms.Label
        Me.lblAcceleration = New System.Windows.Forms.Label
        Me.lblTime = New System.Windows.Forms.Label
        Me.txtDisplacement = New System.Windows.Forms.TextBox
        Me.txtAcceleration = New System.Windows.Forms.TextBox
        Me.txtTime = New System.Windows.Forms.TextBox
        Me.lblResult = New System.Windows.Forms.Label
        Me.lblResult2 = New System.Windows.Forms.Label
        Me.btnCalculate = New System.Windows.Forms.Button
        Me.btnClear = New System.Windows.Forms.Button
        Me.btnExit = New System.Windows.Forms.Button
        Me.radTime = New System.Windows.Forms.RadioButton
        Me.SuspendLayout()
        '
        'lblHeading
        '
        Me.lblHeading.AutoSize = True
        Me.lblHeading.Font = New System.Drawing.Font("Calibri", 18.0!, CType(((System.Drawing.FontStyle.Bold Or System.Drawing.FontStyle.Italic) _
                        Or System.Drawing.FontStyle.Underline), System.Drawing.FontStyle), System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblHeading.Location = New System.Drawing.Point(35, 25)
        Me.lblHeading.Name = "lblHeading"
        Me.lblHeading.Size = New System.Drawing.Size(507, 29)
        Me.lblHeading.TabIndex = 0
        Me.lblHeading.Text = "Physics Acceleration and Displacement Calculator"
        '
        'lblInstructions
        '
        Me.lblInstructions.AutoSize = True
        Me.lblInstructions.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Underline, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblInstructions.Location = New System.Drawing.Point(338, 101)
        Me.lblInstructions.Name = "lblInstructions"
        Me.lblInstructions.Size = New System.Drawing.Size(227, 20)
        Me.lblInstructions.TabIndex = 1
        Me.lblInstructions.Text = "What do you wish to calculate?"
        '
        'radAcceleration
        '
        Me.radAcceleration.AutoSize = True
        Me.radAcceleration.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.radAcceleration.Location = New System.Drawing.Point(393, 159)
        Me.radAcceleration.Name = "radAcceleration"
        Me.radAcceleration.Size = New System.Drawing.Size(107, 22)
        Me.radAcceleration.TabIndex = 2
        Me.radAcceleration.Text = "Acceleration" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.radAcceleration.UseVisualStyleBackColor = True
        '
        'radDisplacement
        '
        Me.radDisplacement.AutoSize = True
        Me.radDisplacement.Checked = True
        Me.radDisplacement.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.radDisplacement.Location = New System.Drawing.Point(393, 126)
        Me.radDisplacement.Name = "radDisplacement"
        Me.radDisplacement.Size = New System.Drawing.Size(116, 22)
        Me.radDisplacement.TabIndex = 1
        Me.radDisplacement.TabStop = True
        Me.radDisplacement.Text = "Displacement"
        Me.radDisplacement.UseVisualStyleBackColor = True
        '
        'lblValues
        '
        Me.lblValues.AutoSize = True
        Me.lblValues.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Underline, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblValues.Location = New System.Drawing.Point(212, 99)
        Me.lblValues.Name = "lblValues"
        Me.lblValues.Size = New System.Drawing.Size(62, 20)
        Me.lblValues.TabIndex = 4
        Me.lblValues.Text = "Values:"
        '
        'lblDistance
        '
        Me.lblDistance.AutoSize = True
        Me.lblDistance.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblDistance.Location = New System.Drawing.Point(11, 128)
        Me.lblDistance.Name = "lblDistance"
        Me.lblDistance.Size = New System.Drawing.Size(162, 18)
        Me.lblDistance.TabIndex = 5
        Me.lblDistance.Text = "Displacement (meters):"
        '
        'lblAcceleration
        '
        Me.lblAcceleration.AutoSize = True
        Me.lblAcceleration.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblAcceleration.Location = New System.Drawing.Point(11, 161)
        Me.lblAcceleration.Name = "lblAcceleration"
        Me.lblAcceleration.Size = New System.Drawing.Size(132, 18)
        Me.lblAcceleration.TabIndex = 6
        Me.lblAcceleration.Text = "Acceleration (m/s):"
        '
        'lblTime
        '
        Me.lblTime.AutoSize = True
        Me.lblTime.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblTime.Location = New System.Drawing.Point(11, 194)
        Me.lblTime.Name = "lblTime"
        Me.lblTime.Size = New System.Drawing.Size(116, 18)
        Me.lblTime.TabIndex = 7
        Me.lblTime.Text = "Time (seconds):"
        '
        'txtDisplacement
        '
        Me.txtDisplacement.Location = New System.Drawing.Point(193, 127)
        Me.txtDisplacement.Name = "txtDisplacement"
        Me.txtDisplacement.Size = New System.Drawing.Size(100, 20)
        Me.txtDisplacement.TabIndex = 4
        Me.txtDisplacement.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'txtAcceleration
        '
        Me.txtAcceleration.Location = New System.Drawing.Point(193, 160)
        Me.txtAcceleration.Name = "txtAcceleration"
        Me.txtAcceleration.Size = New System.Drawing.Size(100, 20)
        Me.txtAcceleration.TabIndex = 5
        Me.txtAcceleration.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'txtTime
        '
        Me.txtTime.Location = New System.Drawing.Point(193, 193)
        Me.txtTime.Name = "txtTime"
        Me.txtTime.Size = New System.Drawing.Size(100, 20)
        Me.txtTime.TabIndex = 6
        Me.txtTime.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'lblResult
        '
        Me.lblResult.AutoSize = True
        Me.lblResult.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblResult.Location = New System.Drawing.Point(200, 243)
        Me.lblResult.Name = "lblResult"
        Me.lblResult.Size = New System.Drawing.Size(176, 18)
        Me.lblResult.TabIndex = 11
        Me.lblResult.Text = "The XXXXXXXXXXXX is:"
        Me.lblResult.Visible = False
        '
        'lblResult2
        '
        Me.lblResult2.AutoSize = True
        Me.lblResult2.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblResult2.Location = New System.Drawing.Point(224, 270)
        Me.lblResult2.Name = "lblResult2"
        Me.lblResult2.Size = New System.Drawing.Size(128, 18)
        Me.lblResult2.TabIndex = 12
        Me.lblResult2.Text = "XXXXXXXXXXXX"
        Me.lblResult2.Visible = False
        '
        'btnCalculate
        '
        Me.btnCalculate.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnCalculate.Location = New System.Drawing.Point(36, 307)
        Me.btnCalculate.Name = "btnCalculate"
        Me.btnCalculate.Size = New System.Drawing.Size(128, 50)
        Me.btnCalculate.TabIndex = 7
        Me.btnCalculate.Text = "Calculate"
        Me.btnCalculate.UseVisualStyleBackColor = True
        '
        'btnClear
        '
        Me.btnClear.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.btnClear.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnClear.Location = New System.Drawing.Point(224, 305)
        Me.btnClear.Name = "btnClear"
        Me.btnClear.Size = New System.Drawing.Size(128, 50)
        Me.btnClear.TabIndex = 8
        Me.btnClear.Text = "Clear"
        Me.btnClear.UseVisualStyleBackColor = True
        '
        'btnExit
        '
        Me.btnExit.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnExit.Location = New System.Drawing.Point(412, 307)
        Me.btnExit.Name = "btnExit"
        Me.btnExit.Size = New System.Drawing.Size(128, 50)
        Me.btnExit.TabIndex = 9
        Me.btnExit.Text = "Exit"
        Me.btnExit.UseVisualStyleBackColor = True
        '
        'radTime
        '
        Me.radTime.AutoSize = True
        Me.radTime.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.radTime.Location = New System.Drawing.Point(393, 192)
        Me.radTime.Name = "radTime"
        Me.radTime.Size = New System.Drawing.Size(59, 22)
        Me.radTime.TabIndex = 3
        Me.radTime.Text = "Time"
        Me.radTime.UseVisualStyleBackColor = True
        '
        'frmPhysicsCalculator
        '
        Me.AcceptButton = Me.btnCalculate
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.Beige
        Me.CancelButton = Me.btnClear
        Me.ClientSize = New System.Drawing.Size(576, 382)
        Me.Controls.Add(Me.radTime)
        Me.Controls.Add(Me.btnExit)
        Me.Controls.Add(Me.btnClear)
        Me.Controls.Add(Me.btnCalculate)
        Me.Controls.Add(Me.lblResult2)
        Me.Controls.Add(Me.lblResult)
        Me.Controls.Add(Me.txtTime)
        Me.Controls.Add(Me.txtAcceleration)
        Me.Controls.Add(Me.txtDisplacement)
        Me.Controls.Add(Me.lblTime)
        Me.Controls.Add(Me.lblAcceleration)
        Me.Controls.Add(Me.lblDistance)
        Me.Controls.Add(Me.lblValues)
        Me.Controls.Add(Me.radDisplacement)
        Me.Controls.Add(Me.radAcceleration)
        Me.Controls.Add(Me.lblInstructions)
        Me.Controls.Add(Me.lblHeading)
        Me.Name = "frmPhysicsCalculator"
        Me.Text = "Physics Acceleration and Displacement Calculator"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents lblHeading As System.Windows.Forms.Label
    Friend WithEvents lblInstructions As System.Windows.Forms.Label
    Friend WithEvents radAcceleration As System.Windows.Forms.RadioButton
    Friend WithEvents radDisplacement As System.Windows.Forms.RadioButton
    Friend WithEvents lblValues As System.Windows.Forms.Label
    Friend WithEvents lblDistance As System.Windows.Forms.Label
    Friend WithEvents lblAcceleration As System.Windows.Forms.Label
    Friend WithEvents lblTime As System.Windows.Forms.Label
    Friend WithEvents txtDisplacement As System.Windows.Forms.TextBox
    Friend WithEvents txtAcceleration As System.Windows.Forms.TextBox
    Friend WithEvents txtTime As System.Windows.Forms.TextBox
    Friend WithEvents lblResult As System.Windows.Forms.Label
    Friend WithEvents lblResult2 As System.Windows.Forms.Label
    Friend WithEvents btnCalculate As System.Windows.Forms.Button
    Friend WithEvents btnClear As System.Windows.Forms.Button
    Friend WithEvents btnExit As System.Windows.Forms.Button
    Friend WithEvents radTime As System.Windows.Forms.RadioButton

End Class
