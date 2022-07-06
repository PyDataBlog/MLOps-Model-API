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
        Me.btnShowAnwser = New System.Windows.Forms.Button()
        Me.btnExit = New System.Windows.Forms.Button()
        Me.lblAnwser = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'btnShowAnwser
        '
        Me.btnShowAnwser.Location = New System.Drawing.Point(51, 135)
        Me.btnShowAnwser.Name = "btnShowAnwser"
        Me.btnShowAnwser.Size = New System.Drawing.Size(74, 36)
        Me.btnShowAnwser.TabIndex = 0
        Me.btnShowAnwser.Text = "Show Anwser"
        Me.btnShowAnwser.UseVisualStyleBackColor = True
        '
        'btnExit
        '
        Me.btnExit.Location = New System.Drawing.Point(174, 135)
        Me.btnExit.Name = "btnExit"
        Me.btnExit.Size = New System.Drawing.Size(74, 36)
        Me.btnExit.TabIndex = 1
        Me.btnExit.Text = "Exit"
        Me.btnExit.UseVisualStyleBackColor = True
        '
        'lblAnwser
        '
        Me.lblAnwser.AutoSize = True
        Me.lblAnwser.Font = New System.Drawing.Font("Comic Sans MS", 21.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblAnwser.Location = New System.Drawing.Point(58, 41)
        Me.lblAnwser.Name = "lblAnwser"
        Me.lblAnwser.Size = New System.Drawing.Size(193, 40)
        Me.lblAnwser.TabIndex = 2
        Me.lblAnwser.Text = "18 + 64 = ?"
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(310, 218)
        Me.Controls.Add(Me.lblAnwser)
        Me.Controls.Add(Me.btnExit)
        Me.Controls.Add(Me.btnShowAnwser)
        Me.Name = "Form1"
        Me.Text = "Form1"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents btnShowAnwser As System.Windows.Forms.Button
    Friend WithEvents btnExit As System.Windows.Forms.Button
    Friend WithEvents lblAnwser As System.Windows.Forms.Label

End Class
