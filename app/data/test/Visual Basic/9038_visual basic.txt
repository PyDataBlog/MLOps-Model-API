<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class divide
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
        Me.btnBack = New System.Windows.Forms.Button()
        Me.txtResult = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.btnCMD = New System.Windows.Forms.Button()
        Me.lblAdd = New System.Windows.Forms.Label()
        Me.txtNum2 = New System.Windows.Forms.TextBox()
        Me.txtNum1 = New System.Windows.Forms.TextBox()
        Me.lblNum2 = New System.Windows.Forms.Label()
        Me.lblNum1 = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'btnBack
        '
        Me.btnBack.Font = New System.Drawing.Font("Microsoft Sans Serif", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnBack.Location = New System.Drawing.Point(84, 274)
        Me.btnBack.Name = "btnBack"
        Me.btnBack.Size = New System.Drawing.Size(197, 44)
        Me.btnBack.TabIndex = 17
        Me.btnBack.Text = "Go Back to Main"
        Me.btnBack.UseVisualStyleBackColor = True
        '
        'txtResult
        '
        Me.txtResult.Location = New System.Drawing.Point(181, 226)
        Me.txtResult.Name = "txtResult"
        Me.txtResult.Size = New System.Drawing.Size(100, 20)
        Me.txtResult.TabIndex = 16
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(80, 226)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(61, 20)
        Me.Label1.TabIndex = 15
        Me.Label1.Text = "Result"
        '
        'btnCMD
        '
        Me.btnCMD.Font = New System.Drawing.Font("Microsoft Sans Serif", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnCMD.Location = New System.Drawing.Point(150, 166)
        Me.btnCMD.Name = "btnCMD"
        Me.btnCMD.Size = New System.Drawing.Size(131, 44)
        Me.btnCMD.TabIndex = 14
        Me.btnCMD.Text = "Compute"
        Me.btnCMD.UseVisualStyleBackColor = True
        '
        'lblAdd
        '
        Me.lblAdd.AutoSize = True
        Me.lblAdd.Font = New System.Drawing.Font("Matura MT Script Capitals", 15.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblAdd.Location = New System.Drawing.Point(127, 33)
        Me.lblAdd.Name = "lblAdd"
        Me.lblAdd.Size = New System.Drawing.Size(109, 28)
        Me.lblAdd.TabIndex = 13
        Me.lblAdd.Text = "Division"
        '
        'txtNum2
        '
        Me.txtNum2.Location = New System.Drawing.Point(181, 122)
        Me.txtNum2.Name = "txtNum2"
        Me.txtNum2.Size = New System.Drawing.Size(100, 20)
        Me.txtNum2.TabIndex = 12
        '
        'txtNum1
        '
        Me.txtNum1.Location = New System.Drawing.Point(181, 82)
        Me.txtNum1.Name = "txtNum1"
        Me.txtNum1.Size = New System.Drawing.Size(100, 20)
        Me.txtNum1.TabIndex = 11
        '
        'lblNum2
        '
        Me.lblNum2.AutoSize = True
        Me.lblNum2.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblNum2.Location = New System.Drawing.Point(80, 122)
        Me.lblNum2.Name = "lblNum2"
        Me.lblNum2.Size = New System.Drawing.Size(86, 20)
        Me.lblNum2.TabIndex = 10
        Me.lblNum2.Text = "Number 2"
        '
        'lblNum1
        '
        Me.lblNum1.AutoSize = True
        Me.lblNum1.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblNum1.Location = New System.Drawing.Point(80, 82)
        Me.lblNum1.Name = "lblNum1"
        Me.lblNum1.Size = New System.Drawing.Size(86, 20)
        Me.lblNum1.TabIndex = 9
        Me.lblNum1.Text = "Number 1"
        '
        'divide
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(360, 351)
        Me.Controls.Add(Me.btnBack)
        Me.Controls.Add(Me.txtResult)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.btnCMD)
        Me.Controls.Add(Me.lblAdd)
        Me.Controls.Add(Me.txtNum2)
        Me.Controls.Add(Me.txtNum1)
        Me.Controls.Add(Me.lblNum2)
        Me.Controls.Add(Me.lblNum1)
        Me.Name = "divide"
        Me.Text = "divide"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents btnBack As System.Windows.Forms.Button
    Friend WithEvents txtResult As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents btnCMD As System.Windows.Forms.Button
    Friend WithEvents lblAdd As System.Windows.Forms.Label
    Friend WithEvents txtNum2 As System.Windows.Forms.TextBox
    Friend WithEvents txtNum1 As System.Windows.Forms.TextBox
    Friend WithEvents lblNum2 As System.Windows.Forms.Label
    Friend WithEvents lblNum1 As System.Windows.Forms.Label
End Class
