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
        Me.Label1 = New System.Windows.Forms.Label
        Me.Label2 = New System.Windows.Forms.Label
        Me.Label3 = New System.Windows.Forms.Label
        Me.Label4 = New System.Windows.Forms.Label
        Me.Label5 = New System.Windows.Forms.Label
        Me.Label6 = New System.Windows.Forms.Label
        Me.Label7 = New System.Windows.Forms.Label
        Me.Label8 = New System.Windows.Forms.Label
        Me.Label9 = New System.Windows.Forms.Label
        Me.Label10 = New System.Windows.Forms.Label
        Me.Label11 = New System.Windows.Forms.Label
        Me.btnCalculate = New System.Windows.Forms.Button
        Me.btnExit = New System.Windows.Forms.Button
        Me.TextBox1 = New System.Windows.Forms.TextBox
        Me.txtEP1 = New System.Windows.Forms.TextBox
        Me.txtEP2 = New System.Windows.Forms.TextBox
        Me.txtEP3 = New System.Windows.Forms.TextBox
        Me.txtSupplies = New System.Windows.Forms.TextBox
        Me.txtRent = New System.Windows.Forms.TextBox
        Me.txtPayroll = New System.Windows.Forms.TextBox
        Me.txtRevenue = New System.Windows.Forms.TextBox
        Me.txtUtilities = New System.Windows.Forms.TextBox
        Me.txtProfit = New System.Windows.Forms.TextBox
        Me.txtLoss = New System.Windows.Forms.TextBox
        Me.txtEP4 = New System.Windows.Forms.TextBox
        Me.txtOther = New System.Windows.Forms.TextBox
        Me.txtTotalExpense = New System.Windows.Forms.TextBox
        Me.txtEP5 = New System.Windows.Forms.TextBox
        Me.btnClear = New System.Windows.Forms.Button
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(52, 36)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(51, 13)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Revenue"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(52, 91)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(53, 13)
        Me.Label2.TabIndex = 1
        Me.Label2.Text = "Expenses"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(52, 138)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(30, 13)
        Me.Label3.TabIndex = 2
        Me.Label3.Text = "Rent"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(52, 161)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(38, 13)
        Me.Label4.TabIndex = 3
        Me.Label4.Text = "Payroll"
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Location = New System.Drawing.Point(52, 185)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(40, 13)
        Me.Label5.TabIndex = 4
        Me.Label5.Text = "Utilities"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(52, 211)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(47, 13)
        Me.Label6.TabIndex = 5
        Me.Label6.Text = "Supplies"
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Location = New System.Drawing.Point(52, 233)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(33, 13)
        Me.Label7.TabIndex = 6
        Me.Label7.Text = "Other"
        '
        'Label8
        '
        Me.Label8.AutoSize = True
        Me.Label8.Location = New System.Drawing.Point(52, 254)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(80, 13)
        Me.Label8.TabIndex = 7
        Me.Label8.Text = "Total Expenses"
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.Location = New System.Drawing.Point(52, 298)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(31, 13)
        Me.Label9.TabIndex = 8
        Me.Label9.Text = "Profit"
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.Location = New System.Drawing.Point(52, 321)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(29, 13)
        Me.Label10.TabIndex = 9
        Me.Label10.Text = "Loss"
        '
        'Label11
        '
        Me.Label11.AutoSize = True
        Me.Label11.Location = New System.Drawing.Point(283, 91)
        Me.Label11.Name = "Label11"
        Me.Label11.Size = New System.Drawing.Size(111, 13)
        Me.Label11.TabIndex = 10
        Me.Label11.Text = "Expense Percentages"
        '
        'btnCalculate
        '
        Me.btnCalculate.Location = New System.Drawing.Point(286, 270)
        Me.btnCalculate.Name = "btnCalculate"
        Me.btnCalculate.Size = New System.Drawing.Size(96, 23)
        Me.btnCalculate.TabIndex = 11
        Me.btnCalculate.Text = "&Calculate"
        Me.btnCalculate.UseVisualStyleBackColor = True
        '
        'btnExit
        '
        Me.btnExit.Location = New System.Drawing.Point(286, 311)
        Me.btnExit.Name = "btnExit"
        Me.btnExit.Size = New System.Drawing.Size(96, 23)
        Me.btnExit.TabIndex = 12
        Me.btnExit.Text = "E&xit"
        Me.btnExit.UseVisualStyleBackColor = True
        '
        'TextBox1
        '
        Me.TextBox1.Location = New System.Drawing.Point(122, 225)
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.Size = New System.Drawing.Size(96, 20)
        Me.TextBox1.TabIndex = 13
        '
        'txtEP1
        '
        Me.txtEP1.Location = New System.Drawing.Point(286, 131)
        Me.txtEP1.Name = "txtEP1"
        Me.txtEP1.ReadOnly = True
        Me.txtEP1.Size = New System.Drawing.Size(96, 20)
        Me.txtEP1.TabIndex = 14
        '
        'txtEP2
        '
        Me.txtEP2.Location = New System.Drawing.Point(286, 161)
        Me.txtEP2.Name = "txtEP2"
        Me.txtEP2.ReadOnly = True
        Me.txtEP2.Size = New System.Drawing.Size(96, 20)
        Me.txtEP2.TabIndex = 15
        '
        'txtEP3
        '
        Me.txtEP3.Location = New System.Drawing.Point(286, 187)
        Me.txtEP3.Name = "txtEP3"
        Me.txtEP3.ReadOnly = True
        Me.txtEP3.Size = New System.Drawing.Size(96, 20)
        Me.txtEP3.TabIndex = 16
        '
        'txtSupplies
        '
        Me.txtSupplies.Location = New System.Drawing.Point(122, 208)
        Me.txtSupplies.Name = "txtSupplies"
        Me.txtSupplies.Size = New System.Drawing.Size(96, 20)
        Me.txtSupplies.TabIndex = 5
        '
        'txtRent
        '
        Me.txtRent.Location = New System.Drawing.Point(122, 135)
        Me.txtRent.Name = "txtRent"
        Me.txtRent.Size = New System.Drawing.Size(96, 20)
        Me.txtRent.TabIndex = 2
        '
        'txtPayroll
        '
        Me.txtPayroll.Location = New System.Drawing.Point(122, 158)
        Me.txtPayroll.Name = "txtPayroll"
        Me.txtPayroll.Size = New System.Drawing.Size(96, 20)
        Me.txtPayroll.TabIndex = 3
        '
        'txtRevenue
        '
        Me.txtRevenue.Location = New System.Drawing.Point(122, 33)
        Me.txtRevenue.Name = "txtRevenue"
        Me.txtRevenue.Size = New System.Drawing.Size(96, 20)
        Me.txtRevenue.TabIndex = 1
        '
        'txtUtilities
        '
        Me.txtUtilities.Location = New System.Drawing.Point(122, 182)
        Me.txtUtilities.Name = "txtUtilities"
        Me.txtUtilities.Size = New System.Drawing.Size(96, 20)
        Me.txtUtilities.TabIndex = 4
        '
        'txtProfit
        '
        Me.txtProfit.Location = New System.Drawing.Point(122, 295)
        Me.txtProfit.Name = "txtProfit"
        Me.txtProfit.ReadOnly = True
        Me.txtProfit.Size = New System.Drawing.Size(96, 20)
        Me.txtProfit.TabIndex = 7
        '
        'txtLoss
        '
        Me.txtLoss.Location = New System.Drawing.Point(122, 321)
        Me.txtLoss.Name = "txtLoss"
        Me.txtLoss.ReadOnly = True
        Me.txtLoss.Size = New System.Drawing.Size(96, 20)
        Me.txtLoss.TabIndex = 23
        '
        'txtEP4
        '
        Me.txtEP4.Location = New System.Drawing.Point(286, 207)
        Me.txtEP4.Name = "txtEP4"
        Me.txtEP4.ReadOnly = True
        Me.txtEP4.Size = New System.Drawing.Size(96, 20)
        Me.txtEP4.TabIndex = 24
        '
        'txtOther
        '
        Me.txtOther.Location = New System.Drawing.Point(122, 230)
        Me.txtOther.Name = "txtOther"
        Me.txtOther.Size = New System.Drawing.Size(96, 20)
        Me.txtOther.TabIndex = 6
        '
        'txtTotalExpense
        '
        Me.txtTotalExpense.Location = New System.Drawing.Point(122, 251)
        Me.txtTotalExpense.Name = "txtTotalExpense"
        Me.txtTotalExpense.ReadOnly = True
        Me.txtTotalExpense.Size = New System.Drawing.Size(96, 20)
        Me.txtTotalExpense.TabIndex = 11
        '
        'txtEP5
        '
        Me.txtEP5.Location = New System.Drawing.Point(286, 233)
        Me.txtEP5.Name = "txtEP5"
        Me.txtEP5.ReadOnly = True
        Me.txtEP5.Size = New System.Drawing.Size(96, 20)
        Me.txtEP5.TabIndex = 27
        '
        'btnClear
        '
        Me.btnClear.Location = New System.Drawing.Point(286, 26)
        Me.btnClear.Name = "btnClear"
        Me.btnClear.Size = New System.Drawing.Size(96, 23)
        Me.btnClear.TabIndex = 28
        Me.btnClear.Text = "C&lear"
        Me.btnClear.UseVisualStyleBackColor = True
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(428, 393)
        Me.Controls.Add(Me.btnClear)
        Me.Controls.Add(Me.txtEP5)
        Me.Controls.Add(Me.txtTotalExpense)
        Me.Controls.Add(Me.txtOther)
        Me.Controls.Add(Me.txtEP4)
        Me.Controls.Add(Me.txtLoss)
        Me.Controls.Add(Me.txtProfit)
        Me.Controls.Add(Me.txtUtilities)
        Me.Controls.Add(Me.txtRevenue)
        Me.Controls.Add(Me.txtPayroll)
        Me.Controls.Add(Me.txtRent)
        Me.Controls.Add(Me.txtSupplies)
        Me.Controls.Add(Me.txtEP3)
        Me.Controls.Add(Me.txtEP2)
        Me.Controls.Add(Me.txtEP1)
        Me.Controls.Add(Me.TextBox1)
        Me.Controls.Add(Me.btnExit)
        Me.Controls.Add(Me.btnCalculate)
        Me.Controls.Add(Me.Label11)
        Me.Controls.Add(Me.Label10)
        Me.Controls.Add(Me.Label9)
        Me.Controls.Add(Me.Label8)
        Me.Controls.Add(Me.Label7)
        Me.Controls.Add(Me.Label6)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Name = "Form1"
        Me.Text = "Profit Loss Calculater"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents Label8 As System.Windows.Forms.Label
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents Label10 As System.Windows.Forms.Label
    Friend WithEvents Label11 As System.Windows.Forms.Label
    Friend WithEvents btnCalculate As System.Windows.Forms.Button
    Friend WithEvents btnExit As System.Windows.Forms.Button
    Friend WithEvents TextBox1 As System.Windows.Forms.TextBox
    Friend WithEvents txtEP1 As System.Windows.Forms.TextBox
    Friend WithEvents txtEP2 As System.Windows.Forms.TextBox
    Friend WithEvents txtEP3 As System.Windows.Forms.TextBox
    Friend WithEvents txtSupplies As System.Windows.Forms.TextBox
    Friend WithEvents txtRent As System.Windows.Forms.TextBox
    Friend WithEvents txtPayroll As System.Windows.Forms.TextBox
    Friend WithEvents txtRevenue As System.Windows.Forms.TextBox
    Friend WithEvents txtUtilities As System.Windows.Forms.TextBox
    Friend WithEvents txtProfit As System.Windows.Forms.TextBox
    Friend WithEvents txtLoss As System.Windows.Forms.TextBox
    Friend WithEvents txtEP4 As System.Windows.Forms.TextBox
    Friend WithEvents txtOther As System.Windows.Forms.TextBox
    Friend WithEvents txtTotalExpense As System.Windows.Forms.TextBox
    Friend WithEvents txtEP5 As System.Windows.Forms.TextBox
    Friend WithEvents btnClear As System.Windows.Forms.Button

End Class
