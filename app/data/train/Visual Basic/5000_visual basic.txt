<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmPostage
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
        Me.lblWeight = New System.Windows.Forms.Label()
        Me.txtWeight = New System.Windows.Forms.TextBox()
        Me.btnCAC = New System.Windows.Forms.Button()
        Me.lblCost = New System.Windows.Forms.Label()
        Me.txtCost = New System.Windows.Forms.TextBox()
        Me.SuspendLayout()
        '
        'lblWeight
        '
        Me.lblWeight.AutoSize = True
        Me.lblWeight.Location = New System.Drawing.Point(10, 15)
        Me.lblWeight.Name = "lblWeight"
        Me.lblWeight.Size = New System.Drawing.Size(126, 13)
        Me.lblWeight.TabIndex = 0
        Me.lblWeight.Text = "Weight of letter (ounces):"
        '
        'txtWeight
        '
        Me.txtWeight.Location = New System.Drawing.Point(142, 12)
        Me.txtWeight.Name = "txtWeight"
        Me.txtWeight.Size = New System.Drawing.Size(100, 20)
        Me.txtWeight.TabIndex = 1
        '
        'btnCAC
        '
        Me.btnCAC.Location = New System.Drawing.Point(13, 38)
        Me.btnCAC.Name = "btnCAC"
        Me.btnCAC.Size = New System.Drawing.Size(229, 35)
        Me.btnCAC.TabIndex = 2
        Me.btnCAC.Text = "Compute Airmail Cost"
        Me.btnCAC.UseVisualStyleBackColor = True
        '
        'lblCost
        '
        Me.lblCost.AutoSize = True
        Me.lblCost.Location = New System.Drawing.Point(52, 82)
        Me.lblCost.Name = "lblCost"
        Me.lblCost.Size = New System.Drawing.Size(31, 13)
        Me.lblCost.TabIndex = 3
        Me.lblCost.Text = "Cost:"
        '
        'txtCost
        '
        Me.txtCost.Location = New System.Drawing.Point(89, 79)
        Me.txtCost.Name = "txtCost"
        Me.txtCost.ReadOnly = True
        Me.txtCost.Size = New System.Drawing.Size(100, 20)
        Me.txtCost.TabIndex = 4
        '
        'frmPostage
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(264, 122)
        Me.Controls.Add(Me.txtCost)
        Me.Controls.Add(Me.lblCost)
        Me.Controls.Add(Me.btnCAC)
        Me.Controls.Add(Me.txtWeight)
        Me.Controls.Add(Me.lblWeight)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.MaximizeBox = False
        Me.Name = "frmPostage"
        Me.Text = "Postage"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents lblWeight As System.Windows.Forms.Label
    Friend WithEvents txtWeight As System.Windows.Forms.TextBox
    Friend WithEvents btnCAC As System.Windows.Forms.Button
    Friend WithEvents lblCost As System.Windows.Forms.Label
    Friend WithEvents txtCost As System.Windows.Forms.TextBox

End Class
