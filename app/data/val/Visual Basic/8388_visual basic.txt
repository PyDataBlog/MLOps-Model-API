<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form15
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form15))
        Me.btnUpdate = New System.Windows.Forms.Button()
        Me.lblCurrent = New System.Windows.Forms.Label()
        Me.lblNew = New System.Windows.Forms.Label()
        Me.txtCurrentPassword = New System.Windows.Forms.TextBox()
        Me.txtNewPassword = New System.Windows.Forms.TextBox()
        Me.SuspendLayout()
        '
        'btnUpdate
        '
        Me.btnUpdate.Location = New System.Drawing.Point(12, 71)
        Me.btnUpdate.Name = "btnUpdate"
        Me.btnUpdate.Size = New System.Drawing.Size(264, 26)
        Me.btnUpdate.TabIndex = 0
        Me.btnUpdate.Text = "Update Password"
        Me.btnUpdate.UseVisualStyleBackColor = True
        '
        'lblCurrent
        '
        Me.lblCurrent.AutoSize = True
        Me.lblCurrent.Location = New System.Drawing.Point(9, 9)
        Me.lblCurrent.Name = "lblCurrent"
        Me.lblCurrent.Size = New System.Drawing.Size(90, 13)
        Me.lblCurrent.TabIndex = 1
        Me.lblCurrent.Text = "Current Password"
        '
        'lblNew
        '
        Me.lblNew.AutoSize = True
        Me.lblNew.Location = New System.Drawing.Point(9, 46)
        Me.lblNew.Name = "lblNew"
        Me.lblNew.Size = New System.Drawing.Size(78, 13)
        Me.lblNew.TabIndex = 2
        Me.lblNew.Text = "New Password"
        '
        'txtCurrentPassword
        '
        Me.txtCurrentPassword.Location = New System.Drawing.Point(105, 6)
        Me.txtCurrentPassword.Name = "txtCurrentPassword"
        Me.txtCurrentPassword.Size = New System.Drawing.Size(171, 20)
        Me.txtCurrentPassword.TabIndex = 3
        '
        'txtNewPassword
        '
        Me.txtNewPassword.Location = New System.Drawing.Point(105, 43)
        Me.txtNewPassword.Name = "txtNewPassword"
        Me.txtNewPassword.Size = New System.Drawing.Size(171, 20)
        Me.txtNewPassword.TabIndex = 4
        '
        'Form15
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(288, 109)
        Me.Controls.Add(Me.txtNewPassword)
        Me.Controls.Add(Me.txtCurrentPassword)
        Me.Controls.Add(Me.lblNew)
        Me.Controls.Add(Me.lblCurrent)
        Me.Controls.Add(Me.btnUpdate)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MaximizeBox = False
        Me.MaximumSize = New System.Drawing.Size(304, 148)
        Me.MinimumSize = New System.Drawing.Size(304, 148)
        Me.Name = "Form15"
        Me.Text = "Update Password"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents btnUpdate As Button
    Friend WithEvents lblCurrent As Label
    Friend WithEvents lblNew As Label
    Friend WithEvents txtCurrentPassword As TextBox
    Friend WithEvents txtNewPassword As TextBox
End Class
