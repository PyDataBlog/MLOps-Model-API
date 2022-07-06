<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class main_login
    Inherits System.Windows.Forms.Form

    'Form reemplaza a Dispose para limpiar la lista de componentes.
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

    'Requerido por el Diseñador de Windows Forms
    Private components As System.ComponentModel.IContainer

    'NOTA: el Diseñador de Windows Forms necesita el siguiente procedimiento
    'Se puede modificar usando el Diseñador de Windows Forms.  
    'No lo modifique con el editor de código.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.login_pic_logo = New System.Windows.Forms.PictureBox()
        Me.login_lbl_user = New System.Windows.Forms.Label()
        Me.login_lbl_pass = New System.Windows.Forms.Label()
        Me.login_txt_user = New System.Windows.Forms.TextBox()
        Me.login_txt_pass = New System.Windows.Forms.TextBox()
        Me.login_pic_enter = New System.Windows.Forms.PictureBox()
        Me.login_pic_close = New System.Windows.Forms.PictureBox()
        Me.login_lbl_footer = New System.Windows.Forms.Label()
        CType(Me.login_pic_logo, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.login_pic_enter, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.login_pic_close, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'login_pic_logo
        '
        Me.login_pic_logo.Location = New System.Drawing.Point(12, 31)
        Me.login_pic_logo.Name = "login_pic_logo"
        Me.login_pic_logo.Size = New System.Drawing.Size(184, 173)
        Me.login_pic_logo.TabIndex = 0
        Me.login_pic_logo.TabStop = False
        '
        'login_lbl_user
        '
        Me.login_lbl_user.AutoSize = True
        Me.login_lbl_user.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.login_lbl_user.Location = New System.Drawing.Point(214, 44)
        Me.login_lbl_user.Name = "login_lbl_user"
        Me.login_lbl_user.Size = New System.Drawing.Size(84, 20)
        Me.login_lbl_user.TabIndex = 1
        Me.login_lbl_user.Text = "USUARIO"
        '
        'login_lbl_pass
        '
        Me.login_lbl_pass.AutoSize = True
        Me.login_lbl_pass.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.login_lbl_pass.Location = New System.Drawing.Point(214, 96)
        Me.login_lbl_pass.Name = "login_lbl_pass"
        Me.login_lbl_pass.Size = New System.Drawing.Size(119, 20)
        Me.login_lbl_pass.TabIndex = 2
        Me.login_lbl_pass.Text = "CONTRASEÑA"
        '
        'login_txt_user
        '
        Me.login_txt_user.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.login_txt_user.Location = New System.Drawing.Point(218, 67)
        Me.login_txt_user.Name = "login_txt_user"
        Me.login_txt_user.Size = New System.Drawing.Size(165, 26)
        Me.login_txt_user.TabIndex = 3
        '
        'login_txt_pass
        '
        Me.login_txt_pass.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.login_txt_pass.Location = New System.Drawing.Point(218, 119)
        Me.login_txt_pass.Name = "login_txt_pass"
        Me.login_txt_pass.Size = New System.Drawing.Size(165, 26)
        Me.login_txt_pass.TabIndex = 4
        '
        'login_pic_enter
        '
        Me.login_pic_enter.Location = New System.Drawing.Point(356, 180)
        Me.login_pic_enter.Name = "login_pic_enter"
        Me.login_pic_enter.Size = New System.Drawing.Size(154, 39)
        Me.login_pic_enter.TabIndex = 5
        Me.login_pic_enter.TabStop = False
        '
        'login_pic_close
        '
        Me.login_pic_close.Location = New System.Drawing.Point(487, 2)
        Me.login_pic_close.Name = "login_pic_close"
        Me.login_pic_close.Size = New System.Drawing.Size(32, 32)
        Me.login_pic_close.TabIndex = 6
        Me.login_pic_close.TabStop = False
        '
        'login_lbl_footer
        '
        Me.login_lbl_footer.AutoSize = True
        Me.login_lbl_footer.Location = New System.Drawing.Point(12, 213)
        Me.login_lbl_footer.Name = "login_lbl_footer"
        Me.login_lbl_footer.Size = New System.Drawing.Size(79, 13)
        Me.login_lbl_footer.TabIndex = 7
        Me.login_lbl_footer.Text = "APP VERSION"
        '
        'main_login
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(522, 231)
        Me.Controls.Add(Me.login_lbl_footer)
        Me.Controls.Add(Me.login_pic_close)
        Me.Controls.Add(Me.login_pic_enter)
        Me.Controls.Add(Me.login_txt_pass)
        Me.Controls.Add(Me.login_txt_user)
        Me.Controls.Add(Me.login_lbl_pass)
        Me.Controls.Add(Me.login_lbl_user)
        Me.Controls.Add(Me.login_pic_logo)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
        Me.Name = "main_login"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        CType(Me.login_pic_logo, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.login_pic_enter, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.login_pic_close, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents login_pic_logo As System.Windows.Forms.PictureBox
    Friend WithEvents login_lbl_user As System.Windows.Forms.Label
    Friend WithEvents login_lbl_pass As System.Windows.Forms.Label
    Friend WithEvents login_txt_user As System.Windows.Forms.TextBox
    Friend WithEvents login_txt_pass As System.Windows.Forms.TextBox
    Friend WithEvents login_pic_enter As System.Windows.Forms.PictureBox
    Friend WithEvents login_pic_close As System.Windows.Forms.PictureBox
    Friend WithEvents login_lbl_footer As System.Windows.Forms.Label

End Class
