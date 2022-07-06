<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class mysqlSettings
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(mysqlSettings))
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.txtPass = New System.Windows.Forms.TextBox()
        Me.txtUser = New System.Windows.Forms.TextBox()
        Me.txtIp = New System.Windows.Forms.TextBox()
        Me.btnSalvar = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(81, 149)
        Me.Label3.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(104, 25)
        Me.Label3.TabIndex = 7
        Me.Label3.Text = "Password:"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(81, 96)
        Me.Label2.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(85, 25)
        Me.Label2.TabIndex = 6
        Me.Label2.Text = "Usuario:"
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(81, 42)
        Me.Label1.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(122, 25)
        Me.Label1.TabIndex = 5
        Me.Label1.Text = "Dirección IP:"
        '
        'txtPass
        '
        Me.txtPass.Location = New System.Drawing.Point(210, 149)
        Me.txtPass.MaxLength = 12
        Me.txtPass.Name = "txtPass"
        Me.txtPass.PasswordChar = Global.Microsoft.VisualBasic.ChrW(42)
        Me.txtPass.Size = New System.Drawing.Size(212, 30)
        Me.txtPass.TabIndex = 10
        Me.txtPass.Text = "ZMalqp10"
        Me.txtPass.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'txtUser
        '
        Me.txtUser.Location = New System.Drawing.Point(210, 96)
        Me.txtUser.MaxLength = 12
        Me.txtUser.Name = "txtUser"
        Me.txtUser.Size = New System.Drawing.Size(212, 30)
        Me.txtUser.TabIndex = 9
        Me.txtUser.Text = "root"
        Me.txtUser.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'txtIp
        '
        Me.txtIp.Location = New System.Drawing.Point(210, 42)
        Me.txtIp.MaxLength = 20
        Me.txtIp.Name = "txtIp"
        Me.txtIp.Size = New System.Drawing.Size(212, 30)
        Me.txtIp.TabIndex = 8
        Me.txtIp.Text = "sh0me.ddns.net"
        Me.txtIp.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'btnSalvar
        '
        Me.btnSalvar.Location = New System.Drawing.Point(163, 225)
        Me.btnSalvar.Name = "btnSalvar"
        Me.btnSalvar.Size = New System.Drawing.Size(179, 47)
        Me.btnSalvar.TabIndex = 11
        Me.btnSalvar.Text = "&Guardar"
        Me.btnSalvar.UseVisualStyleBackColor = True
        '
        'mysqlSettings
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(12.0!, 25.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(499, 311)
        Me.Controls.Add(Me.btnSalvar)
        Me.Controls.Add(Me.txtPass)
        Me.Controls.Add(Me.txtUser)
        Me.Controls.Add(Me.txtIp)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Margin = New System.Windows.Forms.Padding(4, 5, 4, 5)
        Me.Name = "mysqlSettings"
        Me.Text = "Parámetros de conexión al Servidor Rekor 32 bits."
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents txtPass As System.Windows.Forms.TextBox
    Friend WithEvents txtUser As System.Windows.Forms.TextBox
    Friend WithEvents txtIp As System.Windows.Forms.TextBox
    Friend WithEvents btnSalvar As System.Windows.Forms.Button
End Class
