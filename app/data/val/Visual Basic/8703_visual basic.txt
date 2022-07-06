<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class scrPassBusqueda
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(scrPassBusqueda))
        Me.btnAcceso = New System.Windows.Forms.Button()
        Me.txtPass = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.txtUser = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'btnAcceso
        '
        Me.btnAcceso.Location = New System.Drawing.Point(144, 222)
        Me.btnAcceso.Name = "btnAcceso"
        Me.btnAcceso.Size = New System.Drawing.Size(175, 52)
        Me.btnAcceso.TabIndex = 14
        Me.btnAcceso.Text = "&Acceder"
        Me.btnAcceso.UseVisualStyleBackColor = True
        '
        'txtPass
        '
        Me.txtPass.Location = New System.Drawing.Point(233, 124)
        Me.txtPass.Name = "txtPass"
        Me.txtPass.PasswordChar = Global.Microsoft.VisualBasic.ChrW(42)
        Me.txtPass.Size = New System.Drawing.Size(100, 30)
        Me.txtPass.TabIndex = 13
        Me.txtPass.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(116, 127)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(104, 25)
        Me.Label2.TabIndex = 12
        Me.Label2.Text = "Password:"
        '
        'txtUser
        '
        Me.txtUser.Location = New System.Drawing.Point(233, 53)
        Me.txtUser.MaxLength = 5
        Me.txtUser.Name = "txtUser"
        Me.txtUser.Size = New System.Drawing.Size(100, 30)
        Me.txtUser.TabIndex = 11
        Me.txtUser.Text = "admin"
        Me.txtUser.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(116, 56)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(85, 25)
        Me.Label1.TabIndex = 10
        Me.Label1.Text = "Usuario:"
        '
        'scrPassBusqueda
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(12.0!, 25.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(448, 327)
        Me.Controls.Add(Me.btnAcceso)
        Me.Controls.Add(Me.txtPass)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.txtUser)
        Me.Controls.Add(Me.Label1)
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Margin = New System.Windows.Forms.Padding(4, 5, 4, 5)
        Me.Name = "scrPassBusqueda"
        Me.Text = "Rékor 32bits    Acceso de usuarios."
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents btnAcceso As System.Windows.Forms.Button
    Friend WithEvents txtPass As System.Windows.Forms.TextBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents txtUser As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
End Class
