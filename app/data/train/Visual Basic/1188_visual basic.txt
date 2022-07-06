<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class NuevaEmpresa
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
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.txtCif = New System.Windows.Forms.TextBox()
        Me.txtNombre = New System.Windows.Forms.TextBox()
        Me.txtLocalidad = New System.Windows.Forms.TextBox()
        Me.txtProvincia = New System.Windows.Forms.TextBox()
        Me.txtPais = New System.Windows.Forms.TextBox()
        Me.txtCodPostal = New System.Windows.Forms.TextBox()
        Me.txtSector = New System.Windows.Forms.TextBox()
        Me.btnCrearEmpresa = New System.Windows.Forms.Button()
        Me.btnBorrarCajas = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(61, 48)
        Me.Label1.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(38, 20)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "CIF"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(61, 123)
        Me.Label2.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(71, 20)
        Me.Label2.TabIndex = 1
        Me.Label2.Text = "Nombre"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label3.Location = New System.Drawing.Point(61, 190)
        Me.Label3.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(86, 20)
        Me.Label3.TabIndex = 2
        Me.Label3.Text = "Localidad"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label4.Location = New System.Drawing.Point(61, 258)
        Me.Label4.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(81, 20)
        Me.Label4.TabIndex = 3
        Me.Label4.Text = "Provincia"
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label5.Location = New System.Drawing.Point(335, 48)
        Me.Label5.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(43, 20)
        Me.Label5.TabIndex = 4
        Me.Label5.Text = "País"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label6.Location = New System.Drawing.Point(335, 118)
        Me.Label6.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(120, 20)
        Me.Label6.TabIndex = 5
        Me.Label6.Text = "Código Postal"
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label7.Location = New System.Drawing.Point(335, 190)
        Me.Label7.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(62, 20)
        Me.Label7.TabIndex = 6
        Me.Label7.Text = "Sector"
        '
        'txtCif
        '
        Me.txtCif.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtCif.Location = New System.Drawing.Point(169, 44)
        Me.txtCif.Margin = New System.Windows.Forms.Padding(2)
        Me.txtCif.Name = "txtCif"
        Me.txtCif.Size = New System.Drawing.Size(120, 24)
        Me.txtCif.TabIndex = 7
        '
        'txtNombre
        '
        Me.txtNombre.Location = New System.Drawing.Point(169, 118)
        Me.txtNombre.Margin = New System.Windows.Forms.Padding(2)
        Me.txtNombre.Name = "txtNombre"
        Me.txtNombre.Size = New System.Drawing.Size(120, 24)
        Me.txtNombre.TabIndex = 8
        '
        'txtLocalidad
        '
        Me.txtLocalidad.Location = New System.Drawing.Point(169, 188)
        Me.txtLocalidad.Margin = New System.Windows.Forms.Padding(2)
        Me.txtLocalidad.Name = "txtLocalidad"
        Me.txtLocalidad.Size = New System.Drawing.Size(120, 24)
        Me.txtLocalidad.TabIndex = 9
        '
        'txtProvincia
        '
        Me.txtProvincia.Location = New System.Drawing.Point(169, 254)
        Me.txtProvincia.Margin = New System.Windows.Forms.Padding(2)
        Me.txtProvincia.Name = "txtProvincia"
        Me.txtProvincia.Size = New System.Drawing.Size(120, 24)
        Me.txtProvincia.TabIndex = 10
        '
        'txtPais
        '
        Me.txtPais.Location = New System.Drawing.Point(481, 44)
        Me.txtPais.Margin = New System.Windows.Forms.Padding(2)
        Me.txtPais.Name = "txtPais"
        Me.txtPais.Size = New System.Drawing.Size(120, 24)
        Me.txtPais.TabIndex = 11
        '
        'txtCodPostal
        '
        Me.txtCodPostal.Location = New System.Drawing.Point(481, 118)
        Me.txtCodPostal.Margin = New System.Windows.Forms.Padding(2)
        Me.txtCodPostal.Name = "txtCodPostal"
        Me.txtCodPostal.Size = New System.Drawing.Size(120, 24)
        Me.txtCodPostal.TabIndex = 12
        '
        'txtSector
        '
        Me.txtSector.Location = New System.Drawing.Point(481, 186)
        Me.txtSector.Margin = New System.Windows.Forms.Padding(2)
        Me.txtSector.Name = "txtSector"
        Me.txtSector.Size = New System.Drawing.Size(120, 24)
        Me.txtSector.TabIndex = 13
        '
        'btnCrearEmpresa
        '
        Me.btnCrearEmpresa.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnCrearEmpresa.Location = New System.Drawing.Point(481, 296)
        Me.btnCrearEmpresa.Name = "btnCrearEmpresa"
        Me.btnCrearEmpresa.Size = New System.Drawing.Size(120, 23)
        Me.btnCrearEmpresa.TabIndex = 14
        Me.btnCrearEmpresa.Text = "Crear Empresa"
        Me.btnCrearEmpresa.UseVisualStyleBackColor = True
        '
        'btnBorrarCajas
        '
        Me.btnBorrarCajas.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btnBorrarCajas.Location = New System.Drawing.Point(481, 254)
        Me.btnBorrarCajas.Name = "btnBorrarCajas"
        Me.btnBorrarCajas.Size = New System.Drawing.Size(120, 23)
        Me.btnBorrarCajas.TabIndex = 15
        Me.btnBorrarCajas.Text = "Borrar Cajas"
        Me.btnBorrarCajas.UseVisualStyleBackColor = True
        '
        'NuevaEmpresa
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(9.0!, 18.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.Honeydew
        Me.ClientSize = New System.Drawing.Size(733, 350)
        Me.Controls.Add(Me.btnBorrarCajas)
        Me.Controls.Add(Me.btnCrearEmpresa)
        Me.Controls.Add(Me.txtSector)
        Me.Controls.Add(Me.txtCodPostal)
        Me.Controls.Add(Me.txtPais)
        Me.Controls.Add(Me.txtProvincia)
        Me.Controls.Add(Me.txtLocalidad)
        Me.Controls.Add(Me.txtNombre)
        Me.Controls.Add(Me.txtCif)
        Me.Controls.Add(Me.Label7)
        Me.Controls.Add(Me.Label6)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.Name = "NuevaEmpresa"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "NuevaEmpresa"
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
    Friend WithEvents txtCif As System.Windows.Forms.TextBox
    Friend WithEvents txtNombre As System.Windows.Forms.TextBox
    Friend WithEvents txtLocalidad As System.Windows.Forms.TextBox
    Friend WithEvents txtProvincia As System.Windows.Forms.TextBox
    Friend WithEvents txtPais As System.Windows.Forms.TextBox
    Friend WithEvents txtCodPostal As System.Windows.Forms.TextBox
    Friend WithEvents txtSector As System.Windows.Forms.TextBox
    Friend WithEvents btnCrearEmpresa As System.Windows.Forms.Button
    Friend WithEvents btnBorrarCajas As System.Windows.Forms.Button
End Class
