<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Clientes
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Clientes))
        Me.TsClientes = New System.Windows.Forms.ToolStrip()
        Me.TsNuevoCliente = New System.Windows.Forms.ToolStripButton()
        Me.TsModificar = New System.Windows.Forms.ToolStripButton()
        Me.TsBorrar = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.TsPrimero = New System.Windows.Forms.ToolStripButton()
        Me.TsAnterior = New System.Windows.Forms.ToolStripButton()
        Me.TsSiguiente = New System.Windows.Forms.ToolStripButton()
        Me.TsUltimo = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripSeparator3 = New System.Windows.Forms.ToolStripSeparator()
        Me.TsSalir = New System.Windows.Forms.ToolStripButton()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.CmbDNICliente = New System.Windows.Forms.ComboBox()
        Me.DtgClientes = New System.Windows.Forms.DataGridView()
        Me.TxtTelefonoMovil = New System.Windows.Forms.TextBox()
        Me.TxtTelefonoFijo = New System.Windows.Forms.TextBox()
        Me.TxtApellidosCliente = New System.Windows.Forms.TextBox()
        Me.TxtDNICliente = New System.Windows.Forms.TextBox()
        Me.TxtNombreCiente = New System.Windows.Forms.TextBox()
        Me.TelefMovil = New System.Windows.Forms.Label()
        Me.TelefFijo = New System.Windows.Forms.Label()
        Me.ApellidosCliente = New System.Windows.Forms.Label()
        Me.DNICliente = New System.Windows.Forms.Label()
        Me.NombreCliente = New System.Windows.Forms.Label()
        Me.TxtEmailCliente = New System.Windows.Forms.TextBox()
        Me.TxtDirCliente = New System.Windows.Forms.TextBox()
        Me.EmailCliente = New System.Windows.Forms.Label()
        Me.DireccionCliente = New System.Windows.Forms.Label()
        Me.TxtIDCliente = New System.Windows.Forms.TextBox()
        Me.IDCliente = New System.Windows.Forms.Label()
        Me.TsClientes.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        CType(Me.DtgClientes, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'TsClientes
        '
        Me.TsClientes.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.TsNuevoCliente, Me.TsModificar, Me.TsBorrar, Me.ToolStripSeparator1, Me.TsPrimero, Me.TsAnterior, Me.TsSiguiente, Me.TsUltimo, Me.ToolStripSeparator2, Me.ToolStripSeparator3, Me.TsSalir})
        Me.TsClientes.Location = New System.Drawing.Point(0, 0)
        Me.TsClientes.Name = "TsClientes"
        Me.TsClientes.Size = New System.Drawing.Size(579, 25)
        Me.TsClientes.TabIndex = 43
        Me.TsClientes.Text = "ToolStrip1"
        '
        'TsNuevoCliente
        '
        Me.TsNuevoCliente.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsNuevoCliente.Image = CType(resources.GetObject("TsNuevoCliente.Image"), System.Drawing.Image)
        Me.TsNuevoCliente.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsNuevoCliente.Name = "TsNuevoCliente"
        Me.TsNuevoCliente.Size = New System.Drawing.Size(23, 22)
        Me.TsNuevoCliente.Text = "Nuevo Cliente"
        Me.TsNuevoCliente.ToolTipText = "Añadir un cliente nuevo"
        '
        'TsModificar
        '
        Me.TsModificar.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsModificar.Image = CType(resources.GetObject("TsModificar.Image"), System.Drawing.Image)
        Me.TsModificar.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsModificar.Name = "TsModificar"
        Me.TsModificar.Size = New System.Drawing.Size(23, 22)
        Me.TsModificar.Text = "Modificar Cliente"
        Me.TsModificar.ToolTipText = "Modificar el cliente seleccionado"
        '
        'TsBorrar
        '
        Me.TsBorrar.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsBorrar.Image = CType(resources.GetObject("TsBorrar.Image"), System.Drawing.Image)
        Me.TsBorrar.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsBorrar.Name = "TsBorrar"
        Me.TsBorrar.Size = New System.Drawing.Size(23, 22)
        Me.TsBorrar.Text = "Borrar Cliente"
        Me.TsBorrar.ToolTipText = "Borrar el cliente seleccionado"
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        Me.ToolStripSeparator1.Size = New System.Drawing.Size(6, 25)
        '
        'TsPrimero
        '
        Me.TsPrimero.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsPrimero.Image = CType(resources.GetObject("TsPrimero.Image"), System.Drawing.Image)
        Me.TsPrimero.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsPrimero.Name = "TsPrimero"
        Me.TsPrimero.Size = New System.Drawing.Size(23, 22)
        Me.TsPrimero.Text = "Primero"
        Me.TsPrimero.ToolTipText = "Ir al primer cliente"
        '
        'TsAnterior
        '
        Me.TsAnterior.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsAnterior.Image = CType(resources.GetObject("TsAnterior.Image"), System.Drawing.Image)
        Me.TsAnterior.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsAnterior.Name = "TsAnterior"
        Me.TsAnterior.Size = New System.Drawing.Size(23, 22)
        Me.TsAnterior.Text = "Anterior"
        Me.TsAnterior.ToolTipText = "Ir al cliente anterior"
        '
        'TsSiguiente
        '
        Me.TsSiguiente.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsSiguiente.Image = CType(resources.GetObject("TsSiguiente.Image"), System.Drawing.Image)
        Me.TsSiguiente.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsSiguiente.Name = "TsSiguiente"
        Me.TsSiguiente.Size = New System.Drawing.Size(23, 22)
        Me.TsSiguiente.Text = "Siguiente"
        Me.TsSiguiente.ToolTipText = "Ir al siguiente cliente"
        '
        'TsUltimo
        '
        Me.TsUltimo.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsUltimo.Image = CType(resources.GetObject("TsUltimo.Image"), System.Drawing.Image)
        Me.TsUltimo.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsUltimo.Name = "TsUltimo"
        Me.TsUltimo.Size = New System.Drawing.Size(23, 22)
        Me.TsUltimo.Text = "Ultimo"
        Me.TsUltimo.ToolTipText = "Ir al ultimo cliente"
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        Me.ToolStripSeparator2.Size = New System.Drawing.Size(6, 25)
        '
        'ToolStripSeparator3
        '
        Me.ToolStripSeparator3.Name = "ToolStripSeparator3"
        Me.ToolStripSeparator3.Size = New System.Drawing.Size(6, 25)
        '
        'TsSalir
        '
        Me.TsSalir.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsSalir.Image = CType(resources.GetObject("TsSalir.Image"), System.Drawing.Image)
        Me.TsSalir.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsSalir.Name = "TsSalir"
        Me.TsSalir.Size = New System.Drawing.Size(23, 22)
        Me.TsSalir.Text = "Salir"
        Me.TsSalir.ToolTipText = "Salir"
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.CmbDNICliente)
        Me.GroupBox1.Location = New System.Drawing.Point(406, 94)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(153, 62)
        Me.GroupBox1.TabIndex = 41
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Buscar Cliente"
        '
        'CmbDNICliente
        '
        Me.CmbDNICliente.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.CmbDNICliente.FormattingEnabled = True
        Me.CmbDNICliente.Location = New System.Drawing.Point(16, 28)
        Me.CmbDNICliente.Name = "CmbDNICliente"
        Me.CmbDNICliente.Size = New System.Drawing.Size(121, 21)
        Me.CmbDNICliente.TabIndex = 0
        '
        'DtgClientes
        '
        Me.DtgClientes.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DtgClientes.Location = New System.Drawing.Point(16, 267)
        Me.DtgClientes.Name = "DtgClientes"
        Me.DtgClientes.ReadOnly = True
        Me.DtgClientes.Size = New System.Drawing.Size(543, 148)
        Me.DtgClientes.TabIndex = 40
        '
        'TxtTelefonoMovil
        '
        Me.TxtTelefonoMovil.Location = New System.Drawing.Point(155, 176)
        Me.TxtTelefonoMovil.MaxLength = 10
        Me.TxtTelefonoMovil.Name = "TxtTelefonoMovil"
        Me.TxtTelefonoMovil.ReadOnly = True
        Me.TxtTelefonoMovil.Size = New System.Drawing.Size(232, 20)
        Me.TxtTelefonoMovil.TabIndex = 35
        '
        'TxtTelefonoFijo
        '
        Me.TxtTelefonoFijo.Location = New System.Drawing.Point(155, 150)
        Me.TxtTelefonoFijo.MaxLength = 25
        Me.TxtTelefonoFijo.Name = "TxtTelefonoFijo"
        Me.TxtTelefonoFijo.ReadOnly = True
        Me.TxtTelefonoFijo.Size = New System.Drawing.Size(232, 20)
        Me.TxtTelefonoFijo.TabIndex = 34
        '
        'TxtApellidosCliente
        '
        Me.TxtApellidosCliente.Location = New System.Drawing.Point(155, 122)
        Me.TxtApellidosCliente.MaxLength = 5
        Me.TxtApellidosCliente.Name = "TxtApellidosCliente"
        Me.TxtApellidosCliente.ReadOnly = True
        Me.TxtApellidosCliente.Size = New System.Drawing.Size(232, 20)
        Me.TxtApellidosCliente.TabIndex = 33
        '
        'TxtDNICliente
        '
        Me.TxtDNICliente.Location = New System.Drawing.Point(155, 93)
        Me.TxtDNICliente.MaxLength = 50
        Me.TxtDNICliente.Name = "TxtDNICliente"
        Me.TxtDNICliente.ReadOnly = True
        Me.TxtDNICliente.Size = New System.Drawing.Size(232, 20)
        Me.TxtDNICliente.TabIndex = 32
        '
        'TxtNombreCiente
        '
        Me.TxtNombreCiente.Location = New System.Drawing.Point(155, 64)
        Me.TxtNombreCiente.MaxLength = 50
        Me.TxtNombreCiente.Name = "TxtNombreCiente"
        Me.TxtNombreCiente.ReadOnly = True
        Me.TxtNombreCiente.Size = New System.Drawing.Size(232, 20)
        Me.TxtNombreCiente.TabIndex = 31
        '
        'TelefMovil
        '
        Me.TelefMovil.Location = New System.Drawing.Point(12, 176)
        Me.TelefMovil.Name = "TelefMovil"
        Me.TelefMovil.Size = New System.Drawing.Size(117, 13)
        Me.TelefMovil.TabIndex = 29
        Me.TelefMovil.Text = "Telefono Movil:"
        '
        'TelefFijo
        '
        Me.TelefFijo.Location = New System.Drawing.Point(12, 150)
        Me.TelefFijo.Name = "TelefFijo"
        Me.TelefFijo.Size = New System.Drawing.Size(117, 13)
        Me.TelefFijo.TabIndex = 28
        Me.TelefFijo.Text = "Telefono Fijo:"
        '
        'ApellidosCliente
        '
        Me.ApellidosCliente.Location = New System.Drawing.Point(12, 122)
        Me.ApellidosCliente.Name = "ApellidosCliente"
        Me.ApellidosCliente.Size = New System.Drawing.Size(117, 13)
        Me.ApellidosCliente.TabIndex = 27
        Me.ApellidosCliente.Text = "Apellidos:"
        '
        'DNICliente
        '
        Me.DNICliente.Location = New System.Drawing.Point(12, 96)
        Me.DNICliente.Name = "DNICliente"
        Me.DNICliente.Size = New System.Drawing.Size(117, 13)
        Me.DNICliente.TabIndex = 26
        Me.DNICliente.Text = "DNI:"
        '
        'NombreCliente
        '
        Me.NombreCliente.Location = New System.Drawing.Point(12, 67)
        Me.NombreCliente.Name = "NombreCliente"
        Me.NombreCliente.Size = New System.Drawing.Size(117, 13)
        Me.NombreCliente.TabIndex = 25
        Me.NombreCliente.Text = "Nombre:"
        '
        'TxtEmailCliente
        '
        Me.TxtEmailCliente.Location = New System.Drawing.Point(155, 230)
        Me.TxtEmailCliente.MaxLength = 10
        Me.TxtEmailCliente.Name = "TxtEmailCliente"
        Me.TxtEmailCliente.ReadOnly = True
        Me.TxtEmailCliente.Size = New System.Drawing.Size(232, 20)
        Me.TxtEmailCliente.TabIndex = 49
        '
        'TxtDirCliente
        '
        Me.TxtDirCliente.Location = New System.Drawing.Point(155, 204)
        Me.TxtDirCliente.MaxLength = 25
        Me.TxtDirCliente.Name = "TxtDirCliente"
        Me.TxtDirCliente.ReadOnly = True
        Me.TxtDirCliente.Size = New System.Drawing.Size(232, 20)
        Me.TxtDirCliente.TabIndex = 48
        '
        'EmailCliente
        '
        Me.EmailCliente.Location = New System.Drawing.Point(12, 230)
        Me.EmailCliente.Name = "EmailCliente"
        Me.EmailCliente.Size = New System.Drawing.Size(117, 13)
        Me.EmailCliente.TabIndex = 47
        Me.EmailCliente.Text = "Correo Electronico:"
        '
        'DireccionCliente
        '
        Me.DireccionCliente.Location = New System.Drawing.Point(12, 204)
        Me.DireccionCliente.Name = "DireccionCliente"
        Me.DireccionCliente.Size = New System.Drawing.Size(117, 13)
        Me.DireccionCliente.TabIndex = 46
        Me.DireccionCliente.Text = "Direccion:"
        '
        'TxtIDCliente
        '
        Me.TxtIDCliente.Location = New System.Drawing.Point(155, 35)
        Me.TxtIDCliente.MaxLength = 50
        Me.TxtIDCliente.Name = "TxtIDCliente"
        Me.TxtIDCliente.ReadOnly = True
        Me.TxtIDCliente.Size = New System.Drawing.Size(232, 20)
        Me.TxtIDCliente.TabIndex = 51
        '
        'IDCliente
        '
        Me.IDCliente.Location = New System.Drawing.Point(12, 38)
        Me.IDCliente.Name = "IDCliente"
        Me.IDCliente.Size = New System.Drawing.Size(117, 13)
        Me.IDCliente.TabIndex = 50
        Me.IDCliente.Text = "ID:"
        '
        'Clientes
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(579, 427)
        Me.Controls.Add(Me.TxtIDCliente)
        Me.Controls.Add(Me.IDCliente)
        Me.Controls.Add(Me.TxtEmailCliente)
        Me.Controls.Add(Me.TxtDirCliente)
        Me.Controls.Add(Me.EmailCliente)
        Me.Controls.Add(Me.DireccionCliente)
        Me.Controls.Add(Me.TsClientes)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.DtgClientes)
        Me.Controls.Add(Me.TxtTelefonoMovil)
        Me.Controls.Add(Me.TxtTelefonoFijo)
        Me.Controls.Add(Me.TxtApellidosCliente)
        Me.Controls.Add(Me.TxtDNICliente)
        Me.Controls.Add(Me.TxtNombreCiente)
        Me.Controls.Add(Me.TelefMovil)
        Me.Controls.Add(Me.TelefFijo)
        Me.Controls.Add(Me.ApellidosCliente)
        Me.Controls.Add(Me.DNICliente)
        Me.Controls.Add(Me.NombreCliente)
        Me.Name = "Clientes"
        Me.Text = "Clientes"
        Me.TsClientes.ResumeLayout(False)
        Me.TsClientes.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        CType(Me.DtgClientes, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents TsClientes As System.Windows.Forms.ToolStrip
    Friend WithEvents TsNuevoCliente As System.Windows.Forms.ToolStripButton
    Friend WithEvents TsModificar As System.Windows.Forms.ToolStripButton
    Friend WithEvents TsBorrar As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents TsPrimero As System.Windows.Forms.ToolStripButton
    Friend WithEvents TsAnterior As System.Windows.Forms.ToolStripButton
    Friend WithEvents TsSiguiente As System.Windows.Forms.ToolStripButton
    Friend WithEvents TsUltimo As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents ToolStripSeparator3 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents TsSalir As System.Windows.Forms.ToolStripButton
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents CmbDNICliente As System.Windows.Forms.ComboBox
    Friend WithEvents DtgClientes As System.Windows.Forms.DataGridView
    Friend WithEvents TxtTelefonoMovil As System.Windows.Forms.TextBox
    Friend WithEvents TxtTelefonoFijo As System.Windows.Forms.TextBox
    Friend WithEvents TxtApellidosCliente As System.Windows.Forms.TextBox
    Friend WithEvents TxtDNICliente As System.Windows.Forms.TextBox
    Friend WithEvents TxtNombreCiente As System.Windows.Forms.TextBox
    Friend WithEvents TelefMovil As System.Windows.Forms.Label
    Friend WithEvents TelefFijo As System.Windows.Forms.Label
    Friend WithEvents ApellidosCliente As System.Windows.Forms.Label
    Friend WithEvents DNICliente As System.Windows.Forms.Label
    Friend WithEvents NombreCliente As System.Windows.Forms.Label
    Friend WithEvents TxtEmailCliente As System.Windows.Forms.TextBox
    Friend WithEvents TxtDirCliente As System.Windows.Forms.TextBox
    Friend WithEvents EmailCliente As System.Windows.Forms.Label
    Friend WithEvents DireccionCliente As System.Windows.Forms.Label
    Friend WithEvents TxtIDCliente As System.Windows.Forms.TextBox
    Friend WithEvents IDCliente As System.Windows.Forms.Label
End Class
