<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Proveedores
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Proveedores))
        Me.TxtEmailEmpresa = New System.Windows.Forms.TextBox()
        Me.TxtDirEmpresa = New System.Windows.Forms.TextBox()
        Me.EmailEmpresa = New System.Windows.Forms.Label()
        Me.DireccionEmpresa = New System.Windows.Forms.Label()
        Me.TsProveedores = New System.Windows.Forms.ToolStrip()
        Me.TsNuevoProveedor = New System.Windows.Forms.ToolStripButton()
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
        Me.CmbNombreProveedor = New System.Windows.Forms.ComboBox()
        Me.DtgProveedores = New System.Windows.Forms.DataGridView()
        Me.TxtTelefEmpresa = New System.Windows.Forms.TextBox()
        Me.TxtNombreEmpresa = New System.Windows.Forms.TextBox()
        Me.TxtTelefProveedor = New System.Windows.Forms.TextBox()
        Me.TxtNombreProveedor = New System.Windows.Forms.TextBox()
        Me.TelefEmpresa = New System.Windows.Forms.Label()
        Me.TelefProveedor = New System.Windows.Forms.Label()
        Me.NombreEmpresa = New System.Windows.Forms.Label()
        Me.NombreProveedor = New System.Windows.Forms.Label()
        Me.TxtIDProveedor = New System.Windows.Forms.TextBox()
        Me.IDProveedor = New System.Windows.Forms.Label()
        Me.TsProveedores.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        CType(Me.DtgProveedores, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'TxtEmailEmpresa
        '
        Me.TxtEmailEmpresa.Location = New System.Drawing.Point(155, 208)
        Me.TxtEmailEmpresa.MaxLength = 50
        Me.TxtEmailEmpresa.Name = "TxtEmailEmpresa"
        Me.TxtEmailEmpresa.ReadOnly = True
        Me.TxtEmailEmpresa.Size = New System.Drawing.Size(232, 20)
        Me.TxtEmailEmpresa.TabIndex = 69
        '
        'TxtDirEmpresa
        '
        Me.TxtDirEmpresa.Location = New System.Drawing.Point(155, 182)
        Me.TxtDirEmpresa.MaxLength = 50
        Me.TxtDirEmpresa.Name = "TxtDirEmpresa"
        Me.TxtDirEmpresa.ReadOnly = True
        Me.TxtDirEmpresa.Size = New System.Drawing.Size(232, 20)
        Me.TxtDirEmpresa.TabIndex = 68
        '
        'EmailEmpresa
        '
        Me.EmailEmpresa.Location = New System.Drawing.Point(12, 208)
        Me.EmailEmpresa.Name = "EmailEmpresa"
        Me.EmailEmpresa.Size = New System.Drawing.Size(117, 13)
        Me.EmailEmpresa.TabIndex = 67
        Me.EmailEmpresa.Text = "Correo Electronico:"
        '
        'DireccionEmpresa
        '
        Me.DireccionEmpresa.Location = New System.Drawing.Point(12, 182)
        Me.DireccionEmpresa.Name = "DireccionEmpresa"
        Me.DireccionEmpresa.Size = New System.Drawing.Size(117, 13)
        Me.DireccionEmpresa.TabIndex = 66
        Me.DireccionEmpresa.Text = "Direccion:"
        '
        'TsProveedores
        '
        Me.TsProveedores.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.TsNuevoProveedor, Me.TsModificar, Me.TsBorrar, Me.ToolStripSeparator1, Me.TsPrimero, Me.TsAnterior, Me.TsSiguiente, Me.TsUltimo, Me.ToolStripSeparator2, Me.ToolStripSeparator3, Me.TsSalir})
        Me.TsProveedores.Location = New System.Drawing.Point(0, 0)
        Me.TsProveedores.Name = "TsProveedores"
        Me.TsProveedores.Size = New System.Drawing.Size(602, 25)
        Me.TsProveedores.TabIndex = 64
        Me.TsProveedores.Text = "ToolStrip1"
        '
        'TsNuevoProveedor
        '
        Me.TsNuevoProveedor.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsNuevoProveedor.Image = CType(resources.GetObject("TsNuevoProveedor.Image"), System.Drawing.Image)
        Me.TsNuevoProveedor.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsNuevoProveedor.Name = "TsNuevoProveedor"
        Me.TsNuevoProveedor.Size = New System.Drawing.Size(23, 22)
        Me.TsNuevoProveedor.Text = "Nuevo Proveedor"
        Me.TsNuevoProveedor.ToolTipText = "Añadir un proveedor nuevo"
        '
        'TsModificar
        '
        Me.TsModificar.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsModificar.Image = CType(resources.GetObject("TsModificar.Image"), System.Drawing.Image)
        Me.TsModificar.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsModificar.Name = "TsModificar"
        Me.TsModificar.Size = New System.Drawing.Size(23, 22)
        Me.TsModificar.Text = "Modificar Proveedor"
        Me.TsModificar.ToolTipText = "Modificar el proveedor seleccionado"
        '
        'TsBorrar
        '
        Me.TsBorrar.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsBorrar.Image = CType(resources.GetObject("TsBorrar.Image"), System.Drawing.Image)
        Me.TsBorrar.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsBorrar.Name = "TsBorrar"
        Me.TsBorrar.Size = New System.Drawing.Size(23, 22)
        Me.TsBorrar.Text = "Borrar Proveedor"
        Me.TsBorrar.ToolTipText = "Borrar el proveedor seleccionado"
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
        Me.TsPrimero.ToolTipText = "Ir al primer proveedor"
        '
        'TsAnterior
        '
        Me.TsAnterior.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsAnterior.Image = CType(resources.GetObject("TsAnterior.Image"), System.Drawing.Image)
        Me.TsAnterior.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsAnterior.Name = "TsAnterior"
        Me.TsAnterior.Size = New System.Drawing.Size(23, 22)
        Me.TsAnterior.Text = "Anterior"
        Me.TsAnterior.ToolTipText = "Ir al proveedor anterior"
        '
        'TsSiguiente
        '
        Me.TsSiguiente.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsSiguiente.Image = CType(resources.GetObject("TsSiguiente.Image"), System.Drawing.Image)
        Me.TsSiguiente.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsSiguiente.Name = "TsSiguiente"
        Me.TsSiguiente.Size = New System.Drawing.Size(23, 22)
        Me.TsSiguiente.Text = "Siguiente"
        Me.TsSiguiente.ToolTipText = "Ir al siguiente proveedor"
        '
        'TsUltimo
        '
        Me.TsUltimo.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsUltimo.Image = CType(resources.GetObject("TsUltimo.Image"), System.Drawing.Image)
        Me.TsUltimo.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsUltimo.Name = "TsUltimo"
        Me.TsUltimo.Size = New System.Drawing.Size(23, 22)
        Me.TsUltimo.Text = "Ultimo"
        Me.TsUltimo.ToolTipText = "Ir al ultimo proveedor"
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
        Me.GroupBox1.Controls.Add(Me.CmbNombreProveedor)
        Me.GroupBox1.Location = New System.Drawing.Point(424, 83)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(153, 62)
        Me.GroupBox1.TabIndex = 63
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Buscar Proveedor"
        '
        'CmbNombreProveedor
        '
        Me.CmbNombreProveedor.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.CmbNombreProveedor.FormattingEnabled = True
        Me.CmbNombreProveedor.Location = New System.Drawing.Point(16, 28)
        Me.CmbNombreProveedor.Name = "CmbNombreProveedor"
        Me.CmbNombreProveedor.Size = New System.Drawing.Size(121, 21)
        Me.CmbNombreProveedor.TabIndex = 0
        '
        'DtgProveedores
        '
        Me.DtgProveedores.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DtgProveedores.Location = New System.Drawing.Point(15, 243)
        Me.DtgProveedores.Name = "DtgProveedores"
        Me.DtgProveedores.Size = New System.Drawing.Size(562, 148)
        Me.DtgProveedores.TabIndex = 62
        '
        'TxtTelefEmpresa
        '
        Me.TxtTelefEmpresa.Location = New System.Drawing.Point(155, 154)
        Me.TxtTelefEmpresa.MaxLength = 9
        Me.TxtTelefEmpresa.Name = "TxtTelefEmpresa"
        Me.TxtTelefEmpresa.ReadOnly = True
        Me.TxtTelefEmpresa.Size = New System.Drawing.Size(232, 20)
        Me.TxtTelefEmpresa.TabIndex = 9
        '
        'TxtNombreEmpresa
        '
        Me.TxtNombreEmpresa.Location = New System.Drawing.Point(155, 128)
        Me.TxtNombreEmpresa.MaxLength = 25
        Me.TxtNombreEmpresa.Name = "TxtNombreEmpresa"
        Me.TxtNombreEmpresa.ReadOnly = True
        Me.TxtNombreEmpresa.Size = New System.Drawing.Size(232, 20)
        Me.TxtNombreEmpresa.TabIndex = 60
        '
        'TxtTelefProveedor
        '
        Me.TxtTelefProveedor.Location = New System.Drawing.Point(155, 96)
        Me.TxtTelefProveedor.MaxLength = 9
        Me.TxtTelefProveedor.Name = "TxtTelefProveedor"
        Me.TxtTelefProveedor.ReadOnly = True
        Me.TxtTelefProveedor.Size = New System.Drawing.Size(232, 20)
        Me.TxtTelefProveedor.TabIndex = 58
        '
        'TxtNombreProveedor
        '
        Me.TxtNombreProveedor.Location = New System.Drawing.Point(155, 67)
        Me.TxtNombreProveedor.MaxLength = 50
        Me.TxtNombreProveedor.Name = "TxtNombreProveedor"
        Me.TxtNombreProveedor.ReadOnly = True
        Me.TxtNombreProveedor.Size = New System.Drawing.Size(232, 20)
        Me.TxtNombreProveedor.TabIndex = 57
        '
        'TelefEmpresa
        '
        Me.TelefEmpresa.Location = New System.Drawing.Point(12, 154)
        Me.TelefEmpresa.Name = "TelefEmpresa"
        Me.TelefEmpresa.Size = New System.Drawing.Size(117, 13)
        Me.TelefEmpresa.TabIndex = 55
        Me.TelefEmpresa.Text = "Telefono Empresa:"
        '
        'TelefProveedor
        '
        Me.TelefProveedor.Location = New System.Drawing.Point(12, 96)
        Me.TelefProveedor.Name = "TelefProveedor"
        Me.TelefProveedor.Size = New System.Drawing.Size(117, 13)
        Me.TelefProveedor.TabIndex = 54
        Me.TelefProveedor.Text = "Telefono Proveedor:"
        '
        'NombreEmpresa
        '
        Me.NombreEmpresa.Location = New System.Drawing.Point(12, 128)
        Me.NombreEmpresa.Name = "NombreEmpresa"
        Me.NombreEmpresa.Size = New System.Drawing.Size(117, 13)
        Me.NombreEmpresa.TabIndex = 52
        Me.NombreEmpresa.Text = "Nombre Empresa:"
        '
        'NombreProveedor
        '
        Me.NombreProveedor.Location = New System.Drawing.Point(12, 70)
        Me.NombreProveedor.Name = "NombreProveedor"
        Me.NombreProveedor.Size = New System.Drawing.Size(117, 13)
        Me.NombreProveedor.TabIndex = 51
        Me.NombreProveedor.Text = "Nombre Proveedor:"
        '
        'TxtIDProveedor
        '
        Me.TxtIDProveedor.Location = New System.Drawing.Point(155, 37)
        Me.TxtIDProveedor.MaxLength = 50
        Me.TxtIDProveedor.Name = "TxtIDProveedor"
        Me.TxtIDProveedor.ReadOnly = True
        Me.TxtIDProveedor.Size = New System.Drawing.Size(232, 20)
        Me.TxtIDProveedor.TabIndex = 71
        '
        'IDProveedor
        '
        Me.IDProveedor.Location = New System.Drawing.Point(12, 40)
        Me.IDProveedor.Name = "IDProveedor"
        Me.IDProveedor.Size = New System.Drawing.Size(117, 13)
        Me.IDProveedor.TabIndex = 70
        Me.IDProveedor.Text = "ID:"
        '
        'Proveedores
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(602, 412)
        Me.Controls.Add(Me.TxtIDProveedor)
        Me.Controls.Add(Me.IDProveedor)
        Me.Controls.Add(Me.TxtEmailEmpresa)
        Me.Controls.Add(Me.TxtDirEmpresa)
        Me.Controls.Add(Me.EmailEmpresa)
        Me.Controls.Add(Me.DireccionEmpresa)
        Me.Controls.Add(Me.TsProveedores)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.DtgProveedores)
        Me.Controls.Add(Me.TxtTelefEmpresa)
        Me.Controls.Add(Me.TxtNombreEmpresa)
        Me.Controls.Add(Me.TxtTelefProveedor)
        Me.Controls.Add(Me.TxtNombreProveedor)
        Me.Controls.Add(Me.TelefEmpresa)
        Me.Controls.Add(Me.TelefProveedor)
        Me.Controls.Add(Me.NombreEmpresa)
        Me.Controls.Add(Me.NombreProveedor)
        Me.Name = "Proveedores"
        Me.Text = "Proveedores"
        Me.TsProveedores.ResumeLayout(False)
        Me.TsProveedores.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        CType(Me.DtgProveedores, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents TxtEmailEmpresa As System.Windows.Forms.TextBox
    Friend WithEvents TxtDirEmpresa As System.Windows.Forms.TextBox
    Friend WithEvents EmailEmpresa As System.Windows.Forms.Label
    Friend WithEvents DireccionEmpresa As System.Windows.Forms.Label
    Friend WithEvents TsProveedores As System.Windows.Forms.ToolStrip
    Friend WithEvents TsNuevoProveedor As System.Windows.Forms.ToolStripButton
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
    Friend WithEvents CmbNombreProveedor As System.Windows.Forms.ComboBox
    Friend WithEvents DtgProveedores As System.Windows.Forms.DataGridView
    Friend WithEvents TxtTelefEmpresa As System.Windows.Forms.TextBox
    Friend WithEvents TxtNombreEmpresa As System.Windows.Forms.TextBox
    Friend WithEvents TxtTelefProveedor As System.Windows.Forms.TextBox
    Friend WithEvents TxtNombreProveedor As System.Windows.Forms.TextBox
    Friend WithEvents TelefEmpresa As System.Windows.Forms.Label
    Friend WithEvents TelefProveedor As System.Windows.Forms.Label
    Friend WithEvents NombreEmpresa As System.Windows.Forms.Label
    Friend WithEvents NombreProveedor As System.Windows.Forms.Label
    Friend WithEvents TxtIDProveedor As System.Windows.Forms.TextBox
    Friend WithEvents IDProveedor As System.Windows.Forms.Label
End Class
