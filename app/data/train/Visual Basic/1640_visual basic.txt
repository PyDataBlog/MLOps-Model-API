<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class PedidosProveedores
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
        Me.TxtIDPedidoProveedor = New System.Windows.Forms.TextBox()
        Me.IDProveedor = New System.Windows.Forms.Label()
        Me.TxtIDProveedor = New System.Windows.Forms.TextBox()
        Me.TxtPrecioTotPedidoProveedor = New System.Windows.Forms.TextBox()
        Me.TxtFechaPedido = New System.Windows.Forms.TextBox()
        Me.PrecioTotPedido = New System.Windows.Forms.Label()
        Me.NombreEmpresa = New System.Windows.Forms.Label()
        Me.FechaPedido = New System.Windows.Forms.Label()
        Me.DtgPedidosProveedores = New System.Windows.Forms.DataGridView()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.CmbIDPedidoProveedor = New System.Windows.Forms.ComboBox()
        Me.TsClientes = New System.Windows.Forms.ToolStrip()
        Me.TsNuevoPedidoProveedor = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.TsPrimero = New System.Windows.Forms.ToolStripButton()
        Me.TsAnterior = New System.Windows.Forms.ToolStripButton()
        Me.TsSiguiente = New System.Windows.Forms.ToolStripButton()
        Me.TsUltimo = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.TsSalir = New System.Windows.Forms.ToolStripButton()
        Me.AgregarLineasDePedidosProveedores = New System.Windows.Forms.Button()
        Me.DtgLineasDePedidoProveedores = New System.Windows.Forms.DataGridView()
        CType(Me.DtgPedidosProveedores, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox1.SuspendLayout()
        Me.TsClientes.SuspendLayout()
        CType(Me.DtgLineasDePedidoProveedores, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'TxtIDPedidoProveedor
        '
        Me.TxtIDPedidoProveedor.Location = New System.Drawing.Point(153, 39)
        Me.TxtIDPedidoProveedor.MaxLength = 50
        Me.TxtIDPedidoProveedor.Name = "TxtIDPedidoProveedor"
        Me.TxtIDPedidoProveedor.ReadOnly = True
        Me.TxtIDPedidoProveedor.Size = New System.Drawing.Size(232, 20)
        Me.TxtIDPedidoProveedor.TabIndex = 79
        '
        'IDProveedor
        '
        Me.IDProveedor.Location = New System.Drawing.Point(10, 42)
        Me.IDProveedor.Name = "IDProveedor"
        Me.IDProveedor.Size = New System.Drawing.Size(117, 13)
        Me.IDProveedor.TabIndex = 78
        Me.IDProveedor.Text = "ID:"
        '
        'TxtIDProveedor
        '
        Me.TxtIDProveedor.Location = New System.Drawing.Point(153, 130)
        Me.TxtIDProveedor.MaxLength = 25
        Me.TxtIDProveedor.Name = "TxtIDProveedor"
        Me.TxtIDProveedor.ReadOnly = True
        Me.TxtIDProveedor.Size = New System.Drawing.Size(232, 20)
        Me.TxtIDProveedor.TabIndex = 77
        '
        'TxtPrecioTotPedidoProveedor
        '
        Me.TxtPrecioTotPedidoProveedor.Location = New System.Drawing.Point(153, 98)
        Me.TxtPrecioTotPedidoProveedor.MaxLength = 50
        Me.TxtPrecioTotPedidoProveedor.Name = "TxtPrecioTotPedidoProveedor"
        Me.TxtPrecioTotPedidoProveedor.ReadOnly = True
        Me.TxtPrecioTotPedidoProveedor.Size = New System.Drawing.Size(232, 20)
        Me.TxtPrecioTotPedidoProveedor.TabIndex = 76
        '
        'TxtFechaPedido
        '
        Me.TxtFechaPedido.Location = New System.Drawing.Point(153, 69)
        Me.TxtFechaPedido.MaxLength = 50
        Me.TxtFechaPedido.Name = "TxtFechaPedido"
        Me.TxtFechaPedido.ReadOnly = True
        Me.TxtFechaPedido.Size = New System.Drawing.Size(232, 20)
        Me.TxtFechaPedido.TabIndex = 75
        '
        'PrecioTotPedido
        '
        Me.PrecioTotPedido.Location = New System.Drawing.Point(10, 98)
        Me.PrecioTotPedido.Name = "PrecioTotPedido"
        Me.PrecioTotPedido.Size = New System.Drawing.Size(126, 13)
        Me.PrecioTotPedido.TabIndex = 74
        Me.PrecioTotPedido.Text = "Precio Total del Pedido:"
        '
        'NombreEmpresa
        '
        Me.NombreEmpresa.Location = New System.Drawing.Point(10, 130)
        Me.NombreEmpresa.Name = "NombreEmpresa"
        Me.NombreEmpresa.Size = New System.Drawing.Size(117, 13)
        Me.NombreEmpresa.TabIndex = 73
        Me.NombreEmpresa.Text = "ID del Proveedor:"
        '
        'FechaPedido
        '
        Me.FechaPedido.Location = New System.Drawing.Point(10, 72)
        Me.FechaPedido.Name = "FechaPedido"
        Me.FechaPedido.Size = New System.Drawing.Size(117, 13)
        Me.FechaPedido.TabIndex = 72
        Me.FechaPedido.Text = "Fecha Pedido:"
        '
        'DtgPedidosProveedores
        '
        Me.DtgPedidosProveedores.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DtgPedidosProveedores.Location = New System.Drawing.Point(58, 214)
        Me.DtgPedidosProveedores.Name = "DtgPedidosProveedores"
        Me.DtgPedidosProveedores.ReadOnly = True
        Me.DtgPedidosProveedores.Size = New System.Drawing.Size(440, 148)
        Me.DtgPedidosProveedores.TabIndex = 80
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.CmbIDPedidoProveedor)
        Me.GroupBox1.Location = New System.Drawing.Point(412, 56)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(153, 62)
        Me.GroupBox1.TabIndex = 81
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Buscar Pedido Proveedor"
        '
        'CmbIDPedidoProveedor
        '
        Me.CmbIDPedidoProveedor.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.CmbIDPedidoProveedor.FormattingEnabled = True
        Me.CmbIDPedidoProveedor.Location = New System.Drawing.Point(16, 28)
        Me.CmbIDPedidoProveedor.Name = "CmbIDPedidoProveedor"
        Me.CmbIDPedidoProveedor.Size = New System.Drawing.Size(121, 21)
        Me.CmbIDPedidoProveedor.TabIndex = 0
        '
        'TsClientes
        '
        Me.TsClientes.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.TsNuevoPedidoProveedor, Me.ToolStripSeparator1, Me.TsPrimero, Me.TsAnterior, Me.TsSiguiente, Me.TsUltimo, Me.ToolStripSeparator2, Me.TsSalir})
        Me.TsClientes.Location = New System.Drawing.Point(0, 0)
        Me.TsClientes.Name = "TsClientes"
        Me.TsClientes.Size = New System.Drawing.Size(1016, 25)
        Me.TsClientes.TabIndex = 82
        Me.TsClientes.Text = "ToolStrip1"
        '
        'TsNuevoPedidoProveedor
        '
        Me.TsNuevoPedidoProveedor.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsNuevoPedidoProveedor.Image = Global.EmpresaMontaje.My.Resources.Resources._001
        Me.TsNuevoPedidoProveedor.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsNuevoPedidoProveedor.Name = "TsNuevoPedidoProveedor"
        Me.TsNuevoPedidoProveedor.Size = New System.Drawing.Size(23, 22)
        Me.TsNuevoPedidoProveedor.Text = "Nuevo Pedido Proveedor"
        Me.TsNuevoPedidoProveedor.ToolTipText = "Añadir un pedido nuevo"
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        Me.ToolStripSeparator1.Size = New System.Drawing.Size(6, 25)
        '
        'TsPrimero
        '
        Me.TsPrimero.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsPrimero.Image = Global.EmpresaMontaje.My.Resources.Resources.Primero
        Me.TsPrimero.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsPrimero.Name = "TsPrimero"
        Me.TsPrimero.Size = New System.Drawing.Size(23, 22)
        Me.TsPrimero.Text = "Primero"
        Me.TsPrimero.ToolTipText = "Ir al primer pedido"
        '
        'TsAnterior
        '
        Me.TsAnterior.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsAnterior.Image = Global.EmpresaMontaje.My.Resources.Resources.Anterior
        Me.TsAnterior.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsAnterior.Name = "TsAnterior"
        Me.TsAnterior.Size = New System.Drawing.Size(23, 22)
        Me.TsAnterior.Text = "Anterior"
        Me.TsAnterior.ToolTipText = "Ir al pedido anterior"
        '
        'TsSiguiente
        '
        Me.TsSiguiente.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsSiguiente.Image = Global.EmpresaMontaje.My.Resources.Resources.Siguiente
        Me.TsSiguiente.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsSiguiente.Name = "TsSiguiente"
        Me.TsSiguiente.Size = New System.Drawing.Size(23, 22)
        Me.TsSiguiente.Text = "Siguiente"
        Me.TsSiguiente.ToolTipText = "Ir al siguiente pedido"
        '
        'TsUltimo
        '
        Me.TsUltimo.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsUltimo.Image = Global.EmpresaMontaje.My.Resources.Resources.Ultimo
        Me.TsUltimo.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsUltimo.Name = "TsUltimo"
        Me.TsUltimo.Size = New System.Drawing.Size(23, 22)
        Me.TsUltimo.Text = "Ultimo"
        Me.TsUltimo.ToolTipText = "Ir al ultimo pedido"
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        Me.ToolStripSeparator2.Size = New System.Drawing.Size(6, 25)
        '
        'TsSalir
        '
        Me.TsSalir.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsSalir.Image = Global.EmpresaMontaje.My.Resources.Resources._547
        Me.TsSalir.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsSalir.Name = "TsSalir"
        Me.TsSalir.Size = New System.Drawing.Size(23, 22)
        Me.TsSalir.Text = "Salir"
        Me.TsSalir.ToolTipText = "Salir"
        '
        'AgregarLineasDePedidosProveedores
        '
        Me.AgregarLineasDePedidosProveedores.Location = New System.Drawing.Point(329, 176)
        Me.AgregarLineasDePedidosProveedores.Name = "AgregarLineasDePedidosProveedores"
        Me.AgregarLineasDePedidosProveedores.Size = New System.Drawing.Size(236, 23)
        Me.AgregarLineasDePedidosProveedores.TabIndex = 83
        Me.AgregarLineasDePedidosProveedores.Text = "Agregar Lineas de Pedidos de los Proveedores"
        Me.AgregarLineasDePedidosProveedores.UseVisualStyleBackColor = True
        '
        'DtgLineasDePedidoProveedores
        '
        Me.DtgLineasDePedidoProveedores.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DtgLineasDePedidoProveedores.Location = New System.Drawing.Point(591, 39)
        Me.DtgLineasDePedidoProveedores.Name = "DtgLineasDePedidoProveedores"
        Me.DtgLineasDePedidoProveedores.ReadOnly = True
        Me.DtgLineasDePedidoProveedores.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        Me.DtgLineasDePedidoProveedores.Size = New System.Drawing.Size(413, 323)
        Me.DtgLineasDePedidoProveedores.TabIndex = 120
        '
        'PedidosProveedores
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(1016, 374)
        Me.Controls.Add(Me.DtgLineasDePedidoProveedores)
        Me.Controls.Add(Me.AgregarLineasDePedidosProveedores)
        Me.Controls.Add(Me.TsClientes)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.DtgPedidosProveedores)
        Me.Controls.Add(Me.TxtIDPedidoProveedor)
        Me.Controls.Add(Me.IDProveedor)
        Me.Controls.Add(Me.TxtIDProveedor)
        Me.Controls.Add(Me.TxtPrecioTotPedidoProveedor)
        Me.Controls.Add(Me.TxtFechaPedido)
        Me.Controls.Add(Me.PrecioTotPedido)
        Me.Controls.Add(Me.NombreEmpresa)
        Me.Controls.Add(Me.FechaPedido)
        Me.Name = "PedidosProveedores"
        Me.Text = "PedidosProveedores"
        CType(Me.DtgPedidosProveedores, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox1.ResumeLayout(False)
        Me.TsClientes.ResumeLayout(False)
        Me.TsClientes.PerformLayout()
        CType(Me.DtgLineasDePedidoProveedores, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents TxtIDPedidoProveedor As System.Windows.Forms.TextBox
    Friend WithEvents IDProveedor As System.Windows.Forms.Label
    Friend WithEvents TxtIDProveedor As System.Windows.Forms.TextBox
    Friend WithEvents TxtPrecioTotPedidoProveedor As System.Windows.Forms.TextBox
    Friend WithEvents TxtFechaPedido As System.Windows.Forms.TextBox
    Friend WithEvents PrecioTotPedido As System.Windows.Forms.Label
    Friend WithEvents NombreEmpresa As System.Windows.Forms.Label
    Friend WithEvents FechaPedido As System.Windows.Forms.Label
    Friend WithEvents DtgPedidosProveedores As System.Windows.Forms.DataGridView
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents CmbIDPedidoProveedor As System.Windows.Forms.ComboBox
    Friend WithEvents TsClientes As System.Windows.Forms.ToolStrip
    Friend WithEvents TsNuevoPedidoProveedor As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents TsPrimero As System.Windows.Forms.ToolStripButton
    Friend WithEvents TsAnterior As System.Windows.Forms.ToolStripButton
    Friend WithEvents TsSiguiente As System.Windows.Forms.ToolStripButton
    Friend WithEvents TsUltimo As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents TsSalir As System.Windows.Forms.ToolStripButton
    Friend WithEvents AgregarLineasDePedidosProveedores As System.Windows.Forms.Button
    Friend WithEvents DtgLineasDePedidoProveedores As System.Windows.Forms.DataGridView
    Friend WithEvents Label1 As System.Windows.Forms.Label
End Class
