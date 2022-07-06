<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Productos
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Productos))
        Me.TsProductos = New System.Windows.Forms.ToolStrip()
        Me.TsNuevoProducto = New System.Windows.Forms.ToolStripButton()
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
        Me.CmbNombreProducto = New System.Windows.Forms.ComboBox()
        Me.DtgProductos = New System.Windows.Forms.DataGridView()
        Me.TxtPrecioProducto = New System.Windows.Forms.TextBox()
        Me.TxtDescProducto = New System.Windows.Forms.TextBox()
        Me.TxtNombreProducto = New System.Windows.Forms.TextBox()
        Me.DescProducto = New System.Windows.Forms.Label()
        Me.PrecioProducto = New System.Windows.Forms.Label()
        Me.NombreProducto = New System.Windows.Forms.Label()
        Me.TxtIDProducto = New System.Windows.Forms.TextBox()
        Me.IdProducto = New System.Windows.Forms.Label()
        Me.AgregarComponentesDeProductos = New System.Windows.Forms.Button()
        Me.DtgComponentesProducto = New System.Windows.Forms.DataGridView()
        Me.TsProductos.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        CType(Me.DtgProductos, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.DtgComponentesProducto, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'TsProductos
        '
        Me.TsProductos.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.TsNuevoProducto, Me.TsBorrar, Me.ToolStripSeparator1, Me.TsPrimero, Me.TsAnterior, Me.TsSiguiente, Me.TsUltimo, Me.ToolStripSeparator2, Me.ToolStripSeparator3, Me.TsSalir})
        Me.TsProductos.Location = New System.Drawing.Point(0, 0)
        Me.TsProductos.Name = "TsProductos"
        Me.TsProductos.Size = New System.Drawing.Size(883, 25)
        Me.TsProductos.TabIndex = 82
        Me.TsProductos.Text = "ToolStrip1"
        '
        'TsNuevoProducto
        '
        Me.TsNuevoProducto.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsNuevoProducto.Image = CType(resources.GetObject("TsNuevoProducto.Image"), System.Drawing.Image)
        Me.TsNuevoProducto.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsNuevoProducto.Name = "TsNuevoProducto"
        Me.TsNuevoProducto.Size = New System.Drawing.Size(23, 22)
        Me.TsNuevoProducto.Text = "Nuevo Producto"
        Me.TsNuevoProducto.ToolTipText = "Añadir un producto nuevo"
        '
        'TsBorrar
        '
        Me.TsBorrar.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsBorrar.Image = CType(resources.GetObject("TsBorrar.Image"), System.Drawing.Image)
        Me.TsBorrar.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsBorrar.Name = "TsBorrar"
        Me.TsBorrar.Size = New System.Drawing.Size(23, 22)
        Me.TsBorrar.Text = "Borrar Producto"
        Me.TsBorrar.ToolTipText = "Borrar el producto seleccionado"
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
        Me.TsPrimero.ToolTipText = "Ir al primer producto"
        '
        'TsAnterior
        '
        Me.TsAnterior.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsAnterior.Image = CType(resources.GetObject("TsAnterior.Image"), System.Drawing.Image)
        Me.TsAnterior.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsAnterior.Name = "TsAnterior"
        Me.TsAnterior.Size = New System.Drawing.Size(23, 22)
        Me.TsAnterior.Text = "Anterior"
        Me.TsAnterior.ToolTipText = "Ir al producto anterior"
        '
        'TsSiguiente
        '
        Me.TsSiguiente.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsSiguiente.Image = CType(resources.GetObject("TsSiguiente.Image"), System.Drawing.Image)
        Me.TsSiguiente.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsSiguiente.Name = "TsSiguiente"
        Me.TsSiguiente.Size = New System.Drawing.Size(23, 22)
        Me.TsSiguiente.Text = "Siguiente"
        Me.TsSiguiente.ToolTipText = "Ir al siguiente producto"
        '
        'TsUltimo
        '
        Me.TsUltimo.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TsUltimo.Image = CType(resources.GetObject("TsUltimo.Image"), System.Drawing.Image)
        Me.TsUltimo.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TsUltimo.Name = "TsUltimo"
        Me.TsUltimo.Size = New System.Drawing.Size(23, 22)
        Me.TsUltimo.Text = "Ultimo"
        Me.TsUltimo.ToolTipText = "Ir al ultimo producto"
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
        Me.GroupBox1.Controls.Add(Me.CmbNombreProducto)
        Me.GroupBox1.Location = New System.Drawing.Point(432, 48)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(153, 62)
        Me.GroupBox1.TabIndex = 81
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Buscar Producto"
        '
        'CmbNombreProducto
        '
        Me.CmbNombreProducto.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.CmbNombreProducto.FormattingEnabled = True
        Me.CmbNombreProducto.Location = New System.Drawing.Point(16, 28)
        Me.CmbNombreProducto.Name = "CmbNombreProducto"
        Me.CmbNombreProducto.Size = New System.Drawing.Size(121, 21)
        Me.CmbNombreProducto.TabIndex = 0
        '
        'DtgProductos
        '
        Me.DtgProductos.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DtgProductos.Location = New System.Drawing.Point(71, 251)
        Me.DtgProductos.Name = "DtgProductos"
        Me.DtgProductos.ReadOnly = True
        Me.DtgProductos.Size = New System.Drawing.Size(453, 148)
        Me.DtgProductos.TabIndex = 80
        '
        'TxtPrecioProducto
        '
        Me.TxtPrecioProducto.Location = New System.Drawing.Point(153, 175)
        Me.TxtPrecioProducto.MaxLength = 25
        Me.TxtPrecioProducto.Name = "TxtPrecioProducto"
        Me.TxtPrecioProducto.ReadOnly = True
        Me.TxtPrecioProducto.Size = New System.Drawing.Size(232, 20)
        Me.TxtPrecioProducto.TabIndex = 78
        '
        'TxtDescProducto
        '
        Me.TxtDescProducto.Location = New System.Drawing.Point(153, 105)
        Me.TxtDescProducto.MaxLength = 50
        Me.TxtDescProducto.Multiline = True
        Me.TxtDescProducto.Name = "TxtDescProducto"
        Me.TxtDescProducto.ReadOnly = True
        Me.TxtDescProducto.Size = New System.Drawing.Size(232, 58)
        Me.TxtDescProducto.TabIndex = 77
        '
        'TxtNombreProducto
        '
        Me.TxtNombreProducto.Location = New System.Drawing.Point(153, 76)
        Me.TxtNombreProducto.MaxLength = 50
        Me.TxtNombreProducto.Name = "TxtNombreProducto"
        Me.TxtNombreProducto.ReadOnly = True
        Me.TxtNombreProducto.Size = New System.Drawing.Size(232, 20)
        Me.TxtNombreProducto.TabIndex = 76
        '
        'DescProducto
        '
        Me.DescProducto.Location = New System.Drawing.Point(10, 105)
        Me.DescProducto.Name = "DescProducto"
        Me.DescProducto.Size = New System.Drawing.Size(117, 13)
        Me.DescProducto.TabIndex = 73
        Me.DescProducto.Text = "Descripcion:"
        '
        'PrecioProducto
        '
        Me.PrecioProducto.Location = New System.Drawing.Point(10, 175)
        Me.PrecioProducto.Name = "PrecioProducto"
        Me.PrecioProducto.Size = New System.Drawing.Size(117, 13)
        Me.PrecioProducto.TabIndex = 72
        Me.PrecioProducto.Text = "Precio:"
        '
        'NombreProducto
        '
        Me.NombreProducto.Location = New System.Drawing.Point(10, 79)
        Me.NombreProducto.Name = "NombreProducto"
        Me.NombreProducto.Size = New System.Drawing.Size(117, 13)
        Me.NombreProducto.TabIndex = 71
        Me.NombreProducto.Text = "Nombre:"
        '
        'TxtIDProducto
        '
        Me.TxtIDProducto.Location = New System.Drawing.Point(153, 44)
        Me.TxtIDProducto.MaxLength = 50
        Me.TxtIDProducto.Name = "TxtIDProducto"
        Me.TxtIDProducto.ReadOnly = True
        Me.TxtIDProducto.Size = New System.Drawing.Size(232, 20)
        Me.TxtIDProducto.TabIndex = 87
        '
        'IdProducto
        '
        Me.IdProducto.Location = New System.Drawing.Point(10, 47)
        Me.IdProducto.Name = "IdProducto"
        Me.IdProducto.Size = New System.Drawing.Size(117, 13)
        Me.IdProducto.TabIndex = 86
        Me.IdProducto.Text = "ID:"
        '
        'AgregarComponentesDeProductos
        '
        Me.AgregarComponentesDeProductos.Location = New System.Drawing.Point(353, 211)
        Me.AgregarComponentesDeProductos.Name = "AgregarComponentesDeProductos"
        Me.AgregarComponentesDeProductos.Size = New System.Drawing.Size(232, 23)
        Me.AgregarComponentesDeProductos.TabIndex = 88
        Me.AgregarComponentesDeProductos.Text = "Agregar Componentes de los Productos"
        Me.AgregarComponentesDeProductos.UseVisualStyleBackColor = True
        '
        'DtgComponentesProducto
        '
        Me.DtgComponentesProducto.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DtgComponentesProducto.Location = New System.Drawing.Point(611, 44)
        Me.DtgComponentesProducto.Name = "DtgComponentesProducto"
        Me.DtgComponentesProducto.ReadOnly = True
        Me.DtgComponentesProducto.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        Me.DtgComponentesProducto.Size = New System.Drawing.Size(250, 354)
        Me.DtgComponentesProducto.TabIndex = 89
        '
        'Productos
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(883, 411)
        Me.Controls.Add(Me.DtgComponentesProducto)
        Me.Controls.Add(Me.AgregarComponentesDeProductos)
        Me.Controls.Add(Me.TxtIDProducto)
        Me.Controls.Add(Me.IdProducto)
        Me.Controls.Add(Me.TsProductos)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.DtgProductos)
        Me.Controls.Add(Me.TxtPrecioProducto)
        Me.Controls.Add(Me.TxtDescProducto)
        Me.Controls.Add(Me.TxtNombreProducto)
        Me.Controls.Add(Me.DescProducto)
        Me.Controls.Add(Me.PrecioProducto)
        Me.Controls.Add(Me.NombreProducto)
        Me.Name = "Productos"
        Me.Text = "Productos"
        Me.TsProductos.ResumeLayout(False)
        Me.TsProductos.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        CType(Me.DtgProductos, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.DtgComponentesProducto, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents TsProductos As System.Windows.Forms.ToolStrip
    Friend WithEvents TsNuevoProducto As System.Windows.Forms.ToolStripButton
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
    Friend WithEvents CmbNombreProducto As System.Windows.Forms.ComboBox
    Friend WithEvents DtgProductos As System.Windows.Forms.DataGridView
    Friend WithEvents TxtPrecioProducto As System.Windows.Forms.TextBox
    Friend WithEvents TxtDescProducto As System.Windows.Forms.TextBox
    Friend WithEvents TxtNombreProducto As System.Windows.Forms.TextBox
    Friend WithEvents DescProducto As System.Windows.Forms.Label
    Friend WithEvents PrecioProducto As System.Windows.Forms.Label
    Friend WithEvents NombreProducto As System.Windows.Forms.Label
    Friend WithEvents TxtIDProducto As System.Windows.Forms.TextBox
    Friend WithEvents IdProducto As System.Windows.Forms.Label
    Friend WithEvents AgregarComponentesDeProductos As System.Windows.Forms.Button
    Friend WithEvents DtgComponentesProducto As System.Windows.Forms.DataGridView
End Class
