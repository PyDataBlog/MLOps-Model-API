<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class Graficas
    Inherits System.Windows.Forms.Form

    'Form reemplaza a Dispose para limpiar la lista de componentes.
    <System.Diagnostics.DebuggerNonUserCode()>
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
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.CrearToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CasaToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ApartamentoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.VeiculosToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SalirToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CerrarSecionToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SalirToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.Usuarios = New System.Windows.Forms.RadioButton()
        Me.Paquetes = New System.Windows.Forms.RadioButton()
        Me.Casa = New System.Windows.Forms.RadioButton()
        Me.Veiculos = New System.Windows.Forms.RadioButton()
        Me.Apartamentos = New System.Windows.Forms.RadioButton()
        Me.DataGridView1 = New System.Windows.Forms.DataGridView()
        Me.Tramitar = New System.Windows.Forms.Button()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Transaccion = New System.Windows.Forms.GroupBox()
        Me.Alquilartermino = New System.Windows.Forms.RadioButton()
        Me.Prereservar = New System.Windows.Forms.RadioButton()
        Me.Reservar = New System.Windows.Forms.RadioButton()
        Me.Comprar = New System.Windows.Forms.RadioButton()
        Me.Alquilar = New System.Windows.Forms.RadioButton()
        Me.Buscar = New System.Windows.Forms.Button()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.TextBox3 = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.TextBox4 = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.TextBox2 = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.CheckBox1 = New System.Windows.Forms.CheckBox()
        Me.CheckBox2 = New System.Windows.Forms.CheckBox()
        Me.CheckBox3 = New System.Windows.Forms.CheckBox()
        Me.CheckBox4 = New System.Windows.Forms.CheckBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.TextBox5 = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.TextBox6 = New System.Windows.Forms.TextBox()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.TextBox7 = New System.Windows.Forms.TextBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.TextBox8 = New System.Windows.Forms.TextBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.CheckBox5 = New System.Windows.Forms.CheckBox()
        Me.TextBox9 = New System.Windows.Forms.TextBox()
        Me.TextBox10 = New System.Windows.Forms.TextBox()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.TextBox11 = New System.Windows.Forms.TextBox()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.TextBox12 = New System.Windows.Forms.TextBox()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.TextBox13 = New System.Windows.Forms.TextBox()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.TextBox14 = New System.Windows.Forms.TextBox()
        Me.MenuStrip1.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.Transaccion.SuspendLayout()
        Me.SuspendLayout()
        '
        'MenuStrip1
        '
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CrearToolStripMenuItem, Me.SalirToolStripMenuItem})
        Me.MenuStrip1.Location = New System.Drawing.Point(0, 0)
        Me.MenuStrip1.Name = "MenuStrip1"
        Me.MenuStrip1.Size = New System.Drawing.Size(809, 24)
        Me.MenuStrip1.TabIndex = 0
        Me.MenuStrip1.Text = "MenuStrip1"
        '
        'CrearToolStripMenuItem
        '
        Me.CrearToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CasaToolStripMenuItem, Me.ApartamentoToolStripMenuItem, Me.VeiculosToolStripMenuItem})
        Me.CrearToolStripMenuItem.Name = "CrearToolStripMenuItem"
        Me.CrearToolStripMenuItem.Size = New System.Drawing.Size(81, 20)
        Me.CrearToolStripMenuItem.Text = "Administrar"
        '
        'CasaToolStripMenuItem
        '
        Me.CasaToolStripMenuItem.Name = "CasaToolStripMenuItem"
        Me.CasaToolStripMenuItem.Size = New System.Drawing.Size(152, 22)
        Me.CasaToolStripMenuItem.Text = "Crear"
        '
        'ApartamentoToolStripMenuItem
        '
        Me.ApartamentoToolStripMenuItem.Name = "ApartamentoToolStripMenuItem"
        Me.ApartamentoToolStripMenuItem.Size = New System.Drawing.Size(152, 22)
        Me.ApartamentoToolStripMenuItem.Text = "Borrar"
        '
        'VeiculosToolStripMenuItem
        '
        Me.VeiculosToolStripMenuItem.Name = "VeiculosToolStripMenuItem"
        Me.VeiculosToolStripMenuItem.Size = New System.Drawing.Size(152, 22)
        Me.VeiculosToolStripMenuItem.Text = "Editar"
        '
        'SalirToolStripMenuItem
        '
        Me.SalirToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CerrarSecionToolStripMenuItem, Me.SalirToolStripMenuItem1})
        Me.SalirToolStripMenuItem.Name = "SalirToolStripMenuItem"
        Me.SalirToolStripMenuItem.Size = New System.Drawing.Size(41, 20)
        Me.SalirToolStripMenuItem.Text = "Salir"
        '
        'CerrarSecionToolStripMenuItem
        '
        Me.CerrarSecionToolStripMenuItem.Name = "CerrarSecionToolStripMenuItem"
        Me.CerrarSecionToolStripMenuItem.Size = New System.Drawing.Size(152, 22)
        Me.CerrarSecionToolStripMenuItem.Text = "Cerrar secion"
        '
        'SalirToolStripMenuItem1
        '
        Me.SalirToolStripMenuItem1.Name = "SalirToolStripMenuItem1"
        Me.SalirToolStripMenuItem1.Size = New System.Drawing.Size(152, 22)
        Me.SalirToolStripMenuItem1.Text = "Salir"
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.Usuarios)
        Me.GroupBox1.Controls.Add(Me.Paquetes)
        Me.GroupBox1.Controls.Add(Me.Casa)
        Me.GroupBox1.Controls.Add(Me.Veiculos)
        Me.GroupBox1.Controls.Add(Me.Apartamentos)
        Me.GroupBox1.Location = New System.Drawing.Point(13, 37)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(171, 91)
        Me.GroupBox1.TabIndex = 1
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Buscar"
        '
        'Usuarios
        '
        Me.Usuarios.AutoSize = True
        Me.Usuarios.Location = New System.Drawing.Point(6, 68)
        Me.Usuarios.Name = "Usuarios"
        Me.Usuarios.Size = New System.Drawing.Size(66, 17)
        Me.Usuarios.TabIndex = 56
        Me.Usuarios.TabStop = True
        Me.Usuarios.Text = "Usuarios"
        Me.Usuarios.UseVisualStyleBackColor = True
        '
        'Paquetes
        '
        Me.Paquetes.AutoSize = True
        Me.Paquetes.Location = New System.Drawing.Point(82, 42)
        Me.Paquetes.Name = "Paquetes"
        Me.Paquetes.Size = New System.Drawing.Size(70, 17)
        Me.Paquetes.TabIndex = 55
        Me.Paquetes.TabStop = True
        Me.Paquetes.Text = "Paquetes"
        Me.Paquetes.UseVisualStyleBackColor = True
        '
        'Casa
        '
        Me.Casa.AutoSize = True
        Me.Casa.Location = New System.Drawing.Point(6, 19)
        Me.Casa.Name = "Casa"
        Me.Casa.Size = New System.Drawing.Size(54, 17)
        Me.Casa.TabIndex = 0
        Me.Casa.TabStop = True
        Me.Casa.Text = "Casas"
        Me.Casa.UseVisualStyleBackColor = True
        '
        'Veiculos
        '
        Me.Veiculos.AutoSize = True
        Me.Veiculos.Location = New System.Drawing.Point(6, 42)
        Me.Veiculos.Name = "Veiculos"
        Me.Veiculos.Size = New System.Drawing.Size(65, 17)
        Me.Veiculos.TabIndex = 2
        Me.Veiculos.TabStop = True
        Me.Veiculos.Text = "Veiculos"
        Me.Veiculos.UseVisualStyleBackColor = True
        '
        'Apartamentos
        '
        Me.Apartamentos.AutoSize = True
        Me.Apartamentos.Location = New System.Drawing.Point(82, 19)
        Me.Apartamentos.Name = "Apartamentos"
        Me.Apartamentos.Size = New System.Drawing.Size(90, 17)
        Me.Apartamentos.TabIndex = 2
        Me.Apartamentos.TabStop = True
        Me.Apartamentos.Text = "Apartamentos"
        Me.Apartamentos.UseVisualStyleBackColor = True
        '
        'DataGridView1
        '
        Me.DataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DataGridView1.Location = New System.Drawing.Point(19, 267)
        Me.DataGridView1.Name = "DataGridView1"
        Me.DataGridView1.Size = New System.Drawing.Size(778, 156)
        Me.DataGridView1.TabIndex = 2
        '
        'Tramitar
        '
        Me.Tramitar.Location = New System.Drawing.Point(19, 454)
        Me.Tramitar.Name = "Tramitar"
        Me.Tramitar.Size = New System.Drawing.Size(712, 25)
        Me.Tramitar.TabIndex = 3
        Me.Tramitar.Text = "Tramitar"
        Me.Tramitar.UseVisualStyleBackColor = True
        '
        'Button1
        '
        Me.Button1.Location = New System.Drawing.Point(737, 429)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(60, 50)
        Me.Button1.TabIndex = 4
        Me.Button1.Text = "Graficas"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Transaccion
        '
        Me.Transaccion.Controls.Add(Me.Alquilartermino)
        Me.Transaccion.Controls.Add(Me.Prereservar)
        Me.Transaccion.Controls.Add(Me.Reservar)
        Me.Transaccion.Controls.Add(Me.Comprar)
        Me.Transaccion.Controls.Add(Me.Alquilar)
        Me.Transaccion.Location = New System.Drawing.Point(13, 134)
        Me.Transaccion.Name = "Transaccion"
        Me.Transaccion.Size = New System.Drawing.Size(171, 97)
        Me.Transaccion.TabIndex = 5
        Me.Transaccion.TabStop = False
        Me.Transaccion.Text = "Transaccion"
        '
        'Alquilartermino
        '
        Me.Alquilartermino.AutoSize = True
        Me.Alquilartermino.Location = New System.Drawing.Point(6, 66)
        Me.Alquilartermino.Name = "Alquilartermino"
        Me.Alquilartermino.Size = New System.Drawing.Size(105, 17)
        Me.Alquilartermino.TabIndex = 10
        Me.Alquilartermino.TabStop = True
        Me.Alquilartermino.Text = "Alquilar a termino"
        Me.Alquilartermino.UseVisualStyleBackColor = True
        '
        'Prereservar
        '
        Me.Prereservar.AutoSize = True
        Me.Prereservar.Location = New System.Drawing.Point(82, 43)
        Me.Prereservar.Name = "Prereservar"
        Me.Prereservar.Size = New System.Drawing.Size(79, 17)
        Me.Prereservar.TabIndex = 9
        Me.Prereservar.TabStop = True
        Me.Prereservar.Text = "Prereservar"
        Me.Prereservar.UseVisualStyleBackColor = True
        '
        'Reservar
        '
        Me.Reservar.AutoSize = True
        Me.Reservar.Location = New System.Drawing.Point(8, 43)
        Me.Reservar.Name = "Reservar"
        Me.Reservar.Size = New System.Drawing.Size(68, 17)
        Me.Reservar.TabIndex = 8
        Me.Reservar.TabStop = True
        Me.Reservar.Text = "Reservar"
        Me.Reservar.UseVisualStyleBackColor = True
        '
        'Comprar
        '
        Me.Comprar.AutoSize = True
        Me.Comprar.Location = New System.Drawing.Point(8, 19)
        Me.Comprar.Name = "Comprar"
        Me.Comprar.Size = New System.Drawing.Size(64, 17)
        Me.Comprar.TabIndex = 6
        Me.Comprar.TabStop = True
        Me.Comprar.Text = "Comprar"
        Me.Comprar.UseVisualStyleBackColor = True
        '
        'Alquilar
        '
        Me.Alquilar.AutoSize = True
        Me.Alquilar.Location = New System.Drawing.Point(82, 19)
        Me.Alquilar.Name = "Alquilar"
        Me.Alquilar.Size = New System.Drawing.Size(59, 17)
        Me.Alquilar.TabIndex = 7
        Me.Alquilar.TabStop = True
        Me.Alquilar.Text = "Alquilar"
        Me.Alquilar.UseVisualStyleBackColor = True
        '
        'Buscar
        '
        Me.Buscar.Location = New System.Drawing.Point(19, 429)
        Me.Buscar.Name = "Buscar"
        Me.Buscar.Size = New System.Drawing.Size(712, 23)
        Me.Buscar.TabIndex = 6
        Me.Buscar.Text = "Buscar"
        Me.Buscar.UseVisualStyleBackColor = True
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(190, 75)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(39, 13)
        Me.Label3.TabIndex = 30
        Me.Label3.Text = "Label3"
        Me.Label3.Visible = False
        '
        'TextBox3
        '
        Me.TextBox3.Location = New System.Drawing.Point(308, 72)
        Me.TextBox3.Name = "TextBox3"
        Me.TextBox3.Size = New System.Drawing.Size(213, 20)
        Me.TextBox3.TabIndex = 29
        Me.TextBox3.Visible = False
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(190, 96)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(39, 13)
        Me.Label4.TabIndex = 28
        Me.Label4.Text = "Label4"
        Me.Label4.Visible = False
        '
        'TextBox4
        '
        Me.TextBox4.Location = New System.Drawing.Point(308, 93)
        Me.TextBox4.Name = "TextBox4"
        Me.TextBox4.Size = New System.Drawing.Size(213, 20)
        Me.TextBox4.TabIndex = 27
        Me.TextBox4.Visible = False
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(190, 55)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(39, 13)
        Me.Label2.TabIndex = 26
        Me.Label2.Text = "Label2"
        Me.Label2.Visible = False
        '
        'TextBox2
        '
        Me.TextBox2.Location = New System.Drawing.Point(308, 52)
        Me.TextBox2.Name = "TextBox2"
        Me.TextBox2.Size = New System.Drawing.Size(213, 20)
        Me.TextBox2.TabIndex = 25
        Me.TextBox2.Visible = False
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(190, 37)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(39, 13)
        Me.Label1.TabIndex = 24
        Me.Label1.Text = "Label1"
        Me.Label1.Visible = False
        '
        'TextBox1
        '
        Me.TextBox1.Location = New System.Drawing.Point(308, 34)
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.Size = New System.Drawing.Size(213, 20)
        Me.TextBox1.TabIndex = 23
        Me.TextBox1.Visible = False
        '
        'CheckBox1
        '
        Me.CheckBox1.AutoSize = True
        Me.CheckBox1.Location = New System.Drawing.Point(21, 244)
        Me.CheckBox1.Name = "CheckBox1"
        Me.CheckBox1.Size = New System.Drawing.Size(81, 17)
        Me.CheckBox1.TabIndex = 39
        Me.CheckBox1.Text = "CheckBox1"
        Me.CheckBox1.UseVisualStyleBackColor = True
        Me.CheckBox1.Visible = False
        '
        'CheckBox2
        '
        Me.CheckBox2.AutoSize = True
        Me.CheckBox2.Location = New System.Drawing.Point(193, 244)
        Me.CheckBox2.Name = "CheckBox2"
        Me.CheckBox2.Size = New System.Drawing.Size(81, 17)
        Me.CheckBox2.TabIndex = 40
        Me.CheckBox2.Text = "CheckBox2"
        Me.CheckBox2.UseVisualStyleBackColor = True
        Me.CheckBox2.Visible = False
        '
        'CheckBox3
        '
        Me.CheckBox3.AutoSize = True
        Me.CheckBox3.Location = New System.Drawing.Point(367, 244)
        Me.CheckBox3.Name = "CheckBox3"
        Me.CheckBox3.Size = New System.Drawing.Size(81, 17)
        Me.CheckBox3.TabIndex = 41
        Me.CheckBox3.Text = "CheckBox3"
        Me.CheckBox3.UseVisualStyleBackColor = True
        Me.CheckBox3.Visible = False
        '
        'CheckBox4
        '
        Me.CheckBox4.AutoSize = True
        Me.CheckBox4.Location = New System.Drawing.Point(541, 244)
        Me.CheckBox4.Name = "CheckBox4"
        Me.CheckBox4.Size = New System.Drawing.Size(81, 17)
        Me.CheckBox4.TabIndex = 42
        Me.CheckBox4.Text = "CheckBox4"
        Me.CheckBox4.UseVisualStyleBackColor = True
        Me.CheckBox4.Visible = False
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Location = New System.Drawing.Point(190, 117)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(39, 13)
        Me.Label5.TabIndex = 46
        Me.Label5.Text = "Label5"
        Me.Label5.Visible = False
        '
        'TextBox5
        '
        Me.TextBox5.Location = New System.Drawing.Point(308, 114)
        Me.TextBox5.Name = "TextBox5"
        Me.TextBox5.Size = New System.Drawing.Size(213, 20)
        Me.TextBox5.TabIndex = 45
        Me.TextBox5.Visible = False
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(190, 137)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(39, 13)
        Me.Label6.TabIndex = 48
        Me.Label6.Text = "Label6"
        Me.Label6.Visible = False
        '
        'TextBox6
        '
        Me.TextBox6.Location = New System.Drawing.Point(308, 134)
        Me.TextBox6.Name = "TextBox6"
        Me.TextBox6.Size = New System.Drawing.Size(213, 20)
        Me.TextBox6.TabIndex = 47
        Me.TextBox6.Visible = False
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Location = New System.Drawing.Point(190, 156)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(39, 13)
        Me.Label7.TabIndex = 50
        Me.Label7.Text = "Label7"
        Me.Label7.Visible = False
        '
        'TextBox7
        '
        Me.TextBox7.Location = New System.Drawing.Point(308, 153)
        Me.TextBox7.Name = "TextBox7"
        Me.TextBox7.Size = New System.Drawing.Size(213, 20)
        Me.TextBox7.TabIndex = 49
        Me.TextBox7.Visible = False
        '
        'Label8
        '
        Me.Label8.AutoSize = True
        Me.Label8.Location = New System.Drawing.Point(190, 177)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(39, 13)
        Me.Label8.TabIndex = 52
        Me.Label8.Text = "Label8"
        Me.Label8.Visible = False
        '
        'TextBox8
        '
        Me.TextBox8.Location = New System.Drawing.Point(308, 174)
        Me.TextBox8.Name = "TextBox8"
        Me.TextBox8.Size = New System.Drawing.Size(213, 20)
        Me.TextBox8.TabIndex = 51
        Me.TextBox8.Visible = False
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.Location = New System.Drawing.Point(190, 198)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(39, 13)
        Me.Label9.TabIndex = 54
        Me.Label9.Text = "Label9"
        Me.Label9.Visible = False
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.Location = New System.Drawing.Point(190, 218)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(45, 13)
        Me.Label10.TabIndex = 56
        Me.Label10.Text = "Label10"
        Me.Label10.Visible = False
        '
        'CheckBox5
        '
        Me.CheckBox5.AutoSize = True
        Me.CheckBox5.Location = New System.Drawing.Point(715, 244)
        Me.CheckBox5.Name = "CheckBox5"
        Me.CheckBox5.Size = New System.Drawing.Size(81, 17)
        Me.CheckBox5.TabIndex = 57
        Me.CheckBox5.Text = "CheckBox5"
        Me.CheckBox5.UseVisualStyleBackColor = True
        '
        'TextBox9
        '
        Me.TextBox9.Location = New System.Drawing.Point(308, 195)
        Me.TextBox9.Name = "TextBox9"
        Me.TextBox9.Size = New System.Drawing.Size(213, 20)
        Me.TextBox9.TabIndex = 53
        Me.TextBox9.Visible = False
        '
        'TextBox10
        '
        Me.TextBox10.Location = New System.Drawing.Point(308, 215)
        Me.TextBox10.Name = "TextBox10"
        Me.TextBox10.Size = New System.Drawing.Size(213, 20)
        Me.TextBox10.TabIndex = 55
        Me.TextBox10.Visible = False
        '
        'Label11
        '
        Me.Label11.AutoSize = True
        Me.Label11.Location = New System.Drawing.Point(527, 37)
        Me.Label11.Name = "Label11"
        Me.Label11.Size = New System.Drawing.Size(45, 13)
        Me.Label11.TabIndex = 61
        Me.Label11.Text = "Label11"
        Me.Label11.Visible = False
        '
        'TextBox11
        '
        Me.TextBox11.Location = New System.Drawing.Point(687, 34)
        Me.TextBox11.Name = "TextBox11"
        Me.TextBox11.Size = New System.Drawing.Size(110, 20)
        Me.TextBox11.TabIndex = 60
        Me.TextBox11.Visible = False
        '
        'Label12
        '
        Me.Label12.AutoSize = True
        Me.Label12.Location = New System.Drawing.Point(527, 58)
        Me.Label12.Name = "Label12"
        Me.Label12.Size = New System.Drawing.Size(45, 13)
        Me.Label12.TabIndex = 59
        Me.Label12.Text = "Label12"
        Me.Label12.Visible = False
        '
        'TextBox12
        '
        Me.TextBox12.Location = New System.Drawing.Point(687, 55)
        Me.TextBox12.Name = "TextBox12"
        Me.TextBox12.Size = New System.Drawing.Size(110, 20)
        Me.TextBox12.TabIndex = 58
        Me.TextBox12.Visible = False
        '
        'Label13
        '
        Me.Label13.AutoSize = True
        Me.Label13.Location = New System.Drawing.Point(527, 79)
        Me.Label13.Name = "Label13"
        Me.Label13.Size = New System.Drawing.Size(45, 13)
        Me.Label13.TabIndex = 63
        Me.Label13.Text = "Label13"
        Me.Label13.Visible = False
        '
        'TextBox13
        '
        Me.TextBox13.Location = New System.Drawing.Point(687, 76)
        Me.TextBox13.Name = "TextBox13"
        Me.TextBox13.Size = New System.Drawing.Size(110, 20)
        Me.TextBox13.TabIndex = 62
        Me.TextBox13.Visible = False
        '
        'Label14
        '
        Me.Label14.AutoSize = True
        Me.Label14.Location = New System.Drawing.Point(527, 96)
        Me.Label14.Name = "Label14"
        Me.Label14.Size = New System.Drawing.Size(45, 13)
        Me.Label14.TabIndex = 65
        Me.Label14.Text = "Label14"
        Me.Label14.Visible = False
        '
        'TextBox14
        '
        Me.TextBox14.Location = New System.Drawing.Point(687, 93)
        Me.TextBox14.Name = "TextBox14"
        Me.TextBox14.Size = New System.Drawing.Size(110, 20)
        Me.TextBox14.TabIndex = 64
        Me.TextBox14.Visible = False
        '
        'Graficas
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(809, 484)
        Me.Controls.Add(Me.Label14)
        Me.Controls.Add(Me.TextBox14)
        Me.Controls.Add(Me.Label13)
        Me.Controls.Add(Me.TextBox13)
        Me.Controls.Add(Me.Label11)
        Me.Controls.Add(Me.TextBox11)
        Me.Controls.Add(Me.Label12)
        Me.Controls.Add(Me.TextBox12)
        Me.Controls.Add(Me.CheckBox5)
        Me.Controls.Add(Me.Label10)
        Me.Controls.Add(Me.TextBox10)
        Me.Controls.Add(Me.Label9)
        Me.Controls.Add(Me.TextBox9)
        Me.Controls.Add(Me.Label8)
        Me.Controls.Add(Me.TextBox8)
        Me.Controls.Add(Me.Label7)
        Me.Controls.Add(Me.TextBox7)
        Me.Controls.Add(Me.Label6)
        Me.Controls.Add(Me.TextBox6)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.TextBox5)
        Me.Controls.Add(Me.CheckBox4)
        Me.Controls.Add(Me.CheckBox3)
        Me.Controls.Add(Me.CheckBox2)
        Me.Controls.Add(Me.CheckBox1)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.TextBox3)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.TextBox4)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.TextBox2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.TextBox1)
        Me.Controls.Add(Me.Buscar)
        Me.Controls.Add(Me.Transaccion)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.Tramitar)
        Me.Controls.Add(Me.DataGridView1)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.MenuStrip1)
        Me.MainMenuStrip = Me.MenuStrip1
        Me.Name = "Graficas"
        Me.Text = "ScriptLogic"
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.Transaccion.ResumeLayout(False)
        Me.Transaccion.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents MenuStrip1 As MenuStrip
    Friend WithEvents CrearToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents CasaToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ApartamentoToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents VeiculosToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents GroupBox1 As GroupBox
    Friend WithEvents Casa As RadioButton
    Friend WithEvents Veiculos As RadioButton
    Friend WithEvents Apartamentos As RadioButton
    Friend WithEvents DataGridView1 As DataGridView
    Friend WithEvents Tramitar As Button
    Friend WithEvents Button1 As Button
    Friend WithEvents Transaccion As GroupBox
    Friend WithEvents Prereservar As RadioButton
    Friend WithEvents Reservar As RadioButton
    Friend WithEvents Comprar As RadioButton
    Friend WithEvents Alquilar As RadioButton
    Friend WithEvents Buscar As Button
    Friend WithEvents Label3 As Label
    Friend WithEvents TextBox3 As TextBox
    Friend WithEvents Label4 As Label
    Friend WithEvents TextBox4 As TextBox
    Friend WithEvents Label2 As Label
    Friend WithEvents TextBox2 As TextBox
    Friend WithEvents Label1 As Label
    Friend WithEvents TextBox1 As TextBox
    Friend WithEvents CheckBox1 As CheckBox
    Friend WithEvents CheckBox2 As CheckBox
    Friend WithEvents CheckBox3 As CheckBox
    Friend WithEvents CheckBox4 As CheckBox
    Friend WithEvents Label5 As Label
    Friend WithEvents TextBox5 As TextBox
    Friend WithEvents Label6 As Label
    Friend WithEvents TextBox6 As TextBox
    Friend WithEvents Label7 As Label
    Friend WithEvents TextBox7 As TextBox
    Friend WithEvents Label8 As Label
    Friend WithEvents TextBox8 As TextBox
    Friend WithEvents Label9 As Label
    Friend WithEvents Paquetes As RadioButton
    Friend WithEvents Label10 As Label
    Friend WithEvents CheckBox5 As CheckBox
    Friend WithEvents Usuarios As RadioButton
    Friend WithEvents Alquilartermino As RadioButton
    Friend WithEvents TextBox9 As TextBox
    Friend WithEvents TextBox10 As TextBox
    Friend WithEvents Label11 As Label
    Friend WithEvents TextBox11 As TextBox
    Friend WithEvents Label12 As Label
    Friend WithEvents TextBox12 As TextBox
    Friend WithEvents Label13 As Label
    Friend WithEvents TextBox13 As TextBox
    Friend WithEvents Label14 As Label
    Friend WithEvents TextBox14 As TextBox
    Friend WithEvents SalirToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents CerrarSecionToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents SalirToolStripMenuItem1 As ToolStripMenuItem
End Class
