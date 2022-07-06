<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FrmCurvaSPresProy
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
        Me.components = New System.ComponentModel.Container()
        Dim ReportDataSource3 As Microsoft.Reporting.WinForms.ReportDataSource = New Microsoft.Reporting.WinForms.ReportDataSource()
        Dim PaintStyle7 As Klik.Windows.Forms.v1.Common.PaintStyle = New Klik.Windows.Forms.v1.Common.PaintStyle()
        Dim PaintStyle8 As Klik.Windows.Forms.v1.Common.PaintStyle = New Klik.Windows.Forms.v1.Common.PaintStyle()
        Dim PaintStyle9 As Klik.Windows.Forms.v1.Common.PaintStyle = New Klik.Windows.Forms.v1.Common.PaintStyle()
        Me.contenedor = New Klik.Windows.Forms.v1.EntryLib.ELRichPanel()
        Me.rptgeneral = New Microsoft.Reporting.WinForms.ReportViewer()
        Me.btcerrar = New Klik.Windows.Forms.v1.EntryLib.ELButton()
        Me.ElPanel1 = New Klik.Windows.Forms.v1.EntryLib.ELPanel()
        Me.cbproyecto = New System.Windows.Forms.ComboBox()
        Me.ElLabel3 = New Klik.Windows.Forms.v1.EntryLib.ELLabel()
        Me.btbuscar = New Klik.Windows.Forms.v1.EntryLib.ELButton()
        Me.dtfecfin = New System.Windows.Forms.DateTimePicker()
        Me.ElLabel2 = New Klik.Windows.Forms.v1.EntryLib.ELLabel()
        Me.dtfecini = New System.Windows.Forms.DateTimePicker()
        Me.ElLabel1 = New Klik.Windows.Forms.v1.EntryLib.ELLabel()
        Me.KFormManager1 = New Klik.Windows.Forms.v1.Common.KFormManager(Me.components)
        Me.DSFonConvenio = New CtrlConvenios.DSFonConvenio()
        Me.PA_CURVA_S_PRESUPUESTO_X_PROYECTOBindingSource = New System.Windows.Forms.BindingSource(Me.components)
        Me.PA_CURVA_S_PRESUPUESTO_X_PROYECTOTableAdapter = New CtrlConvenios.DSFonConvenioTableAdapters.PA_CURVA_S_PRESUPUESTO_X_PROYECTOTableAdapter()
        Me.GetAllProyectosBindingSource = New System.Windows.Forms.BindingSource(Me.components)
        Me.GetAllProyectosTableAdapter = New CtrlConvenios.DSFonConvenioTableAdapters.GetAllProyectosTableAdapter()
        CType(Me.contenedor, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.contenedor.SuspendLayout()
        CType(Me.btcerrar, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.ElPanel1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ElPanel1.SuspendLayout()
        CType(Me.ElLabel3, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.btbuscar, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.ElLabel2, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.ElLabel1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.KFormManager1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.DSFonConvenio, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PA_CURVA_S_PRESUPUESTO_X_PROYECTOBindingSource, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.GetAllProyectosBindingSource, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'contenedor
        '
        Me.contenedor.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.contenedor.ContainerStyle.BackgroundStyle.GradientAngle = 45.0!
        Me.contenedor.Controls.Add(Me.rptgeneral)
        Me.contenedor.Controls.Add(Me.btcerrar)
        Me.contenedor.Controls.Add(Me.ElPanel1)
        Me.contenedor.Expanded = True
        Me.contenedor.FooterStyle.BackgroundStyle.GradientAngle = 45.0!
        Me.contenedor.FooterStyle.FlashStyle.GradientAngle = 0.0!
        Me.contenedor.FooterStyle.FlashStyle.PaintType = Klik.Windows.Forms.v1.Common.PaintTypes.Solid
        Me.contenedor.FooterStyle.FlashStyle.SolidColor = System.Drawing.Color.FromArgb(CType(CType(253, Byte), Integer), CType(CType(240, Byte), Integer), CType(CType(191, Byte), Integer))
        Me.contenedor.FooterStyle.Height = 25
        Me.contenedor.HeaderStyle.FlashStyle.PaintType = Klik.Windows.Forms.v1.Common.PaintTypes.Solid
        Me.contenedor.HeaderStyle.FlashStyle.SolidColor = System.Drawing.Color.FromArgb(CType(CType(253, Byte), Integer), CType(CType(240, Byte), Integer), CType(CType(191, Byte), Integer))
        Me.contenedor.HeaderStyle.Height = 24
        Me.contenedor.HeaderStyle.TextStyle.Text = "Curva S Físico"
        Me.contenedor.Location = New System.Drawing.Point(12, 12)
        Me.contenedor.Name = "contenedor"
        Me.contenedor.Padding = New System.Windows.Forms.Padding(1, 24, 1, 25)
        Me.contenedor.Size = New System.Drawing.Size(900, 540)
        Me.contenedor.TabIndex = 1
        '
        'rptgeneral
        '
        Me.rptgeneral.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        ReportDataSource3.Name = "dscurvaspres"
        ReportDataSource3.Value = Me.PA_CURVA_S_PRESUPUESTO_X_PROYECTOBindingSource
        Me.rptgeneral.LocalReport.DataSources.Add(ReportDataSource3)
        Me.rptgeneral.LocalReport.ReportEmbeddedResource = "CtrlConvenios.rptcurvaSPresProy.rdlc"
        Me.rptgeneral.Location = New System.Drawing.Point(7, 67)
        Me.rptgeneral.Name = "rptgeneral"
        Me.rptgeneral.ShowBackButton = False
        Me.rptgeneral.ShowFindControls = False
        Me.rptgeneral.ShowPageNavigationControls = False
        Me.rptgeneral.ShowRefreshButton = False
        Me.rptgeneral.ShowStopButton = False
        Me.rptgeneral.Size = New System.Drawing.Size(889, 431)
        Me.rptgeneral.TabIndex = 3
        '
        'btcerrar
        '
        Me.btcerrar.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btcerrar.FlashStyle.PaintType = Klik.Windows.Forms.v1.Common.PaintTypes.Solid
        Me.btcerrar.FlashStyle.SolidColor = System.Drawing.Color.FromArgb(CType(CType(253, Byte), Integer), CType(CType(240, Byte), Integer), CType(CType(191, Byte), Integer))
        Me.btcerrar.ForegroundImageStyle.ImageAlign = System.Drawing.ContentAlignment.MiddleCenter
        Me.btcerrar.Location = New System.Drawing.Point(813, 504)
        Me.btcerrar.Name = "btcerrar"
        Me.btcerrar.Size = New System.Drawing.Size(75, 23)
        Me.btcerrar.TabIndex = 2
        Me.btcerrar.TextStyle.Text = "&Cerrar"
        Me.btcerrar.TextStyle.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ElPanel1
        '
        Me.ElPanel1.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ElPanel1.BackgroundStyle.GradientAngle = 45.0!
        Me.ElPanel1.Controls.Add(Me.cbproyecto)
        Me.ElPanel1.Controls.Add(Me.ElLabel3)
        Me.ElPanel1.Controls.Add(Me.btbuscar)
        Me.ElPanel1.Controls.Add(Me.dtfecfin)
        Me.ElPanel1.Controls.Add(Me.ElLabel2)
        Me.ElPanel1.Controls.Add(Me.dtfecini)
        Me.ElPanel1.Controls.Add(Me.ElLabel1)
        Me.ElPanel1.Location = New System.Drawing.Point(4, 27)
        Me.ElPanel1.Name = "ElPanel1"
        Me.ElPanel1.Size = New System.Drawing.Size(892, 34)
        Me.ElPanel1.TabIndex = 1
        '
        'cbproyecto
        '
        Me.cbproyecto.DataSource = Me.GetAllProyectosBindingSource
        Me.cbproyecto.DisplayMember = "Area"
        Me.cbproyecto.FormattingEnabled = True
        Me.cbproyecto.Location = New System.Drawing.Point(58, 5)
        Me.cbproyecto.Name = "cbproyecto"
        Me.cbproyecto.Size = New System.Drawing.Size(246, 21)
        Me.cbproyecto.TabIndex = 9
        Me.cbproyecto.ValueMember = "IdProyecto"
        '
        'ElLabel3
        '
        PaintStyle7.PaintType = Klik.Windows.Forms.v1.Common.PaintTypes.Solid
        PaintStyle7.SolidColor = System.Drawing.Color.FromArgb(CType(CType(253, Byte), Integer), CType(CType(240, Byte), Integer), CType(CType(191, Byte), Integer))
        Me.ElLabel3.FlashStyle = PaintStyle7
        Me.ElLabel3.Location = New System.Drawing.Point(3, 4)
        Me.ElLabel3.Name = "ElLabel3"
        Me.ElLabel3.Size = New System.Drawing.Size(310, 23)
        Me.ElLabel3.TabIndex = 8
        Me.ElLabel3.TabStop = False
        Me.ElLabel3.TextStyle.Text = "Proyecto:"
        '
        'btbuscar
        '
        Me.btbuscar.FlashStyle.PaintType = Klik.Windows.Forms.v1.Common.PaintTypes.Solid
        Me.btbuscar.FlashStyle.SolidColor = System.Drawing.Color.FromArgb(CType(CType(253, Byte), Integer), CType(CType(240, Byte), Integer), CType(CType(191, Byte), Integer))
        Me.btbuscar.ForegroundImageStyle.ImageAlign = System.Drawing.ContentAlignment.MiddleCenter
        Me.btbuscar.Location = New System.Drawing.Point(703, 3)
        Me.btbuscar.Name = "btbuscar"
        Me.btbuscar.Size = New System.Drawing.Size(75, 23)
        Me.btbuscar.TabIndex = 6
        Me.btbuscar.TextStyle.Text = "&Buscar"
        Me.btbuscar.TextStyle.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'dtfecfin
        '
        Me.dtfecfin.CustomFormat = "dd/MM/yyyy"
        Me.dtfecfin.Format = System.Windows.Forms.DateTimePickerFormat.Custom
        Me.dtfecfin.Location = New System.Drawing.Point(590, 5)
        Me.dtfecfin.Name = "dtfecfin"
        Me.dtfecfin.Size = New System.Drawing.Size(96, 20)
        Me.dtfecfin.TabIndex = 4
        '
        'ElLabel2
        '
        PaintStyle8.PaintType = Klik.Windows.Forms.v1.Common.PaintTypes.Solid
        PaintStyle8.SolidColor = System.Drawing.Color.FromArgb(CType(CType(253, Byte), Integer), CType(CType(240, Byte), Integer), CType(CType(191, Byte), Integer))
        Me.ElLabel2.FlashStyle = PaintStyle8
        Me.ElLabel2.Location = New System.Drawing.Point(502, 4)
        Me.ElLabel2.Name = "ElLabel2"
        Me.ElLabel2.Size = New System.Drawing.Size(195, 23)
        Me.ElLabel2.TabIndex = 5
        Me.ElLabel2.TabStop = False
        Me.ElLabel2.TextStyle.Text = "Fecha Término:"
        '
        'dtfecini
        '
        Me.dtfecini.CustomFormat = "dd/MM/yyyy"
        Me.dtfecini.Format = System.Windows.Forms.DateTimePickerFormat.Custom
        Me.dtfecini.Location = New System.Drawing.Point(392, 4)
        Me.dtfecini.Name = "dtfecini"
        Me.dtfecini.Size = New System.Drawing.Size(96, 20)
        Me.dtfecini.TabIndex = 2
        '
        'ElLabel1
        '
        PaintStyle9.PaintType = Klik.Windows.Forms.v1.Common.PaintTypes.Solid
        PaintStyle9.SolidColor = System.Drawing.Color.FromArgb(CType(CType(253, Byte), Integer), CType(CType(240, Byte), Integer), CType(CType(191, Byte), Integer))
        Me.ElLabel1.FlashStyle = PaintStyle9
        Me.ElLabel1.Location = New System.Drawing.Point(319, 3)
        Me.ElLabel1.Name = "ElLabel1"
        Me.ElLabel1.Size = New System.Drawing.Size(177, 23)
        Me.ElLabel1.TabIndex = 3
        Me.ElLabel1.TabStop = False
        Me.ElLabel1.TextStyle.Text = "Fecha Inicio:"
        '
        'KFormManager1
        '
        Me.KFormManager1.BorderShape.BottomLeft = Klik.Windows.Forms.v1.Common.BorderShapes.Rectangle
        Me.KFormManager1.BorderShape.BottomRight = Klik.Windows.Forms.v1.Common.BorderShapes.Rectangle
        Me.KFormManager1.MainContainer = Me
        '
        'DSFonConvenio
        '
        Me.DSFonConvenio.DataSetName = "DSFonConvenio"
        Me.DSFonConvenio.SchemaSerializationMode = System.Data.SchemaSerializationMode.IncludeSchema
        '
        'PA_CURVA_S_PRESUPUESTO_X_PROYECTOBindingSource
        '
        Me.PA_CURVA_S_PRESUPUESTO_X_PROYECTOBindingSource.DataMember = "PA_CURVA_S_PRESUPUESTO_X_PROYECTO"
        Me.PA_CURVA_S_PRESUPUESTO_X_PROYECTOBindingSource.DataSource = Me.DSFonConvenio
        '
        'PA_CURVA_S_PRESUPUESTO_X_PROYECTOTableAdapter
        '
        Me.PA_CURVA_S_PRESUPUESTO_X_PROYECTOTableAdapter.ClearBeforeFill = True
        '
        'GetAllProyectosBindingSource
        '
        Me.GetAllProyectosBindingSource.DataMember = "GetAllProyectos"
        Me.GetAllProyectosBindingSource.DataSource = Me.DSFonConvenio
        '
        'GetAllProyectosTableAdapter
        '
        Me.GetAllProyectosTableAdapter.ClearBeforeFill = True
        '
        'FrmCurvaSPresProy
        '
        Me.AcceptButton = Me.btbuscar
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(924, 564)
        Me.Controls.Add(Me.contenedor)
        Me.DoubleBuffered = True
        Me.Name = "FrmCurvaSPresProy"
        Me.Text = "FrmCurvaSPresProy"
        CType(Me.contenedor, System.ComponentModel.ISupportInitialize).EndInit()
        Me.contenedor.ResumeLayout(False)
        CType(Me.btcerrar, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.ElPanel1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ElPanel1.ResumeLayout(False)
        CType(Me.ElLabel3, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.btbuscar, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.ElLabel2, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.ElLabel1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.KFormManager1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.DSFonConvenio, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PA_CURVA_S_PRESUPUESTO_X_PROYECTOBindingSource, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.GetAllProyectosBindingSource, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents contenedor As Klik.Windows.Forms.v1.EntryLib.ELRichPanel
    Friend WithEvents rptgeneral As Microsoft.Reporting.WinForms.ReportViewer
    Friend WithEvents btcerrar As Klik.Windows.Forms.v1.EntryLib.ELButton
    Friend WithEvents ElPanel1 As Klik.Windows.Forms.v1.EntryLib.ELPanel
    Friend WithEvents cbproyecto As System.Windows.Forms.ComboBox
    Friend WithEvents ElLabel3 As Klik.Windows.Forms.v1.EntryLib.ELLabel
    Friend WithEvents btbuscar As Klik.Windows.Forms.v1.EntryLib.ELButton
    Friend WithEvents dtfecfin As System.Windows.Forms.DateTimePicker
    Friend WithEvents ElLabel2 As Klik.Windows.Forms.v1.EntryLib.ELLabel
    Friend WithEvents dtfecini As System.Windows.Forms.DateTimePicker
    Friend WithEvents ElLabel1 As Klik.Windows.Forms.v1.EntryLib.ELLabel
    Friend WithEvents KFormManager1 As Klik.Windows.Forms.v1.Common.KFormManager
    Friend WithEvents PA_CURVA_S_PRESUPUESTO_X_PROYECTOBindingSource As System.Windows.Forms.BindingSource
    Friend WithEvents DSFonConvenio As CtrlConvenios.DSFonConvenio
    Friend WithEvents PA_CURVA_S_PRESUPUESTO_X_PROYECTOTableAdapter As CtrlConvenios.DSFonConvenioTableAdapters.PA_CURVA_S_PRESUPUESTO_X_PROYECTOTableAdapter
    Friend WithEvents GetAllProyectosBindingSource As System.Windows.Forms.BindingSource
    Friend WithEvents GetAllProyectosTableAdapter As CtrlConvenios.DSFonConvenioTableAdapters.GetAllProyectosTableAdapter
End Class
