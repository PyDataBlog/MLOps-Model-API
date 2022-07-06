Imports Capa_Entidad
Imports Controladores
Imports CrystalDecisions.CrystalReports.Engine
Imports CrystalDecisions.Shared
Imports CrystalDecisions.CrystalReports
Imports CrystalDecisions.ReportSource

Imports System.Windows.Forms
Imports System.Drawing.Printing
Imports System.IO

Public Class frmReportes
#Region "Variables"
    Private rd1 As ReportDocument
    Private sw As New Stopwatch
    Private terminado As Boolean = False
    Private control As New ReporteBL
    Private NroRegistros As Integer = 0
    Private config As New frmConfiguracionReporte
#End Region
#Region "Contructores"
    Public Sub New()

        ' Esta llamada es exigida por el diseñador.
        InitializeComponent()

        ' Agregue cualquier inicialización después de la llamada a InitializeComponent().

    End Sub
#End Region

    Enum filtros
        fecha = 0
        general = 1
        comprobante = 2
    End Enum
    Private Sub btnReporte_Click(sender As Object, e As EventArgs) Handles btnReporte.Click
        Dim env As Envio

        sw.Restart()
        tmrReporte.Start()
        pbReporte.Visible = True
        btnReporte.Enabled = False

        If config.rdEXCEL.Checked Then
            With opnReporte
                .Filter = ("Excel files |*.xls;*.xlsx")
                .FilterIndex = 4
            End With

            If opnReporte.ShowDialog() = DialogResult.OK Then
                sw.Stop()
                tmrReporte.Stop()
                env = New Envio(opnReporte.FileName,
                                Path.GetExtension(opnReporte.FileName),
                                config.txtNamExcel.Text,
                                New EnvioFormulario(chkAgrupado.Checked))
                env.empresa = frmMain.EmpresaMain
                env.control = control
                sw.Start()
                tmrReporte.Start()
            Else
                Exit Sub
            End If
        Else
            env = New Envio(frmMain.EmpresaMain,
                          New EnvioFormulario(chkAgrupado.Checked, txtDesde.Text, txtHasta.Text),
                          cboPeriodo.SelectedIndex,
                          NroRegistros)
            env.control = control

        End If

        bwReport.RunWorkerAsync(env)
    End Sub
    Private Sub frmReportes_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim fOption As FormOption = DirectCast(Me.Tag, FormOption)
        Me.Name = fOption.FullName
        Select Case fOption.Action
            Case 1
                Me.Text = "Reporte de Compras"
                control.Action = 1
            Case 2
                Me.Text = "Reporte de Ventas"
                control.Action = 2
            Case 3
                Me.Text = "Reporte Caja y Bancos"
        End Select

        cboPeriodo.SelectedIndex = Now.Month
        fecha(cboPeriodo.SelectedIndex)
    End Sub

    Sub fecha(p As Integer)
        Dim f As New Date
        Select Case p
            Case 0
                txtDesde.Text = "01/01/2016"
                txtHasta.Text = "01/01/2016"
            Case 1
                txtDesde.Text = "02/01/2016"
                txtHasta.Text = "29/01/2016"
            Case 2
                txtDesde.Text = "01/02/2016"
                txtHasta.Text = "31/02/2016"
            Case 3
                txtDesde.Text = "01/03/2016"
                txtHasta.Text = "31/03/2016"
            Case 4
                txtDesde.Text = "01/04/2016"
                txtHasta.Text = "30/04/2016"
            Case 5
                txtDesde.Text = "01/05/2016"
                txtHasta.Text = "31/05/2016"
            Case 6
                txtDesde.Text = "01/06/2016"
                txtHasta.Text = "31/06/2016"
            Case 7
                txtDesde.Text = "01/07/2016"
                txtHasta.Text = "30/07/2016"
            Case 8
                txtDesde.Text = "01/08/2016"
                txtHasta.Text = "31/08/2016"
            Case 9
                txtDesde.Text = "01/09/2016"
                txtHasta.Text = "30/09/2016"
            Case 10
                txtDesde.Text = "01/10/2016"
                txtHasta.Text = "31/10/2016"
            Case 11
                txtDesde.Text = "01/11/2016"
                txtHasta.Text = "30/11/2016"
            Case 12
                txtDesde.Text = "01/12/2016"
                txtHasta.Text = "31/12/2016"
            Case 13
                txtDesde.Text = "31/12/2016"
                txtHasta.Text = "31/12/2016"
        End Select
    End Sub
    Function PeriodoName(p As Integer) As String
        Dim f As New Date
        Dim name As String = ""
        Select Case p
            Case 0
                name = "APERTURA"
            Case 1
                name = "ENERO"
            Case 2
                name = "FEBRERO"
            Case 3
                name = "MARZO"
            Case 4
                name = "ABRIL"
            Case 5
                name = "MAYO"
            Case 6
                name = "JUNIO"
            Case 7
                name = "JULIO"
            Case 8
                name = "AGOSTO"
            Case 9
                name = "SEPTIEMBRE"
            Case 10
                name = "OCTUBRE"
            Case 11
                name = "NOVIEMBRE"
            Case 12
                name = "DICIEMBRE"
            Case 13
                name = "CIERRE"
        End Select
        Return name
    End Function
    Private Sub btnSalir_Click(sender As Object, e As EventArgs) Handles btnSalir.Click
        Me.Close()
    End Sub


    Private Sub bwReport_DoWork(sender As Object, e As System.ComponentModel.DoWorkEventArgs) Handles bwReport.DoWork

        Dim env As Envio = CType(e.Argument, Envio)
        Dim objBLViso As ReporteBL = env.control

        Dim toPeriodo, toRUC, toRazonSocial As TextObject
        Dim rd As New ReportDocument
        Dim envio(2) As String
        Dim filtro As Integer

        If config.rdEXCEL.Checked Then

            objBLViso.CrearFormato(env.path, env.extension, env.sheetName)
        Else
            If rdFecha.Checked Then : filtro = filtros.fecha : End If
            If rdGeneral.Checked Then : filtro = filtros.general : End If
            If rdComprobante.Checked Then : filtro = filtros.comprobante : End If

            envio(0) = filtro.ToString
            envio(1) = env.envioFormulario.desde
            envio(2) = env.envioFormulario.hasta
            objBLViso.CrearFormato(envio)

        End If
        Exit Sub

        If env.envioFormulario.chkAgrupado Then
            Select Case objBLViso.Action
                Case ListasBL.listFormato.Compras
                    rd.Load(VisoEN.path & "File/Rpt/Formato81XGroupv2.rpt")
                Case ListasBL.listFormato.Ventas
                Case ListasBL.listFormato.Caja

            End Select

        Else
            Select Case objBLViso.Action
                Case ListasBL.listFormato.Compras
                    rd.Load(VisoEN.path & "File/Rpt/Formato8_1.rpt")
                Case ListasBL.listFormato.Ventas
                    rd.Load(VisoEN.path & "File/Rpt/Formato141.rpt")
                Case ListasBL.listFormato.Caja

            End Select


        End If

        toPeriodo = rd.ReportDefinition.ReportObjects("itoPeriodo")
        toRUC = rd.ReportDefinition.ReportObjects("itoRUC")
        toRazonSocial = rd.ReportDefinition.ReportObjects("itoRazonSocial")

        toPeriodo.Text = PeriodoName(env.periodo) & " en Nuevos Soles"
        toRUC.Text = env.empresa.RUC
        toRazonSocial.Text = env.empresa.Aliass
        env.nroRegistros = objBLViso.nrmRegistros
        env.rd = rd
        bwReport.ReportProgress(20, env)
    End Sub

    Private Sub bwReport_ProgressChanged(sender As Object, e As System.ComponentModel.ProgressChangedEventArgs) Handles bwReport.ProgressChanged

        Dim env As Envio = e.UserState
        rd1 = env.rd
        NroRegistros = env.nroRegistros
    End Sub

    Private Sub bwReport_RunWorkerCompleted(sender As Object, e As System.ComponentModel.RunWorkerCompletedEventArgs) Handles bwReport.RunWorkerCompleted
        Exit Sub
        Dim nroPag As Integer
        nroPag = rd1.FormatEngine.GetLastPageNumber(New CrystalDecisions.Shared.ReportPageRequestContext())
        lblNroPag.Text += " " & nroPag
        lblNroReg.Text += " " & NroRegistros


        If rbPantalla.Checked Then
            Dim frm As New frmReportePantalla(NroRegistros)
            frm.crvFormato8_1.ReportSource = rd1
            frm.crvFormato8_1.Zoom(200)
            terminado = True
            MsgBox("Completado")
            tmrReporte.Stop()
            frm.ShowDialog()
        End If

        If rbExcel.Checked Then
            If System.IO.Directory.Exists(config.txtDestinoExcel.Text) = False Then
                System.IO.Directory.CreateDirectory(config.txtDestinoExcel.Text)
            End If
            Dim expOpt As New ExportOptions
            expOpt.ExportFormatType = ExportFormatType.Excel
            Dim expForOpt As ExcelFormatOptions = ExportOptions.CreateExcelFormatOptions()

            expForOpt.ConvertDateValuesToString = False
            expForOpt.ExcelAreaType = AreaSectionKind.Detail
            expForOpt.ExcelTabHasColumnHeadings = True
            expForOpt.ExcelUseConstantColumnWidth = False
            expForOpt.ShowGridLines = False
            expForOpt.UsePageRange = False

            expForOpt.ExportPageHeadersAndFooters = ExportPageAreaKind.OncePerReport
            expOpt.ExportFormatOptions = expForOpt
            expOpt.ExportDestinationType = ExportDestinationType.DiskFile

            Dim diskOpts As DiskFileDestinationOptions = ExportOptions.CreateDiskFileDestinationOptions()

            diskOpts.DiskFileName = config.txtDestinoExcel.Text &
                "\FormatoCompra_" & Now().ToString("dd-MM-yyyy") & ".xls"
            expOpt.ExportDestinationOptions = diskOpts
            Try

                rd1.Export(expOpt)
            Catch ex As IO.IOException
                tmrReporte.Stop()
                MsgBox("EL ARCHIVO ESTÁ SIENDO USANDO POR OTRO PROGRAMA")
            Catch ex As ExportException
                MsgBox(ex.ToString)
            Catch ex As Exception
                MsgBox(ex.ToString)
            End Try

            terminado = True
            MsgBox("Completado")
            tmrReporte.Stop()
            Debug.WriteLine("---------------")
            Debug.WriteLine(sw.Elapsed.ToString("g"))
        End If

        If rbPdf.Checked Then
            If System.IO.Directory.Exists(config.txtDestinoPDF.Text) = False Then
                Directory.CreateDirectory(config.txtDestinoPDF.Text)
            End If
            rd1.ExportToDisk(ExportFormatType.PortableDocFormat,
                                            config.txtDestinoPDF.Text &
                                            "FormatoCompra_" & Now().ToString("dd-MM-yyyy") & ".pdf")
            terminado = True
            MsgBox("Completado")
            tmrReporte.Stop()
        End If
        If rbImpresion.Checked Then

            rd1.PrintOptions.PrinterName = config.cboImpresora.SelectedItem.ToString
            rd1.PrintToPrinter(CInt(config.nmrCopias.Value), False, 0, 0)
            terminado = True
            MsgBox("Completado")
            tmrReporte.Stop()
        End If
        control.DeleteFile()
        rd1.Dispose()
        pbReporte.Value = 0
        pbReporte.Visible = False
        btnReporte.Enabled = True
        lblNroPag.Text = "Número de Paginas:"
        lblNroReg.Text = "Número de Registros:"
    End Sub
    Private Function CapturarDiaologo(msg As String, title As String, style As Microsoft.VisualBasic.MsgBoxStyle) As Microsoft.VisualBasic.MsgBoxResult
        Return MsgBox(msg, style, title)
    End Function

    Private Sub cboPeriodo_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cboPeriodo.SelectedIndexChanged
        fecha(cboPeriodo.SelectedIndex)
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles tmrReporte.Tick
        pbReporte.PerformStep()
        If pbReporte.Value = pbReporte.Maximum - 20 Then
            pbReporte.Maximum = 100
        End If
        If terminado Then
            Dim resto As Integer = pbReporte.Maximum - pbReporte.Value
            pbReporte.Increment(resto)
            terminado = False
        End If
    End Sub

    Private Sub frmReportes_FormClosing(sender As Object, e As FormClosingEventArgs) Handles MyBase.FormClosing
    End Sub

    Private Sub btnConfiguracion_Click(sender As Object, e As EventArgs) Handles btnConfiguracion.Click
        config.ShowDialog()
    End Sub

    Private Sub frmReportes_DoubleClick(sender As Object, e As EventArgs) Handles MyBase.DoubleClick
        MsgBox(Me.Name)
    End Sub
End Class

Class Envio
    Public empresa As Empresa
    Public periodo As String
    Public nroRegistros As Integer
    Public envioFormulario As EnvioFormulario
    Public rd As ReportDocument
    Public path As String
    Public extension As String
    Public sheetName As String
    Public control As ReporteBL
    Public Sub New(empresa As Empresa, envioFormulario As EnvioFormulario, periodo As String, nroRegistros As Integer)
        Me.empresa = empresa
        Me.periodo = periodo
        Me.nroRegistros = nroRegistros
        Me.envioFormulario = envioFormulario
    End Sub
    Public Sub New()

    End Sub
    Public Sub New(path As String, extension As String, sheetName As String, envioFormulario As EnvioFormulario)
        Me.path = path
        Me.extension = extension
        Me.sheetName = sheetName
        Me.envioFormulario = envioFormulario
    End Sub
End Class

Class EnvioFormulario

    Public chkAgrupado As Boolean
    Public desde As String
    Public hasta As String

    Public Sub New(chkAgrupado As Boolean, desde As String, hasta As String)
        Me.chkAgrupado = chkAgrupado
        Me.desde = desde
        Me.hasta = hasta
    End Sub
    Public Sub New(chkAgrupado As Boolean)
        Me.chkAgrupado = chkAgrupado
    End Sub
    Public Sub New()

    End Sub
End Class