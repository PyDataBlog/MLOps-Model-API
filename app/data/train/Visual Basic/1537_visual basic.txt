Imports System
Imports System.Configuration
Imports System.Data
Imports System.Data.SqlClient
Imports Telerik.Web.UI
Imports SistFoncreagro.BussinesLogic
Imports SistFoncreagro.BussinessEntities

Public Class WebForm1
    Inherits System.Web.UI.Page
    Private _dataItem As Object = Nothing
    Dim ConvenioBL As New ConvenioBL
    Dim _Convenio As Convenio
    Dim nConvenio As New Convenio
    Dim DesemBL As New DesembolsoBL
    Dim nDesem As New Desembolso
    Dim _Desem As Desembolso
    Dim Idconvenio As String

    Public Function ValidarFecha(ByVal Valor As Date) As String
        Dim MisFunciones As New Funciones
        Dim miFecha As String = MisFunciones.CampoFecha(Valor)
        Return miFecha
    End Function

    Public Function AnioFecha(ByVal Valor As Date) As String
        Dim miFecha As String = Year(Valor).ToString
        Return miFecha
    End Function

    Public Function ValidarNumero(ByVal Valor As Decimal) As String
        Dim MisFunciones As New Funciones
        Dim miNumero As String = MisFunciones.CampoNumero(Valor)
        Return miNumero
    End Function

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Dim miId As String
        miId = Request.QueryString("IdConvenio")
        HFIdConvenio.Value = miId
        Dim EsAdenda As String
        EsAdenda = Request.QueryString("A")

        If Not Page.IsPostBack Then
            Me.Button2.Attributes.Add("onClick", "NuevoConveMarco(" & Request.QueryString("IdConvenio") & "); return false;")
        End If

        If EsAdenda = "AD" Then
            RadTabStrip1.SelectedIndex = 2
            Datos.SelectedIndex = 2
            Exit Sub
        End If

        If Not Me.Page.IsPostBack Then
            RadTabStrip1.SelectedIndex = 0
            Datos.SelectedIndex = 0
            If miId = "0" Then
                'campos vacios
                TxtNombre.Text = ""
                TxtCodigo.Text = ""
                TxtMontoIni.Text = ""
                CbFase.Text = ""
                CbEstado.Text = ""
                DtFechaFirma.SelectedDate = DateValue("01/01/1985")
                DTInicio.SelectedDate = DateValue("01/01/1985")
                DTFin.SelectedDate = DateValue("01/01/1985")
                BtnGrabar.Visible = True
                BtnActualizar.Visible = False
                Ver_ConvenioMarco(False)
                ChConvenioMarco.Checked = False
            Else
                Call Llenar_Datos_Convenio(miId)
                BtnGrabar.Visible = False
                BtnActualizar.Visible = True
                TxtMontoIni.Enabled = False
                DtFechaFirma.Enabled = False
                DTInicio.Enabled = False
                DTFin.Enabled = False
            End If
        End If
    End Sub

    Private Sub Llenar_Datos_Convenio(ByVal mIdConvenio As String)
        'nConvenio = New Convenio
        nConvenio = ConvenioBL.GetFromConvenioByIdConvenio(mIdConvenio)

        TxtCodigo.Text = nConvenio.Codigo
        TxtNombre.Text = nConvenio.Nombre
        TxtMontoIni.Text = nConvenio.MontoInicial
        CbFase.Text = nConvenio.FaseCont
        CbEstado.Text = nConvenio.Estado
        If DateValue(nConvenio.FechaFirma) = ("01/01/0001") Then DtFechaFirma.SelectedDate = "01/01/1985" Else DtFechaFirma.SelectedDate = DateValue(nConvenio.FechaFirma)
        If DateValue(nConvenio.FecIni) = ("01/01/0001") Then DTInicio.SelectedDate = "01/01/1985" Else DTInicio.SelectedDate = DateValue(nConvenio.FecIni)
        If DateValue(nConvenio.FecFin) = ("01/01/0001") Then DTFin.SelectedDate = "01/01/1985" Else DTFin.SelectedDate = DateValue(nConvenio.FecFin)
        If nConvenio.IdConveMarco = 0 Then
            ChConvenioMarco.Checked = False
            Ver_ConvenioMarco(False)
        Else
            CbConveMarco.SelectedValue = nConvenio.IdConveMarco
            ChConvenioMarco.Checked = True
            Ver_ConvenioMarco(True)
        End If

    End Sub

    Protected Sub BtnGrabar_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles BtnGrabar.Click
        Call Grabar_Datos_Convenio()
        Response.Redirect("~\Monitoreo\Formularios\FrmBuscarConvenio.aspx")
    End Sub

    Private Sub Grabar_Datos_Convenio()
        If TxtCodigo.Text = "" Then MsgBox("Ingrese el código del convenio", vbCritical, "Error") : Exit Sub
        If TxtNombre.Text = "" Then MsgBox("Ingrese el nombre del convenio", vbCritical, "Error") : Exit Sub
        If DateValue(DTFin.SelectedDate) < DateValue(DTInicio.SelectedDate) Then MsgBox("Las fechas de inicio y fin son incorrectas", vbCritical, "Error") : Exit Sub
        If DateValue(DtFechaFirma.SelectedDate) > DateValue(DTInicio.SelectedDate) Then MsgBox("Las fechas de inicio y firma son incorrectas", vbCritical, "Error") : Exit Sub

        Dim miId As Int32

        nConvenio.Codigo = UCase(TxtCodigo.Text)
        nConvenio.Nombre = UCase(TxtNombre.Text)
        If Len(DTInicio.SelectedDate) = 0 Then nConvenio.FecIni = DateValue("01/01/1985") Else nConvenio.FecIni = DateValue(DTInicio.SelectedDate)
        If Len(DTFin.SelectedDate) = 0 Then nConvenio.FecFin = DateValue("01/01/1985") Else nConvenio.FecFin = DateValue(DTFin.SelectedDate)
        If Len(DtFechaFirma.SelectedDate) = 0 Then nConvenio.FechaFirma = DateValue("01/01/1985") Else nConvenio.FechaFirma = DateValue(DtFechaFirma.SelectedDate)
        nConvenio.IdMoneda = CbMoneda.SelectedValue
        nConvenio.MontoInicial = CDbl(TxtMontoIni.Text)
        nConvenio.IdTipConv = CbTipoConvenio.SelectedValue
        nConvenio.FaseCont = UCase(CbFase.Text)
        nConvenio.Estado = UCase(CbEstado.Text)
        If ChConvenioMarco.Checked = True Then nConvenio.IdConveMarco = CInt(CbConveMarco.SelectedValue) Else nConvenio.IdConveMarco = 0
        miId = ConvenioBL.SaveConvenio(nConvenio)
        HFIdConvenio.Value = miId.ToString
    End Sub

    Protected Sub BtnActualizar_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles BtnActualizar.Click
        Dim miId As String
        miId = Request.QueryString("IdConvenio")
        nConvenio = ConvenioBL.GetFromConvenioByIdConvenio(miId)

        nConvenio.IdConvenio = miId
        nConvenio.Codigo = UCase(TxtCodigo.Text)
        nConvenio.Nombre = UCase(TxtNombre.Text)
        nConvenio.IdMoneda = CbMoneda.SelectedValue
        nConvenio.MontoInicial = CDbl(TxtMontoIni.Text)
        nConvenio.IdTipConv = CbTipoConvenio.SelectedValue
        nConvenio.FaseCont = UCase(CbFase.Text)
        nConvenio.Estado = UCase(CbEstado.Text)
        If ChConvenioMarco.Checked = True Then nConvenio.IdConveMarco = CInt(CbConveMarco.SelectedValue) Else nConvenio.IdConveMarco = 0
        If Len(DTInicio.SelectedDate) = 0 Then nConvenio.FecIni = DateValue("01/01/1985") Else nConvenio.FecIni = DateValue(DTInicio.SelectedDate)
        If Len(DTFin.SelectedDate) = 0 Then nConvenio.FecFin = DateValue("01/01/1985") Else nConvenio.FecFin = DateValue(DTFin.SelectedDate)
        If Len(DtFechaFirma.SelectedDate) = 0 Then nConvenio.FechaFirma = DateValue("01/01/1985") Else nConvenio.FechaFirma = DateValue(DtFechaFirma.SelectedDate)
        ConvenioBL.SaveConvenio(nConvenio)
        Response.Redirect("~\Monitoreo\Formularios\FrmBuscarConvenio.aspx")
    End Sub

    Protected Sub RadTabStrip1_TabClick(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadTabStripEventArgs) Handles RadTabStrip1.TabClick
        Dim nTab, miId As String
        miId = Request.QueryString("IdConvenio")

        nTab = e.Tab.Text
        If nTab = "Datos Generales" And miId <> "0" Then
            Call Llenar_Datos_Convenio(miId)

        ElseIf nTab = "Desembolsos" Then
            RadTabStrip1.SelectedIndex = 1
            Datos.SelectedIndex = 1

        ElseIf nTab = "Adenda" Then
            RadTabStrip1.SelectedIndex = 2
            Datos.SelectedIndex = 2
            Call Mostrar_Adenda(miId)
        End If

    End Sub

    Private Sub Mostrar_Adenda(ByVal mIdConvenio)
        PanelAdenda.Visible = False
    End Sub

    Protected Sub BtnAceptarDesem_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles BtnAceptarDesem.Click
        Grabar_Desembolso("NUEVO")
    End Sub

    Private Sub Grabar_Desembolso(ByVal mAccion As String)
        Dim miId As String
        Dim idDesem As Int32
        miId = Request.QueryString("IdConvenio")

        nDesem.IdConvenio = miId
        'nDesem.Numero = CInt(TxtNumDesem.Text)
        If Len(DtFeDesem.SelectedDate) = 0 Then nDesem.FechaProg = "01/01/1985" Else nDesem.FechaProg = DateValue(DtFeDesem.SelectedDate)
        'If Len(DtFecEjec.SelectedDate) = 0 Then nDesem.FechaEjec = "01/01/1985" Else nDesem.FechaEjec = DateValue(DtFecEjec.SelectedDate)
        If TxtMontoDesem.Text <> "" Then nDesem.MontoProg = CDbl(TxtMontoDesem.Text)
        'If TxtMontoEjec.Text <> "" Then nDesem.MontoEjec = CDbl(TxtMontoEjec.Text)
        nDesem.Estado = UCase(CbEstadoDesem.Text)
        nDesem.IdProveedorCliente = Integer.Parse(CbInstitucionDesem.SelectedValue)
        If mAccion = "NUEVO" Then
            idDesem = DesemBL.SaveDesembolso(nDesem)
            TxtIdDesem.Text = idDesem
        ElseIf mAccion = "EDITAR" Then
            nDesem.IdDesembolso = CInt(TxtIdDesem.Text)
            DesemBL.SaveDesembolso(nDesem)
        End If
        RadGrid1.DataBind()
        PanelDesem.Visible = False
    End Sub

    Private Sub Limpiar_Campos_Desembolso()
        TxtNumDesem.Text = ""
        DtFeDesem.Clear()
        TxtMontoEjec.Text = ""
        DtFecEjec.Clear()
        TxtMontoDesem.Text = ""
        TxtIdDesem.Text = ""
    End Sub

    Private Sub Llenar_Datos_Desem(ByVal mIdDesem As Int32)
        _Desem = DesemBL.GetFromDesembolsoByIdDesembolso(mIdDesem)

        TxtNumDesem.Text = _Desem.Numero
        If DateValue(_Desem.FechaProg) <> "01/01/0001" Then DtFeDesem.SelectedDate = DateValue(_Desem.FechaProg) Else DtFeDesem.Clear()
        TxtMontoDesem.Text = _Desem.MontoProg
        'If DateValue(_Desem.FechaEjec) <> "01/01/0001" Then DtFecEjec.SelectedDate = DateValue(_Desem.FechaEjec) Else DtFecEjec.Clear()
        TxtMontoEjec.Text = _Desem.MontoEjec
        CbEstadoDesem.Text = _Desem.Estado

    End Sub

    Private Sub CbConveMarco_ItemDataBound(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemEventArgs) Handles CbConveMarco.ItemDataBound
        'e.Item.Text = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.ConveMarco)).Marco.ToString()
        'e.Item.Value = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.ConveMarco)).IdConveMarco.ToString()

    End Sub


    Private Sub CbConveMarco_ItemsRequested(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemsRequestedEventArgs) Handles CbConveMarco.ItemsRequested
        'If e.Text.Length > 3 Then

        '    Me.OdsConveMarco.SelectParameters("_Texto").DefaultValue = e.Text.ToString
        '    Me.CbConveMarco.DataBind()
        'End If
    End Sub

    Protected Sub AJAX_MANAGER_AjaxRequest(ByVal sender As Object, ByVal e As Telerik.Web.UI.AjaxRequestEventArgs) Handles AJAX_MANAGER.AjaxRequest
        If e.Argument.StartsWith("ActualizarCombo") Then
            Dim j() As String
            Dim idConveMarco As Int32
            j = Split(e.Argument, "|")

            idConveMarco = Convert.ToInt32(j(1))

            Me.CbConveMarco.DataBind()
            Me.CbConveMarco.SelectedValue = idConveMarco
        End If
    End Sub

    Protected Sub BtnNuevoDesem_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles BtnNuevoDesem.Click
        PanelDesem.Visible = True
        Limpiar_Campos_Desembolso()
        BtnActDesem.Visible = False
        BtnAceptarDesem.Visible = True
    End Sub

    Protected Sub BtnCancelDesem_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles BtnCancelDesem.Click
        PanelDesem.Visible = False
    End Sub

    Protected Sub BtnActDesem_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles BtnActDesem.Click
        Grabar_Desembolso("EDITAR")
    End Sub

    Protected Sub BtnNuevoAd_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles BtnNuevoAd.Click
        PanelAdenda.Visible = True
        Call Limpiar_Adenda()
    End Sub

    Private Sub Limpiar_Adenda()
        ChTiempo.Checked = False
        ChCosto.Checked = False
        ChAlcance.Checked = False
        PanelAdAlcance.Visible = False
        PanelAdCosto.Visible = False
        PanelAdTiempo.Visible = False
        PanelAdenda.Visible = True
        FileUploadControl.Visible = True
        TxtDescAd.Visible = True
        TxtIdAdenda.Text = ""
        TxtMontoAd.Text = ""
        DtFeFinAd.Clear()
        DtFirmaAd.Clear()
        DtFeFinVigAd.Clear()
        DtFeIniVigAd.Clear()
        TxtNumAd.Text = ""
        TxtJustif.Text = ""
        TxtRespAd.Text = ""
    End Sub

    Protected Sub BtnGrabarAd_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles BtnGrabarAd.Click  
        Dim miIdConvenio As String
        Dim idAdenda As Int32
        miIdConvenio = Request.QueryString("IdConvenio")
        Dim _AdendaBL As New AdendaBL
        Dim _Adenda As New Adenda

        _Adenda.IdConvenio = miIdConvenio
        _Adenda.Motivo = UCase(TxtJustif.Text)
        _Adenda.Responsable = UCase(TxtRespAd.Text)
        If Len(DtFeIniVigAd.SelectedDate) = 0 Then _Adenda.VigenciaIni = "01/01/1985" Else _Adenda.VigenciaIni = DateValue(DtFeIniVigAd.SelectedDate)
        If Len(DtFeFinVigAd.SelectedDate) = 0 Then _Adenda.VigenciaFin = "01/01/1985" Else _Adenda.VigenciaFin = DateValue(DtFeFinVigAd.SelectedDate)
        If Len(DtFirmaAd.SelectedDate) = 0 Then _Adenda.Fecha = "01/01/1985" Else _Adenda.Fecha = DateValue(DtFirmaAd.SelectedDate)
        _Adenda.Costo = ChCosto.Checked
        If TxtMontoAd.Text <> "" Then _Adenda.Monto = CDbl(TxtMontoAd.Text)
        _Adenda.Tiempo = ChTiempo.Checked
        If Len(DtFeFinAd.SelectedDate) = 0 Then _Adenda.FechaFin = "01/01/1985" Else _Adenda.FechaFin = DateValue(DtFeFinAd.SelectedDate)
        _Adenda.Alcance = ChAlcance.Checked
        If TxtIdAdenda.Text = "" Then
            idAdenda = _AdendaBL.SaveAdenda(_Adenda)
            TxtIdAdenda.Text = idAdenda
        ElseIf TxtIdAdenda.Text <> "" Then
            idAdenda = CInt(TxtIdAdenda.Text)
            _Adenda.IdAdenda = idAdenda
            _AdendaBL.SaveAdenda(_Adenda)
        End If

        If GridView3.Rows.Count = 0 And FileUploadControl.FileName <> "" Then
            If ChAlcance.Checked = True Then 
                SaveFile(FileUploadControl.PostedFile, "ADENDA", "Adenda", idAdenda, TxtDescAd.Text)
            end if
        End If
        GridView2.DataBind()
        PanelAdenda.Visible = False

    End Sub

    private Sub SaveFile(ByVal file As HttpPostedFile, ByVal mTabla As String, ByVal CarpetaDestino As String, ByVal mIdTabla As Int32, ByVal mDesArchivo As String)

        Dim savePath As String = Server.MapPath("~\Archivos\Monitoreo\" + CarpetaDestino + "\")

        Dim fileName As String = FileUploadControl.FileName

        Dim pathToCheck As String = savePath + fileName

        Dim tempfileName As String
        tempfileName = ""

        If (System.IO.File.Exists(pathToCheck)) Then
            Dim counter As Integer = 2
            While (System.IO.File.Exists(pathToCheck))
                tempfileName = counter.ToString() + fileName
                pathToCheck = savePath + tempfileName
                counter = counter + 1
            End While

            fileName = tempfileName
        End If
        savePath += fileName

        FileUploadControl.SaveAs(savePath)

        Adjunto_BD(fileName, 0, mTabla, mIdTabla, mDesArchivo)
    End Sub

    Private Sub Adjunto_BD(ByVal mRuta As String, ByVal mIdAjunto As Int32, ByVal mTabla As String, ByVal mIdTabla As Int32, ByVal mDesArchivo As String)
        Dim AdjuntoBL As New AdjMonitBL
        Dim mAdjunto As New AdjMonit
        Dim IdAdj As Integer
        If mIdAjunto <> 0 Then mAdjunto.IdAdjMonit = mIdAjunto

        mAdjunto.Id = mIdTabla
        mAdjunto.Tabla = mTabla
        mAdjunto.Ruta = mRuta
        mAdjunto.Descripcion = mDesArchivo
        mAdjunto.Fecha = DateValue(Date.Now)
        IdAdj = AdjuntoBL.SaveAdjMonit(mAdjunto)
    End Sub

    Protected Sub ChCosto_CheckedChanged(ByVal sender As Object, ByVal e As EventArgs) Handles ChCosto.CheckedChanged
        PanelAdCosto.Visible = ChCosto.Checked
    End Sub

    Protected Sub ChTiempo_CheckedChanged(ByVal sender As Object, ByVal e As EventArgs) Handles ChTiempo.CheckedChanged
        PanelAdTiempo.Visible = ChTiempo.Checked
    End Sub

    Protected Sub ChAlcance_CheckedChanged(ByVal sender As Object, ByVal e As EventArgs) Handles ChAlcance.CheckedChanged
        PanelAdAlcance.Visible = ChAlcance.Checked
    End Sub

    Protected Sub BtnCancelAd_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles BtnCancelAd.Click
        PanelAdenda.Visible = False
    End Sub

    Private Sub Llenar_Datos_Adenda(ByVal mIdAdenda As Int32)
        Dim _AdendaBL As New AdendaBL
        Dim _Adenda As New Adenda

        _Adenda = _AdendaBL.GetFromAdendaByIdAdenda(mIdAdenda)
        TxtNumAd.Text = _Adenda.Numero
        TxtJustif.Text = _Adenda.Motivo
        TxtRespAd.Text = _Adenda.Responsable
        If DateValue(_Adenda.Fecha) <> "01/01/0001" Then DtFirmaAd.SelectedDate = DateValue(_Adenda.Fecha) Else DtFirmaAd.Clear()
        If DateValue(_Adenda.VigenciaIni) <> "01/01/0001" Then DtFeIniVigAd.SelectedDate = DateValue(_Adenda.VigenciaIni) Else DtFeIniVigAd.Clear()
        If DateValue(_Adenda.VigenciaFin) <> "01/01/0001" Then DtFeFinVigAd.SelectedDate = DateValue(_Adenda.VigenciaFin) Else DtFeFinVigAd.Clear()
        ChCosto.Checked = _Adenda.Costo
        ChTiempo.Checked = _Adenda.Tiempo
        ChAlcance.Checked = _Adenda.Alcance
        PanelAdAlcance.Visible = ChAlcance.Checked
        PanelAdCosto.Visible = ChCosto.Checked
        PanelAdTiempo.Visible = ChTiempo.Checked
        If ChTiempo.Checked = True Then
            If DateValue(_Adenda.FechaFin) <> "01/01/0001" Then DtFeFinAd.SelectedDate = DateValue(_Adenda.FechaFin) Else DtFeFinAd.Clear()
        End If
        If ChCosto.Checked = True Then
            TxtMontoAd.Text = _Adenda.Monto
        End If
        If ChAlcance.Checked = True Then
            GridView3.DataBind()
            If GridView3.Rows.Count = 0 Then
                FileUploadControl.Visible = True
                TxtDescAd.Visible = True
            Else
                FileUploadControl.Visible = False
                TxtDescAd.Visible = False
            End If
        End If
    End Sub

    Protected Sub GridView3_RowEditing(ByVal sender As Object, ByVal e As System.Web.UI.WebControls.GridViewEditEventArgs) Handles GridView3.RowEditing
        Dim id As String
        id = GridView3.DataKeys(e.NewEditIndex).Values("IdAdjMonit").ToString()

        e.Cancel = True
        Dim _AdendaArchivoBL As New AdjMonitBL
        Dim _AdendaArchivo As New AdjMonit
        _AdendaArchivo = _AdendaArchivoBL.GetADJMONITByIdAdjMonit(id)
        Dim NomArch As String = _AdendaArchivo.Ruta
        Dim archivo As String = (Server.MapPath("~\Archivos\Monitoreo\Adenda\" + NomArch))

        Try
            My.Computer.FileSystem.DeleteFile(archivo, FileIO.UIOption.AllDialogs, FileIO.RecycleOption.SendToRecycleBin, FileIO.UICancelOption.DoNothing)
            Dim AdjuntoBL As New AdjMonitBL
            AdjuntoBL.DeleteAdjMonit(CInt(id))
            GridView3.DataBind()
        Catch ex As Exception
            MsgBox(ex.Message.ToString, MsgBoxStyle.Critical)
        End Try
    End Sub

    Protected Sub SelectedIndexChanged(ByVal sender As Object, ByVal e As EventArgs) Handles GridView2.SelectedIndexChanged, GridView3.SelectedIndexChanged
        PanelAdenda.Visible = True
        Dim id As String
        id = GridView2.SelectedDataKey.Value.ToString()
        If id.ToString <> TxtIdAdenda.Text Then
            TxtIdAdenda.Text = id
            Call Llenar_Datos_Adenda(CInt(id))
        End If
    End Sub

    Protected Sub SelectedIndexChanged1(ByVal sender As Object, ByVal e As EventArgs) Handles GridView3.SelectedIndexChanged, GridView3.SelectedIndexChanged, GridView3.SelectedIndexChanged, GridView3.SelectedIndexChanged, GridView3.SelectedIndexChanged, GridView3.SelectedIndexChanged, GridView3.SelectedIndexChanged, GridView3.SelectedIndexChanged

        Dim id As String
        id = GridView3.SelectedDataKey.Value.ToString()
        Dim NomArchivo As String = GridView3.Rows(GridView3.SelectedIndex).Cells(3).Text
        'Dim myFile As ProcessStartInfo
        'myFile = New ProcessStartInfo(Server.MapPath("~\Monitoreo\Archivos\Adenda\" + NomArchivo))
        'Process.Start(myFile)
        System.Diagnostics.Process.Start("~\Archivos\Monitoreo\Adenda\" + NomArchivo)
    End Sub

    Protected Sub BtnEliminarAd_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs)
        Dim BtnEliminar As ImageButton = CType(sender, ImageButton)
        If BtnEliminar.CommandName = "BorrarAd" Then
            Dim id As String

            id = BtnEliminar.CommandArgument

            Dim _AdendaArchivoBL As New AdjMonitBL
            Dim _AdendaArchivo As New List(Of AdjMonit)
            _AdendaArchivo = _AdendaArchivoBL.GetADJMONITByIdAndTabla(id, "ADENDA")

            If _AdendaArchivo.Count > 0 Then
                Dim NomArch As String = (Server.MapPath("~\Archivos\Monitoreo\Adenda\" + _AdendaArchivo(0).Ruta))
                'borra el archivo y el usuario confirma
                'My.Computer.FileSystem.DeleteFile(NomArch, FileIO.UIOption.AllDialogs, FileIO.RecycleOption.SendToRecycleBin, FileIO.UICancelOption.DoNothing)
                'Borra el archivo
                My.Computer.FileSystem.DeleteFile(NomArch)
            End If
            Dim _AdendaBL As New AdendaBL
            _AdendaBL.DeleteAdenda(id)
            GridView2.DataBind()
        End If
    End Sub


    Protected Sub ChConvenioMarco_CheckedChanged(ByVal sender As Object, ByVal e As EventArgs) Handles ChConvenioMarco.CheckedChanged
        Ver_ConvenioMarco(ChConvenioMarco.Checked)
    End Sub

    Private Sub Ver_ConvenioMarco(ByVal sw1 As Boolean)
        LblConvenioMarco.Visible = sw1
        CbConveMarco.Visible = sw1
        Button2.Visible = sw1
        RFVConvenioMarco.Visible = sw1
    End Sub

    Private Sub RGInstitucion_DeleteCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RGInstitucion.DeleteCommand
        Dim _InstitucionBL As New InstitucionBL
        Dim _Institucion As New Institucion
        Dim EditedItem As GridEditableItem
        EditedItem = CType(e.Item, GridEditableItem)
        Dim miId As Int32
        miId = EditedItem.OwnerTableView.DataKeyValues(EditedItem.ItemIndex)("IdInstitucion")
        _InstitucionBL.DeleteInstitucion(miId)
        RGInstitucion.DataBind()
    End Sub

    Protected Sub RGInstitucion_InsertCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RGInstitucion.InsertCommand
        Dim _InstitucionBL As New InstitucionBL
        Dim _Institucion As New Institucion
        Dim mUserControl As UserControl = CType(e.Item.FindControl(GridEditFormItem.EditFormUserControlID), UserControl)
        Dim idInstitucion As Int32
        Dim sw As Boolean
        sw = False
        If HFIdConvenio.Value = "0" Then
            Call Grabar_Datos_Convenio()
            sw = True
        End If
        _Institucion.IdConvenio = Integer.Parse(HFIdConvenio.Value)
        _Institucion.IdProveedorCliente = (CType(mUserControl.FindControl("CbInstitucion"), RadComboBox).SelectedValue)
        _Institucion.Tipo = (CType(mUserControl.FindControl("CbTipo"), DropDownList).SelectedValue)
        idInstitucion = _InstitucionBL.SaveInstitucion(_Institucion)
        If sw = False Then
            RGInstitucion.DataBind()
        Else
            Response.Redirect("~\Monitoreo\Formularios\FrmDatosConvenio.aspx?IdConvenio=" + HFIdConvenio.Value)
        End If
    End Sub

    Protected Sub RadGrid1_ItemCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RadGrid1.ItemCommand
        If e.CommandName = "Select" Then
            PanelDesem.Visible = True
            Dim id As String
            Dim editedItem As GridEditableItem = CType(e.Item, GridEditableItem)
            id = editedItem.OwnerTableView.DataKeyValues(editedItem.ItemIndex)("IdDesembolso")
            TxtIdDesem.Text = id
            Call Llenar_Datos_Desem(Integer.Parse(id))
            BtnAceptarDesem.Visible = False
            BtnActDesem.Visible = True
        End If
    End Sub

    Protected Sub RadGrid1_DeleteCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RadGrid1.DeleteCommand
        Dim id As Int32
        Dim editedItem As GridEditableItem = CType(e.Item, GridEditableItem)
        id = Integer.Parse(editedItem.OwnerTableView.DataKeyValues(editedItem.ItemIndex)("IdDesembolso"))
        Dim _DesemBL As New DesembolsoBL
        _DesemBL.DeleteDesembolso(id)
        RadGrid1.DataBind()
    End Sub

    Private Sub RGObs_DeleteCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RGObs.DeleteCommand
        Dim editedItem As GridEditableItem = CType(e.Item, GridEditableItem)
        Dim _Id As Int32
        _Id = Integer.Parse(editedItem.OwnerTableView.DataKeyValues(editedItem.ItemIndex)("IdObservacion"))
        Dim _ObsBL As New ObservacionBL
        _ObsBL.DeleteObservacion(_Id)
        RGObs.DataBind()
    End Sub

    Protected Sub RGObs_InsertCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RGObs.InsertCommand
        Dim mUserControl As UserControl = CType(e.Item.FindControl(GridEditFormItem.EditFormUserControlID), UserControl)
        Dim _Obs As New Observacion
        Dim _ObsBL As New ObservacionBL

        _Obs.Id = Integer.Parse(HFIdConvenio.Value)
        _Obs.Observacion = UCase(CType(mUserControl.FindControl("txtObs"), TextBox).Text)
        _Obs.Tabla = "CONVENIO"
        _ObsBL.InsertObservacion(_Obs)
        RGObs.DataBind()
    End Sub

    Private Sub RGObs_UpdateCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RGObs.UpdateCommand
        Dim mUserControl As UserControl = CType(e.Item.FindControl(GridEditFormItem.EditFormUserControlID), UserControl)
        Dim editedItem As GridEditableItem = CType(e.Item, GridEditableItem)
        Dim _Id As Int32
        Dim _Obs As String
        _Obs = UCase(CType(mUserControl.FindControl("txtObs"), TextBox).Text)
        _Id = Integer.Parse(editedItem.OwnerTableView.DataKeyValues(editedItem.ItemIndex)("IdObservacion"))
        Dim _ObsBL As New ObservacionBL
        _ObsBL.UpdateObservacion(_Id, _Obs)
        RGObs.DataBind()
    End Sub

    Protected Sub BtnVerAd_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs)

    End Sub
End Class