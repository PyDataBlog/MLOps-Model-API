Imports System
Imports System.Data
Imports System.Collections
Imports System.Web.UI
Imports Telerik.Web.UI
Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.BussinesLogic

Public Class FrmProyectoCCosto
    Inherits System.Web.UI.Page

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        
        If Not IsPostBack Then

            Me.BtnNueElem.Attributes.Add("onClick", "NuevoElem(" & Request.QueryString("IdProyecto") & "); return false;")

            Dim miId As String
            miId = Request.QueryString("IdProyecto")
            HFIdProyecto.Value = miId
            Dim _Proyecto As New Proyecto
            Dim _ProyectoBL As New ProyectoBL
            _Proyecto = _ProyectoBL.GetFromProyectoByIdProyecto(miId)
            LblProyecto.Text = _Proyecto.Nombre

        End If
    End Sub

    Protected Sub RadGrid1_ItemCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RadGrid1.ItemCommand
        If e.CommandName = "Select" Then
            Dim id As Int32
            Dim nombre As String
            Dim editeditem As GridEditableItem = CType(e.Item, GridEditableItem)
            id = editeditem.OwnerTableView.DataKeyValues(editeditem.ItemIndex)("IdProyAct")
            nombre = Me.RadGrid1.Items(editeditem.ItemIndex)("Actividad").Text
            TxtActividad.Text = nombre
            HFIdProyAct.Value = id.ToString
        End If
    End Sub

    Protected Sub RadGrid2_ItemCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RadGrid2.ItemCommand
        If e.CommandName = "Select" Then
            Dim id As Int32
            Dim nombre As String
            Dim editeditem As GridEditableItem = CType(e.Item, GridEditableItem)
            id = editeditem.OwnerTableView.DataKeyValues(editeditem.ItemIndex)("IdElemGasto")
            nombre = Me.RadGrid2.Items(editeditem.ItemIndex)("NomElemGasto").Text
            TxtElemGasto.Text = nombre
            TxtUnidad.Text = ""
            HFIdElemGasto.Value = id.ToString
        End If
    End Sub

    Protected Sub BtnBuscar_Click(ByVal sender As Object, ByVal e As EventArgs) Handles BtnBuscar.Click
        Panel1.Visible = True
        Panel2.Visible = True
        BtnBuscar.Visible = False
    End Sub

    Protected Sub RgGeneral_ItemCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RgGeneral.ItemCommand
        If e.CommandName = "Select" Then
            Dim id As Int32
            Dim editeditem As GridEditableItem = CType(e.Item, GridEditableItem)
            id = editeditem.OwnerTableView.DataKeyValues(editeditem.ItemIndex)("IdCCosto")
            Dim _CCosto As New CCosto
            Dim _CCostoBL As New CCostoBL
            _CCosto = _CCostoBL.GetCCOSTOByIdCCosto(id)
            TxtElemGasto.Text = _CCosto._ElementoGasto.NomElemGasto
            TxtActividad.Text = _CCosto._Actividad.Nombre
            TxtUnidad.Text = _CCosto.Unidad
            TxtPorcen.Text = _CCosto.Porcentaje
            HFIdCCosto.Value = id.ToString
            HFIdElemGasto.Value = _CCosto.IdElemGasto
            HFIdProyAct.Value = _CCosto.IdProyAct
            Panel1.Visible = True
            Panel2.Visible = True
            BtnBuscar.Visible = False
        End If
    End Sub

    Protected Sub BtnAceptar_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles BtnAceptar.Click
        If HFIdProyAct.Value = "" Then RWMensajes.RadAlert("Es necesario una Actividad", 200, 100, "Error", "") : Exit Sub
        If HFIdElemGasto.Value = "" Then RWMensajes.RadAlert("Es necesario un Elemento Gasto", 200, 100, "Error", "") : Exit Sub
        If TxtUnidad.Text = "" Then RWMensajes.RadAlert("Es necesario la unidad", 200, 100, "Error", "") : Exit Sub

        Dim _CCosto As New CCosto
        Dim _CCostoBL As New CCostoBL
        If HFIdCCosto.Value <> "" Then _CCosto.IdCCosto = Integer.Parse(HFIdCCosto.Value)
        _CCosto.IdElemGasto = CInt(HFIdElemGasto.Value)
        _CCosto.IdProyAct = CInt(HFIdProyAct.Value)
        _CCosto.Unidad = UCase(TxtUnidad.Text)
        _CCosto.Porcentaje = CDbl(TxtPorcen.Text)

        Dim mensaje As String
        mensaje = _CCostoBL.SaveCCosto(_CCosto)
        If mensaje = "" Then
            Panel2.Visible = False
            Panel1.Visible = False
            BtnBuscar.Visible = True
            RgGeneral.DataBind()
        Else
            RWMensajes.RadAlert(mensaje, 300, 100, "Error", "") : Exit Sub
        End If

    End Sub

    Protected Sub RgGeneral_DeleteCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RgGeneral.DeleteCommand
        Dim _CCosto As New CCosto
        Dim _CCostoBL As New CCostoBL
        Dim EditedItem As GridEditableItem
        EditedItem = CType(e.Item, GridEditableItem)
        Dim miError As String
        Dim miId As Int32
        miId = EditedItem.OwnerTableView.DataKeyValues(EditedItem.ItemIndex)("IdCCosto")
        miError = _CCostoBL.DeleteCCosto(miId)
        If miError <> "" Then
            RWMensajes.RadAlert(miError, 300, 120, "ERROR", "")
        End If
    End Sub

    Protected Sub Menu1_MenuItemClick(ByVal sender As Object, ByVal e As System.Web.UI.WebControls.MenuEventArgs) Handles Menu1.MenuItemClick
        If HFIdProyecto.Value = "" Then RWMensajes.RadAlert("Seleccione un proyecto", 230, 100, "Error", "") : Exit Sub
        Select Case e.Item.Value
            Case "Datos"
                Response.Redirect("~\Monitoreo\Formularios\FrmDatosProyecto.aspx?IdProyecto=" + HFIdProyecto.Value)

            Case "Convenio"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoConvenio.aspx?IdProyecto=" + HFIdProyecto.Value)

            Case "Componentes"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoComponentes.aspx?IdProyecto=" + HFIdProyecto.Value)

            Case "Actividades"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoActividad.aspx?IdProyecto=" + HFIdProyecto.Value)

            Case "CCostos"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoCCosto.aspx?IdProyecto=" + HFIdProyecto.Value)

            Case "Ambito"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoAmbito.aspx?IdProyecto=" + HFIdProyecto.Value)

            Case "Informes"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoInformes.aspx?IdProyecto=" + HFIdProyecto.Value)

            Case "ForFin"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoPresupuesto.aspx?IdProyecto=" + HFIdProyecto.Value)

            Case "ForAnual"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoPresupuestoAnual.aspx?IdProyecto=" + HFIdProyecto.Value)

            Case "AvanFis"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoAvanceFisico.aspx?IdProyecto=" + HFIdProyecto.Value)
        End Select
    End Sub

    Protected Sub BtnCancelar_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles BtnCancelar.Click
        Panel1.Visible = False
        Panel2.Visible = False
        BtnBuscar.Visible = True
    End Sub

    Protected Sub AJAX_MANAGER_AjaxRequest(ByVal sender As Object, ByVal e As Telerik.Web.UI.AjaxRequestEventArgs) Handles AJAX_MANAGER.AjaxRequest
        If e.Argument.StartsWith("ActualizarGrilla") Then
            Me.RadGrid2.DataBind()
        End If
    End Sub
End Class