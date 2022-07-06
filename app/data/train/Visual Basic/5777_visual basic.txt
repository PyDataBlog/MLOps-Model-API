Imports Telerik.Web.UI

Public Class FrmListaOrdenesCompraEnviar
    Inherits System.Web.UI.Page

    Public Function ObtenerColor(ByVal estado As String) As Drawing.Color
        Dim vista = New Disenio
        Return vista.ObtenerColorOrdenCompra(estado)
    End Function

    Public Function GetVisible(ByVal estado As String) As Boolean
        Dim visible As Boolean
        If estado = "Aprobado" Then
            visible = True
        Else
            visible = False
        End If
        Return visible
    End Function
    Public Function GetVisible2(ByVal estado As String) As Boolean
        Dim visible As Boolean
        If estado = "Enviado" Or estado = "Recepcionado" Or estado = "Recepcion Parc" Then
            visible = True
        Else
            visible = False
        End If
        Return visible
    End Function

    Protected Sub RadGrid1_ItemCreated(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridItemEventArgs) Handles RadGrid1.ItemCreated
        If TypeOf e.Item Is GridDataItem Then
            Dim imageButon As ImageButton = DirectCast(e.Item.FindControl("ibEnviar"), ImageButton)
            Dim imageButonImprimir As ImageButton = DirectCast(e.Item.FindControl("ibImprimir"), ImageButton)
            Dim idOrdenCompra = e.Item.OwnerTableView.DataKeyValues(e.Item.ItemIndex)("IdOrdenCompra")
            Dim tipo = e.Item.OwnerTableView.DataKeyValues(e.Item.ItemIndex)("Tipo")
            Dim ordenCompra = e.Item.OwnerTableView.DataKeyValues(e.Item.ItemIndex)("NumeroOrden") & " - " & e.Item.OwnerTableView.DataKeyValues(e.Item.ItemIndex)("Tipo")
            imageButon.Attributes.Add("onClick", "CargarBandejaOrdenCompraEnviar(" & idOrdenCompra & ", '" & ordenCompra & "'); return false;")
            imageButonImprimir.Attributes.Add("onClick", "CargarBandejaReporte(" & idOrdenCompra & ", '" & tipo & "'); return false;")
        End If
    End Sub

    Protected Sub RadAjaxManager1_AjaxRequest(ByVal sender As Object, ByVal e As Telerik.Web.UI.AjaxRequestEventArgs) Handles RadAjaxManager1.AjaxRequest
        If e.Argument = "UpdateRadGrid" Then
            Me.RadGrid1.DataBind()
        End If
    End Sub
End Class