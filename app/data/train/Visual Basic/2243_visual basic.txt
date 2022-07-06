Imports Telerik.Web.UI
Imports SistFoncreagro.BussinesLogic
Imports SistFoncreagro.BussinessEntities

Public Class FrmProveedores
    Inherits System.Web.UI.Page

    Dim proveedorCliente As ProveedorCliente
    Dim proveedorBl As New ProveedorClienteBL
    Private _userControl As Object

    Protected Sub RadGrid2_ItemCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RadGrid2.ItemCommand
        If e.CommandName = "InitInsert" Then
            Dim cadena_java As String
            cadena_java = "<script type='text/javascript'> " & _
                                 " NuevoProveedor(); " & _
                                 Chr(60) & "/script>"
            ScriptManager.RegisterStartupScript(Page, GetType(Page), "Cerrar", cadena_java.ToString, False)
            e.Canceled = True
        ElseIf e.CommandName = "Anular" Then
            Dim editedItem As GridEditableItem = CType(e.Item, GridEditableItem)
            Dim IdProveedor As Int32 = editedItem.OwnerTableView.DataKeyValues(editedItem.ItemIndex)("IdProveedorCliente")
            proveedorBl.DeleteProveedorCliente(IdProveedor)
            Me.RadGrid2.DataBind()
        End If
    End Sub

    Protected Sub RadGrid2_ItemCreated(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridItemEventArgs) Handles RadGrid2.ItemCreated
        If TypeOf e.Item Is GridDataItem Then
            Dim imageButon As ImageButton = DirectCast(e.Item.FindControl("EditButton"), ImageButton)
            Dim idProveedor = e.Item.OwnerTableView.DataKeyValues(e.Item.ItemIndex)("IdProveedorCliente")
            imageButon.Attributes.Add("onClick", "CargarVentanaProveedor(" & idProveedor & ", 'editar'); return false;")
        End If
    End Sub
End Class