Imports System
Imports System.Configuration
Imports System.Data
Imports System.Data.SqlClient
Imports Telerik.Web.UI
Imports SistFoncreagro.BussinesLogic
Imports SistFoncreagro.BussinessEntities
Imports System.Collections
Imports System.Web
Imports System.Web.Security
Imports System.Web.UI
Imports System.Web.UI.WebControls
Imports System.Web.UI.WebControls.WebParts
Imports System.Web.UI.HtmlControls
Imports System.Data.OleDb
Public Class FrmRepListRequi
    Inherits System.Web.UI.Page

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

    End Sub

    Protected Sub GridRequi_ItemCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles GridRequi.ItemCommand
        If e.CommandName = "detalle" Then
            Dim idRequerimiento As String = (CType(e.Item, GridDataItem)).OwnerTableView.DataKeyValues(e.Item.ItemIndex)("idRequerimiento").ToString
            Dim radalertscript As String = "<script language='javascript'>function f(){radopen('DetalleReq.aspx?idReq=" + idRequerimiento.ToString + "','Formulario'); Sys.Application.remove_load(f);}; Sys.Application.add_load(f);</script>"
            Page.ClientScript.RegisterStartupScript(Me.[GetType](), "radalert", radalertscript)
            e.Canceled = True
        End If
    End Sub
End Class