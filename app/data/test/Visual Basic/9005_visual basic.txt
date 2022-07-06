Imports Telerik.Web.UI
Public Class ControlManttoNievelAprobacion
    Inherits System.Web.UI.UserControl
    Private _dataItem As Object = Nothing
    Public Property DataItem() As Object
        Get
            Return Me._dataItem
        End Get
        Set(ByVal value As Object)
            Me._dataItem = value
        End Set
    End Property
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        rcbRoles.Filter = 1
    End Sub
    Private Sub Page_DataBinding(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.DataBinding
        If TypeOf DataItem Is GridInsertionObject Then
            txtOrden.Text = ""
            ImageButton3.Visible = False
        Else
            rcbRoles.SelectedValue = DataBinder.Eval(DataItem, "IdRol").ToString
            txtOrden.Text = DataBinder.Eval(DataItem, "Orden").ToString
            cbAreaProyecto.Checked = (IIf(DataBinder.Eval(DataItem, "AreaProyecto").ToString = True, True, False))
            ImageButton2.Visible = False
        End If
    End Sub
End Class