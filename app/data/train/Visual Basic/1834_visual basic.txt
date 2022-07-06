Imports System
Imports System.Data
Imports System.Collections
Imports System.Web.UI
Imports Telerik.Web.UI
Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.BussinesLogic
Public Class ControlCuentaBancaria
    Inherits System.Web.UI.UserControl
    Private _dataItem As Object = Nothing
    Dim PlanContableBl As New PlanContableBL
    Dim _PlanContable As PlanContable

    Public Property DataItem() As Object
        Get
            Return Me._dataItem
        End Get
        Set(ByVal value As Object)
            Me._dataItem = value
        End Set
    End Property
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Me.RadComboBox1.Filter = 1
        Me.RadComboBox2.Filter = 1
    End Sub
    Protected Sub Page_DataBinding(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.DataBinding
        If Not TypeOf DataItem Is GridInsertionObject Then
            'para la lista de valores de cuenta contable
            _PlanContable = PlanContableBl.GetPLANCONTABLEByIdPlan(DataBinder.Eval(DataItem, "IdPlan"))
            If Not _PlanContable Is Nothing Then
                Me.RadComboBox1.DataSourceID = "odsPlanContable1"
                Me.odsPlanContable1.SelectParameters("Texto").DefaultValue = _PlanContable.Nombre
                Me.RadComboBox1.DataBind()
                Me.RadComboBox1.SelectedValue = DataBinder.Eval(DataItem, "IdPlan")
            End If

            Me.TextBox1.Text = DataBinder.Eval(DataItem, "Descripcion")
            Me.DropDownList1.SelectedValue = DataBinder.Eval(DataItem, "IdMoneda")
            Me.DropDownList2.SelectedValue = DataBinder.Eval(DataItem, "IdBanco")
            Me.DropDownList3.SelectedValue = DataBinder.Eval(DataItem, "IdTipoCtaBancaria")
            Me.TextBox2.Text = DataBinder.Eval(DataItem, "NroCta")
            Me.RadComboBox2.SelectedValue = DataBinder.Eval(DataItem, "IdSubdiario")
        End If
    End Sub
    Protected Sub RadComboBox1_ItemDataBound(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemEventArgs) Handles RadComboBox1.ItemDataBound
        e.Item.Text = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.PlanContable)).Cuenta.ToString()
        e.Item.Value = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.PlanContable)).IdPlan.ToString()
    End Sub

    Protected Sub RadComboBox1_ItemsRequested(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemsRequestedEventArgs) Handles RadComboBox1.ItemsRequested
        If e.Text.Length > 2 Then
            Me.RadComboBox1.DataSourceID = "odsPlanContable1"
            Me.odsPlanContable1.SelectParameters("Texto").DefaultValue = e.Text.ToString
            Me.RadComboBox1.DataBind()
        End If
    End Sub
End Class