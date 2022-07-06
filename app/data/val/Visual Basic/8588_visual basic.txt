Imports System
Imports System.Configuration
Imports System.Data
Imports System.Data.SqlClient
Imports Telerik.Web.UI
Imports SistFoncreagro.BussinesLogic
Imports SistFoncreagro.BussinessEntities
Public Class ListaValidarInmuebles
    Inherits System.Web.UI.Page
    Private _ordersExpandedState As Hashtable
    Private _selectedState As Hashtable
    Dim BienInmuebleBl As New BienInmuebleBL
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        If Not IsPostBack Then
            'reset states
            Me._ordersExpandedState = Nothing
            Me.Session("_ordersExpandedState") = Nothing
            Me._selectedState = Nothing
            Me.Session("_selectedState") = Nothing
        End If
    End Sub
    Public Sub ConfigureExport()
        RadGrid1.ExportSettings.ExportOnlyData = True
        RadGrid1.ExportSettings.IgnorePaging = True
        RadGrid1.ExportSettings.OpenInNewWindow = True
    End Sub
    Protected Sub RadGrid1_ItemCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RadGrid1.ItemCommand
        Dim id As String
        Dim cadena_java As String

        If e.CommandName = Telerik.Web.UI.RadGrid.ExportToExcelCommandName OrElse e.CommandName = Telerik.Web.UI.RadGrid.ExportToWordCommandName OrElse e.CommandName = Telerik.Web.UI.RadGrid.ExportToPdfCommandName Then
            ConfigureExport()
        ElseIf e.CommandName = "Editar" Then
            Dim editedItem As GridEditableItem = CType(e.Item, GridEditableItem)
            id = editedItem.OwnerTableView.DataKeyValues(editedItem.ItemIndex)("IdBienInmueble")
            Me.HiddenField1.Value = id
            cadena_java = "<script type='text/javascript'> " & _
                                           " radopen('FrmValidarInmueble.aspx?IdBienInmueble=" + id.ToString + "','Formulario'); " & _
                                           Chr(60) & "/script>"
            ScriptManager.RegisterStartupScript(Page, GetType(Page), "Cerrar", cadena_java.ToString, False)
        End If

        'save the expanded/selected state in the session
        If e.CommandName = RadGrid.ExpandCollapseCommandName Then
            'Is the item about to be expanded or collapsed
            If Not e.Item.Expanded Then
                'Save its unique index among all the items in the hierarchy
                Me.ExpandedStates(e.Item.ItemIndexHierarchical) = True
            Else
                'collapsed
                Me.ExpandedStates.Remove(e.Item.ItemIndexHierarchical)
                Me.ClearSelectedChildren(e.Item.ItemIndexHierarchical)
                Me.ClearExpandedChildren(e.Item.ItemIndexHierarchical)
            End If
            'Is the item about to be selected 
        ElseIf e.CommandName = RadGrid.SelectCommandName Then
            'Save its unique index among all the items in the hierarchy
            Me.SelectedStates(e.Item.ItemIndexHierarchical) = True
            'Is the item about to be deselected 
        ElseIf e.CommandName = RadGrid.DeselectCommandName Then
            Me.SelectedStates.Remove(e.Item.ItemIndexHierarchical)
        End If
    End Sub
    Protected Sub RadAjaxManager1_AjaxRequest(ByVal sender As Object, ByVal e As Telerik.Web.UI.AjaxRequestEventArgs) Handles RadAjaxManager1.AjaxRequest
        If e.Argument = "Validar" Then
            If BienInmuebleBl.GetBIENINMUEBLEByIdBienInmueble(Me.HiddenField1.Value).Estado = "S" Then
                Dim cadena_java As String = _
                                         "<script type='text/javascript'> " & _
                                         " if (confirm('¡Debe validar el Inmueble!')) { radopen('FrmValidarInmueble.aspx?IdBienInmueble=" + Me.HiddenField1.Value + "','Formulario'); }  " & _
                                         Chr(60) & "/script>"
                ScriptManager.RegisterStartupScript(Page, GetType(Page), "AlertaX", cadena_java.ToString, False)
                Me.RadGrid1.Rebind()
            Else
                Me.RadGrid1.Rebind()
            End If
        End If
    End Sub
    Private ReadOnly Property ExpandedStates() As Hashtable
        Get
            If Me._ordersExpandedState Is Nothing Then
                _ordersExpandedState = TryCast(Me.Session("_ordersExpandedState"), Hashtable)
                If _ordersExpandedState Is Nothing Then
                    _ordersExpandedState = New Hashtable()
                    Me.Session("_ordersExpandedState") = _ordersExpandedState
                End If
            End If

            Return Me._ordersExpandedState
        End Get
    End Property

    'Clear the state for all expanded children if a parent item is collapsed
    Private Sub ClearExpandedChildren(ByVal parentHierarchicalIndex As String)
        Dim indexes As String() = New String(Me.ExpandedStates.Keys.Count - 1) {}
        Me.ExpandedStates.Keys.CopyTo(indexes, 0)
        For Each index As String In indexes
            'all indexes of child items
            If index.StartsWith(parentHierarchicalIndex + "_") OrElse index.StartsWith(parentHierarchicalIndex + ":") Then
                Me.ExpandedStates.Remove(index)
            End If
        Next
    End Sub

    Private Sub ClearSelectedChildren(ByVal parentHierarchicalIndex As String)
        Dim indexes As String() = New String(Me.SelectedStates.Keys.Count - 1) {}
        Me.SelectedStates.Keys.CopyTo(indexes, 0)
        For Each index As String In indexes
            'all indexes of child items
            If index.StartsWith(parentHierarchicalIndex + "_") OrElse index.StartsWith(parentHierarchicalIndex + ":") Then
                Me.SelectedStates.Remove(index)
            End If
        Next
    End Sub
    Private ReadOnly Property SelectedStates() As Hashtable
        Get
            If Me._selectedState Is Nothing Then
                _selectedState = TryCast(Me.Session("_selectedState"), Hashtable)
                If _selectedState Is Nothing Then
                    _selectedState = New Hashtable()
                    Me.Session("_selectedState") = _selectedState
                End If
            End If

            Return Me._selectedState
        End Get
    End Property
    Protected Sub RadGrid1_DataBound(ByVal sender As Object, ByVal e As EventArgs) Handles RadGrid1.DataBound
        'Expand all items using our custom storage
        Dim indexes As String() = New String(Me.ExpandedStates.Keys.Count - 1) {}
        Me.ExpandedStates.Keys.CopyTo(indexes, 0)

        Dim arr As New ArrayList(indexes)
        'Sort so we can guarantee that a parent item is expanded before any of 
        'its children
        arr.Sort()

        For Each key As String In arr
            Dim value As Boolean = CBool(Me.ExpandedStates(key))
            If value Then
                RadGrid1.Items(key).Expanded = True
            End If
        Next

        'Select all items using our custom storage
        indexes = New String(Me.SelectedStates.Keys.Count - 1) {}
        Me.SelectedStates.Keys.CopyTo(indexes, 0)

        arr = New ArrayList(indexes)
        'Sort to ensure that a parent item is selected before any of its children
        arr.Sort()

        For Each key As String In arr
            Dim value As Boolean = CBool(Me.SelectedStates(key))
            If value Then
                RadGrid1.Items(key).Selected = True
            End If
        Next
    End Sub

    Protected Sub RadGrid1_ItemDataBound(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridItemEventArgs) Handles RadGrid1.ItemDataBound
        For i = 0 To e.Item.OwnerTableView.Items.Count - 1
            If e.Item.OwnerTableView.Items.Item(i)("Estado").Text = "Anulado" Then
                e.Item.OwnerTableView.Items.Item(i)("Editar").Enabled = False
                e.Item.OwnerTableView.Items.Item(i).BackColor = Drawing.Color.Silver
            ElseIf e.Item.OwnerTableView.Items.Item(i)("Estado").Text = "Sin Validar" Then
                e.Item.OwnerTableView.Items.Item(i)("Estado").ForeColor = Drawing.Color.Red
            ElseIf e.Item.OwnerTableView.Items.Item(i)("Estado").Text = "Validado" Then
                e.Item.OwnerTableView.Items.Item(i)("Estado").ForeColor = Drawing.Color.Blue
            End If

        Next
    End Sub
    Protected Sub RadGrid1_PreRender(ByVal sender As Object, ByVal e As EventArgs) Handles RadGrid1.PreRender
        If (Not Page.IsPostBack) Then
            RadGrid1.MasterTableView.FilterExpression = "(Estado LIKE '%Sin Validar%') "
            Dim column As GridColumn = RadGrid1.MasterTableView.GetColumn("Estado")
            column.CurrentFilterFunction = GridKnownFunction.Contains
            column.CurrentFilterValue = "Sin Validar"
            RadGrid1.MasterTableView.Rebind()
        End If
    End Sub
    Public Function Color(ByVal Estado As String) As Drawing.Color
        Dim col As Drawing.Color
        Select Case Estado
            Case "Sin Validar"
                col = Drawing.Color.Red
            Case "Anulado"
                col = Drawing.Color.Gray
            Case "Validado"
                col = Drawing.Color.Blue
        End Select

        Return col
    End Function
End Class