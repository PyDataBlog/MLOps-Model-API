Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace WipShopFloorDataCollection.WipTrackLotsAndSerials.Data

    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"), _
     System.SerializableAttribute(), _
     System.Diagnostics.DebuggerStepThroughAttribute(), _
     System.ComponentModel.DesignerCategoryAttribute("code"), _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=True), _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=False)> _
    Partial Public Class PostTracking
        Implements Codewell.SysproIntegration.ITransaction

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private itemField As List(Of PostTrackingItem)

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private _xmlOut As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private _params As Params.PostTrackingParameters

        Public Sub New()
            MyBase.New()
            If (Me.itemField Is Nothing) Then
                Me.itemField = New List(Of PostTrackingItem)
            End If
        End Sub

        <System.Xml.Serialization.XmlElementAttribute("Item")> _
        Public Property ItemCollection() As List(Of PostTrackingItem)
            Get
                Return Me.itemField
            End Get
            Set(ByVal value As List(Of PostTrackingItem))
                Me.itemField = value
            End Set
        End Property

        Public Overridable ReadOnly Property XmlOut() As String Implements ITransaction.XmlOut
            Get
                Return _xmlOut
            End Get
        End Property

        Public Overridable ReadOnly Property BusinessObject() As String Implements ITransaction.BusinessObject
            Get
                Return "WIPTLS"
            End Get
        End Property

        Public Overridable ReadOnly Property Params() As Params.PostTrackingParameters
            Get
                If (Me._params Is Nothing) Then
                    Me._params = New Params.PostTrackingParameters
                End If
                Return Me._params
            End Get
        End Property

        Public Overridable ReadOnly Property XMLData() As String Implements ITransaction.XMLData
            Get
                Return Common.SysproUtilities.SerializeToXMLString(Me)
            End Get
        End Property

        Public Overridable ReadOnly Property XmlParam() As String Implements ITransaction.XmlParam
            Get
                Dim objParams As Params.PostTracking
                objParams = New Params.PostTracking
                objParams.Parameters = Me.Params
                Return Common.SysproUtilities.SerializeToXMLString(objParams)
            End Get
        End Property

        Public Overridable Sub Post(ByVal blnThrowExceptionOnErrorInXMLOut As Boolean) Implements ITransaction.Post
            Me._xmlOut = TransactionObject.Post(Me)
            If (blnThrowExceptionOnErrorInXMLOut = True) Then
                Common.SysproUtilities.CheckXMLOut(XMLData, XmlParam, XmlOut)
            End If
        End Sub
    End Class

    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"), _
     System.SerializableAttribute(), _
     System.Diagnostics.DebuggerStepThroughAttribute(), _
     System.ComponentModel.DesignerCategoryAttribute("code"), _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=True)> _
    Partial Public Class PostTrackingItem

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private jobField As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private parentPartField As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private parentWarehouseField As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private warehouseField As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private stockCodeField As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private lineField As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private lotField As List(Of PostTrackingItemLot)

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private serialField As List(Of PostTrackingItemSerial)

        Public Sub New()
            MyBase.New()
            If (Me.serialField Is Nothing) Then
                Me.serialField = New List(Of PostTrackingItemSerial)
            End If
            If (Me.lotField Is Nothing) Then
                Me.lotField = New List(Of PostTrackingItemLot)
            End If
        End Sub

        Public Property Job() As String
            Get
                Return Me.jobField
            End Get
            Set(ByVal value As String)
                Me.jobField = value
            End Set
        End Property

        Public Property ParentPart() As String
            Get
                Return Me.parentPartField
            End Get
            Set(ByVal value As String)
                Me.parentPartField = value
            End Set
        End Property

        Public Property ParentWarehouse() As String
            Get
                Return Me.parentWarehouseField
            End Get
            Set(ByVal value As String)
                Me.parentWarehouseField = value
            End Set
        End Property

        Public Property Warehouse() As String
            Get
                Return Me.warehouseField
            End Get
            Set(ByVal value As String)
                Me.warehouseField = value
            End Set
        End Property

        Public Property StockCode() As String
            Get
                Return Me.stockCodeField
            End Get
            Set(ByVal value As String)
                Me.stockCodeField = value
            End Set
        End Property

        Public Property Line() As String
            Get
                Return Me.lineField
            End Get
            Set(ByVal value As String)
                Me.lineField = value
            End Set
        End Property

        <System.Xml.Serialization.XmlElementAttribute("Lot")> _
        Public Property LotCollection() As List(Of PostTrackingItemLot)
            Get
                Return Me.lotField
            End Get
            Set(ByVal value As List(Of PostTrackingItemLot))
                Me.lotField = value
            End Set
        End Property

        <System.Xml.Serialization.XmlElementAttribute("Serial")> _
        Public Property SerialCollection() As List(Of PostTrackingItemSerial)
            Get
                Return Me.serialField
            End Get
            Set(ByVal value As List(Of PostTrackingItemSerial))
                Me.serialField = value
            End Set
        End Property
    End Class

    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"), _
     System.SerializableAttribute(), _
     System.Diagnostics.DebuggerStepThroughAttribute(), _
     System.ComponentModel.DesignerCategoryAttribute("code"), _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=True)> _
    Partial Public Class PostTrackingItemLot

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private lotNumberField As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private binField As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private quantityField As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private parentLotField As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private parentBinField As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private lineField As String

        Public Property LotNumber() As String
            Get
                Return Me.lotNumberField
            End Get
            Set(ByVal value As String)
                Me.lotNumberField = value
            End Set
        End Property

        Public Property Bin() As String
            Get
                Return Me.binField
            End Get
            Set(ByVal value As String)
                Me.binField = value
            End Set
        End Property

        '''<summary>
        '''Within the lot node this element is mandatory when the ActionType parameter is 'T' (track) or 'C' (change) and indicates the quantity of the component lot to be tracked. Within the serial node this element is mandatory when the ActionType parameter is 'T' (Track) or 'C' (change) and indicates the quantity of the component serial to be tracked. If the serial method for the material allocation is manual then the quantity can only be 1. The quantity tracked cannot be more than what was issued from the lot/serial for the material allocation. The format is up to 11 characters numeric with 7 integers and 3 decimals.
        '''</summary>
        Public Property Quantity() As String
            Get
                Return Me.quantityField
            End Get
            Set(ByVal value As String)
                Me.quantityField = value
            End Set
        End Property

        Public Property ParentLot() As String
            Get
                Return Me.parentLotField
            End Get
            Set(ByVal value As String)
                Me.parentLotField = value
            End Set
        End Property

        Public Property ParentBin() As String
            Get
                Return Me.parentBinField
            End Get
            Set(ByVal value As String)
                Me.parentBinField = value
            End Set
        End Property

        Public Property Line() As String
            Get
                Return Me.lineField
            End Get
            Set(ByVal value As String)
                Me.lineField = value
            End Set
        End Property
    End Class

    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"), _
     System.SerializableAttribute(), _
     System.Diagnostics.DebuggerStepThroughAttribute(), _
     System.ComponentModel.DesignerCategoryAttribute("code"), _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=True)> _
    Partial Public Class PostTrackingItemSerial

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private serialNumberField As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private quantityField As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private parentSerialField As String

        <EditorBrowsable(EditorBrowsableState.Never)> _
        Private lineField As String

        Public Property SerialNumber() As String
            Get
                Return Me.serialNumberField
            End Get
            Set(ByVal value As String)
                Me.serialNumberField = value
            End Set
        End Property

        '''<summary>
        '''Within the lot node this element is mandatory when the ActionType parameter is 'T' (track) or 'C' (change) and indicates the quantity of the component lot to be tracked. Within the serial node this element is mandatory when the ActionType parameter is 'T' (Track) or 'C' (change) and indicates the quantity of the component serial to be tracked. If the serial method for the material allocation is manual then the quantity can only be 1. The quantity tracked cannot be more than what was issued from the lot/serial for the material allocation. The format is up to 11 characters numeric with 7 integers and 3 decimals.
        '''</summary>
        Public Property Quantity() As String
            Get
                Return Me.quantityField
            End Get
            Set(ByVal value As String)
                Me.quantityField = value
            End Set
        End Property

        Public Property ParentSerial() As String
            Get
                Return Me.parentSerialField
            End Get
            Set(ByVal value As String)
                Me.parentSerialField = value
            End Set
        End Property

        Public Property Line() As String
            Get
                Return Me.lineField
            End Get
            Set(ByVal value As String)
                Me.lineField = value
            End Set
        End Property
    End Class
End Namespace
