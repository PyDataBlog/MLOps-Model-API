Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace SalesOrdersPrimaryPosting.TpmPriceMaintenance.Data
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("StandaloneXSDGenerator", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class PostTPMPriceMaintenance
        Implements Codewell.SysproIntegration.ITransaction

        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private itemField As PostTPMPriceMaintenanceItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private _xmlOut As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private _params As Params.PostTPMPriceMaintenanceParameters
        
        Public Sub New()
            MyBase.New
            If (Me.itemField Is Nothing) Then
                Me.itemField = New PostTPMPriceMaintenanceItem()
            End If
        End Sub
        
        Public Property Item() As PostTPMPriceMaintenanceItem
            Get
                Return Me.itemField
            End Get
            Set
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
                Return "TPMTM7"
            End Get
        End Property
        
        Public Overridable ReadOnly Property Params() As Params.PostTPMPriceMaintenanceParameters
            Get
                If (Me._params Is Nothing) Then
                    Me._params = New Params.PostTPMPriceMaintenanceParameters()
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
                Dim objParams As Params.PostTPMPriceMaintenance
                objParams = New Params.PostTPMPriceMaintenance()
                objParams.Parameters = Me.Params
                Return Common.SysproUtilities.SerializeToXMLString(objParams)
            End Get
        End Property
        
        Public Overridable Sub Post(ByVal blnThrowExceptionOnErrorInXMLOut As Boolean) Implements ITransaction.Post
            Me._xmlOut = TransactionObject.Post(Me)
            If (blnThrowExceptionOnErrorInXMLOut = true) Then
Common.SysproUtilities.CheckXMLOut(XMLData, XmlParam, XmlOut)
            End If
        End Sub
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("StandaloneXSDGenerator", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class PostTPMPriceMaintenanceItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private priceTypeField As PostTPMPriceMaintenanceItemPriceType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private keyAreaField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private pickupDeliveryField As PostTPMPriceMaintenanceItemPickupDelivery
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currencyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private selectionTypeField As PostTPMPriceMaintenanceItemSelectionType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private selectionAreaField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private basisField As PostTPMPriceMaintenanceItemBasis
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private priceDiscountFlagField As PostTPMPriceMaintenanceItemPriceDiscountFlag
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private priceBreakLineField As PostTPMPriceMaintenanceItemPriceBreakLine
        
        Public Sub New()
            MyBase.New
            Me.priceTypeField = PostTPMPriceMaintenanceItemPriceType.S
            Me.pickupDeliveryField = PostTPMPriceMaintenanceItemPickupDelivery.P
            Me.selectionTypeField = PostTPMPriceMaintenanceItemSelectionType.Item
            Me.basisField = PostTPMPriceMaintenanceItemBasis.Q
            Me.priceDiscountFlagField = PostTPMPriceMaintenanceItemPriceDiscountFlag.P
            If (Me.priceBreakLineField Is Nothing) Then
                Me.priceBreakLineField = New PostTPMPriceMaintenanceItemPriceBreakLine()
            End If
        End Sub
        
        Public Property PriceType() As PostTPMPriceMaintenanceItemPriceType
            Get
                Return Me.priceTypeField
            End Get
            Set
                Me.priceTypeField = value
            End Set
        End Property
        
        Public Property KeyArea() As String
            Get
                Return Me.keyAreaField
            End Get
            Set
                Me.keyAreaField = value
            End Set
        End Property
        
        Public Property PickupDelivery() As PostTPMPriceMaintenanceItemPickupDelivery
            Get
                Return Me.pickupDeliveryField
            End Get
            Set
                Me.pickupDeliveryField = value
            End Set
        End Property
        
        Public Property Currency() As String
            Get
                Return Me.currencyField
            End Get
            Set
                Me.currencyField = value
            End Set
        End Property
        
        Public Property SelectionType() As PostTPMPriceMaintenanceItemSelectionType
            Get
                Return Me.selectionTypeField
            End Get
            Set
                Me.selectionTypeField = value
            End Set
        End Property
        
        Public Property SelectionArea() As String
            Get
                Return Me.selectionAreaField
            End Get
            Set
                Me.selectionAreaField = value
            End Set
        End Property
        
        Public Property Basis() As PostTPMPriceMaintenanceItemBasis
            Get
                Return Me.basisField
            End Get
            Set
                Me.basisField = value
            End Set
        End Property
        
        Public Property PriceDiscountFlag() As PostTPMPriceMaintenanceItemPriceDiscountFlag
            Get
                Return Me.priceDiscountFlagField
            End Get
            Set
                Me.priceDiscountFlagField = value
            End Set
        End Property
        
        Public Property PriceBreakLine() As PostTPMPriceMaintenanceItemPriceBreakLine
            Get
                Return Me.priceBreakLineField
            End Get
            Set
                Me.priceBreakLineField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("StandaloneXSDGenerator", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostTPMPriceMaintenanceItemPriceType
        
        '''<remarks/>
        S
        
        '''<remarks/>
        P
        
        '''<remarks/>
        G
        
        '''<remarks/>
        D
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("StandaloneXSDGenerator", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostTPMPriceMaintenanceItemPickupDelivery
        
        '''<remarks/>
        P
        
        '''<remarks/>
        D
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("StandaloneXSDGenerator", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostTPMPriceMaintenanceItemSelectionType
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute(" ")>  _
        Item
        
        '''<remarks/>
        A
        
        '''<remarks/>
        B
        
        '''<remarks/>
        C
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("StandaloneXSDGenerator", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostTPMPriceMaintenanceItemBasis
        
        '''<remarks/>
        M
        
        '''<remarks/>
        Q
        
        '''<remarks/>
        V
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("StandaloneXSDGenerator", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostTPMPriceMaintenanceItemPriceDiscountFlag
        
        '''<remarks/>
        P
        
        '''<remarks/>
        D
        
        '''<remarks/>
        C
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("StandaloneXSDGenerator", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class PostTPMPriceMaintenanceItemPriceBreakLine
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private quantityField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueField As String
        
        Public Property Quantity() As String
            Get
                Return Me.quantityField
            End Get
            Set
                Me.quantityField = value
            End Set
        End Property
        
        Public Property Value() As String
            Get
                Return Me.valueField
            End Get
            Set
                Me.valueField = value
            End Set
        End Property
    End Class
End Namespace
