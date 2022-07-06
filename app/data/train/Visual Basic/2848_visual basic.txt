Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace WipTwoStageReceipting.WipInspectionRework.Data
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class PostWIPInspectRework
        Implements Codewell.SysproIntegration.ITransaction
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private itemsField As List(Of PostWIPInspectReworkItem)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private _xmlOut As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private _params As Params.PostWIPInspectReworkParameters
        
        Public Sub New()
            MyBase.New
            If (Me.itemsField Is Nothing) Then
                Me.itemsField = New List(Of PostWIPInspectReworkItem)
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute("Item")>  _
        Public Property ItemsCollection() As List(Of PostWIPInspectReworkItem)
            Get
                Return Me.itemsField
            End Get
            Set
                Me.itemsField = value
            End Set
        End Property
        
        Public Overridable ReadOnly Property XmlOut() As String Implements ITransaction.XmlOut
            Get
                Return _xmlOut
            End Get
        End Property
        
        Public Overridable ReadOnly Property BusinessObject() As String Implements ITransaction.BusinessObject
            Get
                Return "WIPTRI"
            End Get
        End Property
        
        Public Overridable ReadOnly Property Params() As Params.PostWIPInspectReworkParameters
            Get
                If (Me._params Is Nothing) Then
                    Me._params = New Params.PostWIPInspectReworkParameters
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
                Dim objParams As Params.PostWIPInspectRework
                objParams = New Params.PostWIPInspectRework
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
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class PostWIPInspectReworkItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private inspectionReferenceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lotField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reworkQuantityField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private documentField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private narrationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reworkTypeField As PostWIPInspectReworkItemReworkType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reworkJobField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ledgerCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ledgerPasswordField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private journalField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private selectAvailableSerialsField As PostWIPInspectReworkItemSelectAvailableSerials
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reservedSerialsField As PostWIPInspectReworkItemReservedSerials
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reservedSerialsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serialsField As List(Of PostWIPInspectReworkItemSerials)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reservedLotField As String
        
        Public Sub New()
            MyBase.New
            Me.reworkTypeField = PostWIPInspectReworkItemReworkType.I
            Me.selectAvailableSerialsField = PostWIPInspectReworkItemSelectAvailableSerials.N
            If (Me.serialsField Is Nothing) Then
                Me.serialsField = New List(Of PostWIPInspectReworkItemSerials)
            End If
        End Sub
        
        Public Property InspectionReference() As String
            Get
                Return Me.inspectionReferenceField
            End Get
            Set
                Me.inspectionReferenceField = value
            End Set
        End Property
        
        Public Property StockCode() As String
            Get
                Return Me.stockCodeField
            End Get
            Set
                Me.stockCodeField = value
            End Set
        End Property
        
        Public Property Lot() As String
            Get
                Return Me.lotField
            End Get
            Set
                Me.lotField = value
            End Set
        End Property
        
        Public Property ReworkQuantity() As String
            Get
                Return Me.reworkQuantityField
            End Get
            Set
                Me.reworkQuantityField = value
            End Set
        End Property
        
        Public Property Document() As String
            Get
                Return Me.documentField
            End Get
            Set
                Me.documentField = value
            End Set
        End Property
        
        Public Property Narration() As String
            Get
                Return Me.narrationField
            End Get
            Set
                Me.narrationField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(PostWIPInspectReworkItemReworkType.I)>  _
        Public Property ReworkType() As PostWIPInspectReworkItemReworkType
            Get
                Return Me.reworkTypeField
            End Get
            Set
                Me.reworkTypeField = value
            End Set
        End Property
        
        Public Property ReworkJob() As String
            Get
                Return Me.reworkJobField
            End Get
            Set
                Me.reworkJobField = value
            End Set
        End Property
        
        Public Property LedgerCode() As String
            Get
                Return Me.ledgerCodeField
            End Get
            Set
                Me.ledgerCodeField = value
            End Set
        End Property
        
        Public Property LedgerPassword() As String
            Get
                Return Me.ledgerPasswordField
            End Get
            Set
                Me.ledgerPasswordField = value
            End Set
        End Property
        
        Public Property Journal() As String
            Get
                Return Me.journalField
            End Get
            Set
                Me.journalField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(PostWIPInspectReworkItemSelectAvailableSerials.N)>  _
        Public Property SelectAvailableSerials() As PostWIPInspectReworkItemSelectAvailableSerials
            Get
                Return Me.selectAvailableSerialsField
            End Get
            Set
                Me.selectAvailableSerialsField = value
            End Set
        End Property
        
        Public Property ReservedSerials() As PostWIPInspectReworkItemReservedSerials
            Get
                Return Me.reservedSerialsField
            End Get
            Set
                Me.reservedSerialsField = value
                Me.ReservedSerialsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property ReservedSerialsSpecified() As Boolean
            Get
                Return Me.reservedSerialsFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("Serials")>  _
        Public Property SerialsCollection() As List(Of PostWIPInspectReworkItemSerials)
            Get
                Return Me.serialsField
            End Get
            Set
                Me.serialsField = value
            End Set
        End Property
        
        Public Property ReservedLot() As String
            Get
                Return Me.reservedLotField
            End Get
            Set
                Me.reservedLotField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostWIPInspectReworkItemReworkType
        
        '''<remarks/>
        I
        
        '''<remarks/>
        C
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostWIPInspectReworkItemSelectAvailableSerials
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostWIPInspectReworkItemReservedSerials
        
        '''<remarks/>
        R
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class PostWIPInspectReworkItemSerials
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serialNumberField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serialQuantityField As String
        
        Public Property SerialNumber() As String
            Get
                Return Me.serialNumberField
            End Get
            Set
                Me.serialNumberField = value
            End Set
        End Property
        
        Public Property SerialQuantity() As String
            Get
                Return Me.serialQuantityField
            End Get
            Set
                Me.serialQuantityField = value
            End Set
        End Property
    End Class
End Namespace
