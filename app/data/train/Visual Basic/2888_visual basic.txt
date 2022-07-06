Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace SalesOrdersPrimaryQuery.DispatchInvoiceQuery.QueryData
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class Query
        Implements Codewell.SysproIntegration.IQuery
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private optionField As QueryOption
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterField As QueryFilter
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private _queryActionTypeField As CommonEnums.ActionType_Query
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private _xmlOut As String
        
        Public Sub New()
            MyBase.New
            If (Me.filterField Is Nothing) Then
                Me.filterField = New QueryFilter
            End If
            If (Me.optionField Is Nothing) Then
                Me.optionField = New QueryOption
            End If
        End Sub
        
        Public Property [Option]() As QueryOption
            Get
                Return Me.optionField
            End Get
            Set
                Me.optionField = value
            End Set
        End Property
        
        Public Property Filter() As QueryFilter
            Get
                Return Me.filterField
            End Get
            Set
                Me.filterField = value
            End Set
        End Property
        
        Public Overridable Property QueryActionType() As CommonEnums.ActionType_Query Implements IQuery.QueryActionType
            Get
                Return _queryActionTypeField
            End Get
            Set
                Me._queryActionTypeField = value
            End Set
        End Property
        
        Public Overridable ReadOnly Property XmlOut() As String Implements IQuery.XmlOut
            Get
                Return _xmlOut
            End Get
        End Property
        
        Public Overridable ReadOnly Property BusinessObject() As String Implements IQuery.BusinessObject
            Get
                Return "SORQND"
            End Get
        End Property
        
        Public Overridable ReadOnly Property XMLData() As String Implements IQuery.XMLData
            Get
                Dim ms As System.IO.MemoryStream
                Dim s As System.Xml.Serialization.XmlSerializer
                Dim tw As System.Xml.XmlTextWriter
                Dim doc As System.Xml.XmlDocument
                ms = New System.IO.MemoryStream
                s = New System.Xml.Serialization.XmlSerializer(Me.GetType)
                tw = New System.Xml.XmlTextWriter(ms, Nothing)
                doc = New System.Xml.XmlDocument
                s.Serialize(tw, Me)
                ms.Seek(0, 0)
                doc.Load(ms)
                Return doc.OuterXml
            End Get
        End Property
        
        Public Overridable Sub Post(ByVal blnThrowExceptionOnErrorInXMLOut As Boolean) Implements IQuery.Post
            If (Me._queryActionTypeField = 1) Then
                Me._xmlOut = QueryObject.Browse(Me)
                Return
            End If
            If (Me._queryActionTypeField = 0) Then
                Me._xmlOut = QueryObject.Fetch(Me)
                Return
            End If
            If (Me._queryActionTypeField = 4) Then
                Me._xmlOut = QueryObject.Query(Me)
                Return
            End If
            If (Me._queryActionTypeField = 2) Then
                Me._xmlOut = QueryObject.NextKey(Me)
                Return
            End If
            If (Me._queryActionTypeField = 3) Then
                Me._xmlOut = QueryObject.PreviousKey(Me)
                Return
            End If
            If blnThrowExceptionOnErrorInXMLOut Then
                Common.SysproUtilities.CheckXMLOut(XMLData, "", XmlOut)
            End If
        End Sub
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class QueryOption
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reprintField As QueryOptionReprint
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private documentTypeField As QueryOptionDocumentType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeFormsField As QueryOptionIncludeForms
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeCustomerFormsField As QueryOptionIncludeCustomerForms
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeStockFormsField As QueryOptionIncludeStockForms
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeDetailFormsField As QueryOptionIncludeDetailForms
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeNotesField As QueryOptionIncludeNotes
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ignorePrintStatusField As QueryOptionIgnorePrintStatus
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private roundMassField As QueryOptionRoundMass
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private roundMassDecsField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private roundVolumeField As QueryOptionRoundVolume
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private roundVolumeDecsField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private xslStylesheetField As String
        
        Public Sub New()
            MyBase.New
            Me.reprintField = QueryOptionReprint.Y
            Me.documentTypeField = QueryOptionDocumentType.I
            Me.includeFormsField = QueryOptionIncludeForms.N
            Me.includeCustomerFormsField = QueryOptionIncludeCustomerForms.N
            Me.includeStockFormsField = QueryOptionIncludeStockForms.N
            Me.includeDetailFormsField = QueryOptionIncludeDetailForms.N
            Me.includeNotesField = QueryOptionIncludeNotes.N
            Me.ignorePrintStatusField = QueryOptionIgnorePrintStatus.N
            Me.roundMassField = QueryOptionRoundMass.O
            Me.roundVolumeField = QueryOptionRoundVolume.O
        End Sub
        
        <System.ComponentModel.DefaultValueAttribute(QueryOptionReprint.Y)>  _
        Public Property Reprint() As QueryOptionReprint
            Get
                Return Me.reprintField
            End Get
            Set
                Me.reprintField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(QueryOptionDocumentType.I)>  _
        Public Property DocumentType() As QueryOptionDocumentType
            Get
                Return Me.documentTypeField
            End Get
            Set
                Me.documentTypeField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(QueryOptionIncludeForms.N)>  _
        Public Property IncludeForms() As QueryOptionIncludeForms
            Get
                Return Me.includeFormsField
            End Get
            Set
                Me.includeFormsField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(QueryOptionIncludeCustomerForms.N)>  _
        Public Property IncludeCustomerForms() As QueryOptionIncludeCustomerForms
            Get
                Return Me.includeCustomerFormsField
            End Get
            Set
                Me.includeCustomerFormsField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(QueryOptionIncludeStockForms.N)>  _
        Public Property IncludeStockForms() As QueryOptionIncludeStockForms
            Get
                Return Me.includeStockFormsField
            End Get
            Set
                Me.includeStockFormsField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(QueryOptionIncludeDetailForms.N)>  _
        Public Property IncludeDetailForms() As QueryOptionIncludeDetailForms
            Get
                Return Me.includeDetailFormsField
            End Get
            Set
                Me.includeDetailFormsField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(QueryOptionIncludeNotes.N)>  _
        Public Property IncludeNotes() As QueryOptionIncludeNotes
            Get
                Return Me.includeNotesField
            End Get
            Set
                Me.includeNotesField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(QueryOptionIgnorePrintStatus.N)>  _
        Public Property IgnorePrintStatus() As QueryOptionIgnorePrintStatus
            Get
                Return Me.ignorePrintStatusField
            End Get
            Set
                Me.ignorePrintStatusField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(QueryOptionRoundMass.O)>  _
        Public Property RoundMass() As QueryOptionRoundMass
            Get
                Return Me.roundMassField
            End Get
            Set
                Me.roundMassField = value
            End Set
        End Property
        
        Public Property RoundMassDecs() As String
            Get
                Return Me.roundMassDecsField
            End Get
            Set
                Me.roundMassDecsField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(QueryOptionRoundVolume.O)>  _
        Public Property RoundVolume() As QueryOptionRoundVolume
            Get
                Return Me.roundVolumeField
            End Get
            Set
                Me.roundVolumeField = value
            End Set
        End Property
        
        Public Property RoundVolumeDecs() As String
            Get
                Return Me.roundVolumeDecsField
            End Get
            Set
                Me.roundVolumeDecsField = value
            End Set
        End Property
        
        Public Property XslStylesheet() As String
            Get
                Return Me.xslStylesheetField
            End Get
            Set
                Me.xslStylesheetField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryOptionReprint
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        N
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryOptionDocumentType
        
        '''<remarks/>
        I
        
        '''<remarks/>
        G
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryOptionIncludeForms
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("")>  _
        Item
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryOptionIncludeCustomerForms
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("")>  _
        Item
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryOptionIncludeStockForms
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("")>  _
        Item
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryOptionIncludeDetailForms
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("")>  _
        Item
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryOptionIncludeNotes
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("")>  _
        Item
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryOptionIgnorePrintStatus
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("")>  _
        Item
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryOptionRoundMass
        
        '''<remarks/>
        O
        
        '''<remarks/>
        U
        
        '''<remarks/>
        D
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryOptionRoundVolume
        
        '''<remarks/>
        O
        
        '''<remarks/>
        U
        
        '''<remarks/>
        D
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class QueryFilter
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private branchField As QueryFilterBranch
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As QueryFilterCustomer
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private orderTypeField As QueryFilterOrderType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerPoField As QueryFilterCustomerPo
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dispatchNoteField As QueryFilterDispatchNote
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceNumberField As QueryFilterInvoiceNumber
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private gtrReferenceField As QueryFilterGtrReference
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesOrderField As QueryFilterSalesOrder
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceDateField As QueryFilterInvoiceDate
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operatorField As QueryFilterOperator
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private geographicAreaField As QueryFilterGeographicArea
        
        Public Sub New()
            MyBase.New
            If (Me.geographicAreaField Is Nothing) Then
                Me.geographicAreaField = New QueryFilterGeographicArea
            End If
            If (Me.operatorField Is Nothing) Then
                Me.operatorField = New QueryFilterOperator
            End If
            If (Me.invoiceDateField Is Nothing) Then
                Me.invoiceDateField = New QueryFilterInvoiceDate
            End If
            If (Me.salesOrderField Is Nothing) Then
                Me.salesOrderField = New QueryFilterSalesOrder
            End If
            If (Me.gtrReferenceField Is Nothing) Then
                Me.gtrReferenceField = New QueryFilterGtrReference
            End If
            If (Me.invoiceNumberField Is Nothing) Then
                Me.invoiceNumberField = New QueryFilterInvoiceNumber
            End If
            If (Me.dispatchNoteField Is Nothing) Then
                Me.dispatchNoteField = New QueryFilterDispatchNote
            End If
            If (Me.customerPoField Is Nothing) Then
                Me.customerPoField = New QueryFilterCustomerPo
            End If
            If (Me.orderTypeField Is Nothing) Then
                Me.orderTypeField = New QueryFilterOrderType
            End If
            If (Me.customerField Is Nothing) Then
                Me.customerField = New QueryFilterCustomer
            End If
            If (Me.branchField Is Nothing) Then
                Me.branchField = New QueryFilterBranch
            End If
        End Sub
        
        Public Property Branch() As QueryFilterBranch
            Get
                Return Me.branchField
            End Get
            Set
                Me.branchField = value
            End Set
        End Property
        
        Public Property Customer() As QueryFilterCustomer
            Get
                Return Me.customerField
            End Get
            Set
                Me.customerField = value
            End Set
        End Property
        
        Public Property OrderType() As QueryFilterOrderType
            Get
                Return Me.orderTypeField
            End Get
            Set
                Me.orderTypeField = value
            End Set
        End Property
        
        Public Property CustomerPo() As QueryFilterCustomerPo
            Get
                Return Me.customerPoField
            End Get
            Set
                Me.customerPoField = value
            End Set
        End Property
        
        Public Property DispatchNote() As QueryFilterDispatchNote
            Get
                Return Me.dispatchNoteField
            End Get
            Set
                Me.dispatchNoteField = value
            End Set
        End Property
        
        Public Property InvoiceNumber() As QueryFilterInvoiceNumber
            Get
                Return Me.invoiceNumberField
            End Get
            Set
                Me.invoiceNumberField = value
            End Set
        End Property
        
        Public Property GtrReference() As QueryFilterGtrReference
            Get
                Return Me.gtrReferenceField
            End Get
            Set
                Me.gtrReferenceField = value
            End Set
        End Property
        
        Public Property SalesOrder() As QueryFilterSalesOrder
            Get
                Return Me.salesOrderField
            End Get
            Set
                Me.salesOrderField = value
            End Set
        End Property
        
        Public Property InvoiceDate() As QueryFilterInvoiceDate
            Get
                Return Me.invoiceDateField
            End Get
            Set
                Me.invoiceDateField = value
            End Set
        End Property
        
        Public Property [Operator]() As QueryFilterOperator
            Get
                Return Me.operatorField
            End Get
            Set
                Me.operatorField = value
            End Set
        End Property
        
        Public Property GeographicArea() As QueryFilterGeographicArea
            Get
                Return Me.geographicAreaField
            End Get
            Set
                Me.geographicAreaField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class QueryFilterBranch
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeField As QueryFilterBranchFilterType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        Public Sub New()
            MyBase.New
            Me.filterTypeField = QueryFilterBranchFilterType.A
        End Sub
        
        <System.Xml.Serialization.XmlAttributeAttribute(),  _
         System.ComponentModel.DefaultValueAttribute(QueryFilterBranchFilterType.A)>  _
        Public Property FilterType() As QueryFilterBranchFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterValue() As String
            Get
                Return Me.filterValueField
            End Get
            Set
                Me.filterValueField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryFilterBranchFilterType
        
        '''<remarks/>
        A
        
        '''<remarks/>
        R
        
        '''<remarks/>
        S
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("")>  _
        Item
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class QueryFilterCustomer
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeField As QueryFilterCustomerFilterType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        Public Sub New()
            MyBase.New
            Me.filterTypeField = QueryFilterCustomerFilterType.A
        End Sub
        
        <System.Xml.Serialization.XmlAttributeAttribute(),  _
         System.ComponentModel.DefaultValueAttribute(QueryFilterCustomerFilterType.A)>  _
        Public Property FilterType() As QueryFilterCustomerFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterValue() As String
            Get
                Return Me.filterValueField
            End Get
            Set
                Me.filterValueField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryFilterCustomerFilterType
        
        '''<remarks/>
        A
        
        '''<remarks/>
        R
        
        '''<remarks/>
        L
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("")>  _
        Item
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class QueryFilterOrderType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeField As QueryFilterOrderTypeFilterType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        Public Sub New()
            MyBase.New
            Me.filterTypeField = QueryFilterOrderTypeFilterType.A
        End Sub
        
        <System.Xml.Serialization.XmlAttributeAttribute(),  _
         System.ComponentModel.DefaultValueAttribute(QueryFilterOrderTypeFilterType.A)>  _
        Public Property FilterType() As QueryFilterOrderTypeFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterValue() As String
            Get
                Return Me.filterValueField
            End Get
            Set
                Me.filterValueField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryFilterOrderTypeFilterType
        
        '''<remarks/>
        A
        
        '''<remarks/>
        R
        
        '''<remarks/>
        S
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("")>  _
        Item
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class QueryFilterCustomerPo
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeField As QueryFilterCustomerPoFilterType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As QueryFilterCustomerPoFilterValue
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueFieldSpecified As Boolean
        
        Public Sub New()
            MyBase.New
            Me.filterTypeField = QueryFilterCustomerPoFilterType.A
        End Sub
        
        <System.Xml.Serialization.XmlAttributeAttribute(),  _
         System.ComponentModel.DefaultValueAttribute(QueryFilterCustomerPoFilterType.A)>  _
        Public Property FilterType() As QueryFilterCustomerPoFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterValue() As QueryFilterCustomerPoFilterValue
            Get
                Return Me.filterValueField
            End Get
            Set
                Me.filterValueField = value
                Me.FilterValueFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property FilterValueSpecified() As Boolean
            Get
                Return Me.filterValueFieldSpecified
            End Get
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryFilterCustomerPoFilterType
        
        '''<remarks/>
        A
        
        '''<remarks/>
        S
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryFilterCustomerPoFilterValue
        
        '''<remarks/>
        A
        
        '''<remarks/>
        S
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("")>  _
        Item
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class QueryFilterDispatchNote
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeField As QueryFilterDispatchNoteFilterType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        Public Sub New()
            MyBase.New
            Me.filterTypeField = QueryFilterDispatchNoteFilterType.A
        End Sub
        
        <System.Xml.Serialization.XmlAttributeAttribute(),  _
         System.ComponentModel.DefaultValueAttribute(QueryFilterDispatchNoteFilterType.A)>  _
        Public Property FilterType() As QueryFilterDispatchNoteFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterValue() As String
            Get
                Return Me.filterValueField
            End Get
            Set
                Me.filterValueField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryFilterDispatchNoteFilterType
        
        '''<remarks/>
        A
        
        '''<remarks/>
        R
        
        '''<remarks/>
        L
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class QueryFilterInvoiceNumber
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeField As QueryFilterInvoiceNumberFilterType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        Public Sub New()
            MyBase.New
            Me.filterTypeField = QueryFilterInvoiceNumberFilterType.A
        End Sub
        
        <System.Xml.Serialization.XmlAttributeAttribute(),  _
         System.ComponentModel.DefaultValueAttribute(QueryFilterInvoiceNumberFilterType.A)>  _
        Public Property FilterType() As QueryFilterInvoiceNumberFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterValue() As String
            Get
                Return Me.filterValueField
            End Get
            Set
                Me.filterValueField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryFilterInvoiceNumberFilterType
        
        '''<remarks/>
        A
        
        '''<remarks/>
        R
        
        '''<remarks/>
        L
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class QueryFilterGtrReference
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeField As QueryFilterGtrReferenceFilterType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        Public Sub New()
            MyBase.New
            Me.filterTypeField = QueryFilterGtrReferenceFilterType.A
        End Sub
        
        <System.Xml.Serialization.XmlAttributeAttribute(),  _
         System.ComponentModel.DefaultValueAttribute(QueryFilterGtrReferenceFilterType.A)>  _
        Public Property FilterType() As QueryFilterGtrReferenceFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterValue() As String
            Get
                Return Me.filterValueField
            End Get
            Set
                Me.filterValueField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryFilterGtrReferenceFilterType
        
        '''<remarks/>
        A
        
        '''<remarks/>
        R
        
        '''<remarks/>
        L
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class QueryFilterSalesOrder
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeField As QueryFilterSalesOrderFilterType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        Public Sub New()
            MyBase.New
            Me.filterTypeField = QueryFilterSalesOrderFilterType.A
        End Sub
        
        <System.Xml.Serialization.XmlAttributeAttribute(),  _
         System.ComponentModel.DefaultValueAttribute(QueryFilterSalesOrderFilterType.A)>  _
        Public Property FilterType() As QueryFilterSalesOrderFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterValue() As String
            Get
                Return Me.filterValueField
            End Get
            Set
                Me.filterValueField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryFilterSalesOrderFilterType
        
        '''<remarks/>
        A
        
        '''<remarks/>
        R
        
        '''<remarks/>
        L
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("")>  _
        Item
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class QueryFilterInvoiceDate
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeField As QueryFilterInvoiceDateFilterType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        Public Sub New()
            MyBase.New
            Me.filterTypeField = QueryFilterInvoiceDateFilterType.A
        End Sub
        
        <System.Xml.Serialization.XmlAttributeAttribute(),  _
         System.ComponentModel.DefaultValueAttribute(QueryFilterInvoiceDateFilterType.A)>  _
        Public Property FilterType() As QueryFilterInvoiceDateFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterValue() As String
            Get
                Return Me.filterValueField
            End Get
            Set
                Me.filterValueField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryFilterInvoiceDateFilterType
        
        '''<remarks/>
        A
        
        '''<remarks/>
        R
        
        '''<remarks/>
        S
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("")>  _
        Item
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class QueryFilterOperator
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeField As QueryFilterOperatorFilterType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        Public Sub New()
            MyBase.New
            Me.filterTypeField = QueryFilterOperatorFilterType.A
        End Sub
        
        <System.Xml.Serialization.XmlAttributeAttribute(),  _
         System.ComponentModel.DefaultValueAttribute(QueryFilterOperatorFilterType.A)>  _
        Public Property FilterType() As QueryFilterOperatorFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterValue() As String
            Get
                Return Me.filterValueField
            End Get
            Set
                Me.filterValueField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryFilterOperatorFilterType
        
        '''<remarks/>
        A
        
        '''<remarks/>
        S
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("")>  _
        Item
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class QueryFilterGeographicArea
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeField As QueryFilterGeographicAreaFilterType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        Public Sub New()
            MyBase.New
            Me.filterTypeField = QueryFilterGeographicAreaFilterType.A
        End Sub
        
        <System.Xml.Serialization.XmlAttributeAttribute(),  _
         System.ComponentModel.DefaultValueAttribute(QueryFilterGeographicAreaFilterType.A)>  _
        Public Property FilterType() As QueryFilterGeographicAreaFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterValue() As String
            Get
                Return Me.filterValueField
            End Get
            Set
                Me.filterValueField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum QueryFilterGeographicAreaFilterType
        
        '''<remarks/>
        A
        
        '''<remarks/>
        S
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("")>  _
        Item
    End Enum
End Namespace
