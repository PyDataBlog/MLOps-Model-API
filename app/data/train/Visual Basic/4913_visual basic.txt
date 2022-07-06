Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace SalesOrdersPrimaryQuery.ConsolidatedDispatchInvoiceQuery.QueryData
    
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
                Return "SORQCD"
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
        Private returnIndividualLinesField As QueryOptionReturnIndividualLines
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeCustomerFormsField As QueryOptionIncludeCustomerForms
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeCustomerFormsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeStockFormsField As QueryOptionIncludeStockForms
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeStockFormsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeNotesField As QueryOptionIncludeNotes
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeNotesFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ignorePrintStatusField As QueryOptionIgnorePrintStatus
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ignorePrintStatusFieldSpecified As Boolean
        
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
            Me.returnIndividualLinesField = QueryOptionReturnIndividualLines.N
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
        
        <System.ComponentModel.DefaultValueAttribute(QueryOptionReturnIndividualLines.N)>  _
        Public Property ReturnIndividualLines() As QueryOptionReturnIndividualLines
            Get
                Return Me.returnIndividualLinesField
            End Get
            Set
                Me.returnIndividualLinesField = value
            End Set
        End Property
        
        Public Property IncludeCustomerForms() As QueryOptionIncludeCustomerForms
            Get
                Return Me.includeCustomerFormsField
            End Get
            Set
                Me.includeCustomerFormsField = value
                Me.IncludeCustomerFormsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property IncludeCustomerFormsSpecified() As Boolean
            Get
                Return Me.includeCustomerFormsFieldSpecified
            End Get
        End Property
        
        Public Property IncludeStockForms() As QueryOptionIncludeStockForms
            Get
                Return Me.includeStockFormsField
            End Get
            Set
                Me.includeStockFormsField = value
                Me.IncludeStockFormsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property IncludeStockFormsSpecified() As Boolean
            Get
                Return Me.includeStockFormsFieldSpecified
            End Get
        End Property
        
        Public Property IncludeNotes() As QueryOptionIncludeNotes
            Get
                Return Me.includeNotesField
            End Get
            Set
                Me.includeNotesField = value
                Me.IncludeNotesFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property IncludeNotesSpecified() As Boolean
            Get
                Return Me.includeNotesFieldSpecified
            End Get
        End Property
        
        Public Property IgnorePrintStatus() As QueryOptionIgnorePrintStatus
            Get
                Return Me.ignorePrintStatusField
            End Get
            Set
                Me.ignorePrintStatusField = value
                Me.IgnorePrintStatusFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property IgnorePrintStatusSpecified() As Boolean
            Get
                Return Me.ignorePrintStatusFieldSpecified
            End Get
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
    Public Enum QueryOptionReturnIndividualLines
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
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
        Private customerPoField As QueryFilterCustomerPo
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceNumberField As QueryFilterInvoiceNumber
        
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
            If (Me.invoiceNumberField Is Nothing) Then
                Me.invoiceNumberField = New QueryFilterInvoiceNumber
            End If
            If (Me.customerPoField Is Nothing) Then
                Me.customerPoField = New QueryFilterCustomerPo
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
        
        Public Property CustomerPo() As QueryFilterCustomerPo
            Get
                Return Me.customerPoField
            End Get
            Set
                Me.customerPoField = value
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
        Private filterTypeFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterType() As QueryFilterBranchFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
                Me.FilterTypeFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property FilterTypeSpecified() As Boolean
            Get
                Return Me.filterTypeFieldSpecified
            End Get
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
        Private filterTypeFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterType() As QueryFilterCustomerFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
                Me.FilterTypeFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property FilterTypeSpecified() As Boolean
            Get
                Return Me.filterTypeFieldSpecified
            End Get
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
    Partial Public Class QueryFilterCustomerPo
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As QueryFilterCustomerPoFilterValue
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueFieldSpecified As Boolean
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterType() As String
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
    Partial Public Class QueryFilterInvoiceNumber
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterType() As String
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
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class QueryFilterInvoiceDate
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeField As QueryFilterInvoiceDateFilterType
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterTypeFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterType() As QueryFilterInvoiceDateFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
                Me.FilterTypeFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property FilterTypeSpecified() As Boolean
            Get
                Return Me.filterTypeFieldSpecified
            End Get
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
        Private filterTypeFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterType() As QueryFilterOperatorFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
                Me.FilterTypeFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property FilterTypeSpecified() As Boolean
            Get
                Return Me.filterTypeFieldSpecified
            End Get
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
        Private filterTypeFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private filterValueField As String
        
        <System.Xml.Serialization.XmlAttributeAttribute()>  _
        Public Property FilterType() As QueryFilterGeographicAreaFilterType
            Get
                Return Me.filterTypeField
            End Get
            Set
                Me.filterTypeField = value
                Me.FilterTypeFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property FilterTypeSpecified() As Boolean
            Get
                Return Me.filterTypeFieldSpecified
            End Get
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
