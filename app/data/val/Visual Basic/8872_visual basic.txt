Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace ARPrimaryQuery.ARCustomerStockCodeQuery.Output
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class CustomerStockCodes
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private itemsField As List(Of Object)
        
        Public Sub New()
            MyBase.New
            If (Me.itemsField Is Nothing) Then
                Me.itemsField = New List(Of Object)
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute("Customers", GetType(CustomerStockCodesCustomers), Form:=System.Xml.Schema.XmlSchemaForm.Unqualified),  _
         System.Xml.Serialization.XmlElementAttribute("QueryOptions", GetType(CustomerStockCodesQueryOptions), Form:=System.Xml.Schema.XmlSchemaForm.Unqualified),  _
         System.Xml.Serialization.XmlElementAttribute("SystemInformation", GetType(CustomerStockCodesSystemInformation), Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property ItemsCollection() As List(Of Object)
            Get
                Return Me.itemsField
            End Get
            Set
                Me.itemsField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class CustomerStockCodesCustomers
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerDetailsField As List(Of CustomerStockCodesCustomersCustomerDetails)
        
        Public Sub New()
            MyBase.New
            If (Me.customerDetailsField Is Nothing) Then
                Me.customerDetailsField = New List(Of CustomerStockCodesCustomersCustomerDetails)
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute("CustomerDetails", Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerDetailsCollection() As List(Of CustomerStockCodesCustomersCustomerDetails)
            Get
                Return Me.customerDetailsField
            End Get
            Set
                Me.customerDetailsField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class CustomerStockCodesCustomersCustomerDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerHeaderField As CustomerStockCodesCustomersCustomerDetailsCustomerHeader
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerStockCodesField As List(Of CustomerStockCodesCustomersCustomerDetailsCustomerStockCodes)
        
        Public Sub New()
            MyBase.New
            If (Me.customerStockCodesField Is Nothing) Then
                Me.customerStockCodesField = New List(Of CustomerStockCodesCustomersCustomerDetailsCustomerStockCodes)
            End If
            If (Me.customerHeaderField Is Nothing) Then
                Me.customerHeaderField = New CustomerStockCodesCustomersCustomerDetailsCustomerHeader
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerHeader() As CustomerStockCodesCustomersCustomerDetailsCustomerHeader
            Get
                Return Me.customerHeaderField
            End Get
            Set
                Me.customerHeaderField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("CustomerStockCodes", Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerStockCodesCollection() As List(Of CustomerStockCodesCustomersCustomerDetailsCustomerStockCodes)
            Get
                Return Me.customerStockCodesField
            End Get
            Set
                Me.customerStockCodesField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class CustomerStockCodesCustomersCustomerDetailsCustomerHeader
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerDescriptionField As String
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Customer() As String
            Get
                Return Me.customerField
            End Get
            Set
                Me.customerField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerDescription() As String
            Get
                Return Me.customerDescriptionField
            End Get
            Set
                Me.customerDescriptionField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class CustomerStockCodesCustomersCustomerDetailsCustomerStockCodes
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerStockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeDescriptionField As String
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerStockCode() As String
            Get
                Return Me.customerStockCodeField
            End Get
            Set
                Me.customerStockCodeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property StockCode() As String
            Get
                Return Me.stockCodeField
            End Get
            Set
                Me.stockCodeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property StockCodeDescription() As String
            Get
                Return Me.stockCodeDescriptionField
            End Get
            Set
                Me.stockCodeDescriptionField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class CustomerStockCodesQueryOptions
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private newPagePerCustomerField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerFilterTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerFilterValueField As String
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property NewPagePerCustomer() As String
            Get
                Return Me.newPagePerCustomerField
            End Get
            Set
                Me.newPagePerCustomerField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerFilterType() As String
            Get
                Return Me.customerFilterTypeField
            End Get
            Set
                Me.customerFilterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerFilterValue() As String
            Get
                Return Me.customerFilterValueField
            End Get
            Set
                Me.customerFilterValueField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class CustomerStockCodesSystemInformation
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private cssStyleField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private languageField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private decFormatField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateFormatField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private companyIdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private companyNameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operatorCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operatorNameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operatorGroupField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operatorEmailAddressField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operatorLocationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reportDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private useSeparatorOnReportsField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private separatorToUseField As String
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CssStyle() As String
            Get
                Return Me.cssStyleField
            End Get
            Set
                Me.cssStyleField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Language() As String
            Get
                Return Me.languageField
            End Get
            Set
                Me.languageField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property DecFormat() As String
            Get
                Return Me.decFormatField
            End Get
            Set
                Me.decFormatField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property DateFormat() As String
            Get
                Return Me.dateFormatField
            End Get
            Set
                Me.dateFormatField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Version() As String
            Get
                Return Me.versionField
            End Get
            Set
                Me.versionField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CompanyId() As String
            Get
                Return Me.companyIdField
            End Get
            Set
                Me.companyIdField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CompanyName() As String
            Get
                Return Me.companyNameField
            End Get
            Set
                Me.companyNameField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property OperatorCode() As String
            Get
                Return Me.operatorCodeField
            End Get
            Set
                Me.operatorCodeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property OperatorName() As String
            Get
                Return Me.operatorNameField
            End Get
            Set
                Me.operatorNameField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property OperatorGroup() As String
            Get
                Return Me.operatorGroupField
            End Get
            Set
                Me.operatorGroupField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property OperatorEmailAddress() As String
            Get
                Return Me.operatorEmailAddressField
            End Get
            Set
                Me.operatorEmailAddressField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property OperatorLocation() As String
            Get
                Return Me.operatorLocationField
            End Get
            Set
                Me.operatorLocationField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property ReportDate() As String
            Get
                Return Me.reportDateField
            End Get
            Set
                Me.reportDateField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property UseSeparatorOnReports() As String
            Get
                Return Me.useSeparatorOnReportsField
            End Get
            Set
                Me.useSeparatorOnReportsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SeparatorToUse() As String
            Get
                Return Me.separatorToUseField
            End Get
            Set
                Me.separatorToUseField = value
            End Set
        End Property
    End Class
End Namespace
