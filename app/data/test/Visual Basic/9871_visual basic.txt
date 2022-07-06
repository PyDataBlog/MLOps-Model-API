Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace SalesOrdersPrimaryQuery.DispatchNoteValuationQuery.Output
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class OrderValues
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private systemInformationField As OrderValuesSystemInformation
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private orderField As OrderValuesOrder
        
        Public Sub New()
            MyBase.New
            If (Me.orderField Is Nothing) Then
                Me.orderField = New OrderValuesOrder
            End If
            If (Me.systemInformationField Is Nothing) Then
                Me.systemInformationField = New OrderValuesSystemInformation
            End If
        End Sub
        
        Public Property SystemInformation() As OrderValuesSystemInformation
            Get
                Return Me.systemInformationField
            End Get
            Set
                Me.systemInformationField = value
            End Set
        End Property
        
        Public Property Order() As OrderValuesOrder
            Get
                Return Me.orderField
            End Get
            Set
                Me.orderField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class OrderValuesSystemInformation
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private cssStyleField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private languageField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private decFormatField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateFormatField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private roleField As String
        
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
        
        Public Property CssStyle() As String
            Get
                Return Me.cssStyleField
            End Get
            Set
                Me.cssStyleField = value
            End Set
        End Property
        
        Public Property Language() As String
            Get
                Return Me.languageField
            End Get
            Set
                Me.languageField = value
            End Set
        End Property
        
        Public Property DecFormat() As String
            Get
                Return Me.decFormatField
            End Get
            Set
                Me.decFormatField = value
            End Set
        End Property
        
        Public Property DateFormat() As String
            Get
                Return Me.dateFormatField
            End Get
            Set
                Me.dateFormatField = value
            End Set
        End Property
        
        Public Property Role() As String
            Get
                Return Me.roleField
            End Get
            Set
                Me.roleField = value
            End Set
        End Property
        
        Public Property Version() As String
            Get
                Return Me.versionField
            End Get
            Set
                Me.versionField = value
            End Set
        End Property
        
        Public Property CompanyId() As String
            Get
                Return Me.companyIdField
            End Get
            Set
                Me.companyIdField = value
            End Set
        End Property
        
        Public Property CompanyName() As String
            Get
                Return Me.companyNameField
            End Get
            Set
                Me.companyNameField = value
            End Set
        End Property
        
        Public Property OperatorCode() As String
            Get
                Return Me.operatorCodeField
            End Get
            Set
                Me.operatorCodeField = value
            End Set
        End Property
        
        Public Property OperatorName() As String
            Get
                Return Me.operatorNameField
            End Get
            Set
                Me.operatorNameField = value
            End Set
        End Property
        
        Public Property OperatorGroup() As String
            Get
                Return Me.operatorGroupField
            End Get
            Set
                Me.operatorGroupField = value
            End Set
        End Property
        
        Public Property OperatorEmailAddress() As String
            Get
                Return Me.operatorEmailAddressField
            End Get
            Set
                Me.operatorEmailAddressField = value
            End Set
        End Property
        
        Public Property OperatorLocation() As String
            Get
                Return Me.operatorLocationField
            End Get
            Set
                Me.operatorLocationField = value
            End Set
        End Property
        
        Public Property ReportDate() As String
            Get
                Return Me.reportDateField
            End Get
            Set
                Me.reportDateField = value
            End Set
        End Property
        
        Public Property UseSeparatorOnReports() As String
            Get
                Return Me.useSeparatorOnReportsField
            End Get
            Set
                Me.useSeparatorOnReportsField = value
            End Set
        End Property
        
        Public Property SeparatorToUse() As String
            Get
                Return Me.separatorToUseField
            End Get
            Set
                Me.separatorToUseField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class OrderValuesOrder
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dispatchNoteField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currencyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private taxInclusiveField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private localCurrencyField As OrderValuesOrderLocalCurrency
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private orderCurrencyField As OrderValuesOrderOrderCurrency
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private outstMassShipField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private outstVolumeShipField As String
        
        Public Sub New()
            MyBase.New
            If (Me.orderCurrencyField Is Nothing) Then
                Me.orderCurrencyField = New OrderValuesOrderOrderCurrency
            End If
            If (Me.localCurrencyField Is Nothing) Then
                Me.localCurrencyField = New OrderValuesOrderLocalCurrency
            End If
        End Sub
        
        Public Property DispatchNote() As String
            Get
                Return Me.dispatchNoteField
            End Get
            Set
                Me.dispatchNoteField = value
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
        
        Public Property TaxInclusive() As String
            Get
                Return Me.taxInclusiveField
            End Get
            Set
                Me.taxInclusiveField = value
            End Set
        End Property
        
        Public Property LocalCurrency() As OrderValuesOrderLocalCurrency
            Get
                Return Me.localCurrencyField
            End Get
            Set
                Me.localCurrencyField = value
            End Set
        End Property
        
        Public Property OrderCurrency() As OrderValuesOrderOrderCurrency
            Get
                Return Me.orderCurrencyField
            End Get
            Set
                Me.orderCurrencyField = value
            End Set
        End Property
        
        Public Property OutstMassShip() As String
            Get
                Return Me.outstMassShipField
            End Get
            Set
                Me.outstMassShipField = value
            End Set
        End Property
        
        Public Property OutstVolumeShip() As String
            Get
                Return Me.outstVolumeShipField
            End Get
            Set
                Me.outstVolumeShipField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class OrderValuesOrderLocalCurrency
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceValuesField As OrderValuesOrderLocalCurrencyInvoiceValues
        
        Public Sub New()
            MyBase.New
            If (Me.invoiceValuesField Is Nothing) Then
                Me.invoiceValuesField = New OrderValuesOrderLocalCurrencyInvoiceValues
            End If
        End Sub
        
        Public Property InvoiceValues() As OrderValuesOrderLocalCurrencyInvoiceValues
            Get
                Return Me.invoiceValuesField
            End Get
            Set
                Me.invoiceValuesField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class OrderValuesOrderLocalCurrencyInvoiceValues
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netMerchValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netMerchTotalField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netFreightValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netMiscValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private grossMerchValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private discMerchValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceDiscField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private gstValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private taxValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceTotalField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netMerchValueExclField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netMerchTotalExclField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netFreightValueExclField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netMiscValueExclField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private grossMerchValueExclField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private discMerchValueExclField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceDiscExclField As String
        
        Public Property NetMerchValue() As String
            Get
                Return Me.netMerchValueField
            End Get
            Set
                Me.netMerchValueField = value
            End Set
        End Property
        
        Public Property NetMerchTotal() As String
            Get
                Return Me.netMerchTotalField
            End Get
            Set
                Me.netMerchTotalField = value
            End Set
        End Property
        
        Public Property NetFreightValue() As String
            Get
                Return Me.netFreightValueField
            End Get
            Set
                Me.netFreightValueField = value
            End Set
        End Property
        
        Public Property NetMiscValue() As String
            Get
                Return Me.netMiscValueField
            End Get
            Set
                Me.netMiscValueField = value
            End Set
        End Property
        
        Public Property GrossMerchValue() As String
            Get
                Return Me.grossMerchValueField
            End Get
            Set
                Me.grossMerchValueField = value
            End Set
        End Property
        
        Public Property DiscMerchValue() As String
            Get
                Return Me.discMerchValueField
            End Get
            Set
                Me.discMerchValueField = value
            End Set
        End Property
        
        Public Property InvoiceDisc() As String
            Get
                Return Me.invoiceDiscField
            End Get
            Set
                Me.invoiceDiscField = value
            End Set
        End Property
        
        Public Property GstValue() As String
            Get
                Return Me.gstValueField
            End Get
            Set
                Me.gstValueField = value
            End Set
        End Property
        
        Public Property TaxValue() As String
            Get
                Return Me.taxValueField
            End Get
            Set
                Me.taxValueField = value
            End Set
        End Property
        
        Public Property InvoiceTotal() As String
            Get
                Return Me.invoiceTotalField
            End Get
            Set
                Me.invoiceTotalField = value
            End Set
        End Property
        
        Public Property NetMerchValueExcl() As String
            Get
                Return Me.netMerchValueExclField
            End Get
            Set
                Me.netMerchValueExclField = value
            End Set
        End Property
        
        Public Property NetMerchTotalExcl() As String
            Get
                Return Me.netMerchTotalExclField
            End Get
            Set
                Me.netMerchTotalExclField = value
            End Set
        End Property
        
        Public Property NetFreightValueExcl() As String
            Get
                Return Me.netFreightValueExclField
            End Get
            Set
                Me.netFreightValueExclField = value
            End Set
        End Property
        
        Public Property NetMiscValueExcl() As String
            Get
                Return Me.netMiscValueExclField
            End Get
            Set
                Me.netMiscValueExclField = value
            End Set
        End Property
        
        Public Property GrossMerchValueExcl() As String
            Get
                Return Me.grossMerchValueExclField
            End Get
            Set
                Me.grossMerchValueExclField = value
            End Set
        End Property
        
        Public Property DiscMerchValueExcl() As String
            Get
                Return Me.discMerchValueExclField
            End Get
            Set
                Me.discMerchValueExclField = value
            End Set
        End Property
        
        Public Property InvoiceDiscExcl() As String
            Get
                Return Me.invoiceDiscExclField
            End Get
            Set
                Me.invoiceDiscExclField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class OrderValuesOrderOrderCurrency
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceValuesField As OrderValuesOrderOrderCurrencyInvoiceValues
        
        Public Sub New()
            MyBase.New
            If (Me.invoiceValuesField Is Nothing) Then
                Me.invoiceValuesField = New OrderValuesOrderOrderCurrencyInvoiceValues
            End If
        End Sub
        
        Public Property InvoiceValues() As OrderValuesOrderOrderCurrencyInvoiceValues
            Get
                Return Me.invoiceValuesField
            End Get
            Set
                Me.invoiceValuesField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class OrderValuesOrderOrderCurrencyInvoiceValues
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netMerchValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netMerchTotalField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netFreightValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netMiscValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private grossMerchValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private discMerchValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceDiscField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private gstValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private taxValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceTotalField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netMerchValueExclField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netMerchTotalExclField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netFreightValueExclField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private netMiscValueExclField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private grossMerchValueExclField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private discMerchValueExclField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceDiscExclField As String
        
        Public Property NetMerchValue() As String
            Get
                Return Me.netMerchValueField
            End Get
            Set
                Me.netMerchValueField = value
            End Set
        End Property
        
        Public Property NetMerchTotal() As String
            Get
                Return Me.netMerchTotalField
            End Get
            Set
                Me.netMerchTotalField = value
            End Set
        End Property
        
        Public Property NetFreightValue() As String
            Get
                Return Me.netFreightValueField
            End Get
            Set
                Me.netFreightValueField = value
            End Set
        End Property
        
        Public Property NetMiscValue() As String
            Get
                Return Me.netMiscValueField
            End Get
            Set
                Me.netMiscValueField = value
            End Set
        End Property
        
        Public Property GrossMerchValue() As String
            Get
                Return Me.grossMerchValueField
            End Get
            Set
                Me.grossMerchValueField = value
            End Set
        End Property
        
        Public Property DiscMerchValue() As String
            Get
                Return Me.discMerchValueField
            End Get
            Set
                Me.discMerchValueField = value
            End Set
        End Property
        
        Public Property InvoiceDisc() As String
            Get
                Return Me.invoiceDiscField
            End Get
            Set
                Me.invoiceDiscField = value
            End Set
        End Property
        
        Public Property GstValue() As String
            Get
                Return Me.gstValueField
            End Get
            Set
                Me.gstValueField = value
            End Set
        End Property
        
        Public Property TaxValue() As String
            Get
                Return Me.taxValueField
            End Get
            Set
                Me.taxValueField = value
            End Set
        End Property
        
        Public Property InvoiceTotal() As String
            Get
                Return Me.invoiceTotalField
            End Get
            Set
                Me.invoiceTotalField = value
            End Set
        End Property
        
        Public Property NetMerchValueExcl() As String
            Get
                Return Me.netMerchValueExclField
            End Get
            Set
                Me.netMerchValueExclField = value
            End Set
        End Property
        
        Public Property NetMerchTotalExcl() As String
            Get
                Return Me.netMerchTotalExclField
            End Get
            Set
                Me.netMerchTotalExclField = value
            End Set
        End Property
        
        Public Property NetFreightValueExcl() As String
            Get
                Return Me.netFreightValueExclField
            End Get
            Set
                Me.netFreightValueExclField = value
            End Set
        End Property
        
        Public Property NetMiscValueExcl() As String
            Get
                Return Me.netMiscValueExclField
            End Get
            Set
                Me.netMiscValueExclField = value
            End Set
        End Property
        
        Public Property GrossMerchValueExcl() As String
            Get
                Return Me.grossMerchValueExclField
            End Get
            Set
                Me.grossMerchValueExclField = value
            End Set
        End Property
        
        Public Property DiscMerchValueExcl() As String
            Get
                Return Me.discMerchValueExclField
            End Get
            Set
                Me.discMerchValueExclField = value
            End Set
        End Property
        
        Public Property InvoiceDiscExcl() As String
            Get
                Return Me.invoiceDiscExclField
            End Get
            Set
                Me.invoiceDiscExclField = value
            End Set
        End Property
    End Class
End Namespace
