Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace ARPrimaryQuery.CreditManagementQuery.Output
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class ARCreditorsReport
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private systemInformationField As ARCreditorsReportSystemInformation
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private queryOptionsField As ARCreditorsReportQueryOptions
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerItemField As ARCreditorsReportCustomerItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private branchesField As ARCreditorsReportBranches
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesPersonsField As ARCreditorsReportSalesPersons
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private companyTotalsField As ARCreditorsReportCompanyTotals
        
        Public Sub New()
            MyBase.New
            If (Me.companyTotalsField Is Nothing) Then
                Me.companyTotalsField = New ARCreditorsReportCompanyTotals
            End If
            If (Me.salesPersonsField Is Nothing) Then
                Me.salesPersonsField = New ARCreditorsReportSalesPersons
            End If
            If (Me.branchesField Is Nothing) Then
                Me.branchesField = New ARCreditorsReportBranches
            End If
            If (Me.customerItemField Is Nothing) Then
                Me.customerItemField = New ARCreditorsReportCustomerItem
            End If
            If (Me.queryOptionsField Is Nothing) Then
                Me.queryOptionsField = New ARCreditorsReportQueryOptions
            End If
            If (Me.systemInformationField Is Nothing) Then
                Me.systemInformationField = New ARCreditorsReportSystemInformation
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SystemInformation() As ARCreditorsReportSystemInformation
            Get
                Return Me.systemInformationField
            End Get
            Set
                Me.systemInformationField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property QueryOptions() As ARCreditorsReportQueryOptions
            Get
                Return Me.queryOptionsField
            End Get
            Set
                Me.queryOptionsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerItem() As ARCreditorsReportCustomerItem
            Get
                Return Me.customerItemField
            End Get
            Set
                Me.customerItemField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Branches() As ARCreditorsReportBranches
            Get
                Return Me.branchesField
            End Get
            Set
                Me.branchesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SalesPersons() As ARCreditorsReportSalesPersons
            Get
                Return Me.salesPersonsField
            End Get
            Set
                Me.salesPersonsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CompanyTotals() As ARCreditorsReportCompanyTotals
            Get
                Return Me.companyTotalsField
            End Get
            Set
                Me.companyTotalsField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportSystemInformation
        
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
        Private localCurrencyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private localCurrencyDescField As String
        
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
        Public Property Role() As String
            Get
                Return Me.roleField
            End Get
            Set
                Me.roleField = value
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
        Public Property LocalCurrency() As String
            Get
                Return Me.localCurrencyField
            End Get
            Set
                Me.localCurrencyField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property LocalCurrencyDesc() As String
            Get
                Return Me.localCurrencyDescField
            End Get
            Set
                Me.localCurrencyDescField = value
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
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportQueryOptions
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reportAsOfField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reportsSequenceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customersOnHoldField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerFilterTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerFilterValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private shortnameFilterTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private shortnameFilterValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private branchFilterTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private branchFilterValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeZeroBalanceCustomersField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeFutureInvoicesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includePostDatedChecksField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeRetainedInvoicesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeZeroBalanceCurrentInvoiceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private printFullAddressField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currencyValuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private convertToLocalCurrencyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currencyFilterTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currencyFilterValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageingMethodField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageingDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private useStandardAgeingColumnsField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageColumn1Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageColumn2Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageColumn3Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageColumn4Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageColumn5Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private geographicAreaFilterTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private geographicAreaFilterValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerClassFilterTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerClassFilterValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesPersonFilterTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesPersonFilterValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private balanceTypeSelectionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private minimumBalanceAmountField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerLimitandTermsByField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private overLimitAmountField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private termsCheckingBasedOnField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private daysOverTermsField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private creditNarrationsField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private followupDateFilterTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private followupDateFilterValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private termsFilterTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private termsFilterValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private returnNotesinRtfField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reportTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerDetailField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private tagFileField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private branchSelectionByField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeCrNotAvailField As String
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property ReportAsOf() As String
            Get
                Return Me.reportAsOfField
            End Get
            Set
                Me.reportAsOfField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property ReportsSequence() As String
            Get
                Return Me.reportsSequenceField
            End Get
            Set
                Me.reportsSequenceField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomersOnHold() As String
            Get
                Return Me.customersOnHoldField
            End Get
            Set
                Me.customersOnHoldField = value
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
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property ShortnameFilterType() As String
            Get
                Return Me.shortnameFilterTypeField
            End Get
            Set
                Me.shortnameFilterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property ShortnameFilterValue() As String
            Get
                Return Me.shortnameFilterValueField
            End Get
            Set
                Me.shortnameFilterValueField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property BranchFilterType() As String
            Get
                Return Me.branchFilterTypeField
            End Get
            Set
                Me.branchFilterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property BranchFilterValue() As String
            Get
                Return Me.branchFilterValueField
            End Get
            Set
                Me.branchFilterValueField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property IncludeZeroBalanceCustomers() As String
            Get
                Return Me.includeZeroBalanceCustomersField
            End Get
            Set
                Me.includeZeroBalanceCustomersField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property IncludeFutureInvoices() As String
            Get
                Return Me.includeFutureInvoicesField
            End Get
            Set
                Me.includeFutureInvoicesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property IncludePostDatedChecks() As String
            Get
                Return Me.includePostDatedChecksField
            End Get
            Set
                Me.includePostDatedChecksField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property IncludeRetainedInvoices() As String
            Get
                Return Me.includeRetainedInvoicesField
            End Get
            Set
                Me.includeRetainedInvoicesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property IncludeZeroBalanceCurrentInvoice() As String
            Get
                Return Me.includeZeroBalanceCurrentInvoiceField
            End Get
            Set
                Me.includeZeroBalanceCurrentInvoiceField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property PrintFullAddress() As String
            Get
                Return Me.printFullAddressField
            End Get
            Set
                Me.printFullAddressField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CurrencyValues() As String
            Get
                Return Me.currencyValuesField
            End Get
            Set
                Me.currencyValuesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property ConvertToLocalCurrency() As String
            Get
                Return Me.convertToLocalCurrencyField
            End Get
            Set
                Me.convertToLocalCurrencyField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CurrencyFilterType() As String
            Get
                Return Me.currencyFilterTypeField
            End Get
            Set
                Me.currencyFilterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CurrencyFilterValue() As String
            Get
                Return Me.currencyFilterValueField
            End Get
            Set
                Me.currencyFilterValueField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property AgeingMethod() As String
            Get
                Return Me.ageingMethodField
            End Get
            Set
                Me.ageingMethodField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property AgeingDate() As String
            Get
                Return Me.ageingDateField
            End Get
            Set
                Me.ageingDateField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property UseStandardAgeingColumns() As String
            Get
                Return Me.useStandardAgeingColumnsField
            End Get
            Set
                Me.useStandardAgeingColumnsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property AgeColumn1() As String
            Get
                Return Me.ageColumn1Field
            End Get
            Set
                Me.ageColumn1Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property AgeColumn2() As String
            Get
                Return Me.ageColumn2Field
            End Get
            Set
                Me.ageColumn2Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property AgeColumn3() As String
            Get
                Return Me.ageColumn3Field
            End Get
            Set
                Me.ageColumn3Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property AgeColumn4() As String
            Get
                Return Me.ageColumn4Field
            End Get
            Set
                Me.ageColumn4Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property AgeColumn5() As String
            Get
                Return Me.ageColumn5Field
            End Get
            Set
                Me.ageColumn5Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property GeographicAreaFilterType() As String
            Get
                Return Me.geographicAreaFilterTypeField
            End Get
            Set
                Me.geographicAreaFilterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property GeographicAreaFilterValue() As String
            Get
                Return Me.geographicAreaFilterValueField
            End Get
            Set
                Me.geographicAreaFilterValueField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerClassFilterType() As String
            Get
                Return Me.customerClassFilterTypeField
            End Get
            Set
                Me.customerClassFilterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerClassFilterValue() As String
            Get
                Return Me.customerClassFilterValueField
            End Get
            Set
                Me.customerClassFilterValueField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SalesPersonFilterType() As String
            Get
                Return Me.salesPersonFilterTypeField
            End Get
            Set
                Me.salesPersonFilterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SalesPersonFilterValue() As String
            Get
                Return Me.salesPersonFilterValueField
            End Get
            Set
                Me.salesPersonFilterValueField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property BalanceTypeSelection() As String
            Get
                Return Me.balanceTypeSelectionField
            End Get
            Set
                Me.balanceTypeSelectionField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property MinimumBalanceAmount() As String
            Get
                Return Me.minimumBalanceAmountField
            End Get
            Set
                Me.minimumBalanceAmountField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerLimitandTermsBy() As String
            Get
                Return Me.customerLimitandTermsByField
            End Get
            Set
                Me.customerLimitandTermsByField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property OverLimitAmount() As String
            Get
                Return Me.overLimitAmountField
            End Get
            Set
                Me.overLimitAmountField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TermsCheckingBasedOn() As String
            Get
                Return Me.termsCheckingBasedOnField
            End Get
            Set
                Me.termsCheckingBasedOnField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property DaysOverTerms() As String
            Get
                Return Me.daysOverTermsField
            End Get
            Set
                Me.daysOverTermsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CreditNarrations() As String
            Get
                Return Me.creditNarrationsField
            End Get
            Set
                Me.creditNarrationsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property FollowupDateFilterType() As String
            Get
                Return Me.followupDateFilterTypeField
            End Get
            Set
                Me.followupDateFilterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property FollowupDateFilterValue() As String
            Get
                Return Me.followupDateFilterValueField
            End Get
            Set
                Me.followupDateFilterValueField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TermsFilterType() As String
            Get
                Return Me.termsFilterTypeField
            End Get
            Set
                Me.termsFilterTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TermsFilterValue() As String
            Get
                Return Me.termsFilterValueField
            End Get
            Set
                Me.termsFilterValueField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property ReturnNotesinRtf() As String
            Get
                Return Me.returnNotesinRtfField
            End Get
            Set
                Me.returnNotesinRtfField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property ReportType() As String
            Get
                Return Me.reportTypeField
            End Get
            Set
                Me.reportTypeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerDetail() As String
            Get
                Return Me.customerDetailField
            End Get
            Set
                Me.customerDetailField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TagFile() As String
            Get
                Return Me.tagFileField
            End Get
            Set
                Me.tagFileField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property BranchSelectionBy() As String
            Get
                Return Me.branchSelectionByField
            End Get
            Set
                Me.branchSelectionByField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property IncludeCrNotAvail() As String
            Get
                Return Me.includeCrNotAvailField
            End Get
            Set
                Me.includeCrNotAvailField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportCustomerItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerHeaderField As ARCreditorsReportCustomerItemCustomerHeader
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoicesField As ARCreditorsReportCustomerItemInvoices
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerTotalsField As ARCreditorsReportCustomerItemCustomerTotals
        
        Public Sub New()
            MyBase.New
            If (Me.customerTotalsField Is Nothing) Then
                Me.customerTotalsField = New ARCreditorsReportCustomerItemCustomerTotals
            End If
            If (Me.invoicesField Is Nothing) Then
                Me.invoicesField = New ARCreditorsReportCustomerItemInvoices
            End If
            If (Me.customerHeaderField Is Nothing) Then
                Me.customerHeaderField = New ARCreditorsReportCustomerItemCustomerHeader
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerHeader() As ARCreditorsReportCustomerItemCustomerHeader
            Get
                Return Me.customerHeaderField
            End Get
            Set
                Me.customerHeaderField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Invoices() As ARCreditorsReportCustomerItemInvoices
            Get
                Return Me.invoicesField
            End Get
            Set
                Me.invoicesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerTotals() As ARCreditorsReportCustomerItemCustomerTotals
            Get
                Return Me.customerTotalsField
            End Get
            Set
                Me.customerTotalsField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportCustomerItemCustomerHeader
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private nameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private shortnameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerOnHoldField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private creditLimitField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private creditStatusField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private specialInstrsField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private telephoneField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private addTelephoneField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private telephoneExtnField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private telexField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private faxField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private contactField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private soldToAddr1Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private soldToAddr2Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private soldToAddr3Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private soldToAddr4Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private soldToAddr5Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private soldPostalCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currencyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currencyDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerClassField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private tBLARC_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private areaField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private sALARE_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesPersonField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private sALSLS_NameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private termsCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private tBLART_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private branchField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private sALBRN_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdSalesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ytdSalesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private prevYearSalesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateLastSaleField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateLastPayField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private numOutstOrdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private outstOrdValField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdPaymValField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private highInvDaysField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private highInvField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerNotesField As ARCreditorsReportCustomerItemCustomerHeaderCustomerNotes
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private managementNotesField As ARCreditorsReportCustomerItemCustomerHeaderManagementNotes
        
        Public Sub New()
            MyBase.New
            If (Me.managementNotesField Is Nothing) Then
                Me.managementNotesField = New ARCreditorsReportCustomerItemCustomerHeaderManagementNotes
            End If
            If (Me.customerNotesField Is Nothing) Then
                Me.customerNotesField = New ARCreditorsReportCustomerItemCustomerHeaderCustomerNotes
            End If
        End Sub
        
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
        Public Property Name() As String
            Get
                Return Me.nameField
            End Get
            Set
                Me.nameField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Shortname() As String
            Get
                Return Me.shortnameField
            End Get
            Set
                Me.shortnameField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerOnHold() As String
            Get
                Return Me.customerOnHoldField
            End Get
            Set
                Me.customerOnHoldField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CreditLimit() As String
            Get
                Return Me.creditLimitField
            End Get
            Set
                Me.creditLimitField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CreditStatus() As String
            Get
                Return Me.creditStatusField
            End Get
            Set
                Me.creditStatusField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SpecialInstrs() As String
            Get
                Return Me.specialInstrsField
            End Get
            Set
                Me.specialInstrsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Telephone() As String
            Get
                Return Me.telephoneField
            End Get
            Set
                Me.telephoneField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property AddTelephone() As String
            Get
                Return Me.addTelephoneField
            End Get
            Set
                Me.addTelephoneField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TelephoneExtn() As String
            Get
                Return Me.telephoneExtnField
            End Get
            Set
                Me.telephoneExtnField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Telex() As String
            Get
                Return Me.telexField
            End Get
            Set
                Me.telexField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Fax() As String
            Get
                Return Me.faxField
            End Get
            Set
                Me.faxField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Contact() As String
            Get
                Return Me.contactField
            End Get
            Set
                Me.contactField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SoldToAddr1() As String
            Get
                Return Me.soldToAddr1Field
            End Get
            Set
                Me.soldToAddr1Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SoldToAddr2() As String
            Get
                Return Me.soldToAddr2Field
            End Get
            Set
                Me.soldToAddr2Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SoldToAddr3() As String
            Get
                Return Me.soldToAddr3Field
            End Get
            Set
                Me.soldToAddr3Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SoldToAddr4() As String
            Get
                Return Me.soldToAddr4Field
            End Get
            Set
                Me.soldToAddr4Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SoldToAddr5() As String
            Get
                Return Me.soldToAddr5Field
            End Get
            Set
                Me.soldToAddr5Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SoldPostalCode() As String
            Get
                Return Me.soldPostalCodeField
            End Get
            Set
                Me.soldPostalCodeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Currency() As String
            Get
                Return Me.currencyField
            End Get
            Set
                Me.currencyField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CurrencyDescription() As String
            Get
                Return Me.currencyDescriptionField
            End Get
            Set
                Me.currencyDescriptionField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerClass() As String
            Get
                Return Me.customerClassField
            End Get
            Set
                Me.customerClassField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TBLARC_Description() As String
            Get
                Return Me.tBLARC_DescriptionField
            End Get
            Set
                Me.tBLARC_DescriptionField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Area() As String
            Get
                Return Me.areaField
            End Get
            Set
                Me.areaField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SALARE_Description() As String
            Get
                Return Me.sALARE_DescriptionField
            End Get
            Set
                Me.sALARE_DescriptionField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SalesPerson() As String
            Get
                Return Me.salesPersonField
            End Get
            Set
                Me.salesPersonField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SALSLS_Name() As String
            Get
                Return Me.sALSLS_NameField
            End Get
            Set
                Me.sALSLS_NameField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TermsCode() As String
            Get
                Return Me.termsCodeField
            End Get
            Set
                Me.termsCodeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TBLART_Description() As String
            Get
                Return Me.tBLART_DescriptionField
            End Get
            Set
                Me.tBLART_DescriptionField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Branch() As String
            Get
                Return Me.branchField
            End Get
            Set
                Me.branchField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SALBRN_Description() As String
            Get
                Return Me.sALBRN_DescriptionField
            End Get
            Set
                Me.sALBRN_DescriptionField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property MtdSales() As String
            Get
                Return Me.mtdSalesField
            End Get
            Set
                Me.mtdSalesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property YtdSales() As String
            Get
                Return Me.ytdSalesField
            End Get
            Set
                Me.ytdSalesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property PrevYearSales() As String
            Get
                Return Me.prevYearSalesField
            End Get
            Set
                Me.prevYearSalesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property DateLastSale() As String
            Get
                Return Me.dateLastSaleField
            End Get
            Set
                Me.dateLastSaleField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property DateLastPay() As String
            Get
                Return Me.dateLastPayField
            End Get
            Set
                Me.dateLastPayField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property NumOutstOrd() As String
            Get
                Return Me.numOutstOrdField
            End Get
            Set
                Me.numOutstOrdField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property OutstOrdVal() As String
            Get
                Return Me.outstOrdValField
            End Get
            Set
                Me.outstOrdValField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property MtdPaymVal() As String
            Get
                Return Me.mtdPaymValField
            End Get
            Set
                Me.mtdPaymValField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property HighInvDays() As String
            Get
                Return Me.highInvDaysField
            End Get
            Set
                Me.highInvDaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property HighInv() As String
            Get
                Return Me.highInvField
            End Get
            Set
                Me.highInvField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerNotes() As ARCreditorsReportCustomerItemCustomerHeaderCustomerNotes
            Get
                Return Me.customerNotesField
            End Get
            Set
                Me.customerNotesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property ManagementNotes() As ARCreditorsReportCustomerItemCustomerHeaderManagementNotes
            Get
                Return Me.managementNotesField
            End Get
            Set
                Me.managementNotesField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportCustomerItemCustomerHeaderCustomerNotes
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateLastModifiedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private textField As List(Of String)
        
        Public Sub New()
            MyBase.New
            If (Me.textField Is Nothing) Then
                Me.textField = New List(Of String)
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property DateLastModified() As String
            Get
                Return Me.dateLastModifiedField
            End Get
            Set
                Me.dateLastModifiedField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("Text", Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TextCollection() As List(Of String)
            Get
                Return Me.textField
            End Get
            Set
                Me.textField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportCustomerItemCustomerHeaderManagementNotes
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateLastModifiedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private followUpDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private isFollowUpDateDueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private textField As List(Of String)
        
        Public Sub New()
            MyBase.New
            If (Me.textField Is Nothing) Then
                Me.textField = New List(Of String)
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property DateLastModified() As String
            Get
                Return Me.dateLastModifiedField
            End Get
            Set
                Me.dateLastModifiedField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property FollowUpDate() As String
            Get
                Return Me.followUpDateField
            End Get
            Set
                Me.followUpDateField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property IsFollowUpDateDue() As String
            Get
                Return Me.isFollowUpDateDueField
            End Get
            Set
                Me.isFollowUpDateDueField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("Text", Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TextCollection() As List(Of String)
            Get
                Return Me.textField
            End Get
            Set
                Me.textField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportCustomerItemInvoices
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceItemField As ARCreditorsReportCustomerItemInvoicesInvoiceItem
        
        Public Sub New()
            MyBase.New
            If (Me.invoiceItemField Is Nothing) Then
                Me.invoiceItemField = New ARCreditorsReportCustomerItemInvoicesInvoiceItem
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property InvoiceItem() As ARCreditorsReportCustomerItemInvoicesInvoiceItem
            Get
                Return Me.invoiceItemField
            End Get
            Set
                Me.invoiceItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportCustomerItemInvoicesInvoiceItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerPoNumberField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private termsCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private tBLART_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currencyValueField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currencyValueFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private discValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currentField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currentFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days30Field As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days30FieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days60Field As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days60FieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days90Field As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days90FieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days120Field As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days120FieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days150Field As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days150FieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private retainedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private futureInvoiceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private transactionsField As List(Of ARCreditorsReportCustomerItemInvoicesInvoiceItemTransactionDetail)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceNotesField As ARCreditorsReportCustomerItemInvoicesInvoiceItemInvoiceNotes
        
        Public Sub New()
            MyBase.New
            If (Me.invoiceNotesField Is Nothing) Then
                Me.invoiceNotesField = New ARCreditorsReportCustomerItemInvoicesInvoiceItemInvoiceNotes
            End If
            If (Me.transactionsField Is Nothing) Then
                Me.transactionsField = New List(Of ARCreditorsReportCustomerItemInvoicesInvoiceItemTransactionDetail)
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Invoice() As String
            Get
                Return Me.invoiceField
            End Get
            Set
                Me.invoiceField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property InvoiceDate() As String
            Get
                Return Me.invoiceDateField
            End Get
            Set
                Me.invoiceDateField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerPoNumber() As String
            Get
                Return Me.customerPoNumberField
            End Get
            Set
                Me.customerPoNumberField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TermsCode() As String
            Get
                Return Me.termsCodeField
            End Get
            Set
                Me.termsCodeField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TBLART_Description() As String
            Get
                Return Me.tBLART_DescriptionField
            End Get
            Set
                Me.tBLART_DescriptionField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CurrencyValue() As Double
            Get
                Return Me.currencyValueField
            End Get
            Set
                Me.currencyValueField = value
                Me.CurrencyValueFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property CurrencyValueSpecified() As Boolean
            Get
                Return Me.currencyValueFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property DiscValue() As String
            Get
                Return Me.discValueField
            End Get
            Set
                Me.discValueField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Current() As Double
            Get
                Return Me.currentField
            End Get
            Set
                Me.currentField = value
                Me.CurrentFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property CurrentSpecified() As Boolean
            Get
                Return Me.currentFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Days30() As Double
            Get
                Return Me.days30Field
            End Get
            Set
                Me.days30Field = value
                Me.Days30FieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Days30Specified() As Boolean
            Get
                Return Me.days30FieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Days60() As Double
            Get
                Return Me.days60Field
            End Get
            Set
                Me.days60Field = value
                Me.Days60FieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Days60Specified() As Boolean
            Get
                Return Me.days60FieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Days90() As Double
            Get
                Return Me.days90Field
            End Get
            Set
                Me.days90Field = value
                Me.Days90FieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Days90Specified() As Boolean
            Get
                Return Me.days90FieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Days120() As Double
            Get
                Return Me.days120Field
            End Get
            Set
                Me.days120Field = value
                Me.Days120FieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Days120Specified() As Boolean
            Get
                Return Me.days120FieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Days150() As Double
            Get
                Return Me.days150Field
            End Get
            Set
                Me.days150Field = value
                Me.Days150FieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Days150Specified() As Boolean
            Get
                Return Me.days150FieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Retained() As String
            Get
                Return Me.retainedField
            End Get
            Set
                Me.retainedField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property FutureInvoice() As String
            Get
                Return Me.futureInvoiceField
            End Get
            Set
                Me.futureInvoiceField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlArrayAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified),  _
         System.Xml.Serialization.XmlArrayItemAttribute("TransactionDetail", Form:=System.Xml.Schema.XmlSchemaForm.Unqualified, IsNullable:=false)>  _
        Public Property TransactionsCollection() As List(Of ARCreditorsReportCustomerItemInvoicesInvoiceItemTransactionDetail)
            Get
                Return Me.transactionsField
            End Get
            Set
                Me.transactionsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property InvoiceNotes() As ARCreditorsReportCustomerItemInvoicesInvoiceItemInvoiceNotes
            Get
                Return Me.invoiceNotesField
            End Get
            Set
                Me.invoiceNotesField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportCustomerItemInvoicesInvoiceItemTransactionDetail
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private transactionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private journalDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private journalField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private referenceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerPurchaseOrderField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private trnValueField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private trnValueFieldSpecified As Boolean
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Transaction() As String
            Get
                Return Me.transactionField
            End Get
            Set
                Me.transactionField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property JournalDate() As String
            Get
                Return Me.journalDateField
            End Get
            Set
                Me.journalDateField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Journal() As String
            Get
                Return Me.journalField
            End Get
            Set
                Me.journalField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Reference() As String
            Get
                Return Me.referenceField
            End Get
            Set
                Me.referenceField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerPurchaseOrder() As String
            Get
                Return Me.customerPurchaseOrderField
            End Get
            Set
                Me.customerPurchaseOrderField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TrnValue() As Double
            Get
                Return Me.trnValueField
            End Get
            Set
                Me.trnValueField = value
                Me.TrnValueFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property TrnValueSpecified() As Boolean
            Get
                Return Me.trnValueFieldSpecified
            End Get
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportCustomerItemInvoicesInvoiceItemInvoiceNotes
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateLastModifiedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private textField As List(Of String)
        
        Public Sub New()
            MyBase.New
            If (Me.textField Is Nothing) Then
                Me.textField = New List(Of String)
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property DateLastModified() As String
            Get
                Return Me.dateLastModifiedField
            End Get
            Set
                Me.dateLastModifiedField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("Text", Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TextCollection() As List(Of String)
            Get
                Return Me.textField
            End Get
            Set
                Me.textField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportCustomerItemCustomerTotals
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currentField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days30Field As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days60Field As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days90Field As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days120Field As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private days150Field As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private discountAvailableField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalsCurrencyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private balanceField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private creditBalanceField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private averageDaysField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceCountErrorField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerBalanceErrorField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerBalErrAmtField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private postDatedChecksField As List(Of ARCreditorsReportCustomerItemCustomerTotalsPostDatedItem)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private availablePostDatedChecksField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalPostDatedChecksField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueofFuturesField As Double
        
        Public Sub New()
            MyBase.New
            If (Me.postDatedChecksField Is Nothing) Then
                Me.postDatedChecksField = New List(Of ARCreditorsReportCustomerItemCustomerTotalsPostDatedItem)
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Current() As Double
            Get
                Return Me.currentField
            End Get
            Set
                Me.currentField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Days30() As Double
            Get
                Return Me.days30Field
            End Get
            Set
                Me.days30Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Days60() As Double
            Get
                Return Me.days60Field
            End Get
            Set
                Me.days60Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Days90() As Double
            Get
                Return Me.days90Field
            End Get
            Set
                Me.days90Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Days120() As Double
            Get
                Return Me.days120Field
            End Get
            Set
                Me.days120Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Days150() As Double
            Get
                Return Me.days150Field
            End Get
            Set
                Me.days150Field = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property DiscountAvailable() As Double
            Get
                Return Me.discountAvailableField
            End Get
            Set
                Me.discountAvailableField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalsCurrency() As String
            Get
                Return Me.totalsCurrencyField
            End Get
            Set
                Me.totalsCurrencyField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Balance() As Double
            Get
                Return Me.balanceField
            End Get
            Set
                Me.balanceField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CreditBalance() As Double
            Get
                Return Me.creditBalanceField
            End Get
            Set
                Me.creditBalanceField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property AverageDays() As String
            Get
                Return Me.averageDaysField
            End Get
            Set
                Me.averageDaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property InvoiceCountError() As Object
            Get
                Return Me.invoiceCountErrorField
            End Get
            Set
                Me.invoiceCountErrorField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerBalanceError() As String
            Get
                Return Me.customerBalanceErrorField
            End Get
            Set
                Me.customerBalanceErrorField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerBalErrAmt() As String
            Get
                Return Me.customerBalErrAmtField
            End Get
            Set
                Me.customerBalErrAmtField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlArrayAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified),  _
         System.Xml.Serialization.XmlArrayItemAttribute("PostDatedItem", Form:=System.Xml.Schema.XmlSchemaForm.Unqualified, IsNullable:=false)>  _
        Public Property PostDatedChecksCollection() As List(Of ARCreditorsReportCustomerItemCustomerTotalsPostDatedItem)
            Get
                Return Me.postDatedChecksField
            End Get
            Set
                Me.postDatedChecksField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property AvailablePostDatedChecks() As String
            Get
                Return Me.availablePostDatedChecksField
            End Get
            Set
                Me.availablePostDatedChecksField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalPostDatedChecks() As String
            Get
                Return Me.totalPostDatedChecksField
            End Get
            Set
                Me.totalPostDatedChecksField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property ValueofFutures() As Double
            Get
                Return Me.valueofFuturesField
            End Get
            Set
                Me.valueofFuturesField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportCustomerItemCustomerTotalsPostDatedItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private checkNumberField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private availableDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private amountField As String
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CheckNumber() As String
            Get
                Return Me.checkNumberField
            End Get
            Set
                Me.checkNumberField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property AvailableDate() As String
            Get
                Return Me.availableDateField
            End Get
            Set
                Me.availableDateField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Amount() As String
            Get
                Return Me.amountField
            End Get
            Set
                Me.amountField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportBranches
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private branchTotalsField As ARCreditorsReportBranchesBranchTotals
        
        Public Sub New()
            MyBase.New
            If (Me.branchTotalsField Is Nothing) Then
                Me.branchTotalsField = New ARCreditorsReportBranchesBranchTotals
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property BranchTotals() As ARCreditorsReportBranchesBranchTotals
            Get
                Return Me.branchTotalsField
            End Get
            Set
                Me.branchTotalsField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportBranchesBranchTotals
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private branchField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private sALBRN_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private companyCurrencyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total150DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total150DaysFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total120DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total120DaysFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total90DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total90DaysFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total60DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total60DaysFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total30DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total30DaysFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalCurrentField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalCurrentFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalDiscountField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalDiscountFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalFutureInvoiceField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalFutureInvoiceFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalBalanceField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalBalanceFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private adjustmentsField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private adjustmentsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private debitsField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private debitsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private creditBalanceField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private creditBalanceFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private paymentsField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private paymentsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private discountAvailableField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private discountAvailableFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalActivityField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalActivityFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private averageDaysField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private varianceInTotalsField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private varianceInTotalsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerTotalControlErrorField As String
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Branch() As String
            Get
                Return Me.branchField
            End Get
            Set
                Me.branchField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SALBRN_Description() As String
            Get
                Return Me.sALBRN_DescriptionField
            End Get
            Set
                Me.sALBRN_DescriptionField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CompanyCurrency() As String
            Get
                Return Me.companyCurrencyField
            End Get
            Set
                Me.companyCurrencyField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total150Days() As Double
            Get
                Return Me.total150DaysField
            End Get
            Set
                Me.total150DaysField = value
                Me.Total150DaysFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Total150DaysSpecified() As Boolean
            Get
                Return Me.total150DaysFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total120Days() As Double
            Get
                Return Me.total120DaysField
            End Get
            Set
                Me.total120DaysField = value
                Me.Total120DaysFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Total120DaysSpecified() As Boolean
            Get
                Return Me.total120DaysFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total90Days() As Double
            Get
                Return Me.total90DaysField
            End Get
            Set
                Me.total90DaysField = value
                Me.Total90DaysFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Total90DaysSpecified() As Boolean
            Get
                Return Me.total90DaysFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total60Days() As Double
            Get
                Return Me.total60DaysField
            End Get
            Set
                Me.total60DaysField = value
                Me.Total60DaysFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Total60DaysSpecified() As Boolean
            Get
                Return Me.total60DaysFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total30Days() As Double
            Get
                Return Me.total30DaysField
            End Get
            Set
                Me.total30DaysField = value
                Me.Total30DaysFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Total30DaysSpecified() As Boolean
            Get
                Return Me.total30DaysFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalCurrent() As Double
            Get
                Return Me.totalCurrentField
            End Get
            Set
                Me.totalCurrentField = value
                Me.TotalCurrentFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property TotalCurrentSpecified() As Boolean
            Get
                Return Me.totalCurrentFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalDiscount() As Double
            Get
                Return Me.totalDiscountField
            End Get
            Set
                Me.totalDiscountField = value
                Me.TotalDiscountFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property TotalDiscountSpecified() As Boolean
            Get
                Return Me.totalDiscountFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalFutureInvoice() As Double
            Get
                Return Me.totalFutureInvoiceField
            End Get
            Set
                Me.totalFutureInvoiceField = value
                Me.TotalFutureInvoiceFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property TotalFutureInvoiceSpecified() As Boolean
            Get
                Return Me.totalFutureInvoiceFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalBalance() As Double
            Get
                Return Me.totalBalanceField
            End Get
            Set
                Me.totalBalanceField = value
                Me.TotalBalanceFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property TotalBalanceSpecified() As Boolean
            Get
                Return Me.totalBalanceFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Adjustments() As Double
            Get
                Return Me.adjustmentsField
            End Get
            Set
                Me.adjustmentsField = value
                Me.AdjustmentsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property AdjustmentsSpecified() As Boolean
            Get
                Return Me.adjustmentsFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Debits() As Double
            Get
                Return Me.debitsField
            End Get
            Set
                Me.debitsField = value
                Me.DebitsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property DebitsSpecified() As Boolean
            Get
                Return Me.debitsFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CreditBalance() As Double
            Get
                Return Me.creditBalanceField
            End Get
            Set
                Me.creditBalanceField = value
                Me.CreditBalanceFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property CreditBalanceSpecified() As Boolean
            Get
                Return Me.creditBalanceFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Payments() As Double
            Get
                Return Me.paymentsField
            End Get
            Set
                Me.paymentsField = value
                Me.PaymentsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property PaymentsSpecified() As Boolean
            Get
                Return Me.paymentsFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property DiscountAvailable() As Double
            Get
                Return Me.discountAvailableField
            End Get
            Set
                Me.discountAvailableField = value
                Me.DiscountAvailableFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property DiscountAvailableSpecified() As Boolean
            Get
                Return Me.discountAvailableFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalActivity() As Double
            Get
                Return Me.totalActivityField
            End Get
            Set
                Me.totalActivityField = value
                Me.TotalActivityFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property TotalActivitySpecified() As Boolean
            Get
                Return Me.totalActivityFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property AverageDays() As String
            Get
                Return Me.averageDaysField
            End Get
            Set
                Me.averageDaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property VarianceInTotals() As Double
            Get
                Return Me.varianceInTotalsField
            End Get
            Set
                Me.varianceInTotalsField = value
                Me.VarianceInTotalsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property VarianceInTotalsSpecified() As Boolean
            Get
                Return Me.varianceInTotalsFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerTotalControlError() As String
            Get
                Return Me.customerTotalControlErrorField
            End Get
            Set
                Me.customerTotalControlErrorField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportSalesPersons
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesPersonTotalsField As ARCreditorsReportSalesPersonsSalesPersonTotals
        
        Public Sub New()
            MyBase.New
            If (Me.salesPersonTotalsField Is Nothing) Then
                Me.salesPersonTotalsField = New ARCreditorsReportSalesPersonsSalesPersonTotals
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SalesPersonTotals() As ARCreditorsReportSalesPersonsSalesPersonTotals
            Get
                Return Me.salesPersonTotalsField
            End Get
            Set
                Me.salesPersonTotalsField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportSalesPersonsSalesPersonTotals
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesPersonField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private sALSLS_NameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private companyCurrencyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total150DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total150DaysFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total120DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total120DaysFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total90DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total90DaysFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total60DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total60DaysFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total30DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total30DaysFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalCurrentField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalCurrentFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalFutureInvoiceField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalFutureInvoiceFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalBalanceField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalBalanceFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private adjustmentsField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private adjustmentsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private debitsField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private debitsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private creditBalanceField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private creditBalanceFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private paymentsField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private paymentsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private discountAvailableField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private discountAvailableFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalActivityField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalActivityFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private averageDaysField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private varianceInTotalsField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private varianceInTotalsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerTotalControlErrorField As String
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SalesPerson() As String
            Get
                Return Me.salesPersonField
            End Get
            Set
                Me.salesPersonField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property SALSLS_Name() As String
            Get
                Return Me.sALSLS_NameField
            End Get
            Set
                Me.sALSLS_NameField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CompanyCurrency() As String
            Get
                Return Me.companyCurrencyField
            End Get
            Set
                Me.companyCurrencyField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total150Days() As Double
            Get
                Return Me.total150DaysField
            End Get
            Set
                Me.total150DaysField = value
                Me.Total150DaysFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Total150DaysSpecified() As Boolean
            Get
                Return Me.total150DaysFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total120Days() As Double
            Get
                Return Me.total120DaysField
            End Get
            Set
                Me.total120DaysField = value
                Me.Total120DaysFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Total120DaysSpecified() As Boolean
            Get
                Return Me.total120DaysFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total90Days() As Double
            Get
                Return Me.total90DaysField
            End Get
            Set
                Me.total90DaysField = value
                Me.Total90DaysFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Total90DaysSpecified() As Boolean
            Get
                Return Me.total90DaysFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total60Days() As Double
            Get
                Return Me.total60DaysField
            End Get
            Set
                Me.total60DaysField = value
                Me.Total60DaysFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Total60DaysSpecified() As Boolean
            Get
                Return Me.total60DaysFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total30Days() As Double
            Get
                Return Me.total30DaysField
            End Get
            Set
                Me.total30DaysField = value
                Me.Total30DaysFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property Total30DaysSpecified() As Boolean
            Get
                Return Me.total30DaysFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalCurrent() As Double
            Get
                Return Me.totalCurrentField
            End Get
            Set
                Me.totalCurrentField = value
                Me.TotalCurrentFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property TotalCurrentSpecified() As Boolean
            Get
                Return Me.totalCurrentFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalFutureInvoice() As Double
            Get
                Return Me.totalFutureInvoiceField
            End Get
            Set
                Me.totalFutureInvoiceField = value
                Me.TotalFutureInvoiceFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property TotalFutureInvoiceSpecified() As Boolean
            Get
                Return Me.totalFutureInvoiceFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalBalance() As Double
            Get
                Return Me.totalBalanceField
            End Get
            Set
                Me.totalBalanceField = value
                Me.TotalBalanceFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property TotalBalanceSpecified() As Boolean
            Get
                Return Me.totalBalanceFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Adjustments() As Double
            Get
                Return Me.adjustmentsField
            End Get
            Set
                Me.adjustmentsField = value
                Me.AdjustmentsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property AdjustmentsSpecified() As Boolean
            Get
                Return Me.adjustmentsFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Debits() As Double
            Get
                Return Me.debitsField
            End Get
            Set
                Me.debitsField = value
                Me.DebitsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property DebitsSpecified() As Boolean
            Get
                Return Me.debitsFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CreditBalance() As Double
            Get
                Return Me.creditBalanceField
            End Get
            Set
                Me.creditBalanceField = value
                Me.CreditBalanceFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property CreditBalanceSpecified() As Boolean
            Get
                Return Me.creditBalanceFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Payments() As Double
            Get
                Return Me.paymentsField
            End Get
            Set
                Me.paymentsField = value
                Me.PaymentsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property PaymentsSpecified() As Boolean
            Get
                Return Me.paymentsFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property DiscountAvailable() As Double
            Get
                Return Me.discountAvailableField
            End Get
            Set
                Me.discountAvailableField = value
                Me.DiscountAvailableFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property DiscountAvailableSpecified() As Boolean
            Get
                Return Me.discountAvailableFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalActivity() As Double
            Get
                Return Me.totalActivityField
            End Get
            Set
                Me.totalActivityField = value
                Me.TotalActivityFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property TotalActivitySpecified() As Boolean
            Get
                Return Me.totalActivityFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property AverageDays() As String
            Get
                Return Me.averageDaysField
            End Get
            Set
                Me.averageDaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property VarianceInTotals() As Double
            Get
                Return Me.varianceInTotalsField
            End Get
            Set
                Me.varianceInTotalsField = value
                Me.VarianceInTotalsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property VarianceInTotalsSpecified() As Boolean
            Get
                Return Me.varianceInTotalsFieldSpecified
            End Get
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerTotalControlError() As String
            Get
                Return Me.customerTotalControlErrorField
            End Get
            Set
                Me.customerTotalControlErrorField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class ARCreditorsReportCompanyTotals
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private companyCurrencyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalCustomersField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private balanceBroughtForwardField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total150DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private percentage150DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total120DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private percentage120DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total90DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private percentage90DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total60DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private percentage60DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private total30DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private percentage30DaysField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalCurrentField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private percentageCurrentField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalFutureInvoiceField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private percentageFutureInvoiceField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalDiscountField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private percentageDiscountAvailableField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalBalanceField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdSalesField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdIBTField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdTaxField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdAdjustmentField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdDebitsField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdCreditsField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdPaymentsField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdDiscountsField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdErrCorField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdActivityField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private varianceInTotalsField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private averageDaysField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private controlBalanceField As Double
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerTotalControlErrorField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private nextArJnlField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private prevNextArJnlField As String
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CompanyCurrency() As String
            Get
                Return Me.companyCurrencyField
            End Get
            Set
                Me.companyCurrencyField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalCustomers() As Double
            Get
                Return Me.totalCustomersField
            End Get
            Set
                Me.totalCustomersField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property BalanceBroughtForward() As Double
            Get
                Return Me.balanceBroughtForwardField
            End Get
            Set
                Me.balanceBroughtForwardField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total150Days() As Double
            Get
                Return Me.total150DaysField
            End Get
            Set
                Me.total150DaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Percentage150Days() As Double
            Get
                Return Me.percentage150DaysField
            End Get
            Set
                Me.percentage150DaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total120Days() As Double
            Get
                Return Me.total120DaysField
            End Get
            Set
                Me.total120DaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Percentage120Days() As Double
            Get
                Return Me.percentage120DaysField
            End Get
            Set
                Me.percentage120DaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total90Days() As Double
            Get
                Return Me.total90DaysField
            End Get
            Set
                Me.total90DaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Percentage90Days() As Double
            Get
                Return Me.percentage90DaysField
            End Get
            Set
                Me.percentage90DaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total60Days() As Double
            Get
                Return Me.total60DaysField
            End Get
            Set
                Me.total60DaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Percentage60Days() As Double
            Get
                Return Me.percentage60DaysField
            End Get
            Set
                Me.percentage60DaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Total30Days() As Double
            Get
                Return Me.total30DaysField
            End Get
            Set
                Me.total30DaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property Percentage30Days() As Double
            Get
                Return Me.percentage30DaysField
            End Get
            Set
                Me.percentage30DaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalCurrent() As Double
            Get
                Return Me.totalCurrentField
            End Get
            Set
                Me.totalCurrentField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property PercentageCurrent() As Double
            Get
                Return Me.percentageCurrentField
            End Get
            Set
                Me.percentageCurrentField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalFutureInvoice() As Double
            Get
                Return Me.totalFutureInvoiceField
            End Get
            Set
                Me.totalFutureInvoiceField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property PercentageFutureInvoice() As Double
            Get
                Return Me.percentageFutureInvoiceField
            End Get
            Set
                Me.percentageFutureInvoiceField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalDiscount() As Double
            Get
                Return Me.totalDiscountField
            End Get
            Set
                Me.totalDiscountField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property PercentageDiscountAvailable() As Double
            Get
                Return Me.percentageDiscountAvailableField
            End Get
            Set
                Me.percentageDiscountAvailableField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property TotalBalance() As Double
            Get
                Return Me.totalBalanceField
            End Get
            Set
                Me.totalBalanceField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property MtdSales() As Double
            Get
                Return Me.mtdSalesField
            End Get
            Set
                Me.mtdSalesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property MtdIBT() As Double
            Get
                Return Me.mtdIBTField
            End Get
            Set
                Me.mtdIBTField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property MtdTax() As Double
            Get
                Return Me.mtdTaxField
            End Get
            Set
                Me.mtdTaxField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property MtdAdjustment() As Double
            Get
                Return Me.mtdAdjustmentField
            End Get
            Set
                Me.mtdAdjustmentField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property MtdDebits() As Double
            Get
                Return Me.mtdDebitsField
            End Get
            Set
                Me.mtdDebitsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property MtdCredits() As Double
            Get
                Return Me.mtdCreditsField
            End Get
            Set
                Me.mtdCreditsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property MtdPayments() As Double
            Get
                Return Me.mtdPaymentsField
            End Get
            Set
                Me.mtdPaymentsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property MtdDiscounts() As Double
            Get
                Return Me.mtdDiscountsField
            End Get
            Set
                Me.mtdDiscountsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property MtdErrCor() As Double
            Get
                Return Me.mtdErrCorField
            End Get
            Set
                Me.mtdErrCorField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property MtdActivity() As Double
            Get
                Return Me.mtdActivityField
            End Get
            Set
                Me.mtdActivityField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property VarianceInTotals() As Double
            Get
                Return Me.varianceInTotalsField
            End Get
            Set
                Me.varianceInTotalsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property AverageDays() As String
            Get
                Return Me.averageDaysField
            End Get
            Set
                Me.averageDaysField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property ControlBalance() As Double
            Get
                Return Me.controlBalanceField
            End Get
            Set
                Me.controlBalanceField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property CustomerTotalControlError() As String
            Get
                Return Me.customerTotalControlErrorField
            End Get
            Set
                Me.customerTotalControlErrorField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property NextArJnl() As String
            Get
                Return Me.nextArJnlField
            End Get
            Set
                Me.nextArJnlField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(Form:=System.Xml.Schema.XmlSchemaForm.Unqualified)>  _
        Public Property PrevNextArJnl() As String
            Get
                Return Me.prevNextArJnlField
            End Get
            Set
                Me.prevNextArJnlField = value
            End Set
        End Property
    End Class
End Namespace
