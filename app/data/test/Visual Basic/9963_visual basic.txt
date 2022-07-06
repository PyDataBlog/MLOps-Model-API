Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace WipPrimaryQuery.JobStatusQuery.Output
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class WipStatusQuery
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private systemInformationField As WipStatusQuerySystemInformation
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobField As WipStatusQueryJob
        
        Public Sub New()
            MyBase.New
            If (Me.jobField Is Nothing) Then
                Me.jobField = New WipStatusQueryJob
            End If
            If (Me.systemInformationField Is Nothing) Then
                Me.systemInformationField = New WipStatusQuerySystemInformation
            End If
        End Sub
        
        Public Property SystemInformation() As WipStatusQuerySystemInformation
            Get
                Return Me.systemInformationField
            End Get
            Set
                Me.systemInformationField = value
            End Set
        End Property
        
        Public Property Job() As WipStatusQueryJob
            Get
                Return Me.jobField
            End Get
            Set
                Me.jobField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQuerySystemInformation
        
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
    Partial Public Class WipStatusQueryJob
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private masterField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobNumberField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobClassificationField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private classDescriptionField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private longDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iNVWHC_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerNameField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDeliveryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobStartDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private completeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expLabourField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expMaterialField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyToMakeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOutstandingField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyManufacturedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private routeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private bOMROU_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private confirmedFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private wipCtlGlCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private materialAllocationField As List(Of WipStatusQueryJobMaterialAllocation)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationItemField As List(Of WipStatusQueryJobOperationItem)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobField As WipStatusQueryJobJob
        
        Public Sub New()
            MyBase.New
            If (Me.jobField Is Nothing) Then
                Me.jobField = New WipStatusQueryJobJob
            End If
            If (Me.operationItemField Is Nothing) Then
                Me.operationItemField = New List(Of WipStatusQueryJobOperationItem)
            End If
            If (Me.materialAllocationField Is Nothing) Then
                Me.materialAllocationField = New List(Of WipStatusQueryJobMaterialAllocation)
            End If
        End Sub
        
        Public Property Master() As String
            Get
                Return Me.masterField
            End Get
            Set
                Me.masterField = value
            End Set
        End Property
        
        Public Property JobNumber() As String
            Get
                Return Me.jobNumberField
            End Get
            Set
                Me.jobNumberField = value
            End Set
        End Property
        
        Public Property JobDescription() As String
            Get
                Return Me.jobDescriptionField
            End Get
            Set
                Me.jobDescriptionField = value
            End Set
        End Property
        
        Public Property JobClassification() As Object
            Get
                Return Me.jobClassificationField
            End Get
            Set
                Me.jobClassificationField = value
            End Set
        End Property
        
        Public Property ClassDescription() As Object
            Get
                Return Me.classDescriptionField
            End Get
            Set
                Me.classDescriptionField = value
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
        
        Public Property StockDescription() As String
            Get
                Return Me.stockDescriptionField
            End Get
            Set
                Me.stockDescriptionField = value
            End Set
        End Property
        
        Public Property LongDesc() As String
            Get
                Return Me.longDescField
            End Get
            Set
                Me.longDescField = value
            End Set
        End Property
        
        Public Property StockUom() As String
            Get
                Return Me.stockUomField
            End Get
            Set
                Me.stockUomField = value
            End Set
        End Property
        
        Public Property Version() As Object
            Get
                Return Me.versionField
            End Get
            Set
                Me.versionField = value
            End Set
        End Property
        
        Public Property Release() As Object
            Get
                Return Me.releaseField
            End Get
            Set
                Me.releaseField = value
            End Set
        End Property
        
        Public Property Warehouse() As String
            Get
                Return Me.warehouseField
            End Get
            Set
                Me.warehouseField = value
            End Set
        End Property
        
        Public Property INVWHC_Description() As String
            Get
                Return Me.iNVWHC_DescriptionField
            End Get
            Set
                Me.iNVWHC_DescriptionField = value
            End Set
        End Property
        
        Public Property Customer() As Object
            Get
                Return Me.customerField
            End Get
            Set
                Me.customerField = value
            End Set
        End Property
        
        Public Property CustomerName() As Object
            Get
                Return Me.customerNameField
            End Get
            Set
                Me.customerNameField = value
            End Set
        End Property
        
        Public Property JobDeliveryDate() As String
            Get
                Return Me.jobDeliveryDateField
            End Get
            Set
                Me.jobDeliveryDateField = value
            End Set
        End Property
        
        Public Property JobStartDate() As String
            Get
                Return Me.jobStartDateField
            End Get
            Set
                Me.jobStartDateField = value
            End Set
        End Property
        
        Public Property Complete() As String
            Get
                Return Me.completeField
            End Get
            Set
                Me.completeField = value
            End Set
        End Property
        
        Public Property ExpLabour() As String
            Get
                Return Me.expLabourField
            End Get
            Set
                Me.expLabourField = value
            End Set
        End Property
        
        Public Property ExpMaterial() As String
            Get
                Return Me.expMaterialField
            End Get
            Set
                Me.expMaterialField = value
            End Set
        End Property
        
        Public Property QtyToMake() As String
            Get
                Return Me.qtyToMakeField
            End Get
            Set
                Me.qtyToMakeField = value
            End Set
        End Property
        
        Public Property QtyOutstanding() As String
            Get
                Return Me.qtyOutstandingField
            End Get
            Set
                Me.qtyOutstandingField = value
            End Set
        End Property
        
        Public Property QtyManufactured() As String
            Get
                Return Me.qtyManufacturedField
            End Get
            Set
                Me.qtyManufacturedField = value
            End Set
        End Property
        
        Public Property Route() As String
            Get
                Return Me.routeField
            End Get
            Set
                Me.routeField = value
            End Set
        End Property
        
        Public Property BOMROU_Description() As String
            Get
                Return Me.bOMROU_DescriptionField
            End Get
            Set
                Me.bOMROU_DescriptionField = value
            End Set
        End Property
        
        Public Property ConfirmedFlag() As String
            Get
                Return Me.confirmedFlagField
            End Get
            Set
                Me.confirmedFlagField = value
            End Set
        End Property
        
        Public Property WipCtlGlCode() As String
            Get
                Return Me.wipCtlGlCodeField
            End Get
            Set
                Me.wipCtlGlCodeField = value
            End Set
        End Property
        
        Public Property LabCostToDate() As String
            Get
                Return Me.labCostToDateField
            End Get
            Set
                Me.labCostToDateField = value
            End Set
        End Property
        
        Public Property MatCostToDate() As String
            Get
                Return Me.matCostToDateField
            End Get
            Set
                Me.matCostToDateField = value
            End Set
        End Property
        
        Public Property LabValueIssues() As String
            Get
                Return Me.labValueIssuesField
            End Get
            Set
                Me.labValueIssuesField = value
            End Set
        End Property
        
        Public Property MatValueIssues() As String
            Get
                Return Me.matValueIssuesField
            End Get
            Set
                Me.matValueIssuesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("MaterialAllocation")>  _
        Public Property MaterialAllocationCollection() As List(Of WipStatusQueryJobMaterialAllocation)
            Get
                Return Me.materialAllocationField
            End Get
            Set
                Me.materialAllocationField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("OperationItem")>  _
        Public Property OperationItemCollection() As List(Of WipStatusQueryJobOperationItem)
            Get
                Return Me.operationItemField
            End Get
            Set
                Me.operationItemField = value
            End Set
        End Property
        
        Public Property Job() As WipStatusQueryJobJob
            Get
                Return Me.jobField
            End Get
            Set
                Me.jobField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQueryJobMaterialAllocation
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private uomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matValReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyConsumedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyBalanceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private allocCompletedField As String
        
        Public Property StockCode() As String
            Get
                Return Me.stockCodeField
            End Get
            Set
                Me.stockCodeField = value
            End Set
        End Property
        
        Public Property StockDescription() As String
            Get
                Return Me.stockDescriptionField
            End Get
            Set
                Me.stockDescriptionField = value
            End Set
        End Property
        
        Public Property Uom() As String
            Get
                Return Me.uomField
            End Get
            Set
                Me.uomField = value
            End Set
        End Property
        
        Public Property Warehouse() As String
            Get
                Return Me.warehouseField
            End Get
            Set
                Me.warehouseField = value
            End Set
        End Property
        
        Public Property QtyReqd() As String
            Get
                Return Me.qtyReqdField
            End Get
            Set
                Me.qtyReqdField = value
            End Set
        End Property
        
        Public Property QtyIssued() As String
            Get
                Return Me.qtyIssuedField
            End Get
            Set
                Me.qtyIssuedField = value
            End Set
        End Property
        
        Public Property MatValReqd() As String
            Get
                Return Me.matValReqdField
            End Get
            Set
                Me.matValReqdField = value
            End Set
        End Property
        
        Public Property ValueIssued() As String
            Get
                Return Me.valueIssuedField
            End Get
            Set
                Me.valueIssuedField = value
            End Set
        End Property
        
        Public Property QtyConsumed() As String
            Get
                Return Me.qtyConsumedField
            End Get
            Set
                Me.qtyConsumedField = value
            End Set
        End Property
        
        Public Property QtyBalance() As String
            Get
                Return Me.qtyBalanceField
            End Get
            Set
                Me.qtyBalanceField = value
            End Set
        End Property
        
        Public Property AllocCompleted() As String
            Get
                Return Me.allocCompletedField
            End Get
            Set
                Me.allocCompletedField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQueryJobOperationItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private workCentreField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private workCentreDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totRunTimeReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totSetStrTrdReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private runTimeIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totSetStrTrdIssField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labValReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operCompletedField As String
        
        Public Property Operation() As String
            Get
                Return Me.operationField
            End Get
            Set
                Me.operationField = value
            End Set
        End Property
        
        Public Property WorkCentre() As String
            Get
                Return Me.workCentreField
            End Get
            Set
                Me.workCentreField = value
            End Set
        End Property
        
        Public Property WorkCentreDesc() As String
            Get
                Return Me.workCentreDescField
            End Get
            Set
                Me.workCentreDescField = value
            End Set
        End Property
        
        Public Property TotRunTimeReqd() As String
            Get
                Return Me.totRunTimeReqdField
            End Get
            Set
                Me.totRunTimeReqdField = value
            End Set
        End Property
        
        Public Property TotSetStrTrdReqd() As String
            Get
                Return Me.totSetStrTrdReqdField
            End Get
            Set
                Me.totSetStrTrdReqdField = value
            End Set
        End Property
        
        Public Property RunTimeIssued() As String
            Get
                Return Me.runTimeIssuedField
            End Get
            Set
                Me.runTimeIssuedField = value
            End Set
        End Property
        
        Public Property TotSetStrTrdIss() As String
            Get
                Return Me.totSetStrTrdIssField
            End Get
            Set
                Me.totSetStrTrdIssField = value
            End Set
        End Property
        
        Public Property LabValReqd() As String
            Get
                Return Me.labValReqdField
            End Get
            Set
                Me.labValReqdField = value
            End Set
        End Property
        
        Public Property ValueIssued() As String
            Get
                Return Me.valueIssuedField
            End Get
            Set
                Me.valueIssuedField = value
            End Set
        End Property
        
        Public Property OperCompleted() As String
            Get
                Return Me.operCompletedField
            End Get
            Set
                Me.operCompletedField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQueryJobJob
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private subField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobNumberField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobClassificationField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private classDescriptionField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private longDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iNVWHC_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerNameField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDeliveryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobStartDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private completeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expLabourField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expMaterialField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyToMakeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOutstandingField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyManufacturedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private routeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private bOMROU_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private confirmedFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private wipCtlGlCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private materialAllocationField As List(Of WipStatusQueryJobJobMaterialAllocation)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationItemField As List(Of WipStatusQueryJobJobOperationItem)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobField As WipStatusQueryJobJobJob
        
        Public Sub New()
            MyBase.New
            If (Me.jobField Is Nothing) Then
                Me.jobField = New WipStatusQueryJobJobJob
            End If
            If (Me.operationItemField Is Nothing) Then
                Me.operationItemField = New List(Of WipStatusQueryJobJobOperationItem)
            End If
            If (Me.materialAllocationField Is Nothing) Then
                Me.materialAllocationField = New List(Of WipStatusQueryJobJobMaterialAllocation)
            End If
        End Sub
        
        Public Property [Sub]() As String
            Get
                Return Me.subField
            End Get
            Set
                Me.subField = value
            End Set
        End Property
        
        Public Property JobNumber() As String
            Get
                Return Me.jobNumberField
            End Get
            Set
                Me.jobNumberField = value
            End Set
        End Property
        
        Public Property JobDescription() As String
            Get
                Return Me.jobDescriptionField
            End Get
            Set
                Me.jobDescriptionField = value
            End Set
        End Property
        
        Public Property JobClassification() As Object
            Get
                Return Me.jobClassificationField
            End Get
            Set
                Me.jobClassificationField = value
            End Set
        End Property
        
        Public Property ClassDescription() As Object
            Get
                Return Me.classDescriptionField
            End Get
            Set
                Me.classDescriptionField = value
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
        
        Public Property StockDescription() As String
            Get
                Return Me.stockDescriptionField
            End Get
            Set
                Me.stockDescriptionField = value
            End Set
        End Property
        
        Public Property LongDesc() As String
            Get
                Return Me.longDescField
            End Get
            Set
                Me.longDescField = value
            End Set
        End Property
        
        Public Property StockUom() As String
            Get
                Return Me.stockUomField
            End Get
            Set
                Me.stockUomField = value
            End Set
        End Property
        
        Public Property Version() As Object
            Get
                Return Me.versionField
            End Get
            Set
                Me.versionField = value
            End Set
        End Property
        
        Public Property Release() As Object
            Get
                Return Me.releaseField
            End Get
            Set
                Me.releaseField = value
            End Set
        End Property
        
        Public Property Warehouse() As String
            Get
                Return Me.warehouseField
            End Get
            Set
                Me.warehouseField = value
            End Set
        End Property
        
        Public Property INVWHC_Description() As String
            Get
                Return Me.iNVWHC_DescriptionField
            End Get
            Set
                Me.iNVWHC_DescriptionField = value
            End Set
        End Property
        
        Public Property Customer() As Object
            Get
                Return Me.customerField
            End Get
            Set
                Me.customerField = value
            End Set
        End Property
        
        Public Property CustomerName() As Object
            Get
                Return Me.customerNameField
            End Get
            Set
                Me.customerNameField = value
            End Set
        End Property
        
        Public Property JobDeliveryDate() As String
            Get
                Return Me.jobDeliveryDateField
            End Get
            Set
                Me.jobDeliveryDateField = value
            End Set
        End Property
        
        Public Property JobStartDate() As String
            Get
                Return Me.jobStartDateField
            End Get
            Set
                Me.jobStartDateField = value
            End Set
        End Property
        
        Public Property Complete() As String
            Get
                Return Me.completeField
            End Get
            Set
                Me.completeField = value
            End Set
        End Property
        
        Public Property ExpLabour() As String
            Get
                Return Me.expLabourField
            End Get
            Set
                Me.expLabourField = value
            End Set
        End Property
        
        Public Property ExpMaterial() As String
            Get
                Return Me.expMaterialField
            End Get
            Set
                Me.expMaterialField = value
            End Set
        End Property
        
        Public Property QtyToMake() As String
            Get
                Return Me.qtyToMakeField
            End Get
            Set
                Me.qtyToMakeField = value
            End Set
        End Property
        
        Public Property QtyOutstanding() As String
            Get
                Return Me.qtyOutstandingField
            End Get
            Set
                Me.qtyOutstandingField = value
            End Set
        End Property
        
        Public Property QtyManufactured() As String
            Get
                Return Me.qtyManufacturedField
            End Get
            Set
                Me.qtyManufacturedField = value
            End Set
        End Property
        
        Public Property Route() As String
            Get
                Return Me.routeField
            End Get
            Set
                Me.routeField = value
            End Set
        End Property
        
        Public Property BOMROU_Description() As String
            Get
                Return Me.bOMROU_DescriptionField
            End Get
            Set
                Me.bOMROU_DescriptionField = value
            End Set
        End Property
        
        Public Property ConfirmedFlag() As String
            Get
                Return Me.confirmedFlagField
            End Get
            Set
                Me.confirmedFlagField = value
            End Set
        End Property
        
        Public Property WipCtlGlCode() As String
            Get
                Return Me.wipCtlGlCodeField
            End Get
            Set
                Me.wipCtlGlCodeField = value
            End Set
        End Property
        
        Public Property LabCostToDate() As String
            Get
                Return Me.labCostToDateField
            End Get
            Set
                Me.labCostToDateField = value
            End Set
        End Property
        
        Public Property MatCostToDate() As String
            Get
                Return Me.matCostToDateField
            End Get
            Set
                Me.matCostToDateField = value
            End Set
        End Property
        
        Public Property LabValueIssues() As String
            Get
                Return Me.labValueIssuesField
            End Get
            Set
                Me.labValueIssuesField = value
            End Set
        End Property
        
        Public Property MatValueIssues() As String
            Get
                Return Me.matValueIssuesField
            End Get
            Set
                Me.matValueIssuesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("MaterialAllocation")>  _
        Public Property MaterialAllocationCollection() As List(Of WipStatusQueryJobJobMaterialAllocation)
            Get
                Return Me.materialAllocationField
            End Get
            Set
                Me.materialAllocationField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("OperationItem")>  _
        Public Property OperationItemCollection() As List(Of WipStatusQueryJobJobOperationItem)
            Get
                Return Me.operationItemField
            End Get
            Set
                Me.operationItemField = value
            End Set
        End Property
        
        Public Property Job() As WipStatusQueryJobJobJob
            Get
                Return Me.jobField
            End Get
            Set
                Me.jobField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQueryJobJobMaterialAllocation
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private uomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matValReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private allocCompletedField As String
        
        Public Property StockCode() As String
            Get
                Return Me.stockCodeField
            End Get
            Set
                Me.stockCodeField = value
            End Set
        End Property
        
        Public Property StockDescription() As String
            Get
                Return Me.stockDescriptionField
            End Get
            Set
                Me.stockDescriptionField = value
            End Set
        End Property
        
        Public Property Uom() As String
            Get
                Return Me.uomField
            End Get
            Set
                Me.uomField = value
            End Set
        End Property
        
        Public Property Warehouse() As String
            Get
                Return Me.warehouseField
            End Get
            Set
                Me.warehouseField = value
            End Set
        End Property
        
        Public Property QtyReqd() As String
            Get
                Return Me.qtyReqdField
            End Get
            Set
                Me.qtyReqdField = value
            End Set
        End Property
        
        Public Property QtyIssued() As String
            Get
                Return Me.qtyIssuedField
            End Get
            Set
                Me.qtyIssuedField = value
            End Set
        End Property
        
        Public Property MatValReqd() As String
            Get
                Return Me.matValReqdField
            End Get
            Set
                Me.matValReqdField = value
            End Set
        End Property
        
        Public Property ValueIssued() As String
            Get
                Return Me.valueIssuedField
            End Get
            Set
                Me.valueIssuedField = value
            End Set
        End Property
        
        Public Property AllocCompleted() As String
            Get
                Return Me.allocCompletedField
            End Get
            Set
                Me.allocCompletedField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQueryJobJobOperationItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private workCentreField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private workCentreDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totRunTimeReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totSetStrTrdReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private runTimeIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totSetStrTrdIssField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labValReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operCompletedField As String
        
        Public Property Operation() As String
            Get
                Return Me.operationField
            End Get
            Set
                Me.operationField = value
            End Set
        End Property
        
        Public Property WorkCentre() As String
            Get
                Return Me.workCentreField
            End Get
            Set
                Me.workCentreField = value
            End Set
        End Property
        
        Public Property WorkCentreDesc() As String
            Get
                Return Me.workCentreDescField
            End Get
            Set
                Me.workCentreDescField = value
            End Set
        End Property
        
        Public Property TotRunTimeReqd() As String
            Get
                Return Me.totRunTimeReqdField
            End Get
            Set
                Me.totRunTimeReqdField = value
            End Set
        End Property
        
        Public Property TotSetStrTrdReqd() As String
            Get
                Return Me.totSetStrTrdReqdField
            End Get
            Set
                Me.totSetStrTrdReqdField = value
            End Set
        End Property
        
        Public Property RunTimeIssued() As String
            Get
                Return Me.runTimeIssuedField
            End Get
            Set
                Me.runTimeIssuedField = value
            End Set
        End Property
        
        Public Property TotSetStrTrdIss() As String
            Get
                Return Me.totSetStrTrdIssField
            End Get
            Set
                Me.totSetStrTrdIssField = value
            End Set
        End Property
        
        Public Property LabValReqd() As String
            Get
                Return Me.labValReqdField
            End Get
            Set
                Me.labValReqdField = value
            End Set
        End Property
        
        Public Property ValueIssued() As String
            Get
                Return Me.valueIssuedField
            End Get
            Set
                Me.valueIssuedField = value
            End Set
        End Property
        
        Public Property OperCompleted() As String
            Get
                Return Me.operCompletedField
            End Get
            Set
                Me.operCompletedField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQueryJobJobJob
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private masterSubField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobNumberField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobClassificationField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private classDescriptionField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private longDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iNVWHC_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerNameField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDeliveryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobStartDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private completeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expLabourField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expMaterialField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyToMakeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOutstandingField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyManufacturedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private routeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private bOMROU_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private confirmedFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private wipCtlGlCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private materialAllocationField As List(Of WipStatusQueryJobJobJobMaterialAllocation)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationItemField As WipStatusQueryJobJobJobOperationItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobField As List(Of WipStatusQueryJobJobJobJob)
        
        Public Sub New()
            MyBase.New
            If (Me.jobField Is Nothing) Then
                Me.jobField = New List(Of WipStatusQueryJobJobJobJob)
            End If
            If (Me.operationItemField Is Nothing) Then
                Me.operationItemField = New WipStatusQueryJobJobJobOperationItem
            End If
            If (Me.materialAllocationField Is Nothing) Then
                Me.materialAllocationField = New List(Of WipStatusQueryJobJobJobMaterialAllocation)
            End If
        End Sub
        
        Public Property MasterSub() As String
            Get
                Return Me.masterSubField
            End Get
            Set
                Me.masterSubField = value
            End Set
        End Property
        
        Public Property JobNumber() As String
            Get
                Return Me.jobNumberField
            End Get
            Set
                Me.jobNumberField = value
            End Set
        End Property
        
        Public Property JobDescription() As String
            Get
                Return Me.jobDescriptionField
            End Get
            Set
                Me.jobDescriptionField = value
            End Set
        End Property
        
        Public Property JobClassification() As Object
            Get
                Return Me.jobClassificationField
            End Get
            Set
                Me.jobClassificationField = value
            End Set
        End Property
        
        Public Property ClassDescription() As Object
            Get
                Return Me.classDescriptionField
            End Get
            Set
                Me.classDescriptionField = value
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
        
        Public Property StockDescription() As String
            Get
                Return Me.stockDescriptionField
            End Get
            Set
                Me.stockDescriptionField = value
            End Set
        End Property
        
        Public Property LongDesc() As String
            Get
                Return Me.longDescField
            End Get
            Set
                Me.longDescField = value
            End Set
        End Property
        
        Public Property StockUom() As String
            Get
                Return Me.stockUomField
            End Get
            Set
                Me.stockUomField = value
            End Set
        End Property
        
        Public Property Version() As Object
            Get
                Return Me.versionField
            End Get
            Set
                Me.versionField = value
            End Set
        End Property
        
        Public Property Release() As Object
            Get
                Return Me.releaseField
            End Get
            Set
                Me.releaseField = value
            End Set
        End Property
        
        Public Property Warehouse() As String
            Get
                Return Me.warehouseField
            End Get
            Set
                Me.warehouseField = value
            End Set
        End Property
        
        Public Property INVWHC_Description() As String
            Get
                Return Me.iNVWHC_DescriptionField
            End Get
            Set
                Me.iNVWHC_DescriptionField = value
            End Set
        End Property
        
        Public Property Customer() As Object
            Get
                Return Me.customerField
            End Get
            Set
                Me.customerField = value
            End Set
        End Property
        
        Public Property CustomerName() As Object
            Get
                Return Me.customerNameField
            End Get
            Set
                Me.customerNameField = value
            End Set
        End Property
        
        Public Property JobDeliveryDate() As String
            Get
                Return Me.jobDeliveryDateField
            End Get
            Set
                Me.jobDeliveryDateField = value
            End Set
        End Property
        
        Public Property JobStartDate() As String
            Get
                Return Me.jobStartDateField
            End Get
            Set
                Me.jobStartDateField = value
            End Set
        End Property
        
        Public Property Complete() As String
            Get
                Return Me.completeField
            End Get
            Set
                Me.completeField = value
            End Set
        End Property
        
        Public Property ExpLabour() As String
            Get
                Return Me.expLabourField
            End Get
            Set
                Me.expLabourField = value
            End Set
        End Property
        
        Public Property ExpMaterial() As String
            Get
                Return Me.expMaterialField
            End Get
            Set
                Me.expMaterialField = value
            End Set
        End Property
        
        Public Property QtyToMake() As String
            Get
                Return Me.qtyToMakeField
            End Get
            Set
                Me.qtyToMakeField = value
            End Set
        End Property
        
        Public Property QtyOutstanding() As String
            Get
                Return Me.qtyOutstandingField
            End Get
            Set
                Me.qtyOutstandingField = value
            End Set
        End Property
        
        Public Property QtyManufactured() As String
            Get
                Return Me.qtyManufacturedField
            End Get
            Set
                Me.qtyManufacturedField = value
            End Set
        End Property
        
        Public Property Route() As String
            Get
                Return Me.routeField
            End Get
            Set
                Me.routeField = value
            End Set
        End Property
        
        Public Property BOMROU_Description() As String
            Get
                Return Me.bOMROU_DescriptionField
            End Get
            Set
                Me.bOMROU_DescriptionField = value
            End Set
        End Property
        
        Public Property ConfirmedFlag() As String
            Get
                Return Me.confirmedFlagField
            End Get
            Set
                Me.confirmedFlagField = value
            End Set
        End Property
        
        Public Property WipCtlGlCode() As String
            Get
                Return Me.wipCtlGlCodeField
            End Get
            Set
                Me.wipCtlGlCodeField = value
            End Set
        End Property
        
        Public Property LabCostToDate() As String
            Get
                Return Me.labCostToDateField
            End Get
            Set
                Me.labCostToDateField = value
            End Set
        End Property
        
        Public Property MatCostToDate() As String
            Get
                Return Me.matCostToDateField
            End Get
            Set
                Me.matCostToDateField = value
            End Set
        End Property
        
        Public Property LabValueIssues() As String
            Get
                Return Me.labValueIssuesField
            End Get
            Set
                Me.labValueIssuesField = value
            End Set
        End Property
        
        Public Property MatValueIssues() As String
            Get
                Return Me.matValueIssuesField
            End Get
            Set
                Me.matValueIssuesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("MaterialAllocation")>  _
        Public Property MaterialAllocationCollection() As List(Of WipStatusQueryJobJobJobMaterialAllocation)
            Get
                Return Me.materialAllocationField
            End Get
            Set
                Me.materialAllocationField = value
            End Set
        End Property
        
        Public Property OperationItem() As WipStatusQueryJobJobJobOperationItem
            Get
                Return Me.operationItemField
            End Get
            Set
                Me.operationItemField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("Job")>  _
        Public Property JobCollection() As List(Of WipStatusQueryJobJobJobJob)
            Get
                Return Me.jobField
            End Get
            Set
                Me.jobField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQueryJobJobJobMaterialAllocation
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private uomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matValReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private allocCompletedField As String
        
        Public Property StockCode() As String
            Get
                Return Me.stockCodeField
            End Get
            Set
                Me.stockCodeField = value
            End Set
        End Property
        
        Public Property StockDescription() As String
            Get
                Return Me.stockDescriptionField
            End Get
            Set
                Me.stockDescriptionField = value
            End Set
        End Property
        
        Public Property Uom() As String
            Get
                Return Me.uomField
            End Get
            Set
                Me.uomField = value
            End Set
        End Property
        
        Public Property Warehouse() As String
            Get
                Return Me.warehouseField
            End Get
            Set
                Me.warehouseField = value
            End Set
        End Property
        
        Public Property QtyReqd() As String
            Get
                Return Me.qtyReqdField
            End Get
            Set
                Me.qtyReqdField = value
            End Set
        End Property
        
        Public Property QtyIssued() As String
            Get
                Return Me.qtyIssuedField
            End Get
            Set
                Me.qtyIssuedField = value
            End Set
        End Property
        
        Public Property MatValReqd() As String
            Get
                Return Me.matValReqdField
            End Get
            Set
                Me.matValReqdField = value
            End Set
        End Property
        
        Public Property ValueIssued() As String
            Get
                Return Me.valueIssuedField
            End Get
            Set
                Me.valueIssuedField = value
            End Set
        End Property
        
        Public Property AllocCompleted() As String
            Get
                Return Me.allocCompletedField
            End Get
            Set
                Me.allocCompletedField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQueryJobJobJobOperationItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private workCentreField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private workCentreDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totRunTimeReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totSetStrTrdReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private runTimeIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totSetStrTrdIssField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labValReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operCompletedField As String
        
        Public Property Operation() As String
            Get
                Return Me.operationField
            End Get
            Set
                Me.operationField = value
            End Set
        End Property
        
        Public Property WorkCentre() As String
            Get
                Return Me.workCentreField
            End Get
            Set
                Me.workCentreField = value
            End Set
        End Property
        
        Public Property WorkCentreDesc() As String
            Get
                Return Me.workCentreDescField
            End Get
            Set
                Me.workCentreDescField = value
            End Set
        End Property
        
        Public Property TotRunTimeReqd() As String
            Get
                Return Me.totRunTimeReqdField
            End Get
            Set
                Me.totRunTimeReqdField = value
            End Set
        End Property
        
        Public Property TotSetStrTrdReqd() As String
            Get
                Return Me.totSetStrTrdReqdField
            End Get
            Set
                Me.totSetStrTrdReqdField = value
            End Set
        End Property
        
        Public Property RunTimeIssued() As String
            Get
                Return Me.runTimeIssuedField
            End Get
            Set
                Me.runTimeIssuedField = value
            End Set
        End Property
        
        Public Property TotSetStrTrdIss() As String
            Get
                Return Me.totSetStrTrdIssField
            End Get
            Set
                Me.totSetStrTrdIssField = value
            End Set
        End Property
        
        Public Property LabValReqd() As String
            Get
                Return Me.labValReqdField
            End Get
            Set
                Me.labValReqdField = value
            End Set
        End Property
        
        Public Property ValueIssued() As String
            Get
                Return Me.valueIssuedField
            End Get
            Set
                Me.valueIssuedField = value
            End Set
        End Property
        
        Public Property OperCompleted() As String
            Get
                Return Me.operCompletedField
            End Get
            Set
                Me.operCompletedField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQueryJobJobJobJob
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private subField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobNumberField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobClassificationField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private classDescriptionField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private longDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iNVWHC_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerNameField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDeliveryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobStartDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private completeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expLabourField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expMaterialField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyToMakeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOutstandingField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyManufacturedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private routeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private bOMROU_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private confirmedFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private wipCtlGlCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private materialAllocationField As List(Of WipStatusQueryJobJobJobJobMaterialAllocation)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationItemField As List(Of WipStatusQueryJobJobJobJobOperationItem)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobField As WipStatusQueryJobJobJobJobJob
        
        Public Sub New()
            MyBase.New
            If (Me.jobField Is Nothing) Then
                Me.jobField = New WipStatusQueryJobJobJobJobJob
            End If
            If (Me.operationItemField Is Nothing) Then
                Me.operationItemField = New List(Of WipStatusQueryJobJobJobJobOperationItem)
            End If
            If (Me.materialAllocationField Is Nothing) Then
                Me.materialAllocationField = New List(Of WipStatusQueryJobJobJobJobMaterialAllocation)
            End If
        End Sub
        
        Public Property [Sub]() As String
            Get
                Return Me.subField
            End Get
            Set
                Me.subField = value
            End Set
        End Property
        
        Public Property JobNumber() As String
            Get
                Return Me.jobNumberField
            End Get
            Set
                Me.jobNumberField = value
            End Set
        End Property
        
        Public Property JobDescription() As String
            Get
                Return Me.jobDescriptionField
            End Get
            Set
                Me.jobDescriptionField = value
            End Set
        End Property
        
        Public Property JobClassification() As Object
            Get
                Return Me.jobClassificationField
            End Get
            Set
                Me.jobClassificationField = value
            End Set
        End Property
        
        Public Property ClassDescription() As Object
            Get
                Return Me.classDescriptionField
            End Get
            Set
                Me.classDescriptionField = value
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
        
        Public Property StockDescription() As String
            Get
                Return Me.stockDescriptionField
            End Get
            Set
                Me.stockDescriptionField = value
            End Set
        End Property
        
        Public Property LongDesc() As String
            Get
                Return Me.longDescField
            End Get
            Set
                Me.longDescField = value
            End Set
        End Property
        
        Public Property StockUom() As String
            Get
                Return Me.stockUomField
            End Get
            Set
                Me.stockUomField = value
            End Set
        End Property
        
        Public Property Version() As Object
            Get
                Return Me.versionField
            End Get
            Set
                Me.versionField = value
            End Set
        End Property
        
        Public Property Release() As Object
            Get
                Return Me.releaseField
            End Get
            Set
                Me.releaseField = value
            End Set
        End Property
        
        Public Property Warehouse() As String
            Get
                Return Me.warehouseField
            End Get
            Set
                Me.warehouseField = value
            End Set
        End Property
        
        Public Property INVWHC_Description() As String
            Get
                Return Me.iNVWHC_DescriptionField
            End Get
            Set
                Me.iNVWHC_DescriptionField = value
            End Set
        End Property
        
        Public Property Customer() As Object
            Get
                Return Me.customerField
            End Get
            Set
                Me.customerField = value
            End Set
        End Property
        
        Public Property CustomerName() As Object
            Get
                Return Me.customerNameField
            End Get
            Set
                Me.customerNameField = value
            End Set
        End Property
        
        Public Property JobDeliveryDate() As String
            Get
                Return Me.jobDeliveryDateField
            End Get
            Set
                Me.jobDeliveryDateField = value
            End Set
        End Property
        
        Public Property JobStartDate() As String
            Get
                Return Me.jobStartDateField
            End Get
            Set
                Me.jobStartDateField = value
            End Set
        End Property
        
        Public Property Complete() As String
            Get
                Return Me.completeField
            End Get
            Set
                Me.completeField = value
            End Set
        End Property
        
        Public Property ExpLabour() As String
            Get
                Return Me.expLabourField
            End Get
            Set
                Me.expLabourField = value
            End Set
        End Property
        
        Public Property ExpMaterial() As String
            Get
                Return Me.expMaterialField
            End Get
            Set
                Me.expMaterialField = value
            End Set
        End Property
        
        Public Property QtyToMake() As String
            Get
                Return Me.qtyToMakeField
            End Get
            Set
                Me.qtyToMakeField = value
            End Set
        End Property
        
        Public Property QtyOutstanding() As String
            Get
                Return Me.qtyOutstandingField
            End Get
            Set
                Me.qtyOutstandingField = value
            End Set
        End Property
        
        Public Property QtyManufactured() As String
            Get
                Return Me.qtyManufacturedField
            End Get
            Set
                Me.qtyManufacturedField = value
            End Set
        End Property
        
        Public Property Route() As String
            Get
                Return Me.routeField
            End Get
            Set
                Me.routeField = value
            End Set
        End Property
        
        Public Property BOMROU_Description() As String
            Get
                Return Me.bOMROU_DescriptionField
            End Get
            Set
                Me.bOMROU_DescriptionField = value
            End Set
        End Property
        
        Public Property ConfirmedFlag() As String
            Get
                Return Me.confirmedFlagField
            End Get
            Set
                Me.confirmedFlagField = value
            End Set
        End Property
        
        Public Property WipCtlGlCode() As String
            Get
                Return Me.wipCtlGlCodeField
            End Get
            Set
                Me.wipCtlGlCodeField = value
            End Set
        End Property
        
        Public Property LabCostToDate() As String
            Get
                Return Me.labCostToDateField
            End Get
            Set
                Me.labCostToDateField = value
            End Set
        End Property
        
        Public Property MatCostToDate() As String
            Get
                Return Me.matCostToDateField
            End Get
            Set
                Me.matCostToDateField = value
            End Set
        End Property
        
        Public Property LabValueIssues() As String
            Get
                Return Me.labValueIssuesField
            End Get
            Set
                Me.labValueIssuesField = value
            End Set
        End Property
        
        Public Property MatValueIssues() As String
            Get
                Return Me.matValueIssuesField
            End Get
            Set
                Me.matValueIssuesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("MaterialAllocation")>  _
        Public Property MaterialAllocationCollection() As List(Of WipStatusQueryJobJobJobJobMaterialAllocation)
            Get
                Return Me.materialAllocationField
            End Get
            Set
                Me.materialAllocationField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("OperationItem")>  _
        Public Property OperationItemCollection() As List(Of WipStatusQueryJobJobJobJobOperationItem)
            Get
                Return Me.operationItemField
            End Get
            Set
                Me.operationItemField = value
            End Set
        End Property
        
        Public Property Job() As WipStatusQueryJobJobJobJobJob
            Get
                Return Me.jobField
            End Get
            Set
                Me.jobField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQueryJobJobJobJobMaterialAllocation
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private uomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matValReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private allocCompletedField As String
        
        Public Property StockCode() As String
            Get
                Return Me.stockCodeField
            End Get
            Set
                Me.stockCodeField = value
            End Set
        End Property
        
        Public Property StockDescription() As String
            Get
                Return Me.stockDescriptionField
            End Get
            Set
                Me.stockDescriptionField = value
            End Set
        End Property
        
        Public Property Uom() As String
            Get
                Return Me.uomField
            End Get
            Set
                Me.uomField = value
            End Set
        End Property
        
        Public Property Warehouse() As String
            Get
                Return Me.warehouseField
            End Get
            Set
                Me.warehouseField = value
            End Set
        End Property
        
        Public Property QtyReqd() As String
            Get
                Return Me.qtyReqdField
            End Get
            Set
                Me.qtyReqdField = value
            End Set
        End Property
        
        Public Property QtyIssued() As String
            Get
                Return Me.qtyIssuedField
            End Get
            Set
                Me.qtyIssuedField = value
            End Set
        End Property
        
        Public Property MatValReqd() As String
            Get
                Return Me.matValReqdField
            End Get
            Set
                Me.matValReqdField = value
            End Set
        End Property
        
        Public Property ValueIssued() As String
            Get
                Return Me.valueIssuedField
            End Get
            Set
                Me.valueIssuedField = value
            End Set
        End Property
        
        Public Property AllocCompleted() As String
            Get
                Return Me.allocCompletedField
            End Get
            Set
                Me.allocCompletedField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQueryJobJobJobJobOperationItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private workCentreField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private workCentreDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totRunTimeReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totSetStrTrdReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private runTimeIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totSetStrTrdIssField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labValReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operCompletedField As String
        
        Public Property Operation() As String
            Get
                Return Me.operationField
            End Get
            Set
                Me.operationField = value
            End Set
        End Property
        
        Public Property WorkCentre() As String
            Get
                Return Me.workCentreField
            End Get
            Set
                Me.workCentreField = value
            End Set
        End Property
        
        Public Property WorkCentreDesc() As String
            Get
                Return Me.workCentreDescField
            End Get
            Set
                Me.workCentreDescField = value
            End Set
        End Property
        
        Public Property TotRunTimeReqd() As String
            Get
                Return Me.totRunTimeReqdField
            End Get
            Set
                Me.totRunTimeReqdField = value
            End Set
        End Property
        
        Public Property TotSetStrTrdReqd() As String
            Get
                Return Me.totSetStrTrdReqdField
            End Get
            Set
                Me.totSetStrTrdReqdField = value
            End Set
        End Property
        
        Public Property RunTimeIssued() As String
            Get
                Return Me.runTimeIssuedField
            End Get
            Set
                Me.runTimeIssuedField = value
            End Set
        End Property
        
        Public Property TotSetStrTrdIss() As String
            Get
                Return Me.totSetStrTrdIssField
            End Get
            Set
                Me.totSetStrTrdIssField = value
            End Set
        End Property
        
        Public Property LabValReqd() As String
            Get
                Return Me.labValReqdField
            End Get
            Set
                Me.labValReqdField = value
            End Set
        End Property
        
        Public Property ValueIssued() As String
            Get
                Return Me.valueIssuedField
            End Get
            Set
                Me.valueIssuedField = value
            End Set
        End Property
        
        Public Property OperCompleted() As String
            Get
                Return Me.operCompletedField
            End Get
            Set
                Me.operCompletedField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQueryJobJobJobJobJob
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private subField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobNumberField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobClassificationField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private classDescriptionField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private longDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iNVWHC_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerNameField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDeliveryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobStartDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private completeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expLabourField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expMaterialField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyToMakeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOutstandingField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyManufacturedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private routeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private bOMROU_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private confirmedFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private wipCtlGlCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private materialAllocationField As List(Of WipStatusQueryJobJobJobJobJobMaterialAllocation)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationItemField As List(Of WipStatusQueryJobJobJobJobJobOperationItem)
        
        Public Sub New()
            MyBase.New
            If (Me.operationItemField Is Nothing) Then
                Me.operationItemField = New List(Of WipStatusQueryJobJobJobJobJobOperationItem)
            End If
            If (Me.materialAllocationField Is Nothing) Then
                Me.materialAllocationField = New List(Of WipStatusQueryJobJobJobJobJobMaterialAllocation)
            End If
        End Sub
        
        Public Property [Sub]() As String
            Get
                Return Me.subField
            End Get
            Set
                Me.subField = value
            End Set
        End Property
        
        Public Property JobNumber() As String
            Get
                Return Me.jobNumberField
            End Get
            Set
                Me.jobNumberField = value
            End Set
        End Property
        
        Public Property JobDescription() As String
            Get
                Return Me.jobDescriptionField
            End Get
            Set
                Me.jobDescriptionField = value
            End Set
        End Property
        
        Public Property JobClassification() As Object
            Get
                Return Me.jobClassificationField
            End Get
            Set
                Me.jobClassificationField = value
            End Set
        End Property
        
        Public Property ClassDescription() As Object
            Get
                Return Me.classDescriptionField
            End Get
            Set
                Me.classDescriptionField = value
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
        
        Public Property StockDescription() As String
            Get
                Return Me.stockDescriptionField
            End Get
            Set
                Me.stockDescriptionField = value
            End Set
        End Property
        
        Public Property LongDesc() As String
            Get
                Return Me.longDescField
            End Get
            Set
                Me.longDescField = value
            End Set
        End Property
        
        Public Property StockUom() As String
            Get
                Return Me.stockUomField
            End Get
            Set
                Me.stockUomField = value
            End Set
        End Property
        
        Public Property Version() As Object
            Get
                Return Me.versionField
            End Get
            Set
                Me.versionField = value
            End Set
        End Property
        
        Public Property Release() As Object
            Get
                Return Me.releaseField
            End Get
            Set
                Me.releaseField = value
            End Set
        End Property
        
        Public Property Warehouse() As String
            Get
                Return Me.warehouseField
            End Get
            Set
                Me.warehouseField = value
            End Set
        End Property
        
        Public Property INVWHC_Description() As String
            Get
                Return Me.iNVWHC_DescriptionField
            End Get
            Set
                Me.iNVWHC_DescriptionField = value
            End Set
        End Property
        
        Public Property Customer() As Object
            Get
                Return Me.customerField
            End Get
            Set
                Me.customerField = value
            End Set
        End Property
        
        Public Property CustomerName() As Object
            Get
                Return Me.customerNameField
            End Get
            Set
                Me.customerNameField = value
            End Set
        End Property
        
        Public Property JobDeliveryDate() As String
            Get
                Return Me.jobDeliveryDateField
            End Get
            Set
                Me.jobDeliveryDateField = value
            End Set
        End Property
        
        Public Property JobStartDate() As String
            Get
                Return Me.jobStartDateField
            End Get
            Set
                Me.jobStartDateField = value
            End Set
        End Property
        
        Public Property Complete() As String
            Get
                Return Me.completeField
            End Get
            Set
                Me.completeField = value
            End Set
        End Property
        
        Public Property ExpLabour() As String
            Get
                Return Me.expLabourField
            End Get
            Set
                Me.expLabourField = value
            End Set
        End Property
        
        Public Property ExpMaterial() As String
            Get
                Return Me.expMaterialField
            End Get
            Set
                Me.expMaterialField = value
            End Set
        End Property
        
        Public Property QtyToMake() As String
            Get
                Return Me.qtyToMakeField
            End Get
            Set
                Me.qtyToMakeField = value
            End Set
        End Property
        
        Public Property QtyOutstanding() As String
            Get
                Return Me.qtyOutstandingField
            End Get
            Set
                Me.qtyOutstandingField = value
            End Set
        End Property
        
        Public Property QtyManufactured() As String
            Get
                Return Me.qtyManufacturedField
            End Get
            Set
                Me.qtyManufacturedField = value
            End Set
        End Property
        
        Public Property Route() As String
            Get
                Return Me.routeField
            End Get
            Set
                Me.routeField = value
            End Set
        End Property
        
        Public Property BOMROU_Description() As String
            Get
                Return Me.bOMROU_DescriptionField
            End Get
            Set
                Me.bOMROU_DescriptionField = value
            End Set
        End Property
        
        Public Property ConfirmedFlag() As String
            Get
                Return Me.confirmedFlagField
            End Get
            Set
                Me.confirmedFlagField = value
            End Set
        End Property
        
        Public Property WipCtlGlCode() As String
            Get
                Return Me.wipCtlGlCodeField
            End Get
            Set
                Me.wipCtlGlCodeField = value
            End Set
        End Property
        
        Public Property LabCostToDate() As String
            Get
                Return Me.labCostToDateField
            End Get
            Set
                Me.labCostToDateField = value
            End Set
        End Property
        
        Public Property MatCostToDate() As String
            Get
                Return Me.matCostToDateField
            End Get
            Set
                Me.matCostToDateField = value
            End Set
        End Property
        
        Public Property LabValueIssues() As String
            Get
                Return Me.labValueIssuesField
            End Get
            Set
                Me.labValueIssuesField = value
            End Set
        End Property
        
        Public Property MatValueIssues() As String
            Get
                Return Me.matValueIssuesField
            End Get
            Set
                Me.matValueIssuesField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("MaterialAllocation")>  _
        Public Property MaterialAllocationCollection() As List(Of WipStatusQueryJobJobJobJobJobMaterialAllocation)
            Get
                Return Me.materialAllocationField
            End Get
            Set
                Me.materialAllocationField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("OperationItem")>  _
        Public Property OperationItemCollection() As List(Of WipStatusQueryJobJobJobJobJobOperationItem)
            Get
                Return Me.operationItemField
            End Get
            Set
                Me.operationItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQueryJobJobJobJobJobMaterialAllocation
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private uomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matValReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private allocCompletedField As String
        
        Public Property StockCode() As String
            Get
                Return Me.stockCodeField
            End Get
            Set
                Me.stockCodeField = value
            End Set
        End Property
        
        Public Property StockDescription() As String
            Get
                Return Me.stockDescriptionField
            End Get
            Set
                Me.stockDescriptionField = value
            End Set
        End Property
        
        Public Property Uom() As String
            Get
                Return Me.uomField
            End Get
            Set
                Me.uomField = value
            End Set
        End Property
        
        Public Property Warehouse() As String
            Get
                Return Me.warehouseField
            End Get
            Set
                Me.warehouseField = value
            End Set
        End Property
        
        Public Property QtyReqd() As String
            Get
                Return Me.qtyReqdField
            End Get
            Set
                Me.qtyReqdField = value
            End Set
        End Property
        
        Public Property QtyIssued() As String
            Get
                Return Me.qtyIssuedField
            End Get
            Set
                Me.qtyIssuedField = value
            End Set
        End Property
        
        Public Property MatValReqd() As String
            Get
                Return Me.matValReqdField
            End Get
            Set
                Me.matValReqdField = value
            End Set
        End Property
        
        Public Property ValueIssued() As String
            Get
                Return Me.valueIssuedField
            End Get
            Set
                Me.valueIssuedField = value
            End Set
        End Property
        
        Public Property AllocCompleted() As String
            Get
                Return Me.allocCompletedField
            End Get
            Set
                Me.allocCompletedField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class WipStatusQueryJobJobJobJobJobOperationItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private workCentreField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private workCentreDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totRunTimeReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totSetStrTrdReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private runTimeIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totSetStrTrdIssField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labValReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operCompletedField As String
        
        Public Property Operation() As String
            Get
                Return Me.operationField
            End Get
            Set
                Me.operationField = value
            End Set
        End Property
        
        Public Property WorkCentre() As String
            Get
                Return Me.workCentreField
            End Get
            Set
                Me.workCentreField = value
            End Set
        End Property
        
        Public Property WorkCentreDesc() As String
            Get
                Return Me.workCentreDescField
            End Get
            Set
                Me.workCentreDescField = value
            End Set
        End Property
        
        Public Property TotRunTimeReqd() As String
            Get
                Return Me.totRunTimeReqdField
            End Get
            Set
                Me.totRunTimeReqdField = value
            End Set
        End Property
        
        Public Property TotSetStrTrdReqd() As String
            Get
                Return Me.totSetStrTrdReqdField
            End Get
            Set
                Me.totSetStrTrdReqdField = value
            End Set
        End Property
        
        Public Property RunTimeIssued() As String
            Get
                Return Me.runTimeIssuedField
            End Get
            Set
                Me.runTimeIssuedField = value
            End Set
        End Property
        
        Public Property TotSetStrTrdIss() As String
            Get
                Return Me.totSetStrTrdIssField
            End Get
            Set
                Me.totSetStrTrdIssField = value
            End Set
        End Property
        
        Public Property LabValReqd() As String
            Get
                Return Me.labValReqdField
            End Get
            Set
                Me.labValReqdField = value
            End Set
        End Property
        
        Public Property ValueIssued() As String
            Get
                Return Me.valueIssuedField
            End Get
            Set
                Me.valueIssuedField = value
            End Set
        End Property
        
        Public Property OperCompleted() As String
            Get
                Return Me.operCompletedField
            End Get
            Set
                Me.operCompletedField = value
            End Set
        End Property
    End Class
End Namespace
