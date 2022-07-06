Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace InventorySerialTracking.SerialTrackingQuery.Output
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class InvSerialNumberQuery
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private systemInformationField As InvSerialNumberQuerySystemInformation
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serialDetailField As InvSerialNumberQuerySerialDetail
        
        Public Sub New()
            MyBase.New
            If (Me.serialDetailField Is Nothing) Then
                Me.serialDetailField = New InvSerialNumberQuerySerialDetail
            End If
            If (Me.systemInformationField Is Nothing) Then
                Me.systemInformationField = New InvSerialNumberQuerySystemInformation
            End If
        End Sub
        
        Public Property SystemInformation() As InvSerialNumberQuerySystemInformation
            Get
                Return Me.systemInformationField
            End Get
            Set
                Me.systemInformationField = value
            End Set
        End Property
        
        Public Property SerialDetail() As InvSerialNumberQuerySerialDetail
            Get
                Return Me.serialDetailField
            End Get
            Set
                Me.serialDetailField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvSerialNumberQuerySystemInformation
        
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
    Partial Public Class InvSerialNumberQuerySerialDetail
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serialField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iNVWHC_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serialMethodField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iNVMST_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serialDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expiryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private scrapDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyReceivedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyAvailableField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOnHandField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private locationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serviceFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private loadFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serviceDepotFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dueDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private nameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lotField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private archiveFilenameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateCreatedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private prvWarehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private transactionsField As List(Of InvSerialNumberQuerySerialDetailTransactionDetail)
        
        Public Sub New()
            MyBase.New
            If (Me.transactionsField Is Nothing) Then
                Me.transactionsField = New List(Of InvSerialNumberQuerySerialDetailTransactionDetail)
            End If
        End Sub
        
        Public Property Serial() As String
            Get
                Return Me.serialField
            End Get
            Set
                Me.serialField = value
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
        
        Public Property SerialMethod() As String
            Get
                Return Me.serialMethodField
            End Get
            Set
                Me.serialMethodField = value
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
        
        Public Property INVMST_Description() As String
            Get
                Return Me.iNVMST_DescriptionField
            End Get
            Set
                Me.iNVMST_DescriptionField = value
            End Set
        End Property
        
        Public Property SerialDescription() As String
            Get
                Return Me.serialDescriptionField
            End Get
            Set
                Me.serialDescriptionField = value
            End Set
        End Property
        
        Public Property ExpiryDate() As String
            Get
                Return Me.expiryDateField
            End Get
            Set
                Me.expiryDateField = value
            End Set
        End Property
        
        Public Property ScrapDate() As String
            Get
                Return Me.scrapDateField
            End Get
            Set
                Me.scrapDateField = value
            End Set
        End Property
        
        Public Property QtyReceived() As String
            Get
                Return Me.qtyReceivedField
            End Get
            Set
                Me.qtyReceivedField = value
            End Set
        End Property
        
        Public Property QtyAvailable() As String
            Get
                Return Me.qtyAvailableField
            End Get
            Set
                Me.qtyAvailableField = value
            End Set
        End Property
        
        Public Property QtyOnHand() As String
            Get
                Return Me.qtyOnHandField
            End Get
            Set
                Me.qtyOnHandField = value
            End Set
        End Property
        
        Public Property Location() As String
            Get
                Return Me.locationField
            End Get
            Set
                Me.locationField = value
            End Set
        End Property
        
        Public Property ServiceFlag() As String
            Get
                Return Me.serviceFlagField
            End Get
            Set
                Me.serviceFlagField = value
            End Set
        End Property
        
        Public Property LoadFlag() As String
            Get
                Return Me.loadFlagField
            End Get
            Set
                Me.loadFlagField = value
            End Set
        End Property
        
        Public Property ServiceDepotFlag() As String
            Get
                Return Me.serviceDepotFlagField
            End Get
            Set
                Me.serviceDepotFlagField = value
            End Set
        End Property
        
        Public Property DueDate() As String
            Get
                Return Me.dueDateField
            End Get
            Set
                Me.dueDateField = value
            End Set
        End Property
        
        Public Property Customer() As String
            Get
                Return Me.customerField
            End Get
            Set
                Me.customerField = value
            End Set
        End Property
        
        Public Property Name() As String
            Get
                Return Me.nameField
            End Get
            Set
                Me.nameField = value
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
        
        Public Property ArchiveFilename() As String
            Get
                Return Me.archiveFilenameField
            End Get
            Set
                Me.archiveFilenameField = value
            End Set
        End Property
        
        Public Property DateCreated() As String
            Get
                Return Me.dateCreatedField
            End Get
            Set
                Me.dateCreatedField = value
            End Set
        End Property
        
        Public Property PrvWarehouse() As String
            Get
                Return Me.prvWarehouseField
            End Get
            Set
                Me.prvWarehouseField = value
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
        
        Public Property Release() As String
            Get
                Return Me.releaseField
            End Get
            Set
                Me.releaseField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlArrayItemAttribute("TransactionDetail", IsNullable:=false)>  _
        Public Property TransactionsCollection() As List(Of InvSerialNumberQuerySerialDetailTransactionDetail)
            Get
                Return Me.transactionsField
            End Get
            Set
                Me.transactionsField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvSerialNumberQuerySerialDetailTransactionDetail
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private entryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private detailTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private revisionLevelField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private referenceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private notationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private custSupplierField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private custSupNameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private trnQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private trnValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private trnWarehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private newLocationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private newExpiryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private purchaseOrderField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lotField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private trfFromWhField As String
        
        Public Property EntryDate() As String
            Get
                Return Me.entryDateField
            End Get
            Set
                Me.entryDateField = value
            End Set
        End Property
        
        Public Property DetailType() As String
            Get
                Return Me.detailTypeField
            End Get
            Set
                Me.detailTypeField = value
            End Set
        End Property
        
        Public Property RevisionLevel() As String
            Get
                Return Me.revisionLevelField
            End Get
            Set
                Me.revisionLevelField = value
            End Set
        End Property
        
        Public Property Reference() As String
            Get
                Return Me.referenceField
            End Get
            Set
                Me.referenceField = value
            End Set
        End Property
        
        Public Property Job() As String
            Get
                Return Me.jobField
            End Get
            Set
                Me.jobField = value
            End Set
        End Property
        
        Public Property Notation() As String
            Get
                Return Me.notationField
            End Get
            Set
                Me.notationField = value
            End Set
        End Property
        
        Public Property CustSupplier() As String
            Get
                Return Me.custSupplierField
            End Get
            Set
                Me.custSupplierField = value
            End Set
        End Property
        
        Public Property CustSupName() As String
            Get
                Return Me.custSupNameField
            End Get
            Set
                Me.custSupNameField = value
            End Set
        End Property
        
        Public Property TrnQty() As String
            Get
                Return Me.trnQtyField
            End Get
            Set
                Me.trnQtyField = value
            End Set
        End Property
        
        Public Property TrnValue() As String
            Get
                Return Me.trnValueField
            End Get
            Set
                Me.trnValueField = value
            End Set
        End Property
        
        Public Property TrnWarehouse() As String
            Get
                Return Me.trnWarehouseField
            End Get
            Set
                Me.trnWarehouseField = value
            End Set
        End Property
        
        Public Property NewLocation() As String
            Get
                Return Me.newLocationField
            End Get
            Set
                Me.newLocationField = value
            End Set
        End Property
        
        Public Property NewExpiryDate() As String
            Get
                Return Me.newExpiryDateField
            End Get
            Set
                Me.newExpiryDateField = value
            End Set
        End Property
        
        Public Property PurchaseOrder() As String
            Get
                Return Me.purchaseOrderField
            End Get
            Set
                Me.purchaseOrderField = value
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
        
        Public Property Version() As String
            Get
                Return Me.versionField
            End Get
            Set
                Me.versionField = value
            End Set
        End Property
        
        Public Property Release() As String
            Get
                Return Me.releaseField
            End Get
            Set
                Me.releaseField = value
            End Set
        End Property
        
        Public Property TrfFromWh() As String
            Get
                Return Me.trfFromWhField
            End Get
            Set
                Me.trfFromWhField = value
            End Set
        End Property
    End Class
End Namespace
