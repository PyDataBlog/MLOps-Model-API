Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace InventoryPrimaryQuery.InventoryWhereReferencedQuery.Output
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class InvWhereReferencedQuery
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private systemInformationField As InvWhereReferencedQuerySystemInformation
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockItemField As InvWhereReferencedQueryStockItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private purchaseOrderDetailsField As InvWhereReferencedQueryPurchaseOrderDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesOrderDetailsField As InvWhereReferencedQuerySalesOrderDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private sctDetailsField As InvWhereReferencedQuerySctDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private eccDetailsField As InvWhereReferencedQueryEccDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private alternateStockCodeDetailsField As InvWhereReferencedQueryAlternateStockCodeDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private alternateSupplierDetailsField As InvWhereReferencedQueryAlternateSupplierDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private workInProgressDetailsField As InvWhereReferencedQueryWorkInProgressDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private whereUsedDetailsField As InvWhereReferencedQueryWhereUsedDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private bomStructureDetailsField As InvWhereReferencedQueryBomStructureDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supplierStockCodeDetailsField As InvWhereReferencedQuerySupplierStockCodeDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lctRoutesDetailsField As InvWhereReferencedQueryLctRoutesDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private blanketPOrderDetailsField As InvWhereReferencedQueryBlanketPOrderDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private foreignPriceDetailsField As List(Of InvWhereReferencedQueryForeignPriceItem)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private pricingDetailsField As InvWhereReferencedQueryPricingDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dispatchNotesDetailsField As InvWhereReferencedQueryDispatchNotesDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerStockCodeDetailsField As InvWhereReferencedQueryCustomerStockCodeDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private goodsInTransitDetailsField As List(Of InvWhereReferencedQueryReferenceItem)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseItemField As InvWhereReferencedQueryWarehouseItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private requisitionDetailsField As InvWhereReferencedQueryRequisitionDetails
        
        Public Sub New()
            MyBase.New
            If (Me.requisitionDetailsField Is Nothing) Then
                Me.requisitionDetailsField = New InvWhereReferencedQueryRequisitionDetails
            End If
            If (Me.warehouseItemField Is Nothing) Then
                Me.warehouseItemField = New InvWhereReferencedQueryWarehouseItem
            End If
            If (Me.goodsInTransitDetailsField Is Nothing) Then
                Me.goodsInTransitDetailsField = New List(Of InvWhereReferencedQueryReferenceItem)
            End If
            If (Me.customerStockCodeDetailsField Is Nothing) Then
                Me.customerStockCodeDetailsField = New InvWhereReferencedQueryCustomerStockCodeDetails
            End If
            If (Me.dispatchNotesDetailsField Is Nothing) Then
                Me.dispatchNotesDetailsField = New InvWhereReferencedQueryDispatchNotesDetails
            End If
            If (Me.pricingDetailsField Is Nothing) Then
                Me.pricingDetailsField = New InvWhereReferencedQueryPricingDetails
            End If
            If (Me.foreignPriceDetailsField Is Nothing) Then
                Me.foreignPriceDetailsField = New List(Of InvWhereReferencedQueryForeignPriceItem)
            End If
            If (Me.blanketPOrderDetailsField Is Nothing) Then
                Me.blanketPOrderDetailsField = New InvWhereReferencedQueryBlanketPOrderDetails
            End If
            If (Me.lctRoutesDetailsField Is Nothing) Then
                Me.lctRoutesDetailsField = New InvWhereReferencedQueryLctRoutesDetails
            End If
            If (Me.supplierStockCodeDetailsField Is Nothing) Then
                Me.supplierStockCodeDetailsField = New InvWhereReferencedQuerySupplierStockCodeDetails
            End If
            If (Me.bomStructureDetailsField Is Nothing) Then
                Me.bomStructureDetailsField = New InvWhereReferencedQueryBomStructureDetails
            End If
            If (Me.whereUsedDetailsField Is Nothing) Then
                Me.whereUsedDetailsField = New InvWhereReferencedQueryWhereUsedDetails
            End If
            If (Me.workInProgressDetailsField Is Nothing) Then
                Me.workInProgressDetailsField = New InvWhereReferencedQueryWorkInProgressDetails
            End If
            If (Me.alternateSupplierDetailsField Is Nothing) Then
                Me.alternateSupplierDetailsField = New InvWhereReferencedQueryAlternateSupplierDetails
            End If
            If (Me.alternateStockCodeDetailsField Is Nothing) Then
                Me.alternateStockCodeDetailsField = New InvWhereReferencedQueryAlternateStockCodeDetails
            End If
            If (Me.eccDetailsField Is Nothing) Then
                Me.eccDetailsField = New InvWhereReferencedQueryEccDetails
            End If
            If (Me.sctDetailsField Is Nothing) Then
                Me.sctDetailsField = New InvWhereReferencedQuerySctDetails
            End If
            If (Me.salesOrderDetailsField Is Nothing) Then
                Me.salesOrderDetailsField = New InvWhereReferencedQuerySalesOrderDetails
            End If
            If (Me.purchaseOrderDetailsField Is Nothing) Then
                Me.purchaseOrderDetailsField = New InvWhereReferencedQueryPurchaseOrderDetails
            End If
            If (Me.stockItemField Is Nothing) Then
                Me.stockItemField = New InvWhereReferencedQueryStockItem
            End If
            If (Me.systemInformationField Is Nothing) Then
                Me.systemInformationField = New InvWhereReferencedQuerySystemInformation
            End If
        End Sub
        
        Public Property SystemInformation() As InvWhereReferencedQuerySystemInformation
            Get
                Return Me.systemInformationField
            End Get
            Set
                Me.systemInformationField = value
            End Set
        End Property
        
        Public Property StockItem() As InvWhereReferencedQueryStockItem
            Get
                Return Me.stockItemField
            End Get
            Set
                Me.stockItemField = value
            End Set
        End Property
        
        Public Property PurchaseOrderDetails() As InvWhereReferencedQueryPurchaseOrderDetails
            Get
                Return Me.purchaseOrderDetailsField
            End Get
            Set
                Me.purchaseOrderDetailsField = value
            End Set
        End Property
        
        Public Property SalesOrderDetails() As InvWhereReferencedQuerySalesOrderDetails
            Get
                Return Me.salesOrderDetailsField
            End Get
            Set
                Me.salesOrderDetailsField = value
            End Set
        End Property
        
        Public Property SctDetails() As InvWhereReferencedQuerySctDetails
            Get
                Return Me.sctDetailsField
            End Get
            Set
                Me.sctDetailsField = value
            End Set
        End Property
        
        Public Property EccDetails() As InvWhereReferencedQueryEccDetails
            Get
                Return Me.eccDetailsField
            End Get
            Set
                Me.eccDetailsField = value
            End Set
        End Property
        
        Public Property AlternateStockCodeDetails() As InvWhereReferencedQueryAlternateStockCodeDetails
            Get
                Return Me.alternateStockCodeDetailsField
            End Get
            Set
                Me.alternateStockCodeDetailsField = value
            End Set
        End Property
        
        Public Property AlternateSupplierDetails() As InvWhereReferencedQueryAlternateSupplierDetails
            Get
                Return Me.alternateSupplierDetailsField
            End Get
            Set
                Me.alternateSupplierDetailsField = value
            End Set
        End Property
        
        Public Property WorkInProgressDetails() As InvWhereReferencedQueryWorkInProgressDetails
            Get
                Return Me.workInProgressDetailsField
            End Get
            Set
                Me.workInProgressDetailsField = value
            End Set
        End Property
        
        Public Property WhereUsedDetails() As InvWhereReferencedQueryWhereUsedDetails
            Get
                Return Me.whereUsedDetailsField
            End Get
            Set
                Me.whereUsedDetailsField = value
            End Set
        End Property
        
        Public Property BomStructureDetails() As InvWhereReferencedQueryBomStructureDetails
            Get
                Return Me.bomStructureDetailsField
            End Get
            Set
                Me.bomStructureDetailsField = value
            End Set
        End Property
        
        Public Property SupplierStockCodeDetails() As InvWhereReferencedQuerySupplierStockCodeDetails
            Get
                Return Me.supplierStockCodeDetailsField
            End Get
            Set
                Me.supplierStockCodeDetailsField = value
            End Set
        End Property
        
        Public Property LctRoutesDetails() As InvWhereReferencedQueryLctRoutesDetails
            Get
                Return Me.lctRoutesDetailsField
            End Get
            Set
                Me.lctRoutesDetailsField = value
            End Set
        End Property
        
        Public Property BlanketPOrderDetails() As InvWhereReferencedQueryBlanketPOrderDetails
            Get
                Return Me.blanketPOrderDetailsField
            End Get
            Set
                Me.blanketPOrderDetailsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlArrayItemAttribute("ForeignPriceItem", IsNullable:=false)>  _
        Public Property ForeignPriceDetailsCollection() As List(Of InvWhereReferencedQueryForeignPriceItem)
            Get
                Return Me.foreignPriceDetailsField
            End Get
            Set
                Me.foreignPriceDetailsField = value
            End Set
        End Property
        
        Public Property PricingDetails() As InvWhereReferencedQueryPricingDetails
            Get
                Return Me.pricingDetailsField
            End Get
            Set
                Me.pricingDetailsField = value
            End Set
        End Property
        
        Public Property DispatchNotesDetails() As InvWhereReferencedQueryDispatchNotesDetails
            Get
                Return Me.dispatchNotesDetailsField
            End Get
            Set
                Me.dispatchNotesDetailsField = value
            End Set
        End Property
        
        Public Property CustomerStockCodeDetails() As InvWhereReferencedQueryCustomerStockCodeDetails
            Get
                Return Me.customerStockCodeDetailsField
            End Get
            Set
                Me.customerStockCodeDetailsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlArrayItemAttribute("ReferenceItem", IsNullable:=false)>  _
        Public Property GoodsInTransitDetailsCollection() As List(Of InvWhereReferencedQueryReferenceItem)
            Get
                Return Me.goodsInTransitDetailsField
            End Get
            Set
                Me.goodsInTransitDetailsField = value
            End Set
        End Property
        
        Public Property WarehouseItem() As InvWhereReferencedQueryWarehouseItem
            Get
                Return Me.warehouseItemField
            End Get
            Set
                Me.warehouseItemField = value
            End Set
        End Property
        
        Public Property RequisitionDetails() As InvWhereReferencedQueryRequisitionDetails
            Get
                Return Me.requisitionDetailsField
            End Get
            Set
                Me.requisitionDetailsField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQuerySystemInformation
        
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
    Partial Public Class InvWhereReferencedQueryStockItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private descriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private longDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private alternateKey1Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private alternateKey2Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private alternateUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private otherUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private partCategoryField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supplierField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supplierNameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private productClassField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private buyerField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private plannerField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseToUseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private traceableTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mpsFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supercessionDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private drawOfficeNumField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ebqField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dockToStockField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private manufLeadTimeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private tariffCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private userField1Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private userField2Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private userField3Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private userField4Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private userField5Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private leadTimeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private priceCategoryField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private taxCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private massField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private volumeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private abcClassField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private kitTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private clearingFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private listPriceCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private buyingRuleField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private panSizeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private eccFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private eccUserField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private makeToOrderFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private specificGravityField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private implosionNumField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private componentCountField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private fixTimePeriodField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockOnHoldField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockOnHoldReasonField As String
        
        Public Property StockCode() As String
            Get
                Return Me.stockCodeField
            End Get
            Set
                Me.stockCodeField = value
            End Set
        End Property
        
        Public Property Description() As String
            Get
                Return Me.descriptionField
            End Get
            Set
                Me.descriptionField = value
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
        
        Public Property AlternateKey1() As String
            Get
                Return Me.alternateKey1Field
            End Get
            Set
                Me.alternateKey1Field = value
            End Set
        End Property
        
        Public Property AlternateKey2() As String
            Get
                Return Me.alternateKey2Field
            End Get
            Set
                Me.alternateKey2Field = value
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
        
        Public Property StockUom() As String
            Get
                Return Me.stockUomField
            End Get
            Set
                Me.stockUomField = value
            End Set
        End Property
        
        Public Property AlternateUom() As String
            Get
                Return Me.alternateUomField
            End Get
            Set
                Me.alternateUomField = value
            End Set
        End Property
        
        Public Property OtherUom() As String
            Get
                Return Me.otherUomField
            End Get
            Set
                Me.otherUomField = value
            End Set
        End Property
        
        Public Property PartCategory() As String
            Get
                Return Me.partCategoryField
            End Get
            Set
                Me.partCategoryField = value
            End Set
        End Property
        
        Public Property Supplier() As String
            Get
                Return Me.supplierField
            End Get
            Set
                Me.supplierField = value
            End Set
        End Property
        
        Public Property SupplierName() As String
            Get
                Return Me.supplierNameField
            End Get
            Set
                Me.supplierNameField = value
            End Set
        End Property
        
        Public Property ProductClass() As String
            Get
                Return Me.productClassField
            End Get
            Set
                Me.productClassField = value
            End Set
        End Property
        
        Public Property Buyer() As String
            Get
                Return Me.buyerField
            End Get
            Set
                Me.buyerField = value
            End Set
        End Property
        
        Public Property Planner() As String
            Get
                Return Me.plannerField
            End Get
            Set
                Me.plannerField = value
            End Set
        End Property
        
        Public Property WarehouseToUse() As String
            Get
                Return Me.warehouseToUseField
            End Get
            Set
                Me.warehouseToUseField = value
            End Set
        End Property
        
        Public Property TraceableType() As String
            Get
                Return Me.traceableTypeField
            End Get
            Set
                Me.traceableTypeField = value
            End Set
        End Property
        
        Public Property MpsFlag() As String
            Get
                Return Me.mpsFlagField
            End Get
            Set
                Me.mpsFlagField = value
            End Set
        End Property
        
        Public Property SupercessionDate() As String
            Get
                Return Me.supercessionDateField
            End Get
            Set
                Me.supercessionDateField = value
            End Set
        End Property
        
        Public Property DrawOfficeNum() As String
            Get
                Return Me.drawOfficeNumField
            End Get
            Set
                Me.drawOfficeNumField = value
            End Set
        End Property
        
        Public Property Ebq() As String
            Get
                Return Me.ebqField
            End Get
            Set
                Me.ebqField = value
            End Set
        End Property
        
        Public Property DockToStock() As String
            Get
                Return Me.dockToStockField
            End Get
            Set
                Me.dockToStockField = value
            End Set
        End Property
        
        Public Property ManufLeadTime() As String
            Get
                Return Me.manufLeadTimeField
            End Get
            Set
                Me.manufLeadTimeField = value
            End Set
        End Property
        
        Public Property TariffCode() As String
            Get
                Return Me.tariffCodeField
            End Get
            Set
                Me.tariffCodeField = value
            End Set
        End Property
        
        Public Property UserField1() As String
            Get
                Return Me.userField1Field
            End Get
            Set
                Me.userField1Field = value
            End Set
        End Property
        
        Public Property UserField2() As String
            Get
                Return Me.userField2Field
            End Get
            Set
                Me.userField2Field = value
            End Set
        End Property
        
        Public Property UserField3() As String
            Get
                Return Me.userField3Field
            End Get
            Set
                Me.userField3Field = value
            End Set
        End Property
        
        Public Property UserField4() As String
            Get
                Return Me.userField4Field
            End Get
            Set
                Me.userField4Field = value
            End Set
        End Property
        
        Public Property UserField5() As String
            Get
                Return Me.userField5Field
            End Get
            Set
                Me.userField5Field = value
            End Set
        End Property
        
        Public Property LeadTime() As String
            Get
                Return Me.leadTimeField
            End Get
            Set
                Me.leadTimeField = value
            End Set
        End Property
        
        Public Property PriceCategory() As String
            Get
                Return Me.priceCategoryField
            End Get
            Set
                Me.priceCategoryField = value
            End Set
        End Property
        
        Public Property TaxCode() As String
            Get
                Return Me.taxCodeField
            End Get
            Set
                Me.taxCodeField = value
            End Set
        End Property
        
        Public Property Mass() As String
            Get
                Return Me.massField
            End Get
            Set
                Me.massField = value
            End Set
        End Property
        
        Public Property Volume() As String
            Get
                Return Me.volumeField
            End Get
            Set
                Me.volumeField = value
            End Set
        End Property
        
        Public Property AbcClass() As String
            Get
                Return Me.abcClassField
            End Get
            Set
                Me.abcClassField = value
            End Set
        End Property
        
        Public Property KitType() As String
            Get
                Return Me.kitTypeField
            End Get
            Set
                Me.kitTypeField = value
            End Set
        End Property
        
        Public Property ClearingFlag() As String
            Get
                Return Me.clearingFlagField
            End Get
            Set
                Me.clearingFlagField = value
            End Set
        End Property
        
        Public Property ListPriceCode() As String
            Get
                Return Me.listPriceCodeField
            End Get
            Set
                Me.listPriceCodeField = value
            End Set
        End Property
        
        Public Property BuyingRule() As String
            Get
                Return Me.buyingRuleField
            End Get
            Set
                Me.buyingRuleField = value
            End Set
        End Property
        
        Public Property PanSize() As String
            Get
                Return Me.panSizeField
            End Get
            Set
                Me.panSizeField = value
            End Set
        End Property
        
        Public Property EccFlag() As String
            Get
                Return Me.eccFlagField
            End Get
            Set
                Me.eccFlagField = value
            End Set
        End Property
        
        Public Property EccUser() As String
            Get
                Return Me.eccUserField
            End Get
            Set
                Me.eccUserField = value
            End Set
        End Property
        
        Public Property MakeToOrderFlag() As String
            Get
                Return Me.makeToOrderFlagField
            End Get
            Set
                Me.makeToOrderFlagField = value
            End Set
        End Property
        
        Public Property SpecificGravity() As String
            Get
                Return Me.specificGravityField
            End Get
            Set
                Me.specificGravityField = value
            End Set
        End Property
        
        Public Property ImplosionNum() As String
            Get
                Return Me.implosionNumField
            End Get
            Set
                Me.implosionNumField = value
            End Set
        End Property
        
        Public Property ComponentCount() As String
            Get
                Return Me.componentCountField
            End Get
            Set
                Me.componentCountField = value
            End Set
        End Property
        
        Public Property FixTimePeriod() As String
            Get
                Return Me.fixTimePeriodField
            End Get
            Set
                Me.fixTimePeriodField = value
            End Set
        End Property
        
        Public Property StockOnHold() As String
            Get
                Return Me.stockOnHoldField
            End Get
            Set
                Me.stockOnHoldField = value
            End Set
        End Property
        
        Public Property StockOnHoldReason() As String
            Get
                Return Me.stockOnHoldReasonField
            End Get
            Set
                Me.stockOnHoldReasonField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryPurchaseOrderDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private purchaseOrderItemField As InvWhereReferencedQueryPurchaseOrderDetailsPurchaseOrderItem
        
        Public Sub New()
            MyBase.New
            If (Me.purchaseOrderItemField Is Nothing) Then
                Me.purchaseOrderItemField = New InvWhereReferencedQueryPurchaseOrderDetailsPurchaseOrderItem
            End If
        End Sub
        
        Public Property PurchaseOrderItem() As InvWhereReferencedQueryPurchaseOrderDetailsPurchaseOrderItem
            Get
                Return Me.purchaseOrderItemField
            End Get
            Set
                Me.purchaseOrderItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryPurchaseOrderDetailsPurchaseOrderItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private purchaseOrderField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lineTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private orderStatusField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mWarehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mOrderUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mStockingUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mOrderQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mReceivedQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mLatestDueDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mLastReceiptDatField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mSupCatalogueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mDiscPct1Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mDiscPct2Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mDiscPct3Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mDiscValFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mDiscValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mPriceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mForeignPriceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mPriceUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mTaxCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mProductClassField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mCompleteFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mJobField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mGlCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mUserAuthReqnField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mRequisitionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mRequisitionLineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mSalesOrderField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mSalesOrderLineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mOrigDueDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mReschedDueDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mLctConfirmedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mOriginalLineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mVersionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mReleaseField As String
        
        Public Property PurchaseOrder() As String
            Get
                Return Me.purchaseOrderField
            End Get
            Set
                Me.purchaseOrderField = value
            End Set
        End Property
        
        Public Property Line() As String
            Get
                Return Me.lineField
            End Get
            Set
                Me.lineField = value
            End Set
        End Property
        
        Public Property LineType() As String
            Get
                Return Me.lineTypeField
            End Get
            Set
                Me.lineTypeField = value
            End Set
        End Property
        
        Public Property OrderStatus() As String
            Get
                Return Me.orderStatusField
            End Get
            Set
                Me.orderStatusField = value
            End Set
        End Property
        
        Public Property MWarehouse() As String
            Get
                Return Me.mWarehouseField
            End Get
            Set
                Me.mWarehouseField = value
            End Set
        End Property
        
        Public Property MOrderUom() As String
            Get
                Return Me.mOrderUomField
            End Get
            Set
                Me.mOrderUomField = value
            End Set
        End Property
        
        Public Property MStockingUom() As String
            Get
                Return Me.mStockingUomField
            End Get
            Set
                Me.mStockingUomField = value
            End Set
        End Property
        
        Public Property MOrderQty() As String
            Get
                Return Me.mOrderQtyField
            End Get
            Set
                Me.mOrderQtyField = value
            End Set
        End Property
        
        Public Property MReceivedQty() As String
            Get
                Return Me.mReceivedQtyField
            End Get
            Set
                Me.mReceivedQtyField = value
            End Set
        End Property
        
        Public Property MLatestDueDate() As String
            Get
                Return Me.mLatestDueDateField
            End Get
            Set
                Me.mLatestDueDateField = value
            End Set
        End Property
        
        Public Property MLastReceiptDat() As String
            Get
                Return Me.mLastReceiptDatField
            End Get
            Set
                Me.mLastReceiptDatField = value
            End Set
        End Property
        
        Public Property MSupCatalogue() As String
            Get
                Return Me.mSupCatalogueField
            End Get
            Set
                Me.mSupCatalogueField = value
            End Set
        End Property
        
        Public Property MDiscPct1() As String
            Get
                Return Me.mDiscPct1Field
            End Get
            Set
                Me.mDiscPct1Field = value
            End Set
        End Property
        
        Public Property MDiscPct2() As String
            Get
                Return Me.mDiscPct2Field
            End Get
            Set
                Me.mDiscPct2Field = value
            End Set
        End Property
        
        Public Property MDiscPct3() As String
            Get
                Return Me.mDiscPct3Field
            End Get
            Set
                Me.mDiscPct3Field = value
            End Set
        End Property
        
        Public Property MDiscValFlag() As String
            Get
                Return Me.mDiscValFlagField
            End Get
            Set
                Me.mDiscValFlagField = value
            End Set
        End Property
        
        Public Property MDiscValue() As String
            Get
                Return Me.mDiscValueField
            End Get
            Set
                Me.mDiscValueField = value
            End Set
        End Property
        
        Public Property MPrice() As String
            Get
                Return Me.mPriceField
            End Get
            Set
                Me.mPriceField = value
            End Set
        End Property
        
        Public Property MForeignPrice() As String
            Get
                Return Me.mForeignPriceField
            End Get
            Set
                Me.mForeignPriceField = value
            End Set
        End Property
        
        Public Property MPriceUom() As String
            Get
                Return Me.mPriceUomField
            End Get
            Set
                Me.mPriceUomField = value
            End Set
        End Property
        
        Public Property MTaxCode() As String
            Get
                Return Me.mTaxCodeField
            End Get
            Set
                Me.mTaxCodeField = value
            End Set
        End Property
        
        Public Property MProductClass() As String
            Get
                Return Me.mProductClassField
            End Get
            Set
                Me.mProductClassField = value
            End Set
        End Property
        
        Public Property MCompleteFlag() As String
            Get
                Return Me.mCompleteFlagField
            End Get
            Set
                Me.mCompleteFlagField = value
            End Set
        End Property
        
        Public Property MJob() As String
            Get
                Return Me.mJobField
            End Get
            Set
                Me.mJobField = value
            End Set
        End Property
        
        Public Property MGlCode() As String
            Get
                Return Me.mGlCodeField
            End Get
            Set
                Me.mGlCodeField = value
            End Set
        End Property
        
        Public Property MUserAuthReqn() As String
            Get
                Return Me.mUserAuthReqnField
            End Get
            Set
                Me.mUserAuthReqnField = value
            End Set
        End Property
        
        Public Property MRequisition() As String
            Get
                Return Me.mRequisitionField
            End Get
            Set
                Me.mRequisitionField = value
            End Set
        End Property
        
        Public Property MRequisitionLine() As String
            Get
                Return Me.mRequisitionLineField
            End Get
            Set
                Me.mRequisitionLineField = value
            End Set
        End Property
        
        Public Property MSalesOrder() As String
            Get
                Return Me.mSalesOrderField
            End Get
            Set
                Me.mSalesOrderField = value
            End Set
        End Property
        
        Public Property MSalesOrderLine() As String
            Get
                Return Me.mSalesOrderLineField
            End Get
            Set
                Me.mSalesOrderLineField = value
            End Set
        End Property
        
        Public Property MOrigDueDate() As String
            Get
                Return Me.mOrigDueDateField
            End Get
            Set
                Me.mOrigDueDateField = value
            End Set
        End Property
        
        Public Property MReschedDueDate() As String
            Get
                Return Me.mReschedDueDateField
            End Get
            Set
                Me.mReschedDueDateField = value
            End Set
        End Property
        
        Public Property MLctConfirmed() As String
            Get
                Return Me.mLctConfirmedField
            End Get
            Set
                Me.mLctConfirmedField = value
            End Set
        End Property
        
        Public Property MOriginalLine() As String
            Get
                Return Me.mOriginalLineField
            End Get
            Set
                Me.mOriginalLineField = value
            End Set
        End Property
        
        Public Property MVersion() As String
            Get
                Return Me.mVersionField
            End Get
            Set
                Me.mVersionField = value
            End Set
        End Property
        
        Public Property MRelease() As String
            Get
                Return Me.mReleaseField
            End Get
            Set
                Me.mReleaseField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQuerySalesOrderDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesOrderItemField As InvWhereReferencedQuerySalesOrderDetailsSalesOrderItem
        
        Public Sub New()
            MyBase.New
            If (Me.salesOrderItemField Is Nothing) Then
                Me.salesOrderItemField = New InvWhereReferencedQuerySalesOrderDetailsSalesOrderItem
            End If
        End Sub
        
        Public Property SalesOrderItem() As InvWhereReferencedQuerySalesOrderDetailsSalesOrderItem
            Get
                Return Me.salesOrderItemField
            End Get
            Set
                Me.salesOrderItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQuerySalesOrderDetailsSalesOrderItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private itemsField As List(Of String)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private itemsElementNameField As List(Of ItemsChoiceType)
        
        Public Sub New()
            MyBase.New
            If (Me.itemsElementNameField Is Nothing) Then
                Me.itemsElementNameField = New List(Of ItemsChoiceType)
            End If
            If (Me.itemsField Is Nothing) Then
                Me.itemsField = New List(Of String)
            End If
        End Sub
        
        <System.Xml.Serialization.XmlElementAttribute("Customer", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MBackOrderQty", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MBin", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MBomFlag", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MCommissionCode", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MCommitDate", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MCreditOrderLine", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MCreditOrderNo", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MCustRequestDat", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MDiscPct1", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MDiscPct2", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MDiscPct3", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MDiscValFlag", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MDiscValue", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MEccFlag", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MHierarchyJob", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MLastDelNote", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MLeadTime", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MLineReceiptDat", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MLineShipDate", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MOrderQty", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MOrderUom", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MParentKitType", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MPrice", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MPriceUom", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MProductClass", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MQtyDispatched", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MRelease", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MShipQty", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MStockQtyToShp", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MStockingUom", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MTaxCode", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MUnitCost", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MVersion", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("MWarehouse", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("OrderStatus", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("SalesOrder", GetType(String)),  _
         System.Xml.Serialization.XmlElementAttribute("SalesOrderLine", GetType(String)),  _
         System.Xml.Serialization.XmlChoiceIdentifierAttribute("ItemsElementName")>  _
        Public Property ItemsCollection() As List(Of String)
            Get
                Return Me.itemsField
            End Get
            Set
                Me.itemsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute("ItemsElementName"),  _
         System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public Property ItemsElementNameCollection() As List(Of ItemsChoiceType)
            Get
                Return Me.itemsElementNameField
            End Get
            Set
                Me.itemsElementNameField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(IncludeInSchema:=false)>  _
    Public Enum ItemsChoiceType
        
        '''<remarks/>
        Customer
        
        '''<remarks/>
        MBackOrderQty
        
        '''<remarks/>
        MBin
        
        '''<remarks/>
        MBomFlag
        
        '''<remarks/>
        MCommissionCode
        
        '''<remarks/>
        MCommitDate
        
        '''<remarks/>
        MCreditOrderLine
        
        '''<remarks/>
        MCreditOrderNo
        
        '''<remarks/>
        MCustRequestDat
        
        '''<remarks/>
        MDiscPct1
        
        '''<remarks/>
        MDiscPct2
        
        '''<remarks/>
        MDiscPct3
        
        '''<remarks/>
        MDiscValFlag
        
        '''<remarks/>
        MDiscValue
        
        '''<remarks/>
        MEccFlag
        
        '''<remarks/>
        MHierarchyJob
        
        '''<remarks/>
        MLastDelNote
        
        '''<remarks/>
        MLeadTime
        
        '''<remarks/>
        MLineReceiptDat
        
        '''<remarks/>
        MLineShipDate
        
        '''<remarks/>
        MOrderQty
        
        '''<remarks/>
        MOrderUom
        
        '''<remarks/>
        MParentKitType
        
        '''<remarks/>
        MPrice
        
        '''<remarks/>
        MPriceUom
        
        '''<remarks/>
        MProductClass
        
        '''<remarks/>
        MQtyDispatched
        
        '''<remarks/>
        MRelease
        
        '''<remarks/>
        MShipQty
        
        '''<remarks/>
        MStockQtyToShp
        
        '''<remarks/>
        MStockingUom
        
        '''<remarks/>
        MTaxCode
        
        '''<remarks/>
        MUnitCost
        
        '''<remarks/>
        MVersion
        
        '''<remarks/>
        MWarehouse
        
        '''<remarks/>
        OrderStatus
        
        '''<remarks/>
        SalesOrder
        
        '''<remarks/>
        SalesOrderLine
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQuerySctDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private sctItemField As InvWhereReferencedQuerySctDetailsSctItem
        
        Public Sub New()
            MyBase.New
            If (Me.sctItemField Is Nothing) Then
                Me.sctItemField = New InvWhereReferencedQuerySctDetailsSctItem
            End If
        End Sub
        
        Public Property SctItem() As InvWhereReferencedQuerySctDetailsSctItem
            Get
                Return Me.sctItemField
            End Get
            Set
                Me.sctItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQuerySctDetailsSctItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesOrderField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private orderStatusField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reqShipDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private orderDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mOrderQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mWarehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mBinField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mShipQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mBackOrderQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mUnitCostField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mBomFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mParentKitTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mOrderUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mPriceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mPriceUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mCommitDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mVersionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mReleaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private allocatedFlagField As String
        
        Public Property SalesOrder() As String
            Get
                Return Me.salesOrderField
            End Get
            Set
                Me.salesOrderField = value
            End Set
        End Property
        
        Public Property OrderStatus() As String
            Get
                Return Me.orderStatusField
            End Get
            Set
                Me.orderStatusField = value
            End Set
        End Property
        
        Public Property ReqShipDate() As String
            Get
                Return Me.reqShipDateField
            End Get
            Set
                Me.reqShipDateField = value
            End Set
        End Property
        
        Public Property OrderDate() As String
            Get
                Return Me.orderDateField
            End Get
            Set
                Me.orderDateField = value
            End Set
        End Property
        
        Public Property MOrderQty() As String
            Get
                Return Me.mOrderQtyField
            End Get
            Set
                Me.mOrderQtyField = value
            End Set
        End Property
        
        Public Property MWarehouse() As String
            Get
                Return Me.mWarehouseField
            End Get
            Set
                Me.mWarehouseField = value
            End Set
        End Property
        
        Public Property MBin() As String
            Get
                Return Me.mBinField
            End Get
            Set
                Me.mBinField = value
            End Set
        End Property
        
        Public Property MShipQty() As String
            Get
                Return Me.mShipQtyField
            End Get
            Set
                Me.mShipQtyField = value
            End Set
        End Property
        
        Public Property MBackOrderQty() As String
            Get
                Return Me.mBackOrderQtyField
            End Get
            Set
                Me.mBackOrderQtyField = value
            End Set
        End Property
        
        Public Property MUnitCost() As String
            Get
                Return Me.mUnitCostField
            End Get
            Set
                Me.mUnitCostField = value
            End Set
        End Property
        
        Public Property MBomFlag() As String
            Get
                Return Me.mBomFlagField
            End Get
            Set
                Me.mBomFlagField = value
            End Set
        End Property
        
        Public Property MParentKitType() As String
            Get
                Return Me.mParentKitTypeField
            End Get
            Set
                Me.mParentKitTypeField = value
            End Set
        End Property
        
        Public Property MOrderUom() As String
            Get
                Return Me.mOrderUomField
            End Get
            Set
                Me.mOrderUomField = value
            End Set
        End Property
        
        Public Property MPrice() As String
            Get
                Return Me.mPriceField
            End Get
            Set
                Me.mPriceField = value
            End Set
        End Property
        
        Public Property MPriceUom() As String
            Get
                Return Me.mPriceUomField
            End Get
            Set
                Me.mPriceUomField = value
            End Set
        End Property
        
        Public Property MCommitDate() As String
            Get
                Return Me.mCommitDateField
            End Get
            Set
                Me.mCommitDateField = value
            End Set
        End Property
        
        Public Property MVersion() As String
            Get
                Return Me.mVersionField
            End Get
            Set
                Me.mVersionField = value
            End Set
        End Property
        
        Public Property MRelease() As String
            Get
                Return Me.mReleaseField
            End Get
            Set
                Me.mReleaseField = value
            End Set
        End Property
        
        Public Property AllocatedFlag() As String
            Get
                Return Me.allocatedFlagField
            End Get
            Set
                Me.allocatedFlagField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryEccDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private eccItemField As InvWhereReferencedQueryEccDetailsEccItem
        
        Public Sub New()
            MyBase.New
            If (Me.eccItemField Is Nothing) Then
                Me.eccItemField = New InvWhereReferencedQueryEccDetailsEccItem
            End If
        End Sub
        
        Public Property EccItem() As InvWhereReferencedQueryEccDetailsEccItem
            Get
                Return Me.eccItemField
            End Get
            Set
                Me.eccItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryEccDetailsEccItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOnHandField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private availableQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOnOrderField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyInInspectionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOnBackOrderField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyAllocatedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyAllocatedWipField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyInTransitField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyDispatchedField As String
        
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
        
        Public Property QtyOnHand() As String
            Get
                Return Me.qtyOnHandField
            End Get
            Set
                Me.qtyOnHandField = value
            End Set
        End Property
        
        Public Property AvailableQty() As String
            Get
                Return Me.availableQtyField
            End Get
            Set
                Me.availableQtyField = value
            End Set
        End Property
        
        Public Property QtyOnOrder() As String
            Get
                Return Me.qtyOnOrderField
            End Get
            Set
                Me.qtyOnOrderField = value
            End Set
        End Property
        
        Public Property QtyInInspection() As String
            Get
                Return Me.qtyInInspectionField
            End Get
            Set
                Me.qtyInInspectionField = value
            End Set
        End Property
        
        Public Property QtyOnBackOrder() As String
            Get
                Return Me.qtyOnBackOrderField
            End Get
            Set
                Me.qtyOnBackOrderField = value
            End Set
        End Property
        
        Public Property QtyAllocated() As String
            Get
                Return Me.qtyAllocatedField
            End Get
            Set
                Me.qtyAllocatedField = value
            End Set
        End Property
        
        Public Property QtyAllocatedWip() As String
            Get
                Return Me.qtyAllocatedWipField
            End Get
            Set
                Me.qtyAllocatedWipField = value
            End Set
        End Property
        
        Public Property QtyInTransit() As String
            Get
                Return Me.qtyInTransitField
            End Get
            Set
                Me.qtyInTransitField = value
            End Set
        End Property
        
        Public Property QtyDispatched() As String
            Get
                Return Me.qtyDispatchedField
            End Get
            Set
                Me.qtyDispatchedField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryAlternateStockCodeDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeItemField As InvWhereReferencedQueryAlternateStockCodeDetailsStockCodeItem
        
        Public Sub New()
            MyBase.New
            If (Me.stockCodeItemField Is Nothing) Then
                Me.stockCodeItemField = New InvWhereReferencedQueryAlternateStockCodeDetailsStockCodeItem
            End If
        End Sub
        
        Public Property StockCodeItem() As InvWhereReferencedQueryAlternateStockCodeDetailsStockCodeItem
            Get
                Return Me.stockCodeItemField
            End Get
            Set
                Me.stockCodeItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryAlternateStockCodeDetailsStockCodeItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private descriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private longDescField As String
        
        Public Property StockCode() As String
            Get
                Return Me.stockCodeField
            End Get
            Set
                Me.stockCodeField = value
            End Set
        End Property
        
        Public Property Description() As String
            Get
                Return Me.descriptionField
            End Get
            Set
                Me.descriptionField = value
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
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryAlternateSupplierDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supplierItemField As InvWhereReferencedQueryAlternateSupplierDetailsSupplierItem
        
        Public Sub New()
            MyBase.New
            If (Me.supplierItemField Is Nothing) Then
                Me.supplierItemField = New InvWhereReferencedQueryAlternateSupplierDetailsSupplierItem
            End If
        End Sub
        
        Public Property SupplierItem() As InvWhereReferencedQueryAlternateSupplierDetailsSupplierItem
            Get
                Return Me.supplierItemField
            End Get
            Set
                Me.supplierItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryAlternateSupplierDetailsSupplierItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supplierField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supplierNameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currencyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lastPricePaidField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lastPrcUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lastReceiptQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lastReceiptDateField As String
        
        Public Property Supplier() As String
            Get
                Return Me.supplierField
            End Get
            Set
                Me.supplierField = value
            End Set
        End Property
        
        Public Property SupplierName() As String
            Get
                Return Me.supplierNameField
            End Get
            Set
                Me.supplierNameField = value
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
        
        Public Property LastPricePaid() As String
            Get
                Return Me.lastPricePaidField
            End Get
            Set
                Me.lastPricePaidField = value
            End Set
        End Property
        
        Public Property LastPrcUom() As String
            Get
                Return Me.lastPrcUomField
            End Get
            Set
                Me.lastPrcUomField = value
            End Set
        End Property
        
        Public Property LastReceiptQty() As String
            Get
                Return Me.lastReceiptQtyField
            End Get
            Set
                Me.lastReceiptQtyField = value
            End Set
        End Property
        
        Public Property LastReceiptDate() As String
            Get
                Return Me.lastReceiptDateField
            End Get
            Set
                Me.lastReceiptDateField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWorkInProgressDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobField As InvWhereReferencedQueryWorkInProgressDetailsJob
        
        Public Sub New()
            MyBase.New
            If (Me.jobField Is Nothing) Then
                Me.jobField = New InvWhereReferencedQueryWorkInProgressDetailsJob
            End If
        End Sub
        
        Public Property Job() As InvWhereReferencedQueryWorkInProgressDetailsJob
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
    Partial Public Class InvWhereReferencedQueryWorkInProgressDetailsJob
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private masterField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobNumberField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobClassificationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private classDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private longDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iNVWHC_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerNameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDeliveryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobStartDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private completeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private holdFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expLabourField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expMaterialField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyToMakeField As String
        
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
        Private hrsBookToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobField As List(Of InvWhereReferencedQueryWorkInProgressDetailsJobJob)
        
        Public Sub New()
            MyBase.New
            If (Me.jobField Is Nothing) Then
                Me.jobField = New List(Of InvWhereReferencedQueryWorkInProgressDetailsJobJob)
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
        
        Public Property JobClassification() As String
            Get
                Return Me.jobClassificationField
            End Get
            Set
                Me.jobClassificationField = value
            End Set
        End Property
        
        Public Property ClassDescription() As String
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
        
        Public Property Customer() As String
            Get
                Return Me.customerField
            End Get
            Set
                Me.customerField = value
            End Set
        End Property
        
        Public Property CustomerName() As String
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
        
        Public Property HoldFlag() As String
            Get
                Return Me.holdFlagField
            End Get
            Set
                Me.holdFlagField = value
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
        
        Public Property HrsBookToDate() As String
            Get
                Return Me.hrsBookToDateField
            End Get
            Set
                Me.hrsBookToDateField = value
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
        
        <System.Xml.Serialization.XmlElementAttribute("Job")>  _
        Public Property JobCollection() As List(Of InvWhereReferencedQueryWorkInProgressDetailsJobJob)
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
    Partial Public Class InvWhereReferencedQueryWorkInProgressDetailsJobJob
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private masterSubField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private subField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobNumberField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobClassificationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private classDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private longDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iNVWHC_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerNameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDeliveryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobStartDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private completeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private holdFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expLabourField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expMaterialField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyToMakeField As String
        
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
        Private hrsBookToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobField As InvWhereReferencedQueryWorkInProgressDetailsJobJobJob
        
        Public Sub New()
            MyBase.New
            If (Me.jobField Is Nothing) Then
                Me.jobField = New InvWhereReferencedQueryWorkInProgressDetailsJobJobJob
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
        
        Public Property JobClassification() As String
            Get
                Return Me.jobClassificationField
            End Get
            Set
                Me.jobClassificationField = value
            End Set
        End Property
        
        Public Property ClassDescription() As String
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
        
        Public Property Customer() As String
            Get
                Return Me.customerField
            End Get
            Set
                Me.customerField = value
            End Set
        End Property
        
        Public Property CustomerName() As String
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
        
        Public Property HoldFlag() As String
            Get
                Return Me.holdFlagField
            End Get
            Set
                Me.holdFlagField = value
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
        
        Public Property HrsBookToDate() As String
            Get
                Return Me.hrsBookToDateField
            End Get
            Set
                Me.hrsBookToDateField = value
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
        
        Public Property Job() As InvWhereReferencedQueryWorkInProgressDetailsJobJobJob
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
    Partial Public Class InvWhereReferencedQueryWorkInProgressDetailsJobJobJob
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private subField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobNumberField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobClassificationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private classDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private longDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iNVWHC_DescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerNameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobDeliveryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobStartDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private completeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private holdFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expLabourField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expMaterialField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyToMakeField As String
        
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
        Private hrsBookToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matCostToDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labValueIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private matValueIssuesField As String
        
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
        
        Public Property JobClassification() As String
            Get
                Return Me.jobClassificationField
            End Get
            Set
                Me.jobClassificationField = value
            End Set
        End Property
        
        Public Property ClassDescription() As String
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
        
        Public Property Customer() As String
            Get
                Return Me.customerField
            End Get
            Set
                Me.customerField = value
            End Set
        End Property
        
        Public Property CustomerName() As String
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
        
        Public Property HoldFlag() As String
            Get
                Return Me.holdFlagField
            End Get
            Set
                Me.holdFlagField = value
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
        
        Public Property HrsBookToDate() As String
            Get
                Return Me.hrsBookToDateField
            End Get
            Set
                Me.hrsBookToDateField = value
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
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWhereUsedDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private parentPartItemField As InvWhereReferencedQueryWhereUsedDetailsParentPartItem
        
        Public Sub New()
            MyBase.New
            If (Me.parentPartItemField Is Nothing) Then
                Me.parentPartItemField = New InvWhereReferencedQueryWhereUsedDetailsParentPartItem
            End If
        End Sub
        
        Public Property ParentPartItem() As InvWhereReferencedQueryWhereUsedDetailsParentPartItem
            Get
                Return Me.parentPartItemField
            End Get
            Set
                Me.parentPartItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWhereUsedDetailsParentPartItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private parentPartField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private partCategoryField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private descriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private comVersionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private comReleaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private routeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private sequenceNumField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private structureOffDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private structureOnDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationOffsetField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyPerField As String
        
        Public Property ParentPart() As String
            Get
                Return Me.parentPartField
            End Get
            Set
                Me.parentPartField = value
            End Set
        End Property
        
        Public Property PartCategory() As String
            Get
                Return Me.partCategoryField
            End Get
            Set
                Me.partCategoryField = value
            End Set
        End Property
        
        Public Property Description() As String
            Get
                Return Me.descriptionField
            End Get
            Set
                Me.descriptionField = value
            End Set
        End Property
        
        Public Property ComVersion() As String
            Get
                Return Me.comVersionField
            End Get
            Set
                Me.comVersionField = value
            End Set
        End Property
        
        Public Property ComRelease() As String
            Get
                Return Me.comReleaseField
            End Get
            Set
                Me.comReleaseField = value
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
        
        Public Property SequenceNum() As String
            Get
                Return Me.sequenceNumField
            End Get
            Set
                Me.sequenceNumField = value
            End Set
        End Property
        
        Public Property StructureOffDate() As String
            Get
                Return Me.structureOffDateField
            End Get
            Set
                Me.structureOffDateField = value
            End Set
        End Property
        
        Public Property StructureOnDate() As String
            Get
                Return Me.structureOnDateField
            End Get
            Set
                Me.structureOnDateField = value
            End Set
        End Property
        
        Public Property OperationOffset() As String
            Get
                Return Me.operationOffsetField
            End Get
            Set
                Me.operationOffsetField = value
            End Set
        End Property
        
        Public Property QtyPer() As String
            Get
                Return Me.qtyPerField
            End Get
            Set
                Me.qtyPerField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryBomStructureDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationField As InvWhereReferencedQueryBomStructureDetailsOperation
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private componentField As InvWhereReferencedQueryBomStructureDetailsComponent
        
        Public Sub New()
            MyBase.New
            If (Me.componentField Is Nothing) Then
                Me.componentField = New InvWhereReferencedQueryBomStructureDetailsComponent
            End If
            If (Me.operationField Is Nothing) Then
                Me.operationField = New InvWhereReferencedQueryBomStructureDetailsOperation
            End If
        End Sub
        
        Public Property Operation() As InvWhereReferencedQueryBomStructureDetailsOperation
            Get
                Return Me.operationField
            End Get
            Set
                Me.operationField = value
            End Set
        End Property
        
        Public Property Component() As InvWhereReferencedQueryBomStructureDetailsComponent
            Get
                Return Me.componentField
            End Get
            Set
                Me.componentField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryBomStructureDetailsOperation
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private workCentreField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private descriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private timeUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private productClassField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private wcRateIndField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private whatIfWcIndField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private subcontractFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iSetUpTimeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iRunTimeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iStartupTimeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iTeardownTimeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iWaitTimeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iStartupQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iMachineField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iUnitCapacityField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iMaxWorkOpertrsField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iMaxProdUnitsField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iTimeTakenField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private iQuantityField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private milestoneField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private elapsedTimeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private movementTimeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private narrationCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private autoNarrCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private numOfPiecesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private inspectionFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operYieldPctField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operYieldQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private minorSetupField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private minorSetupCodeField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private toolSetField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private toolSetQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private toolConsumptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private timeCalcFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private oprSubContractField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private oprSubContractCostField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private oprLabourField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private oprFixedOverHeadField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private oprVariableOverHeadField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationCostTotalField As String
        
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
        
        Public Property Description() As String
            Get
                Return Me.descriptionField
            End Get
            Set
                Me.descriptionField = value
            End Set
        End Property
        
        Public Property TimeUom() As String
            Get
                Return Me.timeUomField
            End Get
            Set
                Me.timeUomField = value
            End Set
        End Property
        
        Public Property ProductClass() As Object
            Get
                Return Me.productClassField
            End Get
            Set
                Me.productClassField = value
            End Set
        End Property
        
        Public Property WcRateInd() As String
            Get
                Return Me.wcRateIndField
            End Get
            Set
                Me.wcRateIndField = value
            End Set
        End Property
        
        Public Property WhatIfWcInd() As String
            Get
                Return Me.whatIfWcIndField
            End Get
            Set
                Me.whatIfWcIndField = value
            End Set
        End Property
        
        Public Property SubcontractFlag() As String
            Get
                Return Me.subcontractFlagField
            End Get
            Set
                Me.subcontractFlagField = value
            End Set
        End Property
        
        Public Property ISetUpTime() As String
            Get
                Return Me.iSetUpTimeField
            End Get
            Set
                Me.iSetUpTimeField = value
            End Set
        End Property
        
        Public Property IRunTime() As String
            Get
                Return Me.iRunTimeField
            End Get
            Set
                Me.iRunTimeField = value
            End Set
        End Property
        
        Public Property IStartupTime() As String
            Get
                Return Me.iStartupTimeField
            End Get
            Set
                Me.iStartupTimeField = value
            End Set
        End Property
        
        Public Property ITeardownTime() As String
            Get
                Return Me.iTeardownTimeField
            End Get
            Set
                Me.iTeardownTimeField = value
            End Set
        End Property
        
        Public Property IWaitTime() As String
            Get
                Return Me.iWaitTimeField
            End Get
            Set
                Me.iWaitTimeField = value
            End Set
        End Property
        
        Public Property IStartupQty() As String
            Get
                Return Me.iStartupQtyField
            End Get
            Set
                Me.iStartupQtyField = value
            End Set
        End Property
        
        Public Property IMachine() As Object
            Get
                Return Me.iMachineField
            End Get
            Set
                Me.iMachineField = value
            End Set
        End Property
        
        Public Property IUnitCapacity() As String
            Get
                Return Me.iUnitCapacityField
            End Get
            Set
                Me.iUnitCapacityField = value
            End Set
        End Property
        
        Public Property IMaxWorkOpertrs() As String
            Get
                Return Me.iMaxWorkOpertrsField
            End Get
            Set
                Me.iMaxWorkOpertrsField = value
            End Set
        End Property
        
        Public Property IMaxProdUnits() As String
            Get
                Return Me.iMaxProdUnitsField
            End Get
            Set
                Me.iMaxProdUnitsField = value
            End Set
        End Property
        
        Public Property ITimeTaken() As String
            Get
                Return Me.iTimeTakenField
            End Get
            Set
                Me.iTimeTakenField = value
            End Set
        End Property
        
        Public Property IQuantity() As String
            Get
                Return Me.iQuantityField
            End Get
            Set
                Me.iQuantityField = value
            End Set
        End Property
        
        Public Property Milestone() As String
            Get
                Return Me.milestoneField
            End Get
            Set
                Me.milestoneField = value
            End Set
        End Property
        
        Public Property ElapsedTime() As String
            Get
                Return Me.elapsedTimeField
            End Get
            Set
                Me.elapsedTimeField = value
            End Set
        End Property
        
        Public Property MovementTime() As String
            Get
                Return Me.movementTimeField
            End Get
            Set
                Me.movementTimeField = value
            End Set
        End Property
        
        Public Property NarrationCode() As String
            Get
                Return Me.narrationCodeField
            End Get
            Set
                Me.narrationCodeField = value
            End Set
        End Property
        
        Public Property AutoNarrCode() As String
            Get
                Return Me.autoNarrCodeField
            End Get
            Set
                Me.autoNarrCodeField = value
            End Set
        End Property
        
        Public Property NumOfPieces() As String
            Get
                Return Me.numOfPiecesField
            End Get
            Set
                Me.numOfPiecesField = value
            End Set
        End Property
        
        Public Property InspectionFlag() As String
            Get
                Return Me.inspectionFlagField
            End Get
            Set
                Me.inspectionFlagField = value
            End Set
        End Property
        
        Public Property OperYieldPct() As String
            Get
                Return Me.operYieldPctField
            End Get
            Set
                Me.operYieldPctField = value
            End Set
        End Property
        
        Public Property OperYieldQty() As String
            Get
                Return Me.operYieldQtyField
            End Get
            Set
                Me.operYieldQtyField = value
            End Set
        End Property
        
        Public Property MinorSetup() As String
            Get
                Return Me.minorSetupField
            End Get
            Set
                Me.minorSetupField = value
            End Set
        End Property
        
        Public Property MinorSetupCode() As Object
            Get
                Return Me.minorSetupCodeField
            End Get
            Set
                Me.minorSetupCodeField = value
            End Set
        End Property
        
        Public Property ToolSet() As String
            Get
                Return Me.toolSetField
            End Get
            Set
                Me.toolSetField = value
            End Set
        End Property
        
        Public Property ToolSetQty() As String
            Get
                Return Me.toolSetQtyField
            End Get
            Set
                Me.toolSetQtyField = value
            End Set
        End Property
        
        Public Property ToolConsumption() As String
            Get
                Return Me.toolConsumptionField
            End Get
            Set
                Me.toolConsumptionField = value
            End Set
        End Property
        
        Public Property TimeCalcFlag() As String
            Get
                Return Me.timeCalcFlagField
            End Get
            Set
                Me.timeCalcFlagField = value
            End Set
        End Property
        
        Public Property OprSubContract() As String
            Get
                Return Me.oprSubContractField
            End Get
            Set
                Me.oprSubContractField = value
            End Set
        End Property
        
        Public Property OprSubContractCost() As String
            Get
                Return Me.oprSubContractCostField
            End Get
            Set
                Me.oprSubContractCostField = value
            End Set
        End Property
        
        Public Property OprLabour() As String
            Get
                Return Me.oprLabourField
            End Get
            Set
                Me.oprLabourField = value
            End Set
        End Property
        
        Public Property OprFixedOverHead() As String
            Get
                Return Me.oprFixedOverHeadField
            End Get
            Set
                Me.oprFixedOverHeadField = value
            End Set
        End Property
        
        Public Property OprVariableOverHead() As String
            Get
                Return Me.oprVariableOverHeadField
            End Get
            Set
                Me.oprVariableOverHeadField = value
            End Set
        End Property
        
        Public Property OperationCostTotal() As String
            Get
                Return Me.operationCostTotalField
            End Get
            Set
                Me.operationCostTotalField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryBomStructureDetailsComponent
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private componentField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private descriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private longDescField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private partCategoryField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private alternateUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private otherUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ebqField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private panSizeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private comVersionField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private comReleaseField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private sequenceNumField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private structureOnDateField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private structureOffDateField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private opOffsetFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationOffsetField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyPerField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private scrapPercentageField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private scrapQuantityField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private soOptionFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private soPrintFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private inclScrapFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reasonForChangeField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private autoNarrCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private inclKitIssuesField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private createSubJobField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private wetWeightPercentField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeBatchField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeFromJobField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeToJobField As Object
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private comMaterialField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private comSubContractCostField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private comLabourField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private comFixedOverHeadField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private comVariableOverHeadField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private componentCostTotalField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private componentsAttachedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private operationsAttachedField As String
        
        Public Property Component() As String
            Get
                Return Me.componentField
            End Get
            Set
                Me.componentField = value
            End Set
        End Property
        
        Public Property Description() As String
            Get
                Return Me.descriptionField
            End Get
            Set
                Me.descriptionField = value
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
        
        Public Property PartCategory() As String
            Get
                Return Me.partCategoryField
            End Get
            Set
                Me.partCategoryField = value
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
        
        Public Property AlternateUom() As String
            Get
                Return Me.alternateUomField
            End Get
            Set
                Me.alternateUomField = value
            End Set
        End Property
        
        Public Property OtherUom() As String
            Get
                Return Me.otherUomField
            End Get
            Set
                Me.otherUomField = value
            End Set
        End Property
        
        Public Property Ebq() As String
            Get
                Return Me.ebqField
            End Get
            Set
                Me.ebqField = value
            End Set
        End Property
        
        Public Property PanSize() As String
            Get
                Return Me.panSizeField
            End Get
            Set
                Me.panSizeField = value
            End Set
        End Property
        
        Public Property ComVersion() As Object
            Get
                Return Me.comVersionField
            End Get
            Set
                Me.comVersionField = value
            End Set
        End Property
        
        Public Property ComRelease() As Object
            Get
                Return Me.comReleaseField
            End Get
            Set
                Me.comReleaseField = value
            End Set
        End Property
        
        Public Property SequenceNum() As String
            Get
                Return Me.sequenceNumField
            End Get
            Set
                Me.sequenceNumField = value
            End Set
        End Property
        
        Public Property StructureOnDate() As Object
            Get
                Return Me.structureOnDateField
            End Get
            Set
                Me.structureOnDateField = value
            End Set
        End Property
        
        Public Property StructureOffDate() As Object
            Get
                Return Me.structureOffDateField
            End Get
            Set
                Me.structureOffDateField = value
            End Set
        End Property
        
        Public Property OpOffsetFlag() As String
            Get
                Return Me.opOffsetFlagField
            End Get
            Set
                Me.opOffsetFlagField = value
            End Set
        End Property
        
        Public Property OperationOffset() As String
            Get
                Return Me.operationOffsetField
            End Get
            Set
                Me.operationOffsetField = value
            End Set
        End Property
        
        Public Property QtyPer() As String
            Get
                Return Me.qtyPerField
            End Get
            Set
                Me.qtyPerField = value
            End Set
        End Property
        
        Public Property ScrapPercentage() As String
            Get
                Return Me.scrapPercentageField
            End Get
            Set
                Me.scrapPercentageField = value
            End Set
        End Property
        
        Public Property ScrapQuantity() As String
            Get
                Return Me.scrapQuantityField
            End Get
            Set
                Me.scrapQuantityField = value
            End Set
        End Property
        
        Public Property SoOptionFlag() As String
            Get
                Return Me.soOptionFlagField
            End Get
            Set
                Me.soOptionFlagField = value
            End Set
        End Property
        
        Public Property SoPrintFlag() As String
            Get
                Return Me.soPrintFlagField
            End Get
            Set
                Me.soPrintFlagField = value
            End Set
        End Property
        
        Public Property InclScrapFlag() As String
            Get
                Return Me.inclScrapFlagField
            End Get
            Set
                Me.inclScrapFlagField = value
            End Set
        End Property
        
        Public Property ReasonForChange() As Object
            Get
                Return Me.reasonForChangeField
            End Get
            Set
                Me.reasonForChangeField = value
            End Set
        End Property
        
        Public Property AutoNarrCode() As String
            Get
                Return Me.autoNarrCodeField
            End Get
            Set
                Me.autoNarrCodeField = value
            End Set
        End Property
        
        Public Property InclKitIssues() As String
            Get
                Return Me.inclKitIssuesField
            End Get
            Set
                Me.inclKitIssuesField = value
            End Set
        End Property
        
        Public Property CreateSubJob() As String
            Get
                Return Me.createSubJobField
            End Get
            Set
                Me.createSubJobField = value
            End Set
        End Property
        
        Public Property WetWeightPercent() As String
            Get
                Return Me.wetWeightPercentField
            End Get
            Set
                Me.wetWeightPercentField = value
            End Set
        End Property
        
        Public Property IncludeBatch() As String
            Get
                Return Me.includeBatchField
            End Get
            Set
                Me.includeBatchField = value
            End Set
        End Property
        
        Public Property IncludeFromJob() As Object
            Get
                Return Me.includeFromJobField
            End Get
            Set
                Me.includeFromJobField = value
            End Set
        End Property
        
        Public Property IncludeToJob() As Object
            Get
                Return Me.includeToJobField
            End Get
            Set
                Me.includeToJobField = value
            End Set
        End Property
        
        Public Property ComMaterial() As String
            Get
                Return Me.comMaterialField
            End Get
            Set
                Me.comMaterialField = value
            End Set
        End Property
        
        Public Property ComSubContractCost() As String
            Get
                Return Me.comSubContractCostField
            End Get
            Set
                Me.comSubContractCostField = value
            End Set
        End Property
        
        Public Property ComLabour() As String
            Get
                Return Me.comLabourField
            End Get
            Set
                Me.comLabourField = value
            End Set
        End Property
        
        Public Property ComFixedOverHead() As String
            Get
                Return Me.comFixedOverHeadField
            End Get
            Set
                Me.comFixedOverHeadField = value
            End Set
        End Property
        
        Public Property ComVariableOverHead() As String
            Get
                Return Me.comVariableOverHeadField
            End Get
            Set
                Me.comVariableOverHeadField = value
            End Set
        End Property
        
        Public Property ComponentCostTotal() As String
            Get
                Return Me.componentCostTotalField
            End Get
            Set
                Me.componentCostTotalField = value
            End Set
        End Property
        
        Public Property ComponentsAttached() As String
            Get
                Return Me.componentsAttachedField
            End Get
            Set
                Me.componentsAttachedField = value
            End Set
        End Property
        
        Public Property OperationsAttached() As String
            Get
                Return Me.operationsAttachedField
            End Get
            Set
                Me.operationsAttachedField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQuerySupplierStockCodeDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supplierItemField As InvWhereReferencedQuerySupplierStockCodeDetailsSupplierItem
        
        Public Sub New()
            MyBase.New
            If (Me.supplierItemField Is Nothing) Then
                Me.supplierItemField = New InvWhereReferencedQuerySupplierStockCodeDetailsSupplierItem
            End If
        End Sub
        
        Public Property SupplierItem() As InvWhereReferencedQuerySupplierStockCodeDetailsSupplierItem
            Get
                Return Me.supplierItemField
            End Get
            Set
                Me.supplierItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQuerySupplierStockCodeDetailsSupplierItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supplierField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supCatalogueNumField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private demoLeadTimeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lastPricePaidField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private defaultPrcUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lastPrcUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lastReceiptDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private orderQtyUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private poText1Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private poText2Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private poText3Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private poText4Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockItemFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lastReceiptQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lctMerchPriceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private defaultPriceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private altDocPriceField As String
        
        Public Property Supplier() As String
            Get
                Return Me.supplierField
            End Get
            Set
                Me.supplierField = value
            End Set
        End Property
        
        Public Property SupCatalogueNum() As String
            Get
                Return Me.supCatalogueNumField
            End Get
            Set
                Me.supCatalogueNumField = value
            End Set
        End Property
        
        Public Property DemoLeadTime() As String
            Get
                Return Me.demoLeadTimeField
            End Get
            Set
                Me.demoLeadTimeField = value
            End Set
        End Property
        
        Public Property LastPricePaid() As String
            Get
                Return Me.lastPricePaidField
            End Get
            Set
                Me.lastPricePaidField = value
            End Set
        End Property
        
        Public Property DefaultPrcUom() As String
            Get
                Return Me.defaultPrcUomField
            End Get
            Set
                Me.defaultPrcUomField = value
            End Set
        End Property
        
        Public Property LastPrcUom() As String
            Get
                Return Me.lastPrcUomField
            End Get
            Set
                Me.lastPrcUomField = value
            End Set
        End Property
        
        Public Property LastReceiptDate() As String
            Get
                Return Me.lastReceiptDateField
            End Get
            Set
                Me.lastReceiptDateField = value
            End Set
        End Property
        
        Public Property OrderQtyUom() As String
            Get
                Return Me.orderQtyUomField
            End Get
            Set
                Me.orderQtyUomField = value
            End Set
        End Property
        
        Public Property PoText1() As String
            Get
                Return Me.poText1Field
            End Get
            Set
                Me.poText1Field = value
            End Set
        End Property
        
        Public Property PoText2() As String
            Get
                Return Me.poText2Field
            End Get
            Set
                Me.poText2Field = value
            End Set
        End Property
        
        Public Property PoText3() As String
            Get
                Return Me.poText3Field
            End Get
            Set
                Me.poText3Field = value
            End Set
        End Property
        
        Public Property PoText4() As String
            Get
                Return Me.poText4Field
            End Get
            Set
                Me.poText4Field = value
            End Set
        End Property
        
        Public Property StockItemFlag() As String
            Get
                Return Me.stockItemFlagField
            End Get
            Set
                Me.stockItemFlagField = value
            End Set
        End Property
        
        Public Property LastReceiptQty() As String
            Get
                Return Me.lastReceiptQtyField
            End Get
            Set
                Me.lastReceiptQtyField = value
            End Set
        End Property
        
        Public Property LctMerchPrice() As String
            Get
                Return Me.lctMerchPriceField
            End Get
            Set
                Me.lctMerchPriceField = value
            End Set
        End Property
        
        Public Property DefaultPrice() As String
            Get
                Return Me.defaultPriceField
            End Get
            Set
                Me.defaultPriceField = value
            End Set
        End Property
        
        Public Property AltDocPrice() As String
            Get
                Return Me.altDocPriceField
            End Get
            Set
                Me.altDocPriceField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryLctRoutesDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lctRouteItemField As InvWhereReferencedQueryLctRoutesDetailsLctRouteItem
        
        Public Sub New()
            MyBase.New
            If (Me.lctRouteItemField Is Nothing) Then
                Me.lctRouteItemField = New InvWhereReferencedQueryLctRoutesDetailsLctRouteItem
            End If
        End Sub
        
        Public Property LctRouteItem() As InvWhereReferencedQueryLctRoutesDetailsLctRouteItem
            Get
                Return Me.lctRouteItemField
            End Get
            Set
                Me.lctRouteItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryLctRoutesDetailsLctRouteItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateRequiredField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private routeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private descriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private daysBeforeEtaField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private demurrageDaysField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private daysAfterEtaField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lctMerchPriceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private isStandardField As String
        
        Public Property DateRequired() As String
            Get
                Return Me.dateRequiredField
            End Get
            Set
                Me.dateRequiredField = value
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
        
        Public Property Description() As String
            Get
                Return Me.descriptionField
            End Get
            Set
                Me.descriptionField = value
            End Set
        End Property
        
        Public Property DaysBeforeEta() As String
            Get
                Return Me.daysBeforeEtaField
            End Get
            Set
                Me.daysBeforeEtaField = value
            End Set
        End Property
        
        Public Property DemurrageDays() As String
            Get
                Return Me.demurrageDaysField
            End Get
            Set
                Me.demurrageDaysField = value
            End Set
        End Property
        
        Public Property DaysAfterEta() As String
            Get
                Return Me.daysAfterEtaField
            End Get
            Set
                Me.daysAfterEtaField = value
            End Set
        End Property
        
        Public Property LctMerchPrice() As String
            Get
                Return Me.lctMerchPriceField
            End Get
            Set
                Me.lctMerchPriceField = value
            End Set
        End Property
        
        Public Property IsStandard() As String
            Get
                Return Me.isStandardField
            End Get
            Set
                Me.isStandardField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryBlanketPOrderDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private contractItemField As InvWhereReferencedQueryBlanketPOrderDetailsContractItem
        
        Public Sub New()
            MyBase.New
            If (Me.contractItemField Is Nothing) Then
                Me.contractItemField = New InvWhereReferencedQueryBlanketPOrderDetailsContractItem
            End If
        End Sub
        
        Public Property ContractItem() As InvWhereReferencedQueryBlanketPOrderDetailsContractItem
            Get
                Return Me.contractItemField
            End Get
            Set
                Me.contractItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryBlanketPOrderDetailsContractItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private contractField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lineTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supplierField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mWarehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mOrderUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mMinimumQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mTotalQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mOrderQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mReceivedQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mLastReceiptDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mPriceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mPriceUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mForeignPriceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mProductClassField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mCompleteFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mGlCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mLastGrnField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mTaxCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As String
        
        Public Property Contract() As String
            Get
                Return Me.contractField
            End Get
            Set
                Me.contractField = value
            End Set
        End Property
        
        Public Property Line() As String
            Get
                Return Me.lineField
            End Get
            Set
                Me.lineField = value
            End Set
        End Property
        
        Public Property LineType() As String
            Get
                Return Me.lineTypeField
            End Get
            Set
                Me.lineTypeField = value
            End Set
        End Property
        
        Public Property Supplier() As String
            Get
                Return Me.supplierField
            End Get
            Set
                Me.supplierField = value
            End Set
        End Property
        
        Public Property MWarehouse() As String
            Get
                Return Me.mWarehouseField
            End Get
            Set
                Me.mWarehouseField = value
            End Set
        End Property
        
        Public Property MOrderUom() As String
            Get
                Return Me.mOrderUomField
            End Get
            Set
                Me.mOrderUomField = value
            End Set
        End Property
        
        Public Property MMinimumQty() As String
            Get
                Return Me.mMinimumQtyField
            End Get
            Set
                Me.mMinimumQtyField = value
            End Set
        End Property
        
        Public Property MTotalQty() As String
            Get
                Return Me.mTotalQtyField
            End Get
            Set
                Me.mTotalQtyField = value
            End Set
        End Property
        
        Public Property MOrderQty() As String
            Get
                Return Me.mOrderQtyField
            End Get
            Set
                Me.mOrderQtyField = value
            End Set
        End Property
        
        Public Property MReceivedQty() As String
            Get
                Return Me.mReceivedQtyField
            End Get
            Set
                Me.mReceivedQtyField = value
            End Set
        End Property
        
        Public Property MLastReceiptDate() As String
            Get
                Return Me.mLastReceiptDateField
            End Get
            Set
                Me.mLastReceiptDateField = value
            End Set
        End Property
        
        Public Property MPrice() As String
            Get
                Return Me.mPriceField
            End Get
            Set
                Me.mPriceField = value
            End Set
        End Property
        
        Public Property MPriceUom() As String
            Get
                Return Me.mPriceUomField
            End Get
            Set
                Me.mPriceUomField = value
            End Set
        End Property
        
        Public Property MForeignPrice() As String
            Get
                Return Me.mForeignPriceField
            End Get
            Set
                Me.mForeignPriceField = value
            End Set
        End Property
        
        Public Property MProductClass() As String
            Get
                Return Me.mProductClassField
            End Get
            Set
                Me.mProductClassField = value
            End Set
        End Property
        
        Public Property MCompleteFlag() As String
            Get
                Return Me.mCompleteFlagField
            End Get
            Set
                Me.mCompleteFlagField = value
            End Set
        End Property
        
        Public Property MGlCode() As String
            Get
                Return Me.mGlCodeField
            End Get
            Set
                Me.mGlCodeField = value
            End Set
        End Property
        
        Public Property MLastGrn() As String
            Get
                Return Me.mLastGrnField
            End Get
            Set
                Me.mLastGrnField = value
            End Set
        End Property
        
        Public Property MTaxCode() As String
            Get
                Return Me.mTaxCodeField
            End Get
            Set
                Me.mTaxCodeField = value
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
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryForeignPriceItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currencyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private descriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private foreignPriceField As String
        
        Public Property Currency() As String
            Get
                Return Me.currencyField
            End Get
            Set
                Me.currencyField = value
            End Set
        End Property
        
        Public Property Description() As String
            Get
                Return Me.descriptionField
            End Get
            Set
                Me.descriptionField = value
            End Set
        End Property
        
        Public Property ForeignPrice() As String
            Get
                Return Me.foreignPriceField
            End Get
            Set
                Me.foreignPriceField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryPricingDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private pricingItemField As InvWhereReferencedQueryPricingDetailsPricingItem
        
        Public Sub New()
            MyBase.New
            If (Me.pricingItemField Is Nothing) Then
                Me.pricingItemField = New InvWhereReferencedQueryPricingDetailsPricingItem
            End If
        End Sub
        
        Public Property PricingItem() As InvWhereReferencedQueryPricingDetailsPricingItem
            Get
                Return Me.pricingItemField
            End Get
            Set
                Me.pricingItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryPricingDetailsPricingItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private priceCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private sellingPriceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private priceBasisField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private alternateUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private otherUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stockUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currencyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private commissionCodeField As String
        
        Public Property PriceCode() As String
            Get
                Return Me.priceCodeField
            End Get
            Set
                Me.priceCodeField = value
            End Set
        End Property
        
        Public Property SellingPrice() As String
            Get
                Return Me.sellingPriceField
            End Get
            Set
                Me.sellingPriceField = value
            End Set
        End Property
        
        Public Property PriceBasis() As String
            Get
                Return Me.priceBasisField
            End Get
            Set
                Me.priceBasisField = value
            End Set
        End Property
        
        Public Property AlternateUom() As String
            Get
                Return Me.alternateUomField
            End Get
            Set
                Me.alternateUomField = value
            End Set
        End Property
        
        Public Property OtherUom() As String
            Get
                Return Me.otherUomField
            End Get
            Set
                Me.otherUomField = value
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
        
        Public Property Currency() As String
            Get
                Return Me.currencyField
            End Get
            Set
                Me.currencyField = value
            End Set
        End Property
        
        Public Property CommissionCode() As String
            Get
                Return Me.commissionCodeField
            End Get
            Set
                Me.commissionCodeField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryDispatchNotesDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dispatchNoteItemField As InvWhereReferencedQueryDispatchNotesDetailsDispatchNoteItem
        
        Public Sub New()
            MyBase.New
            If (Me.dispatchNoteItemField Is Nothing) Then
                Me.dispatchNoteItemField = New InvWhereReferencedQueryDispatchNotesDetailsDispatchNoteItem
            End If
        End Sub
        
        Public Property DispatchNoteItem() As InvWhereReferencedQueryDispatchNotesDetailsDispatchNoteItem
            Get
                Return Me.dispatchNoteItemField
            End Get
            Set
                Me.dispatchNoteItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryDispatchNotesDetailsDispatchNoteItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dispatchNoteField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dispatchNoteLineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesOrderField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesOrderLineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dispatchNoteStatusField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mWarehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mBinField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerNameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private shippingInstrsField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private specialInstrsField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerPoNumberField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceCreatedDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private actualDeliveryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private confirmationDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private confirmationLineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private origShipSoUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private origShipStkUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private origBoSoUomField As String
        
        Public Property DispatchNote() As String
            Get
                Return Me.dispatchNoteField
            End Get
            Set
                Me.dispatchNoteField = value
            End Set
        End Property
        
        Public Property DispatchNoteLine() As String
            Get
                Return Me.dispatchNoteLineField
            End Get
            Set
                Me.dispatchNoteLineField = value
            End Set
        End Property
        
        Public Property SalesOrder() As String
            Get
                Return Me.salesOrderField
            End Get
            Set
                Me.salesOrderField = value
            End Set
        End Property
        
        Public Property SalesOrderLine() As String
            Get
                Return Me.salesOrderLineField
            End Get
            Set
                Me.salesOrderLineField = value
            End Set
        End Property
        
        Public Property DispatchNoteStatus() As String
            Get
                Return Me.dispatchNoteStatusField
            End Get
            Set
                Me.dispatchNoteStatusField = value
            End Set
        End Property
        
        Public Property MWarehouse() As String
            Get
                Return Me.mWarehouseField
            End Get
            Set
                Me.mWarehouseField = value
            End Set
        End Property
        
        Public Property TotalValue() As String
            Get
                Return Me.totalValueField
            End Get
            Set
                Me.totalValueField = value
            End Set
        End Property
        
        Public Property MBin() As String
            Get
                Return Me.mBinField
            End Get
            Set
                Me.mBinField = value
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
        
        Public Property CustomerName() As String
            Get
                Return Me.customerNameField
            End Get
            Set
                Me.customerNameField = value
            End Set
        End Property
        
        Public Property ShippingInstrs() As String
            Get
                Return Me.shippingInstrsField
            End Get
            Set
                Me.shippingInstrsField = value
            End Set
        End Property
        
        Public Property SpecialInstrs() As String
            Get
                Return Me.specialInstrsField
            End Get
            Set
                Me.specialInstrsField = value
            End Set
        End Property
        
        Public Property CustomerPoNumber() As String
            Get
                Return Me.customerPoNumberField
            End Get
            Set
                Me.customerPoNumberField = value
            End Set
        End Property
        
        Public Property Invoice() As String
            Get
                Return Me.invoiceField
            End Get
            Set
                Me.invoiceField = value
            End Set
        End Property
        
        Public Property InvoiceCreatedDate() As String
            Get
                Return Me.invoiceCreatedDateField
            End Get
            Set
                Me.invoiceCreatedDateField = value
            End Set
        End Property
        
        Public Property ActualDeliveryDate() As String
            Get
                Return Me.actualDeliveryDateField
            End Get
            Set
                Me.actualDeliveryDateField = value
            End Set
        End Property
        
        Public Property ConfirmationDate() As String
            Get
                Return Me.confirmationDateField
            End Get
            Set
                Me.confirmationDateField = value
            End Set
        End Property
        
        Public Property ConfirmationLine() As String
            Get
                Return Me.confirmationLineField
            End Get
            Set
                Me.confirmationLineField = value
            End Set
        End Property
        
        Public Property OrigShipSoUom() As String
            Get
                Return Me.origShipSoUomField
            End Get
            Set
                Me.origShipSoUomField = value
            End Set
        End Property
        
        Public Property OrigShipStkUom() As String
            Get
                Return Me.origShipStkUomField
            End Get
            Set
                Me.origShipStkUomField = value
            End Set
        End Property
        
        Public Property OrigBoSoUom() As String
            Get
                Return Me.origBoSoUomField
            End Get
            Set
                Me.origBoSoUomField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryCustomerStockCodeDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerItemField As InvWhereReferencedQueryCustomerStockCodeDetailsCustomerItem
        
        Public Sub New()
            MyBase.New
            If (Me.customerItemField Is Nothing) Then
                Me.customerItemField = New InvWhereReferencedQueryCustomerStockCodeDetailsCustomerItem
            End If
        End Sub
        
        Public Property CustomerItem() As InvWhereReferencedQueryCustomerStockCodeDetailsCustomerItem
            Get
                Return Me.customerItemField
            End Get
            Set
                Me.customerItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryCustomerStockCodeDetailsCustomerItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private nameField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private custStockCodeField As String
        
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
        
        Public Property CustStockCode() As String
            Get
                Return Me.custStockCodeField
            End Get
            Set
                Me.custStockCodeField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryReferenceItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private referenceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private sourceWarehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private targetWarehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private gtrQuantityField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyReceivedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private initialValueField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueReceivedSrcField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private unitCostField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueReceivedTgtField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private costUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private productClassField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private branchField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private areaField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesOrderField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesOrderLineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private deliveryNoteField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private invoiceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private transfDocNumField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private transactionDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expectedDueDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private transferCompleteField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As String
        
        Public Property Reference() As String
            Get
                Return Me.referenceField
            End Get
            Set
                Me.referenceField = value
            End Set
        End Property
        
        Public Property SourceWarehouse() As String
            Get
                Return Me.sourceWarehouseField
            End Get
            Set
                Me.sourceWarehouseField = value
            End Set
        End Property
        
        Public Property TargetWarehouse() As String
            Get
                Return Me.targetWarehouseField
            End Get
            Set
                Me.targetWarehouseField = value
            End Set
        End Property
        
        Public Property Line() As String
            Get
                Return Me.lineField
            End Get
            Set
                Me.lineField = value
            End Set
        End Property
        
        Public Property GtrQuantity() As String
            Get
                Return Me.gtrQuantityField
            End Get
            Set
                Me.gtrQuantityField = value
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
        
        Public Property InitialValue() As String
            Get
                Return Me.initialValueField
            End Get
            Set
                Me.initialValueField = value
            End Set
        End Property
        
        Public Property ValueReceivedSrc() As String
            Get
                Return Me.valueReceivedSrcField
            End Get
            Set
                Me.valueReceivedSrcField = value
            End Set
        End Property
        
        Public Property UnitCost() As String
            Get
                Return Me.unitCostField
            End Get
            Set
                Me.unitCostField = value
            End Set
        End Property
        
        Public Property ValueReceivedTgt() As String
            Get
                Return Me.valueReceivedTgtField
            End Get
            Set
                Me.valueReceivedTgtField = value
            End Set
        End Property
        
        Public Property CostUom() As String
            Get
                Return Me.costUomField
            End Get
            Set
                Me.costUomField = value
            End Set
        End Property
        
        Public Property ProductClass() As String
            Get
                Return Me.productClassField
            End Get
            Set
                Me.productClassField = value
            End Set
        End Property
        
        Public Property Branch() As String
            Get
                Return Me.branchField
            End Get
            Set
                Me.branchField = value
            End Set
        End Property
        
        Public Property Area() As String
            Get
                Return Me.areaField
            End Get
            Set
                Me.areaField = value
            End Set
        End Property
        
        Public Property SalesOrder() As String
            Get
                Return Me.salesOrderField
            End Get
            Set
                Me.salesOrderField = value
            End Set
        End Property
        
        Public Property SalesOrderLine() As String
            Get
                Return Me.salesOrderLineField
            End Get
            Set
                Me.salesOrderLineField = value
            End Set
        End Property
        
        Public Property DeliveryNote() As String
            Get
                Return Me.deliveryNoteField
            End Get
            Set
                Me.deliveryNoteField = value
            End Set
        End Property
        
        Public Property Invoice() As String
            Get
                Return Me.invoiceField
            End Get
            Set
                Me.invoiceField = value
            End Set
        End Property
        
        Public Property TransfDocNum() As String
            Get
                Return Me.transfDocNumField
            End Get
            Set
                Me.transfDocNumField = value
            End Set
        End Property
        
        Public Property TransactionDate() As String
            Get
                Return Me.transactionDateField
            End Get
            Set
                Me.transactionDateField = value
            End Set
        End Property
        
        Public Property ExpectedDueDate() As String
            Get
                Return Me.expectedDueDateField
            End Get
            Set
                Me.expectedDueDateField = value
            End Set
        End Property
        
        Public Property TransferComplete() As String
            Get
                Return Me.transferCompleteField
            End Get
            Set
                Me.transferCompleteField = value
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
        
        Public Property Version() As String
            Get
                Return Me.versionField
            End Get
            Set
                Me.versionField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWarehouseItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private warehouseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private descriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOnHandField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private availableQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private defaultBinField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOnOrderField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyInInspectionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private minimumQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private maximumQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOnBackOrderField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyAllocatedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdQtyReceivedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdQtyAdjustedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdQtyIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ytdQtySoldField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private prevYearQtySoldField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyInTransitField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyAllocatedWipField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdQtySoldField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private mtdQtyTrfField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private userField1Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private userField2Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private userField3Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateLastSaleField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateLastStockMoveField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateLastCostChangeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateLastStockCntField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateLastPurchaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateWhAddedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private trfBuyingRuleField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private abcClassField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private trfReplenishWhField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private trfDockToStockField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private trfFixTimePeriodField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private unitCostField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private labourCostField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private materialCostField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private fixedOverheadField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private variableOverheadField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private stdLabCostsBillField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private subContractCostField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesQtyByMonthField As InvWhereReferencedQueryWarehouseItemSalesQtyByMonth
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private binDetailsField As InvWhereReferencedQueryWarehouseItemBinDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serialTrackingDetailsField As List(Of InvWhereReferencedQueryWarehouseItemSerialItem)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lotDetailsField As List(Of InvWhereReferencedQueryWarehouseItemLotItem)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private fifoDetailsField As List(Of InvWhereReferencedQueryWarehouseItemFifoItem)
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private grnDetailsField As InvWhereReferencedQueryWarehouseItemGrnDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private allocationDetailsField As InvWhereReferencedQueryWarehouseItemAllocationDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private buildScheduleDetailsField As List(Of InvWhereReferencedQueryWarehouseItemBuildScheduleItem)
        
        Public Sub New()
            MyBase.New
            If (Me.buildScheduleDetailsField Is Nothing) Then
                Me.buildScheduleDetailsField = New List(Of InvWhereReferencedQueryWarehouseItemBuildScheduleItem)
            End If
            If (Me.allocationDetailsField Is Nothing) Then
                Me.allocationDetailsField = New InvWhereReferencedQueryWarehouseItemAllocationDetails
            End If
            If (Me.grnDetailsField Is Nothing) Then
                Me.grnDetailsField = New InvWhereReferencedQueryWarehouseItemGrnDetails
            End If
            If (Me.fifoDetailsField Is Nothing) Then
                Me.fifoDetailsField = New List(Of InvWhereReferencedQueryWarehouseItemFifoItem)
            End If
            If (Me.lotDetailsField Is Nothing) Then
                Me.lotDetailsField = New List(Of InvWhereReferencedQueryWarehouseItemLotItem)
            End If
            If (Me.serialTrackingDetailsField Is Nothing) Then
                Me.serialTrackingDetailsField = New List(Of InvWhereReferencedQueryWarehouseItemSerialItem)
            End If
            If (Me.binDetailsField Is Nothing) Then
                Me.binDetailsField = New InvWhereReferencedQueryWarehouseItemBinDetails
            End If
            If (Me.salesQtyByMonthField Is Nothing) Then
                Me.salesQtyByMonthField = New InvWhereReferencedQueryWarehouseItemSalesQtyByMonth
            End If
        End Sub
        
        Public Property Warehouse() As String
            Get
                Return Me.warehouseField
            End Get
            Set
                Me.warehouseField = value
            End Set
        End Property
        
        Public Property Description() As String
            Get
                Return Me.descriptionField
            End Get
            Set
                Me.descriptionField = value
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
        
        Public Property AvailableQty() As String
            Get
                Return Me.availableQtyField
            End Get
            Set
                Me.availableQtyField = value
            End Set
        End Property
        
        Public Property DefaultBin() As String
            Get
                Return Me.defaultBinField
            End Get
            Set
                Me.defaultBinField = value
            End Set
        End Property
        
        Public Property QtyOnOrder() As String
            Get
                Return Me.qtyOnOrderField
            End Get
            Set
                Me.qtyOnOrderField = value
            End Set
        End Property
        
        Public Property QtyInInspection() As String
            Get
                Return Me.qtyInInspectionField
            End Get
            Set
                Me.qtyInInspectionField = value
            End Set
        End Property
        
        Public Property MinimumQty() As String
            Get
                Return Me.minimumQtyField
            End Get
            Set
                Me.minimumQtyField = value
            End Set
        End Property
        
        Public Property MaximumQty() As String
            Get
                Return Me.maximumQtyField
            End Get
            Set
                Me.maximumQtyField = value
            End Set
        End Property
        
        Public Property QtyOnBackOrder() As String
            Get
                Return Me.qtyOnBackOrderField
            End Get
            Set
                Me.qtyOnBackOrderField = value
            End Set
        End Property
        
        Public Property QtyAllocated() As String
            Get
                Return Me.qtyAllocatedField
            End Get
            Set
                Me.qtyAllocatedField = value
            End Set
        End Property
        
        Public Property MtdQtyReceived() As String
            Get
                Return Me.mtdQtyReceivedField
            End Get
            Set
                Me.mtdQtyReceivedField = value
            End Set
        End Property
        
        Public Property MtdQtyAdjusted() As String
            Get
                Return Me.mtdQtyAdjustedField
            End Get
            Set
                Me.mtdQtyAdjustedField = value
            End Set
        End Property
        
        Public Property MtdQtyIssued() As String
            Get
                Return Me.mtdQtyIssuedField
            End Get
            Set
                Me.mtdQtyIssuedField = value
            End Set
        End Property
        
        Public Property YtdQtySold() As String
            Get
                Return Me.ytdQtySoldField
            End Get
            Set
                Me.ytdQtySoldField = value
            End Set
        End Property
        
        Public Property PrevYearQtySold() As String
            Get
                Return Me.prevYearQtySoldField
            End Get
            Set
                Me.prevYearQtySoldField = value
            End Set
        End Property
        
        Public Property QtyInTransit() As String
            Get
                Return Me.qtyInTransitField
            End Get
            Set
                Me.qtyInTransitField = value
            End Set
        End Property
        
        Public Property QtyAllocatedWip() As String
            Get
                Return Me.qtyAllocatedWipField
            End Get
            Set
                Me.qtyAllocatedWipField = value
            End Set
        End Property
        
        Public Property MtdQtySold() As String
            Get
                Return Me.mtdQtySoldField
            End Get
            Set
                Me.mtdQtySoldField = value
            End Set
        End Property
        
        Public Property MtdQtyTrf() As String
            Get
                Return Me.mtdQtyTrfField
            End Get
            Set
                Me.mtdQtyTrfField = value
            End Set
        End Property
        
        Public Property UserField1() As String
            Get
                Return Me.userField1Field
            End Get
            Set
                Me.userField1Field = value
            End Set
        End Property
        
        Public Property UserField2() As String
            Get
                Return Me.userField2Field
            End Get
            Set
                Me.userField2Field = value
            End Set
        End Property
        
        Public Property UserField3() As String
            Get
                Return Me.userField3Field
            End Get
            Set
                Me.userField3Field = value
            End Set
        End Property
        
        Public Property DateLastSale() As String
            Get
                Return Me.dateLastSaleField
            End Get
            Set
                Me.dateLastSaleField = value
            End Set
        End Property
        
        Public Property DateLastStockMove() As String
            Get
                Return Me.dateLastStockMoveField
            End Get
            Set
                Me.dateLastStockMoveField = value
            End Set
        End Property
        
        Public Property DateLastCostChange() As String
            Get
                Return Me.dateLastCostChangeField
            End Get
            Set
                Me.dateLastCostChangeField = value
            End Set
        End Property
        
        Public Property DateLastStockCnt() As String
            Get
                Return Me.dateLastStockCntField
            End Get
            Set
                Me.dateLastStockCntField = value
            End Set
        End Property
        
        Public Property DateLastPurchase() As String
            Get
                Return Me.dateLastPurchaseField
            End Get
            Set
                Me.dateLastPurchaseField = value
            End Set
        End Property
        
        Public Property DateWhAdded() As String
            Get
                Return Me.dateWhAddedField
            End Get
            Set
                Me.dateWhAddedField = value
            End Set
        End Property
        
        Public Property TrfBuyingRule() As String
            Get
                Return Me.trfBuyingRuleField
            End Get
            Set
                Me.trfBuyingRuleField = value
            End Set
        End Property
        
        Public Property AbcClass() As String
            Get
                Return Me.abcClassField
            End Get
            Set
                Me.abcClassField = value
            End Set
        End Property
        
        Public Property TrfReplenishWh() As String
            Get
                Return Me.trfReplenishWhField
            End Get
            Set
                Me.trfReplenishWhField = value
            End Set
        End Property
        
        Public Property TrfDockToStock() As String
            Get
                Return Me.trfDockToStockField
            End Get
            Set
                Me.trfDockToStockField = value
            End Set
        End Property
        
        Public Property TrfFixTimePeriod() As String
            Get
                Return Me.trfFixTimePeriodField
            End Get
            Set
                Me.trfFixTimePeriodField = value
            End Set
        End Property
        
        Public Property UnitCost() As String
            Get
                Return Me.unitCostField
            End Get
            Set
                Me.unitCostField = value
            End Set
        End Property
        
        Public Property LabourCost() As String
            Get
                Return Me.labourCostField
            End Get
            Set
                Me.labourCostField = value
            End Set
        End Property
        
        Public Property MaterialCost() As String
            Get
                Return Me.materialCostField
            End Get
            Set
                Me.materialCostField = value
            End Set
        End Property
        
        Public Property FixedOverhead() As String
            Get
                Return Me.fixedOverheadField
            End Get
            Set
                Me.fixedOverheadField = value
            End Set
        End Property
        
        Public Property VariableOverhead() As String
            Get
                Return Me.variableOverheadField
            End Get
            Set
                Me.variableOverheadField = value
            End Set
        End Property
        
        Public Property StdLabCostsBill() As String
            Get
                Return Me.stdLabCostsBillField
            End Get
            Set
                Me.stdLabCostsBillField = value
            End Set
        End Property
        
        Public Property SubContractCost() As String
            Get
                Return Me.subContractCostField
            End Get
            Set
                Me.subContractCostField = value
            End Set
        End Property
        
        Public Property SalesQtyByMonth() As InvWhereReferencedQueryWarehouseItemSalesQtyByMonth
            Get
                Return Me.salesQtyByMonthField
            End Get
            Set
                Me.salesQtyByMonthField = value
            End Set
        End Property
        
        Public Property BinDetails() As InvWhereReferencedQueryWarehouseItemBinDetails
            Get
                Return Me.binDetailsField
            End Get
            Set
                Me.binDetailsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlArrayItemAttribute("SerialItem", IsNullable:=false)>  _
        Public Property SerialTrackingDetailsCollection() As List(Of InvWhereReferencedQueryWarehouseItemSerialItem)
            Get
                Return Me.serialTrackingDetailsField
            End Get
            Set
                Me.serialTrackingDetailsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlArrayItemAttribute("LotItem", IsNullable:=false)>  _
        Public Property LotDetailsCollection() As List(Of InvWhereReferencedQueryWarehouseItemLotItem)
            Get
                Return Me.lotDetailsField
            End Get
            Set
                Me.lotDetailsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlArrayItemAttribute("FifoItem", IsNullable:=false)>  _
        Public Property FifoDetailsCollection() As List(Of InvWhereReferencedQueryWarehouseItemFifoItem)
            Get
                Return Me.fifoDetailsField
            End Get
            Set
                Me.fifoDetailsField = value
            End Set
        End Property
        
        Public Property GrnDetails() As InvWhereReferencedQueryWarehouseItemGrnDetails
            Get
                Return Me.grnDetailsField
            End Get
            Set
                Me.grnDetailsField = value
            End Set
        End Property
        
        Public Property AllocationDetails() As InvWhereReferencedQueryWarehouseItemAllocationDetails
            Get
                Return Me.allocationDetailsField
            End Get
            Set
                Me.allocationDetailsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlArrayItemAttribute("BuildScheduleItem", IsNullable:=false)>  _
        Public Property BuildScheduleDetailsCollection() As List(Of InvWhereReferencedQueryWarehouseItemBuildScheduleItem)
            Get
                Return Me.buildScheduleDetailsField
            End Get
            Set
                Me.buildScheduleDetailsField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWarehouseItemSalesQtyByMonth
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesQty1Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesQty2Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesQty3Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesQty4Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesQty5Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesQty6Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesQty7Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesQty8Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesQty9Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesQty10Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesQty11Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesQty12Field As String
        
        Public Property SalesQty1() As String
            Get
                Return Me.salesQty1Field
            End Get
            Set
                Me.salesQty1Field = value
            End Set
        End Property
        
        Public Property SalesQty2() As String
            Get
                Return Me.salesQty2Field
            End Get
            Set
                Me.salesQty2Field = value
            End Set
        End Property
        
        Public Property SalesQty3() As String
            Get
                Return Me.salesQty3Field
            End Get
            Set
                Me.salesQty3Field = value
            End Set
        End Property
        
        Public Property SalesQty4() As String
            Get
                Return Me.salesQty4Field
            End Get
            Set
                Me.salesQty4Field = value
            End Set
        End Property
        
        Public Property SalesQty5() As String
            Get
                Return Me.salesQty5Field
            End Get
            Set
                Me.salesQty5Field = value
            End Set
        End Property
        
        Public Property SalesQty6() As String
            Get
                Return Me.salesQty6Field
            End Get
            Set
                Me.salesQty6Field = value
            End Set
        End Property
        
        Public Property SalesQty7() As String
            Get
                Return Me.salesQty7Field
            End Get
            Set
                Me.salesQty7Field = value
            End Set
        End Property
        
        Public Property SalesQty8() As String
            Get
                Return Me.salesQty8Field
            End Get
            Set
                Me.salesQty8Field = value
            End Set
        End Property
        
        Public Property SalesQty9() As String
            Get
                Return Me.salesQty9Field
            End Get
            Set
                Me.salesQty9Field = value
            End Set
        End Property
        
        Public Property SalesQty10() As String
            Get
                Return Me.salesQty10Field
            End Get
            Set
                Me.salesQty10Field = value
            End Set
        End Property
        
        Public Property SalesQty11() As String
            Get
                Return Me.salesQty11Field
            End Get
            Set
                Me.salesQty11Field = value
            End Set
        End Property
        
        Public Property SalesQty12() As String
            Get
                Return Me.salesQty12Field
            End Get
            Set
                Me.salesQty12Field = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWarehouseItemBinDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private binItemField As InvWhereReferencedQueryWarehouseItemBinDetailsBinItem
        
        Public Sub New()
            MyBase.New
            If (Me.binItemField Is Nothing) Then
                Me.binItemField = New InvWhereReferencedQueryWarehouseItemBinDetailsBinItem
            End If
        End Sub
        
        Public Property BinItem() As InvWhereReferencedQueryWarehouseItemBinDetailsBinItem
            Get
                Return Me.binItemField
            End Get
            Set
                Me.binItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWarehouseItemBinDetailsBinItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private binField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOnHandField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private availableQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private noteField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private onHoldField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private onHoldReasonField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private soQtyToShipField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lastIssueDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lastReceiptDateField As String
        
        Public Property Bin() As String
            Get
                Return Me.binField
            End Get
            Set
                Me.binField = value
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
        
        Public Property AvailableQty() As String
            Get
                Return Me.availableQtyField
            End Get
            Set
                Me.availableQtyField = value
            End Set
        End Property
        
        Public Property Note() As String
            Get
                Return Me.noteField
            End Get
            Set
                Me.noteField = value
            End Set
        End Property
        
        Public Property OnHold() As String
            Get
                Return Me.onHoldField
            End Get
            Set
                Me.onHoldField = value
            End Set
        End Property
        
        Public Property OnHoldReason() As String
            Get
                Return Me.onHoldReasonField
            End Get
            Set
                Me.onHoldReasonField = value
            End Set
        End Property
        
        Public Property SoQtyToShip() As String
            Get
                Return Me.soQtyToShipField
            End Get
            Set
                Me.soQtyToShipField = value
            End Set
        End Property
        
        Public Property LastIssueDate() As String
            Get
                Return Me.lastIssueDateField
            End Get
            Set
                Me.lastIssueDateField = value
            End Set
        End Property
        
        Public Property LastReceiptDate() As String
            Get
                Return Me.lastReceiptDateField
            End Get
            Set
                Me.lastReceiptDateField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWarehouseItemSerialItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serialField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serialDescriptionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serialMethodField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private locationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lotField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serviceFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private loadFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private serviceDepotFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOnHandField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyAvailableField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyReceivedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyInTransitOutField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expiryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private scrapDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dueDateField As String
        
        Public Property Serial() As String
            Get
                Return Me.serialField
            End Get
            Set
                Me.serialField = value
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
        
        Public Property SerialMethod() As String
            Get
                Return Me.serialMethodField
            End Get
            Set
                Me.serialMethodField = value
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
        
        Public Property Lot() As String
            Get
                Return Me.lotField
            End Get
            Set
                Me.lotField = value
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
        
        Public Property QtyOnHand() As String
            Get
                Return Me.qtyOnHandField
            End Get
            Set
                Me.qtyOnHandField = value
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
        
        Public Property QtyReceived() As String
            Get
                Return Me.qtyReceivedField
            End Get
            Set
                Me.qtyReceivedField = value
            End Set
        End Property
        
        Public Property QtyInTransitOut() As String
            Get
                Return Me.qtyInTransitOutField
            End Get
            Set
                Me.qtyInTransitOutField = value
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
        
        Public Property DueDate() As String
            Get
                Return Me.dueDateField
            End Get
            Set
                Me.dueDateField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWarehouseItemLotItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lotJobField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private binField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOnHandField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private availableQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private noteField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lotHoldFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expiryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private origQtyReceivedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyToShipField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private creationDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lastTrnDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyInTransitField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private drawOfficeNumField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private archiveFilenameField As String
        
        Public Property LotJob() As String
            Get
                Return Me.lotJobField
            End Get
            Set
                Me.lotJobField = value
            End Set
        End Property
        
        Public Property Bin() As String
            Get
                Return Me.binField
            End Get
            Set
                Me.binField = value
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
        
        Public Property QtyOnHand() As String
            Get
                Return Me.qtyOnHandField
            End Get
            Set
                Me.qtyOnHandField = value
            End Set
        End Property
        
        Public Property AvailableQty() As String
            Get
                Return Me.availableQtyField
            End Get
            Set
                Me.availableQtyField = value
            End Set
        End Property
        
        Public Property Note() As String
            Get
                Return Me.noteField
            End Get
            Set
                Me.noteField = value
            End Set
        End Property
        
        Public Property LotHoldFlag() As String
            Get
                Return Me.lotHoldFlagField
            End Get
            Set
                Me.lotHoldFlagField = value
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
        
        Public Property OrigQtyReceived() As String
            Get
                Return Me.origQtyReceivedField
            End Get
            Set
                Me.origQtyReceivedField = value
            End Set
        End Property
        
        Public Property QtyToShip() As String
            Get
                Return Me.qtyToShipField
            End Get
            Set
                Me.qtyToShipField = value
            End Set
        End Property
        
        Public Property CreationDate() As String
            Get
                Return Me.creationDateField
            End Get
            Set
                Me.creationDateField = value
            End Set
        End Property
        
        Public Property LastTrnDate() As String
            Get
                Return Me.lastTrnDateField
            End Get
            Set
                Me.lastTrnDateField = value
            End Set
        End Property
        
        Public Property QtyInTransit() As String
            Get
                Return Me.qtyInTransitField
            End Get
            Set
                Me.qtyInTransitField = value
            End Set
        End Property
        
        Public Property DrawOfficeNum() As String
            Get
                Return Me.drawOfficeNumField
            End Get
            Set
                Me.drawOfficeNumField = value
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
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWarehouseItemFifoItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private bucketField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private unitCostField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOnHandField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lastReceiptDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lastIssueDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private noteField As String
        
        Public Property Bucket() As String
            Get
                Return Me.bucketField
            End Get
            Set
                Me.bucketField = value
            End Set
        End Property
        
        Public Property UnitCost() As String
            Get
                Return Me.unitCostField
            End Get
            Set
                Me.unitCostField = value
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
        
        Public Property LastReceiptDate() As String
            Get
                Return Me.lastReceiptDateField
            End Get
            Set
                Me.lastReceiptDateField = value
            End Set
        End Property
        
        Public Property LastIssueDate() As String
            Get
                Return Me.lastIssueDateField
            End Get
            Set
                Me.lastIssueDateField = value
            End Set
        End Property
        
        Public Property Note() As String
            Get
                Return Me.noteField
            End Get
            Set
                Me.noteField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWarehouseItemGrnDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private grnItemField As InvWhereReferencedQueryWarehouseItemGrnDetailsGrnItem
        
        Public Sub New()
            MyBase.New
            If (Me.grnItemField Is Nothing) Then
                Me.grnItemField = New InvWhereReferencedQueryWarehouseItemGrnDetailsGrnItem
            End If
        End Sub
        
        Public Property GrnItem() As InvWhereReferencedQueryWarehouseItemGrnDetailsGrnItem
            Get
                Return Me.grnItemField
            End Get
            Set
                Me.grnItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWarehouseItemGrnDetailsGrnItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private grnField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lotField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supplierField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private grnReceiptDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private purchaseOrderField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private purchaseOrderLinField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supDelNoteField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private deliveryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private certificateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private inspNarrationField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private expiryDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyAdvisedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyOffSiteField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyInspectedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyCountedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyAcceptedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyScrappedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyRejectedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private countCompletedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private inspectCompletedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private grnOnHoldField As String
        
        Public Property Grn() As String
            Get
                Return Me.grnField
            End Get
            Set
                Me.grnField = value
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
        
        Public Property Supplier() As String
            Get
                Return Me.supplierField
            End Get
            Set
                Me.supplierField = value
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
        
        Public Property GrnReceiptDate() As String
            Get
                Return Me.grnReceiptDateField
            End Get
            Set
                Me.grnReceiptDateField = value
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
        
        Public Property PurchaseOrderLin() As String
            Get
                Return Me.purchaseOrderLinField
            End Get
            Set
                Me.purchaseOrderLinField = value
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
        
        Public Property SupDelNote() As String
            Get
                Return Me.supDelNoteField
            End Get
            Set
                Me.supDelNoteField = value
            End Set
        End Property
        
        Public Property DeliveryDate() As String
            Get
                Return Me.deliveryDateField
            End Get
            Set
                Me.deliveryDateField = value
            End Set
        End Property
        
        Public Property Certificate() As String
            Get
                Return Me.certificateField
            End Get
            Set
                Me.certificateField = value
            End Set
        End Property
        
        Public Property InspNarration() As String
            Get
                Return Me.inspNarrationField
            End Get
            Set
                Me.inspNarrationField = value
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
        
        Public Property QtyAdvised() As String
            Get
                Return Me.qtyAdvisedField
            End Get
            Set
                Me.qtyAdvisedField = value
            End Set
        End Property
        
        Public Property QtyOffSite() As String
            Get
                Return Me.qtyOffSiteField
            End Get
            Set
                Me.qtyOffSiteField = value
            End Set
        End Property
        
        Public Property QtyInspected() As String
            Get
                Return Me.qtyInspectedField
            End Get
            Set
                Me.qtyInspectedField = value
            End Set
        End Property
        
        Public Property QtyCounted() As String
            Get
                Return Me.qtyCountedField
            End Get
            Set
                Me.qtyCountedField = value
            End Set
        End Property
        
        Public Property QtyAccepted() As String
            Get
                Return Me.qtyAcceptedField
            End Get
            Set
                Me.qtyAcceptedField = value
            End Set
        End Property
        
        Public Property QtyScrapped() As String
            Get
                Return Me.qtyScrappedField
            End Get
            Set
                Me.qtyScrappedField = value
            End Set
        End Property
        
        Public Property QtyRejected() As String
            Get
                Return Me.qtyRejectedField
            End Get
            Set
                Me.qtyRejectedField = value
            End Set
        End Property
        
        Public Property CountCompleted() As String
            Get
                Return Me.countCompletedField
            End Get
            Set
                Me.countCompletedField = value
            End Set
        End Property
        
        Public Property InspectCompleted() As String
            Get
                Return Me.inspectCompletedField
            End Get
            Set
                Me.inspectCompletedField = value
            End Set
        End Property
        
        Public Property GrnOnHold() As String
            Get
                Return Me.grnOnHoldField
            End Get
            Set
                Me.grnOnHoldField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWarehouseItemAllocationDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private allocationItemField As InvWhereReferencedQueryWarehouseItemAllocationDetailsAllocationItem
        
        Public Sub New()
            MyBase.New
            If (Me.allocationItemField Is Nothing) Then
                Me.allocationItemField = New InvWhereReferencedQueryWarehouseItemAllocationDetailsAllocationItem
            End If
        End Sub
        
        Public Property AllocationItem() As InvWhereReferencedQueryWarehouseItemAllocationDetailsAllocationItem
            Get
                Return Me.allocationItemField
            End Get
            Set
                Me.allocationItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWarehouseItemAllocationDetailsAllocationItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private unitCostField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private valueBilledField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private unitQtyReqdField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyIssuedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyBilledField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private qtyRequiredField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private completeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateRequiredField As String
        
        Public Property Job() As String
            Get
                Return Me.jobField
            End Get
            Set
                Me.jobField = value
            End Set
        End Property
        
        Public Property Line() As String
            Get
                Return Me.lineField
            End Get
            Set
                Me.lineField = value
            End Set
        End Property
        
        Public Property UnitCost() As String
            Get
                Return Me.unitCostField
            End Get
            Set
                Me.unitCostField = value
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
        
        Public Property ValueBilled() As String
            Get
                Return Me.valueBilledField
            End Get
            Set
                Me.valueBilledField = value
            End Set
        End Property
        
        Public Property UnitQtyReqd() As String
            Get
                Return Me.unitQtyReqdField
            End Get
            Set
                Me.unitQtyReqdField = value
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
        
        Public Property QtyBilled() As String
            Get
                Return Me.qtyBilledField
            End Get
            Set
                Me.qtyBilledField = value
            End Set
        End Property
        
        Public Property QtyRequired() As String
            Get
                Return Me.qtyRequiredField
            End Get
            Set
                Me.qtyRequiredField = value
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
        
        Public Property DateRequired() As String
            Get
                Return Me.dateRequiredField
            End Get
            Set
                Me.dateRequiredField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryWarehouseItemBuildScheduleItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateRequiredField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private versionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private referenceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private outstQtyToMakeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private totalQtyReceivedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private resouceParentField As String
        
        Public Property DateRequired() As String
            Get
                Return Me.dateRequiredField
            End Get
            Set
                Me.dateRequiredField = value
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
        
        Public Property Version() As String
            Get
                Return Me.versionField
            End Get
            Set
                Me.versionField = value
            End Set
        End Property
        
        Public Property Line() As String
            Get
                Return Me.lineField
            End Get
            Set
                Me.lineField = value
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
        
        Public Property OutstQtyToMake() As String
            Get
                Return Me.outstQtyToMakeField
            End Get
            Set
                Me.outstQtyToMakeField = value
            End Set
        End Property
        
        Public Property TotalQtyReceived() As String
            Get
                Return Me.totalQtyReceivedField
            End Get
            Set
                Me.totalQtyReceivedField = value
            End Set
        End Property
        
        Public Property ResouceParent() As String
            Get
                Return Me.resouceParentField
            End Get
            Set
                Me.resouceParentField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryRequisitionDetails
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private requisitionItemField As InvWhereReferencedQueryRequisitionDetailsRequisitionItem
        
        Public Sub New()
            MyBase.New
            If (Me.requisitionItemField Is Nothing) Then
                Me.requisitionItemField = New InvWhereReferencedQueryRequisitionDetailsRequisitionItem
            End If
        End Sub
        
        Public Property RequisitionItem() As InvWhereReferencedQueryRequisitionDetailsRequisitionItem
            Get
                Return Me.requisitionItemField
            End Get
            Set
                Me.requisitionItemField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class InvWhereReferencedQueryRequisitionDetailsRequisitionItem
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private requisitionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private originatorField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private lineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reqnStatusField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private orderQtyField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private orderUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateReqnRaisedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dueDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reasonForChangeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supplierField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private priceField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private priceUomField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private discPct1Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private discPct2Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private discPct3Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private discountCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private supCatalogueNumField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private jobField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private glCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private productClassField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private taxCodeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerPoNumberField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private capexNumField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reqnOnHoldFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private reasonForHoldField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private holdUserField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private confirmedPoNumField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private confirmedPoLineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private datePoConfirmedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private buyerField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private currentHolderField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private holdersRouteNumField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateActionedField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private receiptFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private dateLastReceiptField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ackReceiptFlagField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesOrderField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private salesOrderLineField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private orderDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private hierHeadField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private section1Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private section2Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private section3Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private section4Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private subOperationNumField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private quoteField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private quoteLineNumField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private inspectNonStkRecField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private eccHoldField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private revisionField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private releaseField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private userCodeField As String
        
        Public Property Requisition() As String
            Get
                Return Me.requisitionField
            End Get
            Set
                Me.requisitionField = value
            End Set
        End Property
        
        Public Property Originator() As String
            Get
                Return Me.originatorField
            End Get
            Set
                Me.originatorField = value
            End Set
        End Property
        
        Public Property Line() As String
            Get
                Return Me.lineField
            End Get
            Set
                Me.lineField = value
            End Set
        End Property
        
        Public Property ReqnStatus() As String
            Get
                Return Me.reqnStatusField
            End Get
            Set
                Me.reqnStatusField = value
            End Set
        End Property
        
        Public Property OrderQty() As String
            Get
                Return Me.orderQtyField
            End Get
            Set
                Me.orderQtyField = value
            End Set
        End Property
        
        Public Property OrderUom() As String
            Get
                Return Me.orderUomField
            End Get
            Set
                Me.orderUomField = value
            End Set
        End Property
        
        Public Property DateReqnRaised() As String
            Get
                Return Me.dateReqnRaisedField
            End Get
            Set
                Me.dateReqnRaisedField = value
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
        
        Public Property ReasonForChange() As String
            Get
                Return Me.reasonForChangeField
            End Get
            Set
                Me.reasonForChangeField = value
            End Set
        End Property
        
        Public Property Supplier() As String
            Get
                Return Me.supplierField
            End Get
            Set
                Me.supplierField = value
            End Set
        End Property
        
        Public Property Price() As String
            Get
                Return Me.priceField
            End Get
            Set
                Me.priceField = value
            End Set
        End Property
        
        Public Property PriceUom() As String
            Get
                Return Me.priceUomField
            End Get
            Set
                Me.priceUomField = value
            End Set
        End Property
        
        Public Property DiscPct1() As String
            Get
                Return Me.discPct1Field
            End Get
            Set
                Me.discPct1Field = value
            End Set
        End Property
        
        Public Property DiscPct2() As String
            Get
                Return Me.discPct2Field
            End Get
            Set
                Me.discPct2Field = value
            End Set
        End Property
        
        Public Property DiscPct3() As String
            Get
                Return Me.discPct3Field
            End Get
            Set
                Me.discPct3Field = value
            End Set
        End Property
        
        Public Property DiscountCode() As String
            Get
                Return Me.discountCodeField
            End Get
            Set
                Me.discountCodeField = value
            End Set
        End Property
        
        Public Property SupCatalogueNum() As String
            Get
                Return Me.supCatalogueNumField
            End Get
            Set
                Me.supCatalogueNumField = value
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
        
        Public Property GlCode() As String
            Get
                Return Me.glCodeField
            End Get
            Set
                Me.glCodeField = value
            End Set
        End Property
        
        Public Property ProductClass() As String
            Get
                Return Me.productClassField
            End Get
            Set
                Me.productClassField = value
            End Set
        End Property
        
        Public Property TaxCode() As String
            Get
                Return Me.taxCodeField
            End Get
            Set
                Me.taxCodeField = value
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
        
        Public Property CustomerPoNumber() As String
            Get
                Return Me.customerPoNumberField
            End Get
            Set
                Me.customerPoNumberField = value
            End Set
        End Property
        
        Public Property CapexNum() As String
            Get
                Return Me.capexNumField
            End Get
            Set
                Me.capexNumField = value
            End Set
        End Property
        
        Public Property ReqnOnHoldFlag() As String
            Get
                Return Me.reqnOnHoldFlagField
            End Get
            Set
                Me.reqnOnHoldFlagField = value
            End Set
        End Property
        
        Public Property ReasonForHold() As String
            Get
                Return Me.reasonForHoldField
            End Get
            Set
                Me.reasonForHoldField = value
            End Set
        End Property
        
        Public Property HoldUser() As String
            Get
                Return Me.holdUserField
            End Get
            Set
                Me.holdUserField = value
            End Set
        End Property
        
        Public Property ConfirmedPoNum() As String
            Get
                Return Me.confirmedPoNumField
            End Get
            Set
                Me.confirmedPoNumField = value
            End Set
        End Property
        
        Public Property ConfirmedPoLine() As String
            Get
                Return Me.confirmedPoLineField
            End Get
            Set
                Me.confirmedPoLineField = value
            End Set
        End Property
        
        Public Property DatePoConfirmed() As String
            Get
                Return Me.datePoConfirmedField
            End Get
            Set
                Me.datePoConfirmedField = value
            End Set
        End Property
        
        Public Property Buyer() As String
            Get
                Return Me.buyerField
            End Get
            Set
                Me.buyerField = value
            End Set
        End Property
        
        Public Property CurrentHolder() As String
            Get
                Return Me.currentHolderField
            End Get
            Set
                Me.currentHolderField = value
            End Set
        End Property
        
        Public Property HoldersRouteNum() As String
            Get
                Return Me.holdersRouteNumField
            End Get
            Set
                Me.holdersRouteNumField = value
            End Set
        End Property
        
        Public Property DateActioned() As String
            Get
                Return Me.dateActionedField
            End Get
            Set
                Me.dateActionedField = value
            End Set
        End Property
        
        Public Property ReceiptFlag() As String
            Get
                Return Me.receiptFlagField
            End Get
            Set
                Me.receiptFlagField = value
            End Set
        End Property
        
        Public Property DateLastReceipt() As String
            Get
                Return Me.dateLastReceiptField
            End Get
            Set
                Me.dateLastReceiptField = value
            End Set
        End Property
        
        Public Property AckReceiptFlag() As String
            Get
                Return Me.ackReceiptFlagField
            End Get
            Set
                Me.ackReceiptFlagField = value
            End Set
        End Property
        
        Public Property SalesOrder() As String
            Get
                Return Me.salesOrderField
            End Get
            Set
                Me.salesOrderField = value
            End Set
        End Property
        
        Public Property SalesOrderLine() As String
            Get
                Return Me.salesOrderLineField
            End Get
            Set
                Me.salesOrderLineField = value
            End Set
        End Property
        
        Public Property OrderDate() As String
            Get
                Return Me.orderDateField
            End Get
            Set
                Me.orderDateField = value
            End Set
        End Property
        
        Public Property HierHead() As String
            Get
                Return Me.hierHeadField
            End Get
            Set
                Me.hierHeadField = value
            End Set
        End Property
        
        Public Property Section1() As String
            Get
                Return Me.section1Field
            End Get
            Set
                Me.section1Field = value
            End Set
        End Property
        
        Public Property Section2() As String
            Get
                Return Me.section2Field
            End Get
            Set
                Me.section2Field = value
            End Set
        End Property
        
        Public Property Section3() As String
            Get
                Return Me.section3Field
            End Get
            Set
                Me.section3Field = value
            End Set
        End Property
        
        Public Property Section4() As String
            Get
                Return Me.section4Field
            End Get
            Set
                Me.section4Field = value
            End Set
        End Property
        
        Public Property SubOperationNum() As String
            Get
                Return Me.subOperationNumField
            End Get
            Set
                Me.subOperationNumField = value
            End Set
        End Property
        
        Public Property Quote() As String
            Get
                Return Me.quoteField
            End Get
            Set
                Me.quoteField = value
            End Set
        End Property
        
        Public Property QuoteLineNum() As String
            Get
                Return Me.quoteLineNumField
            End Get
            Set
                Me.quoteLineNumField = value
            End Set
        End Property
        
        Public Property InspectNonStkRec() As String
            Get
                Return Me.inspectNonStkRecField
            End Get
            Set
                Me.inspectNonStkRecField = value
            End Set
        End Property
        
        Public Property EccHold() As String
            Get
                Return Me.eccHoldField
            End Get
            Set
                Me.eccHoldField = value
            End Set
        End Property
        
        Public Property Revision() As String
            Get
                Return Me.revisionField
            End Get
            Set
                Me.revisionField = value
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
        
        Public Property UserCode() As String
            Get
                Return Me.userCodeField
            End Get
            Set
                Me.userCodeField = value
            End Set
        End Property
    End Class
End Namespace
