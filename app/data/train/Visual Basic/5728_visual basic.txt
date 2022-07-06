Namespace TransactionObjects.PurchaseOrder
    <Serializable()> Public Class PurchaseOrderStockLine

        Private _PurchaseOrderLine As Integer 'max 4 digits
        Private _LineActionType As CommonEnums.ActionTypeEnum
        Private _StockCode As String 'max 30
        Private _StockDescription As String 'max 30
        Private _Warehouse As String 'max 2
        Private _SupCatalogue As String 'max 30
        Private _OrderQty As Decimal '+- 7.3
        Private _OrderUom As String 'max 3
        Private _Units As Integer 'max 7 digits
        Private _Pieces As Integer 'max 3 digits
        Private _PriceMethod As CommonEnums.POPriceMethodEnum = CommonEnums.POPriceMethodEnum.M_Manual
        Private _SupplierContract As String 'max 6 chars
        Private _Price As Decimal 'format 10.5 positive
        Private _PriceUom As String 'max 3 chars
        Private _LineDiscType As CommonEnums.LineDiscTypeEnum = CommonEnums.LineDiscTypeEnum.Percentage
        Private _LineDiscLessPlus As CommonEnums.DiscountLessPlussEnum = CommonEnums.DiscountLessPlussEnum.Less
        Private _LineDiscPercent1 As Decimal '3.2
        Private _LineDiscPercent2 As Decimal '2.2
        Private _LineDiscPercent3 As Decimal '2.2
        Private _LineDiscValue As Decimal '+- 12.2
        Private _Taxable As CommonEnums.YesNoEnum = CommonEnums.YesNoEnum.Yes
        Private _TaxCode As String 'char 1
        Private _Job As String 'char7
        Private _Version As String 'char3
        Private _Release As String 'char 3
        Private _LatestDueDate As Date
        Private _OriginalDueDate As Date
        Private _RescheduleDueDate As CommonEnums.YesNoEnum = CommonEnums.YesNoEnum.No
        Private _LedgerCode As String  'char(15)
        Private _PasswordForLedgerCode As String 'char(4)
        Private _SubcontractOp As Integer '4 digits
        Private _InspectionReqd As CommonEnums.YesNoEnum = CommonEnums.YesNoEnum.No
        Private _ProductClass As String 'char4
        Private _NonsUnitMass As Decimal '+- 6.6
        Private _NonsUnitVol As Decimal '+- 6.6

#Region " Properties "

        Public Property PurchaseOrderLine() As Integer
            Get
                Return _PurchaseOrderLine
            End Get
            Set(ByVal Value As Integer)
                _PurchaseOrderLine = Value
            End Set
        End Property
        Public Property LineActionType() As CommonEnums.ActionTypeEnum
            Get
                Return _LineActionType
            End Get
            Set(ByVal Value As CommonEnums.ActionTypeEnum)
                _LineActionType = Value
            End Set
        End Property
        Public Property StockCode() As String
            Get
                Return _StockCode
            End Get
            Set(ByVal Value As String)
                _StockCode = Value
            End Set
        End Property
        Public Property StockDescription() As String
            Get
                Return _StockDescription
            End Get
            Set(ByVal Value As String)
                _StockDescription = Value
            End Set
        End Property
        Public Property Warehouse() As String
            Get
                Return _Warehouse
            End Get
            Set(ByVal Value As String)
                _Warehouse = Value
            End Set
        End Property
        Public Property SupCatalogue() As String
            Get
                Return _SupCatalogue
            End Get
            Set(ByVal Value As String)
                _SupCatalogue = Value
            End Set
        End Property
        Public Property OrderQty() As Decimal
            Get
                Return _OrderQty
            End Get
            Set(ByVal Value As Decimal)
                _OrderQty = Value
            End Set
        End Property
        Public Property OrderUom() As String
            Get
                Return _OrderUom
            End Get
            Set(ByVal Value As String)
                _OrderUom = Value
            End Set
        End Property
        Public Property Units() As Integer
            Get
                Return _Units
            End Get
            Set(ByVal Value As Integer)
                _Units = Value
            End Set
        End Property
        Public Property Pieces() As Integer
            Get
                Return _Pieces
            End Get
            Set(ByVal Value As Integer)
                _Pieces = Value
            End Set
        End Property
        Public Property PriceMethod() As CommonEnums.POPriceMethodEnum
            Get
                Return _PriceMethod
            End Get
            Set(ByVal Value As CommonEnums.POPriceMethodEnum)
                _PriceMethod = Value
            End Set
        End Property
        Public Property SupplierContract() As String
            Get
                Return _SupplierContract
            End Get
            Set(ByVal Value As String)
                _SupplierContract = Value
            End Set
        End Property
        Public Property Price() As Decimal
            Get
                Return _Price
            End Get
            Set(ByVal Value As Decimal)
                _Price = Value
            End Set
        End Property
        Public Property PriceUom() As String
            Get
                Return _PriceUom
            End Get
            Set(ByVal Value As String)
                _PriceUom = Value
            End Set
        End Property
        Public Property LineDiscType() As CommonEnums.LineDiscTypeEnum
            Get
                Return _LineDiscType
            End Get
            Set(ByVal Value As CommonEnums.LineDiscTypeEnum)
                _LineDiscType = Value
            End Set
        End Property
        Public Property LineDiscLessPlus() As CommonEnums.DiscountLessPlussEnum
            Get
                Return _LineDiscLessPlus
            End Get
            Set(ByVal Value As CommonEnums.DiscountLessPlussEnum)
                _LineDiscLessPlus = Value
            End Set
        End Property
        Public Property LineDiscPercent1() As Decimal
            Get
                Return _LineDiscPercent1
            End Get
            Set(ByVal Value As Decimal)
                _LineDiscPercent1 = Value
            End Set
        End Property
        Public Property LineDiscPercent2() As Decimal
            Get
                Return _LineDiscPercent2
            End Get
            Set(ByVal Value As Decimal)
                _LineDiscPercent2 = Value
            End Set
        End Property
        Public Property LineDiscPercent3() As Decimal
            Get
                Return _LineDiscPercent3
            End Get
            Set(ByVal Value As Decimal)
                _LineDiscPercent3 = Value
            End Set
        End Property
        Public Property LineDiscValue() As Decimal
            Get
                Return _LineDiscValue
            End Get
            Set(ByVal Value As Decimal)
                _LineDiscValue = Value
            End Set
        End Property
        Public Property Taxable() As CommonEnums.YesNoEnum
            Get
                Return _Taxable
            End Get
            Set(ByVal Value As CommonEnums.YesNoEnum)
                _Taxable = Value
            End Set
        End Property
        Public Property TaxCode() As String
            Get
                Return _TaxCode
            End Get
            Set(ByVal Value As String)
                _TaxCode = Value
            End Set
        End Property
        Public Property Job() As String
            Get
                Return _Job
            End Get
            Set(ByVal Value As String)
                _Job = Value
            End Set
        End Property
        Public Property Version() As String
            Get
                Return _Version
            End Get
            Set(ByVal Value As String)
                _Version = Value
            End Set
        End Property
        Public Property Release() As String
            Get
                Return _Release
            End Get
            Set(ByVal Value As String)
                _Release = Value
            End Set
        End Property
        Public Property LatestDueDate() As Date
            Get
                Return _LatestDueDate
            End Get
            Set(ByVal Value As Date)
                _LatestDueDate = Value
            End Set
        End Property
        Public Property OriginalDueDate() As Date
            Get
                Return _OriginalDueDate
            End Get
            Set(ByVal Value As Date)
                _OriginalDueDate = Value
            End Set
        End Property
        Public Property RescheduleDueDate() As CommonEnums.YesNoEnum
            Get
                Return _RescheduleDueDate
            End Get
            Set(ByVal Value As CommonEnums.YesNoEnum)
                _RescheduleDueDate = Value
            End Set
        End Property
        Public Property LedgerCode() As String
            Get
                Return _LedgerCode
            End Get
            Set(ByVal Value As String)
                _LedgerCode = Value
            End Set
        End Property
        Public Property PasswordForLedgerCode() As String
            Get
                Return _PasswordForLedgerCode
            End Get
            Set(ByVal Value As String)
                _PasswordForLedgerCode = Value
            End Set
        End Property
        Public Property SubcontractOp() As Integer
            Get
                Return _SubcontractOp
            End Get
            Set(ByVal Value As Integer)
                _SubcontractOp = Value
            End Set
        End Property
        Public Property InspectionReqd() As CommonEnums.YesNoEnum
            Get
                Return _InspectionReqd
            End Get
            Set(ByVal Value As CommonEnums.YesNoEnum)
                _InspectionReqd = Value
            End Set
        End Property
        Public Property ProductClass() As String
            Get
                Return _ProductClass
            End Get
            Set(ByVal Value As String)
                _ProductClass = Value
            End Set
        End Property
        Public Property NonsUnitMass() As Decimal
            Get
                Return _NonsUnitMass
            End Get
            Set(ByVal Value As Decimal)
                _NonsUnitMass = Value
            End Set
        End Property
        Public Property NonsUnitVol() As Decimal
            Get
                Return _NonsUnitVol
            End Get
            Set(ByVal Value As Decimal)
                _NonsUnitVol = Value
            End Set
        End Property


#End Region

        Public Sub AppendStockLine(ByRef sbXMLin As System.Text.StringBuilder)
            sbXMLin.Append("<StockLine>")
            sbXMLin.AppendFormat("<PurchaseOrderLine>{0}</PurchaseOrderLine>", PurchaseOrderLine)
            sbXMLin.AppendFormat("<LineActionType>{0}</LineActionType>", Left(LineActionType.ToString, 1))
            sbXMLin.AppendFormat("<StockCode>{0}</StockCode>", StockCode)
            sbXMLin.AppendFormat("<StockDescription>{0}</StockDescription>", StockDescription)
            sbXMLin.AppendFormat("<Warehouse>{0}</Warehouse>", Warehouse)
            sbXMLin.AppendFormat("<SupCatalogue>{0}</SupCatalogue>", SupCatalogue)
            sbXMLin.AppendFormat("<OrderQty>{0}</OrderQty>", OrderQty)
            sbXMLin.AppendFormat("<OrderUom>{0}</OrderUom>", OrderUom)
            sbXMLin.AppendFormat("<Units>{0}</Units>", Units)
            sbXMLin.AppendFormat("<Pieces>{0}</Pieces>", Pieces)
            sbXMLin.AppendFormat("<PriceMethod>{0}</PriceMethod>", Mid(PriceMethod.ToString, 1, PriceMethod.ToString.IndexOf("_") - 1))
            sbXMLin.AppendFormat("<SupplierContract>{0}</SupplierContract>", SupplierContract)
            sbXMLin.AppendFormat("<Price>{0}</Price>", Price)
            sbXMLin.AppendFormat("<PriceUom>{0}</PriceUom>", PriceUom)
            sbXMLin.AppendFormat("<LineDiscType>{0}</LineDiscType>", Left(LineDiscType.ToString, 1))
            sbXMLin.AppendFormat("<LineDiscLessPlus>{0}</LineDiscLessPlus>", Left(LineDiscLessPlus.ToString, 1))
            sbXMLin.AppendFormat("<LineDiscPercent1>{0}</LineDiscPercent1>", LineDiscPercent1)
            sbXMLin.AppendFormat("<LineDiscPercent2>{0}</LineDiscPercent2>", LineDiscPercent2)
            sbXMLin.AppendFormat("<LineDiscPercent3>{0}</LineDiscPercent3>", LineDiscPercent3)
            sbXMLin.AppendFormat("<LineDiscValue>{0}</LineDiscValue>", LineDiscValue)
            sbXMLin.AppendFormat("<Taxable>{0}</Taxable>", Left(Taxable.ToString, 1))
            sbXMLin.AppendFormat("<TaxCode>{0}</TaxCode>", TaxCode)
            sbXMLin.AppendFormat("<Job>{0}</Job>", Job)
            sbXMLin.AppendFormat("<Version>{0}</Version>", Version)
            sbXMLin.AppendFormat("<Release>{0}</Release>", Release)
            If Format(LatestDueDate, "yyyy-MM-dd") = "0001-01-01" Then
                sbXMLin.Append("<LatestDueDate />")
            Else
                sbXMLin.AppendFormat("<LatestDueDate>{0}</LatestDueDate>", Format(LatestDueDate, "yyyy-MM-dd"))
            End If
            If Format(OriginalDueDate, "yyyy-MM-dd") = "0001-01-01" Then
                sbXMLin.Append("<OriginalDueDate />")
            Else
                sbXMLin.AppendFormat("<OriginalDueDate>{0}</OriginalDueDate>", Format(OriginalDueDate, "yyyy-MM-dd"))
            End If
            sbXMLin.AppendFormat("<RescheduleDueDate>{0}</RescheduleDueDate>", Left(RescheduleDueDate.ToString, 1))
            sbXMLin.AppendFormat("<LedgerCode>{0}</LedgerCode>", LedgerCode)
            sbXMLin.AppendFormat("<PasswordForLedgerCode>{0}</PasswordForLedgerCode>", PasswordForLedgerCode)
            sbXMLin.AppendFormat("<SubcontractOp>{0}</SubcontractOp>", SubcontractOp)
            sbXMLin.AppendFormat("<InspectionReqd>{0}</InspectionReqd>", Left(InspectionReqd.ToString, 1))
            sbXMLin.AppendFormat("<ProductClass>{0}</ProductClass>", ProductClass)
            sbXMLin.AppendFormat("<NonsUnitMass>{0}</NonsUnitMass>", NonsUnitMass)
            sbXMLin.AppendFormat("<NonsUnitVol>{0}</NonsUnitVol>", NonsUnitVol)
            sbXMLin.Append("</StockLine>")

        End Sub

    End Class

End Namespace
