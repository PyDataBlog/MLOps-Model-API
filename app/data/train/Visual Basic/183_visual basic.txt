Namespace TransactionObjects.PurchaseOrder
    <Serializable()> Public Class PurchaseOrderMiscChargeLine

        Private _PurchaseOrderLine As Integer '4 digits positive
        Private _LineActionType As CommonEnums.ActionTypeEnum '
        Private _MiscChargeValue As Decimal '12.2 numeric
        Private _MiscChargeTax As String 'char1
        Private _MiscTaxable As CommonEnums.YesNoEnum
        Private _MiscChargeFLoc As CommonEnums.POMiscChargeTypeEnum
        Private _MiscDescription As String 'max 45
        Private _MiscCommentCode As String 'max 3 chars

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
        Public Property MiscChargeValue() As Decimal
            Get
                Return _MiscChargeValue
            End Get
            Set(ByVal Value As Decimal)
                _MiscChargeValue = Value
            End Set
        End Property
        Public Property MiscChargeTax() As String
            Get
                Return _MiscChargeTax
            End Get
            Set(ByVal Value As String)
                _MiscChargeTax = Value
            End Set
        End Property
        Public Property MiscTaxable() As CommonEnums.YesNoEnum
            Get
                Return _MiscTaxable
            End Get
            Set(ByVal Value As CommonEnums.YesNoEnum)
                _MiscTaxable = Value
            End Set
        End Property
        Public Property MiscChargeFLoc() As CommonEnums.POMiscChargeTypeEnum
            Get
                Return _MiscChargeFLoc
            End Get
            Set(ByVal Value As CommonEnums.POMiscChargeTypeEnum)
                _MiscChargeFLoc = Value
            End Set
        End Property
        Public Property MiscDescription() As String
            Get
                Return _MiscDescription
            End Get
            Set(ByVal Value As String)
                _MiscDescription = Value
            End Set
        End Property
        Public Property MiscCommentCode() As String
            Get
                Return _MiscCommentCode
            End Get
            Set(ByVal Value As String)
                _MiscCommentCode = Value
            End Set
        End Property


        Public Sub AppendMiscChargeLine(ByRef sbXMLin As System.Text.StringBuilder)
            sbXMLin.Append("<MiscChargeLine>")
            sbXMLin.AppendFormat("<PurchaseOrderLine>{0}</PurchaseOrderLine>", PurchaseOrderLine)
            sbXMLin.AppendFormat("<LineActionType>{0}</LineActionType>", Left(LineActionType.ToString, 1))
            sbXMLin.AppendFormat("<MiscChargeValue>{0}</MiscChargeValue>", MiscChargeValue)
            sbXMLin.AppendFormat("<MiscChargeTax>{0}</MiscChargeTax>", MiscChargeTax)
            sbXMLin.AppendFormat("<MiscTaxable>{0}</MiscTaxable>", Left(MiscTaxable.ToString, 1))
            sbXMLin.AppendFormat("<MiscChargeFLoc>{0}</MiscChargeFLoc>", Left(MiscChargeFLoc.ToString, 1))
            sbXMLin.AppendFormat("<MiscDescription>{0}</MiscDescription>", MiscDescription)
            sbXMLin.AppendFormat("<MiscCommentCode>{0}</MiscCommentCode>", MiscCommentCode)
            sbXMLin.Append("</MiscChargeLine>")

        End Sub


    End Class
End Namespace
