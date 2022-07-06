Imports System
Imports System.Runtime.Serialization
Imports System.Collections
Imports System.Xml
Imports System.IO
Namespace QueryObjects.DispatchDocumentQuery

    <Serializable()> Public Class DispatchDocumentQuery
        Implements IQuery

#Region "Private Data Members"

        Dim _xQuery As String = String.Empty
        Dim _xOption As String = String.Empty
        Dim _Reprint As DispatchDocumentQuery_Reprint_Enum = DispatchDocumentQuery_Reprint_Enum.Reprint_Upper_N
        Dim _DocumentDate As String = String.Empty
        Dim _DateToCheck As DispatchDocumentQuery_DateToCheck_Enum = DispatchDocumentQuery_DateToCheck_Enum.DateToCheck_Upper_N
        Dim _IncludeSCT As DispatchDocumentQuery_IncludeSCT_Enum = DispatchDocumentQuery_IncludeSCT_Enum.IncludeSCT_Upper_I
        Dim _IncludeForms As DispatchDocumentQuery_IncludeForms_Enum = DispatchDocumentQuery_IncludeForms_Enum.IncludeForms_Upper_N
        Dim _IncludeCustomerForms As DispatchDocumentQuery_IncludeCustomerForms_Enum = DispatchDocumentQuery_IncludeCustomerForms_Enum.IncludeCustomerForms_Upper_N
        Dim _IncludeStockForms As DispatchDocumentQuery_IncludeStockForms_Enum = DispatchDocumentQuery_IncludeStockForms_Enum.IncludeStockForms_Upper_N
        Dim _IncludeDetailForms As DispatchDocumentQuery_IncludeDetailForms_Enum = DispatchDocumentQuery_IncludeDetailForms_Enum.IncludeDetailForms_Upper_N
        Dim _IncludeNotes As DispatchDocumentQuery_IncludeNotes_Enum = DispatchDocumentQuery_IncludeNotes_Enum.IncludeNotes_Upper_N
        Dim _IgnorePrintStatus As DispatchDocumentQuery_IgnorePrintStatus_Enum = DispatchDocumentQuery_IgnorePrintStatus_Enum.IgnorePrintStatus_Upper_N
        Dim _ReprintStatus As DispatchDocumentQuery_ReprintStatus_Enum = DispatchDocumentQuery_ReprintStatus_Enum.ReprintStatus_Upper_D
        Dim _RoundMass As DispatchDocumentQuery_RoundMass_Enum = DispatchDocumentQuery_RoundMass_Enum.RoundMass_Upper_O
        Dim _RoundMassDecs As String = String.Empty
        Dim _RoundVolume As DispatchDocumentQuery_RoundVolume_Enum = DispatchDocumentQuery_RoundVolume_Enum.RoundVolume_Upper_O
        Dim _RoundVolumeDecs As String = String.Empty
        Dim _XslStylesheet As String = String.Empty
        Dim _Branch As String = String.Empty
        Dim _Branch_FilterType As DispatchDocumentQuery_Branch_FilterType_Enum = DispatchDocumentQuery_Branch_FilterType_Enum.Branch_FilterType_Upper_A
        Dim _Branch_FilterValue As String = String.Empty
        Dim _Customer As String = String.Empty
        Dim _Customer_FilterType As DispatchDocumentQuery_Customer_FilterType_Enum = DispatchDocumentQuery_Customer_FilterType_Enum.Customer_FilterType_Upper_A
        Dim _Customer_FilterValue As String = String.Empty
        Dim _CustomerPo As String = String.Empty
        Dim _CustomerPo_FilterType As DispatchDocumentQuery_CustomerPo_FilterType_Enum = DispatchDocumentQuery_CustomerPo_FilterType_Enum.CustomerPo_FilterType_Upper_A
        Dim _CustomerPo_FilterValue As String = String.Empty
        Dim _DispatchNote As String = String.Empty
        Dim _DispatchNote_FilterType As DispatchDocumentQuery_DispatchNote_FilterType_Enum = DispatchDocumentQuery_DispatchNote_FilterType_Enum.DispatchNote_FilterType_Upper_A
        Dim _DispatchNote_FilterValue As String = String.Empty
        Dim _Operator_FilterType As DispatchDocumentQuery_Operator_FilterType_Enum = DispatchDocumentQuery_Operator_FilterType_Enum.Operator_FilterType_Upper_A
        Dim _Operator_FilterValue As String = String.Empty
        Dim _RequestedDate As String = String.Empty
        Dim _RequestedDate_FilterType As DispatchDocumentQuery_RequestedDate_FilterType_Enum = DispatchDocumentQuery_RequestedDate_FilterType_Enum.RequestedDate_FilterType_Upper_A
        Dim _RequestedDate_FilterValue As String = String.Empty
        Dim _DispatchStatus As String = String.Empty
        Dim _DispatchStatus_FilterType As DispatchDocumentQuery_DispatchStatus_FilterType_Enum = DispatchDocumentQuery_DispatchStatus_FilterType_Enum.DispatchStatus_FilterType_Upper_L
        Dim _DispatchStatus_FilterValue As String = String.Empty
        Dim _DispatchInvoice_FilterType As DispatchDocumentQuery_DispatchInvoice_FilterType_Enum = DispatchDocumentQuery_DispatchInvoice_FilterType_Enum.DispatchInvoice_FilterType_Upper_A
        Dim _DispatchInvoice_FilterValue As String = String.Empty


#End Region

#Region "Public Properties"

        ''' <summary> 
        '''  This is the root element
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property xQuery() As String
            Get
                Return _xQuery
            End Get
            Set(ByVal Value As String)
                _xQuery = Value
            End Set
        End Property

        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property xOption() As String
            Get
                Return _xOption
            End Get
            Set(ByVal Value As String)
                _xOption = Value
            End Set
        End Property

        ''' <summary> 
        '''  This indicates if an original document is to be returned, or one which has already been printed or presented as XML. Valid entries are 'N' and 'Y'.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property Reprint() As DispatchDocumentQuery_Reprint_Enum
            Get
                Return _Reprint
            End Get
            Set(ByVal Value As DispatchDocumentQuery_Reprint_Enum)
                _Reprint = Value
            End Set
        End Property

        ''' <summary> 
        '''  This is the date to be output with the document. If provided, it must be of the form CCYY-MM-DD. If not provided, the system date will be used.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property DocumentDate() As String
            Get
                Return _DocumentDate
            End Get
            Set(ByVal Value As String)
                _DocumentDate = Value
            End Set
        End Property

        ''' <summary> 
        '''  The type of date selection required - N - None, L - date of last document, D - dispatch date. Default is 'N'. This is used in conjunction with the filter 'RequestedDate', and determines which date is used for the filter.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property DateToCheck() As DispatchDocumentQuery_DateToCheck_Enum
            Get
                Return _DateToCheck
            End Get
            Set(ByVal Value As DispatchDocumentQuery_DateToCheck_Enum)
                _DateToCheck = Value
            End Set
        End Property

        ''' <summary> 
        '''  This element controls the inclusion of dispatches that are marked as Supply Chain Transfers - valid options are I (Include), E (Exclude) and O (Only process if an SCT). The default is 'I' - include.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property IncludeSCT() As DispatchDocumentQuery_IncludeSCT_Enum
            Get
                Return _IncludeSCT
            End Get
            Set(ByVal Value As DispatchDocumentQuery_IncludeSCT_Enum)
                _IncludeSCT = Value
            End Set
        End Property

        ''' <summary> 
        '''  This element determines whether custom form information attached to the dispatch note should be returned. Valid entries are 'N' - no, and 'Y' - yes. The default is 'N'.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property IncludeForms() As DispatchDocumentQuery_IncludeForms_Enum
            Get
                Return _IncludeForms
            End Get
            Set(ByVal Value As DispatchDocumentQuery_IncludeForms_Enum)
                _IncludeForms = Value
            End Set
        End Property

        ''' <summary> 
        '''  This element determines whether custom form information for the dispatch note customer should be returned. Valid entries are 'N' - no, and 'Y' - yes. The default is 'N'.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property IncludeCustomerForms() As DispatchDocumentQuery_IncludeCustomerForms_Enum
            Get
                Return _IncludeCustomerForms
            End Get
            Set(ByVal Value As DispatchDocumentQuery_IncludeCustomerForms_Enum)
                _IncludeCustomerForms = Value
            End Set
        End Property

        ''' <summary> 
        '''  This element determines whether custom form information for the stock code on a detail stocked merchandise line should be returned. Valid entries are 'N' - no, and 'Y' - yes. The default is 'N'.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property IncludeStockForms() As DispatchDocumentQuery_IncludeStockForms_Enum
            Get
                Return _IncludeStockForms
            End Get
            Set(ByVal Value As DispatchDocumentQuery_IncludeStockForms_Enum)
                _IncludeStockForms = Value
            End Set
        End Property

        ''' <summary> 
        '''  This element determines whether custom form information for the detail merchandise line should be returned. Valid entries are 'N' - no, and 'Y' - yes. The default is 'N'.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property IncludeDetailForms() As DispatchDocumentQuery_IncludeDetailForms_Enum
            Get
                Return _IncludeDetailForms
            End Get
            Set(ByVal Value As DispatchDocumentQuery_IncludeDetailForms_Enum)
                _IncludeDetailForms = Value
            End Set
        End Property

        ''' <summary> 
        '''  This element determines whether notepad information attached to the sales order for the dispatch note should be returned. Valid entries are 'N' - no, and 'Y' - yes. The default is 'N'.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property IncludeNotes() As DispatchDocumentQuery_IncludeNotes_Enum
            Get
                Return _IncludeNotes
            End Get
            Set(ByVal Value As DispatchDocumentQuery_IncludeNotes_Enum)
                _IncludeNotes = Value
            End Set
        End Property

        ''' <summary> 
        '''  If this element is set to 'Y', it will over-ride the contents of the 'Reprint' option. Any dispatch notes selected through other criteria will be returned, regardless of the print status for the document. 
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property IgnorePrintStatus() As DispatchDocumentQuery_IgnorePrintStatus_Enum
            Get
                Return _IgnorePrintStatus
            End Get
            Set(ByVal Value As DispatchDocumentQuery_IgnorePrintStatus_Enum)
                _IgnorePrintStatus = Value
            End Set
        End Property

        ''' <summary> 
        '''  This element specifies whether the status of the dispatches to reprint is to be the default status (status 5 when the SYSPRO option to manually release to invoice is set, or status 5 and 7 when release to invoice is not manual) or can be selected. It can be set to D - default, or S - select status. If not entered, then the default status is used for reprinting. If set to 'S', the contents of the filters 'DispatchStatus' and 'DispatchInvoice' are used to select dispatches. This option is only considered if the option to ignore print status has not been selected, and the reprint option is set to 'Y'
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property ReprintStatus() As DispatchDocumentQuery_ReprintStatus_Enum
            Get
                Return _ReprintStatus
            End Get
            Set(ByVal Value As DispatchDocumentQuery_ReprintStatus_Enum)
                _ReprintStatus = Value
            End Set
        End Property

        ''' <summary> 
        '''  This element controls the rounding of the mass on individual merchandise lines. The rounded figure is added to the total mass for the document. The number of decimals is controlled by the 'RoundMassDecs' option. The rounding is O - 'off', 'U' - up, or 'D' down. 
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property RoundMass() As DispatchDocumentQuery_RoundMass_Enum
            Get
                Return _RoundMass
            End Get
            Set(ByVal Value As DispatchDocumentQuery_RoundMass_Enum)
                _RoundMass = Value
            End Set
        End Property

        ''' <summary> 
        '''  This controls the number of decimals used in the rounding of mass as described in 'RoundMass'. A single digit in the range 0 to 6 must be provided. If this element is not used, then the number of decimal places in the mass (currently 6) is used.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property RoundMassDecs() As String
            Get
                Return _RoundMassDecs
            End Get
            Set(ByVal Value As String)
                _RoundMassDecs = Value
            End Set
        End Property

        ''' <summary> 
        '''  This element controls the rounding of the volume on individual merchandise lines. The rounded figure is added to the total volume for the document. The number of decimals is controlled by the 'RoundVolumeDecs' option. The rounding is O - 'off', 'U' - up, or 'D' down. 
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property RoundVolume() As DispatchDocumentQuery_RoundVolume_Enum
            Get
                Return _RoundVolume
            End Get
            Set(ByVal Value As DispatchDocumentQuery_RoundVolume_Enum)
                _RoundVolume = Value
            End Set
        End Property

        ''' <summary> 
        '''  This controls the number of decimals used in the rounding of volume as described in 'RoundVolume'. A single digit in the range 0 to 6 must be provided. If this element is not used, then the number of decimal places in the volume (currently 6) is used.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property RoundVolumeDecs() As String
            Get
                Return _RoundVolumeDecs
            End Get
            Set(ByVal Value As String)
                _RoundVolumeDecs = Value
            End Set
        End Property

        ''' <summary> 
        '''  The XSL Stylesheet to be used
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property XslStylesheet() As String
            Get
                Return _XslStylesheet
            End Get
            Set(ByVal Value As String)
                _XslStylesheet = Value
            End Set
        End Property

        ''' <summary> 
        '''  Branch to be selected (FilterType of A - All, R - Range or S - Single branch, FilterValue contains a range of branches separated by commas if FilterType is R or a single branch if S).
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property Branch() As String
            Get
                Return _Branch
            End Get
            Set(ByVal Value As String)
                _Branch = Value
            End Set
        End Property

        Public Property Branch_FilterType() As DispatchDocumentQuery_Branch_FilterType_Enum
            Get
                Return _Branch_FilterType
            End Get
            Set(ByVal Value As DispatchDocumentQuery_Branch_FilterType_Enum)
                _Branch_FilterType = Value
            End Set
        End Property

        Public Property Branch_FilterValue() As String
            Get
                Return _Branch_FilterValue
            End Get
            Set(ByVal Value As String)
                _Branch_FilterValue = Value
            End Set
        End Property

        ''' <summary> 
        '''  Customer to be selected (FilterType of A - All, R - Range or L - List of customers, FilterValue contains a range or list of customers separated by commas if FilterType is R or L).
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property Customer() As String
            Get
                Return _Customer
            End Get
            Set(ByVal Value As String)
                _Customer = Value
            End Set
        End Property

        Public Property Customer_FilterType() As DispatchDocumentQuery_Customer_FilterType_Enum
            Get
                Return _Customer_FilterType
            End Get
            Set(ByVal Value As DispatchDocumentQuery_Customer_FilterType_Enum)
                _Customer_FilterType = Value
            End Set
        End Property

        Public Property Customer_FilterValue() As String
            Get
                Return _Customer_FilterValue
            End Get
            Set(ByVal Value As String)
                _Customer_FilterValue = Value
            End Set
        End Property

        Public Property CustomerPo() As String
            Get
                Return _CustomerPo
            End Get
            Set(ByVal Value As String)
                _CustomerPo = Value
            End Set
        End Property

        Public Property CustomerPo_FilterType() As DispatchDocumentQuery_CustomerPo_FilterType_Enum
            Get
                Return _CustomerPo_FilterType
            End Get
            Set(ByVal Value As DispatchDocumentQuery_CustomerPo_FilterType_Enum)
                _CustomerPo_FilterType = Value
            End Set
        End Property


        Public Property CustomerPo_FilterValue() As String
            Get
                Return _CustomerPo_FilterValue
            End Get
            Set(ByVal Value As String)
                _CustomerPo_FilterValue = Value
            End Set
        End Property

        ''' <summary> 
        '''  Dispatch note to be selected (FilterType of A - All, R - Range or L - List of dispatch note numbers, FilterValue contains a range or list of dispatch note numbers separated by commas if FilterType is R or L).
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property DispatchNote() As String
            Get
                Return _DispatchNote
            End Get
            Set(ByVal Value As String)
                _DispatchNote = Value
            End Set
        End Property


        Public Property DispatchNote_FilterType() As DispatchDocumentQuery_DispatchNote_FilterType_Enum
            Get
                Return _DispatchNote_FilterType
            End Get
            Set(ByVal Value As DispatchDocumentQuery_DispatchNote_FilterType_Enum)
                _DispatchNote_FilterType = Value
            End Set
        End Property


        Public Property DispatchNote_FilterValue() As String
            Get
                Return _DispatchNote_FilterValue
            End Get
            Set(ByVal Value As String)
                _DispatchNote_FilterValue = Value
            End Set
        End Property

        ''' <summary> 
        '''  Operator to be selected (FilterType of A - All, S - Single operator, FilterValue contains the selected operator if FilterType is S).
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property Operator_FilterValue() As String
            Get
                Return _Operator_FilterValue
            End Get
            Set(ByVal Value As String)
                _Operator_FilterValue = Value
            End Set
        End Property


        Public Property Operator_FilterType() As DispatchDocumentQuery_Operator_FilterType_Enum
            Get
                Return _Operator_FilterType
            End Get
            Set(ByVal Value As DispatchDocumentQuery_Operator_FilterType_Enum)
                _Operator_FilterType = Value
            End Set
        End Property

        ''' <summary> 
        '''  Date to be selected (FilterType of A - All, R - Range or S - Single date, FilterValue contains a range of dates separated by commas if FilterType is R or a single date if S). The date provided will depend on the contents of the option 'DateToCheck', and may either be the dispatch date or the date last printed. If the option is set to 'N' for no date, then this filter is ignored.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property RequestedDate() As String
            Get
                Return _RequestedDate
            End Get
            Set(ByVal Value As String)
                _RequestedDate = Value
            End Set
        End Property


        Public Property RequestedDate_FilterType() As DispatchDocumentQuery_RequestedDate_FilterType_Enum
            Get
                Return _RequestedDate_FilterType
            End Get
            Set(ByVal Value As DispatchDocumentQuery_RequestedDate_FilterType_Enum)
                _RequestedDate_FilterType = Value
            End Set
        End Property


        Public Property RequestedDate_FilterValue() As String
            Get
                Return _RequestedDate_FilterValue
            End Get
            Set(ByVal Value As String)
                _RequestedDate_FilterValue = Value
            End Set
        End Property

        ''' <summary> 
        '''  This is a list of dispatch note statuses to be used when reprinting dispatch notes. It is only considered if the option 'ReprintStatus' is set to 'S'. FilterType is always L - list, FilterValue contains a list of required statuses separated by commas. Valid entries in the list are 5, 7 and 9.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property DispatchStatus() As String
            Get
                Return _DispatchStatus
            End Get
            Set(ByVal Value As String)
                _DispatchStatus = Value
            End Set
        End Property


        Public Property DispatchStatus_FilterType() As DispatchDocumentQuery_DispatchStatus_FilterType_Enum
            Get
                Return _DispatchStatus_FilterType
            End Get
            Set(ByVal Value As DispatchDocumentQuery_DispatchStatus_FilterType_Enum)
                _DispatchStatus_FilterType = Value
            End Set
        End Property


        Public Property DispatchStatus_FilterValue() As String
            Get
                Return _DispatchStatus_FilterValue
            End Get
            Set(ByVal Value As String)
                _DispatchStatus_FilterValue = Value
            End Set
        End Property

        ''' <summary> 
        '''  This element is only valid if reprinting dispatches, and dispatches of status 9 have been selected. FilterType is A, all or S - single, FilterValue contains a single invoice number if required. If a single invoice number is selected, then the status 9 dispatches will only be returned if they are for this invoice number.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property DispatchInvoice_FilterValue() As String
            Get
                Return _DispatchInvoice_FilterValue
            End Get
            Set(ByVal Value As String)
                _DispatchInvoice_FilterValue = Value
            End Set
        End Property


        Public Property DispatchInvoice_FilterType() As DispatchDocumentQuery_DispatchInvoice_FilterType_Enum
            Get
                Return _DispatchInvoice_FilterType
            End Get
            Set(ByVal Value As DispatchDocumentQuery_DispatchInvoice_FilterType_Enum)
                _DispatchInvoice_FilterType = Value
            End Set
        End Property



#End Region

#Region "Enumerations"

        Public Enum DispatchDocumentQuery_Reprint_Enum
            Reprint_Upper_N
            Reprint_Upper_Y
            Reprint_Upper_
        End Enum
        Public Enum DispatchDocumentQuery_DateToCheck_Enum
            DateToCheck_Upper_N
            DateToCheck_Upper_L
            DateToCheck_Upper_D
        End Enum
        Public Enum DispatchDocumentQuery_IncludeSCT_Enum
            IncludeSCT_Upper_I
            IncludeSCT_Upper_E
            IncludeSCT_Upper_O
            IncludeSCT_Upper_
        End Enum
        Public Enum DispatchDocumentQuery_IncludeForms_Enum
            IncludeForms_Upper_N
            IncludeForms_Upper_Y
            IncludeForms_Upper_
        End Enum
        Public Enum DispatchDocumentQuery_IncludeCustomerForms_Enum
            IncludeCustomerForms_Upper_N
            IncludeCustomerForms_Upper_Y
            IncludeCustomerForms_Upper_
        End Enum
        Public Enum DispatchDocumentQuery_IncludeStockForms_Enum
            IncludeStockForms_Upper_N
            IncludeStockForms_Upper_Y
            IncludeStockForms_Upper_
        End Enum
        Public Enum DispatchDocumentQuery_IncludeDetailForms_Enum
            IncludeDetailForms_Upper_N
            IncludeDetailForms_Upper_Y
            IncludeDetailForms_Upper_
        End Enum
        Public Enum DispatchDocumentQuery_IncludeNotes_Enum
            IncludeNotes_Upper_Y
            IncludeNotes_Upper_N
            IncludeNotes_Upper_
        End Enum
        Public Enum DispatchDocumentQuery_IgnorePrintStatus_Enum
            IgnorePrintStatus_Upper_N
            IgnorePrintStatus_Upper_Y
            IgnorePrintStatus_Upper_
        End Enum
        Public Enum DispatchDocumentQuery_ReprintStatus_Enum
            ReprintStatus_Upper_D
            ReprintStatus_Upper_S
            ReprintStatus_Upper_
        End Enum
        Public Enum DispatchDocumentQuery_RoundMass_Enum
            RoundMass_Upper_O
            RoundMass_Upper_U
            RoundMass_Upper_D
            RoundMass_Upper_
        End Enum
        Public Enum DispatchDocumentQuery_RoundVolume_Enum
            RoundVolume_Upper_O
            RoundVolume_Upper_U
            RoundVolume_Upper_D
            RoundVolume_Upper_
        End Enum
        Public Enum DispatchDocumentQuery_Branch_FilterType_Enum
            Branch_FilterType_Upper_A
            Branch_FilterType_Upper_R
            Branch_FilterType_Upper_S
        End Enum
        Public Enum DispatchDocumentQuery_Customer_FilterType_Enum
            Customer_FilterType_Upper_A
            Customer_FilterType_Upper_R
            Customer_FilterType_Upper_L
        End Enum
        Public Enum DispatchDocumentQuery_CustomerPo_FilterType_Enum
            CustomerPo_FilterType_Upper_A
            CustomerPo_FilterType_Upper_S
        End Enum
        Public Enum DispatchDocumentQuery_DispatchNote_FilterType_Enum
            DispatchNote_FilterType_Upper_A
            DispatchNote_FilterType_Upper_R
            DispatchNote_FilterType_Upper_L
        End Enum
        Public Enum DispatchDocumentQuery_Operator_FilterType_Enum
            Operator_FilterType_Upper_A
            Operator_FilterType_Upper_S
        End Enum
        Public Enum DispatchDocumentQuery_RequestedDate_FilterType_Enum
            RequestedDate_FilterType_Upper_A
            RequestedDate_FilterType_Upper_R
            RequestedDate_FilterType_Upper_S
        End Enum
        Public Enum DispatchDocumentQuery_DispatchStatus_FilterType_Enum
            DispatchStatus_FilterType_Upper_L
        End Enum
        Public Enum DispatchDocumentQuery_DispatchInvoice_FilterType_Enum
            DispatchInvoice_FilterType_Upper_A
            DispatchInvoice_FilterType_Upper_S
        End Enum


#End Region

#Region "XML String Constructor"

        Private Function ConstructXML() As String

            Dim xmlDoc As New System.Xml.XmlDocument

            Dim xQuery As System.Xml.XmlElement = xmlDoc.CreateElement("Query")
            Dim xOption As System.Xml.XmlElement = xmlDoc.CreateElement("Option")
            Dim Reprint As System.Xml.XmlElement = xmlDoc.CreateElement("Reprint")
            Dim DocumentDate As System.Xml.XmlElement = xmlDoc.CreateElement("DocumentDate")
            Dim DateToCheck As System.Xml.XmlElement = xmlDoc.CreateElement("DateToCheck")
            Dim IncludeSCT As System.Xml.XmlElement = xmlDoc.CreateElement("IncludeSCT")
            Dim IncludeForms As System.Xml.XmlElement = xmlDoc.CreateElement("IncludeForms")
            Dim IncludeCustomerForms As System.Xml.XmlElement = xmlDoc.CreateElement("IncludeCustomerForms")
            Dim IncludeStockForms As System.Xml.XmlElement = xmlDoc.CreateElement("IncludeStockForms")
            Dim IncludeDetailForms As System.Xml.XmlElement = xmlDoc.CreateElement("IncludeDetailForms")
            Dim IncludeNotes As System.Xml.XmlElement = xmlDoc.CreateElement("IncludeNotes")
            Dim IgnorePrintStatus As System.Xml.XmlElement = xmlDoc.CreateElement("IgnorePrintStatus")
            Dim ReprintStatus As System.Xml.XmlElement = xmlDoc.CreateElement("ReprintStatus")
            Dim RoundMass As System.Xml.XmlElement = xmlDoc.CreateElement("RoundMass")
            Dim RoundMassDecs As System.Xml.XmlElement = xmlDoc.CreateElement("RoundMassDecs")
            Dim RoundVolume As System.Xml.XmlElement = xmlDoc.CreateElement("RoundVolume")
            Dim RoundVolumeDecs As System.Xml.XmlElement = xmlDoc.CreateElement("RoundVolumeDecs")
            Dim XslStylesheet As System.Xml.XmlElement = xmlDoc.CreateElement("XslStylesheet")
            Dim Filter As System.Xml.XmlElement = xmlDoc.CreateElement("Filter")
            Dim Branch As System.Xml.XmlElement = xmlDoc.CreateElement("Branch")
            Dim Branch_FilterType As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("Branch_FilterType")
            Dim Branch_FilterValue As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("Branch_FilterValue")
            Dim Customer As System.Xml.XmlElement = xmlDoc.CreateElement("Customer")
            Dim Customer_FilterType As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("Customer_FilterType")
            Dim Customer_FilterValue As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("Customer_FilterValue")
            Dim CustomerPo As System.Xml.XmlElement = xmlDoc.CreateElement("CustomerPo")
            Dim CustomerPo_FilterType As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("CustomerPo_FilterType")
            Dim CustomerPo_FilterValue As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("CustomerPo_FilterValue")
            Dim DispatchNote As System.Xml.XmlElement = xmlDoc.CreateElement("DispatchNote")
            Dim DispatchNote_FilterType As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("DispatchNote_FilterType")
            Dim DispatchNote_FilterValue As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("DispatchNote_FilterValue")
            Dim xOperator As System.Xml.XmlElement = xmlDoc.CreateElement("Operator")
            Dim Operator_FilterType As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("Operator_FilterType")
            Dim Operator_FilterValue As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("Operator_FilterValue")
            Dim RequestedDate As System.Xml.XmlElement = xmlDoc.CreateElement("RequestedDate")
            Dim RequestedDate_FilterType As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("RequestedDate_FilterType")
            Dim RequestedDate_FilterValue As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("RequestedDate_FilterValue")
            Dim DispatchStatus As System.Xml.XmlElement = xmlDoc.CreateElement("DispatchStatus")
            Dim DispatchStatus_FilterType As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("DispatchStatus_FilterType")
            Dim DispatchStatus_FilterValue As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("DispatchStatus_FilterValue")
            Dim DispatchInvoice As System.Xml.XmlElement = xmlDoc.CreateElement("DispatchInvoice")
            Dim DispatchInvoice_FilterType As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("DispatchInvoice_FilterType")
            Dim DispatchInvoice_FilterValue As System.Xml.XmlAttribute = xmlDoc.CreateAttribute("DispatchInvoice_FilterValue")

            xmlDoc.AppendChild(xQuery)
            Common.SysproUtilities.AppendXMLElement(xQuery, xOption, "")
            Common.SysproUtilities.AppendXMLElement(xOption, Reprint, Common.SysproUtilities.GetEnumValue(_Reprint.ToString))
            Common.SysproUtilities.AppendXMLElement(xOption, DocumentDate, _DocumentDate.ToString)
            Common.SysproUtilities.AppendXMLElement(xOption, DateToCheck, Common.SysproUtilities.GetEnumValue(_DateToCheck.ToString))
            Common.SysproUtilities.AppendXMLElement(xOption, IncludeSCT, Common.SysproUtilities.GetEnumValue(_IncludeSCT.ToString))
            Common.SysproUtilities.AppendXMLElement(xOption, IncludeForms, Common.SysproUtilities.GetEnumValue(_IncludeForms.ToString))
            Common.SysproUtilities.AppendXMLElement(xOption, IncludeCustomerForms, Common.SysproUtilities.GetEnumValue(_IncludeCustomerForms.ToString))
            Common.SysproUtilities.AppendXMLElement(xOption, IncludeStockForms, Common.SysproUtilities.GetEnumValue(_IncludeStockForms.ToString))
            Common.SysproUtilities.AppendXMLElement(xOption, IncludeDetailForms, Common.SysproUtilities.GetEnumValue(_IncludeDetailForms.ToString))
            Common.SysproUtilities.AppendXMLElement(xOption, IncludeNotes, Common.SysproUtilities.GetEnumValue(_IncludeNotes.ToString))
            Common.SysproUtilities.AppendXMLElement(xOption, IgnorePrintStatus, Common.SysproUtilities.GetEnumValue(_IgnorePrintStatus.ToString))
            Common.SysproUtilities.AppendXMLElement(xOption, ReprintStatus, Common.SysproUtilities.GetEnumValue(_ReprintStatus.ToString))
            Common.SysproUtilities.AppendXMLElement(xOption, RoundMass, Common.SysproUtilities.GetEnumValue(_RoundMass.ToString))
            Common.SysproUtilities.AppendXMLElement(xOption, RoundMassDecs, _RoundMassDecs.ToString)
            Common.SysproUtilities.AppendXMLElement(xOption, RoundVolume, Common.SysproUtilities.GetEnumValue(_RoundVolume.ToString))
            Common.SysproUtilities.AppendXMLElement(xOption, RoundVolumeDecs, _RoundVolumeDecs.ToString)
            Common.SysproUtilities.AppendXMLElement(xOption, XslStylesheet, _XslStylesheet.ToString)
            Common.SysproUtilities.AppendXMLElement(xQuery, Filter, "")
            Common.SysproUtilities.AppendXMLElement(Filter, Branch, _Branch.ToString)
            Common.SysproUtilities.AppendXMLAttribute(Branch, Branch_FilterType, Common.SysproUtilities.GetEnumValue(_Branch_FilterType.ToString))
            Common.SysproUtilities.AppendXMLAttribute(Branch, Branch_FilterValue, _Branch_FilterValue.ToString)
            Common.SysproUtilities.AppendXMLElement(Filter, Customer, _Customer.ToString)
            Common.SysproUtilities.AppendXMLAttribute(Customer, Customer_FilterType, Common.SysproUtilities.GetEnumValue(_Customer_FilterType.ToString))
            Common.SysproUtilities.AppendXMLAttribute(Customer, Customer_FilterValue, _Customer_FilterValue.ToString)
            Common.SysproUtilities.AppendXMLElement(Filter, CustomerPo, _CustomerPo.ToString)
            Common.SysproUtilities.AppendXMLAttribute(CustomerPo, CustomerPo_FilterType, Common.SysproUtilities.GetEnumValue(_CustomerPo_FilterType.ToString))
            Common.SysproUtilities.AppendXMLAttribute(CustomerPo, CustomerPo_FilterValue, _CustomerPo_FilterValue.ToString)
            Common.SysproUtilities.AppendXMLElement(Filter, DispatchNote, _DispatchNote.ToString)
            Common.SysproUtilities.AppendXMLAttribute(DispatchNote, DispatchNote_FilterType, Common.SysproUtilities.GetEnumValue(_DispatchNote_FilterType.ToString))
            Common.SysproUtilities.AppendXMLAttribute(DispatchNote, DispatchNote_FilterValue, _DispatchNote_FilterValue.ToString)
            Common.SysproUtilities.AppendXMLElement(Filter, xOperator, _DispatchNote.ToString)
            Common.SysproUtilities.AppendXMLAttribute(xOperator, Operator_FilterType, Common.SysproUtilities.GetEnumValue(_Operator_FilterType.ToString))
            Common.SysproUtilities.AppendXMLAttribute(xOperator, Operator_FilterValue, Common.SysproUtilities.GetEnumValue(_Operator_FilterValue.ToString))
            Common.SysproUtilities.AppendXMLElement(Filter, RequestedDate, _RequestedDate.ToString)
            Common.SysproUtilities.AppendXMLAttribute(RequestedDate, RequestedDate_FilterType, Common.SysproUtilities.GetEnumValue(_RequestedDate_FilterType.ToString))
            Common.SysproUtilities.AppendXMLAttribute(RequestedDate, RequestedDate_FilterValue, _RequestedDate_FilterValue.ToString)
            Common.SysproUtilities.AppendXMLElement(Filter, DispatchStatus, _DispatchStatus.ToString)
            Common.SysproUtilities.AppendXMLAttribute(DispatchStatus, DispatchStatus_FilterType, Common.SysproUtilities.GetEnumValue(_DispatchStatus_FilterType.ToString))
            Common.SysproUtilities.AppendXMLAttribute(DispatchStatus, DispatchStatus_FilterValue, _DispatchStatus_FilterValue.ToString)
            Common.SysproUtilities.AppendXMLElement(Filter, DispatchInvoice, _DispatchStatus.ToString)
            Common.SysproUtilities.AppendXMLAttribute(DispatchInvoice, DispatchInvoice_FilterType, Common.SysproUtilities.GetEnumValue(_DispatchInvoice_FilterType.ToString))
            Common.SysproUtilities.AppendXMLAttribute(DispatchInvoice, DispatchInvoice_FilterValue, Common.SysproUtilities.GetEnumValue(_DispatchInvoice_FilterValue.ToString))

            Return xmlDoc.OuterXml

        End Function

#End Region

#Region " IQuery Interface "

        Private _actiontype As CommonEnums.ActionType_Query
        Public Property ActionType() As CommonEnums.ActionType_Query Implements IQuery.ActionType
            Get
                Return _actiontype
            End Get
            Set(ByVal value As CommonEnums.ActionType_Query)
                _actiontype = value
            End Set
        End Property

        Public ReadOnly Property BusinessObject() As String Implements IQuery.BusinessObject
            Get
                Return "SORQDD"
            End Get
        End Property

        Public Sub Post() Implements IQuery.Post
            Select Case _actiontype
                Case CommonEnums.ActionType_Query.Browse
                    _xmlOut = Query.Browse(Me)
                Case CommonEnums.ActionType_Query.Fetch
                    _xmlOut = Query.Fetch(Me)
                Case CommonEnums.ActionType_Query.NextKey
                    _xmlOut = Query.NextKey(Me)
                Case CommonEnums.ActionType_Query.PreviousKey
                    _xmlOut = Query.PreviousKey(Me)
                Case CommonEnums.ActionType_Query.Query
                    _xmlOut = Query.Query(Me)
            End Select
        End Sub

        Public ReadOnly Property xmlData() As String Implements IQuery.xmlData
            Get
                Return ConstructXML()
            End Get
        End Property

        Private _xmlOut As String
        Public ReadOnly Property xmlOut() As String Implements IQuery.xmlOut
            Get
                Return _xmlOut
            End Get
        End Property

#End Region

    End Class

End Namespace