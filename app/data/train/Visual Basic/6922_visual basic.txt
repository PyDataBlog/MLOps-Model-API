Imports System
Imports System.Runtime.Serialization
Imports System.Collections
Imports System.Xml
Imports System.IO
Namespace TransactionObjects

    <Serializable()> Public Class CancelDispatchNote
        Implements ITransaction

#Region " Params "

#Region "Private Data Members"

        Dim _PostDispCancel As String = String.Empty
        Dim _Parameters As String = String.Empty
        Dim _ApplyIfEntireDocumentValid As CancelDispatchNote_ApplyIfEntireDocumentValid_Enum = CancelDispatchNote_ApplyIfEntireDocumentValid_Enum.ApplyIfEntireDocumentValid_Upper_Y
        Dim _IgnoreWarnings As CancelDispatchNote_IgnoreWarnings_Enum = CancelDispatchNote_IgnoreWarnings_Enum.IgnoreWarnings_Upper_N
        Dim _CancelAttachedComments As CancelDispatchNote_CancelAttachedComments_Enum = CancelDispatchNote_CancelAttachedComments_Enum.CancelAttachedComments_Upper_N


#End Region

#Region "Public Properties"


        ''' <summary> 
        '''  This contains parameters.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property Parameters() As String
            Get
                Return _Parameters
            End Get
            Set(ByVal Value As String)
                _Parameters = Value
            End Set
        End Property

        ''' <summary> 
        '''  This element indicates whether to process entire document only if all items are valid. If set to 'N', each item is validated and rejected if in error or processed if correct. This is the fastest option. If set to 'Y', all items are validated and only processed if entire document is valid. Default is 'Y'. Valid values are 'Y' and 'N'. 
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property ApplyIfEntireDocumentValid() As CancelDispatchNote_ApplyIfEntireDocumentValid_Enum
            Get
                Return _ApplyIfEntireDocumentValid
            End Get
            Set(ByVal Value As CancelDispatchNote_ApplyIfEntireDocumentValid_Enum)
                _ApplyIfEntireDocumentValid = Value
            End Set
        End Property

        ''' <summary> 
        '''  This element indicates whether or not a dispatch must be rejected if there are any warnings such components attached to a parent. If 'W' is supplied, error will be reported as a warning If not supplied it defaults to 'N'. Valid values are 'N', 'W' and 'Y'.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property IgnoreWarnings() As CancelDispatchNote_IgnoreWarnings_Enum
            Get
                Return _IgnoreWarnings
            End Get
            Set(ByVal Value As CancelDispatchNote_IgnoreWarnings_Enum)
                _IgnoreWarnings = Value
            End Set
        End Property

        ''' <summary> 
        '''  This element indicates whether or not to cancel any further comments attached to the selected line. Default is 'N'. Valid values are 'Y' and 'N'.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property CancelAttachedComments() As CancelDispatchNote_CancelAttachedComments_Enum
            Get
                Return _CancelAttachedComments
            End Get
            Set(ByVal Value As CancelDispatchNote_CancelAttachedComments_Enum)
                _CancelAttachedComments = Value
            End Set
        End Property



#End Region

#Region "Enumerations"

        Public Enum CancelDispatchNote_ApplyIfEntireDocumentValid_Enum
            ApplyIfEntireDocumentValid_Upper_Y
            ApplyIfEntireDocumentValid_Upper_N
        End Enum
        Public Enum CancelDispatchNote_IgnoreWarnings_Enum
            IgnoreWarnings_Upper_N
            IgnoreWarnings_Upper_Y
            IgnoreWarnings_Upper_W
        End Enum
        Public Enum CancelDispatchNote_CancelAttachedComments_Enum
            CancelAttachedComments_Upper_N
            CancelAttachedComments_Upper_Y
        End Enum


#End Region

#Region "XML String Constructor"

        Private Function ConstructXMLParam() As String

            Dim xmlDoc As New System.Xml.XmlDocument

            Dim PostDispCancel As System.Xml.XmlElement = xmlDoc.CreateElement("PostDispCancel")
            Dim Parameters As System.Xml.XmlElement = xmlDoc.CreateElement("Parameters")
            Dim ApplyIfEntireDocumentValid As System.Xml.XmlElement = xmlDoc.CreateElement("ApplyIfEntireDocumentValid")
            Dim IgnoreWarnings As System.Xml.XmlElement = xmlDoc.CreateElement("IgnoreWarnings")
            Dim CancelAttachedComments As System.Xml.XmlElement = xmlDoc.CreateElement("CancelAttachedComments")

            xmlDoc.AppendChild(PostDispCancel)
            Common.SysproUtilities.AppendXMLElement(PostDispCancel, Parameters, _Parameters.ToString)
            Common.SysproUtilities.AppendXMLElement(PostDispCancel, ApplyIfEntireDocumentValid, Common.SysproUtilities.GetEnumValue(_ApplyIfEntireDocumentValid.ToString))
            Common.SysproUtilities.AppendXMLElement(PostDispCancel, IgnoreWarnings, Common.SysproUtilities.GetEnumValue(_IgnoreWarnings.ToString))
            Common.SysproUtilities.AppendXMLElement(PostDispCancel, CancelAttachedComments, Common.SysproUtilities.GetEnumValue(_CancelAttachedComments.ToString))

            Return xmlDoc.OuterXml

        End Function

#End Region

#End Region

#Region " Data "

#Region "Private Data Members"

        Dim _Item As String = String.Empty
        Dim _DispatchNote As String = String.Empty
        Dim _DispatchLine As String = String.Empty

#End Region

#Region "Public Properties"

        ''' <summary> 
        '''  The line number of the dispatch note to be cancelled. Set this element to be zero (or blank) if all lines for the dispatch note are to be cancelled.
        ''' </summary> 
        ''' <returns></returns> 
        ''' <remarks></remarks> 
        Public Property DispatchLine() As String
            Get
                Return _DispatchLine
            End Get
            Set(ByVal Value As String)
                _DispatchLine = Value
            End Set
        End Property

        Public Property DispatchNote() As String
            Get
                Return _DispatchNote
            End Get
            Set(ByVal value As String)
                _DispatchNote = value
            End Set
        End Property

#End Region

#Region "Enumerations"



#End Region

#Region "XML String Constructor"

        Private Function ConstructXML() As String

            Dim xmlDoc As New System.Xml.XmlDocument

            Dim Item As System.Xml.XmlElement = xmlDoc.CreateElement("Item")
            Dim PostDispCancel As System.Xml.XmlElement = xmlDoc.CreateElement("PostDispCancel")
            Dim DispatchNote As System.Xml.XmlElement = xmlDoc.CreateElement("DispatchNote")
            Dim DispatchLine As System.Xml.XmlElement = xmlDoc.CreateElement("DispatchLine")

            xmlDoc.AppendChild(Item)
            Common.SysproUtilities.AppendXMLElement(Item, PostDispCancel, _PostDispCancel.ToString)
            Common.SysproUtilities.AppendXMLElement(Item, DispatchNote, _DispatchNote.ToString)
            Common.SysproUtilities.AppendXMLElement(Item, DispatchLine, _DispatchLine.ToString)

            Return xmlDoc.OuterXml

        End Function

#End Region

#End Region

#Region " Interface "

        Private _ActionTypeTrn As CommonEnums.ActionType_Transaction
        Public Property ActionTypeTrn() As CommonEnums.ActionType_Transaction Implements ITransaction.TransactionActionType
            Get
                Return _ActionTypeTrn
            End Get
            Set(ByVal value As CommonEnums.ActionType_Transaction)
                _ActionTypeTrn = value
            End Set
        End Property

        Public ReadOnly Property BusinessObject() As String Implements ITransaction.BusinessObject
            Get
                Return "SORTDC"
            End Get
        End Property

        Public Sub Post() Implements ITransaction.Post
            If ActionTypeTrn = CommonEnums.ActionType_Transaction.Build Then
                _xmlOut = Transaction.Build(Me)
            ElseIf ActionTypeTrn = CommonEnums.ActionType_Transaction.Post Then
                _xmlOut = Transaction.Post(Me)
            End If

        End Sub

        Public ReadOnly Property xmlData() As String Implements ITransaction.xmlData
            Get
                Return ConstructXML()
            End Get
        End Property

        Private _xmlOut As String
        Public ReadOnly Property xmlOut() As String Implements ITransaction.xmlOut
            Get
                Return _xmlOut
            End Get
        End Property

        Public ReadOnly Property xmlParam() As String Implements ITransaction.xmlParam
            Get
                Return ConstructXMLParam()
            End Get
        End Property

#End Region

    End Class


End Namespace
