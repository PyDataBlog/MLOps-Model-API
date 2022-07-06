Public Class SysproException
    Inherits Exception

    Private _xmlIn As String
    Private _xmlParam As String
    Private _xmlOut As String


    Public Sub New(ByVal strErrorMessages As String, ByVal strXmlIn As String, ByVal strXmlParam As String, ByVal strXmlOut As String)
        MyBase.New(strErrorMessages)
        _xmlIn = strXmlIn
        _xmlParam = strXmlParam
        _xmlOut = strXmlOut
    End Sub

    Public ReadOnly Property XMLIn() As String
        Get
            Return _xmlIn
        End Get
    End Property

    Public ReadOnly Property XMLParam() As String
        Get
            Return _xmlParam
        End Get
    End Property

    Public ReadOnly Property XMLOut() As String
        Get
            Return _xmlOut
        End Get
    End Property

End Class
