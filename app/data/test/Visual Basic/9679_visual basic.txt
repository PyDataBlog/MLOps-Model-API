Imports System.Data.OleDb
Imports Capa_Entidad
Imports Modelos
Public Class ReporteBL
    Inherits BaseBL
    Private _empresa As Empresa
    Private _action As ListasBL.listFormato
    Public nrmRegistros As Integer

#Region "Cont   ructores "
    Public Sub New()

    End Sub
    Public Sub New(objEmpresa As Empresa)
        empresa = objEmpresa
    End Sub
#End Region
#Region "Propiedades"
    Public Property Action As ListasBL.listFormato
        Get
            Return _action
        End Get
        Set(value As ListasBL.listFormato)
            _action = value
        End Set
    End Property
    Public Property Empresa As Empresa
        Get
            Return _empresa
        End Get
        Set(value As Empresa)
            _empresa = Empresa
        End Set
    End Property
#End Region

    Public Sub CrearFormato(envio() As String)
        Dim formatos As New FormatoDao()
        Dim _desde As Date = Date.Parse(envio(1))
        Dim _hasta As Date = Date.Parse(envio(2))
        envio(1) = _desde.ToString("yyyy-MM-dd")
        envio(2) = _hasta.ToString("yyyy-MM-dd")
        nrmRegistros = formatos.nroRegistros
        Select Case _action
            Case ListasBL.listFormato.Compras
                formatos.Empresa = _empresa
                formatos.CrearFormatoXMl_8_1(envio)
            Case ListasBL.listFormato.Ventas
                formatos.CrearFormatoXMl141DB(envio)
            Case ListasBL.listFormato.Caja

        End Select

    End Sub
    Public Sub CrearFormato(path As String, extension As String, sheetName As String)
        Dim dao As New FormatoDao()
        Dim conStr As String = ""
        Dim Excel03ConString As String =
            "Provider=Microsoft.Jet.OLEDB.4.0;Data Source={0};Extended Properties='Excel 8.0;HDR=YES'"
        Dim Excel07ConString As String =
            "Provider=Microsoft.ACE.OLEDB.12.0;Data Source={0};Extended Properties='Excel 8.0;HDR=YES'"

        Select Case extension
            Case ".xls"
                'Excel 97-03
                conStr = String.Format(Excel03ConString, path)
                Exit Select
            Case ".xlsx"
                'Excel 07
                conStr = String.Format(Excel07ConString, path)
                Exit Select
        End Select
        nrmRegistros = dao.nroRegistros
        dao.CrearFormatoXMl_8_1(conStr, sheetName)
    End Sub
    Public Sub DeleteFile()
        Try
            Select Case _action
                Case ListasBL.listFormato.Compras
                    My.Computer.FileSystem.DeleteFile("C:\SoftTemp\formato8_1.xml")
                Case ListasBL.listFormato.Ventas
                    My.Computer.FileSystem.DeleteFile("C:\SoftTemp\formato141.xml")
                Case ListasBL.listFormato.Caja
            End Select
        Catch ex As Exception

        End Try

    End Sub
End Class
