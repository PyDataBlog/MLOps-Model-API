Imports COMMON
Imports Dapper
Imports System.Data.SqlClient

Public Class SaleDAO
    Implements ISale

    Public Sub InsertInvoice(Entity As Sale) Implements ISale.InsertInvoice
        Throw New NotImplementedException()
    End Sub

    Public Sub UpdateInvoice(Entity As Sale) Implements ISale.UpdateInvoice
        Throw New NotImplementedException()
    End Sub

    Public Function SelectInvoice() As List(Of Sale) Implements ISale.SelectInvoice
        Using conn As SqlConnection = New SqlConnection(My.Settings.SqlConn)
            Const strQuery As String = "SELECT * FROM tslsh"
            conn.Open()

            Return conn.Query(Of Sale)(strQuery)
        End Using
    End Function

    Public Function SelectInvoiceBySalesOrg(salesorg As String) As Sale Implements ISale.SelectInvoiceBySalesOrg
        Using conn As SqlConnection = New SqlConnection(My.Settings.SqlConn)
            Const strQuery As String = "SELECT * FROM tslsh WHERE hs_salesorg=@SalesOrg"
            conn.Open()

            Return conn.Query(Of Sale)(strQuery, New With {.SalesOrg = salesorg}).FirstOrDefault()

        End Using
    End Function
End Class
