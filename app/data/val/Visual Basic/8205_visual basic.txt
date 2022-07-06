Imports MySql.Data.MySqlClient
Public Class EjercicioDao
    Inherits BaseDao

    Private conexionValue As MySqlConnection
    Dim sql As String = ""
    Dim cmd As New MySqlCommand()
    Dim laf As Integer = 0
    Dim tbl As String
    Public Function ExistsTablaBy(tabla As String, ByRef men As String) As Boolean
        conexionValue = Me.conexion
        sql = "select count(*) as existe from information_schema.tables " &
                "where table_schema ='bdviso' " &
                "and table_name =@tabla "
        laf = 0
        'If conexionValue.State = ConnectionState.Closed Then
        '    conexionValue.Open()
        'End If

        With cmd
            .Connection = conexionValue
            .CommandText = sql
            .Parameters.Clear()
            .Parameters.AddWithValue("@tabla", tabla)
        End With
        Try
            laf = cmd.ExecuteScalar
            If laf = 0 Then
                Return False
            Else
                Return True
            End If
        Catch ex As Exception
            men = ex.Message
            Return False
        Finally
            conexionValue.Close()
        End Try
    End Function
End Class
