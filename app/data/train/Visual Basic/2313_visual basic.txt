Imports System.Data.SqlClient
Public Class EditQuestion
    Private question_id As String

    Public Sub New(ByVal question_id As String, ByVal question As String)
        InitializeComponent() ' This call is required by the Windows Form Designer.
        Me.question_id = question_id
        tbQuestion.Text = question
    End Sub

    Private Sub btnSave_Click(sender As Object, e As EventArgs) Handles btnSave.Click
        Dim sqlcon As New SqlConnection With {.ConnectionString = "Server=essql1.walton.uark.edu;Database=isys4283jpucket;Trusted_Connection=yes;"}
        Dim sqlcmd As SqlCommand

        Dim query As String
        query = "UPDATE questions SET [question] = @question WHERE [id] = @question_id;"

        Try
            sqlcmd = New SqlCommand(query, sqlcon)
            sqlcmd.Parameters.AddWithValue("@question", tbQuestion.Text)
            sqlcmd.Parameters.AddWithValue("@question_id", Me.question_id)
            sqlcon.Open()
            sqlcmd.ExecuteNonQuery()
            ' destroy the object
            Me.Dispose()
        Catch ex As Exception
            MsgBox(ex.Message)
            Throw ex
        Finally
            If sqlcon.State = ConnectionState.Open Then
                sqlcon.Close()
            End If
        End Try
    End Sub
End Class