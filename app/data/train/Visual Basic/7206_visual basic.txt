Imports MySql.Data.MySqlClient

Public Class Login

    Private Sub btn_cancel_Click(sender As Object, e As EventArgs) Handles btn_cancel.Click
        Me.Close()
    End Sub

    Private Sub btn_login_Click(sender As Object, e As EventArgs) Handles btn_login.Click
        Dim sql As String = "SELECT * FROM t_user WHERE username = '" & txt_username.Text & "' AND password = '" & txt_password.Text & "'"
        Dim userFound As Boolean = False

        Try
            Connect.constring.Open()
            com.Connection = Connect.constring
            com.CommandText = sql
            reader = com.ExecuteReader

            While reader.Read
                If reader.HasRows Then
                    userFound = True
                    Exit While
                End If
            End While
        Catch ex As Exception
            MsgBox(ex.Message)
        Finally
            Connect.constring.Close()
        End Try

        If userFound = True Then
            txt_password.Text = ""
            txt_username.Text = ""
            Me.Hide()
            Main.Show()
        Else
            MsgBox("User not found!")
        End If
    End Sub
End Class
