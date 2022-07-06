Imports System.IO
Imports System.Text

Public Class fm_enterPw
    Dim cred As String
    Dim pw_byte As String
    Private Sub btn_enterPw_OK_Click(sender As Object, e As EventArgs) Handles btn_enterPw_OK.Click
        Try
            Dim pw As String = tb_enterPw.Text

            If pw = My.Settings.Password Then
                fm_blockSites.Show()
                Me.Close()
            Else
                MsgBox("Incorrect Password!", vbInformation)
            End If
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try


    End Sub

    Private Sub tb_enterPw_KeyDown(sender As Object, e As KeyEventArgs) Handles tb_enterPw.KeyDown
        If e.KeyCode = Keys.Enter Then
            btn_enterPw_OK.PerformClick()
        End If
    End Sub


End Class