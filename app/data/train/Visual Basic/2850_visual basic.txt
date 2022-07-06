Public Class FireNote
    Public isTopMost = False

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If Desktop.appFireNoteShowClearMsg = True Then
            FireNoteClear.Show()
        Else
            TextBox1.Clear()
        End If
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        If Desktop.appFireNoteShowCloseMsg = True Then
            FireNoteClose.Show()
        Else
            Close()
        End If
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        If isTopMost = True Then
            TopMost = False
            isTopMost = False
            Button4.Text = "Enable TopMost"
        Else
            TopMost = True
            isTopMost = True
            Button4.Text = "Disable TopMost"
        End If
    End Sub

    Private Sub FireNote_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        TopMost = False
        isTopMost = False
    End Sub

    Private Sub Button3_Click_1(sender As Object, e As EventArgs) Handles Button3.Click
        SaveFileDialog1.ShowDialog()
    End Sub

    Private Sub SaveFileDialog1_FileOk(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles SaveFileDialog1.FileOk
        Dim writer As New IO.StreamWriter(SaveFileDialog1.FileName)
        writer.Write(TextBox1.Text)
        writer.Close()
    End Sub
End Class