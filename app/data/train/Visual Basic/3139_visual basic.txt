Public Class Form1

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim Numero As String = ""

        For i As Integer = 5 To 100
            Numero += i.ToString() & ", "
            MessageBox.Show(i)
            i = i + 4
        Next

        TextBox1.Text = Numero.ToString
    End Sub
End Class
