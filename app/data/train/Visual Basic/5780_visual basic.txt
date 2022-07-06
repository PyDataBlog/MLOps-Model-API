Public Class frmNumbers

    Private Sub btnDisplay_Click(sender As Object, e As EventArgs) Handles btnDisplay.Click
        'Display the numbers from 1 to 7
        Dim num As Integer = 1
        Do While num <= 7
            lstNumbers.Items.Add(num)
            num += 2
        Loop
    End Sub

    Private Sub btnClear_Click(sender As Object, e As EventArgs) Handles btnClear.Click
        lstNumbers.Items.Clear()
    End Sub
End Class
