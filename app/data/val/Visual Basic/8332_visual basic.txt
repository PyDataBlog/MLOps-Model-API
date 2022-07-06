Public Class Form1

    Private Sub btnSinister_Click(sender As Object, e As EventArgs) Handles btnSinister.Click
        lblLeft.Show()
    End Sub

    Private Sub btnDexter_Click(sender As Object, e As EventArgs) Handles btnDexter.Click
        lblRight.Show()
    End Sub

    Private Sub btnMedium_Click(sender As Object, e As EventArgs) Handles btnMedium.Click
        lblCenter.Show()
    End Sub

    Private Sub btnExit_Click(sender As Object, e As EventArgs) Handles btnExit.Click
        Me.Close()
    End Sub
End Class
