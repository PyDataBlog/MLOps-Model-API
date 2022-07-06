Public Class Form_00_HOME

    Private Sub btnRDY_Click(sender As Object, e As EventArgs) Handles btnCarichiRdySeriali.Click
        Form_10_Carichi_Rdy_Seriali.Show()
        Me.Hide()
    End Sub

    Private Sub btnSer_Click(sender As Object, e As EventArgs) Handles btnCarichiSerRdy.Click
        Form_11_Carichi_Seriali_Rdy.Show()
        Me.Hide()
    End Sub

    Private Sub Label3_Click(sender As Object, e As EventArgs)

    End Sub

    Private Sub HOME_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub Label6_Click(sender As Object, e As EventArgs) Handles LabelSetup.Click

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles btnScarichiRdySeriali.Click
        Form_20_Scarichi_Rdy_Seriali.Show()
        Me.Hide()

    End Sub

    Private Sub btnSetup_Click(sender As Object, e As EventArgs) Handles btnSetup.Click
        Form_90_Setup.Show()
        Me.Hide()
    End Sub

    Private Sub btnScarichiSerialiRdy_Click(sender As Object, e As EventArgs) Handles btnScarichiSerialiRdy.Click
        Form_21_Scarichi_Seriali_rdy.Show()
        Me.Hide()
    End Sub
End Class