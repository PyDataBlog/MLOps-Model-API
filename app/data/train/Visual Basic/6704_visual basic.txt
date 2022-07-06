Public Class Inicio

    Private Sub btnEmpezar_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnEmpezar.Click
        If RadioButtonO.Checked Then
            TurnoDe = "O"
            Me.Hide()
            tresenraya.Show()
            limpiar()
        ElseIf RadioButtonX.Checked Then
            TurnoDe = "X"
            Me.Hide()
            tresenraya.Show()
            limpiar()
        End If
    End Sub
End Class