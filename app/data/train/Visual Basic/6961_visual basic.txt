Public Class Form1

    Private Sub btnCalcularIMC_Click(sender As Object, e As EventArgs) Handles btnCalcularIMC.Click
        Dim Imc As Double
        Imc = Format(Val(txtPeso.Text) / (Val(txtAltura.Text) * Val(txtAltura.Text)), "#0.0")
        lblIMC.Text = Imc
        If Imc >= 18.5 And Imc <= 24.9 Then
            lblRiesgo.Text = "Promedio"
        ElseIf Imc >= 25 And Imc <= 29.9 Then
            lblRiesgo.Text = "Aumentado"
        ElseIf Imc >= 30 And Imc <= 34.9 Then
            lblRiesgo.Text = "Moderado"
        ElseIf Imc >= 35 And Imc <= 39.9 Then
            lblRiesgo.Text = "Severo"
        ElseIf Imc >= 40 Then
            lblRiesgo.Text = "Muy Severo"
        End If
    End Sub
End Class
