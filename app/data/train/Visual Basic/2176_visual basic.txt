
Public Class FormSistemaHospital

    Private Sub btnAdministrarPersonal_Click(sender As Object, e As EventArgs) Handles btnAdministrarPersonal.Click
        formAdministrarPersonal.Show()

    End Sub

    Private Sub btnAdministrarPacientes_Click(sender As Object, e As EventArgs) Handles btnAdminstrarPacientes.Click
        tb_direccion.Show()
    End Sub

    Private Sub btnAdminitrarDepartamentos_Click(sender As Object, e As EventArgs) Handles btnAdminitrarDepartamentos.Click
        FormAdministrarDepartamentos.Show()
    End Sub

    Private Sub btnAdminstrarcamillas_Click(sender As Object, e As EventArgs) Handles btnAdministrarCamillas.Click
        FormAdministrarCamillas.Show()
    End Sub


    Private Sub btnDiagnostico_Click(sender As Object, e As EventArgs) Handles btnDiagnostico.Click
        FormDiagnostico.Show()

    End Sub

    Private Sub btnFormasPago_Click(sender As Object, e As EventArgs) Handles btnFormasPago.Click
        FormFormadePago.Show()

    End Sub

    Private Sub btnSalir_Click(sender As Object, e As EventArgs) Handles btnSalir.Click
        Me.Close()

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Login.Show()
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        FormContactoAdmin.Show()
    End Sub
End Class
