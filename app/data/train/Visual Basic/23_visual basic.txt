Public Class mysqlSettings
    Private Sub btnSalvar_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnSalvar.Click
        'Guardamos en sus respectivas variables globales
        _varglobal.ip = txtIp.Text
        _varglobal.pass = txtPass.Text
        _varglobal.user = txtUser.Text

        'Cerramos el formulario
        Me.Close()
    End Sub

    Private Sub mysqlSettings_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        'Guardamos los parámetros de conexión dentro de sus respectivas variables
        _varglobal.ip = txtIp.Text
        _varglobal.pass = txtPass.Text
        _varglobal.user = txtUser.Text
    End Sub
End Class