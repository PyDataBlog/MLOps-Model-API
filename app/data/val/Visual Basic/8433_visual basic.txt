Public Class scrPassBaja
    'Este formulario se ocupará para habilitar los accesos de las diferentes pantallas del menú
    Private Sub btnAcceso_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAcceso.Click
        Dim password As String = "rekor1234" 'Variable para guardar el password de acceso

        'Dentro de un if comparamos los password y ponemos la variable global de acceso en True
        If String.Compare(password, txtPass.Text) = 0 Then
            'Cerramos el formulario
            Me.Close()
            'Llamamos a la pantalla de baja de usuario
            scrBajaUsuarios.Show()
        Else
            MessageBox.Show("El password es incorrecto", "Rekór RFID & WebCam", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End If
    End Sub
End Class