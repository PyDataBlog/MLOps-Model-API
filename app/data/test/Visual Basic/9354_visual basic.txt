Public Class ConsultarNotas
    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Me.Hide()
        uno.Show()
    End Sub

    Private Sub Label6_Click(sender As Object, e As EventArgs) Handles Label6.Click
        Me.Hide()
        uno.Show()
    End Sub

    Private Sub buscar_KeyPress(sender As Object, e As KeyPressEventArgs) Handles buscar.KeyPress
        If Char.IsLetter(e.KeyChar) Then
            e.Handled = False
        ElseIf Char.IsControl(e.KeyChar) Then
            e.Handled = False
        ElseIf Char.IsSeparator(e.KeyChar) Then
            e.Handled = False
        Else
            e.Handled = True
            MsgBox("Sólo Letras", MsgBoxStyle.Information)
        End If
    End Sub

    Private Sub Consultar_Click(sender As Object, e As EventArgs) Handles Consultar.Click
        If (bus.Text <> "Seleccione opción") Then
            Select Case bus.Text
                Case "Todas"
                    BDcadena = "SELECT sisaca.asignatura.Nombre, sisaca.nota.Nota1, sisaca.nota.Nota2, sisaca.nota.Nota3,sisaca.nota.Promedio, sisaca.nota.Estado FROM sisaca.nota inner join sisaca.asignatura on nota.asignatura_codigo_asi=asignatura.codigo_asi WHERE nota.estudiante_codigo_est='1';"
                Case Else
                    BDcadena = "SELECT sisaca.asignatura.Nombre, sisaca.nota.Nota1, sisaca.nota.Nota2, sisaca.nota.Nota3,sisaca.nota.Promedio, sisaca.nota.Estado FROM sisaca.nota inner join sisaca.asignatura on nota.asignatura_codigo_asi=asignatura.codigo_asi WHERE (asignatura.Nombre='" + buscar.Text + "') AND (nota.estudiante_codigo_est);"
            End Select
        Else
            MsgBox("Seleccione un método de búsqueda")
        End If
        TablaDgv.DataSource = ObtenerTabla()
    End Sub
End Class