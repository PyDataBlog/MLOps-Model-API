Public Class frmServicio

    Private Sub btnCancelar_Click(sender As System.Object, e As System.EventArgs)
        cboVaca.Text = ""
        cboToro.Text = ""
        cboDuracion.Text = ""
        cboFecha.Text = ""
        radNatural.Checked = False
        radInseminacion.Checked = False
    End Sub

    Private Sub btnCerrar_Click(sender As System.Object, e As System.EventArgs) Handles btnCerrar.Click
        Me.Close()
    End Sub

    Private Sub ButtonX1_Click(sender As System.Object, e As System.EventArgs) Handles btnEliminar.Click
        cboVaca.Text = ""
        cboToro.Text = ""
        cboDuracion.Text = ""
        cboFecha.Text = ""
        radNatural.Checked = False
        radInseminacion.Checked = False
    End Sub

End Class