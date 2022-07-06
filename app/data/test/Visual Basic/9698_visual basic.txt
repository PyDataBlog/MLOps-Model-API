Public Class Frm_AddModCompuerta
    Public DR_Accionista As DataRow
    Public DT_Accionistas As DataTable
    Public BS_RCompuertas As BindingSource
    Private Sub Frm_AddModAccionista_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Me.Txt_NombreCompuerta.DataBindings.Add("Text", BS_RCompuertas, "NOMBRE_COMPUERTA")
        Me.Txt_AlturaCompuerta.DataBindings.Add("Text", BS_RCompuertas, "ALTURA_COMPUERTA")
        Me.Txt_AnchuraCompuerta.DataBindings.Add("Text", BS_RCompuertas, "ANCHO_COMPUERTA")
        Me.Txt_AccionesMinutoCompuerta.DataBindings.Add("Text", BS_RCompuertas, "ACCIONESMINUTO_COMPUERTA")

    End Sub

    Private Sub Btn_Close_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Btn_Close.Click
        Me.Txt_NombreCompuerta.DataBindings.Clear()
        Me.Txt_AlturaCompuerta.DataBindings.Clear()
        Me.Txt_AnchuraCompuerta.DataBindings.Clear()
        Me.Txt_AccionesMinutoCompuerta.DataBindings.Clear()
        Me.Close()
    End Sub

    Private Sub Btn_Aplicar_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Btn_Aplicar.Click
        Try
            If FuncValida() Then
                BS_RCompuertas.EndEdit()
                Me.Txt_NombreCompuerta.DataBindings.Clear()
                Me.Txt_AlturaCompuerta.DataBindings.Clear()
                Me.Txt_AnchuraCompuerta.DataBindings.Clear()
                Me.Txt_AccionesMinutoCompuerta.DataBindings.Clear()
                Me.DialogResult = Windows.Forms.DialogResult.OK
                Me.Close()
            End If
        Catch ex As Exception
            MessageBox.Show(ex.Message, "Error de la aplicacion", MessageBoxButtons.OK, MessageBoxIcon.Error, MessageBoxDefaultButton.Button1)
            Me.DialogResult = Windows.Forms.DialogResult.Abort
            Me.Close()
        End Try

    End Sub

    Private Function FuncValida() As Boolean
        If (BS_RCompuertas.Find("NOMBRE_COMPUERTA", Txt_NombreCompuerta.Text) > 0) Then
            MessageBox.Show("Nombre Compuerta ya existe")
            Txt_NombreCompuerta.Focus()
            Return False
        End If

        If (Txt_NombreCompuerta.Text.Count < 1) Then
            MessageBox.Show("Ingrese Nombre Compuerta")
            Txt_NombreCompuerta.Focus()
            Return False
        End If

        If (Txt_AlturaCompuerta.Text.Count < 1) Then
            MessageBox.Show("Ingrese Altura Compuerta")
            Txt_AlturaCompuerta.Focus()
            Return False
        End If

        If (Txt_AnchuraCompuerta.Text.Count < 1) Then
            MessageBox.Show("Ingrese Anchura Compuerta")
            Txt_AnchuraCompuerta.Focus()
            Return False
        End If
        Return True
    End Function

    Private Sub Txt_BoxNumeric_TextChanged(sender As System.Object, e As KeyPressEventArgs) Handles Txt_AlturaCompuerta.KeyPress, Txt_AnchuraCompuerta.KeyPress, Txt_AccionesMinutoCompuerta.KeyPress

        If Not Char.IsControl(e.KeyChar) And Not Char.IsDigit(e.KeyChar) And e.KeyChar <> "." Then
            e.Handled = True
        End If

        If (e.KeyChar = ".") And CType(sender, TextBox).Text.IndexOf(".") > -1 Then
            e.Handled = True
        End If
    End Sub
End Class