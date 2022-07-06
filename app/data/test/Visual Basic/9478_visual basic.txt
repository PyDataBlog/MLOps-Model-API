Public Class frmLogin

    Public Sub limpiar()
        txtUsuario.Text = ""
        txtClave.Text = ""
        chkMostrarclave.Checked = False
        txtUsuario.Focus()
    End Sub

    Private Sub frmLogin_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        txtUsuario.Focus()
        If txtUsuario.Text = "" Or txtClave.Text = "" Then
            btnCancelar.Enabled = False
            btnAceptar.Enabled = False
        Else
            btnCancelar.Enabled = True
            btnAceptar.Enabled = True

        End If
    End Sub

    Private Sub CheckBox1_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If chkMostrarclave.Checked Then
            txtClave.PasswordChar = ""
        Else
            txtClave.PasswordChar = "*"
        End If
    End Sub

    Private Sub btnBuscar_Click(sender As System.Object, e As System.EventArgs) Handles btnCancelar.Click
        limpiar()
    End Sub

    Private Sub ButtonX1_Click(sender As System.Object, e As System.EventArgs) Handles btnAceptar.Click
        Try
            cn.Open("sur1", txtUsuario.Text, txtClave.Text)
            MsgBox("Bienvenido! " & txtUsuario.Text & "", MsgBoxStyle.Information, "Acceder")
            Me.Hide()
            System.Threading.Thread.Sleep(2000)
            frmMenu.Show()
        Catch ex As Exception
            MsgBox("Usuario o Contraseña incorrecta.", MsgBoxStyle.Exclamation, "Acceder")
            limpiar()
            Exit Sub
        End Try
        cn.CursorLocation = ADODB.CursorLocationEnum.adUseClient
    End Sub

    Private Sub chkMostrarclave_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles chkMostrarclave.CheckedChanged
        If chkMostrarclave.Checked = True Then
            txtClave.PasswordChar = ""
        Else
            txtClave.PasswordChar = "*"
        End If
    End Sub

    Private Sub btnSalir_Click(sender As System.Object, e As System.EventArgs) Handles btnSalir.Click
        If MsgBox("Está seguro que desea salir del programa", MsgBoxStyle.YesNo, "Salir") = MsgBoxResult.Yes Then
            Me.Close()
            frmSplash.Close()
        Else
            limpiar()
        End If
    End Sub

    Private Sub txtUsuario_TextChanged(sender As System.Object, e As System.EventArgs) Handles txtUsuario.TextChanged
        If txtUsuario.Text = "" Then
            btnCancelar.Enabled = False
            btnAceptar.Enabled = False
        Else
            btnCancelar.Enabled = True
            btnAceptar.Enabled = True
        End If
    End Sub

End Class