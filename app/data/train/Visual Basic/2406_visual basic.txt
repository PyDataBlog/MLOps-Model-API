Imports System.Xml
Public Class frmUsuario
    Dim roles(4) As Integer

    Public Sub cargar_usuarios()
        sql = "select * from usuario"
        Try
            rs.Open(sql, cn)
            cboBuscar.Items.Clear()
        Catch ex As Exception
            MsgBox("Error al buscar el Usuario.", MsgBoxStyle.Exclamation, "Atención")
            Exit Sub
        End Try
        While Not rs.EOF
            cboBuscar.Items.Add(rs("login").Value)
            rs.MoveNext()
        End While
        rs.Close()
    End Sub

    Public Sub cargar_rol()
        Dim i As Integer = 0
        sql = "select * from rol"
        Try
            rs.Open(sql, cn)
            cboRol.Items.Clear()
        Catch ex As Exception
            MsgBox("Error al buscar el Usuario.", MsgBoxStyle.Exclamation, "Atención")
            Exit Sub
        End Try
        While Not rs.EOF
            cboRol.Items.Add(rs("nombre").Value)
            roles(i) = rs("id_rol").Value
            i = i + 1
            rs.MoveNext()
        End While
        rs.Close()
    End Sub

    Public Sub cargargrilla()
        sql = "select * from usuario"
        Try
            rs.Open(sql, cn)
        Catch ex As Exception
            MsgBox("err")
            Exit Sub
        End Try
        If Not cargoGrilla(rs, DataGridView1) Then
            MsgBox("No se pudo cargar grilla")
        End If
        rs.Close()
    End Sub

    Public Sub Limpiar()
        cboBuscar.Text = ""
        txtNombre.Text = ""
        txtApellido.Text = ""
        txtCedula.Text = ""
        txtLogin.Text = ""
        cboRol.Text = ""
        optBloquear.Checked = False
        btnCrear.Enabled = True
        btnModificar.Enabled = False
        btnEliminar.Enabled = False
    End Sub

    Private Sub frmUsuario_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        'Dim i As Integer = 0
        gpoDatos.Enabled = True
        btnModificar.Enabled = False
        btnEliminar.Enabled = False
        cargar_usuarios()
        cargar_rol()
        cargarGrilla()
    End Sub

    Private Sub btnCerrar_Click(sender As System.Object, e As System.EventArgs) Handles btnCerrar.Click
        Me.Close()
    End Sub

    Private Sub btnBuscar_Click_2(sender As System.Object, e As System.EventArgs) Handles btnBuscar.Click
        sql = "select u.*, rol.nombre as nombrerol from usuario u, usu_rol ur, rol where u.id_usuario=ur.id_usuario and ur.id_rol=rol.id_rol and login='" & cboBuscar.Text & "'"
        Try
            rs.Open(sql, cn)
        Catch ex As Exception
            MsgBox("Error al buscar el Usuario.", MsgBoxStyle.Exclamation, "Atención")
            Exit Sub
        End Try
        If rs.RecordCount = 1 Then
            txtCedula.Text = rs("cedula").Value
            txtLogin.Text = rs("login").Value
            txtNombre.Text = rs("nombre").Value
            txtApellido.Text = rs("apellido").Value
            cboRol.SelectedItem = rs("nombrerol").Value
            btnCrear.Enabled = False
            btnModificar.Enabled = True
            btnEliminar.Enabled = True
        Else
            MsgBox("No se encontró el usuario.", MsgBoxStyle.Exclamation, "Atención")
        End If
        rs.Close()
    End Sub

    Private Sub btnCancelar_Click_1(sender As System.Object, e As System.EventArgs) Handles btnCancelar.Click
        Limpiar()
    End Sub

    Private Sub btnAceptar_Click_1(sender As System.Object, e As System.EventArgs) Handles btnCrear.Click
        Dim idusu As Integer
        sql = "insert into usuario (cedula, login, nombre, apellido) Values (" & txtCedula.Text & ",'" & txtLogin.Text & "','" & txtNombre.Text & "','" & txtApellido.Text & "')"
        Try
            cn.Execute(sql)
        Catch ex As Exception
            MsgBox("No se pudo conectar con la Base de Datos.", MsgBoxStyle.Exclamation, "Error")
            Exit Sub
        End Try

        sql = "select max (dbinfo('sqlca.sqlerrd1')) from usuario"

        Try
            rs.Open(sql, cn)
            idusu = rs(0).Value
            rs.Close()
        Catch ex As Exception
            MsgBox("Ocurrió un error", MsgBoxStyle.Exclamation, "Error")
            Exit Sub
        End Try

        sql = "insert into usu_rol (id_usuario, id_rol) Values (" & idusu & "," & roles(cboRol.SelectedIndex) & ")"
        Try
            cn.Execute(sql)
            MsgBox("Usuario registrado correctamente.", MsgBoxStyle.Information, "Atención")
        Catch ex As Exception
            MsgBox(sql + ex.ToString)
            Exit Sub
        End Try
        Limpiar()
        cargar_usuarios()
        cargar_rol()
        cargargrilla()
    End Sub

    Private Sub txtCedula_KeyPress(sender As Object, e As System.Windows.Forms.KeyPressEventArgs) Handles txtCedula.KeyPress
        'TextBox solo numeros
        validonumero(e)
    End Sub

    Private Sub txtCedula_TextChanged(sender As System.Object, e As System.EventArgs) Handles txtCedula.TextChanged

    End Sub

    Private Sub btnEliminar_Click(sender As System.Object, e As System.EventArgs) Handles btnEliminar.Click
        sql = "delete from usu_rol where id_usuario=(select id_usuario from usuario where login='" & cboBuscar.Text & "')"
        Try
            If MsgBox("¿Está seguro que desea eliminar el usuario?", MsgBoxStyle.YesNo) = MsgBoxResult.Yes Then
                cn.Execute(sql)
            End If
        Catch ex As Exception
            MsgBox("No se pudo conectar con la Base de Datos.", MsgBoxStyle.Exclamation, "Error")
            Exit Sub
        End Try

        sql = "delete from usuario where login='" & cboBuscar.Text & "'"
        Try
            cn.Execute(sql)
            MsgBox("Usuario eliminado con éxito", MsgBoxStyle.Information, "Atención")
        Catch ex As Exception
            MsgBox("No se pudo conectar con la Base de Datos.", MsgBoxStyle.Exclamation, "Error")
            Exit Sub
        End Try
        Limpiar()
        cargar_usuarios()
        cargar_rol()
        cargargrilla()
    End Sub

    Private Sub btnModificar_Click(sender As System.Object, e As System.EventArgs) Handles btnModificar.Click
        sql = "update usu_rol set id_rol= (select id_rol from rol where nombre='" & cboRol.Text & "') where id_usuario=(select id_usuario from usuario where login='" & txtLogin.Text & "')"
        Try
            cn.Execute(sql)
        Catch ex As Exception

        End Try

        sql = "update usuario set cedula=" & txtCedula.Text & ", login='" & txtLogin.Text & "', nombre='" & txtNombre.Text & "', apellido='" & txtApellido.Text & "'  where login='" & cboBuscar.Text & "'"
        Try
            cn.Execute(sql)
            MsgBox("Modificación exitosa", MsgBoxStyle.Information, "Atención")
        Catch ex As Exception
            MsgBox("Se produjo un error", MsgBoxStyle.Exclamation, "Atención")
            Exit Sub
        End Try
        Limpiar()
        cargar_usuarios()
        cargar_rol()
        cargargrilla()
    End Sub

    Private Sub ButtonX1_Click(sender As System.Object, e As System.EventArgs)
        'Instancia el objeto usu
        'Instanciamos un nuevo objeto usu en funcion de la clase Usuario
        'Dim usu As New Usuario(txtCedula.Text, txtLogin.Text, txtNombre.Text, txtApellido.Text, cboRol.Text)

        'Declaramos la variable para mostrar el dato
        'Dim ced As Integer
        'Asignamos el objeto usu usando la funcion GetCedula
        'Devuelve la cedula del objeto usu y la almacena en ced
        'ced = usu.GetCedula()
        'MsgBox("Cedula:" & ced)
    End Sub

End Class