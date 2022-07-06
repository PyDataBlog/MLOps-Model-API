Public Class frmVictima
    Dim idanimal As Integer
    Dim nom, sexo, raza, causa As String
    Dim fechanac As String

    Public Sub capturar_datos()
        sql = "select * from animal where id_animal=" & Val(cboID.Text)
        Try
            rs.Open(sql, cn)
        Catch ex As Exception
            MsgBox("Error al buscar el Animal.", MsgBoxStyle.Exclamation, "Atención")
            Exit Sub
        End Try
        If rs.RecordCount = 1 Then
            idanimal = cboID.Text
            nom = rs("nom_animal").Value
            sexo = rs("sexo").Value
            raza = rs("raza").Value
            fechanac = rs("fecha_nac").Value
        Else
            MsgBox("No se encontro el animal.", MsgBoxStyle.Exclamation, "Atención")
        End If
        rs.Close()

        sql = "update animal set x=" & 0 & " where id_animal=" & cboID.Text & ""
        Try
            cn.Execute(sql)
            MsgBox("Modificación exitosa.", MsgBoxStyle.Information, "Atención")
        Catch ex As Exception
            MsgBox("No se pudo conectar con la Base de Datos.", MsgBoxStyle.Exclamation, "Error")
            Exit Sub
        End Try

        sql = "update animal set fecha_muerte='" & dateMuerte.Value.ToString("dd/MM/yyyy") & "' where id_animal=" & cboID.Text & ""
        Try
            MsgBox(sql)
            cn.Execute(sql)
            MsgBox("Modificación exitosa.", MsgBoxStyle.Information, "Atención")
        Catch ex As Exception
            MsgBox("No se pudo conectar con la Base de Datos.", MsgBoxStyle.Exclamation, "Error")
            Exit Sub
        End Try
    End Sub

    Public Sub cargar_toro()
        sql = "select * from toro, animal where id_toro=id_animal and x=1"
        Try
            rs.Open(sql, cn)
            cboID.Items.Clear()
        Catch ex As Exception
            MsgBox("Error al buscar los datos.", MsgBoxStyle.Exclamation, "Atención")
            Exit Sub
        End Try
        While Not rs.EOF
            cboID.Items.Add(rs("id_toro").Value)
            rs.MoveNext()
        End While
        rs.Close()
    End Sub

    Public Sub cargar_vaca()
        sql = "select * from vaca, animal where id_vaca=id_animal and x=1"
        Try
            rs.Open(sql, cn)
            cboID.Items.Clear()
        Catch ex As Exception
            MsgBox("Error al buscar los datos.", MsgBoxStyle.Exclamation, "Atención")
            Exit Sub
        End Try
        While Not rs.EOF
            cboID.Items.Add(rs("id_vaca").Value)
            rs.MoveNext()
        End While
        rs.Close()
    End Sub

    Public Sub cargar_cria()
        sql = "select * from cria, animal where id_cria=id_animal and x=1"
        Try
            rs.Open(sql, cn)
            cboID.Items.Clear()
        Catch ex As Exception
            MsgBox("Error al buscar los datos.", MsgBoxStyle.Exclamation, "Atención")
            Exit Sub
        End Try
        While Not rs.EOF
            cboID.Items.Add(rs("id_cria").Value)
            rs.MoveNext()
        End While
        rs.Close()
    End Sub

    Public Sub cargar_grilla()
        sql = "select * from animal where x=0"
        Try
            rs.Open(sql, cn)
        Catch ex As Exception
            MsgBox("No se pudo cargar el listado de victimas")
            Exit Sub
        End Try
        If Not cargoGrilla(rs, DataGridView1) Then
            MsgBox("No se pudo cargar el listado de victimas")
        End If
        rs.Close()
    End Sub

    Public Sub limpiar()
        cboID.Text = ""
        dateMuerte.Text = ""
        cboCausa.Text = ""
    End Sub

    Private Sub frmMuerte_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load
        cargar_grilla()
    End Sub

    Private Sub btnRegistrar_Click(sender As System.Object, e As System.EventArgs) Handles btnRegistrar.Click
        capturar_datos()
    End Sub

    Private Sub cboTipo_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles cboTipo.SelectedIndexChanged
        If cboTipo.Text = "Toro" Then
            cargar_toro()
        End If
        If cboTipo.Text = "Vaca" Then
            cargar_vaca()
        End If
        If cboTipo.Text = "Cria" Then
            cargar_cria()
        End If
    End Sub

    Private Sub gpoVictima_Enter(sender As System.Object, e As System.EventArgs) Handles gpoVictima.Enter

    End Sub
End Class