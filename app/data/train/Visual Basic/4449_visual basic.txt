Imports Oracle.DataAccess.Client
Imports DevComponents.DotNetBar

Public Class ctrlTienda
    Dim query As String
    Private Sub ctrlTienda_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Try
            frmMain._cnn.Open()
            query = "SELECT idlugar as id, nombre || '-' || direccion as nombre from lugar"
            _LLENAR_CMB(query, cbLugar)
            query = "Select idlu_tipo as id, nombre from lu_tipo"
            _LLENAR_CMB(query, cbTipo)
        Catch ex As Exception
            _ESTADO(ex.Message, frmMain.lbEstado)
        Finally
            frmMain._cnn.Close()
        End Try
    End Sub


    Private Sub btGuardar_Click(sender As Object, e As EventArgs) Handles btGuardar.Click
        Try
            frmMain._cnn.Open()
            Dim dbms As New DBMSOutput(frmMain._cnn)
            If txNombre.Text = "" Or txDireccion.Text = "" Or txTel1.Text = "" Or txSerie.Text = "" Then
                _ESTADO("No Pueden existir Campos Vacios", frmMain.lbEstado)
            ElseIf swBt.Value Then
                dbms.Enable()
                frmMain._cmd = New OracleCommand()
                frmMain._cmd.Connection = frmMain._cnn
                With frmMain._cmd
                    .CommandType = CommandType.StoredProcedure
                    .CommandText = "SP_NUEVO_LUGAR"
                    .Parameters.Add(New OracleParameter("v_idlugar", OracleDbType.Varchar2)).Value = CStr(txSerie.Text)
                    .Parameters.Add(New OracleParameter("v_nombre", OracleDbType.Varchar2)).Value = CStr(txNombre.Text)
                    .Parameters.Add(New OracleParameter("v_direccion", OracleDbType.Varchar2)).Value = CStr(txDireccion.Text)
                    .Parameters.Add(New OracleParameter("v_tel1", OracleDbType.Varchar2)).Value = CStr(txTel1.Text)
                    .Parameters.Add(New OracleParameter("v_tel2", OracleDbType.Varchar2)).Value = CStr(txTel2.Text)
                    .Parameters.Add(New OracleParameter("v_idlutipo", OracleDbType.Varchar2)).Value = CStr(cbTipo.SelectedValue)
                    .Parameters.Add(New OracleParameter("v_estado", OracleDbType.Int32, ParameterDirection.Output))
                End With
                frmMain._cmd.ExecuteNonQuery()

                If frmMain._cmd.Parameters("v_estado").Value > 0 Then

                    dbms.Show(frmMain.lbEstado)
                    dbms.Disable()
                    txSerie.Text = ""
                    txDireccion.Text = ""
                    txNombre.Text = ""
                    txTel1.Text = ""
                    txTel2.Text = ""
                ElseIf frmMain._cmd.Parameters("v_estado").Value = 0 Then
                    dbms.Show(frmMain.lbEstado)
                    txSerie.Focus()
                Else
                    dbms.Show()
                End If
            ElseIf swBt.Value = False Then
                dbms.Enable()
                frmMain._cmd = New OracleCommand()
                frmMain._cmd.Connection = frmMain._cnn
                With frmMain._cmd
                    .CommandType = CommandType.StoredProcedure
                    .CommandText = "SP_EDITAR_ESTADO_LUGAR"
                    .Parameters.Add(New OracleParameter("v_idlugar", OracleDbType.Varchar2)).Value = CStr(txSerie.Text)
                    .Parameters.Add(New OracleParameter("v_nombre", OracleDbType.Varchar2)).Value = CStr(txNombre.Text)
                    .Parameters.Add(New OracleParameter("v_direccion", OracleDbType.Varchar2)).Value = CStr(txDireccion.Text)
                    .Parameters.Add(New OracleParameter("v_tel1", OracleDbType.Varchar2)).Value = CStr(txTel1.Text)
                    .Parameters.Add(New OracleParameter("v_tel2", OracleDbType.Varchar2)).Value = CStr(txTel2.Text)
                    .Parameters.Add(New OracleParameter("v_estado", OracleDbType.Int32, ParameterDirection.Output))
                End With
                frmMain._cmd.ExecuteNonQuery()

                If frmMain._cmd.Parameters("v_estado").Value > 0 Then

                    dbms.Show(frmMain.lbEstado)
                    dbms.Disable()
                    txSerie.Text = ""
                    txDireccion.Text = ""
                    txNombre.Text = ""
                    txTel1.Text = ""
                    txTel2.Text = ""

                Else
                    dbms.Show()
                End If

            End If
            query = "SELECT idlugar as id, nombre || '-' || direccion as nombre from lugar"
            _LLENAR_CMB(query, cbLugar)
        Catch ex As Exception
            _ESTADO(ex.Message, frmMain.lbEstado)
        Finally
            frmMain._cnn.Close()
        End Try
    End Sub
    Private Sub swBt_ValueChanged(sender As Object, e As EventArgs) Handles swBt.ValueChanged
        If swBt.Value = False Then
            cbLugar.Visible = True
            txSerie.Enabled = False
            cbTipo.Enabled = False
            imgBuscar.Visible = True
        Else
            txDireccion.Text = ""
            txNombre.Text = ""
            txSerie.Text = ""
            txTel1.Text = ""
            txTel2.Text = ""
            cbLugar.Visible = False
            cbTipo.Enabled = True
            imgBuscar.Visible = False
            txSerie.Enabled = True
        End If

    End Sub

    Private Sub cbLugar_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbLugar.LostFocus
        Try
            frmMain._cnn.Open()
            If swBt.Value = False Then
                query = "select nombre, direccion, telefono, telefono1 from lugar where idlugar = '" & cbLugar.SelectedValue.ToString & "'"
                ' MsgBox(query)
                frmMain._cmd = New OracleCommand(query, frmMain._cnn)
                Dim cnt As OracleDataReader = frmMain._cmd.ExecuteReader()
                While cnt.Read()
                    txSerie.Text = cbLugar.SelectedValue.ToString
                    txNombre.Text = CStr(cnt.GetString(0))
                    txDireccion.Text = cnt.GetString(1)
                    txTel1.Text = cnt.GetString(2)
                    txTel2.Text = cnt.GetString(3)
                End While
                cnt.Close()
            End If
        Catch ex As Exception
            _ESTADO(ex.Message, frmMain.lbEstado)
        Finally
            frmMain._cnn.Close()
        End Try
    End Sub

    
    Private Sub cbLugar_SelectedIndexChanged_1(sender As Object, e As EventArgs) Handles cbLugar.SelectedIndexChanged

    End Sub
End Class
