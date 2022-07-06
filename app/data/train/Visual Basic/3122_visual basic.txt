Imports MySql.Data.MySqlClient
Imports Capa_Entidad
Public Class ComprobanteDAO
    'Inherits BaseDao
    Inherits DBEmpDao
    Private conexionValue As MySqlConnection
    Dim da As New MySqlDataAdapter
    Dim conn As New MySqlConnection
    Public Function Comprobante_mostrar_cuenta() As DataTable
        Return Consultar_Tabla_MySQL("select CONCAT(codigo,'  ',nombre) as Cuenta from plan_contable where codigo regexp '^33|^60|^62^63|^65|^4|^1|^7'  and length(codigo)>=6")
    End Function
    Public Function Comprobante_mostrar_cuenta_n() As DataTable
        Return Consultar_Tabla_MySQL("select CONCAT(alias,' ',codigo) as alias from plan_contable where codigo regexp '^33|^60|^62^63|^65|^4|^10' and length(codigo)>=6")
    End Function
    Public Function Comprobante_mostrar_moneda() As DataTable
        Return Consultar_Tabla_MySQL("SELECT codigo,descripcion FROM bdsist.moneda")
    End Function
    Public Function Comprobante_mostrar_tipodoc() As DataTable
        Return Consultar_Tabla_MySQL("SELECT Numero,Descripcion FROM bdsist.tipo_documento_pago")
    End Function
    Public Function Comprobante_mostrar_auxiliar() As DataTable
        Return Consultar_Tabla_MySQL("SELECT ruc,razon_social FROM bdsist.auxiliar")
    End Function
    Public Function razon_social(ByVal entCom As Comprobante) As String
        conn = Me.conexion
        da = New MySqlDataAdapter("SELECT razon_social FROM bdsist.auxiliar WHERE ruc = @ruc", conn)
        da.SelectCommand.CommandType = CommandType.Text
        da.SelectCommand.Parameters.AddWithValue("@ruc", entCom.ruc)
        Dim Razon As String = da.SelectCommand.ExecuteScalar
        Return Razon
    End Function
    Public Function comprobante_diario_autogenerado() As String
        conn = Me.conexion
        da = New MySqlDataAdapter("bd16001.sp_comprobante_numero_diario", conn)
        da.SelectCommand.CommandType = CommandType.StoredProcedure
        Dim Razon As String = da.SelectCommand.ExecuteScalar
        Return Razon
    End Function
    'Public Function comprobante_cajabanco_autogenerado() As String
    '    conn = Me.conexion
    '    da = New MySqlDataAdapter("bd16001.sp_cajabanco_codigo_auto", conn)
    '    da.SelectCommand.CommandType = CommandType.StoredProcedure
    '    Dim Razon As String = da.SelectCommand.ExecuteScalar
    '    Return Razon
    'End Function
    'Public Function comprobante_registocompra_autogenerado() As String
    '    conn = Me.conexion
    '    da = New MySqlDataAdapter("bd16001.sp_comprobante_registroc_autogenerado", conn)
    '    da.SelectCommand.CommandType = CommandType.StoredProcedure
    '    Dim Razon As String = da.SelectCommand.ExecuteScalar
    '    Return Razon
    'End Function
    Public Function comprobante_autogenerado_registro(ByVal pre As Object) As String
        conn = Me.conexion
        da = New MySqlDataAdapter("bd16001.sp_comprobante_autogenerado", conn)
        da.SelectCommand.CommandType = CommandType.StoredProcedure
        da.SelectCommand.Parameters.AddWithValue("pre", pre)
        Dim Razon As String = da.SelectCommand.ExecuteScalar
        Return Razon
    End Function

    'Public Function comprobante_cabecera_register(ByVal entCom As Comprobante)
    '    Return Mantenimiento_SQL("sp_comprobante_registrar_c", entCom.nrodiario, entCom.periodo, entCom.nrocompro, entCom.moneda, entCom.tipo_adq, entCom.tipo_doc, entCom.serie, entCom.nrodocu, entCom.fechae, entCom.fechav, entCom.ruc)
    'End Function
    Public Function comprobante_cabecera_CRU(ByVal entCom As Comprobante)
        conexionValue = Me.conexion
        Dim rowsaffected As Integer
        Dim sql As String = "bd16001.sp_comprobante_cabecera"
        Dim consultaSQL As MySqlCommand = New MySqlCommand(sql, conexionValue)

        With consultaSQL
            .Connection = conexionValue
            .CommandType = CommandType.StoredProcedure
            .Parameters.AddWithValue("n_d", entCom.nrodiario)
            .Parameters.AddWithValue("p", entCom.periodo)
            .Parameters.AddWithValue("n_r", entCom.nrocompro)
            .Parameters.AddWithValue("c_m", entCom.moneda)
            .Parameters.AddWithValue("t_adq", entCom.tipo_adq)
            .Parameters.AddWithValue("t_d", entCom.tipo_doc)
            .Parameters.AddWithValue("s_d", entCom.serie)
            .Parameters.AddWithValue("n_doc", entCom.nrodocu)
            .Parameters.AddWithValue("fee_d", entCom.fechae)
            .Parameters.AddWithValue("fev_d", entCom.fechav)
            .Parameters.AddWithValue("r_a", entCom.ruc)
            .Parameters.AddWithValue("est", entCom.estado)

        End With
        Try
            rowsaffected = consultaSQL.ExecuteNonQuery()
        Catch ex As Exception
        Finally
            conexionValue.Close()
        End Try
        Return rowsaffected
    End Function
    Public Function registro_compra_CRU(ByVal entCom As Comprobante)
        conexionValue = Me.conexion
        Dim rowsaffected As Integer
        Dim sql As String = "bd16001.sp_registro_compra"
        Dim consultaSQL As MySqlCommand = New MySqlCommand(sql, conexionValue)

        With consultaSQL

            .Connection = conexionValue
            .CommandType = CommandType.StoredProcedure
            .Parameters.AddWithValue("nro_comp_c", entCom.nrocompro)
            .Parameters.AddWithValue("fec_emi_c", entCom.fechae)
            .Parameters.AddWithValue("fec_vcto_c", entCom.fechav)
            .Parameters.AddWithValue("tip_doc_c", entCom.tipo_doc)
            .Parameters.AddWithValue("ser_doc_c", entCom.serie)
            .Parameters.AddWithValue("ani_emi_dua_c", entCom.fecha_dua_dsi)
            .Parameters.AddWithValue("nro_doc_c", entCom.nrodocu)
            .Parameters.AddWithValue("tip_doc_ide_c", "06")
            .Parameters.AddWithValue("ruc_c", entCom.ruc)
            .Parameters.AddWithValue("razon_social_c", entCom.razonsocial)
            .Parameters.AddWithValue("adq_grav_oper_grav_exp_base_c", entCom.baseimpo1)
            .Parameters.AddWithValue("igv_adq_grav_oper_grav_exp_base_c", entCom.igv1)
            .Parameters.AddWithValue("adq_grav_oper_grav_grav_base_c", entCom.baseimpo2)
            .Parameters.AddWithValue("igv_adq_grav_oper_grav_grav_base_c", entCom.igv2)
            .Parameters.AddWithValue("adq_grav_oper_grav_base_imp_c", entCom.baseimpo3)
            .Parameters.AddWithValue("igv_adq_grav_oper_grav_base_imp_c", entCom.igv3)
            .Parameters.AddWithValue("val_adq_no_grav_base_imp_c", entCom.baseimpo4)
            .Parameters.AddWithValue("isc_c", entCom.isc)
            .Parameters.AddWithValue("otr_tri_car_c", "")
            .Parameters.AddWithValue("imp_total_c", entCom.importe_total)
            .Parameters.AddWithValue("nro_comp_suj_no_domi_c", "")
            .Parameters.AddWithValue("num_const_detra_c", "")
            .Parameters.AddWithValue("fec_detract_c", "")
            .Parameters.AddWithValue("tip_cam_c", entCom.tipo_cambio)
            .Parameters.AddWithValue("ref_fec_c", "")
            .Parameters.AddWithValue("ref_tip_doc_c", "")
            .Parameters.AddWithValue("ref_ser_doc_c", "")
            .Parameters.AddWithValue("ref_nro_doc_c", "")
        End With
        Try
            rowsaffected = consultaSQL.ExecuteNonQuery()
        Catch ex As Exception
        Finally
            conexionValue.Close()
        End Try
        Return rowsaffected
    End Function
    'Public Function comprobante_cabecera_register_cb(ByVal entCom As Comprobante)
    '    conexionValue = Me.conexion
    '    Dim rowsaffected As Integer
    '    Dim sql As String = "bd16001.sp_comprobante_registrar_cb"
    '    Dim consultaSQL As MySqlCommand = New MySqlCommand(sql, conexionValue)

    '    With consultaSQL
    '        .Connection = conexionValue
    '        .CommandType = CommandType.StoredProcedure
    '        .Parameters.AddWithValue("n_d", entCom.nrodiario)
    '        .Parameters.AddWithValue("p", entCom.periodo)
    '        .Parameters.AddWithValue("n_r", entCom.nrocaba)
    '        .Parameters.AddWithValue("est", entCom.estado)

    '    End With
    '    Try
    '        rowsaffected = consultaSQL.ExecuteNonQuery()
    '    Catch ex As Exception
    '    Finally
    '        conexionValue.Close()
    '    End Try
    '    Return rowsaffected
    'End Function
    Public Function comprobante_detalle_CRU(ByVal entCom As Comprobante)
        conexionValue = Me.conexion
        Dim rowsaffected As Integer
        Dim sql As String = "bd16001.sp_comprobante_detalle"
        Dim consultaSQL As MySqlCommand = New MySqlCommand(sql, conexionValue)

        With consultaSQL
            .Connection = conexionValue
            .CommandType = CommandType.StoredProcedure
            .Parameters.AddWithValue("nrd", entCom.nrodetalle)
            .Parameters.AddWithValue("nrdi", entCom.nrodiario)
            .Parameters.AddWithValue("cue", entCom.cuenta)
            .Parameters.AddWithValue("glo", entCom.glosa)
            .Parameters.AddWithValue("deb", entCom.debe)
            .Parameters.AddWithValue("hab", entCom.haber)
        End With
        Try
            rowsaffected = consultaSQL.ExecuteNonQuery()
        Catch ex As Exception
        Finally
            conexionValue.Close()
        End Try
        Return rowsaffected
    End Function
    'Public Function comprobante_detalle_register_cb(ByVal entCom As Comprobante)
    '    conexionValue = Me.conexion
    '    Dim rowsaffected As Integer
    '    Dim sql As String = "bd16001.sp_comprobante_registrar_cbdetalle"
    '    Dim consultaSQL As MySqlCommand = New MySqlCommand(sql, conexionValue)

    '    With consultaSQL
    '        .Connection = conexionValue
    '        .CommandType = CommandType.StoredProcedure
    '        .Parameters.AddWithValue("nrd", entCom.nrodetalle)
    '        .Parameters.AddWithValue("nrr", entCom.nrocaba)
    '        .Parameters.AddWithValue("cue", entCom.cuenta)
    '        .Parameters.AddWithValue("glo", entCom.glosa)
    '        .Parameters.AddWithValue("deb", entCom.debe)
    '        .Parameters.AddWithValue("hab", entCom.haber)
    '    End With
    '    Try
    '        rowsaffected = consultaSQL.ExecuteNonQuery()
    '    Catch ex As Exception
    '    Finally
    '        conexionValue.Close()
    '    End Try
    '    Return rowsaffected
    'End Function

    'Public Function comprobante_detalle_register(ByVal entCom As Comprobante)
    '    Return Mantenimiento_SQL("sp_comprobante_registrar_cdetalle", entCom.nrodetalle, entCom.nrocompro, entCom.cuenta, entCom.glosa, entCom.debe, entCom.haber)
    'End Function
    'Public Function comprobante_detalle_autogenerado(ByVal entCom As Comprobante) As String
    '    conn = Me.conexion
    '    da = New MySqlDataAdapter("SELECT LPAD(IFNULL(MAX(nro_det),0)+10,3,'0') detalle FROM diario_detalle where nro_reg=@nro_reg", conn)
    '    da.SelectCommand.CommandType = CommandType.Text
    '    da.SelectCommand.Parameters.AddWithValue("@nro_reg", entCom.nrocompro)
    '    Dim Razon As String = da.SelectCommand.ExecuteScalar
    '    Return Razon
    'End Function
    Public Function Comprobante_cebecera_llenar(ByVal entCom As Comprobante) As DataTable
        Return Consultar_Tabla_MySQL("SELECT * FROM diario WHERE num_dia='" + entCom.nrodiario + "'")
    End Function
    Public Function Comprobante_detalle_llenar(ByVal entCom As Comprobante) As DataTable
        Return Consultar_Tabla_MySQL("SELECT nro_det,cuenta,p.nombre,glosa,debe,haber FROM diario_detalle d INNER JOIN plan_contable p on d.cuenta=p.codigo WHERE nro_dia='" + entCom.nrodiario + "'")
    End Function

End Class

