Imports Capa_Entidad
Imports Modelos
Public Class ActivacionEmpresasBL
    Inherits BaseBL
    Public ReadOnly count As Integer
    Private dtEmpresa As DataTable
    Private usuario As Usuario
    Private empresas As Empresa()
    Public Sub New(usuario As Usuario, empresas As Empresa())

        Me.usuario = usuario
        Me.empresas = empresas

        dtEmpresa = New DataTable("empresa_permiso")
        dtEmpresa.Columns.Add("ruc", Type.GetType("System.String"))
        dtEmpresa.Columns.Add("rzn_scl", Type.GetType("System.String"))
        dtEmpresa.Columns.Add("rzn_scl_ls", Type.GetType("System.String"))
        dtEmpresa.Columns.Add("cod", Type.GetType("System.String"))
        dtEmpresa.Columns.Add("dig", Type.GetType("System.Int32"))
        dtEmpresa.Columns.Add("id", Type.GetType("System.Int32"))
        dtEmpresa.Columns.Add("compt", Type.GetType("System.Boolean"))
        dtEmpresa.Columns.Add("modu", Type.GetType("System.String"))
        dtEmpresa.Columns.Add("estado", Type.GetType("System.Int32"))
    End Sub
    Public Function getEmpresas() As DataTable

        Dim empDao As New EmpresaDao
        Dim listEmpresa As List(Of Empresa) = empDao.GetAll()

        If usuario.Tipo = "normal" Then
            For Each empresa As Empresa In empresas
                Dim empresaSearch As Empresa = listEmpresa.SingleOrDefault(Function(x) x.RUC = empresa.RUC)
                If empresaSearch Is Nothing Then
                    AddNewDataRow(empresa, Nothing, 1)
                Else
                    Dim permisoDao As New PermisoDao
                    Dim permiso As Permiso = permisoDao.GetByUsuarioAndEmpresa(usuario.Id, empresa.RUC)
                    If permiso Is Nothing Then
                        AddNewDataRow(empresaSearch, permiso, 1)
                    Else
                        AddNewDataRow(empresaSearch, permiso, 0)
                    End If

                End If
            Next
        Else
            For Each empresa As Empresa In empresas
                Dim empresaSearch As Empresa = listEmpresa.SingleOrDefault(Function(x) x.RUC = empresa.RUC)
                If empresaSearch Is Nothing Then
                    AddNewDataRow(empresa, Nothing, 2)
                Else
                    AddNewDataRow(empresaSearch, Nothing, 0)
                End If
            Next
        End If
        Return dtEmpresa
    End Function
    Private Sub AddNewDataRow(emp As Empresa, per As Permiso, est As Integer)
        Dim dr As DataRow = dtEmpresa.NewRow
        dr("ruc") = emp.RUC
        dr("rzn_scl") = emp.Nombre
        dr("rzn_scl_ls") = emp.Aliass
        dr("cod") = emp.Codigo
        dr("dig") = emp.Digito
        dr("id") = If(per Is Nothing, 0, per.Id)
        dr("compt") = If(per Is Nothing, 0, per.ConvertBooleanToCompleto())
        dr("modu") = If(per Is Nothing, 0, per.Modulo)
        dr("estado") = est
        dtEmpresa.Rows.Add(dr)
    End Sub
    Public Function VencimientoPago(ultimoDigito As Integer, periodo As Integer) As VencimientoPagos
        Dim vp As New VencimientoPagos
        vp.Ruc = ultimoDigito
        vp.Periodo = periodo
        Dim modelo As New VencimientoPagoDao(vp)
        Return modelo.GetByPeridoAndRuc
    End Function



End Class
