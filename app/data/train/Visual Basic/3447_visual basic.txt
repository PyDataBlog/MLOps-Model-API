Imports Capa_Entidad
Imports Modelos
Imports System.Text
Public Class SistemaBL
    Public Function Registrar(sistema As Sistema) As Boolean
        Dim sisDao As New SistemaDao
        Return sisDao.Mantenimiento(sistema)
    End Function
    Public Function GetSistema() As Sistema
        Dim sisDao As New SistemaDao
        Return sisDao.GetSistema
    End Function
    Public Function ExistsEmpresa(ruc As String) As Boolean
        Dim empresa As Empresa
        Dim daoEmpresa As New EmpresaDao
        empresa = daoEmpresa.GetById(ruc)
        If IsNothing(empresa) Then
            Return False
        Else
            Return True
        End If
    End Function
    Public Function GeEmpresa(ruc As String) As Empresa
        Dim empresa As Empresa
        Dim daoEmpresa As New EmpresaDao
        empresa = daoEmpresa.GetById(ruc)
        If IsNothing(empresa) Then
            Return New Empresa
        Else
            Return empresa
        End If
    End Function
    Public Function RegistrarEmpresa(empresa As Empresa) As Boolean
        Dim daoEmp As New EmpresaDao
        Return daoEmp.Registrar(empresa)
    End Function
    Public Sub BuildShema(codigo As String)
        Dim daoSistema As New SistemaDao
        Dim anio As String = DateTime.Now.Year.ToString.Substring(2)
        daoSistema.BuildShema(String.Concat(anio, codigo))
    End Sub
End Class
