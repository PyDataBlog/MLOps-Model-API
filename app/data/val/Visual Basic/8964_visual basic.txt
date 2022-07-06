Imports Modelos
Imports Capa_Entidad
Public Class CuentaBL
    Dim Datos As New CuentaDAO
    Public Function Cuenta_Autocompletado(ByVal largo As String) As DataTable
        Return Datos.Cuenta_Mostrar(largo)
    End Function
    Public Function Cuenta_Autocompletado_amarres(ByVal como As String) As DataTable
        Return Datos.Amarres_Mostrar(como)
    End Function
    Public Function Cuenta_Export() As DataTable
        Return Datos.Cuenta_Showall_Export()
    End Function
    Public Function Cuenta_DatosLB(ByVal condicion As Integer, ByVal entcont As Cuenta) As DataTable
        Return Datos.Cuenta_data(condicion, entcont)
    End Function
    Public Function Cuenta_Amarre_RegisterLB(ByVal cond As Integer, ByVal entcont As Cuenta) As Integer
        Dim verificar As Integer
        verificar = Datos.Cuenta_Amarre_Managemenet(cond, entcont)
        Return verificar
    End Function
    Public Function Cuenta_ManagementLB(ByVal condicion As Integer, ByVal entcont As Cuenta) As Integer
        Dim verificar As Integer
        verificar = Datos.Cuenta_Managment(condicion, entcont)
        Return verificar
    End Function
    Public Function Cuenta_Amarre_ShowLB(ByVal entcont As Cuenta) As DataTable
        Return Datos.Cuenta_Amarre_Show(entcont)
    End Function

    Public Function ImportXtoMysqlLB(ByVal cmd As String)
        Datos.importXtomysql(cmd)
        Return Nothing
    End Function
End Class
