Imports SistFoncreagro.BussinessEntities
Imports System.Data.SqlClient
Imports System.Data
Imports System.Data.Common
Public Class AprobarOrdenCompraRepository : Inherits MasterDataAccess : Implements IAprobarOrdenCompraRepository
    Dim aRol As IRolRepository
    Sub New()
        aRol = New RolRepository
    End Sub
    Public Function GetAllFromNivelesAprobarOrdenCompra() As System.Collections.Generic.List(Of BussinessEntities.AprobarOrdenCompra) Implements IAprobarOrdenCompraRepository.GetAllFromNivelesAprobarOrdenCompra
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetAllFromNivelesAprobarOrdenCompra")
        Return SelectObjectFactory(command)
    End Function
    Private Function SelectObjectFactory(ByVal command As SqlCommand) As List(Of AprobarOrdenCompra)
        Dim listaAprobarOc As New List(Of AprobarOrdenCompra)
        Using reader As SqlDataReader = MyBase.ExecuteReader(command)
            While reader.Read()
                Dim aprobarOc As New AprobarOrdenCompra
                With aprobarOc
                    .IdAprobarOrdenCompra = reader.GetInt32(0)
                    .IdRol = reader.GetInt32(1)
                    .Orden = reader.GetInt32(2)
                    .AreaProyecto = reader.GetBoolean(3)
                End With
                aprobarOc.aRol = aRol.GetROLByIdRol(reader.GetInt32(1))
                listaAprobarOc.Add(aprobarOc)
            End While
        End Using
        Return listaAprobarOc
    End Function
    Public Function DeleteNivelAprobarOrdenCompra(ByVal IdAprobarOc As Integer) As Boolean Implements IAprobarOrdenCompraRepository.DeleteNivelAprobarOrdenCompra
        Dim command As SqlCommand = MyBase.CreateSPCommand("DeleteNivelAprobarOrdenCompra")
        Dim resPuestaSql As Boolean

        Dim parametroBase As DbParameter = command.CreateParameter()
        parametroBase.ParameterName = "Resultado"
        parametroBase.Direction = ParameterDirection.Output
        parametroBase.Value = resPuestaSql

        command.Parameters.Add(parametroBase)
        command.Parameters.AddWithValue("IdAprobarOrdenCompra", IdAprobarOc)

        MyBase.ExecuteNonQuery(command)
        resPuestaSql = parametroBase.Value
        Return resPuestaSql
    End Function

    Public Function SaveNivelAprobarOrdenCompra(ByVal aprobarOc As BussinessEntities.AprobarOrdenCompra) As Integer Implements IAprobarOrdenCompraRepository.SaveNivelAprobarOrdenCompra
        Dim command As SqlCommand = MyBase.CreateSPCommand("SaveNivelAprobarOrdenCompra")
        Dim resultado As Int32

        Dim parametroBase As DbParameter = command.CreateParameter()
        parametroBase.ParameterName = "Resultado"
        parametroBase.Direction = ParameterDirection.Output
        parametroBase.Value = resultado

        command.Parameters.Add(parametroBase)
        command.Parameters.AddWithValue("IdRol", aprobarOc.IdRol)
        command.Parameters.AddWithValue("Orden", aprobarOc.Orden)
        command.Parameters.AddWithValue("AreaProyecto", aprobarOc.AreaProyecto)
        MyBase.ExecuteNonQuery(command)
        resultado = parametroBase.Value
        Return resultado
    End Function

    Public Function UpdateNivelAprobarOrdenCompra(ByVal aprobarOc As BussinessEntities.AprobarOrdenCompra) As Integer Implements IAprobarOrdenCompraRepository.UpdateNivelAprobarOrdenCompra
        Dim command As SqlCommand = MyBase.CreateSPCommand("UpdateNivelAprobarOrdenCompra")
        Dim resultado As Int32

        Dim parametroBase As DbParameter = command.CreateParameter()
        parametroBase.ParameterName = "Resultado"
        parametroBase.Direction = ParameterDirection.Output
        parametroBase.Value = resultado

        command.Parameters.Add(parametroBase)
        command.Parameters.AddWithValue("IdAprobarOrdenCompra", aprobarOc.IdAprobarOrdenCompra)
        command.Parameters.AddWithValue("IdRol", aprobarOc.IdRol)
        command.Parameters.AddWithValue("Orden", aprobarOc.Orden)
        command.Parameters.AddWithValue("AreaProyecto", aprobarOc.AreaProyecto)
        MyBase.ExecuteNonQuery(command)
        resultado = parametroBase.Value
        Return resultado
    End Function
End Class
