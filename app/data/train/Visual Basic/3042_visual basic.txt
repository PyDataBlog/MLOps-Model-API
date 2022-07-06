Imports System.Data.SqlClient
Imports System.Data.Common
Imports SistFoncreagro.BussinessEntities

Public Class DetalleRequerimientoRepository : Inherits MasterDataAccess : Implements IDetalleRequerimientoRepository
    Dim catalogoRepository As ICatalogoRepository
    Dim unidadRepository As IUnidadMedidaRepository
    Sub New()
        catalogoRepository = New CatalogoRepository
        unidadRepository = New UnidadMedidaRepository
    End Sub
    Public Sub EliminarDetalle(ByVal idDetalleRequerimiento As Integer) Implements IDetalleRequerimientoRepository.EliminarDetalle
        Dim command As SqlCommand = MyBase.CreateSPCommand("DeleteDETALLEREQUERIMIENTO")
        command.Parameters.AddWithValue("IdDetalleRequerimiento", idDetalleRequerimiento)
        MyBase.ExecuteNonQuery(command)
    End Sub
    Public Function GetAllFromDetalleRequerimientoByEstadoByIdReq(ByVal idRequerimiento As Integer) As System.Collections.Generic.List(Of BussinessEntities.DetalleRequerimiento) Implements IDetalleRequerimientoRepository.GetAllFromDetalleRequerimientoByEstadoByIdReq
        Dim command As SqlCommand =
        MyBase.CreateSPCommand("GetAllFromDetalleRequerimientoByEstadoByIdReq")
        command.Parameters.AddWithValue("IdRequerimiento", idRequerimiento)
        Return SelectObjectFactoryDetalleRequerimiento(command)
    End Function
    Public Function GetAllFromDetalleRequerimientoByIdReq(ByVal idRequerimiento As Integer) As System.Collections.Generic.List(Of BussinessEntities.DetalleRequerimiento) Implements IDetalleRequerimientoRepository.GetAllFromDetalleRequerimientoByIdReq
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetAllFromDETALLERREQUERIMIENTOByID")
        command.Parameters.AddWithValue("IdRequerimiento", idRequerimiento)
        Return SelectObjectFactoryDetalleRequerimiento(command)
    End Function
    Public Function GetAllFromDetalleRequerimientoOrdenadoByIdReq(ByVal idRequerimiento As Integer) As System.Collections.Generic.List(Of BussinessEntities.DetalleRequerimiento) Implements IDetalleRequerimientoRepository.GetAllFromDetalleRequerimientoOrdenadoByIdReq
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetAllFromDetalleRequerimientoOrdenadoByIdReq")
        command.Parameters.AddWithValue("IdRequerimiento", idRequerimiento)
        Return SelectObjectFactoryDetalleRequerimiento(command)
    End Function
    Public Function GetDetalleRequerimientoByIdDetalleReq(ByVal idDetalleRequerimiento As Integer) As BussinessEntities.DetalleRequerimiento Implements IDetalleRequerimientoRepository.GetDetalleRequerimientoByIdDetalleReq
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetDetalleRequerimientoByIdDetalleRequerimiento")
        command.Parameters.AddWithValue("IdDetalleRequerimiento", idDetalleRequerimiento)
        Return SelectObjectFactoryDetalleRequerimiento(command).SingleOrDefault
    End Function

    Public Sub SaveDetalleRequerimiento(ByVal detalleRequerimiento As BussinessEntities.DetalleRequerimiento) Implements IDetalleRequerimientoRepository.SaveDetalleRequerimiento
        Dim command As SqlCommand = MyBase.CreateSPCommand("SaveDETALLEREQUERIMIENTO")
        command.Parameters.AddWithValue("IdRequerimiento", detalleRequerimiento.IdRequerimiento)
        command.Parameters.AddWithValue("IdCatalogo", detalleRequerimiento.IdCatalogo)
        command.Parameters.AddWithValue("IdCentroCosto", detalleRequerimiento.IdCentroCosto)
        MyBase.ExecuteNonQuery(command)
    End Sub

    Public Sub UpdateDetalleRequerimiento(ByVal detalleRequerimiento As BussinessEntities.DetalleRequerimiento) Implements IDetalleRequerimientoRepository.UpdateDetalleRequerimiento
        Dim command As SqlCommand = MyBase.CreateSPCommand("ActualizarDETALLEREQUERIMIENTO")
        command.Parameters.AddWithValue("IdDetalleRequerimiento", detalleRequerimiento.IdDetalleRequerimiento)
        command.Parameters.AddWithValue("IdUnidadMedida", detalleRequerimiento.IdUnidadMedida)
        command.Parameters.AddWithValue("IdCentroCosto", detalleRequerimiento.IdCentroCosto)
        command.Parameters.AddWithValue("Cantidad", detalleRequerimiento.Cantidad)
        command.Parameters.AddWithValue("Afecto", detalleRequerimiento.AfectoIgv)
        command.Parameters.AddWithValue("Observacion", detalleRequerimiento.observacion)
        MyBase.ExecuteNonQuery(command)
    End Sub

    Public Sub UpdateDetalleRequerimiento1(ByVal detalleRequerimiento As System.Collections.Generic.List(Of BussinessEntities.DetalleRequerimiento)) Implements IDetalleRequerimientoRepository.UpdateDetalleRequerimiento1
        For Each det As DetalleRequerimiento In detalleRequerimiento
            UpdateDetalleRequerimiento(det)
        Next
    End Sub

    Public Sub UpdateDetalleReq(ByVal detalleRequerimiento As BussinessEntities.DetalleRequerimiento) Implements IDetalleRequerimientoRepository.UpdateDetalleReq
        Dim command As SqlCommand = MyBase.CreateSPCommand("ActualizarDetalleAprobado")
        command.Parameters.AddWithValue("IdDetalleRequerimiento", detalleRequerimiento.IdDetalleRequerimiento)
        command.Parameters.AddWithValue("Observacion", detalleRequerimiento.observacion)
        command.Parameters.AddWithValue("Afecto", detalleRequerimiento.AfectoIgv)
        MyBase.ExecuteNonQuery(command)
    End Sub

    Public Sub UpdateDetalleReq1(ByVal detalleRequerimiento As System.Collections.Generic.List(Of BussinessEntities.DetalleRequerimiento)) Implements IDetalleRequerimientoRepository.UpdateDetalleReq1
        For Each det As DetalleRequerimiento In detalleRequerimiento
            UpdateDetalleReq(det)
        Next
    End Sub
    Private Function SelectObjectFactoryDetalleRequerimiento(ByVal command As SqlCommand) As List(Of DetalleRequerimiento)
        Dim listaDetalleRequerimiento As New List(Of DetalleRequerimiento)
        Using reader As SqlDataReader = MyBase.ExecuteReader(command)
            While reader.Read()
                Dim detalleRequerimiento As New DetalleRequerimiento()
                With detalleRequerimiento
                    .IdDetalleRequerimiento = reader.GetInt32(0)
                    .IdRequerimiento = reader.GetInt32(1)
                    .IdCatalogo = reader.GetInt32(2)
                    .IdUnidadMedida = reader.GetInt32(3)
                    .Cantidad = reader.GetDecimal(5)
                    .PrecioCompra = reader.GetDecimal(6)
                    .AfectoIgv = reader.GetBoolean(7)
                    .IdCentroCosto = reader.GetInt32(8)
                    .Estado = reader.GetInt32(9)
                End With
                If Not reader.IsDBNull(4) Then
                    detalleRequerimiento.observacion = reader.GetString(4)
                End If
                detalleRequerimiento.cat = catalogoRepository.GetAllFromCatalogoByIdCatalogo(reader.GetInt32(2))
                detalleRequerimiento.unidad = unidadRepository.GetAllFromUnidadMedidaByIdUnidadMedida(reader.GetInt32(3))
                listaDetalleRequerimiento.Add(detalleRequerimiento)
            End While
        End Using
        Return listaDetalleRequerimiento
    End Function
    Public Sub AnularItemDetalle(ByVal idDetalleRequerimiento As Integer, ByVal motivo As String) Implements IDetalleRequerimientoRepository.AnularItemDetalle
        Dim command As SqlCommand = MyBase.CreateSPCommand("AnularItemRequerimiento")
        command.Parameters.AddWithValue("IdDetalleRequerimiento", idDetalleRequerimiento)
        command.Parameters.AddWithValue("Motivo", motivo)
        MyBase.ExecuteNonQuery(command)
    End Sub
End Class
