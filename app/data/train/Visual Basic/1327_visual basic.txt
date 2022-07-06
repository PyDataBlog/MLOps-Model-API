Imports SistFoncreagro.BussinessEntities
Imports System.Data.SqlClient
Imports System.Data

Public Class ActivarPresupuestoRepository : Inherits MasterDataAccess : Implements IActivarPresupuestoRepository

    Private Function SelectObjectFactory(ByVal command As SqlCommand) As List(Of ActivarPresupuesto)
        Dim lista As New List(Of ActivarPresupuesto)
        Using reader As SqlDataReader = MyBase.ExecuteReader(command)
            While reader.Read
                Dim _ActivarPresupuesto As New ActivarPresupuesto() With
                    {
                        .IdActPresup = reader.GetValue(reader.GetOrdinal("IdActPresup"))
                    }
                If Not (reader.IsDBNull(reader.GetOrdinal("Activar"))) Then
                    _ActivarPresupuesto.Activar = reader.GetValue(reader.GetOrdinal("Activar"))
                End If
                If Not (reader.IsDBNull(reader.GetOrdinal("IdEjerPresup"))) Then
                    _ActivarPresupuesto.IdEjerPresup = reader.GetValue(reader.GetOrdinal("IdEjerPresup"))
                End If
                If Not (reader.IsDBNull(reader.GetOrdinal("IdProyecto"))) Then
                    _ActivarPresupuesto.IdProyecto = reader.GetValue(reader.GetOrdinal("IdProyecto"))
                End If
                Dim _miProyecto As New ProyectoRepository
                _ActivarPresupuesto._Proyecto = _miProyecto.GetFromProyectoByIdProyecto(_ActivarPresupuesto.IdProyecto)
                lista.Add(_ActivarPresupuesto)
            End While
        End Using
        Return lista
    End Function

    Public Function GetACTIVARPRESUPByIdEjerPresup(ByVal _IdEjerPresup As Integer) As System.Collections.Generic.List(Of BussinessEntities.ActivarPresupuesto) Implements IActivarPresupuestoRepository.GetACTIVARPRESUPByIdEjerPresup
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetACTIVARPRESUPByIdEjerPresup")
        command.Parameters.AddWithValue("IdEjerPresup", _IdEjerPresup)

        Return SelectObjectFactory(command)
    End Function

    Public Sub NueActivarPresup(ByVal _IdEjerPresup As Integer) Implements IActivarPresupuestoRepository.NueActivarPresup
        Dim command As SqlCommand = MyBase.CreateSPCommand("NueActivarPresup")

        command.Parameters.AddWithValue("IdEjerPresup", _IdEjerPresup)

        MyBase.ExecuteNonQuery(command)
    End Sub

    Public Sub UpdateActivarPresup(ByVal _ActivarPresup As BussinessEntities.ActivarPresupuesto) Implements IActivarPresupuestoRepository.UpdateActivarPresup
        Dim command As SqlCommand = MyBase.CreateSPCommand("UpdateACTIVARPRESUPUESTO")

        command.Parameters.AddWithValue("IdActPresup", _ActivarPresup.IdActPresup)
        command.Parameters.AddWithValue("Activar", _ActivarPresup.Activar)

        MyBase.ExecuteNonQuery(command)
    End Sub

    Public Sub UpdateActivarPresup1(ByVal _ActivarPresup As System.Collections.Generic.List(Of BussinessEntities.ActivarPresupuesto)) Implements IActivarPresupuestoRepository.UpdateActivarPresup1
        For Each det As ActivarPresupuesto In _ActivarPresup
            UpdateActivarPresup(det)
        Next
    End Sub

    Public Function GetACTIVARPRESUPByIdProyAndIdEjerPresup(ByVal _IdEjerPresup As Integer, ByVal _IdProyecto As Integer) As BussinessEntities.ActivarPresupuesto Implements IActivarPresupuestoRepository.GetACTIVARPRESUPByIdProyAndIdEjerPresup
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetACTIVARPRESUPByIdProyAndIdEjerPresup")
        command.Parameters.AddWithValue("IdEjerPresup", _IdEjerPresup)
        command.Parameters.AddWithValue("IdProyecto", _IdProyecto)
        Return SelectObjectFactory(command).SingleOrDefault
    End Function
End Class
