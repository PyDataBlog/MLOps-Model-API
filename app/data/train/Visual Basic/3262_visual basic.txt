Imports SistFoncreagro.BussinessEntities
Imports System.Data.SqlClient
Imports System.Data
Public Class CaracteristicaRepository : Inherits MasterDataAccess : Implements ICaracteristicaRepository
    Private Function SelectObjectFactory(ByVal command As SqlCommand) As List(Of Caracteristica)
        Dim lista As New List(Of Caracteristica)

        Using reader As SqlDataReader = MyBase.ExecuteReader(command)
            While reader.Read
                Dim _Caracteristica As New Caracteristica() With
                {
                 .IdCaracteristica = reader.GetValue(0),
                 .Descripcion = reader.GetValue(1),
                 .HabilitarComentario = reader.GetValue(2),
                 .IdFactor = reader.GetValue(3)
                }
               
                lista.Add(_Caracteristica)
            End While
        End Using
        Return lista
    End Function

    Public Sub DeleteCARACTERISTICA(ByVal IdCaracteristica As Integer) Implements ICaracteristicaRepository.DeleteCARACTERISTICA
        Dim command As SqlCommand = MyBase.CreateSPCommand("DeleteCARACTERISTICA")
        command.Parameters.AddWithValue("IdCaracteristica", IdCaracteristica)
        MyBase.ExecuteNonQuery(command)
    End Sub

    Public Function GetCARACTERISTICAByIdCaracteristica(ByVal IdCaracteristica As Integer) As BussinessEntities.Caracteristica Implements ICaracteristicaRepository.GetCARACTERISTICAByIdCaracteristica
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetCARACTERISTICAByIdCaracteristica")
        command.Parameters.AddWithValue("IdCaracteristica", IdCaracteristica)
        Return SelectObjectFactory(command).SingleOrDefault
    End Function

    Public Function GetCARACTERISTICAByIdFactor(ByVal IdFactor As Integer) As System.Collections.Generic.List(Of BussinessEntities.Caracteristica) Implements ICaracteristicaRepository.GetCARACTERISTICAByIdFactor
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetAllFromBANCO")
        command.Parameters.AddWithValue("IdFactor", IdFactor)
        Return SelectObjectFactory(command)
    End Function

    Public Sub SaveCARACTERISTICA(ByVal _Caracteristica As BussinessEntities.Caracteristica) Implements ICaracteristicaRepository.SaveCARACTERISTICA
        Dim command As SqlCommand = MyBase.CreateSPCommand("SaveCARACTERISTICA")
        command.Parameters.AddWithValue("Descripcion", _Caracteristica.Descripcion)
        command.Parameters.AddWithValue("HabilitarComentario", _Caracteristica.HabilitarComentario)
        command.Parameters.AddWithValue("IdCaracteristica", _Caracteristica.IdCaracteristica)
        command.Parameters.AddWithValue("IdFactor", _Caracteristica.IdFactor)
        MyBase.ExecuteReader(command)
    End Sub
End Class
