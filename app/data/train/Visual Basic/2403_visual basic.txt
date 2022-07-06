Imports SistFoncreagro.BussinessEntities
Imports System.Data.SqlClient
Imports System.Data
Public Class ResponsableRepository : Inherits MasterDataAccess : Implements IResponsableRepository
    Private Function SelectObjectFactory(ByVal command As SqlCommand) As List(Of Responsable)
        Dim lista As New List(Of Responsable)

        Using reader As SqlDataReader = MyBase.ExecuteReader(command)
            While reader.Read
                Dim _Responsable As New Responsable() With
                {
                    .IdResponsable = reader.GetValue(0),
                    .IdPersonal = reader.GetValue(1),
                    .IdControl = reader.GetValue(3)
                }
                If Not reader.IsDBNull(2) Then
                    _Responsable.Observaciones = reader.GetValue(2)
                End If
                lista.Add(_Responsable)
            End While
        End Using
        Return lista
    End Function

    Public Sub DeleteRESPONSABLE(ByVal IdResponsable As Integer) Implements IResponsableRepository.DeleteRESPONSABLE
        Dim command As SqlCommand = MyBase.CreateSPCommand("DeleteRESPONSABLE")
        command.Parameters.AddWithValue("IdResponsable", IdResponsable)
        MyBase.ExecuteNonQuery(command)
    End Sub

    Public Function GetRESPONSABLEByIdControl(ByVal IdControl As Integer) As System.Collections.Generic.List(Of BussinessEntities.Responsable) Implements IResponsableRepository.GetRESPONSABLEByIdControl
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetRESPONSABLEByIdControl")
        command.Parameters.AddWithValue("IdControl", IdControl)
        Return SelectObjectFactory(command)
    End Function

    Public Function GetRESPONSABLEByIdResponsable(ByVal IdResponsable As Integer) As BussinessEntities.Responsable Implements IResponsableRepository.GetRESPONSABLEByIdResponsable
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetRESPONSABLEByIdResponsable")
        command.Parameters.AddWithValue("IdResponsable", IdResponsable)
        Return SelectObjectFactory(command).SingleOrDefault
    End Function

    Public Sub SaveRESPONSABLE(ByVal _Responsable As BussinessEntities.Responsable) Implements IResponsableRepository.SaveRESPONSABLE
        Dim command As SqlCommand = MyBase.CreateSPCommand("SaveRESPONSABLE")
        command.Parameters.AddWithValue("IdControl", _Responsable.IdControl)
        command.Parameters.AddWithValue("IdPersonal", _Responsable.IdPersonal)
        command.Parameters.AddWithValue("IdResponsable", _Responsable.IdResponsable)
        command.Parameters.AddWithValue("Observaciones", _Responsable.Observaciones)
        MyBase.ExecuteNonQuery(command)
    End Sub
End Class
