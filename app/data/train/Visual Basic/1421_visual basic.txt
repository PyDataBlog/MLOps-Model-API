Imports SistFoncreagro.BussinessEntities
Imports System.Data.SqlClient
Imports System.Data
Public Class TareaRepository : Inherits MasterDataAccess : Implements ITareaRepository
    Private Function SelectObjectFactory(ByVal command As SqlCommand) As List(Of Tarea)
        Dim lista As New List(Of Tarea)
        Using reader As SqlDataReader = MyBase.ExecuteReader(command)
            While reader.Read
                Dim _Tarea As New Tarea() With
                    {
                       .IdTarea = reader.GetValue(0),
                       .IdSubActividad = reader.GetValue(1),
                       .NomTarea = reader.GetValue(2),
                       .Unidad = reader.GetValue(3),
                      .FinalizaActividad = reader.GetValue(5)
                    }
                If Not (reader.IsDBNull(reader.GetOrdinal("Porcentaje"))) Then
                    _Tarea.Porcentaje = reader.GetValue(reader.GetOrdinal("Porcentaje"))
                End If
                If Not (reader.IsDBNull(reader.GetOrdinal("Codigo"))) Then
                    _Tarea.Codigo = reader.GetValue(reader.GetOrdinal("Codigo"))
                End If

                lista.Add(_Tarea)
            End While
        End Using
        Return lista
    End Function

    Public Sub DeleteTAREA(IdTarea As Integer) Implements ITareaRepository.DeleteTAREA
        Dim command As SqlCommand = MyBase.CreateSPCommand("DeleteTAREA")
        command.Parameters.AddWithValue("IdTarea", IdTarea)
        MyBase.ExecuteNonQuery(command)
    End Sub

    Public Sub SaveTAREA(_Tarea As BussinessEntities.Tarea) Implements ITareaRepository.SaveTAREA
        Dim command As SqlCommand = MyBase.CreateSPCommand("SaveTAREA")
        command.Parameters.AddWithValue("Codigo", _Tarea.Codigo)
        command.Parameters.AddWithValue("FinalizaActividad", _Tarea.FinalizaActividad)
        command.Parameters.AddWithValue("IdSubActividad", _Tarea.IdSubActividad)
        command.Parameters.AddWithValue("IdTarea", _Tarea.IdTarea)
        command.Parameters.AddWithValue("NomTarea", _Tarea.NomTarea)
        command.Parameters.AddWithValue("Porcentaje", _Tarea.Porcentaje)
        command.Parameters.AddWithValue("Unidad", _Tarea.Unidad)
        MyBase.ExecuteNonQuery(command)
    End Sub
End Class
