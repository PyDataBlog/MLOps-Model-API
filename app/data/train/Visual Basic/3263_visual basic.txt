Imports SistFoncreagro.BussinessEntities
Imports System.Data.SqlClient
Imports System.Data
Public Class TipoCtaBancariaRepository : Inherits MasterDataAccess : Implements ITipoCtaBancariaRepository
    Private Function SelectObjectFactory(ByVal command As SqlCommand) As List(Of TipoCtaBancaria)
        Dim lista As New List(Of TipoCtaBancaria)

        Using reader As SqlDataReader = MyBase.ExecuteReader(command)
            While reader.Read
                Dim _TipoCtaBancaria As New TipoCtaBancaria() With
                {
                  .Descripcion = reader.GetValue(0),
                  .IdTipoCtaBancaria = reader.GetValue(1)
                }

                lista.Add(_TipoCtaBancaria)
            End While
        End Using
        Return lista
    End Function
    Public Function GetAllFromTIPOCTABANCARIA() As System.Collections.Generic.List(Of BussinessEntities.TipoCtaBancaria) Implements ITipoCtaBancariaRepository.GetAllFromTIPOCTABANCARIA
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetAllFromTIPOCTABANCARIA")
        Return SelectObjectFactory(command)
    End Function

    Public Function GetTIPOCTABANCARIAByIdTipoCtaBancaria(ByVal IdTipoCtaBancaria As Integer) As BussinessEntities.TipoCtaBancaria Implements ITipoCtaBancariaRepository.GetTIPOCTABANCARIAByIdTipoCtaBancaria
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetTIPOCTABANCARIAByIdTipoCtaBancaria")
        command.Parameters.AddWithValue("IdTipoCtaBancaria", IdTipoCtaBancaria)
        Return SelectObjectFactory(command).SingleOrDefault
    End Function
End Class
