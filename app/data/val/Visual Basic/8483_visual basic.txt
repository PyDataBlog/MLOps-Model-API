Imports SistFoncreagro.BussinessEntities
Imports System.Data.SqlClient
Imports System.Data
Public Class RecibosRendicionRepository : Inherits MasterDataAccess : Implements IRecibosRendicionRepository
    Private Function SelectObjectFactory(ByVal command As SqlCommand) As List(Of RecibosRendicion)
        Dim lista As New List(Of RecibosRendicion)

        Using reader As SqlDataReader = MyBase.ExecuteReader(command)
            While reader.Read
                Dim _ReciboRendicion As New RecibosRendicion() With
                {
                    .IdReciboRendicion = reader.GetValue(1),
                    .IdRendicion = reader.GetValue(2)
                }
                If Not (reader.IsDBNull(0)) Then
                    _ReciboRendicion.IdRecibo = reader.GetValue(0)
                End If
                
                lista.Add(_ReciboRendicion)
            End While
        End Using
        Return lista
    End Function
    Public Sub DeleteRECIBOSRENDICION(ByVal IdReciboRendicion As Integer) Implements IRecibosRendicionRepository.DeleteRECIBOSRENDICION
        Dim command As SqlCommand = MyBase.CreateSPCommand("DeleteRECIBOSRENDICION")
        command.Parameters.AddWithValue("IdReciboRendicion", IdReciboRendicion)
        MyBase.ExecuteNonQuery(command)
    End Sub

    Public Function GetAllFromRECIBOSRENDICION() As System.Collections.Generic.List(Of BussinessEntities.RecibosRendicion) Implements IRecibosRendicionRepository.GetAllFromRECIBOSRENDICION
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetAllFromRECIBOSRENDICION")
        Return SelectObjectFactory(command)
    End Function

    Public Function GetRECIBOSRENDICIONByIdReciboRendicion(ByVal IdReciboRendicion As Integer) As BussinessEntities.RecibosRendicion Implements IRecibosRendicionRepository.GetRECIBOSRENDICIONByIdReciboRendicion
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetRECIBOSRENDICIONByIdReciboRendicion")
        command.Parameters.AddWithValue("IdReciboRendicion", IdReciboRendicion)
        Return SelectObjectFactory(command).SingleOrDefault
    End Function

    Public Function GetRECIBOSRENDICIONByIdRendicion(ByVal IdRendicion As Integer) As System.Collections.Generic.List(Of BussinessEntities.RecibosRendicion) Implements IRecibosRendicionRepository.GetRECIBOSRENDICIONByIdRendicion
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetRECIBOSRENDICIONByIdRendicion")
        command.Parameters.AddWithValue("IdRendicion", IdRendicion)
        Return SelectObjectFactory(command)
    End Function

    Public Sub SaveRECIBOSRENDICION(ByVal _Recibosrendicion As BussinessEntities.RecibosRendicion) Implements IRecibosRendicionRepository.SaveRECIBOSRENDICION
        Dim command As SqlCommand = MyBase.CreateSPCommand("SaveRECIBOSRENDICION")
        command.Parameters.AddWithValue("IdRecibo", _Recibosrendicion.IdRecibo)
        command.Parameters.AddWithValue("IdRendicion", _Recibosrendicion.IdRendicion)
        MyBase.ExecuteNonQuery(command)
    End Sub

End Class
