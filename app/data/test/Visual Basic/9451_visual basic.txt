Imports SistFoncreagro.BussinessEntities
Imports System.Data.SqlClient
Imports System.Data

Partial Public Class CCostoRepository : Inherits MasterDataAccess : Implements ICCostoRepository

    Public Function GetAllFromCCosto() As System.Collections.Generic.List(Of BussinessEntities.CCosto) Implements ICCostoRepository.GetAllFromCCosto
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetAllFromCCOSTO")
        Return SelectObjectFactory(command)
    End Function

    Private Function SelectObjectFactory(ByVal command As SqlCommand) As List(Of CCosto)
        Dim lista As New List(Of CCosto)
        Using reader As SqlDataReader = MyBase.ExecuteReader(command)

            While reader.Read
                Dim _CCosto As New CCosto() With
                {
                    .IdCCosto = reader.GetValue(reader.GetOrdinal("IdCCosto")),
                    .IdElemGasto = reader.GetValue(reader.GetOrdinal("IdElemGasto")),
                    .IdProyAct = reader.GetValue(reader.GetOrdinal("IdProyAct")),
                    .Unidad = reader.GetValue(reader.GetOrdinal("Unidad"))
                }
                If Not (reader.IsDBNull(reader.GetOrdinal("Codigo"))) Then
                    _CCosto.Codigo = reader.GetValue(reader.GetOrdinal("Codigo"))
                End If
                If Not (reader.IsDBNull(reader.GetOrdinal("Porcentaje"))) Then
                    _CCosto.Porcentaje = reader.GetValue(reader.GetOrdinal("Porcentaje"))
                End If

                Dim _miElementoGasto As New ElementoGastoRepository
                _CCosto._ElementoGasto = _miElementoGasto.GetFromElementoGastoById(_CCosto.IdElemGasto)

                Dim _miProyAct As New ProyActRepository
                _CCosto._ProyAct = _miProyAct.GetProyActById(_CCosto.IdProyAct)

                Dim _miActividad As New ActividadRepository
                _CCosto._Actividad = _miActividad.GetFromActividadById(_CCosto._ProyAct.IdActividad)

                Dim _miProyComp As New ProyCompRepository
                _CCosto._ProyComp = _miProyComp.GetProyCompById(_CCosto._ProyAct.IdProycomp)

                Dim _miComponente As New ComponenteRepository
                _CCosto._Componente = _miComponente.GetFromComponenteById(_CCosto._ProyComp.IdComponente)

                Dim _miConvProy As New ConveProyRepository
                _CCosto._ConvProy = _miConvProy.GetFromConveProyById(_CCosto._ProyComp.IdConvProy)

                Dim _miConvenio As New ConvenioRepository
                _CCosto._Convenio = _miConvenio.GetFromConvenioByIdConvenio(_CCosto._ConvProy.IdConvenio)

                Dim _miProyecto As New ProyectoRepository
                _CCosto._Proyecto = _miProyecto.GetFromProyectoByIdProyecto(_CCosto._ConvProy.IdProyecto)

                lista.Add(_CCosto)
            End While
        End Using
        Return lista
    End Function

    Public Function GetCCOSTOByIdCCosto(ByVal _IdCCosto As Integer) As BussinessEntities.CCosto Implements ICCostoRepository.GetCCOSTOByIdCCosto
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetCCOSTOByIdCCosto")
        command.Parameters.AddWithValue("IdCCosto", _IdCCosto)
        Return SelectObjectFactory(command).SingleOrDefault
    End Function

    Public Function GetCCOSTOByIdProyecto(ByVal _IdProyecto As Integer) As System.Collections.Generic.List(Of BussinessEntities.CCosto) Implements ICCostoRepository.GetCCOSTOByIdProyecto
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetCCOSTOByIdProyecto")
        command.Parameters.AddWithValue("IdProyecto", _IdProyecto)
        Return SelectObjectFactory(command)
    End Function

    Public Function DeleteCCostos(ByVal _id As Integer) As String Implements ICCostoRepository.DeleteCCostos
        Dim command As SqlCommand = MyBase.CreateSPCommand("DeleteCCOSTO")
        command.Parameters.AddWithValue("idCCosto", _id)
        command.Parameters.AddWithValue("miError", "abc")
        Dim mMensaje As String
        mMensaje = ""
        Using reader As SqlDataReader = MyBase.ExecuteReader(command)
            If reader.Read Then
                mMensaje = reader.GetValue(0)
            End If
        End Using
        Return mMensaje
    End Function

    Public Function SaveCCosto(ByVal _cCosto As BussinessEntities.CCosto) As String Implements ICCostoRepository.SaveCCosto
        Dim command As SqlCommand = MyBase.CreateSPCommand("SaveCCOSTO")
        Dim msj As String = "abc"

        command.Parameters.AddWithValue("IdCCosto", _cCosto.IdCCosto)
        command.Parameters.AddWithValue("IdProyAct", _cCosto.IdProyAct)
        command.Parameters.AddWithValue("IdElemGasto", _cCosto.IdElemGasto)
        command.Parameters.AddWithValue("Unidad", _cCosto.Unidad)
        command.Parameters.AddWithValue("Porcentaje", _cCosto.Porcentaje)
        command.Parameters.AddWithValue("Error", "abc")

        Using reader As SqlDataReader = MyBase.ExecuteReader(command)
            If reader.Read Then
                msj = reader.GetValue(0)
            End If
        End Using
        Return msj
    End Function

    Public Function GetCCOSTOByIdOrdenCompra(ByVal IdOrden As Integer) As System.Collections.Generic.List(Of BussinessEntities.CCosto) Implements ICCostoRepository.GetCCOSTOByIdOrdenCompra
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetCCOSTOByIdOrdenCompra")
        command.Parameters.AddWithValue("IdOrdenCompra", IdOrden)
        Return SelectObjectFactoryOc(command)
    End Function
    Private Function SelectObjectFactoryOc(ByVal command As SqlCommand) As List(Of CCosto)
        Dim lista As New List(Of CCosto)
        Using reader As SqlDataReader = MyBase.ExecuteReader(command)

            While reader.Read
                Dim _CCosto As New CCosto() With
                {
                    .IdCCosto = reader.GetValue(reader.GetOrdinal("IdCCosto")),
                    .IdElemGasto = reader.GetValue(reader.GetOrdinal("IdElemGasto")),
                    .IdProyAct = reader.GetValue(reader.GetOrdinal("IdProyAct")),
                    .Unidad = reader.GetValue(reader.GetOrdinal("Unidad")),
                    .IdOrdenCompra = reader.GetValue(reader.GetOrdinal("IdOrdenCompra"))
                }
                If Not (reader.IsDBNull(reader.GetOrdinal("Codigo"))) Then
                    _CCosto.Codigo = reader.GetValue(reader.GetOrdinal("Codigo"))
                End If
                If Not (reader.IsDBNull(reader.GetOrdinal("TotalCCosto"))) Then
                    _CCosto.Porcentaje = reader.GetValue(reader.GetOrdinal("TotalCCosto"))
                End If
                Dim _miElementoGasto As New ElementoGastoRepository
                _CCosto._ElementoGasto = _miElementoGasto.GetFromElementoGastoById(_CCosto.IdElemGasto)
                lista.Add(_CCosto)
            End While
        End Using
        Return lista
    End Function
End Class
