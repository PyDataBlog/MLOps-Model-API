Imports SistFoncreagro.BussinessEntities
Imports System.Data.SqlClient
Imports System.Data
Public Class HorarioAsignadoRepository : Inherits MasterDataAccess : Implements IHorarioAsignadoRepository
    Public Function GetHorarioAsignadoByIdPeriodoLaboralAndByFecha(ByVal IdPeriodoLaboral As Integer, ByVal Fecha As Date) As BussinessEntities.HorarioAsignado Implements IHorarioAsignadoRepository.GetHorarioAsignadoByIdPeriodoLaboralAndByFecha
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetHorarioAsignadoByIdPeriodoLaboralAndByFecha")
        command.Parameters.AddWithValue("IdPeriodoLaboral", IdPeriodoLaboral)
        command.Parameters.AddWithValue("Fecha", Fecha)
        Return SelectObjectFactory(command).SingleOrDefault
    End Function
    Public Function GetHorarioAsignadoByIdPeriodoLaboral(ByVal IdPeriodoLaboral As Integer) As System.Collections.Generic.List(Of BussinessEntities.HorarioAsignado) Implements IHorarioAsignadoRepository.GetHorarioAsignadoByIdPeriodoLaboral
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetHorarioAsignadoByIdPeriodoLaboral")
        command.Parameters.AddWithValue("IdPeriodoLaboral", IdPeriodoLaboral)
        Return SelectObjectFactory(command)
    End Function

    Public Function SaveHORARIOASIGNADO(ByVal HorarioAsignado As BussinessEntities.HorarioAsignado) As Integer Implements IHorarioAsignadoRepository.SaveHORARIOASIGNADO
        Dim command As SqlCommand = MyBase.CreateSPCommand("SaveHORARIOASIGNADO")
        command.Parameters.AddWithValue("IdAsignacionHorario", HorarioAsignado.IdAsignacionHorario)
        command.Parameters.AddWithValue("IdPeriodoLaboral", HorarioAsignado.IdPeriodoLaboral)
        command.Parameters.AddWithValue("IdHorario", HorarioAsignado.IdHorario)
        command.Parameters.AddWithValue("IdJornadaTrabajo", HorarioAsignado.IdJornadaTrabajo)
        command.Parameters.AddWithValue("FechaInicio", HorarioAsignado.FechaInicio)
        command.Parameters.AddWithValue("FechaFin", HorarioAsignado.FechaFin)
        'command.Parameters("idCargo").Direction = ParameterDirection.Output

        'MyBase.ExecuteNonQuery(command)
        Dim i As Int32
        Using reader As SqlDataReader = MyBase.ExecuteReader(command)
            If reader.Read Then
                If Not reader.IsDBNull(0) Then
                    i = reader.GetValue(0)
                End If
            Else
                i = 0 'command.Parameters("idCargo").Value
            End If
        End Using


        Return i
        'command.ExecuteReader(CommandBehavior.SingleRow).Item(0)
    End Function


    Private Function SelectObjectFactory(ByVal command As SqlCommand) As List(Of HorarioAsignado)
        Dim lista As New List(Of HorarioAsignado)
        Using reader As SqlDataReader = MyBase.ExecuteReader(command)
            While reader.Read
                Dim entity As New HorarioAsignado
                If Not reader.IsDBNull(reader.GetOrdinal("FechaFin")) Then
                    entity.FechaFin = reader.GetValue(reader.GetOrdinal("FechaFin"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("FechaInicio")) Then
                    entity.FechaInicio = reader.GetValue(reader.GetOrdinal("FechaInicio"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("IdAsignacionHorario")) Then
                    entity.IdAsignacionHorario = reader.GetValue(reader.GetOrdinal("IdAsignacionHorario"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("IdJornadaTrabajo")) Then
                    entity.IdJornadaTrabajo = reader.GetValue(reader.GetOrdinal("IdJornadaTrabajo"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("IdPeriodoLaboral")) Then
                    entity.IdPeriodoLaboral = reader.GetValue(reader.GetOrdinal("IdPeriodoLaboral"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("IdHorario")) Then
                    entity.IdHorario = reader.GetValue(reader.GetOrdinal("IdHorario"))
                End If

                If entity.IdHorario.HasValue And entity.IdJornadaTrabajo.HasValue Then
                    Dim HorarioRepository As New HorarioRepository
                    entity.HorarioEntity = HorarioRepository.GetHorarioByIdHorario(entity.IdHorario, entity.IdJornadaTrabajo)
                End If

                lista.Add(entity)
            End While
        End Using
        'El indice depende de la columna con la que este en el SP
        Return lista
    End Function


End Class
