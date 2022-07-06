Imports SistFoncreagro.BussinessEntities
Imports System.Data.SqlClient
Imports System.Data
Public Class JornadaLaboralRepository : Inherits MasterDataAccess : Implements IJornadaLaboralRepository

    Public Function GetDiasDescansoEnControlAsistenciaByRangoFechas(ByVal IdPeriodoLaboral As Integer, ByVal FI As Date, ByVal FF As Date) As Decimal Implements IJornadaLaboralRepository.GetDiasDescansoEnControlAsistenciaByRangoFechas
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetDiasDescansoEnControlAsistenciaByRangoFechas")
        command.Parameters.AddWithValue("IdPeriodoLaboral", IdPeriodoLaboral)
        command.Parameters.AddWithValue("FI", FI)
        command.Parameters.AddWithValue("FF", FF)
        Return command.ExecuteScalar()

        'Dim i As Int32
        'Using reader As SqlDataReader = MyBase.ExecuteReader(command)
        '    If reader.Read Then
        '        If Not reader.IsDBNull(0) Then
        '            i = reader.GetValue(0)
        '        End If
        '    Else
        '        i = 0 'command.Parameters("idCargo").Value
        '    End If
        'End Using


        'Return i
    End Function

    Public Function GetDiasFeriadosEnControlAsistenciaByRangoFechas(ByVal IdPeriodoLaboral As Integer, ByVal FI As Date, ByVal FF As Date) As Decimal Implements IJornadaLaboralRepository.GetDiasFeriadosEnControlAsistenciaByRangoFechas
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetDiasFeriadosEnControlAsistenciaByRangoFechas")
        command.Parameters.AddWithValue("IdPeriodoLaboral", IdPeriodoLaboral)
        command.Parameters.AddWithValue("FI", FI)
        command.Parameters.AddWithValue("FF", FF)
        Return command.ExecuteScalar()
    End Function

    Public Function GetDiasNoLaboradosNoSubsidiadosByIdJornadaLaboral(ByVal IdJornalaLaboral As Integer) As Decimal Implements IJornadaLaboralRepository.GetDiasNoLaboradosNoSubsidiadosByIdJornadaLaboral
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetDiasNoLaboradosNoSubsidiadosByIdJornadaLaboral")
        command.Parameters.AddWithValue("IdJornalaLaboral", IdJornalaLaboral)
        Return command.ExecuteScalar()
    End Function

    Public Function GetDiasNoLaboradosSubsidiadosByIdJornadaLaboral(ByVal IdJornalaLaboral As Integer) As Decimal Implements IJornadaLaboralRepository.GetDiasNoLaboradosSubsidiadosByIdJornadaLaboral
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetDiasNoLaboradosSubsidiadosByIdJornadaLaboral")
        command.Parameters.AddWithValue("IdJornalaLaboral", IdJornalaLaboral)
        Return command.ExecuteScalar()
    End Function

    Public Function GetJornadaLaboralPeriodoByIdPeriodoLaboralByIdPeriodoDeclaracion(ByVal IdPeriodoLaboral As Integer, ByVal IdPeriodoDeclaracion As Integer) As BussinessEntities.JornadaLaboralPeriodo Implements IJornadaLaboralRepository.GetJornadaLaboralPeriodoByIdPeriodoLaboralByIdPeriodoDeclaracion
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetJornadaLaboralPeriodoByIdPeriodoLaboralByIdPeriodoDeclaracion")
        command.Parameters.AddWithValue("IdPeriodoLaboral", IdPeriodoLaboral)
        command.Parameters.AddWithValue("IdPeriodoDeclaracion", IdPeriodoDeclaracion)
        Return command.ExecuteScalar()
    End Function

    Public Function GetSumHExtra25ControlAsistByFechasByIdPeriodoLaboral(ByVal IdPeriodoLaboral As Integer, ByVal MinHE25 As Decimal, ByVal MaxHE25 As Decimal, ByVal FI As Date, ByVal FF As Date) As Decimal Implements IJornadaLaboralRepository.GetSumHExtra25ControlAsistByFechasByIdPeriodoLaboral
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetSumHExtra25ControlAsistByFechasByIdPeriodoLaboral")
        command.Parameters.AddWithValue("IdPeriodoLaboral", IdPeriodoLaboral)
        command.Parameters.AddWithValue("MinHE25", MinHE25)
        command.Parameters.AddWithValue("MaxHE25", MaxHE25)
        command.Parameters.AddWithValue("FI", FI)
        command.Parameters.AddWithValue("FF", FF)
        Return command.ExecuteScalar()
    End Function

    Public Function GetSumHExtra35ControlAsistByFechasByIdPeriodoLaboral(ByVal IdPeriodoLaboral As Integer, ByVal MaxHE25 As Decimal, ByVal FI As Date, ByVal FF As Date) As Decimal Implements IJornadaLaboralRepository.GetSumHExtra35ControlAsistByFechasByIdPeriodoLaboral
        Dim command As SqlCommand = MyBase.CreateSPCommand("GetSumHExtra35ControlAsistByFechasByIdPeriodoLaboral")
        command.Parameters.AddWithValue("IdPeriodoLaboral", IdPeriodoLaboral)
        command.Parameters.AddWithValue("MaxHE25", MaxHE25)
        command.Parameters.AddWithValue("FI", FI)
        command.Parameters.AddWithValue("FF", FF)
        Return command.ExecuteScalar()
    End Function

    Public Function SaveJORNADALABORALPERIODO(ByVal JornadaLaboralPeriodo As BussinessEntities.JornadaLaboralPeriodo) As Integer Implements IJornadaLaboralRepository.SaveJORNADALABORALPERIODO
        Dim command As SqlCommand = MyBase.CreateSPCommand("SaveJORNADALABORALPERIODO")
        command.Parameters.AddWithValue("IdJornadaLaboral", JornadaLaboralPeriodo.IdJornadaLaboral)
        command.Parameters.AddWithValue("IdPeriodoDeclaracion", JornadaLaboralPeriodo.IdPeriodoDeclaracion)
        command.Parameters.AddWithValue("IdPeriodoLaboral", JornadaLaboralPeriodo.IdPeriodoLaboral)
        command.Parameters.AddWithValue("DiasEfecTrabajados", JornadaLaboralPeriodo.DiasEfecTrabajados)
        command.Parameters.AddWithValue("DiasDescanso", JornadaLaboralPeriodo.DiasDescanso)
        command.Parameters.AddWithValue("DiasFeriados", JornadaLaboralPeriodo.DiasFeriados)
        command.Parameters.AddWithValue("HorasOrdinarias", JornadaLaboralPeriodo.HorasOrdinarias)
        command.Parameters.AddWithValue("HorasExtraordinarias25", JornadaLaboralPeriodo.HorasExtraordinarias25)
        command.Parameters.AddWithValue("HorasExtraordinarias35", JornadaLaboralPeriodo.HorasExtraordinarias35)
        command.Parameters.AddWithValue("DiasNoSubsidiados", JornadaLaboralPeriodo.DiasNoSubsidiados)
        command.Parameters.AddWithValue("DiasSubsidiados", JornadaLaboralPeriodo.DiasSubsidiados)

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

    Private Function SelectObjectFactory(ByVal command As SqlCommand) As List(Of JornadaLaboralPeriodo)
        Dim lista As New List(Of JornadaLaboralPeriodo)
        Dim ausenciasPeriodoBL As New AusenciaPeriodoRepository
        Using reader As SqlDataReader = MyBase.ExecuteReader(command)
            While reader.Read
                Dim entity As New JornadaLaboralPeriodo
                If Not reader.IsDBNull(reader.GetOrdinal("IdJornadaLaboral")) Then
                    entity.IdJornadaLaboral = reader.GetValue(reader.GetOrdinal("IdJornadaLaboral"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("IdPeriodoDeclaracion")) Then
                    entity.IdPeriodoDeclaracion = reader.GetValue(reader.GetOrdinal("IdPeriodoDeclaracion"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("IdPeriodoLaboral")) Then
                    entity.IdPeriodoLaboral = reader.GetValue(reader.GetOrdinal("IdPeriodoLaboral"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("DiasEfecTrabajados")) Then
                    entity.DiasEfecTrabajados = reader.GetValue(reader.GetOrdinal("DiasEfecTrabajados"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("DiasDescanso")) Then
                    entity.DiasDescanso = reader.GetValue(reader.GetOrdinal("DiasDescanso"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("DiasFeriados")) Then
                    entity.DiasFeriados = reader.GetValue(reader.GetOrdinal("DiasFeriados"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("HorasOrdinarias")) Then
                    entity.HorasOrdinarias = reader.GetValue(reader.GetOrdinal("HorasOrdinarias"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("HorasExtraordinarias25")) Then
                    entity.HorasExtraordinarias25 = reader.GetValue(reader.GetOrdinal("HorasExtraordinarias25"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("HorasExtraordinarias35")) Then
                    entity.HorasExtraordinarias35 = reader.GetValue(reader.GetOrdinal("HorasExtraordinarias35"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("DiasNoSubsidiados")) Then
                    entity.DiasNoSubsidiados = reader.GetValue(reader.GetOrdinal("DiasNoSubsidiados"))
                End If
                If Not reader.IsDBNull(reader.GetOrdinal("DiasSubsidiados")) Then
                    entity.DiasSubsidiados = reader.GetValue(reader.GetOrdinal("DiasSubsidiados"))
                End If

                entity.listaAusenciaEntity = ausenciasPeriodoBL.GetAusenciasPeriodoByIdJornadaLaboral(entity.IdJornadaLaboral)

                lista.Add(entity)
            End While
        End Using
        'El indice depende de la columna con la que este en el SP
        Return lista
        '
    End Function


End Class
