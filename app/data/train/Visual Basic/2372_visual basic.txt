Public Class Horario
    Public Property IdHorario As System.Nullable(Of Int32)
    Public Property ciclo As String
    Public Property Descripcion As String
    Public Property IdJornadaDeTrabajo As System.Nullable(Of Int32)
    Public Property DiaIniDescanso As System.Nullable(Of Int32)
    Public Property TipoTurno As String

    Public Property ListaTurnosRegulares As List(Of TurnosRegulares)

    Private Property DescTurnos As String = ""

    Public ReadOnly Property DescripcionCompleta As String
        Get
            For Each turno As TurnosRegulares In ListaTurnosRegulares
                DescTurnos = DescTurnos & turno.HoraInicio & "-" & turno.HoraFin
            Next
            Return Descripcion & " - " & DescTurnos
        End Get
    End Property

    Public ReadOnly Property ClavesPrimarias As String
        Get
            Return IdHorario & "/" & IdJornadaDeTrabajo
        End Get
    End Property
End Class
