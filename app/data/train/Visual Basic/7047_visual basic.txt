Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.DataAccess

Public Class TurnosRegularesBL : Implements ITurnosRegularesBL

    Dim factoryrepository As ITurnosRegularesRepository

    Public Sub New()
        factoryrepository = New TurnosRegularesRepository
    End Sub

    Public Function GetAllFromTURNOSREGULARES() As System.Collections.Generic.List(Of BussinessEntities.TurnosRegulares) Implements ITurnosRegularesBL.GetAllFromTURNOSREGULARES
        Return factoryrepository.GetAllFromTURNOSREGULARES
    End Function

    Public Function GetTurnosRegularesByIdHorarioIdJornadaTrabajo(ByVal IdHorario As Integer, ByVal IdJornadaTrabajo As Integer) As System.Collections.Generic.List(Of BussinessEntities.TurnosRegulares) Implements ITurnosRegularesBL.GetTurnosRegularesByIdHorarioIdJornadaTrabajo
        Return GetTurnosRegularesByIdHorarioIdJornadaTrabajo(IdHorario, IdJornadaTrabajo)
    End Function

    Public Function GetTURNOSREGULARESByIdTurno(ByVal _IdTurno As Integer) As BussinessEntities.TurnosRegulares Implements ITurnosRegularesBL.GetTURNOSREGULARESByIdTurno
        Return GetTURNOSREGULARESByIdTurno(_IdTurno)
    End Function
End Class
