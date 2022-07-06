Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.DataAccess
Public Class ProgramacionBL : Implements IProgramacionBL

    Dim factoryrepository As IProgramacionRepository

    Public Sub New()
        factoryrepository = New ProgramacionRepository
    End Sub

    Public Sub GenerarProgramacionPoaTecnico(ByVal anio As Integer, ByVal IdProyecto As Integer) Implements IProgramacionBL.GenerarProgramacionPoaTecnico
        factoryrepository.GenerarProgramacionPoaTecnico(anio, IdProyecto)
    End Sub

    Public Sub SavePROGRAMACION(ByVal _Programacion As BussinessEntities.Programacion) Implements IProgramacionBL.SavePROGRAMACION
        factoryrepository.SavePROGRAMACION(_Programacion)
    End Sub

    Public Sub SaveListaProgramacion(ByVal Programacion As System.Collections.Generic.List(Of BussinessEntities.Programacion)) Implements IProgramacionBL.SaveListaProgramacion
        factoryrepository.SaveListaProgramacion(Programacion)
    End Sub
End Class
