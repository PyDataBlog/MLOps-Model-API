Imports SistFoncreagro.BussinessEntities
Public Interface IHorarioAsignadoRepository
    Function GetHorarioAsignadoByIdPeriodoLaboralAndByFecha(ByVal IdPeriodoLaboral As Int32, ByVal Fecha As DateTime) As HorarioAsignado
    Function SaveHORARIOASIGNADO(ByVal HorarioAsignado As HorarioAsignado) As Int32
    Function GetHorarioAsignadoByIdPeriodoLaboral(ByVal IdPeriodoLaboral As Int32) As List(Of HorarioAsignado)
End Interface
