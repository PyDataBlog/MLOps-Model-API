Imports SistFoncreagro.BussinessEntities

Public Interface IFeriadoRepository

    Function GetFeriadoByFecha(ByVal _Fecha As Date) As Feriado
    Function GetFeriadoById(ByVal _Id As Int32) As Feriado
    Function GetAllFromFeriado() As List(Of Feriado)
    Sub DeleteFeriado(ByVal _id As Int32)
    Function SaveFeriado(ByVal _Feriado As Feriado) As Int32


End Interface
