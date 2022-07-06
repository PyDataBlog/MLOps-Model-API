Imports SistFoncreagro.BussinessEntities
Public Interface IEstadoCotizacionBL
    Function GetAllFromEstadoCotizacionByIdCotizacion(ByVal IdCotizacion As Int32) As List(Of EstadoCotizacion)
End Interface
