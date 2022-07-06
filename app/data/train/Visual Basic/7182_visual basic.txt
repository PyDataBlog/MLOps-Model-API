Imports SistFoncreagro.BussinessEntities
Public Interface IDetalleReqCotizacionRepository
    Sub SaveDetalleReqCotizacion(ByVal detalleReqCoti As DetalleReqCotizacion)
    Sub SaveDetalleReqCotizacion1(ByVal detalleReqCoti As List(Of DetalleReqCotizacion), ByVal idCotizacion As Int32)
    Sub SaveItemDetalleReqCotizacon(ByVal detalleReqCoti As DetalleReqCotizacion)
    Sub DeleteItemDetalleReqCotizacion(ByVal IdDetalleReqCotizacion As Int32)
    Function GetAllDetalleRequerimientoCotizacionByIdCotizacion(ByVal idCotizacion As Int32) As List(Of DetalleReqCotizacion)
End Interface
