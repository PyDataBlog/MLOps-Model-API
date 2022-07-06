Imports SistFoncreagro.BussinessEntities
Public Interface IProveedorDetalleCotizacionRepository
    Sub SaveProveedorDetalleCotizacion(ByVal proveedorDetalleCoti As ProveedorDetalleCotizacion)
    Sub SaveProveedorDetalleCotizacion1(ByVal proveedorDetalleCoti As List(Of ProveedorDetalleCotizacion))
    Sub UpdateProveedorDetalleCotizacion(ByVal proveedorDetalleCoti As ProveedorDetalleCotizacion)
    Sub UpdateProveedorDetalleCotizacion1(ByVal proveedorDetalleCoti As List(Of ProveedorDetalleCotizacion))
    Sub DeleteProveedor(ByVal proveedorDetCot As ProveedorDetalleCotizacion)
    Sub DeleteProveedor1(ByVal proveedorDetCot As List(Of ProveedorDetalleCotizacion))
    Function GetAllFromProveedorDetalleCotizacionByIdDetalleReqCotizacion(ByVal idDetalleReqCotizacion As Int32) As List(Of ProveedorDetalleCotizacion)
    Sub UpdateIgvCotizacion(ByVal IdDetalleRequerimiento As Int32, ByVal valor As Boolean)
End Interface
