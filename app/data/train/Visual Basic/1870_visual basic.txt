Imports SistFoncreagro.BussinessEntities
Public Interface IDetalleEjecucionBL
    Function GetDETALLEEJECUCIONByIdProyecto(ByVal IdProyecto As Int32) As List(Of DetalleEjecucion)
    Function SaveDETALLEEJECUCION(ByVal _DetalleEjecucion As DetalleEjecucion) As Int32
    Function GetDETALLEEJECUCIONByIdDetalleEje(ByVal IdDetalleEje As Int32) As DetalleEjecucion
    Sub DeletetDETALLEEJECUCION(ByVal IdDetalleEje As Int32)
    Sub UpdateEstadoDetalleejecucion(ByVal IdDetalleEje As Int32)
End Interface
