Imports SistFoncreagro.BussinessEntities
Public Interface IDetalleEncuestaRepository
    Function GetDETALLEENCUESTAByIdEncuesta(ByVal IdEncuesta As Int32) As List(Of DetalleEncuesta)
    Sub SaveDETALLEENCUESTA(ByVal _detalle As DetalleEncuesta)
    Sub DeleteDETALLEENCUESTA(ByVal IdDetalle As Int32)
End Interface
