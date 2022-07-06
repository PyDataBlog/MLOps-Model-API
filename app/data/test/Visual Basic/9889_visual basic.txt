Imports SistFoncreagro.BussinessEntities
Public Interface ICCostoxDetalleOrdenBL
    Function GetAllFromCCostoDetalleOrdenByIdDetalleOrden(ByVal idDetalleOrden As Int32) As List(Of CCostoxDetalleOrden)
End Interface
