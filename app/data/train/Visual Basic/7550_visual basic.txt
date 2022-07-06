Imports SistFoncreagro.BussinessEntities
Public Interface ITipoDocumentoRepository
    Function GetAllFromTIPODOCUMENTO() As List(Of TipoDocumento)
    Function GetTIPODOCUMENTOByIdTipoDocumento(ByVal IdTipoDocumento As Int32) As TipoDocumento
    Sub SaveTIPODOCUMENTO(ByVal _TipoDocumento As TipoDocumento)
    Sub DeleteTIPODOCUMENTO(ByVal IdTipoDocumento As Int32)
    Sub UpdateEstadoTIPODOCUMENTO(ByVal IdTipoDocumento As Int32)
End Interface
