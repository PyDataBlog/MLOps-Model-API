Imports SistFoncreagro.BussinessEntities
Public Interface IDocumentoRendidoRepository
    Function GetAllFromDOCUMENTORENDIDO() As List(Of DocumentoRendido)
    Function GetDOCUMENTORENDIDOByIdDocRendido(ByVal IdDocRendido) As DocumentoRendido
    Function GetDOCUMENTORENDIDOByIdReciboRendicion(ByVal IdReciboRendicion As Int32) As List(Of DocumentoRendido)
    Function GetDOCUMENTORENDIDOByIdRendicion(ByVal IdRendicion As Int32) As List(Of DocumentoRendido)
    Sub SaveDOCUMENTORENDIDO(ByVal _DocumentoRendido As DocumentoRendido)
    Sub DeleteDOCUMENTORENDIDO(ByVal IdDocRendido As Int32)
End Interface
