Imports SistFoncreagro.BussinessEntities

Public Interface ITipoConvenioBL
    Function GetAllFromTipoConvenio() As List(Of TipoConvenio)
    Function GetFromTipoConvenioById(ByVal id As Int32) As TipoConvenio
    Sub SaveTipoConvenio(ByVal _TipoConvenio As TipoConvenio)
    Sub DeleteTipoConvenio(ByVal id As Int32)
End Interface
