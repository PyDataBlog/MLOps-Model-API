Imports SistFoncreagro.BussinessEntities
Public Interface IMarcaRepository
    Sub SaveMarca(ByVal marca As Marca)
    Sub DeleteMarca(ByVal IdMarca As Int32)
    Function GetAllFromMarca() As List(Of Marca)
    Function GetAllFromByIdMarca(ByVal idMarca As Int32) As Marca
End Interface
