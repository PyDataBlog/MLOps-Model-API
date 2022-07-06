Imports SistFoncreagro.BussinessEntities
Public Interface IDerechoHabienteRepository
    'Function GetDERECHOHABIENTEByIdPersonalAndIdVinculo(ByVal IdPersonal As Int32, ByVal IdVinculo As Int32) As List(Of DerechoHabiente)
    Function GetDERECHOHABIENTEConyugueByIdPersonal(ByVal IdPersonal As Int32) As List(Of DerechoHabiente)
    Function GetDERECHOHABIENTEHijoByIdPersonal(ByVal IdPersonal As Int32) As List(Of DerechoHabiente)
    Function GetDERECHOHABIENTEOtrosVinculosByIdPersonal(ByVal IdPersonal As Int32) As List(Of DerechoHabiente)
    Function SaveDERECHOHABIENTE(ByVal derechoHabiente As DerechoHabiente) As Int32
    Sub DeleteDerechoHabiente(ByVal IdPersonal As Int32, ByVal IdDerechoHabiente As Int32)
End Interface
