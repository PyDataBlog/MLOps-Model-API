Imports SistFoncreagro.BussinessEntities
Public Interface IFormacionRequeridaRepository
    Function GetAllFromFORMACIONREQUERIDA() As List(Of FormacionRequerida)
    Function SaveFORMACIONREQUERIDA(ByVal formacionRequerida As FormacionRequerida) As Int32
    Function GetFORMACIONREQUERIDAByIdFormacionAndIdPosicion(ByVal IdFormacion As Int32, ByVal IdPosicion As Int32) As FormacionRequerida
    Function GetFORMACIONREQUERIDAByIdPosicion(ByVal IdPosicion As Int32) As List(Of FormacionRequerida)
    Sub DeleteREQUISITOADICIONAL(ByVal IdFormacion As Int32, ByVal IdPosicion As Int32)
End Interface

