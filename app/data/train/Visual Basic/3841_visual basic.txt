Imports SistFoncreagro.BussinessEntities
Public Interface IRequisitoAdicionalBL
    Function GetAllFromREQUISITOADICIONAL() As List(Of RequisitoAdicional)
    Function SaveREQUISITOADICIONAL(ByVal RequisitoAdicional As RequisitoAdicional) As Int32
    Function GetREQUISITOADICIONALByIdRequisitoAndIdPosicion(ByVal IdRequisito As Int32, ByVal IdPosicion As Int32) As RequisitoAdicional
    Function GetREQUISITOADICIONALByIdPosicion(ByVal IdPosicion As Int32) As List(Of RequisitoAdicional)
    Sub DeleteREQUISITOADICIONAL(ByVal idRequisito As Int32)
End Interface
