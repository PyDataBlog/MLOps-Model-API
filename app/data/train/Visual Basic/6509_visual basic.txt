Imports SistFoncreagro.BussinessEntities
Public Interface IPosicionBL
    Function GetAllFromPosicion(ByVal argPosicion As ArgumentosPosicion) As List(Of Posicion)
    Function GetAllFromPosicion2() As List(Of Posicion)
    Function GetPosicionByIdPosicion(ByVal IdPosicion As Int32, ByVal argPosicion As ArgumentosPosicion) As Posicion
    Function GetPosicionByIdArea(ByVal IdArea As Int32, ByVal argPosicion As ArgumentosPosicion) As List(Of Posicion)
    Function GetPosicionByIdArea2(ByVal IdArea As Int32) As List(Of Posicion)
    Function SavePosicion(ByVal posicion As Posicion) As Int32
    Function GetJefaturaByIdArea(ByVal idArea As Int32, ByVal argPosicion As ArgumentosPosicion) As Posicion
    Function GetListaPosicionesDeUnaAreaySubAreasByIdArea(ByVal IdArea As Int32) As IList(Of Posicion)
    Sub DeletePosicion(ByVal IdPosicion As Int32)
End Interface
