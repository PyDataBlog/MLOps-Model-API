Imports SistFoncreagro.BussinessEntities
Public Interface IAprobarRequerimientoBL
    Function SaveNivelAprobarRequerimiento(ByVal aprobarReq As AprobarRequerimiento) As Int32
    Function UpdateNivelAprobarRequerimiento(ByVal aprobarReq As AprobarRequerimiento) As Int32
    Function DeleteNivelAprobarRequerimiento(ByVal IdAprobarRequerimiento As Int32) As Boolean
    Function GetAllFromNivelesAprobarRequerimiento() As List(Of AprobarRequerimiento)
End Interface
