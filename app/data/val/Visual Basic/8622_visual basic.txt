Imports SistFoncreagro.BussinessEntities

Public Interface IObservacionBL
    Function GetObservacionById(ByVal _id As Int32) As Observacion
    Function GetObservacionByIdAndTabla(ByVal _id As Int32, ByVal _Tabla As String) As List(Of Observacion)
    Sub UpdateObservacion(ByVal _IdObservacion As Int32, ByVal _Obser As String)
    Sub InsertObservacion(ByVal _Observacion As Observacion)
    Sub DeleteObservacion(ByVal _id As Int32)
End Interface
