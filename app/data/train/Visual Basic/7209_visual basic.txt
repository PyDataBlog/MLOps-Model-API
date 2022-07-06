Imports SistFoncreagro.BussinessEntities

Public Interface IEjerPresupRepository
    Function GetAllFromEjerPresup() As List(Of EjerPresup)
    Function GetEjerPresupByEstado(ByVal _Estado As String) As EjerPresup ' sale uno cuando el estado es en <<proceso>>
    Function GetEjerPresupByIdEjerPresup(ByVal _id As Int32) As EjerPresup
    Function SaveEjerPresup(ByVal _EjerPresup As EjerPresup) As String
    Function DeleteEjerPresup(ByVal _id As Int32) As String
    Function EjerPresupEstado() As String
End Interface
