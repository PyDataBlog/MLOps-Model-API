Imports SistFoncreagro.BussinessEntities

Public Interface IPermisosRepository
    Function GetAllFromPermisos() As List(Of Permisos)
    Function GetPermisosByDescripcion(ByVal descripcion As String) As Permisos
    Function PASolicitaAprobacion(ByVal IdPersonal As Int32) As Int32
End Interface
