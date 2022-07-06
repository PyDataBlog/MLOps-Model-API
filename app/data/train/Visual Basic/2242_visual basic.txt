Imports SistFoncreagro.BussinessEntities
Imports Microsoft.Practices.EnterpriseLibrary.Data

Partial Public Class ActividadRepository : Inherits MasterDataAccess : Implements IActividadRepository

    Public Function GetActividadesByIdProycomp(idProycomp As Integer) As System.Collections.Generic.List(Of BussinessEntities.ActividadDto) Implements IActividadRepository.GetActividadesByIdProycomp

        Dim query = db.ExecuteSprocAccessor(Of ActividadDto)("GetActividadesByIdProycomp", idProycomp)

        Return query.ToList()
    End Function
End Class
