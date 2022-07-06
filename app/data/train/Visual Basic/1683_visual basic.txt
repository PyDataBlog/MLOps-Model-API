Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.DataAccess

Public Class DetEjerPresupBL : Implements IDetEjerPresupBL

    Dim factoryrepository As IDetEjerPresupRepository

    Public Sub New()
        factoryrepository = New DetEjerPresupRepository
    End Sub

    Public Sub DeleteDetEjerPresup(ByVal _id As Integer) Implements IDetEjerPresupBL.DeleteDetEjerPresup
        factoryrepository.DeleteDetEjerPresup(_id)
    End Sub

    Public Function GetAllFromDetEjerPresup() As System.Collections.Generic.List(Of BussinessEntities.DetEjerPresup) Implements IDetEjerPresupBL.GetAllFromDetEjerPresup
        Return factoryrepository.GetAllFromDetEjerPresup
    End Function

    Public Function GetDetEjerPresupByIdDetEjerPresup(ByVal _id As Integer) As BussinessEntities.DetEjerPresup Implements IDetEjerPresupBL.GetDetEjerPresupByIdDetEjerPresup
        Return factoryrepository.GetDetEjerPresupByIdDetEjerPresup(_id)
    End Function

    Public Sub SaveDetEjerPresup(ByVal _DetEjerPresup As BussinessEntities.DetEjerPresup) Implements IDetEjerPresupBL.SaveDetEjerPresup
        factoryrepository.SaveDetEjerPresup(_DetEjerPresup)
    End Sub

    Public Sub SaveDetEjerPresup1(ByVal _DetEjerPresup As System.Collections.Generic.List(Of BussinessEntities.DetEjerPresup)) Implements IDetEjerPresupBL.SaveDetEjerPresup1
        factoryrepository.SaveDetEjerPresup1(_DetEjerPresup)
    End Sub
End Class
