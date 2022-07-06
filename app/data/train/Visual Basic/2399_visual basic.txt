Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.DataAccess
Public Class VehiculoBL : Implements IVehiculoBL
    Dim factoryrepository As IVehiculoRepository
    Public Sub New()
        factoryrepository = New VehiculoRepository
    End Sub

    Public Sub DeleteVEHICULO(ByVal IdVehiculo As Integer) Implements IVehiculoBL.DeleteVEHICULO
        factoryrepository.DeleteVEHICULO(IdVehiculo)
    End Sub

    Public Function GetVEHICULOByIdDetalleEje(ByVal IdDetalleEje As Integer) As System.Collections.Generic.List(Of BussinessEntities.Vehiculo) Implements IVehiculoBL.GetVEHICULOByIdDetalleEje
        Return factoryrepository.GetVEHICULOByIdDetalleEje(IdDetalleEje)
    End Function

    Public Function GetVEHICULOByIdVehiculo(ByVal IdVehiculo As Object) As BussinessEntities.Vehiculo Implements IVehiculoBL.GetVEHICULOByIdVehiculo
        Return factoryrepository.GetVEHICULOByIdVehiculo(IdVehiculo)
    End Function

    Public Sub SaveVEHICULO(ByVal _Vehiculo As BussinessEntities.Vehiculo) Implements IVehiculoBL.SaveVEHICULO
        factoryrepository.SaveVEHICULO(_Vehiculo)
    End Sub
End Class
