Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.DataAccess
Public Class ClimaSocialBL : Implements IClimaSocialBL
    Dim factoryrepository As IClimaSocialRepository

    Public Sub New()
        factoryrepository = New ClimaSocialRepository
    End Sub

    Public Sub DeleteCLIMASOCIAL(ByVal IdClima As Integer) Implements IClimaSocialBL.DeleteCLIMASOCIAL
        factoryrepository.DeleteCLIMASOCIAL(IdClima)
    End Sub

    Public Function GetCLIMASOCIALByIdClima(ByVal IdClima As Integer) As BussinessEntities.ClimaSocial Implements IClimaSocialBL.GetCLIMASOCIALByIdClima
        Return factoryrepository.GetCLIMASOCIALByIdClima(IdClima)
    End Function

    Public Function GetCLIMASOCIALByIdUbigeoandIdProyecto(ByVal Cubigeo As String, ByVal IdProyecto As Integer) As System.Collections.Generic.List(Of BussinessEntities.ClimaSocial) Implements IClimaSocialBL.GetCLIMASOCIALByIdUbigeoandIdProyecto
        Return factoryrepository.GetCLIMASOCIALByIdUbigeoandIdProyecto(Cubigeo, IdProyecto)
    End Function
    Public Function SaveCLIMASOCIAL(ByVal _Clima As BussinessEntities.ClimaSocial) As Integer Implements IClimaSocialBL.SaveCLIMASOCIAL
        Return factoryrepository.SaveCLIMASOCIAL(_Clima)
    End Function
End Class
