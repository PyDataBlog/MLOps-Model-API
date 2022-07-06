Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.DataAccess
Public Class TipoCotizacionBL : Implements ITipoCotizacionBL
    Dim factoryRepostitory As ITipoCotizacionRepository
    Sub New()
        factoryRepostitory = New TipoCotizacionRepository
    End Sub

    Public Function GetAllFromTipoCotizacion() As System.Collections.Generic.List(Of BussinessEntities.TipoCotizacion) Implements ITipoCotizacionBL.GetAllFromTipoCotizacion
        Return factoryRepostitory.GetAllFromTipoCotizacion
    End Function

    Public Sub SaveTipoCotizacion(ByVal tipoCotizacion As BussinessEntities.TipoCotizacion) Implements ITipoCotizacionBL.SaveTipoCotizacion
        factoryRepostitory.SaveTipoCotizacion(tipoCotizacion)
    End Sub
End Class
