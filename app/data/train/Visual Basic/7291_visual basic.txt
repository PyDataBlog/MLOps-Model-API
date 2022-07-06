Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.DataAccess
Public Class VentaBL : Implements IVentaBL
    Dim factoryrepository As IVentaRepository
    Public Sub New()
        factoryrepository = New VentaRepository
    End Sub
    Public Sub DeleteVENTA(ByVal IdVenta As Integer) Implements IVentaBL.DeleteVENTA
        factoryrepository.DeleteVENTA(IdVenta)
    End Sub

    Public Function GetVENTAByAnio(ByVal Anio As Integer) As System.Collections.Generic.List(Of BussinessEntities.Venta) Implements IVentaBL.GetVENTAByAnio
        Return factoryrepository.GetVENTAByAnio(Anio)
    End Function

    Public Function GetVENTAByEstado(ByVal Estado As String) As System.Collections.Generic.List(Of BussinessEntities.Venta) Implements IVentaBL.GetVENTAByEstado
        Return factoryrepository.GetVENTAByEstado(Estado)
    End Function

    Public Function GetVENTAByIdVenta(ByVal IdVenta As Integer) As BussinessEntities.Venta Implements IVentaBL.GetVENTAByIdVenta
        Return factoryrepository.GetVENTAByIdVenta(IdVenta)
    End Function

    Public Function SaveVENTA(ByVal _Venta As BussinessEntities.Venta) As Integer Implements IVentaBL.SaveVENTA
        Return factoryrepository.SaveVENTA(_Venta)
    End Function

    Public Sub UpdateEstadoVENTA(ByVal IdVenta As Integer, ByVal Estado As String) Implements IVentaBL.UpdateEstadoVENTA
        factoryrepository.UpdateEstadoVENTA(IdVenta, Estado)
    End Sub

    Public Sub GenerarComprobanteVenta(ByVal IdVenta As Integer, ByVal IdProyecto As Integer, ByVal IdTipoDocumento As Integer) Implements IVentaBL.GenerarComprobanteVenta
        factoryrepository.GenerarComprobanteVenta(IdVenta, IdProyecto, IdTipoDocumento)
    End Sub

    Public Sub UpdateIgvVenta(ByVal IdCatVenta As Integer, ByVal IdVenta As Integer) Implements IVentaBL.UpdateIgvVenta
        factoryrepository.UpdateIgvVenta(IdCatVenta, IdVenta)
    End Sub

    Public Sub UpdateObservacionVENTA(ByVal IdVenta As Integer, ByVal Observacion As String) Implements IVentaBL.UpdateObservacionVENTA
        factoryrepository.UpdateObservacionVENTA(IdVenta, Observacion)
    End Sub

    Public Function VerifyVentaTituloGratuito(ByVal serie As String, ByVal numero As Integer, ByVal tipo As Integer) As String Implements IVentaBL.VerifyVentaTituloGratuito
        Return factoryrepository.VerifyVentaTituloGratuito(serie, numero, tipo)
    End Function

    Public Function VerifyVentaSemovientes(ByVal serie As String, ByVal numero As Integer, ByVal tipo As Integer) As String Implements IVentaBL.VerifyVentaSemovientes
        Return factoryrepository.VerifyVentaSemovientes(serie, numero, tipo)
    End Function

    Public Sub GenerarAsientoTraslado(ByVal FechaContable As Date, ByVal anio As Integer, ByVal mes As Integer, ByVal usuario As Integer) Implements IVentaBL.GenerarAsientoTraslado
        factoryrepository.GenerarAsientoTraslado(FechaContable, anio, mes, usuario)
    End Sub
End Class
