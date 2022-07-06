Imports SistFoncreagro.Report
Public Class VentanaReporteOrdenCompra
    Inherits System.Web.UI.Page
    Dim reporteProd As ReportOrdenCompraProducto
    Dim reporteServ As ReportOdenCompraServicio

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Dim idOrden As Integer
        Dim tipo As Integer
        idOrden = Me.Request.QueryString("idOc")
        tipo = Me.Request.QueryString("tipo")
        If tipo = 1 Then
            reporteProd = New ReportOrdenCompraProducto
            ReportViewer1.Report = reporteProd
        Else
            reporteServ = New ReportOdenCompraServicio
            ReportViewer1.Report = reporteServ
        End If
        Me.ReportViewer1.Report.ReportParameters(0).Value = idOrden
    End Sub

End Class