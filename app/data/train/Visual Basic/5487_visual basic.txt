Imports SistFoncreagro.Report
Public Class ReporteLibroDiario
    Inherits System.Web.UI.Page
    Dim IdProyecto As String
    Dim IdSubDiario As String
    Dim FechaFin As String
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        IdProyecto = Request.QueryString("IdProyecto")
        IdSubDiario = Request.QueryString("IdSubDiario")
        FechaFin = Request.QueryString("FechaFin")
        Me.ReportViewer1.Report = New RepLibroDiario
        Me.ReportViewer1.Report.ReportParameters(0).Value = IdProyecto
        Me.ReportViewer1.Report.ReportParameters(1).Value = IdSubDiario
        Me.ReportViewer1.Report.ReportParameters(2).Value = FechaFin
    End Sub

End Class