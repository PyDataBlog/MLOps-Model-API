Imports SistFoncreagro.Report
Public Class ReporteAnaliticoCCosto
    Inherits System.Web.UI.Page
    Dim ProyectoInicio As String
    Dim ProyectoFin As String
    Dim CCostoInicio As String
    Dim CCostoFin As String
    Dim FechaInicio As String
    Dim FechaFin As String
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        ProyectoInicio = Request.QueryString("ProyectoInicio")
        ProyectoFin = Request.QueryString("ProyectoFin")
        CCostoInicio = Request.QueryString("CCostoInicio")
        CCostoFin = Request.QueryString("CCostoFin")
        FechaInicio = Request.QueryString("FechaInicio")
        FechaFin = Request.QueryString("FechaFin")
        Me.ReportViewer1.Report = New RepAnaliticoCCosto
        Me.ReportViewer1.Report.ReportParameters(0).Value = ProyectoInicio
        Me.ReportViewer1.Report.ReportParameters(1).Value = ProyectoFin
        Me.ReportViewer1.Report.ReportParameters(2).Value = CCostoInicio
        Me.ReportViewer1.Report.ReportParameters(3).Value = CCostoFin
        Me.ReportViewer1.Report.ReportParameters(4).Value = FechaInicio
        Me.ReportViewer1.Report.ReportParameters(5).Value = FechaFin
        Me.ReportViewer1.ZoomMode = Microsoft.Reporting.WebForms.ZoomMode.PageWidth
    End Sub

End Class