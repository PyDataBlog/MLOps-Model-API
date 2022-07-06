Imports SistFoncreagro.Report
Public Class ReporteSaldoCuenta19
    Inherits System.Web.UI.Page
    Dim IdProveedor As String
    Dim IdMoneda As String
    Dim FechaFin As String
    Dim IdProyecto As String
    Dim Proyecto As String
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        IdProveedor = Request.QueryString("IdProveedor")
        IdMoneda = Request.QueryString("IdMoneda")
        FechaFin = Request.QueryString("FechaFin")
        IdProyecto = Request.QueryString("IdProyecto")
        Proyecto = Request.QueryString("Proyecto")

        Me.ReportViewer1.Report = New RepSaldoCuenta19
        Me.ReportViewer1.Report.ReportParameters(0).Value = FechaFin
        Me.ReportViewer1.Report.ReportParameters(1).Value = IdProveedor
        Me.ReportViewer1.Report.ReportParameters(2).Value = IdMoneda
        Me.ReportViewer1.Report.ReportParameters(3).Value = IdProyecto
        Me.ReportViewer1.Report.ReportParameters(4).Value = Proyecto
    End Sub

End Class