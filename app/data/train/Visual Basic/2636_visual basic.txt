Imports SistFoncreagro.Report
Public Class ReporteActivo
    Inherits System.Web.UI.Page
    Dim Tipo As String
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Tipo = Request.QueryString("Tipo")
        Me.ReportViewer1.Report = New RepActivo
        Me.ReportViewer1.Report.ReportParameters(0).Value = Tipo
    End Sub

End Class