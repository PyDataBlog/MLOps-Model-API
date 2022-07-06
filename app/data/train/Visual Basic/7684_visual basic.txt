Imports SistFoncreagro.Report
Public Class ReporteReposicionFondoFijo
    Inherits System.Web.UI.Page

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Me.ReportViewer1.Report = New RepReposicionFondoFijo
    End Sub

End Class