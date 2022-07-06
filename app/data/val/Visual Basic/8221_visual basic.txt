Imports SistFoncreagro.Report
Public Class ReporteGastos1
    Inherits System.Web.UI.Page
    Dim IdProyecto As String
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        IdProyecto = Session("IdProyecto")
        Me.ReportViewer1.Report = New RepGastosXCaserio
        Me.ReportViewer1.Report.ReportParameters(0).Value = IdProyecto
    End Sub

End Class