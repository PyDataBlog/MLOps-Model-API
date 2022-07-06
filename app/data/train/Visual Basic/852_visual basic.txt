Public Class cetakstruk
    Dim id As String
    Sub panggil()
        strreportname = "rptPengambilan"
        Dim strreportpath As String = Application.StartupPath & "\" & strreportname & ".rpt"
        If Not IO.File.Exists(strreportpath) Then
            Throw (New Exception("Unable to locate report file : " & vbCrLf & strreportpath))
        End If
        Dim rptdoc As New CrystalDecisions.CrystalReports.Engine.ReportClass()
        rptdoc.FileName = strreportpath
        rptdoc.Load(strreportpath)
        rptdoc.SetDataSource(ds.Tables(0))
        CRV1.ShowRefreshButton = False
        CRV1.ShowCloseButton = False
        CRV1.ReportSource = rptdoc
    End Sub
    Private Sub struk_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        koneksi()
        id = Laundry.cmbnota.Text
    End Sub
End Class