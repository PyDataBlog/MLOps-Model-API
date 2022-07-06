Imports Microsoft.Reporting.WinForms
Imports MySql.Data.MySqlClient

Namespace Printing
    Public Class ReportDetteClient

        Public query As String
        Public point_vente As String
        Public titre As String

        Private Sub ReportEtatStock_Load(sender As Object, e As EventArgs) Handles MyBase.Load
            Timer1.Start()
        End Sub

        Dim counter As Integer = 0
        Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
            If counter = 1 Then
                Timer1.Stop()
                RefreshReport()
            End If
            counter += 1
        End Sub

        Sub RefreshReport()

            Try
                Dim cmd = Database.GetCommand
                Dim ds As New DataSetTables
                cmd.CommandText = query
                Dim adapter As New MySqlDataAdapter
                adapter.SelectCommand = cmd
                adapter.Fill(ds.DettesClients)

                Report.LocalReport.ReportPath = System.Environment.CurrentDirectory & "\reports\ReportDetteClient.rdlc"


                With Report
                    .ZoomMode = ZoomMode.PageWidth
                    .ProcessingMode = Microsoft.Reporting.WinForms.ProcessingMode.Local
                    .LocalReport.DataSources.Clear()
                    .LocalReport.DataSources.Add(New ReportDataSource("DataSetTables", CType(ds.DettesClients, DataTable)))
                    .RefreshReport()
                End With

                'Me.report.RefreshReport()
                cmd.Connection.Close()
            Catch ex As Exception
                If My.Settings.app_debug_mode Then
                    Util.ShowMessage(ex.ToString, 2)
                Else
                    Util.ShowMessage("Echec de connexion a la base de donnees", 2)
                End If
            Finally
                Database.CloseConnection()
                GC.Collect()
            End Try
        End Sub


    End Class
End Namespace