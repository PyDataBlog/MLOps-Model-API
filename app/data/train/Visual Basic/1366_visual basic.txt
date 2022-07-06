Imports System.Drawing.Printing

Public Class ImpresionJV

    Public valid As Boolean = False

    Private Sub ImpresionJV_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Util.PopulateCombo(cmbPoint, "SELECT id, nom FROM point_ventes", "id", "nom")
        PopulatePrinter()

        cmbDu.MaxDate = Today
        cmbAu.MaxDate = Today
    End Sub


    Sub PopulatePrinter()
        Try
            Dim i As Integer
            Dim pkInstalledPrinter As String
            For i = 0 To PrinterSettings.InstalledPrinters.Count - 1
                pkInstalledPrinter = PrinterSettings.InstalledPrinters.Item(i)
                cmbPrinter.Items.Add(pkInstalledPrinter)
            Next
        Catch ex As Exception
            If My.Settings.app_debug_mode Then
                Util.ShowMessage(ex.ToString, 2)
            Else
                Util.ShowMessage("Impossible de recuperer la liste des imprimantes installer", 2)
            End If
        Finally
            Dim pp As New PrinterSettings
            cmbPrinter.Text = pp.PrinterName
        End Try
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Close()
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If cmbPoint.Text.Length = 0 Then
            Util.ShowMessage("Selectioner le poit de vente svp!", 1)
        ElseIf cmbPrinter.SelectedIndex = -1 Then
            Util.ShowMessage("Veuilez selectioner une imprimante valide", 1)
        Else
            valid = True
            Close()
        End If
    End Sub

    Private Sub cmbAu_ValueChanged(sender As Object, e As EventArgs) Handles cmbAu.ValueChanged
        cmbDu.MaxDate = cmbAu.Value
        cmbAu.MinDate = cmbDu.Value
    End Sub

    Private Sub cmbDu_ValueChanged(sender As Object, e As EventArgs) Handles cmbDu.ValueChanged
        cmbDu.MaxDate = cmbAu.Value
        cmbAu.MinDate = cmbDu.Value
    End Sub
End Class