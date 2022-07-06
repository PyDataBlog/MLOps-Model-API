Public Class frmComputer

    Private Sub radDeluxe_CheckedChanged(sender As Object, e As EventArgs) Handles radDeluxe.CheckedChanged,
        radSuper.CheckedChanged, chkVideo.CheckedChanged, chkInternet.CheckedChanged, chkMemory.CheckedChanged

        Const DELUXE_COST As Double = 1000,
            SUPER_COST As Double = 1500,
            UPGRADE_VIDEO As Double = 200,
            UPGRADE_INTERNET As Double = 30,
            UPGRADE_MEMORY As Double = 120

        Dim Cost As Double

        If radDeluxe.Checked Then   'Deluxe model
            Cost = DELUXE_COST
        Else                        'Super model
            Cost = SUPER_COST
        End If

        If chkVideo.Checked Then
            Cost += UPGRADE_VIDEO
        End If

        If chkInternet.Checked Then
            Cost += UPGRADE_INTERNET
        End If

        If chkMemory.Checked Then
            Cost += UPGRADE_MEMORY
        End If

        txtResult.Text = (Cost).ToString("C")
    End Sub

End Class
