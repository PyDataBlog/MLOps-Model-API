Public Class Form1

    'Variables
    Dim intAge, intMonths, intTotalMonths As Integer

    Private Sub btnCalc_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCalc.Click


        'Assign Variables
        intAge = CInt(txtAge.Text)
        intMonths = CInt(txtMonths.Text)

        'Calculate
        intTotalMonths = intAge * 12 + intMonths

        'Display
        MessageBox.Show("You are " + CStr(intTotalMonths) + " Months old!")

        'Clear
        txtAge.Clear()
        txtMonths.Clear()

    End Sub
End Class

'   ******************
'   *  Kaleb Haslam  *
'   * Age Calculator *
'   ******************