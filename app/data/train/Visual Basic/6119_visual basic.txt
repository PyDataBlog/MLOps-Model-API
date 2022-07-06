Public Class Form1



    Private Sub btnPlus_Click(sender As Object, e As EventArgs) Handles btnPlus.Click
        'This event handler performs addition
        'Declare a variable to hold the result
        Dim dblResult As Double

        lblOperation.Text = " + "

        dblResult = CDbl(txtNumber1.Text) + CDbl(txtNumber2.Text)

        lblResult.Text = CStr(dblResult)

    End Sub

    Private Sub btnMinus_Click(sender As Object, e As EventArgs) Handles btnMinus.Click
        'This event handler performs subtraction
        Dim dblResult As Double

        lblOperation.Text = " - "

        dblResult = CDbl(txtNumber1.Text) - CDbl(txtNumber2.Text)

        lblResult.Text = CStr(dblResult)
    End Sub

    Private Sub btnMultiply_Click(sender As Object, e As EventArgs) Handles btnMultiply.Click
        'This event handler performs multipulcation
        Dim dblResult As Double

        lblOperation.Text = " * "

        dblResult = CDbl(txtNumber1.Text) * CDbl(txtNumber2.Text)

        lblResult.Text = CStr(dblResult)
    End Sub

    Private Sub btnDivide_Click(sender As Object, e As EventArgs) Handles btnDivide.Click
        'This event handler performs divition
        Dim dblResult As Double

        lblOperation.Text = " / "

        dblResult = CDbl(txtNumber1.Text) / CDbl(txtNumber2.Text)

        lblResult.Text = CStr(dblResult)
    End Sub

    Private Sub btnExponent_Click(sender As Object, e As EventArgs) Handles btnExponent.Click
        'This event handler performs exponent
        Dim dblResult As Double

        lblOperation.Text = " ^ "

        dblResult = CDbl(txtNumber1.Text) ^ CDbl(txtNumber2.Text)

        lblResult.Text = CStr(dblResult)
    End Sub

    Private Sub btnModulus_Click(sender As Object, e As EventArgs) Handles btnModulus.Click
        'This event handler performs modulus
        Dim dblResult As Double

        lblOperation.Text = " MOD "

        dblResult = CDbl(txtNumber1.Text) Mod CDbl(txtNumber2.Text)

        lblResult.Text = CStr(dblResult)
    End Sub

    Private Sub btnClear_Click(sender As Object, e As EventArgs) Handles btnClear.Click
        txtNumber1.Clear()
        txtNumber2.Clear()
        lblResult.Text = String.Empty
        lblOperation.Text = String.Empty
    End Sub

    Private Sub btnExit_Click(sender As Object, e As EventArgs) Handles btnExit.Click
        Me.Close()
    End Sub
End Class
