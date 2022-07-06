Public Class Calls

    Private Sub HuraButton1_Click(sender As Object, e As EventArgs) Handles HuraButton1.Click
        Me.Close()
    End Sub

    Private Sub HuraButton2_Click(sender As Object, e As EventArgs) Handles HuraButton2.Click
        Me.WindowState = FormWindowState.Minimized
    End Sub

    Private Sub HuraForm1_Click(sender As Object, e As EventArgs) Handles HuraForm1.Click

    End Sub

    Private Sub HuraButton3_Click(sender As Object, e As EventArgs) Handles HuraButton3.Click
        TextBox1.Clear()
        Label1.Text = ""
        Label2.Text = ""
        Label3.Text = ""
    End Sub

    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged

    End Sub

    Private Sub HuraButton4_Click(sender As Object, e As EventArgs) Handles HuraButton4.Click
        Dim sqrt As Double
        sqrt = Convert.ToDouble(TextBox1.Text)
        TextBox1.Text = Convert.ToString(Math.Sqrt(sqrt))
    End Sub

    Private Sub HuraButton5_Click(sender As Object, e As EventArgs) Handles HuraButton5.Click
        Dim numb As Double
        numb = Convert.ToDouble(TextBox1.Text)
        Dim nu As Double
        nu = numb - numb - numb
        TextBox1.Text = Convert.ToString(nu)
    End Sub

    Private Sub HuraButton7_Click(sender As Object, e As EventArgs) Handles HuraButton7.Click
        TextBox1.AppendText(1)
    End Sub

    Private Sub HuraButton8_Click(sender As Object, e As EventArgs) Handles HuraButton8.Click
        TextBox1.AppendText(2)
    End Sub

    Private Sub HuraButton9_Click(sender As Object, e As EventArgs) Handles HuraButton9.Click
        TextBox1.AppendText(3)
    End Sub

    Private Sub HuraButton10_Click(sender As Object, e As EventArgs) Handles HuraButton10.Click
        TextBox1.AppendText(4)
    End Sub

    Private Sub HuraButton11_Click(sender As Object, e As EventArgs) Handles HuraButton11.Click
        TextBox1.AppendText(5)
    End Sub

    Private Sub HuraButton12_Click(sender As Object, e As EventArgs) Handles HuraButton12.Click
        TextBox1.AppendText(6)
    End Sub

    Private Sub HuraButton13_Click(sender As Object, e As EventArgs) Handles HuraButton13.Click
        TextBox1.AppendText(7)
    End Sub

    Private Sub HuraButton14_Click(sender As Object, e As EventArgs) Handles HuraButton14.Click
        TextBox1.AppendText(8)
    End Sub

    Private Sub HuraButton15_Click(sender As Object, e As EventArgs) Handles HuraButton15.Click
        TextBox1.AppendText(9)
    End Sub

    Private Sub HuraButton20_Click(sender As Object, e As EventArgs) Handles HuraButton20.Click
        TextBox1.AppendText(0)
    End Sub

    Private Sub HuraButton19_Click(sender As Object, e As EventArgs) Handles HuraButton19.Click
        TextBox1.AppendText(".")
    End Sub

    Private Sub HuraButton6_Click(sender As Object, e As EventArgs) Handles HuraButton6.Click
        Label1.Text = TextBox1.Text
        Label2.Text = "/"
        TextBox1.Clear()
    End Sub

    Private Sub HuraButton16_Click(sender As Object, e As EventArgs) Handles HuraButton16.Click
        Label1.Text = TextBox1.Text
        Label2.Text = "*"
        TextBox1.Clear()
    End Sub

    Private Sub HuraButton17_Click(sender As Object, e As EventArgs) Handles HuraButton17.Click
        Label1.Text = TextBox1.Text
        Label2.Text = "-"
        TextBox1.Clear()
    End Sub

    Private Sub HuraButton18_Click(sender As Object, e As EventArgs) Handles HuraButton18.Click
        Label1.Text = TextBox1.Text
        Label2.Text = "+"
        TextBox1.Clear()
    End Sub

    Private Sub HuraButton22_Click(sender As Object, e As EventArgs) Handles HuraButton22.Click
        Label3.Text = TextBox1.Text
        Dim sign As Char
        sign = Label2.Text
        Dim n1 As Double
        n1 = Convert.ToDouble(Label1.Text)
        Dim n2 As Double
        n2 = Convert.ToDouble(Label3.Text)
        Dim n3 As Double

        Select Case (sign)
            Case "+"
                n3 = n1 + n2
            Case "-"
                n3 = n1 - n2
            Case "*"
                n3 = n1 * n2
            Case "/"
                n3 = n1 / n2
        End Select
        TextBox1.Text = Convert.ToString(n3)
    End Sub
End Class