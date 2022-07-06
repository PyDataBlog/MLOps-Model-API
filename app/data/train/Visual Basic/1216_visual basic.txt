Public Class Calc
    Public operation = "OP"
    Public firstnum As Integer = 0
    Public secnum As Integer = 0
    Public a As Integer = 0



    Private Sub Calc_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.TopMost = True
        TextBox1.Enabled = False
        TextBox2.Enabled = False
        Button9.Enabled = False
        Button8.Enabled = False
        Button7.Enabled = False
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Me.Close()
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Me.WindowState = FormWindowState.Minimized
    End Sub

    Private Sub Label1_Click(sender As Object, e As EventArgs) Handles Label1.Click
        MsgBox("The program open is: Calc.vb")
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        operation = "+"
        Button7.Enabled = True
        TextBox1.Enabled = True
        Button3.Enabled = False
        Button4.Enabled = False
        Button5.Enabled = False
        Button6.Enabled = False
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        operation = "-"
        Button7.Enabled = True
        TextBox1.Enabled = True
        Button3.Enabled = False
        Button4.Enabled = False
        Button5.Enabled = False
        Button6.Enabled = False
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        operation = "/"
        Button7.Enabled = True
        TextBox1.Enabled = True
        Button3.Enabled = False
        Button4.Enabled = False
        Button5.Enabled = False
        Button6.Enabled = False
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        operation = "*"
        Button7.Enabled = True
        TextBox1.Enabled = True
        Button3.Enabled = False
        Button4.Enabled = False
        Button5.Enabled = False
        Button6.Enabled = False
    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        firstnum = TextBox1.Text
        Button7.Enabled = False
        TextBox1.Enabled = False
        TextBox2.Enabled = True
        Button8.Enabled = True
    End Sub

    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        secnum = TextBox2.Text
        TextBox2.Enabled = False
        Button8.Enabled = False
        Button9.Enabled = True
    End Sub

    Private Sub Button9_Click(sender As Object, e As EventArgs) Handles Button9.Click
        If operation = "+" Then
            MsgBox(firstnum + secnum)
        ElseIf operation = "-" Then
            MsgBox(firstnum - secnum)
        ElseIf operation = "/" Then
            MsgBox(firstnum / secnum)
        ElseIf operation = "*" Then
            MsgBox(firstnum * secnum)
        Else
            MsgBox("Operation not found!")
        End If
    End Sub

End Class