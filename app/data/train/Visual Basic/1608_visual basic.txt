Public Class Form1
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim a As Integer = TextBox1.Text
        Dim b As Integer = TextBox2.Text
        Dim c, i, gcd As Integer
        If (a > b) Then
            c = a
        Else
            c = b
        End If
        For i = 1 To c
            If a Mod i = 0 And b Mod i = 0 Then
                gcd = i
            End If
        Next
        MsgBox(gcd)
    End Sub
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim a As Integer = TextBox1.Text
        Dim b As Integer = TextBox2.Text
        Dim c, i, lcm As Integer
        c = a * b
        If (a > b And a Mod b = 0) Then
            lcm = a
        ElseIf (b > a And b Mod a = 0) Then
            lcm = b
        Else
            For i = 1 To c
                If i Mod a = 0 And i Mod b = 0 Then
                    lcm = i
                End If
            Next
        End If
        MsgBox(lcm)
    End Sub
End Class

