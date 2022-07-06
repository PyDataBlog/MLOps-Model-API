Public Class Form1
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim a = InputBox("Display")
        If a = "1" Then
            MsgBox("one")
        ElseIf a = "2" Then
            MsgBox("two")
        ElseIf a = "3" Then
            MsgBox("three")
        ElseIf a = "4" Then
            MsgBox("four")
        ElseIf a = "5" Then
            MsgBox("five")
        ElseIf a = "6" Then
            MsgBox("six")
        ElseIf a = "7" Then
            MsgBox("seven")
        ElseIf a = "8" Then
            MsgBox("eight")
        ElseIf a = "9" Then
            MsgBox("nine")
        ElseIf a = "10" Then
            MsgBox("ten")
        End If
    End Sub
End Class

