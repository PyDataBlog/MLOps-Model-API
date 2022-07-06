Public Class Form1
    Dim a(), i, r As Integer
    Private Sub Form1_Load(ByVal sender As Object, _
    ByVal e As System.EventArgs) Handles MyBase.Load
        r = InputBox("Enter size of the array")
        ReDim a(r - 1)
        For i = 0 To r - 1
            a(i) = InputBox("Enter array element")
            ListView1.Items.Add(a(i))
        Next i
        ListView1.Visible = True
    End Sub
    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        End
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim n, pos As Integer
        pos = -1
        n = InputBox("Enter number to search")
        For i = 0 To r - 1
            If Val(n) = Val(a(i)) Then
                pos = i
            End If
        Next i
        If pos = -1 Then
            MsgBox("Item not present")
        Else
            MsgBox("Item present at position " & (pos + 1))
        End If
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim j As Integer
        For i = 0 To r - 2
            For j = 0 To r - 2 - i
                If Val(a(j)) > Val(a(j + 1)) Then
                    Dim temp = a(j + 1)
                    a(j + 1) = a(j)
                    a(j) = temp
                End If
            Next j
        Next i
        For i = 0 To r - 1
            ListView2.Items.Add(a(i))
        Next i
    End Sub
End Class
