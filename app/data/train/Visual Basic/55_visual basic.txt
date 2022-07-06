Imports System.Linq.Expressions

Public Class Form1

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Switch8.Click
        Dim number As Integer
        If Integer.TryParse(Monitor.Text, number) Then
            If Integer.Parse(Monitor.Text) = 0 Then
                Monitor.Text = "8"
            Else
                Monitor.Text = Monitor.Text + "8"
            End If
        Else
            Monitor.Text = Monitor.Text + "8"
        End If
    End Sub

    Private Sub Switch0_Click(sender As Object, e As EventArgs) Handles Switch0.Click
        Dim number As Integer
        If Integer.TryParse(Monitor.Text, number) Then
            If Integer.Parse(Monitor.Text) = 0 Then
                Monitor.Text = "0"
            Else
                Monitor.Text = Monitor.Text + "0"
            End If
        Else
            Monitor.Text = Monitor.Text + "0"
        End If

    End Sub

    Private Sub Switch1_Click(sender As Object, e As EventArgs) Handles Switch1.Click
        Dim number As Integer
        If Integer.TryParse(Monitor.Text, number) Then
            If Integer.Parse(Monitor.Text) = 0 Then
                Monitor.Text = "1"
            Else
                Monitor.Text = Monitor.Text + "1"
            End If
        Else
            Monitor.Text = Monitor.Text + "1"
        End If
    End Sub

    Private Sub Switch2_Click(sender As Object, e As EventArgs) Handles Switch2.Click
        Dim number As Integer
        If Integer.TryParse(Monitor.Text, number) Then
            If Integer.Parse(Monitor.Text) = 0 Then
                Monitor.Text = "2"
            Else
                Monitor.Text = Monitor.Text + "2"
            End If
        Else
            Monitor.Text = Monitor.Text + "2"
        End If
    End Sub

    Private Sub Switch3_Click(sender As Object, e As EventArgs) Handles Switch3.Click
        Dim number As Integer
        If Integer.TryParse(Monitor.Text, number) Then
            If Integer.Parse(Monitor.Text) = 0 Then
                Monitor.Text = "3"
            Else
                Monitor.Text = Monitor.Text + "3"
            End If
        Else
            Monitor.Text = Monitor.Text + "3"
        End If
    End Sub

    Private Sub Switch4_Click(sender As Object, e As EventArgs) Handles Switch4.Click
        Dim number As Integer
        If Integer.TryParse(Monitor.Text, number) Then
            If Integer.Parse(Monitor.Text) = 0 Then
                Monitor.Text = "4"
            Else
                Monitor.Text = Monitor.Text + "4"
            End If
        Else
            Monitor.Text = Monitor.Text + "4"
        End If
    End Sub

    Private Sub Switch5_Click(sender As Object, e As EventArgs) Handles Switch5.Click
        Dim number As Integer
        If Integer.TryParse(Monitor.Text, number) Then
            If Integer.Parse(Monitor.Text) = 0 Then
                Monitor.Text = "5"
            Else
                Monitor.Text = Monitor.Text + "5"
            End If
        Else
            Monitor.Text = Monitor.Text + "5"
        End If
    End Sub

    Private Sub Switch6_Click(sender As Object, e As EventArgs) Handles Switch6.Click
        Dim number As Integer
        If Integer.TryParse(Monitor.Text, number) Then
            If Integer.Parse(Monitor.Text) = 0 Then
                Monitor.Text = "6"
            Else
                Monitor.Text = Monitor.Text + "6"
            End If
        Else
            Monitor.Text = Monitor.Text + "6"
        End If
    End Sub

    Private Sub Switch7_Click(sender As Object, e As EventArgs) Handles Switch7.Click
        Dim number As Integer
        If Integer.TryParse(Monitor.Text, number) Then
            If Integer.Parse(Monitor.Text) = 0 Then
                Monitor.Text = "7"
            Else
                Monitor.Text = Monitor.Text + "7"
            End If
        Else
            Monitor.Text = Monitor.Text + "7"
        End If
    End Sub

    Private Sub Switch9_Click(sender As Object, e As EventArgs) Handles Switch9.Click
        Dim number As Integer
        If Integer.TryParse(Monitor.Text, number) Then
            If Integer.Parse(Monitor.Text) = 0 Then
                Monitor.Text = "9"
            Else
                Monitor.Text = Monitor.Text + "9"
            End If
        Else
            Monitor.Text = Monitor.Text + "9"
        End If
    End Sub

    Private Sub SwitchDot_Click(sender As Object, e As EventArgs) Handles SwitchDot.Click
        Monitor.Text = "0"
    End Sub

    Private Sub SwitchEqualsTo_Click(sender As Object, e As EventArgs) Handles SwitchEqualsTo.Click
        Dim TextOnScreen As String = Monitor.Text
        Dim result = New DataTable().Compute(TextOnScreen, Nothing)
        Monitor.Text = result
    End Sub

    Private Sub SwitchDividedBy_Click(sender As Object, e As EventArgs) Handles SwitchDividedBy.Click
        Monitor.Text = Monitor.Text + "/"
    End Sub

    Private Sub SwitchMultipliedBy_Click(sender As Object, e As EventArgs) Handles SwitchMultipliedBy.Click
        Monitor.Text = Monitor.Text + "*"
    End Sub

    Private Sub SwitchMinus_Click(sender As Object, e As EventArgs) Handles SwitchMinus.Click
        Monitor.Text = Monitor.Text + "-"
    End Sub

    Private Sub SwitchPlus_Click(sender As Object, e As EventArgs) Handles SwitchPlus.Click
        Monitor.Text = Monitor.Text + "+"
    End Sub
End Class
