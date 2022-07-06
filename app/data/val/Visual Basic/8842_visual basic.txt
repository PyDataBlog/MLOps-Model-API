Public Class Form1
    Dim counter As Integer


    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Timer1.Start()
        counter = 0
        Panel1.BackColor = Color.Red

    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        counter += 1
        If counter = 50 Then
            Panel1.BackColor = Control.DefaultBackColor
            Panel2.BackColor = Color.Yellow
        End If
        If counter = 70 Then
            Panel1.BackColor = Control.DefaultBackColor
            Panel2.BackColor = Control.DefaultBackColor
            Panel3.BackColor = Color.Green
        End If
    End Sub
End Class
