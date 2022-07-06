Public Class ChronoC
    Dim time = 0
    Dim min = 0
    Dim heure = 0

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles Me.Load
        Label1.Text = min & " minutes"
        Label2.Text = time & " secondes"
        Label3.Text = heure & " heures"
    End Sub
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Timer1.Start()
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        time = time + 1
        If time = 60 Then
            min = min + 1
            time = 0
        End If
        If min = 60 Then
            heure = heure + 1
            min = 0
        End If
        Label1.Text = min & " minutes"
        Label2.Text = time & " secondes"
        Label3.Text = heure & " heures"
    End Sub


    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Timer1.Stop()
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        min = 0
        time = 0
        heure = 0
        Label1.Text = min & " minutes"
        Label2.Text = time & " secondes"
        Label3.Text = heure & " heures"
        Timer1.Start()
    End Sub

    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click
        Aether.Show()
        Me.Close()
    End Sub
End Class
