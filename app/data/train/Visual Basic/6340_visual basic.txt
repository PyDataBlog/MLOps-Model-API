Public Class Form1

    Dim Hit As Integer, score As Integer, live As Integer = 3

    Private Sub Form1_Click(sender As Object, e As EventArgs) Handles Me.Click

        score = score * Hit 'multiplie le score par le hit
        Hit = 0
        My.Computer.Audio.Play("‪son.wav") 'joue le son
        live = live - 1 'deduit une vie

        Select Case live 'affiche les croix
            Case 2
                PictureBox1.BackgroundImage = My.Resources.Sans_titre
            Case 1
                PictureBox2.BackgroundImage = My.Resources.Sans_titre
            Case 0
                PictureBox3.BackgroundImage = My.Resources.Sans_titre
        End Select

        If live = 0 Then 'detect quand il y a plus de vie

            StartBt.Enabled = True
            Timer1.Stop()

        End If



    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles StartBt.Click


        score = 0 'remet tou a zero
        Hit = 0
        live = 3
        PictureBox1.BackgroundImage = Nothing
        PictureBox2.BackgroundImage = Nothing
        PictureBox3.BackgroundImage = Nothing

        'tim = True 'active le jeux
        StartBt.Enabled = False
        Gollum.Enabled = True
        Timer1.Start()

    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick

        If Gollum.Location.X < Me.Width Then 'calcule de la position de Gollum

            Dim d As Int16 = Gollum.Location.X
            d = d + 1 + Hit / 2
            Gollum.Location = New Point(d, Me.Gollum.Location.Y)

        Else

            Gollum.Location = New Point(-45, Me.Gollum.Location.Y)

        End If

        Label1.Text = "Hit : " + Hit.ToString 'affiche les scores
        Label2.Text = "Score : " + score.ToString

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Gollum.Click

        Hit = Hit + 1 'ajout aux scores
        score = score + 1

    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click

        Form2.Show()

    End Sub

    Private Sub Form1_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        Application.Exit()
    End Sub
End Class
