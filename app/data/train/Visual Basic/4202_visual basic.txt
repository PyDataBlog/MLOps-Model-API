Public Class screenLoad

    Private Sub screenLoad_Load(sender As Object, e As EventArgs) Handles Me.Load
        Timer.Enabled = True
    End Sub

    Private Sub Timer_Tick(sender As Object, e As EventArgs) Handles Timer.Tick
        My.Computer.Audio.Play(My.Resources.BatmanOpening, AudioPlayMode.WaitToComplete)
        screenHome.Show()
        Timer.Enabled = False
    End Sub
End Class