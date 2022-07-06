Public Class Clic
    Dim Touche
    Declare Sub mouse_event Lib "user32" Alias "mouse_event" (ByVal dwFlags As Integer, ByVal dx As Integer, ByVal dy As Integer, ByVal cButtons As Integer, ByVal dwExtraInfo As Integer)
    Public Const MOUSEEVENTF_LEFTDOWN = &H2
    Public Const MOUSEEVENTF_LEFTUP = &H4


    Private Sub Clic_Load(sender As Object, e As EventArgs) Handles Me.Load
        Button2.Hide()
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Button2.Show()
        Button1.Hide()
        Cliqueur.Start()

    End Sub
    Private Sub Cliqueur_Tick(sender As Object, e As EventArgs) Handles Cliqueur.Tick
        If My.Computer.Keyboard.CtrlKeyDown Or My.Computer.Keyboard.AltKeyDown Or My.Computer.Keyboard.ShiftKeyDown Then
            Clic()
        End If
    End Sub

    Sub Clic()
        mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0)
        mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0)
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Cliqueur.Stop()
        Button1.Show()
        Button2.Hide()
    End Sub


    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox1.CheckedChanged
        If CheckBox1.Checked = True Then
            Me.TopMost = True
        Else
            Me.TopMost = False
        End If
    End Sub

    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click
        Aether.Show()
        Me.Close()
    End Sub
End Class
