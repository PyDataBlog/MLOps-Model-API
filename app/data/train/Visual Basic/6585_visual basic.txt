Public Class cmd
    Private Sub ExitToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExitToolStripMenuItem.Click
        Close()
    End Sub

    Private Sub CloseToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CloseToolStripMenuItem.Click
        Close()
    End Sub

    Private Sub EndFireplaceToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles EndFireplaceToolStripMenuItem.Click
        shutdown.Show()
    End Sub

    Private Sub AboutToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AboutToolStripMenuItem.Click
        cmd_about.Show()
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If TextBox2.Text = "hello" Then
            TextBox1.Text = "Hello!"
            TextBox2.Clear()
        ElseIf TextBox2.Text = "close" Then
            Close()
        ElseIf TextBox2.Text = "end" Then
            End
        Else
            TextBox1.Text = "Command not found!"
            TextBox2.Clear()
        End If
    End Sub
End Class