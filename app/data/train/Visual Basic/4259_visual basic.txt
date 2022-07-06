Namespace BasicWM
    Public Class Desktop

        Public openterminals As Integer = 0

        Private Sub btnnewterminal_Click(sender As Object, e As EventArgs) Handles btnnewterminal.Click
            If openterminals = 3 Then
                AddLine("The maximum ammount of Terminal windows are open.")
                AddLine(username + "@" + osname + " " & Terminal.currentdir.ToLower.Replace("c:\shiftos", "~") & "$> ")
            Else
                Dim term As New Terminal
                term.StartPosition = FormStartPosition.CenterParent
                term.TopMost = True
                term.Show()
                openterminals += 1
            End If
        End Sub

        Private Sub Desktop_Load(sender As Object, e As EventArgs) Handles MyBase.Load
            Terminal.Close()
            Me.WindowState = FormWindowState.Maximized
            Dim term As New Terminal
            term.StartPosition = FormStartPosition.CenterParent
            term.TopMost = True
            term.Show()
            openterminals += 1
        End Sub
    End Class
End Namespace