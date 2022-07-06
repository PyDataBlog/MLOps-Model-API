Imports System.IO

Public Class About
    Private Sub About_Load(ByVal sender As Object, ByVal e As EventArgs) Handles MyBase.Load
        'Reads from about.txt
        Using reader As StreamReader = New StreamReader("Input/about.txt", False)
            TextBox.Text = reader.ReadToEnd() ' read eveything in a richtext box
        End Using
        '
    End Sub

End Class