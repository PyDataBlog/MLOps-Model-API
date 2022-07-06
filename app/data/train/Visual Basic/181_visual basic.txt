Public Class ManagerCP


    Private Sub ManagerCP_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        LoggedInStr.Text = "Logged in as: " + LoginForm.UserName
        LoggedInStr.Refresh()

    End Sub

    Private Sub LogoutButton_Click(sender As Object, e As EventArgs) Handles LogoutButton.Click
        Me.Close()
        LoginForm.LogOut()
    End Sub

    Private Sub ViewEmployeeButton_Click(sender As Object, e As EventArgs) Handles ViewEmployeeButton.Click
        EmployeeInfo.Show()
    End Sub

    Private Sub MoviesButton_Click(sender As Object, e As EventArgs) Handles MoviesButton.Click
        MovieInfo.Show()
    End Sub

    Private Sub ActorButton_Click(sender As Object, e As EventArgs) Handles ActorButton.Click
        ActorInfo.Show()
    End Sub
End Class