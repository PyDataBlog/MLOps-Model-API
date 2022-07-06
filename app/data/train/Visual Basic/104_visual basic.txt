Imports System.Data
Imports System.Data.SqlClient

Public Class ForgotPasswordForm

    ' We use disconnected architecture here because we need to show the security question first, if the user gives us the proper username
    Dim conn As SqlConnection
    Dim ds As DataSet
    Dim adapter As SqlDataAdapter

    ' This is the row that contains the username, we will need this to check the securityQuestion against the answer
    Dim matchingUserRow As DataRow

    Private Sub ShowPasswordButton_Click(sender As Object, e As EventArgs) Handles ShowPasswordButton.Click
        For Each row In ds.Tables("SQ").Rows
            If row("username") = UsernameTextBox.Text And row("answer") = AnswerTextBox.Text Then
                MsgBox("Password: " & row("password"))
            End If
        Next
    End Sub

    Private Sub UsernameTextBox_TextChanged(sender As Object, e As EventArgs) Handles UsernameTextBox.TextChanged
        ' First check if user name exists

        For Each row In ds.Tables("SQ").Rows
            If row("username") = UsernameTextBox.Text Then
                SecurityQuestionTextBox.Text = row("securityQuestion")
                Exit For
            End If
        Next

    End Sub

    Private Sub ForgotPasswordForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        conn = New SqlConnection("Data Source=(LocalDB)\v11.0;AttachDbFilename=C:\Users\dhaval\Documents\logindb.mdf;Integrated Security=True;Connect Timeout=30")
        ds = New DataSet
        adapter = New SqlDataAdapter("SELECT * FROM securityQuestions", conn)
        adapter.Fill(ds, "SQ")
    End Sub
End Class