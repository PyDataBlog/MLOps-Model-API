'Made by: Benjamin Blackwell, James Stewart, and Jack Helm.
'Date Worked on:4/6/16
'The purpose of this form is to allow the user to set the custom commands.
Public Class Form16
    Private Sub Form16_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        txtName1.Text = My.Settings.Command1Name
        txtName2.Text = My.Settings.Command2Name
        txtName3.Text = My.Settings.Command3Name
        txtName4.Text = My.Settings.Command4Name
        txtName5.Text = My.Settings.Command5Name
        txtCommand1.Text = My.Settings.NewCommand1
        txtCommand2.Text = My.Settings.NewCommand2
        txtCommand3.Text = My.Settings.NewCommand3
        txtCommand4.Text = My.Settings.NewCommand4
        txtCommand5.Text = My.Settings.NewCommand5
    End Sub
    Private Sub btnSubmit_Click(sender As Object, e As EventArgs) Handles btnSubmit.Click
        My.Settings.Command1Name = txtName1.Text
        My.Settings.Command2Name = txtName2.Text
        My.Settings.Command3Name = txtName3.Text
        My.Settings.Command4Name = txtName4.Text
        My.Settings.Command5Name = txtName5.Text
        My.Settings.NewCommand1 = txtCommand1.Text
        My.Settings.NewCommand2 = txtCommand2.Text
        My.Settings.NewCommand3 = txtCommand3.Text
        My.Settings.NewCommand4 = txtCommand4.Text
        My.Settings.NewCommand5 = txtCommand5.Text
        MainForm.bolIndexStart = False
        MainForm.RefreshCommands()
        Me.Close()
    End Sub
End Class