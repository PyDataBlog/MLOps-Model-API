Imports System
Imports System.Text
Imports System.Collections
Imports System.DirectoryServices
Imports System.IO

Public Class Main

    'Sets up varibles.
    Public loginName As String
    Public userName As String
    Public Results(5) As String '1 = Profile folder, 2 = Homdir Folder, 3 to 5 = Appdata folders
    Public path As String

    'Sets up code files for use
    Dim File_Deletion As New File_Deletion()
    Dim AD As New AD()

    Private Sub cmdRun_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdRun.Click

        userName = txtUser.Text
        path = "LDAP://" + My.Settings.LDAP_IP + "/CN=" + userName + ", " + My.Settings.LDAP_Path

        If txtUser.Text = "" Then 'Validation to make sure there is a users name entered
            MessageBox.Show("Please enter a users name", "Enter Text")
        Else
            If AD.VerifyUser(userName) = False Then 'Validation to make sure the user exists in AD
                MessageBox.Show(userName + " not found", "User Not Found")
            Else
                AD.GetLoginName() 'Grab the username of the user
                File_Deletion.Deletions() 'Function to delete user's folders
            End If
        End If

    End Sub

    Public Sub cmdAD_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdAD.Click

        'userName = txtUser.Text
        userName = lstUsers.SelectedItem
        path = "LDAP://" + My.Settings.LDAP_IP + "/CN=" + userName + ", " + My.Settings.LDAP_Path

        If txtUser.Text = "" Then 'Validation to make sure there is a user name entered
            MessageBox.Show("Please enter a users name", "Enter Text")
        Else
            If AD.VerifyUser(userName) = False Then 'Validation to make sure the username exists in AD
                MessageBox.Show(userName + " not found", "User Not Found")
            Else
                If MessageBox.Show("You are about to delete " + userName + "'s AD account are you sure you want to do this", _
                                           "AD Deletion", MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation) _
                                           = Windows.Forms.DialogResult.OK Then
                    AD.DeleteUser(userName)
                    MessageBox.Show(userName + "'s AD Account Deleted")
                    'First asks user if they definatly want to delete the user from AD.
                    'If true it then passes of to function for deleting.
                End If
            End If
        End If

    End Sub

    Private Sub ExitToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ExitToolStripMenuItem.Click
        End
    End Sub

    Private Sub Leavers_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        AD.SetCultureAndIdentity()

        For Each item In AD.ListADUsers()

            Dim strItem As String = item.ToString()
            'Dim strItemMod As String = strItem.Substring(3)
            lstUsers.Items.Add(strItem)
        Next

    End Sub

    Private Sub AboutToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AboutToolStripMenuItem.Click
        Dim About As New About
        About.Show()
    End Sub

    Private Sub SettingsToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SettingsToolStripMenuItem.Click
        Dim Settings As New Config
        Config.Show()
    End Sub

    Private Sub txtUser_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtUser.KeyDown
        If (e.KeyValue = Keys.Enter) Then
            cmdRun_Click(sender, e)
        End If
    End Sub

End Class