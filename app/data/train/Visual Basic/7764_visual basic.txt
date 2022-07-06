Imports ITT_Admin.DBFunction
Imports ITT_Admin.Variables

Public Class UserAdministration

    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click

    End Sub

    Private Sub UserAdministration_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Dim users As DataSet = getUsers()

        ' cbxUsers.Items.Add("--- New User ---")

        For Each row As DataRow In users.Tables.Item(0).Rows
            cbxUsers.Items.Add(row.Item("name").ToString & " (" & row.Item("user_Id").ToString & ")")

        Next

        '   cbxUsers.Items.RemoveAt(0)


    End Sub



    Private Sub btnOK_Click(sender As Object, e As EventArgs) Handles btnOK.Click


        Dim v_chngPasswd As String = "Y"
        Dim v_isAdmin As String = "N"

        If chkChangePassword.Checked = False Then
            v_chngPasswd = "N"
        End If

        If chkIsAdmin.Checked Then
            v_isAdmin = "Y"
        End If

        If txtUserId.Text = "" Then
            MsgBox("Please fill out username")
            txtUserId.Focus()
        Else

            If isValidUser(txtUserId.Text) Then
                updateUser(txtUserId.Text, txtFullname.Text, v_chngPasswd, v_isAdmin)

            Else
                createUser(txtUserId.Text, "password", txtFullname.Text, v_chngPasswd, v_isAdmin)

            End If


        End If
    End Sub

    Private Sub btnCancel_Click(sender As Object, e As EventArgs) Handles btnCancel.Click
        Admin.Show()
        Me.Close()

    End Sub

    Private Sub cbxUsers_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbxUsers.SelectedIndexChanged
        ' MsgBox(cbxUsers.SelectedItem.ToString)
        txtFullname.Text = cbxUsers.SelectedItem.ToString.Substring(0, cbxUsers.SelectedItem.ToString.IndexOf("(") - 1)
        '  MsgBox(txtFullname.Text)
        Dim userData As String = cbxUsers.SelectedItem.ToString.Substring(cbxUsers.SelectedItem.ToString.IndexOf("(") + 1)
        'getUserId(txtFullname.Text) 
        userData = userData.Substring(0, userData.ToString.IndexOf(")"))
        txtUserId.Text = userData
        ' MsgBox(txtUserId.Text)

        Dim chkData As String = isUserAdmin(txtUserId.Text)

        If chkData = "Y" Then
            chkIsAdmin.Checked = True
        Else
            chkIsAdmin.Checked = False
        End If

        chkData = getChangePassword(txtUserId.Text)
        If chkData = "Y" Then
            chkChangePassword.Checked = True
        Else
            chkChangePassword.Checked = False
        End If


    End Sub
End Class