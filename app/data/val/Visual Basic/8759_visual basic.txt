Public Class Config

    Private Sub Config_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        txtIP.Text = My.Settings.LDAP_IP
        txtPath.Text = My.Settings.LDAP_Path
        txtPassword.Text = My.Settings.Password
        txtHomedirs.Text = My.Settings.Homdirs
        txtProfile.Text = My.Settings.Profile
        txtAppdataTerminal.Text = My.Settings.appdata_terminal
        txtAppdataLaptop.Text = My.Settings.appdata_laptop
        txtAppdataWorkstation.Text = My.Settings.appdata_workstation
    End Sub

    Private Sub cmdSave_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdSave.Click
        My.Settings.LDAP_IP = txtIP.Text
        My.Settings.LDAP_Path = txtPath.Text
        My.Settings.Password = txtPassword.Text
        My.Settings.Homdirs = txtHomedirs.Text
        My.Settings.Profile = txtProfile.Text
        My.Settings.appdata_terminal = txtAppdataTerminal.Text
        My.Settings.appdata_laptop = txtAppdataLaptop.Text
        My.Settings.appdata_workstation = txtAppdataWorkstation.Text
        My.Settings.Save()
        MessageBox.Show("Settings Saved")
    End Sub

    Private Sub cmdClose_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdClose.Click
        Close()
    End Sub

    Private Sub cmdDefault_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdDefault.Click
        If MessageBox.Show("This will reset setting to default are you sure", "Default Settings", _
                        MessageBoxButtons.OKCancel, MessageBoxIcon.Question) _
                        = Windows.Forms.DialogResult.OK Then
            My.Settings.Reset()
            txtIP.Text = My.Settings.LDAP_IP
            txtPath.Text = My.Settings.LDAP_Path
            txtPassword.Text = My.Settings.Password
            txtHomedirs.Text = My.Settings.Homdirs
            txtProfile.Text = My.Settings.Profile
            txtAppdataTerminal.Text = My.Settings.appdata_terminal
            txtAppdataLaptop.Text = My.Settings.appdata_laptop
            txtAppdataWorkstation.Text = My.Settings.appdata_workstation
        End If
    End Sub
End Class