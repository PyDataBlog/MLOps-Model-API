'Will Eccles
'Date: 12/18/13
'Project: Final Project - Browser
'Language: VB.net

Option Explicit On
Imports System.Windows.Forms
Imports System.IO

Public Class Form2
    'When you click the OK button...
    Private Sub OK_Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OK_Button.Click
		Me.DialogResult = System.Windows.Forms.DialogResult.OK
		Me.Close()
		My.Settings.UserHomePage = TBox_HomeSet.Text
		My.Settings.Save()
        'Save data.
        If rbut_no.Checked = True Then
			My.Settings.Rbut2chosen = True
			My.Settings.Rbut1chosen = False
			My.Settings.Save()
		ElseIf rbut_yes.Checked = True Then
			My.Settings.Rbut1chosen = True
			My.Settings.Rbut2chosen = False
			My.Settings.Save()
		End If
	End Sub
    'Close app.
    Private Sub Cancel_Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Cancel_Button.Click
		Me.DialogResult = System.Windows.Forms.DialogResult.Cancel
		Me.Close()
	End Sub

	Private Sub Dialog1_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load
        'WRE when you open settings, set box to home page.
        TBox_HomeSet.Text = My.Settings.UserHomePage
		If My.Settings.Rbut1chosen = True Then
			rbut_yes.Checked = True
		ElseIf My.Settings.Rbut2chosen = True Then
			rbut_no.Checked = True
		End If
	End Sub
    'Change the home page back to default.
    Private Sub But_HPageReset_Click(sender As System.Object, e As System.EventArgs) Handles But_HPageReset.Click
        TBox_HomeSet.Text = Path.GetFullPath("Home.html")
        My.Settings.UserHomePage = TBox_HomeSet.Text
        My.Settings.Save()
    End Sub
End Class
