Imports System.Data
Imports System.Data.SqlClient
Imports Basic.DAL
Imports Basic.DAL.Utils
Imports Basic.Constants.ProjConst

Public Class frmPostedPass
    Inherits System.Windows.Forms.Form

    'Dim objAccount As New cAccounts

    Private Sub btnCancel_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnCancel.Click
        Me.Close()
    End Sub

    Private Sub btnOK_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnOK.Click
        If Trim(txtPassword.Text) = "" Then
            MsgBox("Please Enter Password", vbInformation, SysCompany)
            txtPassword.Focus()
            Exit Sub
        ElseIf Trim(txtPassword.Text) <> SysPostedPass Then
            MsgBox("Invalid Password", vbInformation, SysCompany)
            txtPassword.Focus()
            Exit Sub
        ElseIf Trim(txtPassword.Text) = SysPostedPass Then
            Me.Close()
            frmPostedVouchers.ShowDialog()
        End If
    End Sub

    Private Sub frmPostedPass_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        mEPostVoch = False
    End Sub

    Private Sub frmPostedPass_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.GotFocus

    End Sub

    Private Sub frmPostedPass_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Me.MdiParent = frmMdi
        'Me.WindowState = FormWindowState.Normal
        txtPassword.Text = ""
    End Sub

    Private Sub txtPassword_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtPassword.KeyDown
        If e.KeyCode = Keys.Enter Then
            SendKeys.Send("{Tab}")
        End If
    End Sub

    Private Sub frmPostedPass_SizeChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.SizeChanged
        Me.WindowState = FormWindowState.Normal
    End Sub
End Class