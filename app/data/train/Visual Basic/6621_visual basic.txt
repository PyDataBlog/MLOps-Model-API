Imports System.Net
Imports System.Net.Sockets

Public Class GlobalSettings

    Public Sub CheckIP()
        Try
            If TxbIP1.Text > 255 Or TxbIP1.Text < 0 Then
                MsgBox("enter number between 0 & 255")
                Return
            ElseIf TxbIP2.Text > 255 Or TxbIP2.Text < 0 Then
                MsgBox("enter number between 0 & 255")
                Return
            ElseIf TxbIP3.Text > 255 Or TxbIP3.Text < 0 Then
                MsgBox("enter number between 0 & 255")
                Return
            ElseIf TxbIP4.Text > 255 Or TxbIP4.Text < 0 Then
                MsgBox("enter number between 0 & 255")
                Return
            ElseIf TxbPort.Text > 65535 Or TxbPort.Text < 0 Then
                MsgBox("enter number between 0 & 65535")
                Return
            End If
        Catch ex As Exception
            MsgBox("Error get IP or Port , try one more time")
            Return
        End Try
    End Sub
    Public Function IsPortOpen(ByVal Host As String, ByVal PortNumber As Integer) As Boolean
        Dim Client As TcpClient = Nothing
        Try
            Client = New TcpClient(Host, PortNumber)
            Return True
        Catch ex As SocketException
            Return False
        Finally
            If Not Client Is Nothing Then
                Client.Close()
            End If
        End Try
    End Function

    Private Sub BtnSave_Click(sender As Object, e As EventArgs) Handles BtnSave.Click
        My.Settings.Save()
        Me.Close()
    End Sub

    Private Sub BtnSmtpCancel_Click(sender As Object, e As EventArgs) Handles BtnSmtpCancel.Click
        My.Settings.Reload()
    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles WindowsStartChb.CheckedChanged
        Dim applicationName As String = Application.ProductName
        Dim applicationPath As String = Application.ExecutablePath

        If WindowsStartChb.Checked Then
            Dim regKey As Microsoft.Win32.RegistryKey
            regKey = Microsoft.Win32.Registry.CurrentUser.OpenSubKey("SOFTWARE\Microsoft\Windows\CurrentVersion\Run", True)
            regKey.SetValue(applicationName, """" & applicationPath & """")
            regKey.Close()
        Else
            Dim regKey As Microsoft.Win32.RegistryKey
            regKey = Microsoft.Win32.Registry.CurrentUser.OpenSubKey("SOFTWARE\Microsoft\Windows\CurrentVersion\Run", True)
            regKey.DeleteValue(applicationName, False)
            regKey.Close()
        End If
    End Sub
End Class