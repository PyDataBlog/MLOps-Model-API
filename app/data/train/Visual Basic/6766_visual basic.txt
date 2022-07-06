Imports System.IO
Public Class FileChecksNotifier

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        FirmwareDownloader.Text = "Select iOS - iFirmware"
        FirmwareDownloader.Label1.Text = "Select iOS"
        FirmwareDownloader.Label2.Text = "Select a specific iOS version you wish to download"
        FirmwareDownloader.ComboBox1.Enabled = True
        FirmwareDownloader.DownloadButton.Enabled = True
        FirmwareDownloader.ProgressLabel1.Visible = False
        FirmwareDownloader.ProgressBar1.Visible = False
        FirmwareDownloader.ProgressLabel1.Text = "Progress..."
        Me.Close()
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        If FirmwareDownloader.ComboBox1.Text = "Apple TV 2 (iOS 4.1) [8M89]" Then
            If File.Exists("C:\iFirmware\AppleTV2,1_4.1_8M89_Restore.ipsw") Then
                File.Delete("C:\iFirmware\AppleTV2,1_4.1_8M89_Restore.ipsw")
            End If
            FirmwareDownloader.iFirmwareBGRunner.RunWorkerAsync()
            Me.Close()
        End If
        If FirmwareDownloader.ComboBox1.Text = "Apple TV 2 (iOS 4.2.1) [8C154" Then
            If File.Exists("C:\iFirmware\AppleTV2,1_4.2.1_8C154_Restore.ipsw") Then
                File.Delete("C:\iFirmware\AppleTV2,1_4.2.1_8C154_Restore.ipsw")
            End If
            FirmwareDownloader.iFirmwareBGRunner.RunWorkerAsync()
            Me.Close()
        End If
        If FirmwareDownloader.ComboBox1.Text = "Apple TV 2 (iOS 4.3) [F191m]" Then
            If File.Exists("C:\iFirmware\AppleTV2,1_4.3_8F191m_Restore.ipsw") Then
                File.Delete("C:\iFirmware\AppleTV2,1_4.3_8F191m_Restore.ipsw")
            End If
            FirmwareDownloader.iFirmwareBGRunner.RunWorkerAsync()
            Me.Close()
        End If
        If FirmwareDownloader.ComboBox1.Text = "Apple TV 2 (iOS 5.1) [10A406e]" Then
            If File.Exists("C:\iFirmware\AppleTV2,1_5.1_10A406e_Restore.ipsw") Then
                File.Delete("C:\iFirmware\AppleTV2,1_5.1_10A406e_Restore.ipsw")
            End If
            FirmwareDownloader.iFirmwareBGRunner.RunWorkerAsync()
            Me.Close()
        End If
    End Sub

    Private Sub FileChecksNotifier_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

    End Sub
End Class
