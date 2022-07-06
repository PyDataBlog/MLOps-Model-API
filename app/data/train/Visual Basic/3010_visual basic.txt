Imports System.ComponentModel
Imports System.IO
Imports System.Environment
'Lock Icons: http://creativecommons.org/licenses/by/3.0/
'Save and remove icons: http://creativecommons.org/licenses/by-nd/3.0/
'Source: https://www.iconfinder.com

'Status icons: http://www.icons-land.com

Public Class fm_main
    Public block_status As Boolean
    Public writehosts_back As String
    Public hosts_path As String = "C:\Windows\System32\drivers\etc\hosts"
    Dim appdatapath As String = GetFolderPath(SpecialFolder.ApplicationData) 'app data path
    Dim fullapppath As String = appdatapath & "\Blockify" 'path where blockify will save files
    Public sites_save_path As String = fullapppath & "\sites.bat" ' path for list of websites
    Public hosts_bkup_save_path As String = fullapppath & "\hosts_bkup" 'path for hosts file backup
    Dim sw As StreamWriter
    Dim pw As String
    Dim sites As String = ""
    Dim edge() As Process ' dim browser processes
    Dim chrome() As Process 'google chrome
    Dim firefox() As Process 'firefox
    Dim opera() As Process  'opera
    Dim safari() As Process 'safari
    Dim iexplore() As Process   'internet explorer
    Dim browser_proc_names As New ArrayList 'list of browser process names for process killing


    Private Sub hide_inputs() 'initial controls from form (password field, label and save button)
        lbl_enterPw.Visible = False
        btn_savePw.Visible = False
        tb_savePw.Visible = False
    End Sub
    Private Sub show_inputs() 'show initial controls on form
        lbl_enterPw.Visible = True
        tb_savePw.Visible = True
        btn_savePw.Visible = True
    End Sub

    Private Sub block_websites() 'perform block
        Try
            sw = New StreamWriter(hosts_path, True)
            Dim sitetoblock As String = (Environment.NewLine & "127.0.0.1 " & sites) 'has to be www.google.com or google.com | NOT: http://www.google.com/
            sw.Write(sitetoblock)
            sw.Close()
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try

    End Sub

    Private Sub get_running_browsers() 'browser process names for running check

        chrome = Process.GetProcessesByName("chrome")
        edge = Process.GetProcessesByName("MicrosoftEdge")
        firefox = Process.GetProcessesByName("firefox")
        opera = Process.GetProcessesByName("opera")
        safari = Process.GetProcessesByName("Safari")
        iexplore = Process.GetProcessesByName("iexplore")
    End Sub
    Private Sub browser_process_arraylist() ' browser process names added to arraylist

        browser_proc_names.Add("chrome")
        browser_proc_names.Add("MicrosoftEdge")
        browser_proc_names.Add("firefox")
        browser_proc_names.Add("opera")
        browser_proc_names.Add("Safari")
        browser_proc_names.Add("iexplore")
    End Sub
    Private objMutex As System.Threading.Mutex 'Mutex

    Private Sub check_if_already_running()
        'Check to prevent running twice
        objMutex = New System.Threading.Mutex(False, "MyApplicationName")
        If objMutex.WaitOne(0, False) = False Then
            objMutex.Close()
            objMutex = Nothing
            MessageBox.Show("Another instance Is already running!")
            End
        End If
        'If you get to this point it's first instance

    End Sub


    Private Sub fm_main_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.Height = 170

        Try
            check_if_already_running() 'check if app already running
            block_status = False 'Set block bool to false

            If My.Settings.Password = "" Then
                MsgBox("Please enter a password")
                Me.Height = 250
                btn_block.Enabled = False
                btn_unblock.Enabled = False
                MainMenuStrip.Items.Item(0).Enabled = False 'disable actions toolstrip
                show_inputs()
                tb_savePw.Focus()
            End If

            'check if a list of websites exist

            If File.Exists(sites_save_path) Then
                Dim blocked_websites As String
                Using reader As StreamReader = New StreamReader(sites_save_path)

                    blocked_websites = reader.ReadToEnd
                End Using
                tb_blockedSites.Text = blocked_websites

            Else
                tb_blockedSites.Text = "No websites are currently being blocked."
            End If


            'TODO Update about me.


            If Directory.Exists(fullapppath) = False Then
                Directory.CreateDirectory(appdatapath & "\Blockify")
            End If

            If File.Exists(fullapppath & "\hosts_bkup") = False Then
                File.Copy(hosts_path, fullapppath & "\hosts_bkup", True)
            End If

        Catch ex As Exception
            MsgBox(ex.Message)
        End Try

    End Sub

    Private Sub fm_main_Closing(sender As Object, e As CancelEventArgs) Handles Me.Closing
        If My.Settings.Password <> "" Then
            fm_enterpw_closing.Show()
            e.Cancel = True 'cancel form closing. form called fm_enterpw_closing will be responsible for closing application
        End If
    End Sub

    Private Sub TabControl1_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TabControl1.SelectedIndexChanged

        Dim i As Integer
        i = TabControl1.SelectedIndex
        If i = 1 Then
            Height = 250
        ElseIf My.Settings.Password = "" Then
            Height = 250
        Else
            Height = 170
        End If

    End Sub

    Private Sub BlockSitesToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles BlockSitesToolStripMenuItem.Click
        fm_enterPw.Show()
    End Sub


    Private Sub btn_savePw_Click(sender As Object, e As EventArgs) Handles btn_savePw.Click
        Try
            pw = tb_savePw.Text

            My.Settings.Password = pw
            My.Settings.Save()
            MsgBox("Password Saved!", vbInformation, "")
            Me.Height = 165
            hide_inputs()

            btn_block.Enabled = True
            btn_unblock.Enabled = True
            MainMenuStrip.Items.Item(0).Enabled = True 'enable actions toolstrip
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try


    End Sub

    Private Sub btn_block_Click(sender As Object, e As EventArgs) Handles btn_block.Click
        Try

            get_running_browsers() 'check if browsers opened
            If edge.Count > 0 Or chrome.Count > 0 Or firefox.Count > 0 Or opera.Count > 0 Or safari.Count > 0 Or iexplore.Count > 0 Then
                If MessageBox.Show("You currently have a web browser opened. Click ""Yes"" if you would Like this program to automatically close it/them AFTER you have saved your work. ", "Opened Web Browser Detected", MessageBoxButtons.YesNo) = DialogResult.Yes Then

                    browser_process_arraylist()
                    Dim i As Integer
                    For i = 0 To browser_proc_names.Count - 1 'loop through array list
                        Dim proc_name As String = browser_proc_names(i).ToString

                        For Each p As Process In Process.GetProcessesByName(proc_name) 'kill processes of browsers within arraylist

                            p.Kill() 'kill process

                        Next
                    Next
                    GoTo continueblocking
                Else
                    MessageBox.Show("Please try clicking the block button again after you have saved your work and closed all browsers.", "Opened Web Browser Detected")

                End If
            Else
continueblocking:
                If block_status = False Then
                    If File.Exists(sites_save_path) Then 'check for the sites to block

                        Dim Numofwebsites As Integer = File.ReadAllLines(sites_save_path).Length

                        Dim sr As New StreamReader(sites_save_path)

                        Do Until sr.Peek = -1
                            sites = sr.ReadLine()
                            block_websites() 'Block the websites
                        Loop
                        sr.Close() 'close the stream reader
                        MessageBox.Show("Block Activated.")
                        pb_status.Image = My.Resources.Circle_Green

                        lbl_status.Text = "Blocking" & " " & Numofwebsites & " " & "websites."
                        lbl_status.Visible = True
                        block_status = True
                    Else
                        MsgBox("Please enter websites to block", vbInformation, "Block Websites")
                        fm_blockSites.Show()
                    End If
                Else
                    MsgBox("Websites already being blocked.")
                End If
            End If

        Catch ex As Exception
            MsgBox(ex.Message)
        End Try



    End Sub

    Private Sub BlockedWebsitesToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles BlockedWebsitesToolStripMenuItem.Click
        fm_change_pw.Show()
    End Sub

    Private Sub btn_unblock_Click(sender As Object, e As EventArgs) Handles btn_unblock.Click
        Try
            If block_status = True Then
                fm_enterPw_unblock.Show()

            Else
                MsgBox("No websites are currently being blocked.", vbInformation, "")
            End If
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try

    End Sub

    Private Sub AboutToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AboutToolStripMenuItem.Click
        fm_about.Show()
    End Sub

    Private Sub ToolStripMenuItem1_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem1.Click
        If File.Exists(sites_save_path) Then
            fm_enterpw_editblocked_websites_list.Show()
        Else
            MsgBox("No websites are currently being blocked. Please block a website first", vbInformation, "")
        End If


    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs)
        My.Settings.Password = ""
    End Sub

    Private Sub tb_savePw_TextChanged(sender As Object, e As EventArgs) Handles tb_savePw.TextChanged
        btn_savePw.Enabled = True
    End Sub

    Private Sub tb_savePw_KeyDown(sender As Object, e As KeyEventArgs) Handles tb_savePw.KeyDown
        If e.KeyCode = Keys.Enter Then
            btn_savePw.PerformClick()
        End If


    End Sub
End Class