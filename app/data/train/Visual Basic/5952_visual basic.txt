Imports CUE.NET
Public Class frmMain
    Dim keyboard As Devices.Keyboard.CorsairKeyboard
    Dim arguments As String()

    Private Sub frmMain_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Visible = False
        arguments = Environment.GetCommandLineArgs

        If arguments.Length <> 3 Then
            MsgBox("Error: Incorrect number of parameters supplied." & vbNewLine & "Syntax: " & """" & "CueBright.exe [hex color (#xxxxxx)] [brightness (0.0-1.0)]" & """", MsgBoxStyle.Critical, "Error")
            Application.Exit()
        Else
            If DetectLockState.IsWorkstationLocked() = False Then 'If unlocked
                InitializeSDK()

                SetLEDs() 'Set LEDs
            End If

            AddHandler Microsoft.Win32.SystemEvents.SessionSwitch, AddressOf WindowsLocked 'Add handler for lock/unlock event
        End If
    End Sub

    Private Sub WindowsLocked(sender As Object, e As Microsoft.Win32.SessionSwitchEventArgs)
        If e.Reason = Microsoft.Win32.SessionSwitchReason.SessionUnlock Then 'If event is unlock event
            If CueSDK.IsInitialized() = True Then
                Try
                    CueSDK.Reinitialize(False) 'Try to reinitialize
                Catch
                    WindowsLocked(sender, e) 'Keep trying
                Finally
                    CueSDK.Reinitialize(True)
                    SetLEDs() 'Gain control and set LEDs
                End Try
            Else
                Do Until CueSDK.IsSDKAvailable(Devices.Generic.Enums.CorsairDeviceType.Keyboard) = True 'Wait until keyboard is detected, yes, I know this is horrible
                    Threading.Thread.Sleep(50)
                Loop
                InitializeSDK()

                SetLEDs() 'Set LEDs
            End If
        End If
    End Sub

    Private Sub InitializeSDK()
        Try
            CueSDK.Initialize(True) 'Initialize CueSDK
            keyboard = CueSDK.KeyboardSDK
            If keyboard Is Nothing Then
                Throw New Exceptions.WrapperException("No keyboard detected")
            End If
        Catch ex As Exceptions.WrapperException
            MsgBox("Error: Wrapper Exception:  " & ex.Message, MsgBoxStyle.Critical, "Error")
            Application.Exit()
        Catch ex As Exceptions.CUEException
            MsgBox("Error: CUE Exception. Error Code: " + [Enum].GetName(GetType(Devices.Generic.Enums.CorsairError), ex.[Error]), MsgBoxStyle.Critical, "Error")
            Application.Exit()
        End Try
    End Sub

    Private Sub SetLEDs()
        Dim brush As Brushes.SolidColorBrush 'Create brush

        Try 'Try to set brush color from argument
            brush = New Brushes.SolidColorBrush(ColorTranslator.FromHtml(arguments(1)))
        Catch ex As Exception
            MsgBox("Error: Invalid color - color should be in the form of a hex code (#xxxxxx)", MsgBoxStyle.Critical, "Error")
            Application.Exit()
        End Try

        If arguments(2) < 0 Or arguments(2) > 1 Then 'Check brightness
            MsgBox("Error: Invalid brightness - brightness should be between 0.0 and 1.0 (inclusive)", MsgBoxStyle.Critical, "Error")
            Application.Exit()
        End If

        Try 'Try to set brush brightness
            brush.Brightness = Convert.ToDouble(arguments(2))
        Catch ex As Exception
            MsgBox("Error: Invalid brightness", MsgBoxStyle.Critical, "Error")
            Application.Exit()
        End Try

        Try 'Try to set LEDs
            keyboard = CueSDK.KeyboardSDK
            keyboard.Brush = brush
            keyboard.Update()
        Catch ex As Exception
            MsgBox("Error setting lighting: " & ex.Message, MsgBoxStyle.Critical, "Error")
            Application.Exit()
        End Try
    End Sub

    Private Sub CueBrightToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CueBrightToolStripMenuItem.Click
        Visible = True
        WindowState = FormWindowState.Normal 'Show window
    End Sub

    Private Sub DisableToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DisableToolStripMenuItem.Click
        If DisableToolStripMenuItem.Checked = True Then
            CueSDK.Reinitialize(False) 'Give control back to CUE
        Else
            CueSDK.Reinitialize(True) 'Regain control
            SetLEDs() 'Set LEDs again
        End If
    End Sub

    Private Sub QuitToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles QuitToolStripMenuItem.Click
        Application.Exit()
    End Sub

    Private Sub TrayIcon_MouseDoubleClick(sender As Object, e As MouseEventArgs) Handles TrayIcon.MouseDoubleClick
        Visible = True
        WindowState = FormWindowState.Normal 'Show window
    End Sub

    Private Sub btnHide_Click(sender As Object, e As EventArgs) Handles btnHide.Click
        Visible = False
        WindowState = FormWindowState.Minimized 'Hide window
    End Sub

    Private Sub lnkLblLicense_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles lnkLblLicense.LinkClicked
        lnkLblLicense.LinkVisited = True
        Process.Start("https://github.com/DarthAffe/CUE.NET/blob/master/LICENSE") 'Open link
    End Sub
End Class
