Public Class FireplaceScriptAPP

    Public ibinput = "Input not set!"
    Public askshutdown As Boolean = True
    Public windowCreated As Boolean = False
    Public windowOpened As Boolean = False
    Public windowCreatedCount As Integer = 0
    Public frameTitle As String

    Dim frame As Form = New Form

    Private Sub Button1_Click(sender As Object, e As EventArgs)
        End
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs)
        WindowState = FormWindowState.Minimized
    End Sub

    Private Sub Button2_Click_1(sender As Object, e As EventArgs)
        WindowState = FormWindowState.Minimized
    End Sub

    Private Sub Button1_Click_1(sender As Object, e As EventArgs) Handles Button1.Click
        Close()
    End Sub

    Private Sub FireplaceScriptAPP_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ListBox1.Visible = False
        TextBox1.WordWrap = False
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        SaveFileDialog1.ShowDialog()
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        OpenFileDialog1.ShowDialog()
    End Sub

    Private Sub SaveFileDialog1_FileOk(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles SaveFileDialog1.FileOk
        Dim writer As New IO.StreamWriter(SaveFileDialog1.FileName)
        writer.Write(TextBox1.Text)
        writer.Close()
    End Sub

    Private Sub OpenFileDialog1_FileOk(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles OpenFileDialog1.FileOk
        Dim reader As New IO.StreamReader(OpenFileDialog1.FileName)
        TextBox1.Text = reader.ReadToEnd
        reader.Close()
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        ListBox1.Items.Clear()
        ListBox1.Items.AddRange(TextBox1.Lines)
        For i = 0 To ListBox1.Items.Count - 1
            Dim sstring As String = ListBox1.Items.Item(i)
            Dim sarray() As String = Split(sstring)

            If sarray(0) = "helloworld" Then
                MsgBox("Hello World")
            End If

            If sarray(0) = "message" Then
                If sarray.Length < 3 Then
                    MsgBox("Not enough data!", MsgBoxStyle.Critical, "Fireplace App Internal Error!")
                ElseIf sarray(1) = "" Then
                    MsgBox("Not enough data!", MsgBoxStyle.Critical, "Fireplace App Internal Error!")
                ElseIf sarray(2) = "" Then
                    MsgBox("Not enough data!", MsgBoxStyle.Critical, "Fireplace App Internal Error!")
                Else
                    MsgBox(sarray(1), , sarray(2))
                End If
            End If

            If sarray(0) = "errorbeep" Then
                Beep()
            End If

            If sarray(0) = "errorwin" Then
                ErrorWindow.Show()
            End If

            If sarray(0) = "error" Then
                Beep()
                ErrorWindow.Show()
            End If

            If sarray(0) = "ib" Then
                ibinput = InputBox("Input:", "Please enter your input")
                MsgBox("You entered: " + ibinput)
            End If

            If sarray(0) = "runcalc" Then
                Calc.Show()
                Calc.TopMost = True
                Calc.TopMost = False
            End If

            If sarray(0) = "console_echo" Then
                MsgBox(sarray(1))
            End If

            If sarray(0) = "fperror_critical" Then
                engine.mjrerr()
            End If

            If sarray(0) = "error_critical" Then
                engine.mjrerrs()
            End If

            If sarray(0) = "fperror_warn" Then
                engine.warn()
            End If

            If sarray(0) = "error_warn" Then
                engine.warns()
            End If

            If sarray(0) = "fpe_warn_notImplemented" Then
                engine.fpFeatureNotFound()
            End If

            If sarray(0) = "warn_notImplemented" Then
                engine.fpsFeatureNotImplemented()
            End If

            If sarray(0) = "fp_ver" Then
                fpVer.Show()
            End If

            If sarray(0) = "fps_ver" Then
                About.Show()
            End If

            If sarray(0) = "fps_about" Then
                About.Show()
            End If

            If sarray(0) = "fps_credit" Then
                Credits.Show()
            End If

            If sarray(0) = "fp_about" Then
                MoreIntroductionInfomation.Show()
            End If

            If sarray(0) = "fp_loginInfo" Then
                MoreIntroductionInfomation.Show()
            End If

            If sarray(0) = "fp_credit" Then
                Credits.Show()
            End If

            If sarray(0) = "fp_end" Then
                If askshutdown = True Then
                    ShutdownWarning.Show()
                Else
                    End
                End If
            End If

            If sarray(0) = "endfp" Then
                If askshutdown = True Then
                    ShutdownWarning.Show()
                Else
                    End
                End If
            End If

            If sarray(0) = "cwindow" Then
                frame = New Form()
                If sarray.Length = 5 Then
                    frame.SetBounds(sarray(1), sarray(2), sarray(3), sarray(4))
                Else
                    MsgBox("Argument mismatch - Only 4 arguments allowed.", MsgBoxStyle.Critical, "Internal App Error!")
                End If

            End If

                If sarray(0) = "cwindow_tick" Then
                MsgBox("The command 'cwindow_tick' is obsolete. Can cause problems. Using this command is not recommened and will be removed soon.", MsgBoxStyle.Exclamation, "Internal App Warning.")
                If Application.OpenForms().OfType(Of Form).Contains(frame) Then
                    windowOpened = True
                Else
                    windowOpened = False
                End If

                If frame.IsDisposed = False Then
                    windowCreated = True
                ElseIf frame.IsDisposed = True Then
                    windowCreated = False
                Else
                    MsgBox("Invalid disposed state!", MsgBoxStyle.Critical, "Internal App Error!")
                End If
            End If

            If sarray(0) = "cwindow_show" Then
                If windowOpened = True Then
                    Console.WriteLine("APP ERROR: WINDOW ALREADY OPENED")
                    Return
                End If

                frame.Show()
            End If

            If sarray(0) = "cwindow_setTitle" Then
                If sarray.Length < 2 Then
                    MsgBox("Argument mismatch - Not enough arguments!", MsgBoxStyle.Critical, "Internal App Error!")
                    Return
                ElseIf sarray.Length > 2 Then
                    MsgBox("Argument mismatch - Too many arguments!", MsgBoxStyle.Critical, "Internal App Error!")
                    Return
                ElseIf sarray(1) = " " Then
                    MsgBox("Argument mismatch - Argument empty!", MsgBoxStyle.Critical, "Internal App Error!")
                Else
                    frameTitle = sarray(1)
                    frame.Text = frameTitle
                End If
            End If

            If sarray(0) = "echo" Then
                If sarray.Length <= 1 Then
                    MsgBox("Argument mismatch - Too many arguments!", MsgBoxStyle.Critical, "Internal App Error!")
                ElseIf sarray(1) = vbNullString Then
                    MsgBox("Arugument mismatch - Argument empty!", MsgBoxStyle.Critical, "Internal App Error!")
                ElseIf sarray(1) = " " Then
                    MsgBox("Arugument mismatch - Argument empty!", MsgBoxStyle.Critical, "Internal App Error!")
                Else
                    MsgBox(sarray(1), MsgBoxStyle.OkOnly, "Message from app")
                End If
            End If

            If sarray(0) = "echo_title" Then
                If sarray.Length <= 1 Then
                    MsgBox("Argument mismatch - Too many arguments!", MsgBoxStyle.Critical, "Internal App Error!")
                ElseIf sarray(1) = vbNullString Then
                    MsgBox("Arugument mismatch - Argument (message) empty!", MsgBoxStyle.Critical, "Internal App Error!")
                ElseIf sarray(1) = " " Then
                    MsgBox("Arugument mismatch - Argument (message) empty!", MsgBoxStyle.Critical, "Internal App Error!")
                ElseIf sarray(2) = vbNullString Then
                    MsgBox("Arugument mismatch - Argument (title) empty!", MsgBoxStyle.Critical, "Internal App Error!")
                ElseIf (sarray(2)) = " " Then
                    MsgBox("Arugument mismatch - Argument (title) empty!", MsgBoxStyle.Critical, "Internal App Error!")
                Else
                    MsgBox(sarray(1), MsgBoxStyle.OkOnly, sarray(2))
                End If
            End If
        Next
    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        About.Show()
    End Sub

    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        Credits.Show()
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        NewQuestion.Show()
    End Sub

    Private Sub Button9_Click(sender As Object, e As EventArgs) Handles Button9.Click
        Export.Show()
    End Sub

    Private Sub CreateNewMessageCommandToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CreateNewMessageCommandToolStripMenuItem.Click
        NewMessage.Show()
    End Sub

    Private Sub Button2_Click_2(sender As Object, e As EventArgs) Handles Button2.Click
        FPA_Player.Show()
    End Sub

    Private Sub CreateNewcwindowCommandToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CreateNewcwindowCommandToolStripMenuItem.Click
        NewCWindow.Show()
    End Sub
End Class