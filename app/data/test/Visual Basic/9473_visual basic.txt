Imports Microsoft.WindowsAPICodePack
Imports Microsoft.WindowsAPICodePack.Taskbar
Imports Microsoft.WindowsAPICodePack.Shell

Public Class Form1

    ' Keep a reference to the Taskbar instance
    Private windowsTaskbar As TaskbarManager = TaskbarManager.Instance
    Private button0 As ThumbnailToolBarButton
    Private buttonLess As ThumbnailToolBarButton
    Private buttonMore As ThumbnailToolBarButton
    Private button100 As ThumbnailToolBarButton

    Private Sub Form1_Shown(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Shown
        button0 = New ThumbnailToolBarButton(My.Resources.first, "Set to 0%")
        AddHandler button0.Click, AddressOf Empty

        buttonLess = New ThumbnailToolBarButton(My.Resources.prevArrow, "Decrease by 1%")
        AddHandler buttonLess.Click, AddressOf Less

        buttonMore = New ThumbnailToolBarButton(My.Resources.nextArrow, "Increase by 1%")
        AddHandler buttonMore.Click, AddressOf More

        button100 = New ThumbnailToolBarButton(My.Resources.last, "Set to 100%")
        AddHandler button100.Click, AddressOf Fill

        TaskbarManager.Instance.ThumbnailToolBars.AddButtons(Me.Handle, button0, buttonLess, buttonMore, button100)

        For Each state As String In System.Enum.GetNames(GetType(TaskbarProgressBarState))
            comboBoxProgressBarStates.Items.Add(state)
        Next state
        comboBoxProgressBarStates.SelectedItem = "Normal"
    End Sub

    Private Sub TrackBar1_Scroll(ByVal sender As Object, ByVal e As System.EventArgs) Handles TrackBar1.Scroll
        calc()
    End Sub

    Private Sub comboBoxProgressBarStates_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles comboBoxProgressBarStates.SelectedIndexChanged
        Dim state As TaskbarProgressBarState = CType(System.Enum.Parse(GetType(TaskbarProgressBarState), CStr(comboBoxProgressBarStates.SelectedItem)), TaskbarProgressBarState)
        windowsTaskbar.SetProgressState(state)
    End Sub

    Sub calc()
        Label1.Text = TrackBar1.Value
        ProgressBar1.Value = TrackBar1.Value
        windowsTaskbar.SetProgressValue(TrackBar1.Value, 100)
        If TrackBar1.Value = 0 Then
            button0.Enabled = False
            buttonLess.Enabled = False
            buttonMore.Enabled = True
            button100.Enabled = True
        ElseIf TrackBar1.Value = 100 Then
            button0.Enabled = True
            buttonLess.Enabled = True
            buttonMore.Enabled = False
            button100.Enabled = False
        Else
            button0.Enabled = True
            buttonLess.Enabled = True
            buttonMore.Enabled = True
            button100.Enabled = True
        End If
    End Sub

    Sub Empty()
        TrackBar1.Value = 0
        calc()
    End Sub

    Sub Less()
        TrackBar1.Value -= 1
        calc()
    End Sub

    Sub More()
        TrackBar1.Value += 1
        calc()
    End Sub

    Sub Fill()
        TrackBar1.Value = 100
        calc()
    End Sub
End Class
