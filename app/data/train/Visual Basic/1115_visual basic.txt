Public Class Form1
    Public isBlackout As Boolean = False
    Public blackouts As New ArrayList

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub btnToggle_Click(sender As Object, e As EventArgs) Handles btnToggle.Click
        isBlackout = Not isBlackout
        If isBlackout Then
            btnToggle.Text = "Disable Blackout"
            For i As Integer = 0 To Screen.AllScreens.Length - 1
                Dim curScreen As Screen = Screen.AllScreens(i)
                Dim newForm As New blackoutForm
                newForm.Show()
                newForm.Visible = True
                newForm.BringToFront()
                newForm.FormBorderStyle = FormBorderStyle.None
                newForm.BackColor = Color.Black
                newForm.Left = curScreen.Bounds.Left
                newForm.Top = curScreen.Bounds.Top
                newForm.Width = curScreen.Bounds.Width
                newForm.Height = curScreen.Bounds.Height

                blackouts.Add(newForm)
            Next
        Else
            btnToggle.Text = "Enable Blackout"
            For Each newForm As blackoutForm In blackouts
                newForm.Hide()
                newForm.isSilentClosing = True
                newForm.Close()
            Next
            blackouts.Clear()
        End If
    End Sub

    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click
        Process.Start("http://r4u.ca?ref=TopMostApp&sref=Icon")
    End Sub

    Private Sub lblAbout_Click(sender As Object, e As EventArgs) Handles lblAbout.Click
        Process.Start("http://r4u.ca?ref=TopMostApp&sref=Text")
    End Sub
End Class

Public Class blackoutForm
    Inherits Form
    Private _silentClose As Boolean
    Public Property isSilentClosing() As Boolean
        Get
            Return _silentClose
        End Get
        Set(ByVal value As Boolean)
            _silentClose = value
        End Set
    End Property

    Private _btnHovered As Boolean
    Public Property isCloseHovered() As Boolean
        Get
            Return _btnHovered
        End Get
        Set(ByVal value As Boolean)
            _btnHovered = value
        End Set
    End Property

    Public WithEvents btnClose As Button
    Private WithEvents hoverTimer As Timer

    Private Sub blackoutForm_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        If Not isSilentClosing Then
            Form1.blackouts.Remove(sender)
            If Form1.blackouts.Count = 0 Then
                Form1.isBlackout = False
                Form1.btnToggle.Text = "Enable Blackout"
            End If
        End If
    End Sub

    Private Sub blackoutForm_HandleCreated(sender As Object, e As EventArgs) Handles Me.HandleCreated
        hoverTimer = New Timer
        btnClose = New Button
        With btnClose
            .ForeColor = Color.White
            .Parent = Me
            .Text = "Close"
            .Width = 75
            .Height = 25
            .Top = 0
            .Left = Me.Width - btnClose.Width
            .Visible = False
        End With
    End Sub

    Private Sub blackoutForm_MouseEnter(sender As Object, e As EventArgs) Handles Me.MouseEnter
        btnClose.Visible = True
        With btnClose
            .ForeColor = Color.White
            .Text = "Close"
            .Width = 75
            .Height = 25
            .Top = 0
            .Left = Me.Width - btnClose.Width
        End With
    End Sub

    Private Sub blackoutForm_MouseLeave(sender As Object, e As EventArgs) Handles Me.MouseLeave
        If Not isCloseHovered Then
            hoverTimer.Interval = 100
            hoverTimer.Enabled = True
            hoverTimer.Start()
        End If
    End Sub

    Private Sub btnClose_MouseEnter(sender As Object, e As EventArgs) Handles btnClose.MouseEnter
        isCloseHovered = True
    End Sub

    Private Sub btnClose_MouseLeave(sender As Object, e As EventArgs) Handles btnClose.MouseLeave
        isCloseHovered = False
    End Sub

    Private Sub btnClose_MouseHover(sender As Object, e As EventArgs) Handles btnClose.MouseHover
        isCloseHovered = True
    End Sub

    Private Sub hoverTimer_Tick(sender As Object, e As EventArgs) Handles hoverTimer.Tick
        If Not isCloseHovered Then
            btnClose.Visible = False
        End If
        hoverTimer.Enabled = False
        hoverTimer.Stop()
    End Sub

    Private Sub btnClose_Click(sender As Object, e As EventArgs) Handles btnClose.Click
        Me.Close()
    End Sub
End Class