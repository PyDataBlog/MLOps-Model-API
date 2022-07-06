Imports System.IO
Imports System.Windows.Threading

Class MainWindow
    Dim chatFilePath As String
    Dim username As String
    Dim userFilePath As String
    Dim fileReader As StreamReader
    Dim fileWriter As StreamWriter
    Dim refreshTimer As New DispatcherTimer

    Private Sub MainWindow_Loaded(ByVal sender As Object, ByVal e As System.Windows.RoutedEventArgs) Handles Me.Loaded
        chatFilePath = My.Settings.filePath
        username = My.Settings.username
        tFilePath.Text = chatFilePath
        tUsername.Text = username

        'Create and start timer
        refreshTimer = New DispatcherTimer
        refreshTimer.Interval = TimeSpan.FromSeconds(5)
        AddHandler refreshTimer.Tick, AddressOf timerTick

    End Sub


    Private Sub MainWindow_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Input.MouseButtonEventArgs) Handles Me.MouseDown
        If (e.LeftButton = MouseButtonState.Pressed) Then
            DragMove()
        End If

    End Sub

    Private Sub tExitButton_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Input.MouseButtonEventArgs) Handles tExitButton.MouseDown
        Close()
    End Sub

    Private Sub bBrowse_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles bBrowse.Click
        Dim ofd As New Microsoft.Win32.OpenFileDialog
        ofd.Filter = "Text Files (*.txt)|*.txt"
        ofd.Title = "Choose chat text file"
        ofd.FileName = ""
        Dim result? As Boolean = ofd.ShowDialog()
        If result = True Then
            chatFilePath = ofd.FileName
            tFilePath.Text = chatFilePath
            My.Settings.filePath = chatFilePath
            My.Settings.Save()
        End If
    End Sub

    Private Sub bConnect_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles bConnect.Click
        username = tUsername.Text
        My.Settings.username = username
        My.Settings.Save()
        refreshTimer.Start()
        connect()
    End Sub

    Public Sub connect()
        'Disable connect area
        enableSetup(False)

        'Enable send box and button
        tSend.IsEnabled = True
        bSend.IsEnabled = True

        'Enable refresh timer
        refreshTimer.Start()

        'Update window
        updateWindow()

    End Sub
    Public Sub enableSetup(ByVal Bool As Boolean)
        tFilePath.IsEnabled = Bool
        tUsername.IsEnabled = Bool
        bConnect.IsEnabled = Bool
        bBrowse.IsEnabled = Bool
    End Sub

    Private Sub bSend_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles bSend.Click
        Select Case tSend.Text
            Case "cls"
                clearFile()
                tSend.Text = ""

            Case "nyan cat attack"
                nyanAttack()

            Case Else
                'Add username before text
                Dim editedText As String
                editedText = username & ">>  " & tSend.Text
                WriteLine(editedText)
                tSend.Text = ""
        End Select
    End Sub
    Public Sub updateWindow()
        Try
            'Read text file and display to chat window
            fileReader = New StreamReader(chatFilePath)
            tChatWindow.Text = fileReader.ReadToEnd()
            fileReader.Close()
            tChatWindow.ScrollToEnd()
        Catch ex As Exception
            tChatWindow.Text = "Cound not fetch chat file contents!"
        End Try
    End Sub
    Public Sub clearFile()
        My.Computer.FileSystem.WriteAllText(chatFilePath, "", False)
        updateWindow()
    End Sub
    Private Sub timerTick()
        updateWindow()
    End Sub
    Public Sub nyanAttack()
        Dim counter As Integer
        For counter = 1 To 50
            writeLine("Nyan Cat! Meow!")
        Next
    End Sub
    Public Sub writeLine(ByVal line As String)
        Try
            'Write line to text file
            Dim fileWriter As System.IO.StreamWriter
            fileWriter = My.Computer.FileSystem.OpenTextFileWriter(chatFilePath, True)
            fileWriter.WriteLine(line)
            fileWriter.Close()
            updateWindow()
        Catch ex As Exception
            MsgBox("Error sending message!", MsgBoxStyle.Critical, "Error")
        End Try
    End Sub
    Private Sub tSend_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Input.KeyEventArgs) Handles tSend.KeyDown
        If e.Key = Key.Enter Then
            bSend.RaiseEvent(New RoutedEventArgs(Primitives.ButtonBase.ClickEvent))
        End If

    End Sub
 
    Private Sub tUsername_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Input.KeyEventArgs) Handles tUsername.KeyDown
        If e.Key = Key.Enter Then
            bConnect.RaiseEvent(New RoutedEventArgs(Primitives.ButtonBase.ClickEvent))
        End If
    End Sub


End Class
