Imports System.Windows.Forms
Imports Microsoft.Win32
Public Event KeyDown As KeyEventHandler
Public Class Form1
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        WebBrowser1.Navigate("https://www.google.com") 'go to google on load because who really likes msn.com
    End Sub
    Public Sub History() 'sub used to store history just for slightly cleaner code
        ListBox1.Items.Add(TextBox2.Text) 'add items from hidden URL box to history list, using textbox2 so user cannot edit history
    End Sub
    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click 'if the url bar contains any known domain phrases it will then attempt to load the page as a domain, if not it'll google search it
        If TextBox1.Text.Contains("http://") Or TextBox1.Text.Contains("https://") Or TextBox1.Text.Contains("www.") Or TextBox1.Text.Contains(".com") Or TextBox1.Text.Contains(".net") Or TextBox1.Text.Contains(".co.uk") Or TextBox1.Text.Contains(".org") Then
            WebBrowser1.Navigate(TextBox1.Text)
            Button7.Enabled = True
            History()
        ElseIf TextBox1.Text = "" Or TextBox1.Text = "Enter your URL or search term here" Then
            MsgBox("Please enter a valid query")
            History()
        Else
            WebBrowser1.Navigate("https://www.google.com/search?hl=en&q=" & TextBox1.Text)
            Button7.Enabled = True
            History()
        End If
    End Sub
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click 'back button
        WebBrowser1.GoBack()
    End Sub
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click 'forward button
        WebBrowser1.GoForward()
    End Sub
    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click 'refresh button
        WebBrowser1.Refresh()
    End Sub
    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click  'go home button
        WebBrowser1.GoHome()
    End Sub
    Private Sub TextBox1_Click(sender As Object, e As EventArgs) Handles TextBox1.Click 'when url bar is clicked clear it
        TextBox1.Text = ""
    End Sub
    Private Sub WebBrowser1_DocumentCompleted(sender As Object, e As Windows.Forms.WebBrowserDocumentCompletedEventArgs) Handles WebBrowser1.DocumentCompleted 'when page is loaded check the URL bar to see if the connecting is using SSL then set the label to the appropriate value
        TextBox2.Text = WebBrowser1.Url.ToString 'using textbox2 to prevent any issues (textbox 2 is a mirror of textbox1)
        TextBox1.Text = WebBrowser1.Url.ToString
        If TextBox2.Text.Contains("https://") Then
            ToolStripStatusLabel1.Text = "Secure connection"
            Button7.Enabled = False
        ElseIf TextBox2.Text.Contains("http://") Then
            ToolStripStatusLabel1.Text = "Unsecure connection"
            Button7.Enabled = False
        Else
            ToolStripStatusLabel1.Text = "Unknown connection"
            Button7.Enabled = False
        End If
    End Sub
    Private Sub WebBrowser1_ProgressChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.WebBrowserProgressChangedEventArgs) Handles WebBrowser1.ProgressChanged
        Try 'when a change is happening on the webpage the progress bar will try to update accordingly
            If e.MaximumProgress <> 0 And e.MaximumProgress >= e.CurrentProgress Then
                ToolStripProgressBar1.Value = Convert.ToInt32(100 * e.CurrentProgress / e.MaximumProgress)
            Else
                With ToolStripProgressBar1
                    .Value = 100
                    .Visible = True
                End With
            End If
        Catch ex As Exception
            ToolStripStatusLabel1.Text = "Error Loading page" 'otherwise tell the user there's been an error
        End Try
    End Sub
    Private Sub textBox1_KeyDown(sender As Object, e As System.Windows.Forms.KeyEventArgs) Handles TextBox1.KeyDown 'when enter is clicked on the URL bar the go button function will be executed
        If e.KeyCode = Keys.Enter Then
            Button5_Click(sender, e)
        End If
    End Sub
    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged 'changes the title of the form to correspond with the current webpage
        Me.Text = WebBrowser1.DocumentTitle & " - IE shell web browser: GitHub: Material-Design"
    End Sub
    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click 'edits registry to change homepage (IE only!)
        Dim value = InputBox("Set a new homepage:" & vbNewLine & "Note, your input must include 'http(s)'")
        Dim regKey As RegistryKey
        regKey = Registry.CurrentUser.OpenSubKey("Software\Microsoft\Internet Explorer\Main", True)
        regKey.SetValue("Start Page", value)
    End Sub
    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click 'stop loading webpage
        WebBrowser1.Stop()
    End Sub
    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click 'print webpage dialog
        WebBrowser1.ShowPrintDialog()
    End Sub
    Private Sub Button11_Click(sender As Object, e As EventArgs) Handles Button11.Click 'show page information
        WebBrowser1.ShowPropertiesDialog()
    End Sub
    Private Sub ListBox1_DoubleClicked(sender As Object, e As EventArgs) Handles ListBox1.DoubleClick 'when history box is double clicked copy clicked value to URL bar then press go
        TextBox1.Text = ListBox1.Text
        Button5_Click(sender, e)
    End Sub
    Private Sub Button9_Click(sender As Object, e As EventArgs) Handles Button9.Click 'enables clearing of listbox history, because everyone has their secrerts...
        ListBox1.Items.Clear()
    End Sub
End Class