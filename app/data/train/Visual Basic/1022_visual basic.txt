Public Class Form1

    Private Sub NewToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NewToolStripMenuItem.Click
        'Check if there's text added to the textbox

        If TextBox1.Modified Then

            'If the text of notepad changed, the program will ask the user if they want to save the changes

            Dim ask As MsgBoxResult

            ask = MsgBox("Do you want to save changes to your text?", MsgBoxStyle.YesNoCancel, "New Document")

            If ask = MsgBoxResult.No Then

                TextBox1.Clear()

            ElseIf ask = MsgBoxResult.Cancel Then

            ElseIf ask = MsgBoxResult.Yes Then

                SaveFileDialog1.ShowDialog()

                If SaveFileDialog1.FileName.Length > 0 Then
                    My.Computer.FileSystem.WriteAllText(SaveFileDialog1.FileName, TextBox1.Text, False)

                    TextBox1.Clear()
                End If
            End If

                'If textbox's text is still the same, notepad will open a new page:

        Else

            TextBox1.Clear()

        End If
    End Sub

    Private Sub OpenToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OpenToolStripMenuItem.Click
        'Check if there's text added to the textbox

        On Error GoTo ErrHandler

        If TextBox1.Modified Then

            'If the text of notepad changed, the program will ask the user if they want to save the changes

            Dim ask As MsgBoxResult

            ask = MsgBox("Do you want to save changes to your text?", MsgBoxStyle.YesNoCancel, "Open Document")

            If ask = MsgBoxResult.No Then

                OpenFileDialog1.ShowDialog()

                TextBox1.Text = My.Computer.FileSystem.ReadAllText(OpenFileDialog1.FileName)

            ElseIf ask = MsgBoxResult.Cancel Then

            ElseIf ask = MsgBoxResult.Yes Then

                SaveFileDialog1.ShowDialog()

                If SaveFileDialog1.FileName.Length > 0 Then
                    My.Computer.FileSystem.WriteAllText(SaveFileDialog1.FileName, TextBox1.Text, False)
                End If

                TextBox1.Clear()

                End If

        Else

            'If textbox's text is still the same, notepad will show the OpenFileDialog

            OpenFileDialog1.ShowDialog()

            TextBox1.Text = My.Computer.FileSystem.ReadAllText(OpenFileDialog1.FileName)

        End If

ErrHandler:
        ' User pressed Cancel button.
        Exit Sub
    End Sub

    Private Sub SaveToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SaveToolStripMenuItem.Click
        SaveFileDialog1.ShowDialog()

        ' the application will check if the file already exists, if it exists, it will ask the user if they want to replace it

        If My.Computer.FileSystem.FileExists(SaveFileDialog1.FileName) Then

            Dim ask As MsgBoxResult

            ask = MsgBox("File already exists; would you like to replace it?", MsgBoxStyle.YesNo, "File Exists")

            'if the user decides not to replace the existing file

            If ask = MsgBoxResult.No Then

                SaveFileDialog1.ShowDialog()

                'if the user decides to replace the existing file

            ElseIf ask = MsgBoxResult.Yes Then
                If SaveFileDialog1.FileName.Length > 0 Then
                    My.Computer.FileSystem.WriteAllText(SaveFileDialog1.FileName, TextBox1.Text, False)
                End If
            End If

                'if the file doesn't exist

        Else
            If SaveFileDialog1.FileName.Length > 0 Then
                My.Computer.FileSystem.WriteAllText(SaveFileDialog1.FileName, TextBox1.Text, False)
            End If
        End If
    End Sub

    Private Sub UndoToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UndoToolStripMenuItem.Click
        'check if textbox can undo

        If TextBox1.CanUndo Then

            TextBox1.Undo()

        Else

        End If
    End Sub

    Private Sub CutToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CutToolStripMenuItem.Click
        My.Computer.Clipboard.Clear()

        If TextBox1.SelectionLength > 0 Then

            My.Computer.Clipboard.SetText(TextBox1.SelectedText)

        End If

        TextBox1.SelectedText = ""
    End Sub

    Private Sub CopyToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CopyToolStripMenuItem.Click
        My.Computer.Clipboard.Clear()

        If TextBox1.SelectionLength > 0 Then

            My.Computer.Clipboard.SetText(TextBox1.SelectedText)

        End If
    End Sub

    Private Sub PasteToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PasteToolStripMenuItem.Click
        If My.Computer.Clipboard.ContainsText Then

            TextBox1.Paste()

        End If
    End Sub

    Private Sub SelectAllToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SelectAllToolStripMenuItem.Click
        TextBox1.SelectAll()
    End Sub

    Private Sub FindToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FindToolStripMenuItem.Click
        Dim a As String

        Dim b As String

        a = InputBox("Enter text to be found")

        b = InStr(TextBox1.Text, a)

        If b Then

            TextBox1.Focus()

            TextBox1.SelectionStart = b - 1

            TextBox1.SelectionLength = Len(a)

        Else

            MsgBox("Text not found.")

        End If
    End Sub

    Private Sub FontToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FontToolStripMenuItem.Click
        FontDialog1.ShowDialog()

        TextBox1.Font = FontDialog1.Font
    End Sub

    Private Sub TextColourToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextColourToolStripMenuItem.Click
        ColorDialog1.ShowDialog()

        TextBox1.ForeColor = ColorDialog1.Color
    End Sub

    Private Sub LeftToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles LeftToolStripMenuItem.Click
        TextBox1.TextAlign = HorizontalAlignment.Left

        LeftToolStripMenuItem.Checked = True

        CentreToolStripMenuItem.Checked = False

        RightToolStripMenuItem.Checked = False
    End Sub

    Private Sub CentreToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CentreToolStripMenuItem.Click
        TextBox1.TextAlign = HorizontalAlignment.Center

        LeftToolStripMenuItem.Checked = False

        CentreToolStripMenuItem.Checked = True

        RightToolStripMenuItem.Checked = False
    End Sub

    Private Sub RightToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RightToolStripMenuItem.Click
        TextBox1.TextAlign = HorizontalAlignment.Right

        LeftToolStripMenuItem.Checked = False

        CentreToolStripMenuItem.Checked = False

        RightToolStripMenuItem.Checked = True
    End Sub

    Private Sub BackColourToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BackColourToolStripMenuItem.Click
        ColorDialog1.ShowDialog()

        TextBox1.BackColor = ColorDialog1.Color
    End Sub

    Private Sub AboutToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AboutToolStripMenuItem.Click
        MsgBox("Victoria's Notepad." & vbCrLf & vbCrLf & _
               "Programmed by Victoria" & vbCrLf & vbCrLf & _
               "August 2010")
    End Sub

    Private Sub PrintPreviewToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PrintPreviewToolStripMenuItem.Click
        PrintPreviewDialog1.Document = PrintDocument1

        PrintPreviewDialog1.ShowDialog()
    End Sub

    Private Sub PrintToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PrintToolStripMenuItem.Click
        PrintDialog1.ShowDialog()

        If PrintDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then

            PrintDocument1.Print()

        End If
    End Sub

    Private Sub PrintDocument1_PrintPage(ByVal sender As System.Object, ByVal e As System.Drawing.Printing.PrintPageEventArgs) Handles PrintDocument1.PrintPage
        Static currentChar As Integer

        Static currentLine As Integer

        Dim textfont As Font = TextBox1.Font

        Dim h, w As Integer

        Dim left, top As Integer

        With PrintDocument1.DefaultPageSettings

            h = .PaperSize.Height - .Margins.Top - .Margins.Bottom

            w = .PaperSize.Width - .Margins.Left - .Margins.Right

            left = PrintDocument1.DefaultPageSettings.Margins.Left

            top = PrintDocument1.DefaultPageSettings.Margins.Top

        End With

        e.Graphics.DrawRectangle(Pens.Blue, New Rectangle(left, top, w, h))

        If PrintDocument1.DefaultPageSettings.Landscape Then

            Dim a As Integer

            a = h

            h = w

            w = a

        End If

        Dim lines As Integer = CInt(Math.Round(h / textfont.Height))

        Dim b As New Rectangle(left, top, w, h)

        Dim format As StringFormat

        If Not TextBox1.WordWrap Then

            format = New StringFormat(StringFormatFlags.NoWrap)

            format.Trimming = StringTrimming.EllipsisWord

            Dim i As Integer

            For i = currentLine To Math.Min(currentLine + lines, TextBox1.Lines.Length - 1)

                e.Graphics.DrawString(TextBox1.Lines(i), textfont, Brushes.Black, New RectangleF(left, top + textfont.Height * (i - currentLine), w, textfont.Height), format)

            Next

            currentLine += lines

            If currentLine >= TextBox1.Lines.Length Then

                e.HasMorePages = False

                currentLine = 0

            Else

                e.HasMorePages = True

            End If

            Exit Sub

        End If

        format = New StringFormat(StringFormatFlags.LineLimit)

        Dim line, chars As Integer

        e.Graphics.MeasureString(Mid(TextBox1.Text, currentChar + 1), textfont, New SizeF(w, h), format, chars, line)

        If currentChar + chars < TextBox1.Text.Length Then

            If TextBox1.Text.Substring(currentChar + chars, 1) <> " " And TextBox1.Text.Substring(currentChar + chars, 1) <> vbLf Then

                While chars > 0

                    TextBox1.Text.Substring(currentChar + chars, 1)

                    TextBox1.Text.Substring(currentChar + chars, 1)

                    chars -= 1

                End While

                chars += 1

            End If

        End If

        e.Graphics.DrawString(TextBox1.Text.Substring(currentChar, chars), textfont, Brushes.Black, b, format)

        currentChar = currentChar + chars

        If currentChar < TextBox1.Text.Length Then

            e.HasMorePages = True

        Else

            e.HasMorePages = False

            currentChar = 0

        End If
    End Sub

    Private Sub Form1_Closing(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing
        ' Determine if text has changed in the textbox by comparing to original text.
        If TextBox1.Modified Then
            ' Display a MsgBox asking the user to save changes or abort.
            If MessageBox.Show("Do you want to save changes to your text?", "Victoria's Notepad", MessageBoxButtons.YesNo) = DialogResult.Yes Then
                ' Cancel the Closing event from closing the form.
                e.Cancel = True
                SaveFileDialog1.ShowDialog()

                If SaveFileDialog1.FileName.Length > 0 Then
                    My.Computer.FileSystem.WriteAllText(SaveFileDialog1.FileName, TextBox1.Text, False)

                    TextBox1.Clear()
                End If
            End If ' Call method to save file...   
        End If
    End Sub 'Form1_Closing

    Private Sub ExitToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ExitToolStripMenuItem.Click
        If TextBox1.Modified Then
            ' Display a MsgBox asking the user to save changes or abort.
            If MessageBox.Show("Do you want to save changes to your text?", "Victoria's Notepad", MessageBoxButtons.YesNo) = DialogResult.Yes Then
                SaveFileDialog1.ShowDialog()

                If SaveFileDialog1.FileName.Length > 0 Then
                    My.Computer.FileSystem.WriteAllText(SaveFileDialog1.FileName, TextBox1.Text, False)
                    TextBox1.Clear()
                End If
            End If ' Call method to save file...   
        End If
        End
    End Sub
End Class
