Imports System.ComponentModel
Imports System.IO.File

Public Class MainForm

    Dim vocabChanged As Boolean = False
    Dim currentFileName As String = ""
    Dim loaded As Boolean = False

    Dim parentSplash As SplashForm
    'Dim My.Settings.IsSidePaneOn = True

    Dim words As New List(Of String)
    ' Dim lfrm As LoadingForm

    Public Sub SetParentSplash(parentSpl As SplashForm)
        parentSplash = parentSpl
    End Sub

    Private Sub NewEntry()
        NewEntryDialog.ShowDialog()
    End Sub

    Private Sub DeleteSelectedWord()
        If WordsListBox.SelectedIndex = -1 Then
            MsgBox("Nem jelölt ki elemet.", vbExclamation)
            Exit Sub
        End If

        Dim torole = MsgBox("Tényleg törli az elemet?", vbQuestion + vbYesNo)
        If torole = vbYes Then
            Try
                Dim index = WordsListBox.SelectedIndex
                RemoveItem(index)
                vocabChanged = True
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub Form1_Activated1(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Activated
        List1_SelectedIndexChanged_1()
    End Sub

    Private Sub Form1_FormClosing() Handles Me.FormClosing
        My.Settings.Save()
        If vocabChanged Then
            Dim a = MsgBox("Kívánja menteni '" & currentFileName & "' szószedetet?", MsgBoxStyle.Question + MsgBoxStyle.YesNo)
            If a = vbYes Then
                SaveVocabulary()
                Exit Sub
            End If
        End If
    End Sub

    Public Sub New()
        ' This call is required by the Windows Form Designer.
        InitializeComponent()
    End Sub

    Private Sub DeleteAllWords()
        Dim torole = MsgBox("Valóban törli az összes elemet?", vbQuestion + vbYesNo)
        If torole = vbYes Then
            Try
                WordsListBox.Items.Clear()
                vocabChanged = True
            Catch ex As Exception
                MsgBox("Hiba!" + vbNewLine + ex.Message, vbExclamation)
            End Try
        End If
    End Sub

    Private Sub OKToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OkToolStripMenuItem.Click
        NotifyIcon.Visible() = False
    End Sub

    Sub megnyitas()
        RefreshVocabulary()
    End Sub

    Private Sub filenam_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Text = My.Application.Info.Title.ToString + " - " + currentFileName
        'FilenameToolStripLabel.Text = currentFileName
    End Sub

    Private Sub ShowSettingsDialog()
        Dim settingsDlg = New SettingsDialog
        If settingsDlg.ShowDialog() = Windows.Forms.DialogResult.OK Then
            ' Save settings
        End If
    End Sub

    Sub LoadSettings()
        Dim StarUpFile As String = ""
        Dim oneFile As String = ""

        ' Settings-bol alapadatok
        StarUpFile = My.Settings.StartupFile
        currentFileName = StarUpFile

        ' Beallitasok alkalmazasa
        ToolStrip.Visible = My.Settings.IsToolbarOn
        StatusStrip.Visible = My.Settings.IsStatusbarOn
        WordsListBox.Font = My.Settings.Font
        ListView.Font = My.Settings.Font

        ' ui
        ToolbarToolStripMenuItem.Checked = My.Settings.IsToolbarOn
        StatusbarToolStripMenuItem.Checked = My.Settings.IsStatusbarOn
        FontDialog.Font = My.Settings.Font

        ChangeView(My.Settings.View)
        ChangeColor(My.Settings.BackColor)

        SidePaneMenuItem.Checked = My.Settings.IsSidePaneOn

        ' sound on load
        SoundOnLoadToolStripMenuItem.Checked = My.Settings.SoundOnLoad

        ' find startup vocabulary, create it if doesn't exist
        If Not My.Computer.FileSystem.FileExists(currentFileName) Then
            FileOpen(1, currentFileName, OpenMode.Output)
            FileClose(1)
        End If

        Try
            ' Load Voclist File
            FileOpen(10, "voclist.dat", OpenMode.Input)
            Do Until EOF(10)
                Input(10, oneFile)
                SettingsDialog.VocListBox.Items.Clear()
                ChooseVocabDialog.VocabListBox.Items.Clear()
                ChooseVocabDialog.VocabListBox.Items.Add(oneFile)
                SettingsDialog.VocListBox.Items.Add(oneFile)
            Loop

        Catch ex As Exception
            MsgBox("A beállításokat nem lehetett betölteni, mert OPTIONS.DAT nem található vagy sérült." + vbNewLine + ex.Message, MsgBoxStyle.Critical)
            'End
        Finally
            FileClose(10)
        End Try
    End Sub

    Private Sub ShowAboutDialog()
        AboutDialog.ShowDialog()
    End Sub

    Private Sub gomb_atnev_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs)
        ShowRewriteDialog()
    End Sub

    Private Sub ShowRewriteDialog()

        If Me.WordsListBox.SelectedIndex = -1 Then
            MsgBox("Először válasszon ki egy szót.")
            Exit Sub
        End If


        Dim rewriteDialog = New RewriteDialog

        Dim currentWord = Me.WordsListBox.SelectedItem.ToString
        Dim wordParts = currentWord.Split("=")

        rewriteDialog.word.Text = wordParts(0)
        If wordParts.Length < 2 Then
            MsgBox("Ez a szó hibás. Nem tartalmaz elválasztójelet. Az ezután felbukkanó ablakban külön kell választania szót és a jelentést is.")
        Else
            rewriteDialog.meaning.Text = wordParts(1)
        End If

        If (rewriteDialog.ShowDialog = Windows.Forms.DialogResult.OK) Then
            Dim wordString As String = rewriteDialog.word.Text + "=" + rewriteDialog.meaning.Text
            Dim index = Me.WordsListBox.SelectedIndex
            RenameItem(index, wordString)
            vocabChanged = True
        End If
    End Sub

    Private Sub ShowNewEntryDialog()
        Dim newEntryDialog = New NewEntryDialog
        If newEntryDialog.ShowDialog = Windows.Forms.DialogResult.OK Then
            Dim word = newEntryDialog.LeftTextBox.Text & "=" & newEntryDialog.RightTextBox.Text
            InsertItem(-1, word)
            vocabChanged = True
        End If
    End Sub

    Private Sub gomb_torol_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs)
        DeleteSelectedWord()
    End Sub
    Private Sub ShowSearchDialog()
        Dim searchDialog = New SearchDialog
        searchDialog.Show()
    End Sub

    Private Sub List1_SelectedIndexChanged_1() Handles WordsListBox.SelectedIndexChanged

        Try
            If WordsListBox.SelectedItems.Count > 0 Then
                InfoToolStripStatusLabel.Text = WordsListBox.Items(WordsListBox.SelectedIndex)
                WordDisplayToolStripStatusLabel.Text = WordsListBox.SelectedIndex + "1" & "/" & WordsListBox.Items.Count
                WordLengthToolStripStatusLabel.Text = "Hossz: " & Len(WordsListBox.Items(WordsListBox.SelectedIndex))
            End If
        Catch ex As Exception
            MsgBox(ex.Message, vbCritical)
        End Try
    End Sub

    Private Sub UpdateTitle()
        If vocabChanged Then
            Text = My.Application.Info.Title.ToString + " - " + currentFileName + "*"
        Else
            Text = My.Application.Info.Title.ToString + " - " + currentFileName
        End If
    End Sub

    Private Sub ÚjSzótárToolStripMenuItem_Click() Handles NewVocabToolStripMenuItem.Click
    End Sub

    Sub SaveWordList()
        FileOpen(10, "voclist.dat", OpenMode.Output)
        For e = 0 To ChooseVocabDialog.VocabListBox.Items.Count - 1
            PrintLine(10, ChooseVocabDialog.VocabListBox.Items(e))
        Next
        FileClose(10)
    End Sub

    Private Sub NewVocabulary()
        Dim newVocabularyDialog = New NewVocabDialog
        'SaveVocabulary()
        'RefreshVocabulary()
        If newVocabularyDialog.ShowDialog() = Windows.Forms.DialogResult.OK Then
            Dim newVocabFileName As String = newVocabularyDialog.VocabNameTextBox.Text + ".voc"
            Dim setBaseVocab As Boolean = newVocabularyDialog.AddToVocabListCheckBox.Checked
            Dim addToVocabList As Boolean = newVocabularyDialog.SetAsBaseVocabCheckBox.Checked
            Try
                FileOpen(20, newVocabFileName, OpenMode.Output)
                FileClose(20)
                currentFileName = newVocabFileName
                megnyitas()
                If setBaseVocab Then
                    ChooseVocabDialog.VocabListBox.Items.Add(newVocabFileName)
                    SettingsDialog.VocListBox.Items.Add(newVocabFileName)
                    SaveWordList()
                End If
                If addToVocabList Then
                    SettingsDialog.StartupOpenTextBox.Text = newVocabFileName
                    SettingsDialog.SaveSettings()
                End If
            Catch ex As Exception
                MsgBox("Rossz név.", MsgBoxStyle.Exclamation)
            End Try
        End If
    End Sub

    Private Sub SaveVocabulary()
        FileClose(1)
        FileOpen(1, currentFileName, OpenMode.Output)
        For e = 0 To words.Count - 1
            PrintLine(1, words(e))
        Next e
        FileClose(1)
        vocabChanged = False
        RefreshVocabulary()
    End Sub

    Private Sub RefreshVocabulary()
        ShowSidePane(False)
        Cursor = Cursors.WaitCursor

        WaitPictureBox.Visible = True
        WaitPictureBox.Dock = DockStyle.Fill
        WaitPictureBox.Refresh()
        WaitPictureBox.Focus()

        Dim currentLine As String = ""
        ' Try
        WordsListBox.Items.Clear()
        ListView.Items.Clear()
        words.Clear()

        Dim lineCount = ReadAllLines(currentFileName).Length
        ToolStripProgressBar.Maximum = lineCount
        ToolStripProgressBar.Value = 0
        ToolStripProgressBar.Visible = True

        ' FileName on ToolStripLabel
        Text = My.Application.Info.Title.ToString + " - " + currentFileName


        ' lfrm = New LoadingForm()
        ' lfrm.setProgressBarMaximum(lineCount)

        LoadFileBackgroundWorker.RunWorkerAsync()
    End Sub

    Private Sub EditWordToolStripButton_Click(sender As Object, e As EventArgs)
        ShowRewriteDialog()
    End Sub

    Private Sub RenameToolStripMenuItem_Click(sender As Object, e As EventArgs)
        ShowRewriteDialog()
    End Sub

    Private Sub NewWordToolStripMenuItem_Click(sender As Object, e As EventArgs)

    End Sub

    Private Sub NewWordToolStripButton_Click(sender As Object, e As EventArgs) Handles NewWordToolStripButton.Click
        ShowNewEntryDialog()
    End Sub

    Private Sub SearchToolStripButton_Click(sender As Object, e As EventArgs) Handles SearchToolStripButton.Click
        ShowSearchDialog()
    End Sub

    Private Sub SearchToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SearchToolStripMenuItem.Click
        ShowSearchDialog()
    End Sub

    Private Sub EditContextToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles EditContextToolStripMenuItem.Click
        ShowRewriteDialog()
    End Sub

    Private Sub DeleteContextToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DeleteContextToolStripMenuItem.Click
        DeleteSelectedWord()
    End Sub

    Private Sub DeleteToolStripMenuItem_Click(sender As Object, e As EventArgs)
        DeleteSelectedWord()
    End Sub

    Private Sub AboutToolStripButton_Click(sender As Object, e As EventArgs)
        ShowAboutDialog()
    End Sub

    Private Sub AboutToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AboutToolStripMenuItem.Click
        ShowAboutDialog()
    End Sub

    Private Sub ChooseVocabulary()
        ' SaveVocabulary()
        ' RefreshVocabulary()
        Dim chooseVocabDlg = New ChooseVocabDialog
        If chooseVocabDlg.ShowDialog() = Windows.Forms.DialogResult.OK Then
            Try
                currentFileName = chooseVocabDlg.chosenFileName
                megnyitas()
            Catch ex As Exception
                MsgBox("Rossz név.", MsgBoxStyle.Exclamation)
            End Try
        End If
    End Sub

    Private Sub ChooseVocabToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ChooseVocabToolStripMenuItem.Click
        ChooseVocabulary()
    End Sub

    Private Sub SaveToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SaveToolStripMenuItem.Click
        SaveVocabulary()
    End Sub

    Private Sub RefreshToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles RefreshToolStripMenuItem.Click
        RefreshVocabulary()
    End Sub

    Private Sub Minimize()
        If Me.WindowState = FormWindowState.Minimized Then Me.WindowState = FormWindowState.Normal Else Me.WindowState = FormWindowState.Minimized
    End Sub

    Private Sub ExitProgram()
        Form1_FormClosing()
        End
    End Sub

    Private Sub MinimizeToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles MinimizeToolStripMenuItem.Click
        Minimize()
    End Sub

    Private Sub ExitToolItem_Click(sender As Object, e As EventArgs) Handles ExitToolItem.Click
        ExitProgram()
    End Sub

    Private Sub DeleteAllToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DeleteAllToolStripMenuItem.Click
        DeleteAllWords()
    End Sub

    Private Sub BasicSettingsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles BasicSettingsToolStripMenuItem.Click
        ShowSettingsDialog()
    End Sub

    Private Sub NewItemContextToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles NewItemContextToolStripMenuItem.Click
        ShowNewEntryDialog()
    End Sub

    Private Sub SearchContextToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SearchContextToolStripMenuItem.Click
        ShowSearchDialog()
    End Sub

    Private Sub MainForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.Text = My.Application.Info.Title.ToString
        WordsListBox_Click(sender, e)
        ShowSidePane(False)
        LoadSettings()

    End Sub

    Private Sub OpenVocabToolStripButton_Click(sender As Object, e As EventArgs)
        ChooseVocabulary()
    End Sub
    Private Sub WordsListBox_Click(sender As Object, e As EventArgs)

    End Sub

    Private Sub SettingsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SettingsToolStripMenuItem.Click

    End Sub

    Private Sub ToolbarToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ToolbarToolStripMenuItem.Click
        ToolbarToolStripMenuItem.Checked = Not ToolbarToolStripMenuItem.Checked
        ToolStrip.Visible = ToolbarToolStripMenuItem.Checked
        My.Settings.IsToolbarOn = ToolbarToolStripMenuItem.Checked
    End Sub

    Private Sub StatusbarToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles StatusbarToolStripMenuItem.Click
        StatusbarToolStripMenuItem.Checked = Not StatusbarToolStripMenuItem.Checked
        StatusStrip.Visible = StatusbarToolStripMenuItem.Checked
        My.Settings.IsStatusbarOn = StatusbarToolStripMenuItem.Checked
    End Sub

    Private Sub ChooseFontToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ChooseFontToolStripMenuItem.Click
        FontDialog.Font = New Font(WordsListBox.Font.Name, WordsListBox.Font.Size, WordsListBox.Font.Style)
        Dim result = FontDialog.ShowDialog
        If result = DialogResult.OK Then
            ApplyFontDialog()
        End If
    End Sub

    Private Sub FontDialog_Apply(sender As Object, e As EventArgs) Handles FontDialog.Apply
        ApplyFontDialog()
    End Sub

    Private Sub ApplyFontDialog()
        My.Settings.Font = FontDialog.Font
        WordsListBox.Font = My.Settings.Font
        ListView.Font = My.Settings.Font
    End Sub
    Private Sub ChangeColor(color As Color)
        WordsListBox.BackColor = color
        ListView.BackColor = color
        Dim avg = (Convert.ToInt32(color.R) + Convert.ToInt32(color.G) + Convert.ToInt32(color.B)) / 3
        If avg < 100 Then
            WordsListBox.ForeColor = Color.White
            ListView.ForeColor = Color.White
        Else
            WordsListBox.ForeColor = Color.Black
            ListView.ForeColor = Color.Black
        End If
    End Sub

    Private Sub ChooseColorToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ChooseColorToolStripMenuItem.Click
        Dim result = ColorDialog.ShowDialog
        If result = DialogResult.OK Then

            My.Settings.BackColor = ColorDialog.Color
            ChangeColor(ColorDialog.Color)

        End If
    End Sub

    Private Sub DeleteToolStripMenuItem_Click_1(sender As Object, e As EventArgs) Handles DeleteToolStripMenuItem.Click
        DeleteSelectedWord()
    End Sub

    Private Sub NewWordToolStripMenuItem_Click_1(sender As Object, e As EventArgs) Handles NewWordToolStripMenuItem.Click
        ShowNewEntryDialog()
    End Sub

    Private Sub KönyvtárMegnyitásaToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles OpenDirToolStripMenuItem.Click
        Process.Start(FileSystem.CurDir)
    End Sub

    Private Sub RenameToolStripMenuItem_Click_1(sender As Object, e As EventArgs) Handles RenameToolStripMenuItem.Click
        ShowRewriteDialog()
    End Sub

    Private Sub ClassicToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ClassicToolStripMenuItem.Click
        ChangeView(0)
    End Sub

    Private Sub ChangeView(viewId As Integer)

        WaitPictureBox.Visible = False
        My.Settings.View = viewId
        If viewId = 1 Then
            WordsListBox.Visible = False
            ListView.Visible = True
            ListView.Dock = DockStyle.Fill

            ClassicToolStripMenuItem.Checked = False
            ListViewToolStripMenuItem.Checked = True

            ListView.Focus()
        Else
            ListView.Visible = False
            WordsListBox.Visible = True
            WordsListBox.Dock = DockStyle.Fill

            ClassicToolStripMenuItem.Checked = True
            ListViewToolStripMenuItem.Checked = False

            WordsListBox.Focus()
        End If
    End Sub

    Private Sub WaitView()
        WordsListBox.Visible = False
        ListView.Visible = False
        WaitPictureBox.Visible = True
    End Sub

    Private Sub ChangeListViewSort(sort As Integer)
        My.Settings.ListViewSort = sort
        If sort = 1 Then
            ListView.Sorting = SortOrder.Ascending
        ElseIf sort = 2 Then
            ListView.Sorting = SortOrder.Descending
        Else
            ListView.Sorting = SortOrder.None
        End If
        ListView.Sort()
    End Sub

    Private Sub ListViewToolStripMenuItem_Click(sender As Object, e As EventArgs)
        ChangeView(1)
    End Sub

    Private Sub UnsortedToolStripMenuItem_Click(sender As Object, e As EventArgs)
        ChangeListViewSort(0)
    End Sub

    Private Sub AscToolStripMenuItem_Click(sender As Object, e As EventArgs)
        ChangeListViewSort(1)
    End Sub

    Private Sub DescToolStripMenuItem_Click(sender As Object, e As EventArgs)
        ChangeListViewSort(2)
    End Sub

    Private Sub LoadFileBackgroundWorker_DoWork(sender As Object, e As System.ComponentModel.DoWorkEventArgs) Handles LoadFileBackgroundWorker.DoWork
        Dim currentLine As String = ""
        FileOpen(1, currentFileName, OpenMode.Input)
        Do Until EOF(1)
            Input(1, currentLine)
            'WordListbox-ba
            words.Add(currentLine)
        Loop
        FileClose(1)
    End Sub

    Private Sub LoadFileBackgroundWorker_RunWorkerCompleted(sender As Object, e As RunWorkerCompletedEventArgs) Handles LoadFileBackgroundWorker.RunWorkerCompleted

        ' lfrm.Show()
        ' lfrm.Focus()

        Dim i = 0
        ToolStripProgressBar.Focus()

        Dim oldpercent = 0
        Dim SCALE = 15

        For Each word As String In words
            InsertItem(-1, word, False)
            ToolStripProgressBar.Value = i

            ' Play Sounds
            If My.Settings.SoundOnLoad Then
                Dim percent As Integer = (ToolStripProgressBar.Value / ToolStripProgressBar.Maximum) * 100
                If percent = 100 And oldpercent <> percent Then
                    My.Computer.Audio.Play(My.Resources.dne,
        AudioPlayMode.WaitToComplete)
                    oldpercent = percent
                ElseIf (percent Mod SCALE) = 0 And oldpercent <> percent Then
                    My.Computer.Audio.Play(My.Resources.katt2,
        AudioPlayMode.WaitToComplete)
                    oldpercent = percent
                End If
            End If


            i = i + 1
        Next

        ToolStripProgressBar.Visible = False
        vocabChanged = False

        Cursor = Cursors.Default

        ShowSidePane(My.Settings.IsSidePaneOn)
        ChangeView(My.Settings.View)
        ' lfrm.Close()
    End Sub

    Private Sub EditorToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles EditorToolStripMenuItem.Click
        Dim editorDialog As New EditorDialog
        editorDialog.Text = currentFileName
        For Each item As String In words
            editorDialog.TextBox.Text = editorDialog.TextBox.Text + item + vbNewLine
        Next

        If editorDialog.ShowDialog() = DialogResult.OK Then
            WriteAllText(currentFileName, editorDialog.TextBox.Text)
            RefreshVocabulary()
        End If
    End Sub

    Private Sub BkImToolStripMenuItem_Click(sender As Object, e As EventArgs)
        Dim bkDlg As New BackgroundDialog
        bkDlg.ShowDialog()
    End Sub

    Private Sub LoadFileBackgroundWorker_ProgressChanged(sender As Object, e As ProgressChangedEventArgs) Handles LoadFileBackgroundWorker.ProgressChanged
        ToolStripProgressBar.PerformStep()
    End Sub

    Private Sub MainForm_Shown(sender As Object, e As EventArgs) Handles MyBase.Shown
        RefreshVocabulary()
    End Sub

    Private Sub ListViewToolStripMenuItem_Click_1(sender As Object, e As EventArgs) Handles ListViewToolStripMenuItem.Click
        ChangeView(1)
    End Sub

    Private Sub SplitContainer1_Panel2_Paint(sender As Object, e As PaintEventArgs) Handles SplitContainer.Panel2.Paint

    End Sub

    Private Sub WordsListBox_Click_1(sender As Object, e As EventArgs) Handles WordsListBox.Click

    End Sub

    Private Sub WordsListBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles WordsListBox.SelectedIndexChanged
        If (WordsListBox.SelectedIndex > -1) Then
            ListView.Items(ListView.Items.IndexOfKey(WordsListBox.SelectedIndex)).Selected = True
            ListView.Items(ListView.Items.IndexOfKey(WordsListBox.SelectedIndex)).Focused = True
        End If

        'SidePane
        Try
            Dim word As String = WordsListBox.SelectedItems(0).ToString()
            Dim wordParts() As String = word.Split("=")
            Dim wordMeaningParts() = wordParts(1).Split(",")

            Label.Text = wordParts(0)

            For Each item As String In wordMeaningParts
                Label.Text = Label.Text + vbNewLine + " × " + item
            Next
        Catch ex As Exception
            Label.Text = "Nincs megtekintés."
        End Try

    End Sub

    Private Sub CorrectButton_Click(sender As Object, e As EventArgs) Handles CorrectButton.Click
        ShowRewriteDialog()
    End Sub

    Private Sub SeparatorToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SeparatorToolStripMenuItem.Click
        InsertItem(WordsListBox.SelectedIndex + 1, "")
    End Sub

    Private Sub InsertItem(indexAfter As Integer, word As String, Optional insertToListContainer As Boolean = True)

        'list
        If insertToListContainer Then
            If indexAfter = -1 Then
                words.Add(word)
            Else
                words.Insert(indexAfter, word)
            End If
        End If

        'WordListBox
        If indexAfter = -1 Then
            WordsListBox.Items.Add(word)
        Else
            WordsListBox.Items.Insert(indexAfter, word)
        End If

        'Listview
        Dim lvi As New ListViewItem
        lvi.Text = word

        If word.Length = 0 Then
            lvi.ImageIndex = 2
        ElseIf VocabularyHelper.isBrokenWord(word) Then
            lvi.ImageIndex = 1
        Else
            lvi.ImageIndex = 0
        End If

        lvi.Name = Convert.ToString(WordsListBox.Items.Count - 1)
        lvi.Tag = Convert.ToString(WordsListBox.Items.Count - 1)

        If indexAfter = -1 Then
            ListView.Items.Add(lvi)
        Else
            MsgBox("Menne")
            ListView.Items.Insert(indexAfter, lvi)
        End If

    End Sub

    Private Sub RenameItem(index As Integer, newText As String)
        'List
        words(index) = newText
        'Listbox
        Me.WordsListBox.Items(index) = newText
        Me.WordsListBox.SelectedItem = index
        'ListView
        Me.ListView.SelectedItems.Item(0).Text = newText
    End Sub

    Private Sub RemoveItem(index As Integer)
        'List
        words.RemoveAt(index)
        'ListBox
        WordsListBox.Items.RemoveAt(WordsListBox.SelectedIndex)
        'ListView
        ListView.SelectedItems.Item(0).Remove()
    End Sub

    Private Sub ListView_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ListView.SelectedIndexChanged
        If (ListView.SelectedItems.Count > 0) Then
            WordsListBox.SelectedIndex = Convert.ToInt32(ListView.SelectedItems.Item(0).Name)
        End If
    End Sub

    Private Sub SwitchSidePane()
        My.Settings.IsSidePaneOn = Not My.Settings.IsSidePaneOn
        SplitContainer.Panel2Collapsed = Not My.Settings.IsSidePaneOn
        SidePaneMenuItem.Checked = My.Settings.IsSidePaneOn
    End Sub

    Private Sub ShowSidePane(isShown As Boolean)
        SplitContainer.Panel2Collapsed = Not isShown
    End Sub

    Private Sub SidePaneMenuItem_Click(sender As Object, e As EventArgs) Handles SidePaneMenuItem.Click
        SwitchSidePane()
    End Sub

    Private Sub CloseSidePaneButton_Click(sender As Object, e As EventArgs) Handles CloseSidePaneButton.Click
        SwitchSidePane()
    End Sub

    Private Sub GithubToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles GithubToolStripMenuItem.Click
        Process.Start("https://github.com/eleda/InfraVocWindows3")
    End Sub

    Private Sub WordsListBox_DoubleClick_1(sender As Object, e As EventArgs) Handles WordsListBox.DoubleClick
        ShowRewriteDialog()
    End Sub

    Private Sub ListView_HandleCreated(sender As Object, e As EventArgs) Handles ListView.HandleCreated

    End Sub

    Private Sub ListView_Click(sender As Object, e As EventArgs) Handles ListView.Click
        WordsListBox_Click(sender, e)
    End Sub

    Private Sub ListView_DoubleClick(sender As Object, e As EventArgs) Handles ListView.DoubleClick
        ShowRewriteDialog()
    End Sub

    Private Sub RemoveButton_Click(sender As Object, e As EventArgs) Handles RemoveButton.Click
        DeleteSelectedWord()
    End Sub

    Private Sub WordsListBox_KeyUp(sender As Object, e As KeyEventArgs)

    End Sub

    Private Sub ListView_KeyUp(sender As Object, e As KeyEventArgs)

    End Sub

    Private Sub ListView_KeyU(sender As Object, e As KeyEventArgs)
    End Sub

    Private Sub SoundOnLoadToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SoundOnLoadToolStripMenuItem.Click
        My.Settings.SoundOnLoad = Not My.Settings.SoundOnLoad
        SoundOnLoadToolStripMenuItem.Checked = My.Settings.SoundOnLoad
    End Sub

    Private Sub StatToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles StatToolStripMenuItem.Click
        Dim statText As String = ""
        statText = statText + "Ez a fájl " + Convert.ToString(WordsListBox.Items.Count) + " darab szót tartalmaz. "
        MsgBox(statText, vbInformation)
    End Sub

    Private Sub ÚjSzótárToolStripMenuItem_Click(sender As Object, e As EventArgs)

    End Sub

    Private Sub NewVocabToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles NewVocabToolStripMenuItem.Click
        NewVocabulary()
    End Sub
End Class