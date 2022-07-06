Option Strict Off
Option Explicit On
Imports Microsoft.VisualBasic.PowerPacks

Friend Class frmDispCon
    Inherits System.Windows.Forms.Form

    '**************************************************
    '* frmDispCon version info:
    '*  See basGlobals (Globals.bas).
    '**************************************************
    Public bShowTip As Boolean
    Public WavNamePart1 As String '* 'Con' (Set in Form_Load)
    Public CurrIndex As Short

    Private iMouseButton As Short
    Private Const INISection As String = "Consonants"
    Private Const ShowTipEntry As String = "ShowTip"
    Private Const TBarButtons As String = "PlayOnly;PlayInterVocalic;PlaySeparator;Record;StopRec;PlayRec;PlayRecSpeaker;RecordSeparator;Test;TestSeparator;Exit;"
    Private Const MaxCons As Short = 84
    Private Const FrmMaxWidth As Short = 9045
    Private Const FrmMaxHeight As Short = 5550
    Private Const statusMsg1 As String = " Click on a consonant to select it. "
    Private Const statusMsg2 As String = "Press a position button to " & "see a list of words using " & "the selected consonant."

    Public Sub IPAHelpPrint(ByRef bToPrinter As Boolean, ByRef bColorBackground As Boolean)

        Dim pic As System.Drawing.Image
        'UPGRADE_NOTE: Capture was upgraded to Capture_Renamed. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="A9E4979A-37FA-4718-9994-97DD76ED70A7"'
        Dim Capture_Renamed As clsCapture

        On Error Resume Next
        pic = New System.Drawing.Bitmap(1, 1)
        Capture_Renamed = New clsCapture

        If (CurrIndex > -1) Then
            Con(CurrIndex).BackColor = System.Drawing.SystemColors.Control
            Con(CurrIndex).ForeColor = System.Drawing.SystemColors.ControlText
            System.Windows.Forms.Application.DoEvents()
        End If

        If (bToPrinter) Then
            pic = Capture_Renamed.CaptureWindowArea(Me, 0, 4, 595, 342, True)
            Capture_Renamed.PrintChart(pic, "Chart:" & vbTab & vbTab & "IPA Consonants")
        Else
            pic = Capture_Renamed.CaptureWindowArea(Me, 0, 0, 595, 342, IIf(bColorBackground, False, True))
            My.Computer.Clipboard.Clear()
            My.Computer.Clipboard.SetImage(pic)
        End If

        'UPGRADE_NOTE: Object Capture_Renamed may not be destroyed until it is garbage collected. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6E35BFF6-CD74-4B09-9689-3E1A43DF8969"'
        Capture_Renamed = Nothing
        'UPGRADE_NOTE: Object pic may not be destroyed until it is garbage collected. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6E35BFF6-CD74-4B09-9689-3E1A43DF8969"'
        pic = Nothing

        If (CurrIndex > -1) Then
            Con(CurrIndex).BackColor = System.Drawing.SystemColors.Highlight
            Con(CurrIndex).ForeColor = System.Drawing.SystemColors.HighlightText
        End If

    End Sub

    Public Sub Play(ByRef iButton As Short)

        '**************************************************
        '* This function allows mdiHelpCharts to play the
        '* wave file corresponding to the currently
        '* selected symbol, without knowing which form is
        '* active.
        '**************************************************

        On Error Resume Next
        If (Len(gMMCtrl.Tag) > 0) Then Exit Sub
        If (iButton = 1) Then CType(mdiHelpCharts.Controls("Timer1"), Object).Enabled = False
        gMMCtrl.Tag = cMCIBusy
        gMMCtrl.Wait = True
        Call PlayWav(WavNamePart1 & "-" & VB6.Format(Trim(Str(CurrIndex)), "00") & IIf(iButton = 0, "a", "b") & ".wav")
        gMMCtrl.Tag = ""

    End Sub

    Private Function ReadConsFromDB() As Boolean

        '**************************************************
        '* This function loads the characters from the INI
        '* file and displays them on the form.
        '**************************************************

        Dim i As Short
        Dim sINIVals(,) As String

        On Error GoTo ReadConsFromDBErr
        ReadConsFromDB = False
        sINIVals = GetAllINISettings(gINIPath, INISection)
        CurrIndex = 0
        If (IsDBNull(sINIVals)) Then Exit Function

        For i = 0 To UBound(sINIVals, 1)
            With Con(i)
                Call GetCharFromINIStr(sINIVals(i, 1), Con(i))
                .BackColor = System.Drawing.SystemColors.Control
                .ForeColor = System.Drawing.SystemColors.ControlText
                .SendToBack()
            End With
        Next i

        ReadConsFromDB = True
        Exit Function

ReadConsFromDBErr:

    End Function

    Public Sub UpdateAfterRecordAndPlayback()

        On Error Resume Next
        mdiHelpCharts.EnableTBarButtons(TBarButtons)

    End Sub

    Public Sub UpdateFormAfterTest()

        '**************************************************
        '* This function restores anything changed on the
        '* form for testing purposed (location of symbols,
        '* Visible properties, etc.).
        '**************************************************

        Dim i As Short
        Dim iConGrpInd As Short
        Dim iGrpInd As Short
        Dim iOldLeft As Short
        Dim iOldTop As Integer

        lblSmile.Visible = False
        lblFrown.Visible = False
        With Con(gItemNumber)
            .ForeColor = System.Drawing.SystemColors.ControlText
            .BackColor = System.Drawing.SystemColors.Control
        End With

        CurrIndex = 0
        With Con(CurrIndex)
            .BackColor = System.Drawing.SystemColors.Highlight
            .ForeColor = System.Drawing.SystemColors.HighlightText
        End With

        '**********************************************
        '* Restore Cons to original location (if Brief
        '* test mode), Enabled and Visible state.
        '**********************************************
        iConGrpInd = 0
        iGrpInd = 0
        For i = 0 To MaxCons - 1
            'UPGRADE_WARNING: Couldn't resolve default property of object gPhonGrpColl(AllCon)(iConGrpInd). Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
            If i = gPhonGrpColl.Item("AllCon")(iConGrpInd) Then
                'UPGRADE_WARNING: Couldn't resolve default property of object gTestGrp(iGrpInd). Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
                If gTestLayout = cTestBrief And i = gTestGrp(iGrpInd) Then
                    If gTestGrpCorrect(cLastTest, iGrpInd) <> System.Windows.Forms.CheckState.Checked Then
                        '* Restore location.  *
                        With Con(i)
                            .Visible = False
                            iOldLeft = Val(Mid(.Tag, 1, 4))
                            iOldTop = Val(Mid(.Tag, 5, 4))
                            .Left = VB6.TwipsToPixelsX(iOldLeft)
                            .Top = VB6.TwipsToPixelsY(iOldTop)
                            .Tag = Mid(.Tag, 9)
                        End With
                    Else
                        '* Enable untested cons (in this group) with sounds *
                        Con(i).Enabled = True
                    End If
                    iGrpInd = IIf(iGrpInd = UBound(gTestGrp), iGrpInd, iGrpInd + 1)
                Else
                    '* Enable other Cons with sounds. *
                    Con(i).Enabled = True
                End If
                iConGrpInd = IIf(iConGrpInd = UBound(gPhonGrpColl.Item("AllCon")), iConGrpInd, iConGrpInd + 1)
            End If
        Next i

        If gTestLayout = cTestBrief Then
            '* Make all Cons visible.  *
            For i = 0 To MaxCons - 1
                Con(i).Visible = True
            Next i
        End If

        '**********************************************
        '* If in brief mode, replace the scenery.
        '**********************************************
        If gTestLayout = cTestBrief Then
            For i = 0 To 54
                Select Case i
                    Case 0 To 9
                        Line1(i).Visible = True
                        label1(i).Visible = True
                        Shape1(i).Visible = True
                    Case 10 To 24
                        Line1(i).Visible = True
                        label1(i).Visible = True
                    Case 25 To 54
                        label1(i).Visible = True
                End Select
            Next i
            With Frame1
                .Left = VB6.TwipsToPixelsX(45)
                .Top = VB6.TwipsToPixelsY(3060)
                .Visible = True
            End With
            With Frame2
                .Left = VB6.TwipsToPixelsX(3945)
                .Top = VB6.TwipsToPixelsY(3060)
                .Visible = True
            End With
        End If

        mdiHelpCharts.EnableTBarButtons(TBarButtons)

    End Sub

    Public Sub UpdateFormForTest()

        '***********************************************************************
        '* This function performs any changes to the form
        '* necessary in preparation for testing.
        '*
        '* Whether a con is visible or not is now determined by its status in
        '* gTestGrpCorrect. This means that gTestGrp does not change depending on
        '* how many the user got correct. This also allows the indexing to be
        '* the same for both gTestGrp and gTestGrpCorrect. CLW 1/27/99
        '***********************************************************************

        Dim i As Short
        Dim iPrev As Short
        Dim iTestCount As Short '* Number of cons tested on CLW 1/27/99
        Dim iGrpInd As Short
        Dim iMainCon As Short
        Dim iNonPulCon As Short
        Dim iOtherCon As Short
        Dim iMainRowLength As Short
        Dim iNonPulRowLength As Short
        Dim iOtherRowLength As Short
        Dim iCol As Short
        Dim iLeftStart As Short
        Dim iTop As Short
        Dim iOldLeft As Short
        Dim iOldTop As Short

        iMainCon = 0
        iNonPulCon = 0
        iOtherCon = 0

        With Con(CurrIndex)
            .BackColor = System.Drawing.SystemColors.Control
            .ForeColor = System.Drawing.SystemColors.ControlText
        End With

        '**********************************************
        '* First disable all Cons.
        '* If Brief mode, make invisible.
        '**********************************************
        For i = 0 To MaxCons - 1
            With Con(i)
                .Enabled = False
                If gTestLayout = cTestBrief Then .Visible = False
            End With
        Next i

        Select Case gTestLayout
            Case cTestBrief
                '********************************************
                '* Clear the extra scenery.
                '********************************************
                For i = 0 To 54
                    Select Case i
                        Case 0 To 9
                            Line1(i).Visible = False
                            label1(i).Visible = False
                            Shape1(i).Visible = False
                        Case 10 To 24
                            Line1(i).Visible = False
                            label1(i).Visible = False
                        Case 25 To 54
                            label1(i).Visible = False
                    End Select
                Next i
                Select Case gTestGrpName
                    Case "AllCon"
                        ' Find frame divisions in test group
                        For i = 0 To UBound(gTestGrp)
                            If gTestGrpCorrect(cLastTest, i) <> System.Windows.Forms.CheckState.Checked Then
                                Select Case gTestGrp(i)
                                    Case 0 To 57 : iMainCon = iMainCon + 1
                                    Case 58 To 71 : iNonPulCon = iNonPulCon + 1
                                    Case 72 To 83 : iOtherCon = iOtherCon + 1
                                End Select
                            End If
                        Next i

                        iLeftStart = (VB6.PixelsToTwipsX(Width) - iMainRowLength * 315) / 2
                        iTop = (VB6.PixelsToTwipsY(Height) - VB6.PixelsToTwipsY(Frame1.Height) - (Int((iMainCon + 9) / 10)) * 425) / 2

                    Case "NonPulCon"
                        Frame2.Visible = False
                        With Frame1
                            .Top = VB6.TwipsToPixelsY(VB6.PixelsToTwipsY(Frame1.Top) / 2)
                            .Left = VB6.TwipsToPixelsX(VB6.PixelsToTwipsX(Frame2.Width) / 2)
                        End With
                        '* Calculate number of NonPulCons in visible group
                        For i = 0 To UBound(gTestGrp)
                            If gTestGrpCorrect(cLastTest, i) <> System.Windows.Forms.CheckState.Checked Then iNonPulCon = iNonPulCon + 1
                        Next i

                    Case "OtherCon"
                        Frame1.Visible = False
                        With Frame2
                            .Top = VB6.TwipsToPixelsY(VB6.PixelsToTwipsY(Frame2.Top) / 2)
                            .Left = VB6.TwipsToPixelsX(VB6.PixelsToTwipsX(Frame1.Width) / 2)
                        End With
                        '* Calculate number of OtherCons in visible group
                        For i = 0 To UBound(gTestGrp)
                            If gTestGrpCorrect(cLastTest, i) <> System.Windows.Forms.CheckState.Checked Then iOtherCon = iOtherCon + 1
                        Next i

                    Case Else
                        Frame1.Visible = False
                        Frame2.Visible = False
                        '* Calculate number of MainCons in visible group
                        For i = 0 To UBound(gTestGrp)
                            If gTestGrpCorrect(cLastTest, i) <> System.Windows.Forms.CheckState.Checked Then iMainCon = iMainCon + 1
                        Next i
                        iTop = (VB6.PixelsToTwipsY(Height) - (Int((iMainCon + 9) / 10)) * 425) / 2
                End Select

                '********************************************
                '* Rearrange test Cons in rows of 10,
                '* and make visible.
                '********************************************
                iMainRowLength = IIf(iMainCon < 10, iMainCon, 10)
                iNonPulRowLength = IIf(iNonPulCon < 10, iNonPulCon, 10)
                iOtherRowLength = IIf(iOtherCon < 10, iOtherCon, 10)

                For iGrpInd = 0 To UBound(gTestGrp)
                    'UPGRADE_WARNING: Couldn't resolve default property of object gTestGrp(). Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
                    i = gTestGrp(iGrpInd)
                    If iGrpInd = 0 Then
                        iPrev = 0
                    Else
                        'UPGRADE_WARNING: Couldn't resolve default property of object gTestGrp(). Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
                        iPrev = gTestGrp(iGrpInd - 1)
                    End If
                    'Initialize new container

                    If iGrpInd = 0 Or Con(i).Parent.Name <> Con(iPrev).Parent.Name Then
                        iCol = 0
                        Select Case i
                            Case 0 To 57 : iLeftStart = (VB6.PixelsToTwipsX(Width) - iMainRowLength * 315) / 2
                            Case 58 To 71 : iLeftStart = (VB6.PixelsToTwipsX(Frame1.Width) - iNonPulRowLength * 315) / 2
                                iTop = (VB6.PixelsToTwipsY(Frame1.Height) - (Int((iNonPulCon + 9) / 10)) * 425) / 2
                            Case 72 To 83 : iLeftStart = (VB6.PixelsToTwipsX(Frame2.Width) - iOtherRowLength * 315) / 2
                                iTop = (VB6.PixelsToTwipsY(Frame2.Height) - (Int((iOtherCon + 9) / 10)) * 425) / 2
                        End Select
                    End If

                    If gTestGrpCorrect(cLastTest, iGrpInd) <> System.Windows.Forms.CheckState.Checked Then
                        'Begin new line
                        If iCol = 10 Then
                            iTop = iTop + 425
                            iCol = 0
                        End If
                        'Position Cons
                        With Con(i)
                            iOldLeft = VB6.PixelsToTwipsX(.Left)
                            iOldTop = VB6.PixelsToTwipsY(.Top)
                            .Tag = VB6.Format(iOldLeft, "0000") & VB6.Format(iOldTop, "0000") & .Tag
                            .Left = VB6.TwipsToPixelsX(iLeftStart + 315 * iCol)
                            .Top = VB6.TwipsToPixelsY(iTop)
                            .Enabled = True
                            .Visible = True
                            iCol = iCol + 1
                        End With
                    End If
                Next iGrpInd

            Case cTestChart
                For iGrpInd = 0 To UBound(gTestGrp)
                    If gTestGrpCorrect(cLastTest, iGrpInd) <> System.Windows.Forms.CheckState.Checked Then
                        'UPGRADE_WARNING: Couldn't resolve default property of object gTestGrp(). Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
                        i = gTestGrp(iGrpInd)
                        Con(i).Enabled = True
                    End If
                Next iGrpInd
        End Select

    End Sub

    Private Sub Con_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Con.Click
        Dim Index As Short = Con.GetIndex(eventSender)

        If iMouseButton = VB6.MouseButtonConstants.LeftButton Then
            '**********************************************
            '* If in test mode ...
            '**********************************************
            If gTestActive Then
                '*********************************************
                '* ... check to see if correct IPA character
                '* selected. If so, put smile on top of
                '* character.
                '*********************************************
                'UPGRADE_WARNING: Couldn't resolve default property of object gItemNumber. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
                If gItemNumber = Index Then
                    lblFrown.Visible = False
                    With lblSmile
                        .Visible = False
                        '****************************************
                        '* Update container when switching
                        '* between 'main consonant',
                        '* 'nonpulmonic', and 'other' frames.
                        '****************************************
                        If Not (.Parent Is Con(Index).Parent) Then
                            .Parent = Con(Index).Parent
                        End If
                        .Height = Con(Index).Height
                        .Width = Con(Index).Width
                        .Top = Con(Index).Top
                        .Left = Con(Index).Left
                        .Visible = True
                        .BringToFront()
                    End With
                    Call MarkItemCorrect(System.Windows.Forms.CheckState.Checked)
                    '* Make smile visible for 2 seconds, the continue
                    With mdiHelpCharts.Timer2
                        .Enabled = False
                        .Interval = 2000
                        .Enabled = True
                    End With
                    '*********************
                    '* If not, put frown.
                    '*********************
                ElseIf lblSmile.Visible = False Then
                    With lblFrown
                        .Visible = False
                        If Not (.Parent Is Con(Index).Parent) Then
                            .Parent = Con(Index).Parent
                        End If
                        .Height = Con(Index).Height
                        .Width = Con(Index).Width
                        .Top = Con(Index).Top
                        .Left = Con(Index).Left
                        .Visible = True
                        .BringToFront()
                    End With
                    Call MarkItemCorrect(System.Windows.Forms.CheckState.Unchecked, Index)
                End If
            Else
                '*********************************************
                '* If not in test mode, start double-click
                '* timer.
                '*********************************************
                With CType(mdiHelpCharts.Controls("Timer1"), Object)
                    'UPGRADE_WARNING: Timer property .Interval cannot have a value of 0. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="169ECF4A-1968-402D-B243-16603CC08604"'
                    .Interval = GetDoubleClickTime()
                    .Enabled = True
                End With
            End If
        End If

    End Sub

    Private Sub Con_DoubleClick(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Con.DoubleClick
        Dim Index As Short = Con.GetIndex(eventSender)

        On Error Resume Next
        If iMouseButton = VB6.MouseButtonConstants.LeftButton Then Call Play(1)

    End Sub

    Private Sub Con_MouseDown(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.MouseEventArgs) Handles Con.MouseDown
        Dim Button As Short = eventArgs.Button \ &H100000
        Dim Shift As Short = System.Windows.Forms.Control.ModifierKeys \ &H10000
        Dim X As Single = VB6.PixelsToTwipsX(eventArgs.X)
        Dim Y As Single = VB6.PixelsToTwipsY(eventArgs.Y)
        Dim Index As Short = Con.GetIndex(eventSender)

        '***********************************************
        '* Make the character that CurrIndex points
        '* to not look selected and make the character
        '* just clicked on look selected by changing
        '* its background color.
        '***********************************************

        On Error Resume Next
        If gTestActive = False Then
            With Con(CurrIndex)
                '* Deselect previous selection.
                .BackColor = System.Drawing.SystemColors.Control
                .ForeColor = System.Drawing.SystemColors.ControlText
            End With
            With Con(Index)
                '* Select new selection.
                .BackColor = System.Drawing.SystemColors.Highlight
                .ForeColor = System.Drawing.SystemColors.HighlightText
            End With
        End If
        CurrIndex = Index
        iMouseButton = Button

    End Sub

    Private Sub Con_MouseMove(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.MouseEventArgs) Handles Con.MouseMove
        Dim Button As Short = eventArgs.Button \ &H100000
        Dim Shift As Short = System.Windows.Forms.Control.ModifierKeys \ &H10000
        Dim X As Single = VB6.PixelsToTwipsX(eventArgs.X)
        Dim Y As Single = VB6.PixelsToTwipsY(eventArgs.Y)
        Dim Index As Short = Con.GetIndex(eventSender)

        On Error Resume Next
        'UPGRADE_WARNING: Couldn't resolve default property of object GetCaptionFromTag(). Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
        If Not gTestActive Then gStatLine.Text = GetCaptionFromTag(Con(Index).Tag)

    End Sub

    'UPGRADE_WARNING: Form event frmDispCon.Activate has a new behavior. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6BA9B8D2-2A32-4B6E-8D36-44949974A5B4"'
    Private Sub frmDispCon_Activated(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Activated

        On Error Resume Next
        Show()
        WindowState = System.Windows.Forms.FormWindowState.Maximized
        gStatLine.Text = ""

        Dim vAdvice As Object
        With mdiHelpCharts
            CType(.Controls("panStatus"), Object).Visible = True
            .ShowTBarButtons(TBarButtons)
            .EnableTBarButtons(TBarButtons)
            CType(.Controls("mnuExportBitmap"), Object).Visible = True
            CType(.Controls("mnuPrint"), Object)(0).Visible = True
            CType(.Controls("mnuPrint"), Object)(1).Visible = True

            '**************************************************
            '* Give the user advice the first time. Optionally,
            '* (specified in the .ini file) IPAHelp can always
            '* play the selected symbol (by default, this is
            '* the first symbol on the form, when opened).
            '**************************************************
            'UPGRADE_WARNING: Couldn't resolve default property of object vAdvice. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
            vAdvice = GetINIEntry(cSettingsSect, cNewUserAdviceEntry, gINIPath)
            Select Case vAdvice
                Case "1" 'Offer advice to a new user the first time only
                    gMsg = "IPA Help can play audio samples for the IPA symbols." & vbCrLf & "Use the mouse pointer to select and payback an audio sample."
                    MsgBox(gMsg, MsgBoxStyle.Information, My.Application.Info.Title)
                    Call WriteINIEntry(cSettingsSect, cNewUserAdviceEntry, "0", gINIPath)
                Case "2" 'Always play the opening sound
                    .Timer1.Interval = 2000
                    .Timer1.Enabled = True
                Case Else
                    '* Do nothing
            End Select
        End With

        'UPGRADE_ISSUE: Unable to determine which constant to upgrade vbNormal to. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="B3B44E51-B5F1-4FD7-AA29-CAD31B71F487"'
        If WindowState = vbNormal Then
            Top = VB6.TwipsToPixelsY(-VB6.PixelsToTwipsY(Height))
            WindowState = System.Windows.Forms.FormWindowState.Maximized
        End If

    End Sub

    'UPGRADE_WARNING: Form event frmDispCon.Deactivate has a new behavior. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6BA9B8D2-2A32-4B6E-8D36-44949974A5B4"'
    Private Sub frmDispCon_Deactivate(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Deactivate

        On Error Resume Next
        gStatLine.Text = ""
        CType(mdiHelpCharts.Controls("mnuExportBitmap"), Object).Visible = False
        CType(mdiHelpCharts.Controls("mnuPrint"), Object)(0).Visible = False
        CType(mdiHelpCharts.Controls("mnuPrint"), Object)(1).Visible = False

    End Sub

    Private Sub frmDispCon_KeyDown(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.KeyEventArgs) Handles MyBase.KeyDown
        Dim KeyCode As Short = eventArgs.KeyCode
        Dim Shift As Short = eventArgs.KeyData \ &H10000

        '************************************************************
        '* This routine will monitor keypresses looking for arrow
        '* keys. If one of the arrow keys is pressed then the
        '* current consonant is changed to the next or previous
        '* enabled consonant (depending upon which arrow is pressed).
        '************************************************************

        Dim i As Short

        On Error Resume Next
        If gTestActive Then Exit Sub

        i = CurrIndex
        Select Case KeyCode
            Case System.Windows.Forms.Keys.Down, System.Windows.Forms.Keys.Right 'Is keypress right or down?
                i = IIf(CurrIndex = MaxCons - 1, 0, CurrIndex + 1) 'Move to next consonant that
                While Not Con(i).Enabled And i <> CurrIndex '  is enabled or until we've
                    i = IIf(i = MaxCons - 1, 0, i + 1) '  gone full circle through
                End While '  all the consonants.

            Case System.Windows.Forms.Keys.Up, System.Windows.Forms.Keys.Left 'Is keypress left or up?
                i = IIf(CurrIndex = 0, MaxCons - 1, CurrIndex - 1) 'Move to previous consonant
                While Not Con(i).Enabled And i <> CurrIndex '  that's enabled or until
                    i = IIf(i = 0, MaxCons - 1, i - 1) '  we've gone full circle
                End While '  through all the consonants.

            Case System.Windows.Forms.Keys.Return : Call Play(IIf((Shift And VB6.ShiftConstants.ShiftMask) = 0, 0, 1))

        End Select

        '**************************************************************
        '* If we landed on an enabled Con  act like we clicked on it.
        '**************************************************************
        If Con(i).Enabled Then Call Con_MouseDown(Con.Item(i), New System.Windows.Forms.MouseEventArgs(&H100000, 0, 0, 0, 0))

    End Sub

    Private Sub frmDispCon_KeyPress(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.KeyPressEventArgs) Handles MyBase.KeyPress
        Dim KeyAscii As Short = Asc(eventArgs.KeyChar)

        On Error Resume Next
        If KeyAscii = System.Windows.Forms.Keys.Escape Then
            If gTestActive Then
                Call mdiHelpCharts.mnuTestStop_Click(Nothing, New System.EventArgs())
            Else
                Me.Close()
            End If
        End If

        eventArgs.KeyChar = Chr(KeyAscii)
        If KeyAscii = 0 Then
            eventArgs.Handled = True
        End If
    End Sub

    Private Sub frmDispCon_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load

        mdiHelpCharts.panStatus.Visible = True
        'UPGRADE_WARNING: Screen property Screen.MousePointer has a new behavior. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6BA9B8D2-2A32-4B6E-8D36-44949974A5B4"'
        System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.WaitCursor
        WavNamePart1 = "Con"

        'UPGRADE_ISSUE: Unable to determine which constant to upgrade vbNormal to. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="B3B44E51-B5F1-4FD7-AA29-CAD31B71F487"'
        If WindowState = vbNormal Then
            Height = VB6.TwipsToPixelsY(FrmMaxHeight)
            Width = VB6.TwipsToPixelsX(FrmMaxWidth)
            Top = 0
            Left = 0
        End If
        CurrIndex = -1

        'UPGRADE_WARNING: Couldn't resolve default property of object ReadConsFromDB(). Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
        If ReadConsFromDB() Then
            'If next line is false then there's no consonants in DB.
            If (CurrIndex > -1 And Not gTestActive) Then
                With Con(CurrIndex)
                    .BackColor = System.Drawing.SystemColors.Highlight 'Select first enabled consonant.
                    .ForeColor = System.Drawing.SystemColors.HighlightText
                End With
            End If
        Else
            'UPGRADE_WARNING: Screen property Screen.MousePointer has a new behavior. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6BA9B8D2-2A32-4B6E-8D36-44949974A5B4"'
            System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.Default
            MsgBox("Error reading consonant information from the settings file.", MsgBoxStyle.Information + MsgBoxStyle.OkOnly, My.Application.Info.Title)
            Me.Close()
            Exit Sub
        End If

        'UPGRADE_WARNING: Screen property Screen.MousePointer has a new behavior. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6BA9B8D2-2A32-4B6E-8D36-44949974A5B4"'
        System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.Default
        Top = VB6.TwipsToPixelsY(-VB6.PixelsToTwipsY(Height))
        Show()
        WindowState = System.Windows.Forms.FormWindowState.Maximized

    End Sub

    Private Sub frmDispCon_MouseMove(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.MouseEventArgs) Handles MyBase.MouseMove
        Dim Button As Short = eventArgs.Button \ &H100000
        Dim Shift As Short = System.Windows.Forms.Control.ModifierKeys \ &H10000
        Dim X As Single = VB6.PixelsToTwipsX(eventArgs.X)
        Dim Y As Single = VB6.PixelsToTwipsY(eventArgs.Y)

        On Error Resume Next
        If gTestActive Then gStatLine.Text = "Select correct character" Else gStatLine.Text = ""

    End Sub

    Private Sub frmDispCon_FormClosing(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        Dim Cancel As Boolean = eventArgs.Cancel
        Dim UnloadMode As System.Windows.Forms.CloseReason = eventArgs.CloseReason

        On Error Resume Next
        'UPGRADE_WARNING: Form event frmDispCon.Deactivate has a new behavior. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6BA9B8D2-2A32-4B6E-8D36-44949974A5B4"'
        frmDispCon_Deactivate(Me, New System.EventArgs())
        'UPGRADE_NOTE: Object frmDispCon may not be destroyed until it is garbage collected. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6E35BFF6-CD74-4B09-9689-3E1A43DF8969"'
        Me.Dispose()

        eventArgs.Cancel = Cancel
    End Sub
End Class