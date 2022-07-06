Module Module1
    'INI 스트링을 읽어오기 위한 API 선언 
    Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As String, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Int32, ByVal lpFileName As String) As Long
    'INI 스트링을 기록하기 위한 API 선언 
    Declare Function WritePrivateProfileString Lib "kernel32" Alias "WritePrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As String, ByVal lpString As String, ByVal lpFileName As String) As Long
    '마우스 이벤트 API 선언
    Declare Sub mouse_event Lib "user32" Alias "mouse_event" (ByVal dwFlags As Integer, ByVal dx As Integer, ByVal dy As Integer, ByVal cButtons As Integer, ByVal dwExtaInfo As Integer)

    '뮤텍스 선언
    Public blCreated As Boolean
    Public tdMtx As New System.Threading.Mutex(True, "MyMutex", blCreated)

    Public blCtrlKeyTF, blRedayTF, blCaptureTF, blMacroStatus, blChange As Boolean
    Public strIOPath, strIOFile, strSavePath As String
    Public intMacroType, intTestNumber, intTestNumberTotal, intAddNumber, intCheckNumber, intFileNumber As Integer
    'Public intLocation(,) As Integer = {}

    Public Const fileSize As Integer = 23000
    Public Const filterName As String = "M9 Macro 설정 파일 (*.m9m)|*.m9m"

    '폴더 생성 함수
    Public Sub CreateSettingFolder(ByVal folderPath As String)
        '폴더가 없다면 폴더 생성
        If Not System.IO.Directory.Exists(folderPath) Then
            System.IO.Directory.CreateDirectory(folderPath)
        End If
    End Sub

    '*.ini 파일 형식 읽기/쓰기 함수
    Public Function INIRead(ByVal Session As String, ByVal KeyValue As String, ByVal INIFILE As String) As String
        'm9m 값 읽기 
        Dim str As String
        Dim ReturnValue As Long

        str = Space(255)
        ReturnValue = GetPrivateProfileString(Session, KeyValue, "", str, 255, INIFILE)
        str = Trim(str)
        str = str.Substring(0, Len(str) - 1)
        INIRead = str
    End Function
    Public Sub INIWrite(ByVal Session As String, ByVal KeyValue As String, ByVal DataValue As String, ByVal INIFILE As String)
        'm9m 값 쓰기
        Dim ReturnValue As Long

        ReturnValue = WritePrivateProfileString(Session, KeyValue, DataValue, INIFILE)
    End Sub

    '*.m9m 저장 함수
    Public Sub SettingFileSave(ByVal a1 As String, ByVal b1 As Integer, ByVal b2 As Integer, ByVal b3 As Integer, ByVal b4 As Integer, ByVal b5 As String, ByVal b6 As Integer, ByVal b7 As Boolean, ByVal c1 As Integer, ByVal c2 As Integer, ByVal c3 As Double, ByVal c4 As Double, ByVal d1 As String, ByVal e1 As Boolean, ByVal e2 As Boolean, ByVal e3 As Boolean, ByVal e4 As Boolean)
        '모델명
        INIWrite("Model", "Name", a1, strIOPath)

        '기본 설정
        INIWrite("Default Setting", "Time", b1, strIOPath)
        INIWrite("Default Setting", "Start Direction", b2, strIOPath)
        INIWrite("Default Setting", "Test Count", b3, strIOPath)
        INIWrite("Default Setting", "Test Number", b4, strIOPath)
        INIWrite("Default Setting", "Folder Name", b5, strIOPath)
        INIWrite("Default Setting", "Numeric Count", b6, strIOPath)
        INIWrite("Default Setting", "Auto Add", b7, strIOPath)

        '이동 설정
        INIWrite("Move Setting", "Column Count", c1, strIOPath)
        INIWrite("Move Setting", "Row Count", c2, strIOPath)
        INIWrite("Move Setting", "X Position", c3, strIOPath)
        INIWrite("Move Setting", "Y Position", c4, strIOPath)

        '경로 설정
        INIWrite("Path Setting", "Save Folder", d1, strIOPath)

        '고급 설정
        INIWrite("High Setting", "Status Bar", e1, strIOPath)
        INIWrite("High Setting", "Right End", e2, strIOPath)
        INIWrite("High Setting", "Target End", e3, strIOPath)
        INIWrite("High Setting", "Position Update", e4, strIOPath)
    End Sub

    '*.m9m 불러오기 함수
    Public Sub SettingFileLoad()
        If strIOPath <> "" Then
            '* 비활성화
            blChange = False

            '검사 수량의 논리 오류 방지
            mainForm.NmrDefaultTestCount.Maximum = 400

            '모델명
            mainForm.lbModelName.Text = INIRead("Model", "Name", strIOPath)

            '기본 설정
            mainForm.NmrDefaultTime.Value = INIRead("Default Setting", "Time", strIOPath)
            mainForm.cbbDefaultFirstDirection.SelectedIndex = INIRead("Default Setting", "Start Direction", strIOPath)
            mainForm.NmrDefaultTestCount.Value = INIRead("Default Setting", "Test Count", strIOPath)
            mainForm.NmrDefaultTestNumber.Value = INIRead("Default Setting", "Test Number", strIOPath)
            mainForm.txtDefaultFolderName.Text = INIRead("Default Setting", "Folder Name", strIOPath)
            mainForm.NmrDefaultNumericCount.Value = INIRead("Default Setting", "Numeric Count", strIOPath)
            mainForm.chbDefaultAutoAdd.Checked = INIRead("Default Setting", "Auto Add", strIOPath)

            '이동 설정
            mainForm.NmrMoveColumnCount.Value = INIRead("Move Setting", "Column Count", strIOPath)
            mainForm.NmrMoveRowCount.Value = INIRead("Move Setting", "Row Count", strIOPath)
            mainForm.NmrMoveXPosition.Text = INIRead("Move Setting", "X Position", strIOPath)
            mainForm.NmrMoveYPosition.Text = INIRead("Move Setting", "Y Position", strIOPath)

            '경로 설정
            mainForm.txtPathSaveFolder.Text = INIRead("Path Setting", "Save Folder", strIOPath)

            '고급 설정
            mainForm.chbPremiumStatusBar.Checked = INIRead("High Setting", "Status Bar", strIOPath)
            mainForm.chbPremiumRightEnd.Checked = INIRead("High Setting", "Right End", strIOPath)
            mainForm.chbPremiumTargetEnd.Checked = INIRead("High Setting", "Target End", strIOPath)
            mainForm.chbPremiumPositionUpdate.Checked = INIRead("High Setting", "Position Update", strIOPath)

            mainForm.NmrDefaultTestCount.Maximum = mainForm.NmrMoveColumnCount.Value * mainForm.NmrMoveRowCount.Value

            '* 활성화
            blChange = True
        End If
    End Sub

    '매크로 시작
    Public Sub MacroStart()
        With mainForm
            .btnMacroReady.Enabled = False
            blMacroStatus = True
            intTestNumber = 1
            intCheckNumber = 0

            '검사 수량
            If .NmrDefaultTestNumber.Value = 1 Then
                intTestNumberTotal = .NmrDefaultTestCount.Value
            Else
                Select Case .cbbDefaultFirstDirection.SelectedIndex
                    Case 0, 1
                        If (.NmrDefaultTestCount.Value - 1) Mod (.NmrMoveColumnCount.Value * .NmrDefaultTestNumber.Value) >= .NmrMoveColumnCount.Value - 1 Then
                            intTestNumberTotal = (((.NmrDefaultTestCount.Value - 1) \ (.NmrMoveColumnCount.Value * .NmrDefaultTestNumber.Value)) + 1) * .NmrMoveColumnCount.Value
                        Else
                            intTestNumberTotal = (((.NmrDefaultTestCount.Value) \ (.NmrMoveColumnCount.Value * .NmrDefaultTestNumber.Value)) * .NmrMoveColumnCount.Value) + ((.NmrDefaultTestCount.Value) Mod (.NmrMoveColumnCount.Value * .NmrDefaultTestNumber.Value))
                        End If
                    Case 2, 3
                        If (.NmrDefaultTestCount.Value - 1) Mod (.NmrMoveRowCount.Value * .NmrDefaultTestNumber.Value) >= .NmrMoveRowCount.Value - 1 Then
                            intTestNumberTotal = (((.NmrDefaultTestCount.Value - 1) \ (.NmrMoveRowCount.Value * .NmrDefaultTestNumber.Value)) + 1) * .NmrMoveRowCount.Value
                        Else
                            intTestNumberTotal = (((.NmrDefaultTestCount.Value) \ (.NmrMoveRowCount.Value * .NmrDefaultTestNumber.Value)) * .NmrMoveRowCount.Value) + ((.NmrDefaultTestCount.Value) Mod (.NmrMoveRowCount.Value * .NmrDefaultTestNumber.Value))
                        End If
                End Select
            End If

            '사진 번호 설정
            Select Case .cbbDefaultFirstDirection.SelectedIndex
                Case 0, 1
                    '가로 수의 따라 시작 값이 설정
                    intAddNumber = mainForm.NmrMoveColumnCount.Value - 1
                Case 2, 3
                    '가로 수의 따라 시작 값이 설정
                    intAddNumber = mainForm.NmrMoveRowCount.Value - 1
            End Select

            .Timer1.Interval = 10
            .Timer1.Start()

            If .chbPremiumStatusBar.Checked Then
                'viewForm.Show()
            End If
        End With

        Console.Beep(850, 250)
    End Sub

    '매크로 일시정지
    Public Sub MacroPause()
        With mainForm
            If .Timer1.Enabled Then
                .Timer1.Enabled = False
            Else
                .Timer1.Interval = 100
                .Timer1.Enabled = True
            End If
        End With
    End Sub

    '매크로 정지
    Public Sub MacroStop()
        With mainForm
            .btnMacroReady.Text = "준비 X"
            .btnMacroReady.Enabled = True
            blMacroStatus = False
            blRedayTF = False

            .Timer1.Stop()

            If .chbPremiumRightEnd.Checked Or .chbPremiumTargetEnd.Checked Then
                Call MacroLoad()
            End If
        End With

        Console.Beep(850, 250)
    End Sub

    '매크로 로드
    Public Sub MacroLoad()
        Cursor.Position = New Point(1675, 1035)
        mouse_event(&H2, 0, 0, 0, 0)
        Threading.Thread.Sleep(100)
        mouse_event(&H4, 0, 0, 0, 0)

        Cursor.Position = New Point(1553, 230)
        mouse_event(&H2, 0, 0, 0, 0)
        Threading.Thread.Sleep(100)
        mouse_event(&H4, 0, 0, 0, 0)

        Cursor.Position = New Point(1571, 1038)
        mouse_event(&H2, 0, 0, 0, 0)
        Threading.Thread.Sleep(100)
        mouse_event(&H4, 0, 0, 0, 0)

        Cursor.Position = New Point(Screen.PrimaryScreen.Bounds.Width / 2, Screen.PrimaryScreen.Bounds.Height / 2)
    End Sub

    '매크로 이동 클릭
    Public Sub MacroClick(ByVal type As Integer)
        Select Case type
            Case 0
                '오른쪽 이동
                Cursor.Position = New Point(1556, 953)
                'Cursor.Position = New Point(380, 450)
            Case 1
                '왼쪽 이동
                Cursor.Position = New Point(1619, 961)
                'Cursor.Position = New Point(450, 450)
            Case 2
                '아래 이동
                Cursor.Position = New Point(1585, 920)
                'Cursor.Position = New Point(410, 410)
            Case 3
                '위 이동
                Cursor.Position = New Point(1586, 993)
                'Cursor.Position = New Point(410, 485)
        End Select

        '왼쪽 클릭 (다운, 100ms, 업)
        mouse_event(&H2, 0, 0, 0, 0)
        Threading.Thread.Sleep(100)
        mouse_event(&H4, 0, 0, 0, 0)
    End Sub

    '매크로 이동 방향
    Public Sub MacroDirection(ByVal direction As Integer)
        Select Case direction
            Case 0
                '→
                If (intTestNumber Mod mainForm.NmrMoveColumnCount.Value) = 0 Then
                    '아래 이동
                    Call MacroClick(2)
                ElseIf ((intTestNumber \ mainForm.NmrMoveColumnCount.Value) Mod 2) = 0 Then
                    '오른쪽 이동
                    Call MacroClick(0)
                ElseIf ((intTestNumber \ mainForm.NmrMoveColumnCount.Value) Mod 2) = 1 Then
                    '왼쪽 이동
                    Call MacroClick(1)
                End If
            Case 1
                '←
                If (intTestNumber Mod mainForm.NmrMoveColumnCount.Value) = 0 Then
                    '아래 이동
                    Call MacroClick(2)
                ElseIf ((intTestNumber \ mainForm.NmrMoveColumnCount.Value) Mod 2) = 0 Then
                    '왼쪽 이동
                    Call MacroClick(1)
                ElseIf ((intTestNumber \ mainForm.NmrMoveColumnCount.Value) Mod 2) = 1 Then
                    '오른쪽 이동
                    Call MacroClick(0)
                End If
            Case 2
                '↓
                If (intTestNumber Mod mainForm.NmrMoveRowCount.Value) = 0 Then
                    '오른쪽 이동
                    Call MacroClick(0)
                ElseIf ((intTestNumber \ mainForm.NmrMoveRowCount.Value) Mod 2) = 0 Then
                    '아래 이동
                    Call MacroClick(2)
                ElseIf ((intTestNumber \ mainForm.NmrMoveRowCount.Value) Mod 2) = 1 Then
                    '위 이동
                    Call MacroClick(3)
                End If
            Case 3
                '↑
                If (intTestNumber Mod mainForm.NmrMoveRowCount.Value) = 0 Then
                    '오른쪽 이동
                    Call MacroClick(0)
                ElseIf ((intTestNumber \ mainForm.NmrMoveRowCount.Value) Mod 2) = 0 Then
                    '위 이동
                    Call MacroClick(3)
                ElseIf ((intTestNumber \ mainForm.NmrMoveRowCount.Value) Mod 2) = 1 Then
                    '아래 이동
                    Call MacroClick(2)
                End If
        End Select
    End Sub

    '화면 캡쳐 함수
    Public Sub CaptureScreen(ByVal x As Integer, ByVal y As Integer, ByVal w As Integer, ByVal h As Integer, ByVal filename As String)
        Dim myBitmap As New Bitmap(w, h)
        Dim myGraphic As Graphics = Graphics.FromImage(myBitmap)

        myGraphic.CopyFromScreen(New Point(x, y), New Point(0, 0), myBitmap.Size)
        Call CreateSettingFolder(IO.Path.GetDirectoryName(filename))
        myBitmap.Save(filename, Imaging.ImageFormat.Jpeg)
        myGraphic.Dispose()
        myBitmap.Dispose()

        strSavePath = filename
    End Sub

    '* 표시 추가 함수
    Public Sub ModelAddStar()
        mainForm.TabControl2.SelectedTab.Text = strIOFile & "*"
    End Sub

    '메세지 함수
    Public Sub MessageShow(ByVal msg As String)
        MessageBox.Show(msg, "오류", MessageBoxButtons.OK, MessageBoxIcon.Warning)
    End Sub

    '파일 검사 함수
    Public Function FileCheck(ByVal path As String) As Boolean
        Dim info As New IO.FileInfo(path)

        If info.Length > fileSize Then
            FileCheck = True
        Else
            FileCheck = False
        End If
    End Function

    '상한 값 더하기 함수
    'Public Function NumericAdd(ByVal type As Integer, ByVal value As Integer, ByVal add As Integer, ByVal condition As Integer)
    '    If condition > value Then
    '        If type = 0 Then
    '            Call ModelAddStar()
    '        End If
    '        NumericAdd = value + add
    '    Else
    '        NumericAdd = value
    '    End If
    'End Function

    '하한 값 더하기 함수
    'Public Function NumericDel(ByVal type As Integer, ByVal value As Integer, ByVal del As Integer, ByVal condition As Integer)
    '    If condition < value Then
    '        If type = 0 Then
    '            Call ModelAddStar()
    '        End If
    '        NumericDel = value + del
    '    Else
    '        NumericDel = value
    '    End If
    'End Function
End Module
