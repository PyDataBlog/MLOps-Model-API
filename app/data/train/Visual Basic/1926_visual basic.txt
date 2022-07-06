Imports System
Imports System.IO
Imports System.Collections

Module Module1

    Sub Main()

        Console.Clear()

        Console.WriteLine("아무말 v2.00")
        Console.WriteLine("Build by null, at 20170622. Twitter @ThisIsMoneLTAN")
        Console.WriteLine(" ")
        Console.WriteLine("특정 명령어에 대한 자세한 내용이 필요하면 help를 입력하세요.")

        Do
            Dim txtxt As String
            Console.WriteLine("현재 타임스탬프는 " & Format(Now, "yyyy-MM-dd hh:mm:ss") & " 입니다.")
            Console.Write("아무말 입력:")
            txtxt = Console.ReadLine()
            Select Case txtxt
                Case "help"
                    Console.WriteLine(" ")
                    Console.WriteLine("lf       여러 줄로 메모를 작성합니다.")
                    Console.WriteLine("cls      화면이 청소됩니다.")
                    Console.WriteLine("open     데이터베이스 파일을 엽니다.")
                    Console.WriteLine("copy     데이터베이스가 자동 백업됩니다.")
                    Console.WriteLine("bye      프로그램이 종료됩니다.")
                    Console.WriteLine(" ")
                    Console.WriteLine("자세한 정보는 해당 프로그램에 대한 GitHub 레포지토리 (https://github.com/NewMoneL/AmuMal2)를 참조해주세요.")
                    Console.WriteLine(" ")

                Case "lf"
                    lf()

                Case "cls"
                    Console.Clear()

                Case "open"
                    If System.IO.File.Exists(System.IO.Directory.GetCurrentDirectory & "\amumal.txt") = True Then
                        Process.Start(System.IO.Directory.GetCurrentDirectory & "\amumal.txt")
                    Else
                        Console.WriteLine("데이터베이스가 없거나 아무 내용도 없습니다. 먼저 메모를 추가하세요.")
                    End If

                Case "copy"
                    Copy()

                Case "bye"
                    End

                Case Else
                    Write(txtxt)
            End Select
        Loop

    End Sub

    Sub Write(txt As String)

        Dim FileNum As Integer
        Dim FileName As String

        FileNum = FreeFile()
        FileName = System.IO.Directory.GetCurrentDirectory & "\amumal.txt"

        FileOpen(FileNum, FileName, OpenMode.Append)
        Print(FileNum, vbCrLf & txt)
        Print(FileNum, vbCrLf & Format(Now, "yyyy-MM-dd hh:mm:ss"))
        Print(FileNum, vbCrLf & "----------")
        FileClose(FileNum)

    End Sub

    Sub lf()

        Dim FileNum As Integer
        Dim FileName As String
        Dim txt2 As String
        Dim ExitMe As Boolean = False

        FileNum = FreeFile()
        FileName = System.IO.Directory.GetCurrentDirectory & "\amumal.txt"

        Console.WriteLine("여러 줄로 메모를 작성합니다. 끝내려면 end를 입력하세요.")

        Do While ExitMe = False
            txt2 = Console.ReadLine()
            If txt2 = "end" Then
                ExitMe = True
            Else
                FileOpen(FileNum, FileName, OpenMode.Append)
                Print(FileNum, vbCrLf & txt2)
                FileClose(FileNum)
            End If
        Loop

        FileOpen(FileNum, FileName, OpenMode.Append)
        Print(FileNum, vbCrLf & Format(Now, "yyyy-MM-dd hh:mm:ss"))
        Print(FileNum, vbCrLf & "----------")
        FileClose(FileNum)

    End Sub

    Sub Copy()
        Dim FileToCopy As String = System.IO.Directory.GetCurrentDirectory & "\amumal.txt"
        Dim NewCopy As String = System.IO.Directory.GetCurrentDirectory & "\amumal_" & System.Guid.NewGuid.ToString & ".txt"

        If System.IO.File.Exists(FileToCopy) = True Then
            System.IO.File.Copy(FileToCopy, NewCopy)
            Console.WriteLine("복사되었습니다.")
            Console.WriteLine("파일 이름은 " & NewCopy & " 입니다.")
        Else
            Console.WriteLine("데이터베이스가 없거나 아무 내용도 없습니다. 먼저 메모를 추가하세요.")
        End If
    End Sub

End Module
