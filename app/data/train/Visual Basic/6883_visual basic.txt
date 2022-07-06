
''' Tail.Net
''' Implements the Linux Tail utility on the .net platform


Imports System
Imports System.IO
Imports System.Threading
Imports Microsoft.VisualBasic

Module Tail

   dim fsw As FileSystemWatcher
   dim lineNum As Long = 0
   dim seek As Long = 0

   Sub Main()

      seek = 0
      lineNum = 0
      Dim fi As FileInfo

      If Environment.GetCommandLineArgs.Length > 1 Then
         fi = New FileInfo(Environment.GetCommandLineArgs(1).ToString)
      Else
         Console.WriteLine("Usage: Tail.exe Path")
         End
      End If


      fsw = New FileSystemWatcher(fi.DirectoryName, fi.Name)

      AddHandler fsw.Changed, AddressOf fsw_Changed
      fsw.EnableRaisingEvents = True
      Dim fs As FileStream = New FileStream(fi.FullName, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
      Dim sr As StreamReader = New StreamReader(fs)
      Dim data As String = sr.ReadToEnd
      Console.Write(data & vbCrLf)
      seek = data.Length
      sr.Close()
      fs.Close()
      Do While True
         Thread.Sleep(500)
      Loop
   End Sub

   Sub fsw_Changed(ByVal sender As Object, ByVal e As FileSystemEventArgs)
      Try
         Dim fs As FileStream = New FileStream(e.FullPath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
         Dim sr As StreamReader = New StreamReader(fs)
         sr.BaseStream.Seek(seek, SeekOrigin.Begin)
         Dim data As String = sr.ReadToEnd
         seek += data.Length
         Console.Write(data)         '& vbCrLf)
         sr.Close()
         fs.Close()
      Catch ex As Exception
         Console.WriteLine(ex.Message)
      End Try
   End Sub



End Module
