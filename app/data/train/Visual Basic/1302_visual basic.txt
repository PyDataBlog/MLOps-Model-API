Imports System.Text.RegularExpressions

Public Class Syncer
    Private _SourceFolder As String = String.Empty
    Private _TargetFolder As String = String.Empty

    Private IsAnalyzed As Boolean = False
    Private SourceRegEx As Regex
    Private TargetRegEx As Regex
    Private DeletedEmptyDirectoryCount As Integer = 0
    Private FileIndex As Integer = 0
    Private FileCount As Integer = 0
    Private CancelSync As Boolean = False

    Property SourceFolder As String
        Get
            Return _SourceFolder
        End Get
        Set(value As String)
            _SourceFolder = value
            IsAnalyzed = False
        End Set
    End Property

    Property TargetFolder As String
        Get
            Return _TargetFolder
        End Get
        Set(value As String)
            _TargetFolder = value
            IsAnalyzed = False
        End Set
    End Property

    Property NewFiles As New List(Of String)
    Property ChangedFiles As New List(Of String)
    Property DeletedFiles As New List(Of String)
    Property SameFiles As New List(Of String)

    Property Output As PS.ExeBase.Output.IOutputWriter = New PS.ExeBase.Output.DummyOutputWriter
    Property AnalysisUpdateInterval As Integer = 100
    Property AnalysisFolderCount As Integer = 0
    Property AnalysisFolderIndex As Integer = 0

    ReadOnly Property HasChanges
        Get
            Return NewFiles.Count > 0 Or ChangedFiles.Count > 0 Or DeletedFiles.Count > 0
        End Get
    End Property

    Public Sub Analyze()
        If IsAnalyzed = False Then
            Validate()

            NewFiles.Clear()
            ChangedFiles.Clear()
            DeletedFiles.Clear()
            SameFiles.Clear()

            SourceRegEx = New Regex(Regex.Escape(SourceFolder))
            TargetRegEx = New Regex(Regex.Escape(TargetFolder))

            AnalysisFolderCount = 0
            AnalysisFolderIndex = 0

            'get a count of the folders to analyze
            CountFolders(SourceFolder)

            CatalogFilesForChange(SourceFolder)
            CatalogFilesForDelete(TargetFolder)

            IsAnalyzed = True
        End If

    End Sub

    Public Sub Synchronize()

        CancelSync = False

        Analyze()

        FileIndex = 1
        FileCount = NewFiles.Count + ChangedFiles.Count + DeletedFiles.Count

        If FileCount = 0 Then
            Output.WriteLine("There are no changes to synchronize")
            Exit Sub
        End If

        For Each f In NewFiles
            'Threading.Thread.Sleep(2000)
            If CancelSync Then
                Output.WriteLine("Synchronization cancelled")
                Exit Sub
            End If
            CopyFileToTarget(f)
        Next
        For Each f In ChangedFiles
            'Threading.Thread.Sleep(2000)
            If CancelSync Then
                Output.WriteLine("Synchronization cancelled")
                Exit Sub
            End If
            CopyFileToTarget(f)
        Next
        For Each f In DeletedFiles
            'Threading.Thread.Sleep(2000)
            If CancelSync Then
                Output.WriteLine("Synchronization cancelled")
                Exit Sub
            End If
            DeleteFileFromTarget(f)
        Next

        DeleteEmptyDirectories()

        Output.WriteLine("Synchronization is complete")
    End Sub

    Public Sub Cancel()
        CancelSync = True
    End Sub

    Private Sub CatalogFilesForChange(Folder As String)

        AnalysisFolderIndex += 1

        If AnalysisFolderIndex = 1 Then
            Output.WriteFormatLine("Analyzing folder 1 of {0}", AnalysisFolderCount)
        End If

        If AnalysisFolderIndex Mod AnalysisUpdateInterval = 0 Then
            Output.WriteFormatLine("Analyzing folder {0} of {1}", AnalysisFolderIndex, AnalysisFolderCount)
        End If

        For Each f In IO.Directory.GetFiles(Folder)

            Dim TargetFile As String = SourceRegEx.Replace(f, TargetFolder, 1)

            If IO.File.Exists(TargetFile) Then
                'exists in both places.  check size and modified date
                If AreFilesSame(f, TargetFile) Then
                    SameFiles.Add(f)
                Else
                    ChangedFiles.Add(f)
                End If
            Else
                NewFiles.Add(f)
            End If
        Next

        For Each f In IO.Directory.GetDirectories(Folder)
            CatalogFilesForChange(f)
        Next
    End Sub

    Private Sub CatalogFilesForDelete(Folder As String)
        For Each f In IO.Directory.GetFiles(Folder)
            Dim filepart As String = f.Replace(TargetFolder, "")

            Dim SourceFile As String = TargetRegEx.Replace(f, SourceFolder, 1)

            If IO.File.Exists(SourceFile) = False Then
                DeletedFiles.Add(f)
            End If
        Next

        For Each f In IO.Directory.GetDirectories(Folder)
            CatalogFilesForDelete(f)
        Next
    End Sub

    Private Sub CopyFileToTarget(SourceFile)
        Dim Directory As String = New IO.FileInfo(SourceFile).DirectoryName
        Directory = SourceRegEx.Replace(Directory, TargetFolder, 1)
        If IO.Directory.Exists(Directory) = False Then
            Output.WriteFormatLine("Creating directory: {0}", Directory)
            IO.Directory.CreateDirectory(Directory)
        End If
        Dim TargetFile As String = SourceRegEx.Replace(SourceFile, TargetFolder, 1)
        Output.WriteFormatLine("Copying file: {0} - ({1} of {2})", SourceFile, FileIndex, FileCount)
        Try
            IO.File.Delete(TargetFile)
            IO.File.Copy(SourceFile, TargetFile, True)
        Catch ex As Exception
            Output.WriteFormatLine("ERROR OCCURED:{0}{1}", vbCrLf, ex.ToString)
        End Try

        FileIndex += 1
    End Sub

    Private Sub DeleteFileFromTarget(TargetFile)
        Output.WriteFormatLine("Deleting file: {0} - ({1} of {2})", TargetFile, FileIndex, FileCount)
        IO.File.Delete(TargetFile)
        FileIndex += 1
    End Sub

    Private Sub DeleteEmptyDirectories()

        Dim StopDelete As Boolean = False
        While StopDelete = False
            DeletedEmptyDirectoryCount = 0
            DeleteEmptyDirectories2(TargetFolder)
            If DeletedEmptyDirectoryCount = 0 Then
                StopDelete = True
            End If
            If CancelSync Then
                Output.WriteLine("Synchronization cancelled")
                Exit Sub
            End If
        End While

    End Sub

    Private Sub DeleteEmptyDirectories2(Folder As String)
        If CancelSync Then
            Output.WriteLine("Synchronization cancelled")
            Exit Sub
        End If
        If IO.Directory.GetFiles(Folder).Count = 0 AndAlso IO.Directory.GetDirectories(Folder).Count = 0 Then
            Output.WriteFormatLine("Deleting empty folder: {0}", Folder)
            IO.Directory.Delete(Folder)
            DeletedEmptyDirectoryCount += 1
        Else
            For Each f In IO.Directory.GetDirectories(Folder)
                DeleteEmptyDirectories2(f)
            Next
        End If
    End Sub

    Private Sub Validate()
        If IO.Directory.Exists(SourceFolder) = False Then
            Throw New InvalidOperationException("A valid SourceFolder is required")
        End If
        If IO.Directory.Exists(TargetFolder) = False Then
            Throw New InvalidOperationException("A valid TargetFolder is required")
        End If
    End Sub

    Private Function AreFilesSame(File1 As String, File2 As String) As Boolean
        Dim Ret As Boolean = False

        'If File1.Contains("PictureUploader.xml") Then
        '    Debug.Print("d")
        'End If

        Dim f1 As New IO.FileInfo(File1)
        Dim f2 As New IO.FileInfo(File2)

        If f1.Length <> f2.Length OrElse (f1.LastWriteTime - f2.LastWriteTime).TotalMinutes > 1 Then
            Ret = False
        Else
            Ret = True
        End If

        Return Ret
    End Function

    Private Sub CountFolders(Folder)
        For Each f In IO.Directory.GetDirectories(Folder)
            AnalysisFolderCount += 1
            CountFolders(f)
        Next
    End Sub
End Class
