Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary

Module Core

    Public config As Configuracao

    Public sConn As String = "Data Source=C:\Users\CISISTEMAS04\Dropbox\Junior\Projetos\DocSmarter\DocSmarter\DocSmarter\DocSmarter.db"

    Public drive As String = Path.GetPathRoot(Environment.GetFolderPath(Environment.SpecialFolder.System))
    Public userfolder As String = drive & "Users\" & Environment.UserName
    Public slimfolder As String = userfolder & "\Slimdesk"
    Public Folders As String = slimfolder & "\Folders"
    Public Desktop As String = My.Computer.FileSystem.SpecialDirectories.Desktop
    'Public CleanFolder As String = Desktop & "\Cleanup"

    Sub Serialize(path As String, config As Configuracao)
        Dim obj As Byte() = ObjToByteArray(config)
        IO.File.WriteAllBytes(path, obj)
    End Sub

    Function Deserialize(path As String) As Configuracao
        Dim obj As Object = IO.File.ReadAllBytes(path)
        Return CType(ByteArrayToObj(obj), Configuracao)
    End Function

    Function ObjToByteArray(obj As Object) As Byte()
        Try
            If IsNothing(obj) Then Return Nothing
            Dim bf As New BinaryFormatter
            Dim ms As New MemoryStream
            bf.Serialize(ms, obj)
            Return ms.ToArray

        Catch ex As Exception
            MsgBox(ex.Message)
        End Try

    End Function

    Function ByteArrayToObj(obj As Byte()) As Object
        Dim ms As New MemoryStream
        Dim bf As New BinaryFormatter
        ms.Write(obj, 0, obj.Length)
        ms.Seek(0, SeekOrigin.Begin)
        Dim o As Object = bf.Deserialize(ms)
        Return o
    End Function


End Module
