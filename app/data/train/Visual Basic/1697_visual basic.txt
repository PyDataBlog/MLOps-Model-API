Imports System.ComponentModel
Imports System.Net
Imports System.Text

Public Class DownloadManager

    Public WithEvents DlWebClient As WebClient
    Public _
        CompletedListener As DownloadStatusListener.OnDownloadCompletedDelegate,
        ProgressChangeListener As DownloadStatusListener.OnProgressChangedDelegate,
        StringCompletedListener As DownloadStatusListener.OnDownloadStringCompletedDelegate

    Private CurrentBytesToReceive As Long = 0
    Private CurrentBytesReceived  As Long = 0
    
    Private UrlToDownload, FileNameToDownload As String

    Public Sub New()
        Console.WriteLine()
        Console.WriteLine("Initializing a new download manager...")
        DlWebClient = New WebClient()
        DlWebClient.Encoding = Encoding.UTF8
        Console.WriteLine("Download manager initialized!")
    End Sub

    Private Sub DlWebClient_DownloadFileCompleted(sender As Object, e As AsyncCompletedEventArgs) Handles DlWebClient.DownloadFileCompleted
        Console.WriteLine("Download completed.")
        If Not IsNothing(CompletedListener) Then CompletedListener.Invoke(IsNothing(e.Error), e.Error, e.Cancelled)
        If Not IsNothing(ProgressChangeListener) Then ProgressChangeListener.Invoke(100, CurrentBytesReceived, CurrentBytesToReceive)
    End Sub

    Private Sub DlWebClient_DownloadProgressChanged(sender As Object, e As DownloadProgressChangedEventArgs) Handles DlWebClient.DownloadProgressChanged
        CurrentBytesReceived  = e.BytesReceived
        CurrentBytesToReceive = e.TotalBytesToReceive
        If Not IsNothing(ProgressChangeListener) Then ProgressChangeListener.Invoke(e.ProgressPercentage, e.BytesReceived, e.TotalBytesToReceive)
    End Sub

    Private Sub DlWebClient_DownloadStringCompleted(sender As Object, e As DownloadStringCompletedEventArgs) Handles DlWebClient.DownloadStringCompleted
        Console.WriteLine("Download completed.")
        If Not IsNothing(CompletedListener) Then CompletedListener.Invoke(IsNothing(e.Error), e.Error, e.Cancelled)
        If Not IsNothing(StringCompletedListener) Then StringCompletedListener.Invoke(e.Result)
    End Sub

    Public Function DownloadFile(Url As String, Filename As String,
                                 Optional OnCompletedListener As DownloadStatusListener.OnDownloadCompletedDelegate = Nothing,
                                 Optional OnProgressChangeListener As DownloadStatusListener.OnProgressChangedDelegate = Nothing) As Boolean
        Console.WriteLine("Starting download of """ & Url & """ to """ & Filename & """")
        CompletedListener = OnCompletedListener
        OnProgressChangeListener = OnProgressChangeListener
        Try
            If Not IO.Directory.Exists(FileName.Substring(0, FileName.LastIndexOf("/"))) Then
                Console.WriteLine("Creating necessary directory """ & Filename.Substring(0, Filename.LastIndexOf("/")) & """")
                IO.Directory.CreateDirectory(FileName.Substring(0, FileName.LastIndexOf("/")))
            End If

            DlWebClient.DownloadFileAsync(New Uri(Url), FileName)
            Return True
        Catch ex As Exception
            Console.WriteLine("Exception occured while starting download: " & ex.Message & vbNewLine & _
                              ex.StackTrace)
            Return False
        End Try
    End Function

    Public Function DownloadString(Url As String, 
                                            OnReceiveListener As DownloadStatusListener.OnDownloadStringCompletedDelegate,
                                   Optional OnProgressChangeListener As DownloadStatusListener.OnProgressChangedDelegate = Nothing,
                                   Optional OnCompletedListener As DownloadStatusListener.OnDownloadCompletedDelegate = Nothing) As Boolean
        Console.WriteLine("Starting download of """ & Url & """")
        StringCompletedListener = OnReceiveListener
        ProgressChangeListener  = OnProgressChangeListener
        CompletedListener       = OnCompletedListener
        Try
            DlWebClient.DownloadStringAsync(New Uri(Url))
            Return True
        Catch ex As Exception
            Return False
        End Try
    End Function

    Private Sub SetOnDownloadCompletedListener(OnCompletedListener As DownloadStatusListener.OnDownloadCompletedDelegate)
        CompletedListener = OnCompletedListener
    End Sub

    Private Sub SetOnProgressChangeListener(OnProgressChangeListener As DownloadStatusListener.OnProgressChangedDelegate)
        ProgressChangeListener = OnProgressChangeListener
    End Sub

    Public Sub SetListeners(OnCompletedListener As DownloadStatusListener.OnDownloadCompletedDelegate,
                            OnProgressChangeListener As DownloadStatusListener.OnProgressChangedDelegate)
        SetOnDownloadCompletedListener  (OnCompletedListener)
        SetOnProgressChangeListener     (OnProgressChangeListener)
    End Sub

    Public Sub SetDownloadFile(Url As String, Filename As String)
        UrlToDownload = Url
        FileNameToDownload = Filename
    End Sub

    Public Sub StartDownload()
        If UrlToDownload.Equals("") Or FileNameToDownload.Equals("") Then Return
        DownloadFile(UrlToDownload, FileNameToDownload, CompletedListener, ProgressChangeListener)
        UrlToDownload       = ""
        FileNameToDownload  = ""
    End Sub

    Public Class DownloadStatusListener
        Public Delegate Sub OnDownloadCompletedDelegate(Success As Boolean, Ex As Exception, WasCancelled As Boolean)
        Public Delegate Sub OnProgressChangedDelegate(Progress As Integer, BytesReceived As Long, BytesToReceive As Long)
        Public Delegate Sub OnDownloadStringCompletedDelegate(ReceivedString As String)
    End Class

End Class
