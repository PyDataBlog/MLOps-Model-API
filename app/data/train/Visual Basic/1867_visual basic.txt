Imports CefSharp
Imports CefSharp.WinForms
Public Class frmBrowser

    Private WithEvents _browser As ChromiumWebBrowser

    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        _browser = New ChromiumWebBrowser("http://google.com")


        pnlBrowser.Controls.Add(_browser)
    End Sub

    Private Sub btnMSDN_Click(sender As Object, e As EventArgs) Handles btnMSDN.Click
        _browser.Load("https://msdn.com")
    End Sub

    Private Sub Form1_Disposed(sender As Object, e As EventArgs) Handles Me.Disposed
        _browser.Dispose()
        CefSharp.Cef.Shutdown()
    End Sub
    Private Sub _browser_LoadingStateChanged(sender As Object, e As LoadingStateChangedEventArgs) Handles _browser.LoadingStateChanged

        Dim xtn = Sub()
                      pnlActions.Enabled = Not e.IsLoading
                  End Sub
        If InvokeRequired Then
            Invoke(Sub()
                       xtn()
                   End Sub
                )
        Else
            xtn.Invoke
        End If

    End Sub

End Class
