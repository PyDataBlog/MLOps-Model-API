
Partial Class ucRodape
    Inherits System.Web.UI.UserControl

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        Call sCarregaRodape()

    End Sub

    Public Sub sCarregaRodape()

        Dim wPortal As New wsCash.wPortal
        Dim func As New clsAcessos

        wPortal = func.fCarregarPortal()

        Dim wPastaImgEditor As String = ""

        If ConfigurationManager.AppSettings("URLIMAGENS").IndexOf("http") > 0 Then
            wPastaImgEditor = ConfigurationManager.AppSettings("URLIMAGENS") & "Grupos/"
        Else
            wPastaImgEditor = "http://" & ConfigurationManager.AppSettings("URLIMAGENS") & "Grupos/"
        End If

        With wPortal
            lblRodape.Text = .tRodape.Replace(Chr(10), "<br/>").Replace("src=""Grupos", "src=""" & wPastaImgEditor)
        End With


    End Sub

End Class
