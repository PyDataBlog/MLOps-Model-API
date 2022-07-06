Imports System.Data

Partial Class LinkContador
    Inherits System.Web.UI.Page

    Dim func As New clsAcessos

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        If Not IsPostBack Then

            Call sCarregarLinks()

        End If

    End Sub

    Public Function fRetornaCaminhoImagem(ByVal wImagem As String) As String

        Dim wCorpo As String = ""
        Dim ws As New wsCash.wsCash

        wCorpo = "<IMG src='---caminhoimagem---' width=110  />"

        If wImagem <> "" Then
            wCorpo = Replace(wCorpo, "---caminhoimagem---", ws.fRetornaURLImagem(ConfigurationManager.AppSettings("URLIMAGENS"), "IMAGEMDESTAQUE", wImagem))

        Else
            wCorpo = ""
        End If

        Return wCorpo

    End Function

    Sub sCarregarLinks()

        Dim wSQL As String
        Dim ds As DataSet
        Dim func As New clsAcessos
        Dim wPortal As New wsCash.wPortal
        Dim ws As New wsCash.wsCash
        Dim wQuantAcessos As Integer = 0
        Dim wResposta As String
        Dim wURL As String

        Dim wCodigoLink As String
        wCodigoLink = func.fRetornaParametroSeguro(Request.Params("cLink"))

        wSQL = " Select * "
        wSQL += " from tbLinks"
        wSQL += " where cLink = " & wCodigoLink

        ws = func.fRetornaWS()
        wPortal = ws.fCarregarPortal(func.fPortal())
        ds = ws.fRetornaDataSet(wSQL)

        wQuantAcessos += ds.Tables(0).Rows(0).Item("qAcessos") + 1

        wSQL = "Update tbLinks set qAcessos = " & wQuantAcessos & " where cLink = " & wCodigoLink

        wResposta = ws.fAtualizaQtdAcessosLinks(wCodigoLink, wQuantAcessos)

        wURL = ds.Tables(0).Rows(0).Item("tURL")
        wURL = wURL.Replace("http://", "")
        wURL = "http://" & wURL
        Response.Redirect(wURL)

    End Sub

End Class