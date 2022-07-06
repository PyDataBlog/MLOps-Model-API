Imports System.data

Partial Class ucpublicidade_main
    Inherits System.Web.UI.UserControl

    Protected Class clsDestaqueBanner

        Private arquivo As String
        Private extensao As Integer
        Private link As String
        Private detaquebanner As String

        Public Property tArquivo() As String
            Get
                Return arquivo
            End Get
            Set(ByVal value As String)
                arquivo = value
            End Set
        End Property

        Public Property cExtensao() As Integer
            Get
                Return extensao
            End Get
            Set(ByVal value As Integer)
                extensao = value
            End Set
        End Property

        Public Property tLink() As String
            Get
                Return link
            End Get
            Set(ByVal value As String)
                link = value
            End Set
        End Property

        Public Property tDestaqueBanner() As String
            Get
                Return detaquebanner
            End Get
            Set(ByVal value As String)
                detaquebanner = value
            End Set
        End Property

    End Class

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        If Not IsPostBack Then

            Session("cPonteiroDestaque") = 0
            Call sBindDestaqueBanner(fCarregarDestaqueBanner())

        End If

    End Sub

    Function fCarregarDestaqueBanner() As ArrayList

        Dim func As New clsAcessos
        Dim ds As New DataSet
        Dim wPortal As New wsCash.wPortal
        Dim wContador As Integer = 0
        Dim aDestaques As New ArrayList
        Dim aDestaqueRetorno As New ArrayList
        Dim wCount As Integer = 0
        Dim ws As New wsCash.wsCash

        If Session("aDestaquesBanner") Is Nothing Then

            'Carregar Data Set de Detaques Banners
            ws = func.fRetornaWS()

            ds = ws.fRetornaDestaqueBannerCadastrado(func.fPortal())

            If ds.Tables(0).Rows.Count > 0 Then

                For I As Integer = 0 To ds.Tables(0).Rows.Count - 1

                    Dim clsDestaqueBanner As New clsDestaqueBanner

                    With clsDestaqueBanner
                        .tArquivo = ds.Tables(0).Rows(I).Item("tArquivo")
                        .cExtensao = ds.Tables(0).Rows(I).Item("cTipoBanner")
                        .tLink = IIf(ds.Tables(0).Rows(I).Item("tUrl") Is DBNull.Value, "", ds.Tables(0).Rows(I).Item("tUrl"))
                        .tDestaqueBanner = fRetornaDestaqueBanner(.tArquivo, .cExtensao, .tLink)
                        aDestaques.Add(clsDestaqueBanner)
                    End With


                Next

                Session("aDestaquesBanner") = aDestaques

            End If

        Else

            aDestaques = Session("aDestaquesBanner")

        End If

        If aDestaques.Count > 0 Then

            aDestaqueRetorno.Add(aDestaques(Session("cPonteiroDestaque")))
            Session("cPonteiroDestaque") += 1

        End If

        If Session("cPonteiroDestaque") = aDestaques.Count Then
            Session("cPonteiroDestaque") = 0
        End If

        Return aDestaqueRetorno

    End Function

    Sub sBindDestaqueBanner(ByVal aDestaques As ArrayList)

        With dlPublicidades
            .DataSource = aDestaques
            .DataBind()
        End With

    End Sub

    Protected Sub ratDestaqueBanner_Tick(ByVal sender As Object, ByVal e As Telerik.WebControls.TickEventArgs) Handles ratDestaqueBanner.Tick

        Call sBindDestaqueBanner(fCarregarDestaqueBanner())

    End Sub

    Function fRetornaDestaqueBanner(ByVal wArquivo As String, ByVal wTipoBAnner As Integer, ByVal wLink As String) As String

        Dim ws As New wsCash.wsCash
        Dim wTagImg As String

        ws.Url = ConfigurationManager.AppSettings("wsURL")

        Dim wURL As String

        wURL = ws.fRetornaURLImagem(ConfigurationManager.AppSettings("URLIMAGENS"), "BANNER", wArquivo)

        If wTipoBAnner = 1 Then

            wTagImg = " <object id=" & Chr(34) & "FlashID" & Chr(34) & " classid=" & Chr(34) & "clsid:D27CDB6E-AE6D-11cf-96B8-444553540000" & Chr(34) & " width=" & Chr(34) & "438" & Chr(34) & " height=" & Chr(34) & "143" & Chr(34) & ">"
            wTagImg += " <param name=" & Chr(34) & "movie" & Chr(34) & " value=" & Chr(34) & wURL & Chr(34) & "/>"
            wTagImg += " <param name=" & Chr(34) & "quality" & Chr(34) & " value=" & Chr(34) & "high" & Chr(34) & "/>"
            wTagImg += "  <param name=" & Chr(34) & "wmode" & Chr(34) & " value=" & Chr(34) & "opaque" & Chr(34) & "/>"
            wTagImg += " <param name=" & Chr(34) & "swfversion" & Chr(34) & " value=" & Chr(34) & "9.0.45.0" & Chr(34) & "/>"
            wTagImg += " <param name=" & Chr(34) & "expressinstall" & Chr(34) & " value=" & Chr(34) & "Scripts/expressInstall.swf" & Chr(34) & " />"
            wTagImg += " </object>"

        Else

            If wLink <> "" Then

                wLink.Replace("http://", "")

                wTagImg = "<a href=" & Chr(34) & "http://" & wLink.Trim & Chr(34) & " target=" & Chr(34) & "_blank " & Chr(34) & "><img src=" & Chr(34) & wURL & Chr(34) & " style=" & Chr(34) & "width: 275px; " & Chr(34) & " /></a>"

            Else

                wTagImg = "<img src=" & Chr(34) & wURL & Chr(34) & " style=" & Chr(34) & "width: 275px;" & Chr(34) & " />"

            End If

        End If

        Return wTagImg

    End Function

End Class
