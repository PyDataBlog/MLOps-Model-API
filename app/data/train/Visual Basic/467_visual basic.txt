Imports System.data

Partial Class ucdestaque_banner
    Inherits System.Web.UI.UserControl

    Protected Class clsHalfBanner

        Private arquivo As String
        Private extensao As Integer
        Private link As String
        Private halfbanner As String

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

        Public Property tHalfBanner() As String
            Get
                Return halfbanner
            End Get
            Set(ByVal value As String)
                halfbanner = value
            End Set
        End Property

    End Class

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        If Not IsPostBack Then

            'Session("aHalfBanner") = Nothing
            Session("cPonteiroHalfBanner") = 0
            Call sBindDestaqueBanner(fCarregarHalfBanner)

        End If

    End Sub

    Function fCarregarDestaqueBanner2() As ArrayList

        Dim func As New clsAcessos
        Dim ds As New DataSet
        Dim wPortal As New wsCash.wPortal
        Dim wContador As Integer = 0
        Dim aHalfBanner As New ArrayList
        Dim wCount As Integer = 0

        'ws = func.fRetornaWS()
        'wPortal = ws.fCarregarPortal(func.fPortal())

        ' wPortal = func.fCarregarPortal()

        ds = Session("dsHalfBanners")

        If ds Is Nothing Then

            'wPortal = func.fCarregarPortal()

            ds = func.fRetornaHalfBannerCadastrado()

            Session("dsHalfBanners") = ds

        Else

            ds = Session("dsHalfBanners")

        End If

        While wCount < 1

            If ds.Tables(0).Rows.Count = 0 Then

                wCount += 1

            End If

            For I As Integer = Session("cPonteiroHalfBanner") To ds.Tables(0).Rows.Count - 1

                'If wContador = wPortal.qDestaques Then
                '    Session("cPonteiroPublicidade") =  = I
                '    Exit For
                'Else
                '    wContador += 1
                'End If

                Dim clsHalfBanner As New clsHalfBanner
                clsHalfBanner.tArquivo = ds.Tables(0).Rows(I).Item("tArquivo")
                clsHalfBanner.cExtensao = ds.Tables(0).Rows(I).Item("cTipoBanner")
                clsHalfBanner.tLink = IIf(ds.Tables(0).Rows(I).Item("tUrl") Is DBNull.Value, "", ds.Tables(0).Rows(I).Item("tUrl"))
                aHalfBanner.Add(clsHalfBanner)

                wCount += 1

                If Session("cPonteiroHalfBanner") = ds.Tables(0).Rows.Count - 1 Then

                    Session("cPonteiroHalfBanner") = 0

                Else

                    Session("cPonteiroHalfBanner") = wCount

                End If

                Exit For

            Next

        End While

        Return aHalfBanner

    End Function

    Function fCarregarHalfBanner() As ArrayList

        Dim wContador As Integer = 0
        Dim aHalfBanners As New ArrayList
        Dim aHalfBannersRetorno As New ArrayList
        Dim wCount As Integer = 0

        'lblControle.Text = CType(lblControle.Text, Integer) + 1

        If Session("aHalfBanner") Is Nothing Then

            Dim ds As New DataSet
            Dim wPortal As New wsCash.wPortal
            Dim ws As New wsCash.wsCash
            Dim func As New clsAcessos

            'Carregar Data Set de Detaques Banners
            ws = func.fRetornaWS()

            ds = ws.fRetornaHalfBannerCadastrado(func.fPortal())

            If ds.Tables(0).Rows.Count > 0 Then

                For I As Integer = 0 To ds.Tables(0).Rows.Count - 1

                    Dim clsHalfBanner As New clsHalfBanner

                    With clsHalfBanner
                        .tArquivo = ds.Tables(0).Rows(I).Item("tArquivo")
                        .cExtensao = ds.Tables(0).Rows(I).Item("cTipoBanner")
                        .tLink = IIf(ds.Tables(0).Rows(I).Item("tUrl") Is DBNull.Value, "", ds.Tables(0).Rows(I).Item("tUrl"))
                        .tHalfBanner = fRetornaDestaqueBanner(.tArquivo, .cExtensao, .tLink)
                        aHalfBanners.Add(clsHalfBanner)
                    End With


                Next

                Session("aHalfBanner") = aHalfBanners

            Else
                Dim clsHalfBanner As New clsHalfBanner

                With clsHalfBanner
                    .tArquivo = ""
                    .cExtensao = 0
                    .tLink = ""
                    .tHalfBanner = ""
                    aHalfBanners.Add(clsHalfBanner)

                    Session("aHalfBanner") = aHalfBanners

                End With

            End If

        Else

            aHalfBanners = Session("aHalfBanner")

        End If

        If aHalfBanners.Count > 0 Then

            aHalfBannersRetorno.Add(aHalfBanners(Session("cPonteiroHalfBanner")))
            Session("cPonteiroHalfBanner") += 1

        End If

        If Session("cPonteiroHalfBanner") = aHalfBanners.Count Then
            Session("cPonteiroHalfBanner") = 0
        End If

        Return aHalfBannersRetorno

    End Function

    Sub sBindDestaqueBanner(ByVal aDestaques As ArrayList)

        With dlPublicidades
            .DataSource = aDestaques
            .DataBind()
        End With

    End Sub

    Protected Sub ratDestaqueBanner_Tick(ByVal sender As Object, ByVal e As Telerik.WebControls.TickEventArgs) Handles ratDestaqueBanner.Tick

        'lblControle.Text = CType(lblControle.Text, Integer) + 1
        Call sBindDestaqueBanner(fCarregarHalfBanner())

    End Sub

    Function fRetornaDestaqueBanner(ByVal wArquivo As String, ByVal wTipoBAnner As Integer, ByVal wLink As String) As String

        Dim ws As New wsCash.wsCash
        Dim wTagImg As String

        ws.Url = ConfigurationManager.AppSettings("wsURL")

        Dim wURL As String

        wURL = ws.fRetornaURLImagem(ConfigurationManager.AppSettings("URLIMAGENS"), "BANNER", wArquivo)

        If wTipoBAnner = 1 Then

            wTagImg = " <object id=" & Chr(34) & "FlashID" & Chr(34) & " classid=" & Chr(34) & "clsid:D27CDB6E-AE6D-11cf-96B8-444553540000" & Chr(34) & " width=" & Chr(34) & "505" & Chr(34) & " height=" & Chr(34) & "142" & Chr(34) & ">"
            wTagImg += " <param name=" & Chr(34) & "movie" & Chr(34) & " value=" & Chr(34) & wURL & Chr(34) & "/>"
            wTagImg += " <param name=" & Chr(34) & "quality" & Chr(34) & " value=" & Chr(34) & "high" & Chr(34) & "/>"
            wTagImg += "  <param name=" & Chr(34) & "wmode" & Chr(34) & " value=" & Chr(34) & "opaque" & Chr(34) & "/>"
            wTagImg += " <param name=" & Chr(34) & "swfversion" & Chr(34) & " value=" & Chr(34) & "9.0.45.0" & Chr(34) & "/>"
            wTagImg += " <param name=" & Chr(34) & "expressinstall" & Chr(34) & " value=" & Chr(34) & "Scripts/expressInstall.swf" & Chr(34) & " />"
            wTagImg += " </object>"

        Else

            If wLink <> "" Then

                wLink.Replace("http://", "")

                wTagImg = "<a href=" & Chr(34) & "http://" & wLink.Trim & Chr(34) & " target=" & Chr(34) & "_blank " & Chr(34) & "><img src=" & Chr(34) & wURL & Chr(34) & " style=" & Chr(34) & "width: 189px; " & Chr(34) & " /></a>"

            Else

                wTagImg = "<img src=" & Chr(34) & wURL & Chr(34) & " style=" & Chr(34) & "width: 189px;" & Chr(34) & " />"

            End If

        End If

        Return wTagImg

    End Function

End Class
