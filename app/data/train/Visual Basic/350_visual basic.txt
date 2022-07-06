Imports System.Configuration
Imports System.Data

Partial Class ucRedeCredenciadaComResultado
    Inherits System.Web.UI.UserControl

    Dim func As New Funcoes
    Dim wSQL As String

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        If Not IsPostBack Then

            Call sConfiguraSecao()
            Call sCarregarComboEstados()
            Call sCarregarComboCidades()
            Call sCarregarComboCategorias()
            Call sCarregarComboEspecialidades()

        End If

    End Sub

    Sub sConfiguraSecao()

        Dim ws As New wsCash.wsCash
        ws.Url = ConfigurationManager.AppSettings("wsURL")

        Dim wCodigoSecao As String = Request.Params("cSecao")

        If wCodigoSecao = "" Then
            Session("cSecao") = 1
        Else
            Session("cSecao") = wCodigoSecao
        End If

        Dim wSecao As New wsCash.wSecao
        wSecao = ws.fSecao(Session("cSecao"))

        If wSecao.cEspecialidades = True Then
            lblEspecialidade.Visible = True
            lblDivisor.Visible = True
            cmbEspecialidades.Visible = True
        Else
            lblEspecialidade.Visible = False
            lblDivisor.Visible = False
            cmbEspecialidades.Visible = False
        End If

    End Sub

    Sub sCarregarComboEstados()

        Dim ds As dataset
        Dim ws As New wsCash.wsCash
        ws.Url = ConfigurationManager.AppSettings("wsURL")

        wSQL = "  select distinct e.cEstado, e.tEstado, e.tDescricao"
        wSQL += " from tbEstados E"
        wSQL += " join tbCidades C on e.cEstado = c.cEstado"
        wSQL += " join tbAssociados A on c.cCidade = a.cCidade"
        wSQL += " order by tEstado"

        Call func.sCarregaCombo(ws.fRetornaDataSet(wSQL), cmbEstados, "tDescricao", "cEstado")

        If Session("cEstado") Is Nothing Then

            wSQL = "  select *  "
            wSQL += " from tbEstados "
            wSQL += " where cPreferencial = 1 "

            Dim wPreferencial As Integer

            ds = ws.fRetornaDataSet(wSQL)

            If Not ds.Tables(0).Rows.Count = 0 Then
                wPreferencial = ds.Tables(0).Rows(0).Item("cEstado")
            Else
                wPreferencial = 1
            End If

            Session("cEstado") = wPreferencial

        End If

        If Not Session("cEstado") Is Nothing Then
            Call func.sSetaCombo(cmbEstados, Session("cEstado"))
            Session("cEstado") = cmbEstados.SelectedItem.Value
        End If

    End Sub

    Sub sCarregarComboCidades()

        Dim ws As New wsCash.wsCash
        ws.Url = ConfigurationManager.AppSettings("wsURL")

        wSQL = "Select * from tbCidades where cEstado = " & cmbEstados.SelectedItem.Value & " order by tCidade "
        Call func.sCarregaCombo(ws.fRetornaDataSet(wSQL), cmbCidades, "tCidade", "cCidade")

        If Not Session("cCidade") Is Nothing Then
            Call func.sSetaCombo(cmbCidades, Session("cCidade"))
        End If

    End Sub

    Sub sCarregarComboCategorias()

        Dim ws As New wsCash.wsCash
        ws.Url = ConfigurationManager.AppSettings("wsURL")

        wSQL = "  Select c.tCategoria, c.cCategoria"
        wSQL += " from tbCategorias C "
        wSQL += " join tbAssociados A on C.cCategoria = A.cCategoria"
        wSQL += " where a.cCidade = " & cmbCidades.SelectedItem.Value
        wSQL += " and cSecao = " & Session("cSecao")
        wSQL += " group by c.tCategoria, c.cCategoria"
        wSQL += " order by c.tCategoria "

        Call func.sCarregaCombo(ws.fRetornaDataSet(wSQL), cmbCategorias, "tCategoria", "cCategoria", "(Todas as categorias)")

        If Not Session("cCategoria") Is Nothing Then
            Call func.sSetaCombo(cmbCategorias, Session("cCategoria"))
        End If

    End Sub

    Sub sCarregarComboEspecialidades()

        Dim ws As New wsCash.wsCash
        ws.Url = ConfigurationManager.AppSettings("wsURL")

        cmbEspecialidades.Items.Clear()

        wSQL = "  Select E.tEspecialidade, E.cEspecialidade"
        wSQL += " from tbEspecialidades E  "
        wSQL += " join tbAssociadosEspecialidades AE on E.cEspecialidade = AE.cEspecialidade "
        wSQL += " join tbAssociados A on a.cAssociado = ae.cAssociado"
        wSQL += " where a.cCidade = " & cmbCidades.SelectedItem.Value
        If cmbCategorias.SelectedItem.Value <> "0" Then
            wSQL += " and A.cCategoria = " & cmbCategorias.SelectedItem.Value
        End If
        wSQL += " group by E.tEspecialidade, E.cEspecialidade "
        wSQL += " order by E.tEspecialidade "

        Call func.sCarregaCombo(ws.fRetornaDataSet(wSQL), cmbEspecialidades, "tEspecialidade", "cEspecialidade", "(Todas as especialidades)")

        If Not Session("cEspecialidade") Is Nothing Then
            Call func.sSetaCombo(cmbEspecialidades, Session("cEspecialidade"))
        End If

    End Sub

    Protected Sub cmbEstados_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cmbEstados.SelectedIndexChanged

        Call sCarregarComboCidades()
        Call sCarregarComboCategorias()
        Call sCarregarComboEspecialidades()

    End Sub

    Protected Sub cmbCidades_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cmbCidades.SelectedIndexChanged

        Call sCarregarComboCategorias()
        Call sCarregarComboEspecialidades()

    End Sub

    Protected Sub cmdConsultar_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles cmdConsultar.Click

        Call sConsultarRedeCredenciada()

    End Sub

    Sub sConsultarRedeCredenciada()

        Session("cEstado") = cmbEstados.SelectedItem.Value
        Session("cCidade") = cmbCidades.SelectedItem.Value
        If cmbCategorias.Items.Count <> 0 Then
            Session("cCategoria") = cmbCategorias.SelectedItem.Value
        Else
            Session("cCategoria") = "0"
        End If

        If cmbEspecialidades.Items.Count <> 0 Then
            Session("cEspecialidade") = cmbEspecialidades.SelectedItem.Value
        Else
            Session("cEspecialidade") = "0"
        End If

        Call sCarregarResultadoPesquisa()

    End Sub

    Sub sCarregarResultadoPesquisa()

        Dim ws As New wsCash.wsCash
        ws.Url = ConfigurationManager.AppSettings("wsURL")

        'wSQL = "  Select * "
        wSQL = "  Select distinct tAssociado, a.cAssociado, tEndereco, tNumero, tComplemento, tTelefone1, tTelefone2, tEmail, tSite, tObservacao  "
        wSQL += " from tbAssociados A"
        wSQL += " join tbAssociadosEspecialidades AE on a.cAssociado = ae.cAssociado "
        wSQL += " join tbCategorias C on a.cCategoria = c.cCategoria "
        wSQL += " where cCidade = " & Session("cCidade")
        wSQL += " and c.cSecao = " & Session("cSecao")

        If Session("cCategoria") <> "0" Then
            wSQL += " and c.cCategoria = " & Session("cCategoria")
        End If

        If Session("cEspecialidade") <> "0" Then
            wSQL += " and cEspecialidade = " & Session("cEspecialidade")
        End If

        wSQL += " order by tAssociado "

        With gvResultados

            .DataSource = ws.fRetornaDataSet(wSQL)
            .DataBind()

        End With

    End Sub

    Function fRetornaAssociado(ByVal wAssociado As String, ByVal wEndereco As String, ByVal wNumero As String, ByVal wComplemento As String, ByVal wTelefone As String, ByVal wSite As String, ByVal wEmail As String, ByVal wObservacao As String) As String

        Dim wResposta As String

        wResposta = wAssociado.ToUpper & "<br>"

        If wObservacao.Trim.Length <> 0 Then
            wResposta += wObservacao.Trim & "<br>"
        End If

        wResposta += wEndereco.Trim & wNumero.Trim & " - " & wComplemento.Trim & "<br>"
        wResposta += wTelefone.Trim & "<br>"

        If wSite.Trim.Length <> 0 Then
            wResposta += "<a href=http://" & wSite.Trim & ">" & wSite.Trim & "</a>" & "<br>"
        End If

        If wEmail.Trim.Length <> 0 Then
            wResposta += "<a href=mailto://" & wEmail.Trim & ">" & wEmail.Trim & "</a>" & "<br>"
        End If

        Return wResposta

    End Function

    Protected Sub gvResultados_PageIndexChanging(ByVal sender As Object, ByVal e As System.Web.UI.WebControls.GridViewPageEventArgs) Handles gvResultados.PageIndexChanging

        gvResultados.PageIndex = e.NewPageIndex
        Call sCarregarResultadoPesquisa()

    End Sub



End Class
