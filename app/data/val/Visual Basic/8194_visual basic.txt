Imports HG.Db.ORM

Public Class ChoixUniter

    Public produit_id As String = "56"
    Public user_validate As Boolean = False
    Public point_vente As Integer = 2
    Public qte_stock As String = ""
    Dim total_payer As Double = 0


    Sub LoadQte()
        Try

            Dim cmd = Database.GetCommand
            cmd.CommandText = "SELECT ligne_stocks.id, produits.nom, SUM(qte) AS qte, SUM(qte_vendu) AS qte_vendu, prix_achat, prix_vente, SUM(qte - qte_vendu) AS qte_en_stock, stocks.created  FROM ligne_stocks INNER JOIN produits ON ligne_stocks.produit_id = produits.id INNER JOIN stocks ON ligne_stocks.stock_id = stocks.id WHERE produit_id = @pid AND stocks.point_vente_id = @pvid GROUP BY prix_vente, prix_achat, produits.id  HAVING SUM(qte_vendu) < SUM(qte)  ORDER BY stocks.created ASC LIMIT 1"

            If point_vente = "3" Then
                cmd.Parameters.AddWithValue("@pvid", 1)

                labelRemise.Text = "Prix vente :"

            Else
                cmd.Parameters.AddWithValue("@pvid", point_vente)
            End If

            cmd.Prepare()
            cmd.Parameters.AddWithValue("@pid", produit_id)
            Dim result = cmd.ExecuteReader

            If Not result.HasRows Then
                Util.ShowMessage("Ce produit n'a pas de stock disponible pour ce point de vente", 1)
                Close()
                Exit Sub
            End If

            result.Read()
            qte_stock = result.Item("qte_en_stock")

            PanelCenter.Controls.Clear()

            If point_vente = "1" OrElse point_vente = "3" Then
                Dim tags As New UI.UniteTag
                tags.id = result.Item("id")
                tags.displayText = "Carton"
                tags.abbr = "Crtn"
                tags.montant = result.Item("prix_vente")
                tags.prix_achat = result.Item("prix_achat")

                Dim btn = UI.UI.GetButtonChoix(tags)
                AddHandler btn.Click, AddressOf btnUniteClieked
                PanelCenter.Controls.Add(btn)

                'Util.ShowMessage(PanelCenter.Controls.Count)

                SelectButton(btn)
                txtQte.Enabled = True
                txtQte.Focus()

                labelStock.Text = result.Item("qte_en_stock").ToString & "  Carton(s)"

                
            ElseIf point_vente = "2" Then
                labelStock.Text = Util.GetHumanBoucherieQte(result.Item("qte_en_stock"))

                For i As Integer = 1 To 3
                    Dim tags As New UI.UniteTag
                    tags.id = result.Item("id")
                    tags.prix_achat = result.Item("prix_achat")
                    tags.montant = result.Item("prix_vente")

                    If i = 1 Then
                        tags.displayText = "Killo"
                        tags.abbr = "Kg"
                    ElseIf i = 2 Then
                        tags.displayText = "1/2 Killo"
                        tags.abbr = "1/2 Kg"
                    ElseIf i = 3 Then
                        tags.displayText = "1/4 Killo"
                        tags.abbr = "1/4 Kg"
                    End If

                    Dim btn = UI.UI.GetButtonChoix(tags)
                    AddHandler btn.Click, AddressOf btnUniteClieked
                    PanelCenter.Controls.Add(btn)
                Next

            End If


        Catch ex As Exception
            If My.Settings.app_debug_mode Then
                Util.ShowMessage(ex.ToString)
            Else
                Util.ShowMessage("Echec de connexion a la base de donnees", 2)
            End If
        Finally
            Database.CloseConnection()
        End Try
    End Sub

    Public selected As UI.UniteTag

    Private Sub btnClose_Click(sender As Object, e As EventArgs) Handles btnClose.Click
        Close()
    End Sub





    Private Sub ChoixUniter_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        LoadQte()

        form_initialized = True

    End Sub


    Private Sub btnUniteClieked(sender As Object, e As EventArgs)
        SelectButton(DirectCast(sender, Button))
    End Sub

    Sub SelectButton(ByRef btn As Button)
        For Each c As Control In PanelCenter.Controls
            With DirectCast(c, Button)
                .BackColor = Color.Turquoise
                .ForeColor = Color.Black
            End With
        Next

        With btn
            .BackColor = Color.DimGray
            .ForeColor = Color.White
        End With

        txtQte.Enabled = True
        txtQte.Focus()
        selected = DirectCast(btn.Tag, UI.UniteTag)
        txtPU.Text = selected.montant & " " & My.Settings.unite_monetaire
        txtTTC.Text = txtPU.Text
        total_payer = selected.montant

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        If IsNothing(selected) Then
            Util.ShowMessage("Selectioner l'unite svp", 2)
            Exit Sub
        ElseIf txtQte.Text.Length = 0 Or txtQte.Text = "0" Or txtTTC.Text.Replace(My.Settings.unite_monetaire, "").Trim = "0" Then
            Util.ShowMessage("Veuillez saisir la quantite", 2)
            Exit Sub

        ElseIf (point_vente = "3" And txtRemise.Text.Trim.Length = 0) OrElse (point_vente = "3" And txtRemise.Text.Trim = "0") Then
            Util.ShowMessage("Veuillez saisir les informations correct", 2)
            Exit Sub
        End If

        user_validate = True
        Close()
    End Sub

  

    Private Sub txtQte_KeyPress(sender As Object, e As KeyPressEventArgs) Handles txtQte.KeyPress
        Util.checkkey(e)
    End Sub

    Dim form_initialized As Boolean = False

    Private Sub txtQte_TextChanged(sender As Object, e As EventArgs) Handles txtQte.TextChanged
        If form_initialized Then
            If txtQte.Text.Length > 0 And Not IsNothing(selected) Then
                CheckValideQte()

                If point_vente <> "3" Then
                    txtTTC.Text = ((selected.montant * CInt(txtQte.Text)) - CInt(txtRemise.Text)) & " " & My.Settings.unite_monetaire
                    total_payer = ((selected.montant * CInt(txtQte.Text)) - CInt(txtRemise.Text))
                Else

                    txtTTC.Text = (CInt(txtRemise.Text) * CInt(txtQte.Text)) & " " & My.Settings.unite_monetaire
                    total_payer = (CInt(txtRemise.Text) * CInt(txtQte.Text))

                End If

                
            End If
        End If
    End Sub


    Function CheckValideQte() As Boolean
        Dim oper As Double = 1
        Dim back = True

        If selected.displayText.Equals(Constants.DEMI) Then
            oper = 0.5
        ElseIf selected.displayText.Equals(Constants.QUARD) Then
            oper = 0.25
        End If

        If (CInt(txtQte.Text) * oper) > CDbl(qte_stock) Then
            Util.ShowMessage("La quantite saisie est superieur a celle disponible en stock" & Environment.NewLine & Environment.NewLine & "note : changer l'unite de vente ou saisir une quantite valide", 2)
            back = False
            txtQte.Text = "0"
        End If
        Return back
    End Function

    Private Sub txtQte_Leave(sender As Object, e As EventArgs) Handles txtQte.Leave
        If txtQte.Text.Length = 0 Or txtQte.Text = "0" Then
            txtQte.Text = "0"
            CheckValideQte()
        End If
    End Sub

    Private Sub txtRemise_Leave(sender As Object, e As EventArgs) Handles txtRemise.Leave
        If txtRemise.Text.Length = 0 Then
            txtRemise.Text = "0"
        End If
    End Sub

    Private Sub txtRemise_TextChanged(sender As Object, e As EventArgs) Handles txtRemise.TextChanged
        If form_initialized AndAlso Not IsNothing(selected) Then
            If txtRemise.Text.Length = 0 Then
                txtRemise.Text = "0"
            End If
            If txtRemise.Text.Length > 0 AndAlso point_vente <> "3" Then
                txtTTC.Text = ((selected.montant * CInt(txtQte.Text)) - CInt(txtRemise.Text)) & " " & My.Settings.unite_monetaire

                Dim saisie = txtRemise.Text.Replace(My.Settings.unite_monetaire, "").Trim

                If CDbl(saisie) > CDbl(total_payer) Then
                    Beep()
                    txtRemise.Text = 0
                End If

                total_payer = ((selected.montant * CInt(txtQte.Text)) - CInt(txtRemise.Text))
            ElseIf txtRemise.Text.Length > 0 AndAlso point_vente = "3" Then
                txtTTC.Text = (CInt(txtRemise.Text) * CInt(txtQte.Text)) & " " & My.Settings.unite_monetaire
            End If
        Else
            Beep()
            txtRemise.Text = "0"
        End If
    End Sub

    Private Sub txtRemise_KeyPress(sender As Object, e As KeyPressEventArgs) Handles txtRemise.KeyPress
        Util.checkkey(e)
    End Sub



End Class