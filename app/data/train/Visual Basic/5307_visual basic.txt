Public Class Paiements

    Public fac_id As String = ""

    Dim total As Double = 0
    Dim payer As Double = 0
    Dim restePayer As Double = 0
    Private point_vente_id As String = ""
    Private point_vente_name As String = ""


    Sub LoadDetails()
        Try
            Dim sql = "SELECT numero, created, client, IFNULL(ttc, 0) AS ttc, IFNULL(payer, 0) AS payer, point_vente_id, point_vente FROM (SELECT factures.id AS facture_id, numero, factures.created, CONCAT_WS(' ', clients.nom, clients.prenom) AS client, SUM((qte*pu) - ligne_factures.remise) AS ttc, point_vente_id, point_ventes.nom AS point_vente FROM factures INNER JOIN ligne_factures ON factures.id = ligne_factures.facture_id INNER JOIN clients ON factures.client_id = clients.id INNER JOIN point_ventes ON factures.point_vente_id = point_ventes.id GROUP BY facture_id) AS A NATURAL LEFT JOIN (SELECT facture_id, SUM(montant) AS payer FROM paiements GROUP BY facture_id) AS B WHERE facture_id = '" & fac_id & "'"

  
            Dim cmd = Database.GetCommand
            cmd.CommandText = sql
            Dim result = cmd.ExecuteReader
            If result.HasRows Then
                result.Read()
                txtNo.Text = result.GetValue(0).ToString
                txtDate.Text = result.GetDateTime(1).ToString
                txtClient.Text = result.GetValue(2).ToString
                txtTTC.Text = result.GetValue(3).ToString & " " & My.Settings.unite_monetaire
                txtPayer.Text = result.GetValue(4).ToString & " " & My.Settings.unite_monetaire
                Dim reste As Double = CDbl(result.GetValue(3).ToString) - CDbl(result.GetValue(4).ToString)
                txtRestePayer.Text = reste & " " & My.Settings.unite_monetaire

                total = CDbl(result.GetValue(3).ToString)
                payer = CDbl(result.GetValue(4).ToString)
                restePayer = total - payer

                point_vente_id = result.GetValue(5).ToString
                point_vente_name = result.GetValue(6).ToString
            End If
            result.Close()
            cmd.Dispose()
            cmd.Connection.Clone()
        Catch ex As Exception
            If My.Settings.app_debug_mode Then
                Util.ShowMessage(ex.ToString, 2)
            Else
                Util.ShowMessage("Echec de connexion a la base de donnees", 2)
            End If
        Finally
            Database.CloseConnection()
            GC.Collect()
        End Try

    End Sub



    Sub LoadHistorique()
        Try
            Database.Query("SELECT paiements.created, username, montant FROM paiements INNER JOIN users ON paiements.user_id = users.id WHERE facture_id = '" & fac_id & "'")
            Dim resutl = Database.ExecuteResult
            ListHistorique.Items.Clear()
            While resutl.Read
                Dim item As New ListViewItem(resutl.GetValue(0).ToString)
                item.SubItems.Add(resutl.GetValue(1).ToString)
                item.SubItems.Add(resutl.GetValue(2).ToString)
                ListHistorique.Items.Add(item)
            End While
        Catch ex As Exception
            If My.Settings.app_debug_mode Then
                Util.ShowMessage(ex.ToString, 2)
            Else
                Util.ShowMessage("Echec de connexion a la base de donnees", 2)
            End If
        Finally
            Database.CloseConnection()
            GC.Collect()
        End Try
    End Sub

    Private Sub txtMontant_KeyPress(sender As Object, e As KeyPressEventArgs) Handles txtMontant.KeyPress
        Util.checkkey(e)
    End Sub

    Private Sub Paiements_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Util.setupFrame(Me)
        LoadDetails()
        LoadHistorique()
    End Sub

    Private Sub txtMontant_Enter(sender As Object, e As EventArgs) Handles txtMontant.Enter
        If txtMontant.Text = "0" Then
            txtMontant.Text = restePayer
        End If
    End Sub

    Private Sub txtMontant_KeyDown(sender As Object, e As KeyEventArgs) Handles txtMontant.KeyDown
        If DirectCast(sender, TextBox).Text = "0" Then
            DirectCast(sender, TextBox).Text = ""
        End If
    End Sub

    Private Sub txtRexu_TextChanged(sender As Object, e As EventArgs) Handles txtMontant.TextChanged
        If txtMontant.Text.Length > 0 Then
            Try
                If CDbl(txtMontant.Text) > restePayer Then
                    txtReste.Text = (CDbl(txtMontant.Text) - restePayer) & " " & My.Settings.unite_monetaire
                Else
                    txtReste.Text = "0"
                End If
            Catch ex As Exception

            End Try
        Else
            txtReste.Text = "0"
        End If
    End Sub


    Private Sub btnSolder_Click(sender As Object, e As EventArgs) Handles btnSolder.Click
        Try

            If (txtMontant.Text.Length = 0 Or txtMontant.Text = "0") Then
                Util.ShowMessage("Veuillez saisir le montant reçu du client", 1)
                Exit Sub
            End If

            Dim cnx = Database.GetConnection
            Dim transaction = cnx.BeginTransaction

            Dim cmd = cnx.CreateCommand
            cmd.Transaction = transaction
            Dim sql As String = ""
            Dim affectedRow As Long = 0

            Dim aPayer As Double = restePayer

            If txtReste.Text = "0" Then
                aPayer = CDbl(txtMontant.Text)
            End If

            sql = "INSERT INTO paiements (facture_id, montant, user_id, created) VALUE (@fid, @mnt, @u_id, NOW())"

            cmd.CommandText = sql
            cmd.Prepare()
            cmd.Parameters.AddWithValue("@fid", fac_id)
            cmd.Parameters.AddWithValue("@mnt", aPayer)
            cmd.Parameters.AddWithValue("@u_id", My.Settings.user_id)

            affectedRow = cmd.ExecuteNonQuery()
            cmd.Dispose()

            cmd = cnx.CreateCommand
            cmd.Transaction = transaction

            sql = "INSERT INTO transactions(nature, origine, origine_id, user_id, designation, montant, created) " & _
                " VALUE ('RECETTE', @ori, @ori_id, @u_id, @des, @mnt, NOW())"
            cmd.CommandText = sql
            cmd.Prepare()
            cmd.Parameters.AddWithValue("@ori", point_vente_name)
            cmd.Parameters.AddWithValue("@ori_id", point_vente_id)
            cmd.Parameters.AddWithValue("@u_id", My.Settings.user_id)
            cmd.Parameters.AddWithValue("@des", "Paiement du client " & txtClient.Text)
            cmd.Parameters.AddWithValue("@mnt", aPayer)

            affectedRow += cmd.ExecuteNonQuery
            cmd.Dispose()

            cmd = cnx.CreateCommand
            cmd.Transaction = transaction

            cmd.CommandText = "CALL get_total_facture('" & fac_id & "')"
            Dim result = cmd.ExecuteReader
            result.Read()

            Dim total_facture = result.GetDouble(0)
            result.Close()

            cmd.CommandText = "CALL get_total_paiement('" & fac_id & "')"
            result = cmd.ExecuteReader
            result.Read()
            Dim total_paiement = result.GetValue(0).ToString
            result.Close()
            cmd.Dispose()

            Try
                total_paiement = CDbl(total_paiement)
            Catch ex As Exception
                total_paiement = 0
            End Try

            If total_facture = total_paiement Then
                cmd = cnx.CreateCommand
                cmd.Transaction = transaction
                cmd.CommandText = "UPDATE factures SET solder = 1, status = '2' WHERE id = @f_id"
                cmd.Prepare()
                cmd.Parameters.AddWithValue("@f_id", fac_id)
                affectedRow += cmd.ExecuteNonQuery
                Util.ShowMessage("La facture a été solder avec succès")
                cmd.Dispose()
            Else
                cmd = Database.GetCommand
                cmd.Transaction = transaction
                cmd.CommandText = "UPDATE factures SET solder = 1, status = '3' WHERE id = @f_id"
                cmd.Prepare()
                cmd.Parameters.AddWithValue("@f_id", fac_id)
                affectedRow += cmd.ExecuteNonQuery
                Util.ShowMessage("Acompte enregistrer avec succès")
                cmd.Dispose()
            End If

            transaction.Commit()
            cnx.Dispose()
            cnx.Close()

            Close()

        Catch ex As Exception
            If My.Settings.app_debug_mode Then
                Util.ShowMessage(ex.ToString, 2)
            Else
                Util.ShowMessage("Echec de connexion a la base de donnees", 2)
            End If
        Finally
            Database.CloseConnection()
            GC.Collect()
        End Try
    End Sub
End Class