Imports System.Threading

Namespace Reports
    Public Class ListeFactures

        Public mode As String = ""

        Private total As Double = 0

        Private date_filter As String = "today"

        Public etat_facture = "1"

        Private facturesTable As New Db.Model.Factures

        Public client_id As String = ""

        Public forme_loaded As Boolean = False


        Private Delegate Sub LoadDataDelegate(ByVal today As Boolean)

        Private Sub ProcessLoadData()
            If InvokeRequired Then
                forme_loaded = True
                BeginInvoke(New LoadDataDelegate(AddressOf LoadFactures), True)
            Else
                LoadFactures(True)
            End If
        End Sub


        Private Sub PanelLoader_VisibleChanged(sender As Object, e As EventArgs) Handles PanelLoader.VisibleChanged
            If PanelLoader.Visible = False Then
                ProgressBar1.Value = 0
            End If
        End Sub

        Private Sub IncrementProgress()
            Try
                ProgressBar1.Value = ProgressBar1.Value + 1
                If ProgressBar1.Value = ProgressBar1.Maximum Then
                    ProgressBar1.Value = 0
                End If
            Catch ex As Exception

            End Try
        End Sub

        Sub LoadFactures(Optional byToday As Boolean = False, Optional byWeek As Boolean = False, Optional byMonth As Boolean = False, Optional byItervall As Boolean = False, Optional byAll As Boolean = False, Optional byNumero As Boolean = False, Optional byFilter As Boolean = False)

            If forme_loaded = False Then
                Exit Sub
            End If

            total = 0
            ListFactures.Items.Clear()

            Dim en_attente As Double = 0
            Dim payer As Double = 0
            Dim annuler As Double = 0
            Dim impayer As Double = 0
            Dim acompte As Double = 0
            Dim total_remise As Double = 0

            Try

                Dim result As MySql.Data.MySqlClient.MySqlDataReader

                If byToday Then
                    result = facturesTable.GetToday
                ElseIf byWeek Then
                    result = facturesTable.GetLastWeek
                ElseIf byMonth Then
                    result = facturesTable.GetLastMonth
                ElseIf byAll Then
                    result = facturesTable.GetAll
                ElseIf byItervall Then
                    result = facturesTable.GetInterval(txtDateDu, txtDateAu)
                ElseIf byNumero Then
                    result = facturesTable.GetByNumero(txtNumero.Text)
                ElseIf byFilter Then
                    result = facturesTable.GetByFilter(date_filter & "|" & GetHeaderFilter())
                ElseIf Not byToday And Not byWeek And Not byMonth Then
                    result = facturesTable.GetDefault
                End If

                Dim iter As Integer = 1

                PanelLoader.Visible = True
                GroupBoxFiltre.Enabled = False

                While result.Read
                    Dim item As New ListViewItem(CStr(result.Item("numero")))
                    item.SubItems.Add(CStr(result.Item("created")))
                    item.SubItems.Add(CStr(result.Item("nom")))
                    item.SubItems.Add(CStr(result.Item("client")))
                    item.SubItems.Add(CStr(result.Item("total") & " " & My.Settings.unite_monetaire))
                    item.SubItems.Add(CStr(result.Item("remise") & " " & My.Settings.unite_monetaire))

                    Dim remise As Double = 0
                    Double.TryParse(CStr(result.Item("remise")), remise)
                    total_remise += remise

                    Dim status = CStr(result.Item("status"))
                    Dim text = "En attente"
                    If status = "1" Then
                        item.BackColor = Color.Yellow
                        en_attente += 1
                    ElseIf status = "2" Then
                        text = "Solder"
                        item.BackColor = Color.YellowGreen
                        payer += 1
                    ElseIf status = "3" Then
                        item.BackColor = Color.Cyan
                        text = "En cours de paiement"
                        acompte += 1
                    ElseIf status = "0" Then
                        item.BackColor = Color.Red
                        text = "Annuler"
                        annuler += 1
                    ElseIf status = "4" Then
                        text = "Non payer"
                        impayer += 1
                    End If
                    item.SubItems.Add(text)
                    item.SubItems.Add(CStr(result.Item("ttc") & " " & My.Settings.unite_monetaire))
                    item.SubItems.Add(CStr(result.Item("id")))

                    ListFactures.Items.Add(item)
                    iter += 1
                    total += CDbl(result.Item("total"))

                    IncrementProgress()
                End While

                result.Close()

            Catch ex As Exception
                If My.Settings.app_debug_mode Then
                    Util.ShowMessage(ex.ToString)
                Else
                    Util.ShowMessage("Une erreur fatale est survenue lors de la lecture des données", 2)
                End If
            Finally
                Database.CloseConnection()

                labelAttente.Text = "Factures en attente (" & en_attente & ")"
                labelSolder.Text = "Factures soldées (" & payer & ")"
                labelAnnuler.Text = "Factures annulées (" & annuler & ")"
                labelAcompte.Text = "Factures en cours de paiement (" & acompte & ")"
                labelNonPayer.Text = "Factures impayées (" & impayer & ")"

                labelTotal.Text = (total - total_remise) & " " & My.Settings.unite_monetaire

                label_remise.Text = total_remise

                PanelLoader.Visible = False
                GroupBoxFiltre.Enabled = True

                GC.Collect()
            End Try
        End Sub

        
        Private Sub ListeFactures_Load(sender As Object, e As EventArgs) Handles MyBase.Load
            Util.setupFrame(Me)
            facturesTable.facture_frame = Me
            ProgressBar1.Style = ProgressBarStyle.Marquee

            If mode.Length > 0 Then
                facturesTable.mode = mode
                btnPaiement.Visible = True
            End If

            txtDateAu.MaxDate = Today
            txtDateDu.MaxDate = Today

            txtDateAu.Value = Today
            txtDateDu.Value = Today

            Util.PopulateCombo(cmbPoint, "SELECT id, nom FROM point_ventes", "id", "nom")
            Util.PopulateCombo(cmbClient, "SELECT id, CONCAT_WS(' ', nom, prenom) AS nom FROM clients WHERE id > 1 AND statut_client = 1", "id", "nom")

            If client_id.Length > 0 Then
                ckClient.Checked = True
                ckClient.Enabled = False
                cmbClient.Enabled = False
                date_filter = "month"
                cmbClient.SelectedValue = client_id
                btnFusion.Visible = True
                btnAll.Enabled = False
            End If

            Dim threadLoad As New Thread(New ThreadStart(AddressOf ProcessLoadData))
            threadLoad.IsBackground = True
            threadLoad.Start()

            date_filter = "today"
            btnAujourdui.BackColor = Color.FromArgb(33, 150, 243)

        End Sub

        Private Sub btnAujourdui_Click(sender As Object, e As EventArgs) Handles btnAujourdui.Click
            date_filter = "today"
            If ckPoint.Checked Or ckEtat.Checked Or ckClient.Checked Then
                LoadFactures(False, False, False, False, False, False, True)
            Else
                LoadFactures(True)
            End If

            RePaintButtons()
            DirectCast(sender, Button).BackColor = Color.FromArgb(33, 150, 243)
        End Sub

        Private Sub Button4_Click(sender As Object, e As EventArgs) Handles btnSemaine.Click
            date_filter = "week"
            If ckPoint.Checked Or ckEtat.Checked Or ckClient.Checked Then
                LoadFactures(False, False, False, False, False, False, True)
            Else
                LoadFactures(False, True)
            End If
            RePaintButtons()
            DirectCast(sender, Button).BackColor = Color.FromArgb(33, 150, 243)
        End Sub

        Private Sub Button7_Click(sender As Object, e As EventArgs) Handles btnAll.Click
            date_filter = "all"
            ckClient.Checked = False
            ckPoint.Checked = False
            ckEtat.Checked = False
            ckNumero.Checked = False

            LoadFactures(False, False, False, False, True)
            RePaintButtons()
            DirectCast(sender, Button).BackColor = Color.FromArgb(33, 150, 243)
        End Sub

        Private Sub Button6_Click(sender As Object, e As EventArgs) Handles btnInterval.Click
            RePaintButtons()
            DirectCast(sender, Button).BackColor = Color.FromArgb(33, 150, 243)
            PanelPeriode.Enabled = True
            date_filter = "interval"
        End Sub


        Sub RePaintButtons()
            For Each c As Control In SidePanel.Controls
                If TypeOf c Is Button Then
                    c.BackColor = SystemColors.ActiveCaption
                End If
            Next
            PanelPeriode.Enabled = False
            ckNumero.Checked = False
        End Sub

        Private Sub btnMois_Click(sender As Object, e As EventArgs) Handles btnMois.Click
            date_filter = "month"
            If ckPoint.Checked Or ckEtat.Checked Or ckClient.Checked Then
                LoadFactures(False, False, False, False, False, False, True)
            Else
                LoadFactures(False, False, True)
            End If
            RePaintButtons()
            DirectCast(sender, Button).BackColor = Color.FromArgb(33, 150, 243)
        End Sub

        Private Sub Responsive(sender As Object, e As EventArgs) Handles MyBase.SizeChanged
            Dim location As New Point
            location.X = (Me.Width - PanelContainer.Width) / 2
            location.Y = (Me.Height - PanelContainer.Height) / 2
            PanelContainer.Location = location
        End Sub

        Private Sub itemActivate(sender As Object, e As EventArgs) Handles ListFactures.ItemActivate
            Selection()
        End Sub

        Sub Selection()
            If ListFactures.SelectedItems.Count > 0 Then
                Dim form As New DetailsTicket
                form.fac_id = ListFactures.SelectedItems(0).SubItems(8).Text
                form.labelTitre.Text = "Facture No: " & ListFactures.SelectedItems(0).Text
                form.Label1.Text = ListFactures.SelectedItems(0).SubItems(2).Text
                form.txtState.Text = ListFactures.SelectedItems(0).SubItems(6).Text

                Try
                    form.date_facture = CType(ListFactures.SelectedItems(0).SubItems(1).Text, DateTime)
                Catch ex As Exception
                    form.date_facture = Today
                End Try
                form.ShowDialog()
                If form.changed Then
                    LoadFactures(True)
                End If
            End If
        End Sub

        Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
            Close()
        End Sub

        Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
            Selection()
        End Sub

        Private Sub txtDateAu_ValueChanged(sender As Object, e As EventArgs) Handles txtDateAu.ValueChanged
            txtDateDu.MaxDate = txtDateAu.Value
            If ckPoint.Checked Or ckEtat.Checked Or ckClient.Checked Then
                LoadFactures(False, False, False, False, False, False, True)
            Else
                LoadFactures(False, False, False, True)
            End If
        End Sub

        Private Sub txtDateDu_ValueChanged(sender As Object, e As EventArgs) Handles txtDateDu.ValueChanged
            txtDateAu.MinDate = txtDateDu.Value
            If ckPoint.Checked Or ckEtat.Checked Or ckClient.Checked Then
                LoadFactures(False, False, False, False, False, False, True)
            Else
                LoadFactures(False, False, False, True)
            End If
        End Sub

        Private Sub btnExport_Click(sender As Object, e As EventArgs) Handles btnExport.Click
            If ListFactures.Items.Count > 0 Then
                Dim exporter As New Db.ExcelExporter(ListFactures)
                exporter.export()
            Else
                Util.ShowMessage("Il  n'y a rien a exporter", 1)
            End If
        End Sub

        Private Sub AfficherModifierToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AfficherModifierToolStripMenuItem.Click
            Selection()
        End Sub

        Private Sub btnPaiement_Click(sender As Object, e As EventArgs) Handles btnPaiement.Click
            Paiement()
        End Sub

        Sub Paiement()
            If ListFactures.SelectedIndices.Count > 0 Then
                Dim form As New Paiements
                form.fac_id = ListFactures.SelectedItems(0).SubItems(8).Text
                form.ShowDialog()
                LoadFactures(True)
            End If
        End Sub

        Private Sub txtNumero_TextChanged(sender As Object, e As KeyEventArgs) Handles txtNumero.KeyUp
            LoadFactures(False, False, False, False, False, True)
        End Sub



        Private Sub cmbPoint_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cmbPoint.SelectedIndexChanged
            If IsNumeric(cmbPoint.SelectedValue) Then
                If cmbPoint.SelectedValue <> "0" Then
                    LoadFactures(False, False, False, False, False, False, True)
                End If
            End If
        End Sub

        Private Sub ckPoint_CheckedChanged(sender As Object, e As EventArgs) Handles ckPoint.CheckedChanged
            cmbPoint.Enabled = ckPoint.Checked
            If ckPoint.Checked Then
                ckNumero.Checked = False
            Else
                cmbPoint.Text = ""
            End If
        End Sub

        Private Sub ckNumero_CheckedChanged(sender As Object, e As EventArgs) Handles ckNumero.CheckedChanged
            If ckNumero.Checked Then
                txtNumero.Enabled = True
                ckPoint.Checked = False
                ckEtat.Checked = False
                ckClient.Checked = False
            Else
                txtNumero.Text = ""
                txtNumero.Enabled = False
            End If
        End Sub

        Function GetHeaderFilter() As String
            Dim f As String = ""

            If ckPoint.Checked And Not ckEtat.Checked And Not ckClient.Checked Then
                f = "point"
            ElseIf Not ckPoint.Checked And Not ckClient.Checked And ckEtat.Checked Then
                f = "etat"
            ElseIf Not ckPoint.Checked And ckClient.Checked And Not ckEtat.Checked Then
                f = "client"
            ElseIf ckPoint.Checked And ckEtat.Checked And Not ckClient.Checked Then
                f = "point|etat"
            ElseIf ckPoint.Checked And ckClient.Checked And Not ckEtat.Checked Then
                f = "point|client"
            ElseIf Not ckPoint.Checked And ckClient.Checked And ckEtat.Checked Then
                f = "etat|client"
            ElseIf ckPoint.Checked And ckClient.Checked And ckEtat.Checked Then
                f = "point|etat|client"
            Else
                f = ""
            End If

            Return f
        End Function

        Private Sub cmbEtat_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cmbEtat.SelectedIndexChanged
            If cmbEtat.Text = "Annuler" Then
                etat_facture = "0"
            ElseIf cmbEtat.Text = "En attente" Then
                etat_facture = "1"
            ElseIf cmbEtat.Text = "Solder" Then
                etat_facture = "2"
            ElseIf cmbEtat.Text = "En cours de paiement" Then
                etat_facture = "3"
            ElseIf cmbEtat.Text = "Non paye" Then
                etat_facture = "4"
            Else
                etat_facture = "2"
            End If

            If cmbEtat.Text.Length > 0 Then
                LoadFactures(False, False, False, False, False, False, True)
            End If
        End Sub

        Private Sub ckEtat_CheckedChanged(sender As Object, e As EventArgs) Handles ckEtat.CheckedChanged
            cmbEtat.Enabled = ckEtat.Checked
            If ckEtat.Checked Then
                ckNumero.Checked = False
            Else
                cmbEtat.Text = ""
            End If
        End Sub

        Private Sub ckClient_CheckedChanged(sender As Object, e As EventArgs) Handles ckClient.CheckedChanged
            cmbClient.Enabled = ckClient.Checked
            btnFusion.Visible = ckClient.Checked
            If ckClient.Checked Then
                ckNumero.Checked = False
            Else
                cmbClient.Text = ""
            End If
        End Sub

        Private Sub cmbClient_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cmbClient.SelectedIndexChanged
            If IsNumeric(cmbClient.SelectedValue) Then
                If cmbClient.SelectedValue <> "0" Then
                    LoadFactures(False, False, False, False, False, False, True)
                End If
            End If
        End Sub



        Private Sub ListFactures_ItemSelectionChanged(sender As Object, e As ListViewItemSelectionChangedEventArgs) Handles ListFactures.ItemSelectionChanged
            If e.IsSelected Then
                If ListFactures.SelectedItems(0).SubItems(3).Text <> "Anonyme" Then
                    btnPaiement.Visible = True
                    PaiementStrip.Enabled = True
                Else
                    btnPaiement.Visible = False
                    PaiementStrip.Enabled = False
                End If
            Else
                btnPaiement.Visible = False
                PaiementStrip.Enabled = False
            End If
        End Sub


        Sub Fusion(ByVal p_id As String)

            Try
                Dim sql = "SELECT id FROM factures WHERE status = '4' AND client_id = '" & cmbClient.SelectedValue & "' AND point_vente_id = " & p_id
                Database.Query(sql)
                Dim result = Database.ExecuteResult
                Dim needFusion = result.HasRows

                If Not needFusion Then
                    result.Close()
                    Database.CloseConnection()
                    Exit Sub
                End If


                Dim facture_id = ""
                Dim list_facture As String = ""
                Dim first As Boolean = True

                While result.Read
                    If first Then
                        facture_id = result.GetValue(0).ToString
                        first = False
                    End If
                    list_facture &= result.GetValue(0).ToString & ","
                End While
                result.Close()


                list_facture = list_facture.Remove(list_facture.LastIndexOf(","))

                sql = "UPDATE ligne_factures SET facture_id = '" & facture_id & "'  WHERE facture_id IN (" & list_facture & ") "

                Database.Query(sql)
                Database.Execute()
                Database.CloseConnection()

                GC.Collect()
            Catch ex As Exception
                If My.Settings.app_debug_mode Then
                    Util.ShowMessage(ex.ToString, 2)
                Else
                    Util.ShowMessage("Une erreur est survenu, veuillez ressayer plus tard", 2)
                End If
            Finally
                Database.CloseConnection()
            End Try
        End Sub


        Private Sub btnFusion_Click(sender As Object, e As EventArgs) Handles btnFusion.Click

            If Not IsNumeric(cmbClient.SelectedValue) OrElse cmbClient.SelectedValue = 0 Then
                Util.ShowMessage("Veuillez selectionner un client", 2)
                Exit Sub
            End If

            Dim msg = "Voulez-vous vraiment fusionner les factures impayés de ce client pour en faire qu’une ? " & vbCrLf & "note: Cette action est irréversible"
            Beep()
            If MsgBox(msg, MsgBoxStyle.YesNo) = MsgBoxResult.Yes Then
                Fusion(1)
                Fusion(2)
                Util.ShowMessage("Opération terminé avec succès")
                LoadFactures(False, False, False, False, False, False, True)
            End If
        End Sub
    End Class
End Namespace