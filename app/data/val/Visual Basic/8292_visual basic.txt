Imports HG.Db.ORM
Imports System.Threading

Namespace Reports
    Public Class JournalCaisse

        Dim table As New Table

        Dim active_query As String = ""

        Private Delegate Sub LoadDatasDeletegate()

        Sub ProcessLoadData()
            If InvokeRequired Then
                BeginInvoke(New LoadDatasDeletegate(AddressOf LoadDatas))
            Else
                LoadDatas()
            End If
        End Sub

        Private Sub Responsive(sender As Object, e As EventArgs) Handles MyBase.SizeChanged
            Dim location As New Point
            location.X = (Me.Width - PanelContainer.Width) / 2
            location.Y = (Me.Height - PanelContainer.Height) / 2
            PanelContainer.Location = location
        End Sub

        Private Sub JournalCaisse_Load(sender As Object, e As EventArgs) Handles MyBase.Load
            Util.setupFrame(Me)
            table.TableName = "transactions"

            Dim threadLoadData As New Thread(New ThreadStart(AddressOf ProcessLoadData))
            threadLoadData.IsBackground = True
            threadLoadData.Start()

            Util.PopulateCombo(cmbPoint, "SELECT id,nom FROM point_ventes", "id", "nom")
            Util.PopulateCombo(cmbUser, "SELECT id, username FROM users", "id", "username")

            labelDu.Text = FormatDateTime(Today, DateFormat.LongDate)
            labelAu.Text = FormatDateTime(Today, DateFormat.LongDate)
            cmbDu.MaxDate = Today
            cmbAu.MaxDate = Today
            ProgressBar1.Style = ProgressBarStyle.Marquee
        End Sub

        Dim total As Double = 0
        Dim totalRectte As Double = 0
        Dim totalDepense As Double = 0

        Sub LoadDatas(Optional byAll As Boolean = False, Optional byToday As Boolean = False, Optional byNature As Boolean = False, Optional byPoint As Boolean = False, Optional byDate As Boolean = False, Optional byPoinNature As Boolean = False, Optional byUser As Boolean = False, Optional ByVal byPointUser As Boolean = False)

            Dim fond_caisse As Double = 0
            labelDu.Text = FormatDateTime(cmbDu.Value, DateFormat.LongDate)
            labelAu.Text = FormatDateTime(cmbAu.Value, DateFormat.LongDate)


            Try

                Dim transactions As New List(Of Entity)

                If byNature Then

                    If cmbNature.Text.StartsWith("MOUVEMENT") Then
                        active_query = table.Find.Fields("origine", "designation", "created", "username", "nature", "montant").HaveJoin("users").Where("transactions.external", 1).Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").GetSqlQuery

                        transactions = table.Find.Fields("transactions.id", "origine", "transactions.designation", "transactions.created", "users.username", "transactions.nature", "transactions.montant").HaveJoin("users").Where("transactions.external", 1).Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").All
                    Else

                        active_query = table.Find.Fields("origine", "designation", "created", "username", "nature", "montant").HaveJoin("users").Where("transactions.designation LIKE '" & Util.Escape(cmbNature.Text) & "%'").Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").GetSqlQuery

                        transactions = table.Find.Fields("transactions.id", "origine", "transactions.designation", "transactions.created", "users.username", "transactions.nature", "transactions.montant").HaveJoin("users").Where("transactions.designation LIKE '" & Util.Escape(cmbNature.Text) & "%'").Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").All
                    End If

                ElseIf byPoinNature Then


                    If cmbNature.Text.StartsWith("MOUVEMENT") Then
                        active_query = table.Find.Fields("origine", "designation", "created", "username", "nature", "montant").HaveJoin("users").Where("transactions.designation LIKE '" & Util.Escape(cmbNature.Text) & "%'").AndWhere("transactions.origine", cmbPoint.Text).Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").GetSqlQuery

                        transactions = table.Find.Fields("transactions.id", "origine", "transactions.designation", "transactions.created", "users.username", "transactions.nature", "transactions.montant").HaveJoin("users").Where("transactions.external", 1).AndWhere("transactions.origine", cmbPoint.Text).Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").All
                    Else
                        active_query = table.Find.Fields("origine", "designation", "created", "username", "nature", "montant").HaveJoin("users").Where("transactions.external", 1).AndWhere("transactions.origine", cmbPoint.Text).Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").GetSqlQuery



                        transactions = table.Find.Fields("transactions.id", "origine", "transactions.designation", "transactions.created", "users.username", "transactions.nature", "transactions.montant").HaveJoin("users").Where("transactions.designation LIKE '" & Util.Escape(cmbNature.Text) & "%'").Where("transactions.origine", cmbPoint.Text).Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").All
                    End If
                ElseIf byPoint Then

                    active_query = table.Find.Fields("origine", "designation", "created", "username", "nature", "montant").HaveJoin("users").Where("transactions.origine", cmbPoint.Text).Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").GetSqlQuery

                    transactions = table.Find.Fields("transactions.id", "origine", "transactions.designation", "transactions.created", "users.username", "transactions.nature", "transactions.montant").HaveJoin("users").Where("transactions.origine", cmbPoint.Text).Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").All

                ElseIf byDate Then
                    active_query = table.Find.Fields("origine", "designation", "created", "username", "nature", "montant").HaveJoin("users").Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").GetSqlQuery

                    transactions = table.Find.Fields("transactions.id", "origine", "transactions.designation", "transactions.created", "users.username", "transactions.nature", "transactions.montant").HaveJoin("users").Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").All


                ElseIf byToday Then

                    active_query = table.Find.Fields("origine", "designation", "created", "username", "nature", "montant").HaveJoin("users").Where("DATE(transactions.created) = DATE(NOW())").OrderBY("transactions.created DESC").GetSqlQuery

                    transactions = table.Find.Fields("transactions.id", "origine", "transactions.designation", "transactions.created", "users.username", "transactions.nature", "transactions.montant").HaveJoin("users").Where("DATE(transactions.created) = DATE(NOW())").OrderBY("transactions.created DESC").All

                ElseIf byAll Then

                    active_query = table.Find.Fields("origine", "designation", "created", "username", "nature", "montant").HaveJoin("users").OrderBY("transactions.created DESC").GetSqlQuery

                    transactions = table.Find.Fields("transactions.id", "origine", "transactions.designation", "transactions.created", "users.username", "transactions.nature", "transactions.montant").HaveJoin("users").OrderBY("transactions.created DESC").All


                    labelDu.Text = "----"
                    labelAu.Text = "----"

                ElseIf byUser Then
                    active_query = table.Find.Fields("origine", "designation", "created", "username", "nature", "montant").HaveJoin("users").Where("transactions.user_id", cmbUser.SelectedValue).Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").GetSqlQuery

                    transactions = table.Find.Fields("transactions.id", "origine", "transactions.designation", "transactions.created", "users.username", "transactions.nature", "transactions.montant").HaveJoin("users").Where("transactions.user_id", cmbUser.SelectedValue).Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").All

                ElseIf byPointUser Then

                    active_query = table.Find.Fields("origine", "designation", "created", "username", "nature", "montant").HaveJoin("users").Where("transactions.user_id", cmbUser.SelectedValue).Where("transactions.user_id", cmbUser.SelectedValue).Where("transactions.origine", cmbPoint.Text).Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").GetSqlQuery

                    transactions = table.Find.Fields("transactions.id", "origine", "transactions.designation", "transactions.created", "users.username", "transactions.nature", "transactions.montant").HaveJoin("users").Where("transactions.user_id", cmbUser.SelectedValue).Where("transactions.origine", cmbPoint.Text).Where("DATE(transactions.created) BETWEEN '" & Util.GetDateToMysqlFormat(cmbDu) & "' AND '" & Util.GetDateToMysqlFormat(cmbAu) & "'").OrderBY("transactions.created DESC").All

                Else
                    active_query = table.Find.Fields("origine", "designation", "created", "username", "nature", "montant").HaveJoin("users").Where("DATE(transactions.created) = DATE(NOW())").Limit(50).OrderBY("transactions.created DESC").GetSqlQuery

                    transactions = table.Find.Fields("transactions.id", "origine", "transactions.designation", "transactions.created", "users.username", "transactions.nature", "transactions.montant").HaveJoin("users").Where("DATE(transactions.created) = DATE(NOW())").OrderBY("transactions.created DESC").All

                End If

                ListJournal.Items.Clear()
                total = 0
                totalDepense = 0
                totalRectte = 0

                Dim iter As Integer = 1
                PanelLoader.Visible = True

                btnAujourdui.Enabled = False
                btnRecherche.Enabled = False
                btnTout.Enabled = False

                For Each transaction As Entity In transactions
                    Dim item As New ListViewItem(CStr(transaction.GetProperty("id")))
                    item.SubItems.Add(transaction.GetProperty("origine"))
                    item.SubItems.Add(transaction.GetProperty("designation"))
                    item.SubItems.Add(transaction.GetProperty("created"))
                    item.SubItems.Add(transaction.GetProperty("username"))
                    Dim nature = transaction.GetProperty("nature").ToString

                    If nature.ToLower.Equals("recette") Then
                        item.SubItems.Add(transaction.GetProperty("montant") & " " & My.Settings.unite_monetaire)
                        item.SubItems.Add("0 " & My.Settings.unite_monetaire)
                        Try
                            totalRectte += CDbl(transaction.GetProperty("montant"))
                        Catch ex As Exception
                            totalRectte += 0
                        End Try
                    Else
                        item.SubItems.Add("0 " & My.Settings.unite_monetaire)
                        item.SubItems.Add(transaction.GetProperty("montant") & " " & My.Settings.unite_monetaire)
                        Try
                            totalDepense += CDbl(transaction.GetProperty("montant"))
                        Catch ex As Exception
                            totalDepense += 0
                        End Try
                    End If

                    Dim desig As String = DirectCast(transaction.GetProperty("designation"), String)
                    If desig.ToLower.StartsWith("facture") OrElse desig.ToLower.StartsWith("paiement") Then
                        Try
                            fond_caisse += CDbl(transaction.GetProperty("montant"))
                        Catch ex As Exception
                            fond_caisse += 0
                        End Try
                    ElseIf desig.ToLower.StartsWith("annulation") Then
                        Try
                            fond_caisse -= CDbl(transaction.GetProperty("montant"))
                        Catch ex As Exception
                            fond_caisse -= 0
                        End Try
                    End If


                    If iter = 4 Then
                        item.BackColor = Color.AliceBlue
                        iter = 0
                    End If

                    ListJournal.Items.Add(item)
                    iter += 1
                    IncrementProgress()
                Next

                total = totalRectte - totalDepense
                labelRecette.Text = totalRectte & " " & My.Settings.unite_monetaire
                labelDepense.Text = totalDepense & " " & My.Settings.unite_monetaire
                labelToday.Text = total & " " & My.Settings.unite_monetaire

                PanelLoader.Visible = False
                btnAujourdui.Enabled = True
                btnRecherche.Enabled = True
                btnTout.Enabled = True

                labelFondCaisse.Text = fond_caisse & " " & My.Settings.unite_monetaire

                GC.Collect()
            Catch ex As Exception
                If My.Settings.app_debug_mode Then
                    Util.ShowMessage(ex.ToString)
                Else
                    Util.ShowMessage("Une erreur est survue, veuillez réessayez plus tard" & vbCrLf & "note : si le problème persiste, contactez l'administrateur", 2)
                End If
            Finally
                Database.CloseConnection()
            End Try
        End Sub

        Private Sub btnClose_Click(sender As Object, e As EventArgs) Handles btnClose.Click
            Close()
        End Sub

        Private Sub ckNature_CheckedChanged(sender As Object, e As EventArgs) Handles ckNature.CheckedChanged
            labelNature.Enabled = Not labelNature.Enabled
            cmbNature.Enabled = Not cmbNature.Enabled
            If ckNature.Checked = False Then
                cmbNature.Text = ""
            Else
                ckUser.Checked = False
            End If

        End Sub

        Private Sub ckPoint_CheckedChanged(sender As Object, e As EventArgs) Handles ckPoint.CheckedChanged
            labelPoint.Enabled = Not labelPoint.Enabled
            cmbPoint.Enabled = Not cmbPoint.Enabled
            If ckPoint.Checked = False Then
                cmbPoint.Text = ""
            End If


        End Sub


        Private Sub btnAujourdui_Click(sender As Object, e As EventArgs) Handles btnAujourdui.Click
            LoadDatas(False, True)
            ckNature.Checked = False
            ckPoint.Checked = False
            cmbDu.Value = Today
            cmbAu.Value = Today
        End Sub

        Private Sub btnTout_Click(sender As Object, e As EventArgs) Handles btnTout.Click
            LoadDatas(True)
            ckNature.Checked = False
            ckPoint.Checked = False
            cmbDu.Value = Today
            cmbAu.Value = Today
        End Sub

        Private Sub btnRecherche_Click(sender As Object, e As EventArgs) Handles btnRecherche.Click
            If ckNature.Checked And ckPoint.Checked = False Then
                If cmbNature.Text.Length = 0 Then
                    Util.ShowMessage("Veuillez sélectionner la nature de la transaction", 1)
                    Exit Sub
                End If
                LoadDatas(False, False, True)
            ElseIf ckPoint.Checked And ckNature.Checked = False And ckUser.Checked = False Then
                If cmbPoint.Text.Length = 0 Then
                    Util.ShowMessage("Veuillez sélectionner le point de vente", 1)
                    Exit Sub
                End If
                LoadDatas(False, False, False, True)
            ElseIf ckNature.Checked = False And ckPoint.Checked = False And ckUser.Checked = False Then
                LoadDatas(False, False, False, False, True)
            ElseIf ckNature.Checked And ckPoint.Checked Then
                If cmbPoint.Text.Length = 0 Or cmbNature.Text.Length = 0 Then
                    Util.ShowMessage("Veuillez specifier la nature et le point de vente", 1)
                    Exit Sub
                End If
                LoadDatas(False, False, False, False, False, True)
            ElseIf ckUser.Checked And Not ckPoint.Checked Then
                LoadDatas(False, False, False, False, False, False, True)
            ElseIf ckUser.Checked And ckPoint.Checked Then
                LoadDatas(False, False, False, False, False, False, False, True)

            End If
        End Sub

        Private Sub cmbAu_ValueChanged(sender As Object, e As EventArgs) Handles cmbAu.ValueChanged
            cmbDu.MaxDate = cmbAu.Value
            cmbAu.MinDate = cmbDu.Value
        End Sub

        Private Sub cmbDu_ValueChanged(sender As Object, e As EventArgs) Handles cmbDu.ValueChanged
            cmbDu.MaxDate = cmbAu.Value
            cmbAu.MinDate = cmbDu.Value
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

        Private Sub ckUser_CheckedChanged(sender As Object, e As EventArgs) Handles ckUser.CheckedChanged
            labelUser.Enabled = Not labelUser.Enabled
            cmbUser.Enabled = Not cmbUser.Enabled
            If ckUser.Checked = False Then
                cmbUser.Text = ""
            Else
                ckNature.Checked = False
            End If
        End Sub



        Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
            Dim form As New ReportJournalCaisse
            form.query = active_query
            form.total_recette = labelRecette.Text
            form.total_depense = labelDepense.Text
            form.titre = "JOURNAL DE CAISSE DU : " & labelDu.Text & " AU : " & labelAu.Text

            form.ShowDialog()
        End Sub
    End Class
End Namespace