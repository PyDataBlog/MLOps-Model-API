Public Class ListeOrigineTransactions

    Sub LoadOrigines()
        Try
            Dim cmd = Database.GetCommand
            cmd.CommandText = "SELECT id, designation, description, type_origine FROM origine_transactions"
            Dim result = cmd.ExecuteReader

            ListOrigine.Items.Clear()

            While result.Read
                Dim item As New ListViewItem(result.GetValue(0).ToString)
                item.SubItems.Add(result.GetValue(1).ToString)
                item.SubItems.Add(result.GetValue(2).ToString)
                If result.GetValue(3).ToString = "1" Then
                    item.SubItems.Add("RECETTE")
                Else
                    item.SubItems.Add("DEPENSE")
                End If

                ListOrigine.Items.Add(item)
            End While


            result.Close()
            cmd.Dispose()
            cmd.Connection.Close()
        Catch ex As Exception
            If My.Settings.app_debug_mode Then
                Util.ShowMessage(ex.ToString, 2)
            Else
                Util.ShowMessage("Echec de connexion à la base de données", 2)
            End If
        End Try
    End Sub


    Private Sub btnNouveau_Click(sender As Object, e As EventArgs) Handles btnNouveau.Click
        Dim form As New OrigineTransactions
        form.ShowDialog()
        LoadOrigines()
    End Sub

    Private Sub btnEdit_Click(sender As Object, e As EventArgs) Handles btnEdit.Click
        If ListOrigine.SelectedItems.Count = 0 Then
            Util.ShowMessage("Selectionner un element avant de continuer", 1)
            Exit Sub
        End If
        Dim form As New OrigineTransactions
        form.id = ListOrigine.SelectedItems(0).Text
        form.ShowDialog()
        LoadOrigines()
    End Sub

    Private Sub ListeOrigineTransactions_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Util.setupFrame(Me)
        LoadOrigines()
    End Sub
End Class