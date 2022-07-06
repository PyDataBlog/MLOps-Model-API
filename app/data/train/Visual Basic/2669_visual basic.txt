Public Class Tambah_produk

    Private Sub Button_Cancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button_Cancel.Click
        Produk.Show()
        Me.Close()

    End Sub

    Private Sub Button_Next_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button_Next.Click
        Try
            create("INSERT INTO produk (nama_produk) VALUES ('" + NamaProduk.Text + "')")
            Kebutuhan_produk.Show()
            Produk.Hide()
            Me.Close()

        Catch ex As Exception
            MsgBox(ex.Message)
        End Try
    End Sub
End Class