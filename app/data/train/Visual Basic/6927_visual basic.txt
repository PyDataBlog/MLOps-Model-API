Public Class Kebutuhan

    Private Sub LihatDataToolStripMenuItem1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles LihatDataToolStripMenuItem1.Click
        Laporan.Show()
        Me.Hide()

    End Sub

    Private Sub BerandaToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BerandaToolStripMenuItem.Click
        Beranda.Show()
        Me.Hide()
    End Sub

    Private Sub KeluarToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KeluarToolStripMenuItem.Click
        Login.Show()
        Me.Close()

    End Sub

    Private Sub DataToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DataToolStripMenuItem.Click
        Produk.Show()
        Me.Hide()

    End Sub

    Private Sub TambahDataToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TambahDataToolStripMenuItem.Click
        Produk.Show()
        Me.Hide()
        Tambah_produk.ShowDialog()
        'Me.Close()

    End Sub

    Private Sub TambahDataToolStripMenuItem1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TambahDataToolStripMenuItem1.Click
        Tambah_kebutuhan.ShowDialog()
        'Me.Hide()
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Tambah_kebutuhan.ShowDialog()
        'Me.Hide()
    End Sub

    
    Private Sub Kebutuhan_Shown(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Shown
        Try
            dataload("SELECT id_kebutuhan, nama_kebutuhan, harga_kebutuhan, satuan, jenis, listrik, watt, umur_pemakaian  FROM kebutuhan")
            filltable(DataKebutuhan)
            'DataKebutuhan.Columns(0).Visible = False
            DataKebutuhan.ClearSelection()
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Ubah_Kebutuhan.id_kebutuhan.Text = CType(DataKebutuhan.CurrentRow.Cells(1).Value, String)
        Ubah_Kebutuhan.nama_kebutuhan.Text = CType(DataKebutuhan.CurrentRow.Cells(2).Value, String)
        Ubah_Kebutuhan.harga_kebutuhan.Text = CType(DataKebutuhan.CurrentRow.Cells(3).Value, String)
        Ubah_Kebutuhan.satuan.Text = CType(DataKebutuhan.CurrentRow.Cells(4).Value, String)
        Ubah_Kebutuhan.Jenis.Text = CType(DataKebutuhan.CurrentRow.Cells(5).Value, String)
        Ubah_Kebutuhan.Listrik.Text = CType(DataKebutuhan.CurrentRow.Cells(6).Value, String)
        Ubah_Kebutuhan.Watt.Text = CType(DataKebutuhan.CurrentRow.Cells(7).Value, String)
        Ubah_Kebutuhan.Umur_pemakaian.Text = CType(DataKebutuhan.CurrentRow.Cells(8).Value, String)

        Ubah_Kebutuhan.ShowDialog()
        'Me.Hide()
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        Try
            delete("DELETE FROM kebutuhan WHERE id_kebutuhan = '" + CType(DataKebutuhan.CurrentRow.Cells(1).Value, String) + "'")
            dataload("SELECT id_kebutuhan, nama_kebutuhan, harga_kebutuhan, satuan, jenis, listrik, watt, umur_pemakaian FROM kebutuhan")
            filltable(DataKebutuhan)
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try

    End Sub
End Class