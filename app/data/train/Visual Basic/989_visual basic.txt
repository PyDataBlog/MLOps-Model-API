Imports System.Data.OleDb

Public Class FPengembalianCucian

    Dim sisa As String = ""

    Public Sub awal()
        txtfaktur.Text = ""
        txtfaktur.Focus()
        txtkons.Text = ""
        txtberat.Text = ""
        txtpakaian.Text = ""
        txtharga.Text = ""
        txtdiskon.Text = ""
        txtpanjar.Text = ""
        cmbket.Text = ""
        rdobukan.Checked = True
        cmbkode.Text = ""
        cmbanggota.Text = ""
        txtbayar.Text = ""
        txtsisa.Text = ""
    End Sub

    Public Sub cekfaktur()
        'cek faktur sesuai dengan inputan, jika ada maka tampilkan semua field2 nya dalam textbox
        Dim cmd As OleDbCommand
        Dim rd As OleDbDataReader
        cmd = New OleDbCommand("SELECT * FROM Penyerahan_Cucian WHERE Faktur ='" & txtfaktur.Text & "'", koneksi)
        rd = cmd.ExecuteReader
        rd.Read()
        If rd.HasRows Then
            txtkons.Text = rd.Item("Konsumen")
            txtberat.Text = rd.Item("Berat_Cucian")
            txtpakaian.Text = rd.Item("Jumlah_Pakaian")
            txtharga.Text = rd.Item("Harga")
            txtdiskon.Text = rd.Item("Diskon")
            txtpanjar.Text = rd.Item("Panjar")
            txtbayar.Text = rd.Item("Total_Bayar")
            txtsisa.Text = rd.Item("Sisa")
            sisa = txtsisa.Text
            cmbket.Text = rd.Item("Keterangan")
            If rd.Item("Diskon") = "15" Then
                txtkons.Text = ""
                cmbanggota.Text = rd.Item("Konsumen")
            Else
                txtkons.Text = rd.Item("Konsumen")
                cmbkode.Text = ""
                cmbanggota.Text = ""
            End If
            If cmbanggota.Text = "" Then
                cmbkode.Text = ""
            Else
                cmd = New OleDbCommand("SELECT * FROM Anggota WHERE Nama_Anggota = '" & cmbanggota.Text & "'", koneksi)
                rd = cmd.ExecuteReader
                rd.Read()
                If rd.HasRows Then
                    cmbkode.Text = rd.Item("Kode_Anggota")
                End If
            End If
        Else
            MsgBox("Faktur Tidak Terdaftar", MsgBoxStyle.Critical)
        End If
    End Sub

    Private Sub txtfaktur_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles txtfaktur.KeyPress
        If e.KeyChar = Chr(13) Then 'jika user menekan enter
            cekfaktur()
        End If
    End Sub

    Private Sub FPengembalianCucian_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        bukakoneksi()
        lbltanggal.Text = Now.Date
    End Sub

    Private Sub cmbket_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmbket.SelectedIndexChanged
        If cmbket.Text = "Lunas" Then
            txtsisa.Text = "0"
            btnsimpan.Enabled = True
        ElseIf cmbket.Text = "Belum Lunas" Then
            txtsisa.Text = sisa
            btnsimpan.Enabled = False
        End If
    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        jam.Text = Format(Now.Hour, "00") & ":" & Format(Now.Minute, "00") & ":" & Format(Now.Second, "00")
    End Sub

    Private Sub btnreset_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnreset.Click
        awal()
    End Sub

    Private Sub btnsimpan_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnsimpan.Click
        Dim kons As String = ""
        If txtkons.Text = "" Then
            kons = cmbanggota.Text
        ElseIf cmbanggota.Text = "" Then
            kons = txtkons.Text
        End If
        'menyimpan data ke dalam database pengembalian cucian
        Dim cmd As OleDbCommand
        cmd = New OleDbCommand("INSERT INTO Pengembalian_Cucian (Faktur, Tanggal_Serah, Tanggal2, Konsumen, Berat_Cucian, Jumlah_Pakaian, Harga, Diskon, Total_Bayar, Panjar, Sisa, Keterangan) " & _
                                 "VALUES ('" & txtfaktur.Text & "', '" & lbltanggal.Text & "', '" & lbltanggal.Text & "', '" & kons & "', '" & txtberat.Text & "', '" & txtpakaian.Text & "', '" & txtharga.Text & "', '" & txtdiskon.Text & "', '" & txtbayar.Text & "', '" & txtpanjar.Text & "', '" & txtsisa.Text & "', '" & cmbket.Text & "')", koneksi)
        cmd.ExecuteNonQuery()

        'menyimpan data ke dalam database pemasukan
        cmd = New OleDbCommand("INSERT INTO Pemasukan (Faktur, Tanggal_Serah, Tanggal2, Konsumen, Harga, Diskon, Bayar) " & _
                               "VALUES ('" & txtfaktur.Text & "', '" & lbltanggal.Text & "', '" & lbltanggal.Text & "', '" & kons & "', '" & txtharga.Text & "', '" & txtdiskon.Text & "', '" & txtbayar.Text & "')", koneksi)
        cmd.ExecuteNonQuery()

        'menghapus data dari penyerahan cucian
        cmd = New OleDbCommand("DELETE * FROM Penyerahan_Cucian WHERE Faktur = '" & txtfaktur.Text & "'", koneksi)
        cmd.ExecuteNonQuery()

        awal()
        FCucian.tampilcucian()
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        FCucian.Show()
    End Sub
End Class