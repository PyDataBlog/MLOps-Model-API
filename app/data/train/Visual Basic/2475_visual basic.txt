Imports System.Data.OleDb
Public Class pelanggan
    Dim id, nama, alamat, no As String
    Dim masa, waktu As Date
    Private Sub Kosongkan()
        cmbidpel.Text = ""
        txtNama.Text = ""
        txtalamat.Text = ""
        txtno.Text = ""
        cmbidpel.Focus()
    End Sub
    Sub tampilidpel()
        cmd = New OleDbCommand("Select * From Pelanggan", Conn)
        rd = cmd.ExecuteReader
        While rd.Read
            cmbidpel.Items.Add(rd.GetString(0))
        End While
    End Sub
    Private Sub pelanggan_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        koneksi()
        tampilidpel()
        waktu = Date.Today
        masa = DateAdd(DateInterval.Year, 1, waktu)
        Label1.Text = waktu
    End Sub

    Private Sub btntambah_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btntambah.Click
        id = cmbidpel.Text
        nama = txtnama.Text
        alamat = txtalamat.Text
        no = txtno.Text
        cmbidpel.Items.Add(cmbidpel.Text)
        If id = "" Or nama = "" Or alamat = "" Or no = "" Then
            MsgBox("Data belum lengkap")
            Exit Sub
        Else
            cmd = New OleDbCommand("select * from Pelanggan where ID_Pelanggan = '" & id & "'", Conn)
            rd = cmd.ExecuteReader
            rd.Read()
            If Not rd.HasRows Then
                Dim tambah As String
                tambah = "Insert into Pelanggan(ID_Pelanggan, Nama, Alamat, No_telp, Masa_Aktif)" & " VALUES ('" & id & "', '" & nama & "', '" & alamat & "','" & no & "', '" & masa & "')"
                cmd = New OleDbCommand(tambah, Conn)
                cmd.ExecuteNonQuery()
                MsgBox("Data berhasil ditambah")
                Call Kosongkan()
            Else
                Call Kosongkan()
                MsgBox("Data Gagal Ditambah")
            End If


        End If
    End Sub

    Private Sub btnedit_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnedit.Click
        id = cmbidpel.Text
        nama = txtnama.Text
        alamat = txtalamat.Text
        no = txtno.Text
        If id = "" Or nama = "" Or alamat = "" Or no = "" Then
            MsgBox("Data belum lengkap")
            Exit Sub
        Else
            cmd = New OleDbCommand("Select * from Pelanggan where ID_Pelanggan = '" & id & "'", Conn)
            rd = cmd.ExecuteReader
            rd.Read()
            If rd.HasRows Then
                Dim editNm As String
                editNm = "Update Pelanggan set Nama = '" & Me.nama & "' where [ID_Pelanggan] = '" & Me.id & "'"

                cmd = New OleDbCommand(editNm, Conn)
                cmd.ExecuteNonQuery()
                Dim edital As String
                edital = "Update Pelanggan set Alamat = '" & Me.alamat & "' where [ID_Pelanggan] = '" & Me.id & "'"
                cmd = New OleDbCommand(edital, Conn)
                cmd.ExecuteNonQuery()
                Dim editno As String
                editno = "Update Pelanggan set No_telp = '" & Me.no & "' where [ID_Pelanggan] = '" & Me.id & "'"
                cmd = New OleDbCommand(editno, Conn)
                cmd.ExecuteNonQuery()
                MsgBox("Data Berhasil di edit")
                Call Kosongkan()
            Else
                MsgBox("Data gagal di edit")

            End If

        End If

    End Sub

    Private Sub btnhapus_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnhapus.Click
        id = cmbidpel.Text
        cmbidpel.Items.Remove(cmbidpel.Text)
        If id = "" Then
            MsgBox("Isi ID_Pelanggan terlebih dahulu")
            cmbidpel.Focus()
            Exit Sub
        Else
            If MessageBox.Show("Yakin akan menghapus data?", "", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
                cmd = New OleDbCommand("Delete * from Pelanggan where ID_Pelanggan = '" & id & "'", Conn)
                cmd.ExecuteNonQuery()
            Else
                MsgBox("Data Tidak Jadi di Hapus")
            End If
            Call Kosongkan()
        End If
    End Sub

    Private Sub cmbidpel_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles cmbidpel.KeyPress
        txtnama.Clear()
        txtalamat.Clear()
        txtno.Clear()
    End Sub

    Private Sub cmbidpel_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmbidpel.SelectedIndexChanged
        id = cmbidpel.Text
        Try
            cmd = New OleDbCommand("Select * from Pelanggan where ID_Pelanggan= '" & id & "'", Conn)
            rd = cmd.ExecuteReader
            rd.Read()
            If rd.HasRows = True Then
                txtnama.Text = rd.GetString(1)
                txtalamat.Text = rd.GetValue(2)
                txtno.Text = rd.GetValue(3)
                txtnama.Focus()
            Else
                Call Kosongkan()
                txtnama.Focus()
            End If
        Catch ex As Exception
        End Try
        
    End Sub
End Class