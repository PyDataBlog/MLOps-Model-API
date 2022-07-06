'vindu for blodtapping
Public Class BlodGivning2
    Private Sub BlodGivning2_Closed(sender As Object, e As EventArgs) Handles Me.Closed
        Me.Hide()
    End Sub
    'finner donor blod type
    Private Sub BlodGivning2_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim blodtype As String = find_blodtype(bruker.getPersonnr)
        ComboBox1.Items.Add("O+")
        ComboBox1.Items.Add("O-")
        ComboBox1.Items.Add("A+")
        ComboBox1.Items.Add("A-")
        ComboBox1.Items.Add("B+")
        ComboBox1.Items.Add("B-")
        ComboBox1.Items.Add("AB+")
        ComboBox1.Items.Add("AB-")
        ComboBox1.Text = blodtype
    End Sub
    'sender info til db
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        blod = New Blod_pakke(giver_id, ComboBox1.Text, NumericUpDown1.Value, CheckHiv.Checked, CheckHepatittB.Checked, CheckHepatittC.Checked, TextBox1.Text)
        blod.send()
        Me.Close()
        AnsattBrukerOversikt.Show()
    End Sub
    Private Sub btnLoggUt_Click(sender As Object, e As EventArgs) Handles btnLoggUt.Click
        Main.Show()
        Me.Close()
        Ansatt.Close()
    End Sub
    'navigerings knapper
    Private Sub btnKalender_Click(sender As Object, e As EventArgs) Handles btnKalender.Click
        AnsattKalender.Show()
        Me.Close()
    End Sub

    Private Sub btnBestill_Click(sender As Object, e As EventArgs) Handles btnBestill.Click
        blodbank.Show()
        Me.Close()
    End Sub

    Private Sub btnStatestikk_Click(sender As Object, e As EventArgs) Handles btnStatestikk.Click
        statestikk.Show()
        Me.Close()
    End Sub

    Private Sub btnSjekkBlod_Click(sender As Object, e As EventArgs) Handles btnSjekkBlod.Click
        InnkallingBaserPaaBehov.Show()
        Me.Close()
    End Sub

    Private Sub btnMinSide_Click(sender As Object, e As EventArgs) Handles btnMinSide.Click
        Ansatt.Show()
        Me.Close()
    End Sub

    Private Sub btnTilbake_Click(sender As Object, e As EventArgs) Handles btnTilbake.Click
        BlodGivning.Show()
        Me.Close()
    End Sub
End Class