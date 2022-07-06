Public Class Activation
    Private Sub txtKey_TextChanged(sender As Object, e As EventArgs) Handles txtKey.TextChanged
        If txtKey.Text.Length = 5 Then
            txtKey.Text = txtKey.Text & "-"
        ElseIf txtKey.Text.Length = 11 Then
            txtKey.Text = txtKey.Text & "-"
        ElseIf txtKey.Text.Length = 17 Then
            txtKey.Text = txtKey.Text & "-"
        ElseIf txtKey.Text.Length = 23 Then
            txtKey.Text = txtKey.Text & "-"
        ElseIf txtKey.Text.Length = 29 Then
            btnSubmit.Enabled = True
        Else
            btnSubmit.Enabled = False
        End If
    End Sub

    Private Sub btnSubmit_Click(sender As Object, e As EventArgs) Handles btnSubmit.Click
        If txtKey.Text = "LJTHA-5AYU1-E3S95-A7DUX-FCF0Z" Then
            trayProcess.ShowBalloonTip(99999999, "Az aktiváció sikeres | Virusless AntiVirus ALPHA", "A beírt termékkulcs érvényes! Ezt többször nem kell beírnia, de lehetőleg őrizze meg, mert a frissítések során előfordulhat, hogy szüksége lesz rá!", ToolTipIcon.Info)
            My.Settings.Reload()
            My.Settings.fogiunhV = True
            My.Settings.Save()
            Form1.Show()
            Me.Close()
        ElseIf txtKey.Text = "CS4KN-3MVK1-L0PN1-4K4RJ-43ZT0" Then
            My.Settings.Reload()
            My.Settings.dsmJFV = "1"
            My.Settings.Save()
        Else
            trayProcess.ShowBalloonTip(99999999, "Az aktiváció sikertelen | Virusless AntiVirus ALPHA", "A beírt termékkulcs érvénytelen! Legyen szíves ellenőrizze le, hogy helyesen írta-e be! Kettő tippet a hibaüzenetben olvashat.", ToolTipIcon.Error)
            MsgBox("A beírt termékkulcs érvénytelen! Legyen szíves ellenőrizze le, hogy helyesen írta-e be! TIPP: 1) Amennyiben nincs termékkulcs a CD-hez mellékelve, lépjen kapcsolatba a Virusless ügyfélszolgálattal a http://go.winizsol.ml/vav weboldalon! 2) Minden 5. karakter után egy vessző kerül beillesztésre, de ekkor a kurzor a szövegdoboz elejére kerül. Előfordulhat, hogy ez okozta.", MsgBoxStyle.Critical, "Az aktiváció sikertelen! | Virusless AntiVirus ALPHA")
        End If
    End Sub

    Private Sub Activation_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        trayProcess.Text = "Virusless AntiVirus " + My.Settings.Edition + " " + My.Settings.Version + " b" + My.Settings.Build + " (AKTIVÁLATLAN)"
        If My.Settings.dsmJFV = "1" Then
            MsgBox("Előfordulhat, hogy nem kellett volna kalózkodni...", MsgBoxStyle.Critical, "Egy igazi kalózzal van itt dolgunk | Virusless AntiVirus ALPHA")
            Process.Start("http://go.winizsol.ml/vavinfo")
            Me.Hide()
        End If
        If My.Settings.fogiunhV1 = True Then
            Form1.Show()
            Me.Close()
        End If
    End Sub

    Private Sub lnkGPL_en_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles lnkGPL_en.LinkClicked
        Process.Start("https://www.gnu.org/licenses/old-licenses/gpl-2.0.html")
    End Sub

    Private Sub lnkGPL_hu_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles lnkGPL_hu.LinkClicked
        Process.Start("http://www.drdudas.hu/gpl_v2_magyarul")
    End Sub
End Class
