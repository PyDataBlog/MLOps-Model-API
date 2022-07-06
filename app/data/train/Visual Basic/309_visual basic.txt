Public Class frm_IPAufloesen

    Private DNS As New cls_DNS


#Region "Formular Events"

    Private Sub frm_IPAufloesen_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Me.txt_Eingabe.Text = "web.de"
    End Sub

    Private Sub frm_IPAufloesen_FormClosed(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
        DNS = Nothing
    End Sub

#End Region

#Region "Formular Buttons"

    Private Sub btn_Suchen_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btn_Suchen.Click
        Me.txt_Ergebnis.Text = String.Empty

        If IsNumeric(Me.txt_Eingabe.Text) Then
            Me.txt_Ergebnis.Text = DNS.IP2DNS(Me.txt_Eingabe.Text)

        Else
            Me.txt_Ergebnis.Text = DNS.DNS2IP(Me.txt_Eingabe.Text)

        End If
    End Sub

    Private Sub btn_Exit_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btn_Exit.Click
        Me.Close()
    End Sub

#End Region

End Class