Public Class ÖhmischerRechner
    Dim Ergebnis As String
    Dim Spannung As Single
    Dim Stromstärke As Single
    Dim Widerstand As Single

    Private Sub cmd_Widerstand_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmd_Widerstand.Click
        cmd_Widerstand.Visible = False
        cmd_Spannung.Visible = False
        cmd_Stromstärke.Visible = False
        SpannungTEXT.Visible = True
        StromstärkeTEXT.Visible = True
        TextBox1.Visible = True
        txt_Spannung.Visible = True
        txt_Stromstärke.Visible = True
        txt_Ergebnis.Visible = True
        cmd_Teilen1.Visible = True
        cmd_Hauptmenü.Visible = True
    End Sub

    Private Sub cmd_Spannung_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmd_Spannung.Click
        cmd_Spannung.Visible = False
        cmd_Stromstärke.Visible = False
        cmd_Widerstand.Visible = False
        WiderstandTEXT.Visible = True
        StromstärkeTEXT.Visible = True
        TextBox2.Visible = True
        txt_Stromstärke.Visible = True
        txt_Widerstand.Visible = True
        txt_Ergebnis.Visible = True
        cmd_Mal.Visible = True
        cmd_Hauptmenü.Visible = True
    End Sub

    Private Sub cmd_Stromstärke_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmd_Stromstärke.Click
        cmd_Stromstärke.Visible = False
        cmd_Spannung.Visible = False
        cmd_Widerstand.Visible = False
        WiderstandTEXT.Visible = True
        SpannungTEXT.Visible = True
        TextBox3.Visible = True
        txt_Spannung.Visible = True
        txt_Widerstand.Visible = True
        txt_Ergebnis.Visible = True
        cmd_Teilen2.Visible = True
        cmd_Hauptmenü.Visible = True
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        cmd_Hauptmenü.Visible = False
        cmd_Clear.Visible = False
        txt_Ergebnis.Visible = False
        txt_Spannung.Visible = False
        txt_Stromstärke.Visible = False
        txt_Widerstand.Visible = False
        SpannungTEXT.Visible = True
        StromstärkeTEXT.Visible = True
        WiderstandTEXT.Visible = True
        cmd_Mal.Visible = False
        cmd_Teilen1.Visible = False
        cmd_Teilen2.Visible = False
        WiderstandTEXT.Visible = False
        SpannungTEXT.Visible = False
        StromstärkeTEXT.Visible = False
        TextBox1.Visible = False
        TextBox2.Visible = False
        TextBox3.Visible = False
    End Sub

    Private Sub cmd_Beenden_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmd_Beenden.Click
        End
    End Sub

    Private Sub cmd_Clear_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmd_Clear.Click
        txt_Spannung.Clear()
        txt_Stromstärke.Clear()
        txt_Widerstand.Clear()
        txt_Ergebnis.Clear()
    End Sub

    Private Sub cmd_Hauptmenü_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmd_Hauptmenü.Click
        cmd_Spannung.Visible = True
        cmd_Stromstärke.Visible = True
        cmd_Widerstand.Visible = True
        txt_Ergebnis.Visible = False
        txt_Spannung.Visible = False
        txt_Stromstärke.Visible = False
        txt_Widerstand.Visible = False
        SpannungTEXT.Visible = True
        StromstärkeTEXT.Visible = True
        WiderstandTEXT.Visible = True
        cmd_Mal.Visible = False
        cmd_Teilen1.Visible = False
        cmd_Teilen2.Visible = False
        WiderstandTEXT.Visible = False
        SpannungTEXT.Visible = False
        StromstärkeTEXT.Visible = False
        TextBox1.Visible = False
        TextBox2.Visible = False
        TextBox3.Visible = False

        txt_Ergebnis.Text = ""
        txt_Spannung.Text = ""
        txt_Stromstärke.Text = ""
        txt_Widerstand.Text = ""
    End Sub

    Private Sub cmd_Mal_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmd_Mal.Click
        Stromstärke = txt_Stromstärke.Text
        Widerstand = txt_Widerstand.Text
        Ergebnis = Stromstärke * Widerstand
        txt_Ergebnis.Text = Ergebnis
    End Sub

    Private Sub cmd_Teilen1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmd_Teilen1.Click
        Spannung = txt_Spannung.Text
        Stromstärke = txt_Stromstärke.Text
        Ergebnis = Spannung / Stromstärke
        txt_Ergebnis.Text = Ergebnis
    End Sub

    Private Sub cmd_Teilen2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmd_Teilen2.Click
        Spannung = txt_Spannung.Text
        Widerstand = txt_Widerstand.Text
        Ergebnis = Spannung / Widerstand
        txt_Ergebnis.Text = Ergebnis
    End Sub

End Class