Public Class Form1
    Dim a As Integer, neg As Integer, pos As Integer, nbrz As Integer
    Dim key As String, negposs As Boolean
    Private Sub button1_Click(sender As Object, e As EventArgs) Handles button1.Click

        negpos()

        If textBox1.Text <> "" And negposs = True Then 'si le text est pas vide

            Dim charr As String 'premiere lettre de la textbox
            Dim code As Integer

            For i = 1 To textBox1.Text.Count 'boucle pour chaque char

                charr = Mid(textBox1.Text, 1, 1) 'trouve le premier char
                textBox1.Text = textBox1.Text.Remove(0, 1) 'le supprime

                code = Asc(charr) 'recuper le code ascii

                Dim code1 As Integer, code2 As Integer

                code1 = code - neg 'opere sur le code
                code2 = code + pos

                textBox2.AppendText(Chr(code1) + Chr(code2)) 'affiche le message crypter

            Next

        Else

            MsgBox("La chaine n'est pas decryptable")

        End If

    End Sub

    Private Sub button2_Click(sender As Object, e As EventArgs) Handles button2.Click

        negpos()

        If textBox2.Text.Count >= 2 And negposs = True Then 'verifie que le text est long d'au moin 2 char

            Dim charr As String
            Dim code1 As Integer
            Dim code2 As Integer

            For i = 2 To textBox2.Text.Count 'bouche pour tout les 2 chars

                charr = Mid(textBox2.Text, 1, 2) 'recuper les 2 premier char
                textBox2.Text = textBox2.Text.Remove(0, 2) 'les supprime

                Dim scode1 As String, scode2 As String 'separe les deux char
                scode1 = Mid(charr, 1, 1)
                scode2 = Mid(charr, 2, 1)

                code1 = Asc(scode1) + neg 'effecture l'operation
                code2 = Asc(scode2) - pos

                If code1 = code2 Then 'verifie qu'il se suive bien
                    textBox1.AppendText(Chr(code1)) 'affiche le msg decoder
                Else
                    i = textBox2.Text.Count
                    MsgBox("La chaine n'est pas decryptable")
                End If

                i = i + 1 'ajout 1 a i pour rester dans un nombre pair

            Next

        Else
            MsgBox("La chaine n'est pas decryptable")

        End If

    End Sub

    Private Sub button3_Click(sender As Object, e As EventArgs) Handles button3.Click

        MsgBox("Crypter/decrypter Fortement optimiser" + vbNewLine + "©FormFL inc 2015 Dev Lebecque Florian")

    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click

        TextBox3.Clear() 'active la generation
        Timer1.Start()
        a = 64

      
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick

        If a > 0 Then 'gen de la cle

            Dim nbr As Integer, ran As New Random
            nbr = ran.Next(32, 254)
            TextBox3.AppendText(Chr(nbr))
            a = a - 1
        Else
            Timer1.Stop()

        End If


    End Sub

    Sub negpos()

        If TextBox3.Text <> "" Then

            nbrz = 0

            key = TextBox3.Text

            For i = 1 To key.Count 'recupere la somme du code ascii de tout les charatheres

                nbrz = nbrz + Asc(Mid(key, 1, 1))
                key = key.Remove(0, 1)

            Next

            ' MsgBox(nbrz.ToString)

           
            Dim n As String 'additione les chiffre formant la somme du code ascii

            n = nbrz.ToString

            nbrz = 0

            For b = 1 To n.Count

                nbrz = nbrz + (Mid(n, b, 1))

            Next

            ' MsgBox(nbrz.ToString)



            Dim nbrtxt As String
            nbrtxt = nbrz.ToString

            neg = Mid(nbrtxt, 1, 1) 'definit le decalage
            pos = Mid(nbrtxt, 2, 1)

            negposs = True
        Else
            MsgBox("Mettez une clé !")
            negposs = False
        End If
    End Sub
End Class
