Public Class Form1
    Dim save, Vloss, FoAWG, Vs, Rwire, I, Pload, Vload, TB1C, TB2C, TB3C, TB4C, TB5C As Decimal

    Private Sub Button9_Click(sender As Object, e As EventArgs) Handles Button9.Click
        If Not ListBox1.SelectedIndex = -1 Then
            ListBox1.Items.Remove(ListBox1.SelectedItem)
        Else
            MsgBox("Please select an itom from the listbox on the left")
        End If
    End Sub

    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        'reset button for user entered values
        Label1.Visible = False
        Label2.Text = "Rwire"
        Label3.Text = "Amps"
        Label4.Text = "Pload"
        Label5.Text = "Vload"
        Label6.Text = "Vs"
        ListBox1.SelectedIndex = -1
        TextBox3.Text = "Vs"
        TextBox4.Text = "Pload"
        TextBox5.Text = "Ft of AWG"
        TextBox2.Text = "ENTER:  ""Name"""
        TextBox1.Text = "ENTER: Ohms/1000ft"
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        'load button
        If save = True Then ' if the user have saved before
            Dim sr As IO.StreamReader = IO.File.OpenText("AWG1.TXT")
            ListBox1.Items.Clear()
            Do Until sr.EndOfStream
                ListBox1.Items.Add(sr.ReadLine)
            Loop
            sr.Close()

        ElseIf save = False Then
            MsgBox("Please save first")
        End If
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        'reset listbox button
        ListBox1.Items.Clear()
        ListBox1.Items.Add("25.7 Ohms / 1000ft - 24 AWG")
        ListBox1.Items.Add("1.6 Ohms / 1000ft - 12 AWG")
        ListBox1.Items.Add("1 Ohm / 1000ft - 10 AWG")
        ListBox1.Items.Add("0.2 Ohms / 1000ft - main")
    End Sub



    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        'clear listbox
        ListBox1.Items.Clear()
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        'calculate button
        'error check
        If ListBox1.SelectedIndex = -1 Or Not (IsNumeric(TextBox3.Text) And IsNumeric(TextBox4.Text) And IsNumeric(TextBox5.Text)) Then
            MsgBox("Please select a size from the list above and fill in the 3 boxes below with numbers")
            Exit Sub
        End If

        'some string minipulation vars so i can clean up the list to a numric value
        Dim Listitom As String = ListBox1.SelectedItem
        Dim Listitomconv As Decimal
        Dim O As String = "O"
        Dim FirstO As Integer = Listitom.IndexOf("O")
        Listitomconv = Listitom.Remove(FirstO)

        'maths
        FoAWG = TextBox5.Text
        Rwire = (Listitomconv / 1000) * FoAWG
        Label2.Text = FormatNumber(Rwire, 2) & " Ohms"
        Vs = TextBox3.Text
        Pload = TextBox4.Text
        I = Pload / Vs
        Label6.Text = FormatNumber(Vs, 2) & " Volts"
        Label4.Text = Pload & " Watts"
        Label3.Text = FormatNumber(I, 2) & " Amps"
        Vload = Vs - I * Rwire
        Label5.Text = FormatNumber(Vload, 2) & " Volts"
        Vloss = (Vs - Vload) / Vs * 100
        If Vloss > 5 Then
            'red warning
            Label1.ForeColor = Color.Red
            Label1.Text = "low Voltage " & Vloss & "%"
            Label1.Visible = True
        End If
        'end maths
    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        'close button
        Close()
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        'save
        save = True
        Dim Highestindex As Integer
        Dim sw As IO.StreamWriter = IO.File.CreateText("AWG1.TXT")
        Highestindex = ListBox1.Items.Count - 1

        For counter = 0 To Highestindex
            sw.WriteLine(ListBox1.Items(counter))
        Next
        sw.Close()
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        'set some bace values on load
        TB1C = False
        TB2C = False
        save = False
        ListBox1.Items.Add("25.7 Ohms / 1000ft - 24 AWG")
        ListBox1.Items.Add("1.6 Ohms / 1000ft - 12 AWG")
        ListBox1.Items.Add("1 Ohm / 1000ft - 10 AWG")
        ListBox1.Items.Add("0.2 Ohms / 1000ft - main")


    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        'add AWG button
        If TextBox1.Text < 0 Then
            MsgBox("only positive values in ohms per 100 ft")
        End If
        If TB1C And TB2C = True And IsNumeric(TextBox1.Text) = True Then
            ListBox1.Items.Add(TextBox1.Text & " Ohms/1000ft - " & TextBox2.Text)
        Else
            MsgBox("Please enter  an ammount in Ohms/1000Ft as a number ""Ex: 2.0"" and a name")
        End If
    End Sub

    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged
        'was textbox 1 changed?
        TB1C = True
    End Sub

    Private Sub TextBox2_TextChanged(sender As Object, e As EventArgs) Handles TextBox2.TextChanged
        'was textbox 2 changed
        TB2C = True
    End Sub
End Class
