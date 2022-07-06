Public Class FormPrincipal
    Private miFormComboListas As FormComboListas
    Private Sub FormPrincipal_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        miFormComboListas = New FormComboListas()
    End Sub
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles btnMostrarLongitud.Click
        MessageBox.Show("longitud de texto multilena: " & txtMultilinea.TextLength.ToString() &
                        vbCrLf & "longitud texto 2: " & txtDiezCaracteres.TextLength.ToString())
    End Sub
    Private Sub TextBox1_2_TextChanged(sender As Object, e As EventArgs) Handles txtMultilinea.TextChanged, txtDiezCaracteres.TextChanged
        lblLongitudes.Text = "longitud de texto multilena: " & txtMultilinea.TextLength.ToString() &
                        vbCrLf & "longitud texto 2: " & txtDiezCaracteres.TextLength.ToString()
    End Sub
    Private Sub btnAdd_Click(sender As Object, e As EventArgs) Handles btnAdd.Click
        lstListaDatos.Items.Add(txtMultilinea.Text)
    End Sub
    Private Sub btnAdd2_Click(sender As Object, e As EventArgs) Handles btnAdd2.Click
        lstListaDatos.Items.Add(txtDiezCaracteres.Text)
        txtDiezCaracteres.Clear()
    End Sub
    Private Sub btnEliminarDeLista_Click(sender As Object, e As EventArgs) Handles btnEliminarDeLista.Click
        lstListaDatos.Items.RemoveAt(lstListaDatos.SelectedIndex)
    End Sub
    Private Sub lstListaDatos_SelectedIndexChanged(sender As Object, e As EventArgs) Handles lstListaDatos.SelectedIndexChanged

        btnEliminarDeLista.Enabled = (lstListaDatos.SelectedIndex >= 0)
    End Sub
    Private Sub btnAbrirForm_Click(sender As Object, e As EventArgs) Handles btnAbrirForm.Click
        If miFormComboListas Is Nothing Or miFormComboListas.IsDisposed Then
            miFormComboListas = New FormComboListas()
        End If
        miFormComboListas.ShowDialog()
    End Sub
    Private Sub btnCerrar_Click(sender As Object, e As EventArgs) Handles btnCerrar.Click
        miFormComboListas.Hide()  'nuevoFormComboListas.Close()
    End Sub

    Private Sub btnCambiar_Click(sender As Object, e As EventArgs) Handles btnCambiar.Click, btnMostrarLongitud.Click
        Dim tipoFuente As Integer
        'tipoFuente = System.Drawing.FontStyle.Bold + System.Drawing.FontStyle.Strikeout     ' 1001 = 9
        tipoFuente = System.Drawing.FontStyle.Italic + System.Drawing.FontStyle.Underline   ' 0110 = 6
        'tipoFuente = System.Drawing.FontStyle.Bold + System.Drawing.FontStyle.Italic        ' 0011 = 3

        Me.lblLongitudes.Font = New Font("Microsoft Sans Serif", 16, tipoFuente,
                                         System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        ' Ampliar el tamaño de txtMultilinea
        Me.txtMultilinea.Multiline = True
        Dim tamañoTxtMult As Size = Me.txtMultilinea.Size
        tamañoTxtMult.Height += 100
        Me.txtMultilinea.Size = tamañoTxtMult
        ' Mover a la derecha el boton pulsado
        Dim esteBoton As Button = CType(sender, Button)
        esteBoton.Location = New Point(esteBoton.Location.X + 20, esteBoton.Location.Y)

        ' Poner btnAbrirForm a la izquierda del boton Cerrar
        btnAbrirForm.Location = New Point(btnCerrar.Location.X - btnAbrirForm.Size.Width - 10,
                                            btnCerrar.Location.Y)

        ' Cambiar las propiedades del propio formulario
        Me.Text = "Nuevo titulo ventana"
        Me.BackColor = Color.BurlyWood
        Me.BackColor = Color.FromArgb(255, 255, 255, 0) '32 bits. 8 bit por color, 1 byte por color

        '417; 45 / 245; 184
        lstListaDatos.SetBounds(417, 45, 245, 185)
        'lstListaDatos.BringToFront()
        Label5.SendToBack()

        '       Dim nuevoBoton As Button
    End Sub

    Private Sub DateTimePicker1_ValueChanged(sender As Object, e As EventArgs) Handles DateTimePicker1.ValueChanged
        Dim fecha As Date = DateTimePicker1.Value
        fecha = fecha.AddDays(1)

        Me.Text = "Ventana: " & fecha.ToString()

        If fecha.Year = 2017 Then
            Me.Text += "¡Este mismo año!"
        End If
    End Sub

    Private Sub NuevoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles NuevoToolStripMenuItem.Click
        Dim nuevoBoton As Button
        nuevoBoton = New Button()
        nuevoBoton.Size = New Size(200, 200)
        nuevoBoton.Location = New Point(100, 100)
        nuevoBoton.Text = "Pulsa para eliminar"
        nuevoBoton.Parent = Me
        nuevoBoton.BringToFront()
        AddHandler nuevoBoton.Click, AddressOf EliminarComponente
    End Sub
    Private Sub EliminarComponente(sender As Button, e As EventArgs)
        sender.Dispose()
    End Sub

    Private Sub MonthCalendar1_DateChanged(sender As Object, e As DateRangeEventArgs) Handles MonthCalendar1.DateChanged

        DateTimePicker1.Value = MonthCalendar1.SelectionRange.Start
        DateTimePicker2.Value = MonthCalendar1.SelectionRange.End

    End Sub

    Public Sub MostrarDiaSemana(dateTime As DateTimePicker, e As EventArgs) Handles DateTimePicker1.ValueChanged
        Select Case dateTime.Value.DayOfWeek
            Case DayOfWeek.Monday
                Label6.Text = "Lunes"
            Case DayOfWeek.Tuesday
                Label6.Text = "Martes"
            Case DayOfWeek.Wednesday
                Label6.Text = "Miércoles"
            Case DayOfWeek.Thursday
                Label6.Text = "Jueves"
            Case DayOfWeek.Friday
                Label6.Text = "Viernes"
            Case DayOfWeek.Saturday
                Label6.Text = "Sábado"
            Case DayOfWeek.Sunday
                Label6.Text = "Domingo"
        End Select
    End Sub

    Public Sub MostrarComparativa(dateTime As DateTimePicker, e As EventArgs) Handles DateTimePicker1.ValueChanged, DateTimePicker2.ValueChanged

        If DateTimePicker1.Value.CompareTo(DateTimePicker2.Value) < 0 Then
            Label7.Text = "La primera fecha es menor"
        ElseIf DateTimePicker1.Value.CompareTo(DateTimePicker2.Value) > 0 Then
            Label7.Text = "La primera fecha es mayor"
        Else
            Label7.Text = "Las fechas son iguales"
        End If

    End Sub

    Private Sub Button1_Click_1(sender As Object, e As EventArgs) Handles Button1.Click

        If TextBox1.Text.CompareTo(TextBox2.Text) < 0 Then
            Label8.Text = "Texto A está antes por orden alfabético"
        ElseIf TextBox1.Text.CompareTo(TextBox2.Text) > 0 Then
            Label8.Text = "Texto A está después por orden alfabético"
        Else
            Label8.Text = "Los textos son iguales"
        End If

    End Sub

End Class
