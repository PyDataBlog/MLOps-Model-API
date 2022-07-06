Imports System.Data.SqlClient

Public Class ModProductForm

    Dim contador
    Dim ds As New DataSet
    Dim da As SqlDataAdapter
    Dim con As SqlConnection
    Dim cmd As SqlCommand

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        'Cerramos la ventana y volvemos
        Me.Close()
    End Sub

    Private Sub ModProductForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        TextBoxCodProd.ReadOnly = True

        'Hacemos la conexion
        con = New SqlConnection(My.Settings.conexion)

        'Establece la conexion con el origen de los datos
        Dim sql As String = "SELECT cod_prod, nom_prod, precio, stock, stock_minimo FROM PRODUCTO"

        'consulta
        cmd = New SqlCommand(sql, con)
        da = New SqlDataAdapter(cmd)

        'Ejecuta la consulta y recupera los datos del proveedor

        'conj de comandos y conexion para rellenar el dataset
        da.Fill(ds, "PRODUCTO")

        'LLamamos al metodo rellenar
        rellenarTextBox()


        'Rellenamos el combobox
        'Rellenamos el combobox
        '    Dim total As Integer = (ds.Tables("PRODUCTO").Rows.Count - 1)
        '   Dim cont = 0
        '  For index As Integer = 0 To total
        '
        ' ComboBox1.Items.Add(ds.Tables("PRODUCTO").Rows(cont).Item("nom_prod").ToString)
        'cont += 1
        ' Next





    End Sub
    Sub rellenarTextBox()
        Try
            TextBoxCodProd.Text = ds.Tables("PRODUCTO").Rows(contador).Item("cod_prod").ToString()
            TextBoxNombre.Text = ds.Tables("PRODUCTO").Rows(contador).Item("nom_prod").ToString()
            TextBoxPrecio.Text = ds.Tables("PRODUCTO").Rows(contador).Item("precio").ToString()
            TextBoxStock.Text = ds.Tables("PRODUCTO").Rows(contador).Item("stock").ToString()
            TextBoxStockMin.Text = ds.Tables("PRODUCTO").Rows(contador).Item("stock_minimo").ToString
            TextBoxDescripcion.Text = ds.Tables("PRODUCTO").Rows(contador).Item("descripcion").ToString
        Catch ex As Exception
        End Try
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        'Boton de guardar, insertamos la informacion en la base de datos.


        Dim mbd As New ManejadorBD

        mbd.modificarProducto(Me.TextBoxCodProd.Text, Me.TextBoxNombre.Text, Me.TextBoxPrecio.Text, Me.TextBoxStock.Text, Me.TextBoxStockMin.Text, Me.TextBoxDescripcion.Text)


        'consulta
        da.Update(ds, "PRODUCTO")
        contador = 0
        rellenarTextBox()
        Me.Close()

    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        ' Boton de ir al principio ( boton << )

        If contador = 0 Then
            Beep()
            MsgBox("Estas en el primer elemento de la tabla")
        Else
            contador = 0
        End If

        rellenarTextBox()

    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        ' Boton de retroceder de uno en uno  (boton <) 
        If contador <> 0 Then
            contador = contador - 1

        Else
            Beep()
            MsgBox("Has llegado al Principio de la lista")
        End If

        rellenarTextBox()

    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        ' Boton de avanzar de uno en uno
        If contador < ds.Tables("PRODUCTO").Rows.Count - 1 Then
            contador = contador + 1

        Else
            Beep()
            MsgBox("Has llegado al final de la lista")
        End If

        rellenarTextBox()

    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        ' Boton de ir al final  (boton >> )

        If contador = ds.Tables("PRODUCTO").Rows.Count Then
            Beep()
            MsgBox("Estas en el ultimo elemento de la tabla")
        Else
            contador = ds.Tables("PRODUCTO").Rows.Count - 1
        End If

        rellenarTextBox()

    End Sub


End Class