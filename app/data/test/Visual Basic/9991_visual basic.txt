Module Module1
    Public Sub limpieza(ByVal origen As String)
        Select Case origen
            Case "calculadora"
                calculadora.numero1.Text = ""
                calculadora.numero2.Text = ""
                calculadora.txtOperador.Text = ""
                calculadora.txtResultado.Text = ""
                calculadora.ListResultados.Items.Clear()
            Case "conversor"
                conversor.txtEuro.Text = ""
                conversor.txtDolar.Text = ""
                conversor.txtLibra.Text = ""
                conversor.txtYen.Text = ""
        End Select
    End Sub
End Module
