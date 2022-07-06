Module ModuloIVA
    Const IVA_GLOBAL As Decimal = 1.21
    Private Function CalculoPrecioConIVA(precioSinIVA As Decimal, Optional iva As Decimal = IVA_GLOBAL, Optional Descuento As Decimal = 0) As Decimal
        ' Aplicamos un IVA del 21%
        Return precioSinIVA * iva - Descuento
    End Function

    Public Sub CalcularIVA()
        MostrarPreciosEstandar()
        Console.WriteLine("Introduzca precio sin IVA: ")
        Dim precioSin As Decimal = Console.ReadLine
        Console.WriteLine("Introduzca IVA: ")
        Dim iva As Decimal = Console.ReadLine
        Console.WriteLine("Introduzca Descuento: ")
        Dim Descuento As Decimal = Console.ReadLine
        Dim precioCON As Decimal
        If iva = 0 Then
            If Descuento = 0 Then
                precioCON = CalculoPrecioConIVA(precioSin)
            Else
                precioCON = CalculoPrecioConIVA(precioSin, , Descuento)
            End If
        ElseIf Descuento = 0 Then
            precioCON = CalculoPrecioConIVA(precioSin, iva)
        Else
            precioCON = CalculoPrecioConIVA(precioSin, iva, Descuento)
        End If


        Console.WriteLine("El  precio con IVA es " & precioCON)
    End Sub
    Sub MostrarPreciosEstandar()
        Dim precios() As Decimal = {50, 100, 200}
        For i As Integer = 0 To 2
            Console.WriteLine("IVA de " & precios(i) & " es " & CalculoPrecioConIVA(precios(i)))
            CalculoPrecioConIVA(precios(i), 1.21)
        Next
    End Sub

End Module
