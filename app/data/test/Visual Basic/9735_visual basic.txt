Module Module1

    Sub Main()
        Dim opcion As Char
        Dim operador As Char
        Dim operando_1, operando_2, resultado As Single
        'Do
        Do
            Console.WriteLine("Introduzca operando 1")
            operando_1 = Console.ReadLine
            Console.WriteLine("Operador")
            operador = Console.ReadLine
            Console.WriteLine("Introduzca operando 2")
            operando_2 = Console.ReadLine
            Select Case operador
                Case "*"
                    resultado = operando_1 * operando_2
                Case "/"
                    resultado = operando_1 / operando_2
                Case "+"
                    resultado = operando_1 + operando_2
                Case "-"
                    resultado = operando_1 - operando_2
            End Select
            Console.WriteLine("El resultado es " & resultado)
            Console.WriteLine("Calcular (c)?")
            opcion = Console.ReadLine
        Loop While opcion = "c"
    End Sub



End Module
