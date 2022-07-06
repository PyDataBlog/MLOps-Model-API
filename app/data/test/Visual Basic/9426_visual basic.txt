Module Ejemplo_Tabla_Multiplicar_ForNext
    Sub main()

        Console.WriteLine("Introduce un número: ")
        Dim número As Integer

        For número = 1 To 10

            número = Console.ReadLine
            Dim i As Integer = 1
            Do
                Console.WriteLine(número & " x " & i & " = " & número * i)
                i = i + 1
            Loop Until i = 10

        Next

        Console.ReadLine()



    End Sub

End Module
