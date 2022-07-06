Module Module1

    Sub Main()
        Dim counter As Integer = 0
        Dim AuthorName As String() = New String(9) {}
        AuthorName(0) = "Puneet"
        AuthorName(1) = "Shambhu"
        AuthorName(2) = "Chitranjan"
        AuthorName(3) = "Yogendra"
        AuthorName(4) = "Indubhushan"
        Console.WriteLine("The Student names are:")
        For counter = 0 To 4
            Console.WriteLine(AuthorName(counter))
        Next counter
        Console.Write("press ENTER to exit...")
        Console.ReadLine()

    End Sub

End Module
