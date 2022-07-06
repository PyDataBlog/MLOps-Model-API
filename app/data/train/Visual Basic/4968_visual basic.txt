Module Module1

    Public Class Rectangle
        Public length, width As Integer
        'Public width As Integer
        Public Function Area() As Integer
            Dim ar As Integer = length * width
            Return ar
        End Function
    End Class
    Sub Main()
        Dim rct As New Rectangle
        With rct
            .length = 100
            .width = 50
        End With
        Console.WriteLine("The area of the rectangle is " & Constants.vbLf & rct.Area())
        Console.Write(Constants.vbLf & "Press ENTER to quit...")
        Console.ReadLine()
    End Sub
End Module

