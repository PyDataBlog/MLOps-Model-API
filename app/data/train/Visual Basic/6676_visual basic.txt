Module Module1
    Public Class TemperatureConverter
        Private Sub New()
        End Sub
		Public Shared Function CelsiusToFahrenheit(ByVal temperatureCelsius As String) As Double
            ' Convert argument to double for calculations.
            Dim celsius As Double = Double.Parse(temperatureCelsius)

            ' Convert Celsius to Fahrenheit.
            Dim fahrenheit As Double = (celsius * 9 \ 5) + 32
            Return fahrenheit
        End Function

        Public Shared Function FahrenheitToCelsius(ByVal temperatureFahrenheit As String) As Double
            ' Convert argument to double for calculations.
            Dim fahrenheit As Double = Double.Parse(temperatureFahrenheit)

            ' Convert Fahrenheit to Celsius.
            Dim celsius As Double = (fahrenheit - 32) * 5 \ 9
            Return celsius
        End Function
    End Class
    Sub Main()
        Console.WriteLine("Please Select The Convertor")
        Console.WriteLine("1. Celsius to Fahrenheit")
        Console.WriteLine("2. Fahrenheit to Celsius")
        Console.Write(Constants.vbLf & "Press either 1 or 2:")
        Dim Selection As String
        Selection = Console.ReadLine()
        Dim F As Double, C As Double = 0
        Select Case Selection
            Case "1"
                Console.Write("Please enter the Celsius temperature: ")
                F = TemperatureConverter.CelsiusToFahrenheit(System.Console.ReadLine())
                Console.WriteLine("Temperature in Fahrenheit: {0:F2}", F)

            Case "2"
                Console.Write("Please enter the Fahrenheit temperature: ")
                C = TemperatureConverter.FahrenheitToCelsius(System.Console.ReadLine())
                Console.WriteLine("Temperature in Celsius: {0:F2}", C)

            Case Else
                Console.WriteLine("Please select a convertor.")

        End Select
        Console.Write(Constants.vbLf & "Press ENTER to Quit..")
        Console.ReadLine()

    End Sub

End Module
