

Public MustInherit Class Figura
    Implements ICalculable

    Protected _area As Single
    Protected _perimetro As Single
    Public ReadOnly Property Area As Single Implements IConAreaCalculable.Area
        Get
            Return _area
            ' Throw New NotImplementedException()
        End Get
    End Property

    Public ReadOnly Property Perimetro As Single Implements IConPerimetroCalculable.Perimetro
        Get
            Return _perimetro
        End Get
    End Property

    Public MustOverride Function CalcularArea() As Single Implements IConAreaCalculable.CalcularArea
    Public MustOverride Function CalcularPerimetro() As Single Implements IConPerimetroCalculable.CalcularPerimetro

    Public Overridable Sub Mostrar()
        Console.WriteLine("Mostrando figura")
    End Sub


End Class

