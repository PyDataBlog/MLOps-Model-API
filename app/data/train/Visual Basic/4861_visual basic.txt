''' <summary>
''' Class with common properties and methods for working with numeric value objects.
''' </summary>
''' <remarks></remarks>
Public MustInherit Class Value

    Private _ValueMagnitude As Double
    ''' <summary>
    ''' The numeric magnitude of the value.
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Property ValueMagnitude As Double
        Get
            Return _ValueMagnitude
        End Get
        Set(value As Double)
            _ValueMagnitude = value
        End Set
    End Property

End Class
