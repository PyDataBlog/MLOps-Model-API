Partial Public Class Coche ' Hereda de Object

    Private transmision As TransmisionCoche
    ' Clase anidada
    Private Class TransmisionCoche
        Public delantera As Boolean
        Public trasera As Boolean
    End Class
    ' Constructor por defecto
    Private Sub New()
    End Sub
    Sub New(modelo As String)

        Me._modelo = modelo
        transmision = New TransmisionCoche()
        _ruedas = New Int32(3) {50, 60, 70, 80}
        numeroCoches = numeroCoches + 1

    End Sub
    ' Otros constructores
    Public Sub New(nuevoKM As Integer, modelo As String)
        _kilometraje = nuevoKM
        Me.transmision = New TransmisionCoche()
        Me._modelo = modelo
        _ruedas = New Int32(3) {50, 60, 70, 80}
        numeroCoches = numeroCoches + 1
    End Sub
    ' Este metedo es llamado por el GC (Garbage Collector o Recolector de Basura)
    Protected Overrides Sub Finalize()
        Console.WriteLine("Eliminando coche " &
                          _modelo)
        'Console.ReadLine()
    End Sub
End Class
