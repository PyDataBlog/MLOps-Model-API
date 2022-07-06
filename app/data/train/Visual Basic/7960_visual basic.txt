Public Class Convenio

    Public Property IdConvenio As Int32
    Public Property Codigo As String
    Public Property Nombre As String
    Public Property FecIni As Date
    Public Property FecFin As Date
    Public Property IdMoneda As Int32
    Public Property MontoInicial As String
    Public Property IdTipConv As Int32
    Public Property FaseCont As String
    Public Property Estado As String
    Public Property Ejecutado As Decimal
    Public Property FechaFirma As Date
    Public Property MontoTotal As Decimal
    Public Property Comprometido As Decimal
    Public Property Presupuestado As Decimal
    Public Property IdConveMarco As Int32
    Public Property _ConveMarco As ConveMarco
    Public Property _TipoConvenio As TipoConvenio
    Public Property _Moneda As Moneda

    Public ReadOnly Property NombreconvenioMarco As String
        Get
            Return _ConveMarco.Nombre
        End Get
    End Property

    Public ReadOnly Property NombreTipoConvenio As String
        Get
            Return _TipoConvenio.Nombre
        End Get
    End Property

    Public ReadOnly Property NombreMoneda As String
        Get
            Return _Moneda.Nombre
        End Get
    End Property

End Class
