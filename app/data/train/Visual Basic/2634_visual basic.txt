Public Class Partida

    Public Sub New()
        dados.Add(New Dado)
        dados.Add(New Dado)
        dados.Add(New Dado)
        dados.Add(New Dado)
        dados.Add(New Dado)
        jugadores.Add(New Jugador("Jugador1"))
        jugadores.Add(New Jugador("Jugador2"))
        empezada = True
    End Sub

    Private _jugadores As New List(Of Jugador)
    Public Property jugadores() As List(Of Jugador)
        Get
            Return _jugadores
        End Get
        Set(ByVal value As List(Of Jugador))
            _jugadores = value
        End Set
    End Property

    Private _id As Integer
    Public Property id() As Integer
        Get
            Return _id
        End Get
        Set(ByVal value As Integer)
            _id = value
        End Set
    End Property

    Private _ganador As New Jugador
    Public Property ganador() As Jugador
        Get
            Return _ganador
        End Get
        Set(ByVal value As Jugador)
            _ganador = value
        End Set
    End Property

    Private _cubilete As New Cubilete
    Public Property cubilete() As Cubilete
        Get
            Return _cubilete
        End Get
        Set(ByVal value As Cubilete)
            _cubilete = value
        End Set
    End Property

    Private _dados As New List(Of Dado)
    Public Property dados() As List(Of Dado)
        Get
            Return _dados
        End Get
        Set(ByVal value As List(Of Dado))
            _dados = value
        End Set
    End Property

    Private _empezada As Boolean
    Public Property empezada() As Boolean
        Get
            Return _empezada
        End Get
        Set(ByVal value As Boolean)
            _empezada = value
        End Set
    End Property

    Private _tiempo As Integer
    Public Property tiempo() As Integer
        Get
            Return _tiempo
        End Get
        Set(ByVal value As Integer)
            _tiempo = value
        End Set
    End Property

    Private _empate As Boolean
    Public Property empate() As Boolean
        Get
            Return _empate
        End Get
        Set(ByVal value As Boolean)
            _empate = value
        End Set
    End Property

End Class
