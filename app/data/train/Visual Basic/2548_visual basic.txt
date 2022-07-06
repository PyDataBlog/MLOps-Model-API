Public Class CondicionesContrato
    Public Property IdCondicion As System.Nullable(Of Int32)
    Public Property IdContrato As System.Nullable(Of Int32)
    Public Property IdPeriodoLaboral As System.Nullable(Of Int32)
    Public Property IdPosicion As System.Nullable(Of Int32)
    Public Property IdSituacionEspecial As System.Nullable(Of Int32)

    Public Property FechaCambio As System.Nullable(Of DateTime)
    Public Property Sueldo As System.Nullable(Of Decimal)
    Public Property FechaAmpliacionContrato As System.Nullable(Of DateTime)
    Public Property cambioSueldo As System.Nullable(Of Boolean)
    Public Property cambioCargo As System.Nullable(Of Boolean)
    Public Property AmpliacionContrato As System.Nullable(Of Boolean)
    Public Property CondicionDelCambio As String
    Public Property NroAdemda As String

    Public Property Posicion As Posicion
    Public Property periodoLaboral As PeriodoLaboral
    Public Property contrato As ContratoConvenio
    Public Property situacionEspecial As SituacionEspecial

    Public Property archivo As String

End Class
