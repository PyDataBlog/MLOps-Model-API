Public Class DetalleConciliacion
    Public Property IdDetalle As Int32
    Public Property IdConciliacion As Int32
    Public Property Fecha As Date
    Public Property Glosa As String
    Public Property IdProveedorCliente As Int32
    Public Property Monto As Decimal
    Public Property IdTipoMovimiento As Int32
    Public Property Coa As ProveedorCliente
    Public Property TipoMovimiento As TipoMovimiento
    Public ReadOnly Property ProveedorCliente As String
        Get
            Return Coa.Ruc + " " + Coa.RazonSocial
        End Get
    End Property
    Public ReadOnly Property TipoMovimientoDesc As String
        Get
            Return TipoMovimiento.Descripcion
        End Get
    End Property
End Class
