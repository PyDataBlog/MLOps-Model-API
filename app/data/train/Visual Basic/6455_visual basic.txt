Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.DataAccess
Public Class ControlarBL : Implements IControlarBL
    Dim factoryrepository As IControlarRepository
    Public Sub New()
        factoryrepository = New ControlarRepository
    End Sub

    Public Sub DeleteCONTROLAR(ByVal IdControl As Integer) Implements IControlarBL.DeleteCONTROLAR
        factoryrepository.DeleteCONTROLAR(IdControl)
    End Sub

    Public Function GetCONTROLARByIdControl(ByVal IdControl As Integer) As BussinessEntities.Controlar Implements IControlarBL.GetCONTROLARByIdControl
        Return factoryrepository.GetCONTROLARByIdControl(IdControl)
    End Function

    Public Function GetCONTROLARByIdDetalleEje(ByVal IdDetalleEje As Object) As System.Collections.Generic.List(Of BussinessEntities.Controlar) Implements IControlarBL.GetCONTROLARByIdDetalleEje
        Return factoryrepository.GetCONTROLARByIdDetalleEje(IdDetalleEje)
    End Function

    Public Sub SaveCONTROLAR(ByVal _Controlar As BussinessEntities.Controlar) Implements IControlarBL.SaveCONTROLAR
        factoryrepository.SaveCONTROLAR(_Controlar)
    End Sub
End Class
