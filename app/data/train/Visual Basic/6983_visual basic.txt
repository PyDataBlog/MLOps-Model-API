Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.DataAccess

Public Class SituacionEspecialBL : Implements ISituacionEspecialBL
    Dim factoryRepository As ISituacionEspecialRepository
    Sub New()
        factoryRepository = New SituacionEspecialRepository
    End Sub

    Public Function GetAllSituacionEspecial() As System.Collections.Generic.List(Of BussinessEntities.SituacionEspecial) Implements ISituacionEspecialBL.GetAllSituacionEspecial
        Return Me.factoryRepository.GetAllSituacionEspecial
    End Function

    Public Function GetSituacionEspecialByIdSituacionespecial(ByVal idSituacionEspecial As Integer) As BussinessEntities.SituacionEspecial Implements ISituacionEspecialBL.GetSituacionEspecialByIdSituacionespecial
        Return Me.factoryRepository.GetSituacionEspecialByIdSituacionespecial(idSituacionEspecial)
    End Function
End Class
