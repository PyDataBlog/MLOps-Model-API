Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.DataAccess
Public Class EspecieAnimalBL : Implements IEspecieAnimalBL
    Dim factoryrepository As IEspecieAnimalRepository

    Public Sub New()
        factoryrepository = New EspecieAnimalRepository
    End Sub

    Public Function GetAllFromEspecieAnimal() As System.Collections.Generic.List(Of BussinessEntities.EspecieAnimal) Implements IEspecieAnimalBL.GetAllFromEspecieAnimal
        Return factoryrepository.GetAllFromEspecieAnimal
    End Function

    Public Function GetEspecieAnimalByIdRaza(ByVal idRaza As Integer) As BussinessEntities.EspecieAnimal Implements IEspecieAnimalBL.GetEspecieAnimalByIdRaza
        Return factoryrepository.GetEspecieAnimalByIdRaza(idRaza)
    End Function
End Class
