Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.DataAccess
Public Class InteresadosBL : Implements IInteresadosBL

    Dim factoryrepository As IInteresadosRepository

    Public Sub New()
        factoryrepository = New InteresadosRepository
    End Sub

    Public Sub DeleteINTERESADOS(ByVal IdInteresado As Integer) Implements IInteresadosBL.DeleteINTERESADOS
        factoryrepository.DeleteINTERESADOS(IdInteresado)
    End Sub

    Public Function GetINTERESADOByParametro(ByVal Parametro As String) As System.Collections.Generic.List(Of BussinessEntities.Interesados) Implements IInteresadosBL.GetINTERESADOByParametro
        Return factoryrepository.GetINTERESADOByParametro(Parametro)
    End Function

    Public Function GetINTERESADOSByIdInteresado(ByVal IdInteresado As Integer) As BussinessEntities.Interesados Implements IInteresadosBL.GetINTERESADOSByIdInteresado
        Return factoryrepository.GetINTERESADOSByIdInteresado(IdInteresado)
    End Function

    Public Function SaveINTERESADO(ByVal _Interesados As BussinessEntities.Interesados) As Integer Implements IInteresadosBL.SaveINTERESADO
        Return factoryrepository.SaveINTERESADO(_Interesados)
    End Function

    Public Function VerifyInteresado(dni As String) As Integer Implements IInteresadosBL.VerifyInteresado
        Return factoryrepository.VerifyInteresado(dni)
    End Function

    Public Function VerifyInteresado1(nombre As String) As Integer Implements IInteresadosBL.VerifyInteresado1
        Return factoryrepository.VerifyInteresado1(nombre)
    End Function
End Class
