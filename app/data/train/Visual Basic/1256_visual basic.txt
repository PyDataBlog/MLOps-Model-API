Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.DataAccess
Public Class RequisitoAdicionalBL : Implements IRequisitoAdicionalBL
    Dim factoryrepository As IRequisitoAdicionalRepository

    Public Sub New()
        factoryrepository = New RequisitoAdicionalRepository
    End Sub

    Public Sub DeleteREQUISITOADICIONAL(ByVal idRequisito As Integer) Implements IRequisitoAdicionalBL.DeleteREQUISITOADICIONAL
        factoryrepository.DeleteREQUISITOADICIONAL(idRequisito)
    End Sub

    Public Function GetAllFromREQUISITOADICIONAL() As System.Collections.Generic.List(Of BussinessEntities.RequisitoAdicional) Implements IRequisitoAdicionalBL.GetAllFromREQUISITOADICIONAL
        Return factoryrepository.GetAllFromREQUISITOADICIONAL()
    End Function

    Public Function GetREQUISITOADICIONALByIdPosicion(ByVal IdPosicion As Integer) As System.Collections.Generic.List(Of BussinessEntities.RequisitoAdicional) Implements IRequisitoAdicionalBL.GetREQUISITOADICIONALByIdPosicion
        Return factoryrepository.GetREQUISITOADICIONALByIdPosicion(IdPosicion)
    End Function

    Public Function GetREQUISITOADICIONALByIdRequisitoAndIdPosicion(ByVal IdRequisito As Integer, ByVal IdPosicion As Integer) As BussinessEntities.RequisitoAdicional Implements IRequisitoAdicionalBL.GetREQUISITOADICIONALByIdRequisitoAndIdPosicion
        Return factoryrepository.GetREQUISITOADICIONALByIdRequisitoAndIdPosicion(IdRequisito, IdPosicion)
    End Function

    Public Function SaveREQUISITOADICIONAL(ByVal RequisitoAdicional As BussinessEntities.RequisitoAdicional) As Integer Implements IRequisitoAdicionalBL.SaveREQUISITOADICIONAL
        Return factoryrepository.SaveREQUISITOADICIONAL(RequisitoAdicional)
    End Function
End Class
