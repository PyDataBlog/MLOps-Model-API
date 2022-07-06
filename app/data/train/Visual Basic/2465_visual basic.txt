Imports SistFoncreagro.BussinessEntities
Public Interface ICuentaEnlaceBL
    Function GetCUENTAENLACEByCtaOperacion(ByVal CtaOperacion As Int32, ByVal Destino As String, ByVal Tipo As String) As IList(Of CuentaEnlace)
    Function GetAllFromCUENTAENLACE() As List(Of CuentaEnlace)
    Function GetCUENTAENLACEByParameter(ByVal Texto As String, ByVal Tipo As String) As List(Of CuentaEnlace)
    Function GetCUENTAENLACEByCtaOperacionAndCtaPagar(ByVal CtaOperacion As Int32, ByVal Destino As String, ByVal CtaPagar As Int32, ByVal Tipo As String) As CuentaEnlace
    Function GetCUENTAENLACEByCtaPagar(ByVal CtaPagar As Int32, ByVal Tipo As String) As CuentaEnlace
    Function GetCtaCobrarByCtaPagarAndDestino(ByVal CtaPagar As Int32, ByVal Destino As String, ByVal Tipo As String) As Int32
    Function GetCUENTAENLACEByIdCtaEnlace(ByVal IdCtaEnlace As Int32) As CuentaEnlace
    Function SaveCUENTAENLACE(ByVal _CuentaEnlace As CuentaEnlace) As Int32
    Sub UpdateEstadoCUENTAENLACE(ByVal IdCtaEnlace As Int32)
    Function GetCtaPagarByCtaCobrarAndDestino(ByVal CtaCobrar As Int32, ByVal Destino As String, ByVal Tipo As String) As Int32
    Function GetCUENTAENLACEByCtaOperacionAndCtaCobrar(ByVal CtaOperacion As Int32, ByVal Destino As String, ByVal CtaCobrar As Int32, ByVal Tipo As String) As CuentaEnlace
    Function GetOrigenCtaEnlace(ByVal CtaPagar As Int32, ByVal Destino As String) As String
    Function GetOrigenCtaEnlace1(ByVal CtaCobrar As Int32, ByVal Destino As String) As String
    Function GetCtaOperacionByCtaCobrarAndDestino(ByVal CtaCobrar As Int32, ByVal Destino As String, ByVal Tipo As String) As String
    Function GetCtaOperacionByCtaPagarAndDestino(ByVal CtaPagar As Int32, ByVal Destino As String, ByVal Tipo As String) As String
    Function VerifyExisteCUENTAENLACE(ByVal Origen As String, ByVal Destino As String, ByVal CtaOperacion As Int32, ByVal CtaPagar As Int32, ByVal CtaCobrar As Int32, ByVal Tipo As String) As Int32
End Interface

