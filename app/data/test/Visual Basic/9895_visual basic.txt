Imports SistFoncreagro.BussinessEntities

Public Interface IPersonalBL
    Function GetAllFromPersonal() As List(Of Personal)
    Function GetPersonalById(ByVal id As Int32) As Personal
    Function GetPersonalById(ByVal id As Int32, ByVal argPersonal As ArgumentosPersonal) As Personal
    Function GetFiltrarPERSONALPorParametro(ByVal apeMat As String, ByVal apePat As String, ByVal nomb As String, ByVal di As String) As List(Of Personal)
    Function SavePersonal(ByVal Personal As Personal) As Int32
    Sub UpdateFotoPERSONAL(ByVal IdPersonal As Int32, ByVal ArchivoFoto As String)
    Sub UpdateCVPERSONAL(ByVal IdPersonal As Int32, ByVal ArchivoCV As String)
    Sub UpdateArchivoCertificadoTipoSangre(ByVal CertificadoTipoSangre As String, ByVal IdPersonal As Int32)
    Sub DeletePersonal(ByVal id As Int32)
    Function SaveViculoLaboral(ByVal personal As Personal) As System.Nullable(Of Int32)

    '
    Function GetPERSONALActualmenteLaborandoByIdCategoria(ByVal IdCategoria As Integer) As System.Collections.Generic.List(Of BussinessEntities.Personal)
    'Function GetPERSONALALiquidadoPorRangoFecha(ByVal IdCategoria As Integer, ByVal fechaIni As DateTime, ByVal fechaFin As DateTime) As System.Collections.Generic.List(Of BussinessEntities.Personal)
    Function GetPERSONALALiquidadoPorRangoFecha(ByVal IdCategoria As Integer) As System.Collections.Generic.List(Of BussinessEntities.Personal)
    Function GetPERSONALSinVinculoLaboral() As System.Collections.Generic.List(Of BussinessEntities.Personal)
    Function GetPERSONALByIdCategoria(ByVal idCategoria As Integer) As System.Collections.Generic.List(Of Personal)
    Function GetPERSONALSinVinculoYConVinculoApartirDeUnaFecha(ByVal fechaIni As DateTime) As System.Collections.Generic.List(Of BussinessEntities.Personal)
    Function GetPersonalByFechaByIdProyecto(ByVal Fecha As DateTime, ByVal idProyecto As Int32) As List(Of Personal)
    Function GetPERSONALinControlAsistencia(ByVal Fecha As DateTime) As List(Of Personal)

    Function GetCertificadoTipoSangrePERSONALByIdPersonal(ByVal IdPersonal As Int32) As String
    Function GetAsignacionFamiliarByIdPersonal(ByVal Idpersonal As Int32) As Decimal

    ''' <summary>
    ''' Metodo Devuelve una coleccion de Tipo Personal segun criterio
    ''' </summary>
    ''' <param name="criterio">apellidos o nombres</param>
    ''' <returns>Coleccion tipo Personal</returns>
    ''' <remarks></remarks>
    Function GetPersonalByCriterio(ByVal criterio As String) As IEnumerable(Of Personal)

End Interface
