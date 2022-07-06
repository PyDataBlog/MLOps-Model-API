'------------------------------------------------------------------------------
' <generado automáticamente>
'     Este código fue generado por una herramienta.
'
'     Los cambios en este archivo podrían causar un comportamiento incorrecto y se perderán si
'     se vuelve a generar el código. 
' </generado automáticamente>
'------------------------------------------------------------------------------

Option Strict On
Option Explicit On


Partial Public Class VentanaCentroCostoFactura

    '''<summary>
    '''Control Head1.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents Head1 As Global.System.Web.UI.HtmlControls.HtmlHead

    '''<summary>
    '''Control form1.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents form1 As Global.System.Web.UI.HtmlControls.HtmlForm

    '''<summary>
    '''Control RadScriptManager1.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents RadScriptManager1 As Global.Telerik.Web.UI.RadScriptManager

    '''<summary>
    '''Control odsCentroCostoOfOrden.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents odsCentroCostoOfOrden As Global.System.Web.UI.WebControls.ObjectDataSource

    '''<summary>
    '''Control odsTipoDocumento.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents odsTipoDocumento As Global.System.Web.UI.WebControls.ObjectDataSource

    '''<summary>
    '''Control odsFacturaCentroCosto.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents odsFacturaCentroCosto As Global.System.Web.UI.WebControls.ObjectDataSource

    '''<summary>
    '''Control rcbOrdenCentroCosto.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents rcbOrdenCentroCosto As Global.Telerik.Web.UI.RadComboBox

    '''<summary>
    '''Control rcbTipoDocumento.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents rcbTipoDocumento As Global.Telerik.Web.UI.RadComboBox

    '''<summary>
    '''Control rfvcCosto0.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents rfvcCosto0 As Global.System.Web.UI.WebControls.RequiredFieldValidator

    '''<summary>
    '''Control txtSerie.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents txtSerie As Global.System.Web.UI.WebControls.TextBox

    '''<summary>
    '''Control rfvcCosto1.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents rfvcCosto1 As Global.System.Web.UI.WebControls.RequiredFieldValidator

    '''<summary>
    '''Control txtNumero.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents txtNumero As Global.System.Web.UI.WebControls.TextBox

    '''<summary>
    '''Control rfvcCosto2.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents rfvcCosto2 As Global.System.Web.UI.WebControls.RequiredFieldValidator

    '''<summary>
    '''Control txtMonto.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents txtMonto As Global.System.Web.UI.WebControls.TextBox

    '''<summary>
    '''Control rfvMonto.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents rfvMonto As Global.System.Web.UI.WebControls.RequiredFieldValidator

    '''<summary>
    '''Control revMonto.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents revMonto As Global.System.Web.UI.WebControls.RegularExpressionValidator

    '''<summary>
    '''Control lbAgregar.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents lbAgregar As Global.System.Web.UI.WebControls.LinkButton

    '''<summary>
    '''Control gvFacturaCentroCosto.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents gvFacturaCentroCosto As Global.System.Web.UI.WebControls.GridView

    '''<summary>
    '''Control lbFinalizar.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents lbFinalizar As Global.System.Web.UI.WebControls.LinkButton

    '''<summary>
    '''Control RadAjaxManager1.
    '''</summary>
    '''<remarks>
    '''Campo generado automáticamente.
    '''Para modificarlo, mueva la declaración del campo del archivo del diseñador al archivo de código subyacente.
    '''</remarks>
    Protected WithEvents RadAjaxManager1 As Global.Telerik.Web.UI.RadAjaxManager
End Class
