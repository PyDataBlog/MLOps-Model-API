Imports SistFoncreagro.BussinesLogic
Public Class FrmTrasladoSaldos
    Inherits System.Web.UI.Page
    Dim EjercicioBL As New EjercicioBL
    Dim TipoCambioBl As New TipocambioBL
    Dim TransaccionBl As New TransaccionBL
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Dim estadoMes As String = ""
        estadoMes = EjercicioBL.GetEstadoMes(Session("Anio"), Session("Mes"))
      

        If estadoMes = "C" Then
            Dim cadena_java As String = _
                          "<script language='javascript'>function f(){radalert('<b>¡Mes Cerrado, Opción Deshabilitada!</b> ', 330, 100,'¡Alerta!'); Sys.Application.remove_load(f);}; Sys.Application.add_load(f);</script>"
            ScriptManager.RegisterStartupScript(Page, GetType(Page), "AlertaX", cadena_java.ToString, False)
            Me.Image4.Visible = False
        Else
            Dim radconfirmscript As String = "<script language='javascript'>function f(){radconfirm('¡Se Procesará el Traslado de Saldos del Ejercicio!', confirmCallBackFn, 330, 100, null, 'Mensaje de Confirmación'); Sys.Application.remove_load(f);}; Sys.Application.add_load(f); ;</script>"
            Page.ClientScript.RegisterStartupScript(Me.[GetType](), "radconfirm", radconfirmscript)
        End If
    End Sub
    Protected Sub RadAjaxManager1_AjaxRequest(ByVal sender As Object, ByVal e As Telerik.Web.UI.AjaxRequestEventArgs) Handles RadAjaxManager1.AjaxRequest
        Dim codigo As String = ""

        If e.Argument.StartsWith("Generar") Then
            Dim j() As String
            Dim valor As String
            j = Split(e.Argument, "|")

            valor = j(1)
            If valor = "true" Then
                'agregar codigo
                TransaccionBl.GenerarAsientoTraslado(Session("Fecha"), Session("Anio"), Session("Mes"), Session("IdUser"))
                Dim cadena_java As String = _
                               "<script language='javascript'>function f(){radalert('<b>Se ha Procesado el Traslado de Saldos del Ejercicio</b>', 330, 100,'Mensaje de Información',alertCallBackFn); Sys.Application.remove_load(f);}; Sys.Application.add_load(f);</script>"
                ScriptManager.RegisterStartupScript(Page, GetType(Page), "AlertaX", cadena_java.ToString, False)
            Else
                Me.Image4.Visible = False
            End If
        End If
    End Sub
End Class