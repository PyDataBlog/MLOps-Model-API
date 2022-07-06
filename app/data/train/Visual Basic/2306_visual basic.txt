Imports SistFoncreagro.BussinesLogic
Imports SistFoncreagro.BussinessEntities
Imports Telerik.Web.UI
Public Class FrmAprobarRendicion
    Inherits System.Web.UI.Page
    Dim ReciboRendicionBL As New RecibosRendicionBL
    Dim _ReciboRendicion As New RecibosRendicion
    Dim RendicionBL As New RendicionBL
    Dim _Rendicion As New Rendicion
    Dim Rendicion As Rendicion
    Dim IdRendicion As String
    Dim _DocumentoRendido As New DocumentoRendido
    Dim DocumentoRendidoBL As New DocumentoRendidoBL
    Dim ReciboBL As New ReciboBL
    Dim TransaccionBl As New TransaccionBL
    Dim EjercicioBL As New EjercicioBL
    Dim estadoMes As String = ""
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        estadoMes = EjercicioBL.GetEstadoMes(Session("Anio"), Session("Mes"))
        If estadoMes = "C" Then
            Me.ImageButton6.Visible = False
            Me.ImageButton7.Visible = False
        End If

        Me.ImageButton6.Attributes.Add("onClick", "radconfirm('¿Esta acción APROBARÁ la Rendición?', confirmCallBackFn, 330, 100, null,'Mensaje de Confirmación'); return false;")
        'If Me.RadTextBox3.Text.Length > 0 Then
        '    Me.ImageButton7.Attributes.Add("onClick", "radconfirm('¿Esta acción REGRESARÁ la Rendicion para su corrección?', confirmCallBackFn1, 330, 100, null, 'Mensaje de Confirmación'); return false;")
        'End If
       
        IdRendicion = Request.QueryString("IdRendicion")
        Me.RadTextBox2.Text = IdRendicion

        If Page.IsPostBack = False Then

            If Me.RadTextBox2.Text > 0 Then
                'para recuperar datos de rendicion
                Rendicion = RendicionBL.GetRENDICIONByIdRendicion(IdRendicion)
                Me.RadTextBox2.Text = Rendicion.IdRendicion
                Me.RadComboBox5.SelectedValue = Rendicion.IdOpFinanciera
                Me.RadComboBox4.SelectedValue = Rendicion.IdSubDiario
                Me.RadTextBox1.Text = Rendicion.Codigo
                Me.RadComboBox3.SelectedValue = Rendicion.IdProyecto
                Me.txtDate.Text = Rendicion.Fecha
                Me.RadTextBox3.Text = Rendicion.Observacion
                Me.RadNumericTextBox1.Text = RendicionBL.GetTotalRendicion(Me.RadTextBox2.Text)
            End If
        End If
    End Sub
    Protected Sub RadAjaxManager1_AjaxRequest(ByVal sender As Object, ByVal e As Telerik.Web.UI.AjaxRequestEventArgs) Handles RadAjaxManager1.AjaxRequest
        If e.Argument.StartsWith("Enviar") Then
            Dim j() As String
            Dim valor As String
            j = Split(e.Argument, "|")

            valor = j(1)
            If valor = "true" Then
                Dim IdTransaccion As Int32
                Dim codigo As String

                IdTransaccion = TransaccionBl.GenerarTRANSACCIONByIdRendicion(IdRendicion, Session("IdUser"))
                RendicionBL.UpdateEstadoRENDICION(IdRendicion, "3")
                codigo = TransaccionBl.GetTransaccionByIdTransaccion(IdTransaccion).IdSubDiario.ToString + "-" + TransaccionBl.GetTransaccionByIdTransaccion(IdTransaccion).Codigo

                Dim cadena_java As String = _
                               "<script language='javascript'>function f(){radalert('<b>Se ha generado el Comprobante: " + codigo.ToString + "</b> ', 330, 100,'Mensaje de Información',alertCallBackFn); Sys.Application.remove_load(f);}; Sys.Application.add_load(f);</script>"
                ScriptManager.RegisterStartupScript(Page, GetType(Page), "AlertaX", cadena_java.ToString, False)

                
            End If
        ElseIf e.Argument.StartsWith("Observar") Then
            Dim j() As String
            Dim valor As String
            j = Split(e.Argument, "|")

            valor = j(1)
            If valor = "true" Then
                RendicionBL.UpdateObservacionRENDICION(IdRendicion, Me.RadTextBox3.Text)
                RendicionBL.UpdateEstadoRENDICION(IdRendicion, "4")
                Response.Redirect("ListaRendicionesPorAprobar.aspx")
            End If
        ElseIf e.Argument.StartsWith("Redireccionar") Then
            Response.Redirect("ListaRendicionesPorAprobar.aspx")
        End If
    End Sub

    Protected Sub ImageButton7_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles ImageButton7.Click
        If Me.RadTextBox3.Text.Length > 0 Then
            Dim radconfirmscript As String = "<script language='javascript'>function f(){radconfirm('¿Esta acción REGRESARÁ la Rendicion para su corrección?', confirmCallBackFn1, 330, 100, null, 'Mensaje de Confirmación'); Sys.Application.remove_load(f);}; Sys.Application.add_load(f); ;</script>"
            Page.ClientScript.RegisterStartupScript(Me.[GetType](), "radconfirm", radconfirmscript)

        End If

    End Sub

    Protected Sub ImageButton9_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles ImageButton9.Click
        Dim cadena_java As String
        cadena_java = "<script type='text/javascript'> " & _
                                     " window.open('ReporteContabilizacionFondoFijo.aspx?IdRendicion=" + Me.RadTextBox2.Text + "'); " & _
                                     Chr(60) & "/script>"
        ScriptManager.RegisterStartupScript(Page, GetType(Page), "Cerrar", cadena_java.ToString, False)
    End Sub
End Class