Public Class ParamRepLibroCajaBancos
    Inherits System.Web.UI.Page

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Me.RadComboBox3.Filter = 1
        Me.RadComboBox1.Filter = 1
        Me.RadComboBox4.Filter = 1
        If Me.Page.IsPostBack = False Then
            Me.RadDatePicker1.SelectedDate = Date.Parse(Session("Fecha"))
        End If
    End Sub

    Protected Sub ImageButton1_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles ImageButton1.Click
        Dim idProyecto As Int32 = 0
        Dim CuentaInicio As String = ""
        Dim CuentaFin As String = ""

        If Me.RadComboBox3.SelectedValue.Length > 0 Then
            idProyecto = Me.RadComboBox3.SelectedValue
        End If
        If Me.RadComboBox1.SelectedValue.Length > 0 Then
            CuentaInicio = Me.RadComboBox1.SelectedValue
        End If
        If Me.RadComboBox4.SelectedValue.Length > 0 Then
            CuentaFin = Me.RadComboBox4.SelectedValue
        End If

        Dim cadena_java As String
        cadena_java = "<script type='text/javascript'> " & _
                                     " window.open('ReporteLibroCajaBancos.aspx?Fecha=" + Me.RadDatePicker1.SelectedDate.ToString + "&idProyecto=" + idProyecto.ToString + "&CuentaInicio=" + CuentaInicio.ToString + "&CuentaFin=" + CuentaFin.ToString + "' ); " & _
                                     Chr(60) & "/script>"
        ScriptManager.RegisterStartupScript(Page, GetType(Page), "Cerrar", cadena_java.ToString, False)
    End Sub
End Class