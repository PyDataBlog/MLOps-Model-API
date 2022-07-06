Imports SistFoncreagro.BussinesLogic
Imports SistFoncreagro.BussinessEntities
Public Class FrmCotizacionEditarAprobar
    Inherits System.Web.UI.Page
    Dim cotizacionBl As New CotizacionBL
    Dim proveedorDetalleCotizBl As IProveedorDetalleCotizacionBL

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        'If Not Page.IsPostBack Then
        '    'txtIdCotizacion.Text = Request.QueryString("IdCoti")
        '    'txtIdRequerimiento.Text = Request.QueryString("IdReq")
        '    'Dim idCotizacion As String = Request.QueryString("IdCoti")
        '    'rcbProductoServicio.Filter = 1
        '    'Dim coti = cotizacionBl.GetFromCotizacionByIdCotizacion(CInt(idCotizacion.ToString))
        '    'CargarCotizacion(coti)
        'End If
        If Not Page.IsPostBack Then
            txtIdCotizacion.Text = Request.QueryString("IdCoti")
            txtIdRequerimiento.Text = Request.QueryString("IdReq")
            Dim idCotizacion As String = Request.QueryString("IdCoti")
            Dim coti = cotizacionBl.GetFromCotizacionByIdCotizacion(CInt(idCotizacion.ToString))
            CargarCotizacion(coti)
        End If
    End Sub
    Public Sub CargarCotizacion(ByVal coti As Cotizacion)
        Dim estado As Int32
        If (coti.IdMoneda = 1) Then
            lblMoneda.Text = "Soles"
        Else
            lblMoneda.Text = "Dólares"
        End If
        lblNombre.Text = coti.Nombre
        lblRequerimiento.Text = coti.NroRequerimiento
        lblEstado.Text = coti.Estado
        lblArea.Text = coti.CodigoArea
        lblObservaciones.Text = coti.Observaciones
        lblFechaRegistro.Text = coti.FechaRegistro.ToShortDateString
        If coti.Estado <> "En Espera" Then
            gvDetalleCotizacion.Columns(1).Visible = False
            For i = 0 To gvDetalleCotizacion.Rows.Count - 1
                Dim gvProveedorBloq As GridView = CType(Me.gvDetalleCotizacion.Rows(i).FindControl("gvProveedor"), GridView)
                gvProveedorBloq.Columns(0).Visible = False
                gvProveedorBloq.Columns(1).Visible = False
            Next
        End If
        lbAdjuntarArchivo.Attributes.Add("onClick", "CargarVentanaCotizacion(" & txtIdCotizacion.Text & "); return false;")
        lbFormato.Attributes.Add("onClick", "CargarFormato(" & txtIdCotizacion.Text & "); return false;")
        lbAnularSoul.Attributes.Add("onClick", "AnularCotizacion(); return false;")
        'CargarDatos(coti.IdCotizacion)
        'CargarParrafo(coti.IdTipoCotizacion, estado)
    End Sub

    Protected Sub lbAprobarOc_Click(ByVal sender As Object, ByVal e As EventArgs) Handles lbAprobarOc.Click
        cotizacionBl = New CotizacionBL
        cotizacionBl.AprobarSoulSource(Request.QueryString("IdCoti"), Session("IdUser"))
        Response.Redirect("FrmListaCotizacionesPorAprobar.aspx")
    End Sub

    Protected Sub RadAjaxManager1_AjaxRequest(ByVal sender As Object, ByVal e As Telerik.Web.UI.AjaxRequestEventArgs) Handles RadAjaxManager1.AjaxRequest
        If e.Argument = "AnularCotizacion" Then
            cotizacionBl.AnularSoulSource(CInt(txtIdCotizacion.Text), Session("IdUser"))
            Response.Redirect("FrmListaCotizacionesPorAprobar.aspx")
        End If
    End Sub
End Class