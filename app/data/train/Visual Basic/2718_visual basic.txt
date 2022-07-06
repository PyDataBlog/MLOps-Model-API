Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.BussinesLogic
Public Class VentanaRegistroFacturaDetalle
    Inherits System.Web.UI.Page
    Dim detOcFac As IDetOcxFacturaBL
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        If Not Page.IsPostBack Then
            lblDocumento.Text = Request.QueryString("Doc")
            lblTipo.Text = Request.QueryString("tip")
            lblMonto.Text = Request.QueryString("Mont")
            If Request.QueryString("tipoRend") = 1 Then
                rdbPrecio.Checked = True
                rdbCantidad.Enabled = False

                GridView1.Columns(2).Visible = True
                GridView1.Columns(3).Visible = False

                GridView2.Columns(3).HeaderText = "Monto"
                GridView2.Columns(4).Visible = False
                GridView2.Columns(5).Visible = False
            Else
                'rdbPrecio.Enabled = False
            End If
            'If Session("fact") = "True" Then
            '    GridView2.Columns(0).Visible = False
            'End If
            If GridView2.Rows.Count > 0 Then
                rdbPrecio.Enabled = False
                rdbCantidad.Enabled = False
            End If
        End If
    End Sub

    Protected Sub btnGuardar_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnGuardar.Click
        detOcFac = New DetOcxFacturaBL
        Dim lista As New List(Of DetOcxFactura)
        If rdbCantidad.Checked = True Then
            For i = 0 To GridView1.Rows.Count - 1
                Dim cbSelec As CheckBox = CType(Me.GridView1.Rows(i).FindControl("cbDetalle"), CheckBox)
                If cbSelec.Checked = True Then
                    Dim detOc As New DetOcxFactura
                    Dim txtIdDetalleOc As TextBox = CType(Me.GridView1.Rows(i).FindControl("txtIdDetOc"), TextBox)
                    Dim txtCantidad As TextBox = CType(Me.GridView1.Rows(i).FindControl("txtCantidadDet"), TextBox)
                    detOc.IdOcxFactura = CInt(Request.QueryString("idOcfac"))
                    detOc.IdDetalleOrdenCompra = CInt(txtIdDetalleOc.Text)
                    detOc.Cantidad = CDec(txtCantidad.Text)
                    lista.Add(detOc)
                End If
            Next
            detOcFac.SaveDetOcxFactura1(lista)
        Else
            If rdbPrecio.Checked = True Then
                For i = 0 To GridView1.Rows.Count - 1
                    Dim cbSelec As CheckBox = CType(Me.GridView1.Rows(i).FindControl("cbDetalle"), CheckBox)
                    If cbSelec.Checked = True Then
                        Dim detOc As New DetOcxFactura
                        Dim txtIdDetalleOc As TextBox = CType(Me.GridView1.Rows(i).FindControl("txtIdDetOc"), TextBox)
                        Dim txtMonto As TextBox = CType(Me.GridView1.Rows(i).FindControl("txtPrecioDet"), TextBox)
                        detOc.IdOcxFactura = CInt(Request.QueryString("idOcfac"))
                        detOc.IdDetalleOrdenCompra = CInt(txtIdDetalleOc.Text)
                        detOc.PCompraDetalle = CDec(txtMonto.Text)
                        lista.Add(detOc)
                    End If
                Next
                detOcFac.SaveDetOcxFacturaPrec1(lista)
                GridView2.Columns(3).HeaderText = "Monto"
                GridView2.Columns(4).Visible = False
                GridView2.Columns(5).Visible = False
            End If
        End If
        GridView1.DataBind()
        GridView2.DataBind()
    End Sub

    Protected Sub GridView2_RowCommand(ByVal sender As Object, ByVal e As System.Web.UI.WebControls.GridViewCommandEventArgs) Handles GridView2.RowCommand
        detOcFac = New DetOcxFacturaBL
        Dim index = e.CommandArgument
        If e.CommandName = "Eliminar" Then
            Dim txtIdDetalleRequerimiento As TextBox = CType(GridView2.Rows(index).FindControl("txtIdDet"), TextBox)
            detOcFac.DeleteDetOcxFactura(CInt(txtIdDetalleRequerimiento.Text))
            GridView2.DataBind()
            GridView1.DataBind()
        End If
    End Sub
    Function Validar(ByVal cant As Decimal) As Boolean
        If cant = 0 Then
            Return False
        Else
            Return True
        End If
    End Function
    Protected Sub LinkButton1_Click(ByVal sender As Object, ByVal e As EventArgs) Handles LinkButton1.Click
        Response.Redirect("VentanaRegistroFactura.aspx?idoc=" + Session("id") + "&nro=" + Session("NroOrden") + "&are=" + Session("ar") + "&fac=" + Session("fact") + "&Mont=" + Session("Monto") + "&Moned=" + Session("Moneda"))
    End Sub

    Protected Sub btnFinalizar_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnFinalizar.Click
        detOcFac = New DetOcxFacturaBL
        detOcFac.FinalizarRegistroDetOcxFactura(CInt(Request.QueryString("idOcfac")))
        Response.Redirect("VentanaRegistroFactura.aspx?idoc=" + Session("id") + "&nro=" + Session("NroOrden") + "&are=" + Session("ar") + "&fac=" + Session("fact") + "&Mont=" + Session("Monto") + "&Moned=" + Session("Moneda"))
    End Sub

    Protected Sub rdbPrecio_CheckedChanged(ByVal sender As Object, ByVal e As EventArgs) Handles rdbPrecio.CheckedChanged
        If rdbPrecio.Checked = True Then
            GridView1.Columns(2).Visible = True
            GridView1.Columns(3).Visible = False
        End If
    End Sub

    Protected Sub rdbCantidad_CheckedChanged(ByVal sender As Object, ByVal e As EventArgs) Handles rdbCantidad.CheckedChanged
        If rdbCantidad.Checked = True Then
            GridView1.Columns(2).Visible = False
            GridView1.Columns(3).Visible = True
        End If
    End Sub
End Class