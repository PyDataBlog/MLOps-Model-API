Imports System
Imports System.Data
Imports System.Collections
Imports System.Web.UI
Imports Telerik.Web.UI
Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.BussinesLogic
Public Class FrmBienes
    Inherits System.Web.UI.Page
    Dim _bienMueble As New BIENMUEBLE
    Dim bienMueble As BIENMUEBLE
    Dim bienMueblePadre As BIENMUEBLE
    Dim bienMuebleBL As New BienMuebleBL
    Dim IdBienMueble As String
    Dim _PlanContable1 As PlanContable
    Dim PlanContableBL As New PlanContableBL
    Dim _catalogo As Catalogo
    Dim catalogoBL As New CatalogoBL
    Dim _Responsable As New ResponsableBienMueble
    Dim ResponsableBL As New ResponsableBienMuebleBL
    Dim _EstadoBien As New EstadoBienMueble
    Dim EstadoBienBL As New EstadoBienMuebleBL
    Dim _clasificacion As Clasificacion
    Dim clasificacionBL As New ClasificacionBL
    Dim ParametrosBL As New ParametrosBL
    Dim Proveedor As New ProveedorClienteBL
    Dim _Proveedor As ProveedorCliente
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        IdBienMueble = Request.QueryString("IdBienMueble")

        Me.RadComboBox1.Filter = 1
        Me.RadComboBox2.Filter = 1
        Me.RadComboBox3.Filter = 1
        Me.RadComboBox4.Filter = 1
        Me.RadComboBox6.Filter = 1
        Me.RadComboBox7.Filter = 1
        Me.RadComboBox8.Filter = 1
        Me.RadComboBox9.Filter = 1
        Me.RadComboBox10.Filter = 1
        Me.RadComboBox11.Filter = 1

        'para agregarle un evento 
        Me.ImageButton7.Attributes.Add("onClick", "Abrir(); return false;")
        Me.ImageButton8.Attributes.Add("onClick", "Abrir1(); return false;")
        Me.TextBox3.Attributes.Add("onChange", "ponerCeros();")

        If Page.IsPostBack = False Then
            Me.txtDate.Text = Now

            If IdBienMueble > 0 Then
                'para recuperar datos de transaccion
                bienMueble = bienMuebleBL.GetBIENMUEBLEByIdBienMueble(IdBienMueble)

                _catalogo = catalogoBL.GetAllFromCatalogoByIdCatalogo(bienMueble.IdCatalogo)
                Me.RadComboBox1.DataSourceID = "odsCatalogoBusqueda"
                Me.odsCatalogoBusqueda.SelectParameters("tipo").DefaultValue = 0
                Me.odsCatalogoBusqueda.SelectParameters("parametro").DefaultValue = _catalogo.Descripcion
                Me.RadComboBox1.DataBind()
                Me.RadComboBox1.SelectedValue = bienMueble.IdCatalogo
                'PARA QUE EL CATALOGO YA NO SE PUEDA EDITAR 
                Me.RadComboBox1.Enabled = False

                Me.TextBox1.Text = bienMueble.Codigo
                Me.RadComboBox2.SelectedValue = bienMueble.IdProyecto
                If bienMueble.IdCCosto > 0 Then
                    Me.RadComboBox3.SelectedValue = bienMueble.IdCCosto
                End If
                Me.RadioButtonList1.SelectedValue = bienMueble.Tipo
                If bienMueble.Donacion = "S" Then
                    Me.CheckBox1.Checked = True
                Else
                    Me.CheckBox1.Checked = False
                End If
                Me.DropDownList1.SelectedValue = bienMueble.Conservacion
                Me.txtDate.Text = bienMueble.FechaIngreso
                Me.RadNumericTextBox3.Text = bienMueble.Costo
                If bienMueble.IdProveedorCliente > 0 Then
                    Me.RadComboBox4.SelectedValue = bienMueble.IdProveedorCliente
                End If

                If bienMueble.IdTipoDocumento > 0 Then
                    Me.DropDownList4.SelectedValue = bienMueble.IdTipoDocumento
                End If

                If bienMueble.NroDocumento > 0 Then
                    Me.TextBox2.Text = bienMueble.NroDocumento
                End If

                Me.TextBox3.Text = bienMueble.SerieDocumento

                If bienMueble.IdBienPadre > 0 Then
                    bienMueblePadre = bienMuebleBL.GetBIENMUEBLEByIdBienMueble(bienMueble.IdBienPadre)
                    Me.RadComboBox9.DataSourceID = "OdsBienPadre"
                    Me.OdsBienPadre.SelectParameters("Texto").DefaultValue = bienMueblePadre.Codigo
                    Me.RadComboBox9.DataBind()
                    Me.RadComboBox9.SelectedValue = bienMueble.IdBienPadre
                End If

                Me.TextBox10.Text = bienMueble.Descripcion

                If bienMueble.IdMarca > 0 Then
                    Me.RadComboBox10.SelectedValue = bienMueble.IdMarca
                End If

                Me.TextBox4.Text = bienMueble.Modelo
                Me.TextBox5.Text = bienMueble.Serie
                Me.TextBox6.Text = bienMueble.Placa
                Me.TextBox7.Text = bienMueble.Motor
                Me.TextBox8.Text = bienMueble.Color
                Me.TextBox9.Text = bienMueble.Otro
                Me.TextBox11.Text = bienMueble.CodigoReferencia
                Me.RadComboBox11.SelectedValue = bienMueble.IdOficina
                If bienMueble.IdPlanCuenta > 0 Then
                    _PlanContable1 = PlanContableBL.GetPLANCONTABLEByIdPlan(bienMueble.IdPlanCuenta)
                    Me.RadComboBox6.DataSourceID = "odsPlanContable1"
                    Me.odsPlanContable1.SelectParameters("Texto").DefaultValue = _PlanContable1.Nombre
                    Me.RadComboBox6.DataBind()
                    Me.RadComboBox6.SelectedValue = bienMueble.IdPlanCuenta
                End If


                If bienMueble.IdPlanDepreciacion > 0 Then
                    _PlanContable1 = PlanContableBL.GetPLANCONTABLEByIdPlan(bienMueble.IdPlanDepreciacion)
                    Me.RadComboBox7.DataSourceID = "odsPlanContable2"
                    Me.odsPlanContable2.SelectParameters("Texto").DefaultValue = _PlanContable1.Nombre
                    Me.RadComboBox7.DataBind()
                    Me.RadComboBox7.SelectedValue = bienMueble.IdPlanDepreciacion
                End If

                If bienMueble.IdPlanDepreciacion1 > 0 Then
                    _PlanContable1 = PlanContableBL.GetPLANCONTABLEByIdPlan(bienMueble.IdPlanDepreciacion1)
                    Me.RadComboBox8.DataSourceID = "odsPlanContable3"
                    Me.odsPlanContable3.SelectParameters("Texto").DefaultValue = _PlanContable1.Nombre
                    Me.RadComboBox8.DataBind()
                    Me.RadComboBox8.SelectedValue = bienMueble.IdPlanDepreciacion1
                End If

                If bienMueble.IdProveedorCliente > 0 Then
                    _Proveedor = Proveedor.GetAllFromProveedorClienteById(bienMueble.IdProveedorCliente)
                    Me.RadComboBox4.DataSourceID = "odsProveedor"
                    Me.odsProveedor.SelectParameters("Parametro").DefaultValue = _Proveedor.Ruc
                    Me.RadComboBox4.DataBind()
                    Me.RadComboBox4.SelectedValue = bienMueble.IdProveedorCliente
                End If
                
            End If

        End If

    End Sub

    Protected Sub RadComboBox1_ItemDataBound(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemEventArgs) Handles RadComboBox1.ItemDataBound
        e.Item.Text = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.Catalogo)).CodigoDescripcion.ToString()
        e.Item.Value = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.Catalogo)).IdCatalogo.ToString()
    End Sub

    Protected Sub RadComboBox1_ItemsRequested(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemsRequestedEventArgs) Handles RadComboBox1.ItemsRequested
        If e.Text.Length > 2 Then
            Me.odsCatalogoBusqueda.SelectParameters("parametro").DefaultValue = e.Text.ToString
            Me.RadComboBox1.DataBind()
        End If
    End Sub

    Protected Sub RadComboBox4_ItemDataBound(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemEventArgs) Handles RadComboBox4.ItemDataBound
        e.Item.Text = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.ProveedorCliente)).Ruc.ToString() + " " + (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.ProveedorCliente)).RazonSocial.ToString()
        e.Item.Value = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.ProveedorCliente)).IdProveedorCliente.ToString()
    End Sub

    Protected Sub RadComboBox4_ItemsRequested(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemsRequestedEventArgs) Handles RadComboBox4.ItemsRequested
        If e.Text.Length > 2 Then
            Me.RadComboBox4.DataSourceID = "odsProveedor"
            Me.odsProveedor.SelectParameters("Parametro").DefaultValue = e.Text.ToString
            Me.RadComboBox4.DataBind()
        End If
    End Sub

    Protected Sub RadComboBox6_ItemDataBound(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemEventArgs) Handles RadComboBox6.ItemDataBound
        e.Item.Text = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.PlanContable)).Cuenta.ToString()
        e.Item.Value = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.PlanContable)).IdPlan.ToString()
    End Sub

    Protected Sub RadComboBox6_ItemsRequested(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemsRequestedEventArgs) Handles RadComboBox6.ItemsRequested
        If e.Text.Length > 1 Then
            Me.RadComboBox6.DataSourceID = "odsPlanContable1"
            Me.odsPlanContable1.SelectParameters("Texto").DefaultValue = e.Text.ToString
            Me.RadComboBox6.DataBind()
        End If
    End Sub

    Protected Sub RadComboBox7_ItemDataBound(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemEventArgs) Handles RadComboBox7.ItemDataBound
        e.Item.Text = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.PlanContable)).Cuenta.ToString()
        e.Item.Value = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.PlanContable)).IdPlan.ToString()
    End Sub

    Protected Sub RadComboBox7_ItemsRequested(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemsRequestedEventArgs) Handles RadComboBox7.ItemsRequested
        If e.Text.Length > 1 Then
            Me.RadComboBox7.DataSourceID = "odsPlanContable2"
            Me.odsPlanContable2.SelectParameters("Texto").DefaultValue = e.Text.ToString
            Me.RadComboBox7.DataBind()
        End If
    End Sub

    Protected Sub RadComboBox8_ItemDataBound(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemEventArgs) Handles RadComboBox8.ItemDataBound
        e.Item.Text = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.PlanContable)).Cuenta.ToString()
        e.Item.Value = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.PlanContable)).IdPlan.ToString()
    End Sub

    Protected Sub RadComboBox8_ItemsRequested(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemsRequestedEventArgs) Handles RadComboBox8.ItemsRequested
        If e.Text.Length > 1 Then
            Me.RadComboBox8.DataSourceID = "odsPlanContable3"
            Me.odsPlanContable3.SelectParameters("Texto").DefaultValue = e.Text.ToString
            Me.RadComboBox8.DataBind()
        End If
    End Sub

    Protected Sub ImageButton3_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles ImageButton3.Click
        Dim cadena_java As String

        _bienMueble.Color = Me.TextBox8.Text
        _bienMueble.Conservacion = Me.DropDownList1.SelectedValue
        _bienMueble.Costo = Me.RadNumericTextBox3.Text
        _bienMueble.Descripcion = Me.TextBox10.Text
        If Me.CheckBox1.Checked Then
            _bienMueble.Donacion = "S"
        Else
            _bienMueble.Donacion = "N"
        End If

        _bienMueble.FechaIngreso = Me.txtDate.Text
        _bienMueble.IdBienMueble = IdBienMueble
        If Me.RadComboBox9.SelectedValue.Length > 0 Then
            _bienMueble.IdBienPadre = Me.RadComboBox9.SelectedValue
        Else
            _bienMueble.IdBienPadre.Equals(DBNull.Value)
        End If

        _bienMueble.IdCatalogo = Me.RadComboBox1.SelectedValue
        If Me.RadComboBox3.SelectedValue.Length > 0 Then
            _bienMueble.IdCCosto = Me.RadComboBox3.SelectedValue
        Else
            _bienMueble.IdCCosto.Equals(DBNull.Value)
        End If
        If Me.RadComboBox10.SelectedValue.Length > 0 Then
            _bienMueble.IdMarca = Me.RadComboBox10.SelectedValue
        Else
            _bienMueble.IdMarca.Equals(DBNull.Value)
        End If

        _bienMueble.IdOficina = Me.RadComboBox11.SelectedValue
        _bienMueble.IdPlanCuenta = Me.RadComboBox6.SelectedValue
        If Me.RadComboBox7.SelectedValue.Length > 0 Then
            _bienMueble.IdPlanDepreciacion = Me.RadComboBox7.SelectedValue
        Else
            _bienMueble.IdPlanDepreciacion.Equals(DBNull.Value)
        End If
        If Me.RadComboBox8.SelectedValue.Length > 0 Then
            _bienMueble.IdPlanDepreciacion1 = Me.RadComboBox8.SelectedValue
        Else
            _bienMueble.IdPlanDepreciacion1.Equals(DBNull.Value)
        End If
        If Me.RadComboBox4.SelectedValue.Length > 0 Then
            _bienMueble.IdProveedorCliente = Me.RadComboBox4.SelectedValue
        Else
            _bienMueble.IdProveedorCliente.Equals(DBNull.Value)
        End If

        _bienMueble.IdProyecto = Me.RadComboBox2.SelectedValue
        If Me.DropDownList4.SelectedValue > 0 Then
            _bienMueble.IdTipoDocumento = Me.DropDownList4.SelectedValue
        Else
            _bienMueble.IdTipoDocumento.Equals(DBNull.Value)
        End If

        _bienMueble.IdUsuario = Session("IdUser")
        _bienMueble.Modelo = Me.TextBox4.Text
        _bienMueble.Motor = Me.TextBox7.Text
        If Me.TextBox2.Text <> "" Then
            _bienMueble.NroDocumento = Me.TextBox2.Text
        Else
            _bienMueble.NroDocumento.Equals(DBNull.Value)
        End If

        _bienMueble.Otro = Me.TextBox9.Text
        _bienMueble.Placa = Me.TextBox6.Text
        _bienMueble.Serie = Me.TextBox5.Text
        _bienMueble.SerieDocumento = Me.TextBox3.Text
        _bienMueble.Tipo = Me.RadioButtonList1.SelectedValue
        _bienMueble.CodigoReferencia = Me.TextBox11.Text

        If Me.RadioButtonList1.SelectedValue = "A" And Me.RadNumericTextBox3.Text = 0 Then
            cadena_java = _
                                    "<script type='text/javascript'> " & _
                                    " alert('¡Para que el Bien sea Activo debe ingresar el Valor de Compra!'); " & _
                                    Chr(60) & "/script>"
            ScriptManager.RegisterStartupScript(Page, GetType(Page), "AlertaX", cadena_java.ToString, False)
        Else
            If IdBienMueble > 0 Then
                bienMuebleBL.SaveBIENMUEBLE(_bienMueble)
            Else
                IdBienMueble = bienMuebleBL.SaveBIENMUEBLE(_bienMueble)
            End If
            Response.Redirect("FrmBienes.aspx?IdBienMueble=" + IdBienMueble.ToString)
        End If


    End Sub

    Protected Sub RadComboBox9_ItemDataBound(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemEventArgs) Handles RadComboBox9.ItemDataBound
        e.Item.Text = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.BIENMUEBLE)).Bien.ToString()
        e.Item.Value = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.BIENMUEBLE)).IdBienMueble.ToString()
    End Sub
    Protected Sub RadComboBox9_ItemsRequested(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemsRequestedEventArgs) Handles RadComboBox9.ItemsRequested
        If e.Text.Length > 2 Then
            Me.RadComboBox9.DataSourceID = "OdsBienPadre"
            Me.OdsBienPadre.SelectParameters("Texto").DefaultValue = e.Text.ToString
            Me.RadComboBox9.DataBind()
        End If
    End Sub

    Protected Sub ImageButton4_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles ImageButton4.Click
        Response.Redirect("FrmBienes.aspx?IdBienMueble=0")
    End Sub

    Protected Sub RadGrid1_InsertCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RadGrid1.InsertCommand
        Dim userControl As UserControl = CType(e.Item.FindControl(GridEditFormItem.EditFormUserControlID), UserControl)

        If CType(userControl.FindControl("txtDate2"), TextBox).Text.Length > 0 Then
            _Responsable.FechaFin = CType(userControl.FindControl("txtDate2"), TextBox).Text
        Else
            _Responsable.FechaFin.Equals(DBNull.Value)

        End If
        _Responsable.FechaInicio = CType(userControl.FindControl("txtDate1"), TextBox).Text
        _Responsable.IdBienMueble = IdBienMueble
        _Responsable.IdPersonal = CType(userControl.FindControl("RadComboBox1"), RadComboBox).SelectedValue
        _Responsable.Observacion = CType(userControl.FindControl("TextBox1"), TextBox).Text

        ResponsableBL.SaveRESPONSABLEBIENMUEBLE(_Responsable)

    End Sub

    Protected Sub RadGrid1_DeleteCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RadGrid1.DeleteCommand
        Dim editedItem As GridEditableItem = CType(e.Item, GridEditableItem)
        Dim id As Int32
        id = editedItem.OwnerTableView.DataKeyValues(editedItem.ItemIndex)("IdResponsable")
        ResponsableBL.DeleteRESPONSABLEBIENMUEBLE(id)
    End Sub

   
    Protected Sub RadGrid1_UpdateCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RadGrid1.UpdateCommand
        Dim userControl As UserControl = CType(e.Item.FindControl(GridEditFormItem.EditFormUserControlID), UserControl)
        Dim editedItem As GridEditableItem = CType(e.Item, GridEditableItem)
       
        _Responsable.IdResponsable = editedItem.OwnerTableView.DataKeyValues(editedItem.ItemIndex)("IdResponsable")

        If CType(userControl.FindControl("txtDate2"), TextBox).Text.Length > 0 Then
            _Responsable.FechaFin = CType(userControl.FindControl("txtDate2"), TextBox).Text
        Else
            _Responsable.FechaFin.Equals(DBNull.Value)

        End If
        _Responsable.FechaInicio = CType(userControl.FindControl("txtDate1"), TextBox).Text
        _Responsable.IdBienMueble = IdBienMueble
        _Responsable.IdPersonal = CType(userControl.FindControl("RadComboBox1"), RadComboBox).SelectedValue
        _Responsable.Observacion = CType(userControl.FindControl("TextBox1"), TextBox).Text

        ResponsableBL.SaveRESPONSABLEBIENMUEBLE(_Responsable)

    End Sub

    Protected Sub RadGrid2_InsertCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RadGrid2.InsertCommand
        Dim userControl As UserControl = CType(e.Item.FindControl(GridEditFormItem.EditFormUserControlID), UserControl)

        _EstadoBien.FechaInicio = CType(userControl.FindControl("txtDate1"), TextBox).Text
        _EstadoBien.IdBienMueble = IdBienMueble
        _EstadoBien.IdTipo = CType(userControl.FindControl("RadComboBox1"), RadComboBox).SelectedValue
        _EstadoBien.Observacion = CType(userControl.FindControl("TextBox1"), TextBox).Text
        _EstadoBien.DocumentoBaja = CType(userControl.FindControl("TextBox3"), TextBox).Text
        If CType(userControl.FindControl("RadComboBox2"), RadComboBox).Text.Length > 0 Then
            _EstadoBien.IdMotivo = CType(userControl.FindControl("RadComboBox2"), RadComboBox).SelectedValue
        Else
            _EstadoBien.IdMotivo.Equals(DBNull.Value)
        End If

        If CType(userControl.FindControl("CheckBox1"), CheckBox).Checked Then
            _EstadoBien.BajaContable = "S"
        Else
            _EstadoBien.BajaContable = "N"
        End If
        If CType(userControl.FindControl("CheckBox2"), CheckBox).Checked Then
            _EstadoBien.BajaPatrimonio = "S"
        Else
            _EstadoBien.BajaPatrimonio = "N"
        End If

        EstadoBienBL.SaveESTADOBIENMUEBLE(_EstadoBien)
    End Sub

    Protected Sub RadGrid2_UpdateCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RadGrid2.UpdateCommand
        Dim userControl As UserControl = CType(e.Item.FindControl(GridEditFormItem.EditFormUserControlID), UserControl)
        Dim editedItem As GridEditableItem = CType(e.Item, GridEditableItem)

        _EstadoBien.IdEstado = editedItem.OwnerTableView.DataKeyValues(editedItem.ItemIndex)("IdEstado")

        _EstadoBien.FechaInicio = CType(userControl.FindControl("txtDate1"), TextBox).Text
        _EstadoBien.IdBienMueble = IdBienMueble
        _EstadoBien.IdTipo = CType(userControl.FindControl("RadComboBox1"), RadComboBox).SelectedValue
        _EstadoBien.Observacion = CType(userControl.FindControl("TextBox1"), TextBox).Text
        _EstadoBien.DocumentoBaja = CType(userControl.FindControl("TextBox3"), TextBox).Text
        If CType(userControl.FindControl("RadComboBox2"), RadComboBox).Text.Length > 0 Then
            _EstadoBien.IdMotivo = CType(userControl.FindControl("RadComboBox2"), RadComboBox).SelectedValue
        Else
            _EstadoBien.IdMotivo.Equals(DBNull.Value)
        End If
        If CType(userControl.FindControl("CheckBox1"), CheckBox).Checked Then
            _EstadoBien.BajaContable = "S"
        Else
            _EstadoBien.BajaContable = "N"
        End If
        If CType(userControl.FindControl("CheckBox2"), CheckBox).Checked Then
            _EstadoBien.BajaPatrimonio = "S"
        Else
            _EstadoBien.BajaPatrimonio = "N"
        End If

        EstadoBienBL.SaveESTADOBIENMUEBLE(_EstadoBien)
    End Sub

    Protected Sub RadGrid2_DeleteCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RadGrid2.DeleteCommand
        Dim editedItem As GridEditableItem = CType(e.Item, GridEditableItem)
        Dim id As Int32
        id = editedItem.OwnerTableView.DataKeyValues(editedItem.ItemIndex)("IdEstado")
        EstadoBienBL.DeleteESTADOBIENMUEBLE(id)
    End Sub

    Protected Sub RadAjaxManager1_AjaxRequest(ByVal sender As Object, ByVal e As Telerik.Web.UI.AjaxRequestEventArgs) Handles RadAjaxManager1.AjaxRequest
        If e.Argument = "ActualizarLista" Then
            Me.RadComboBox4.DataBind()
        ElseIf e.Argument = "ActualizarLista1" Then
            Me.RadComboBox10.DataBind()
        End If

    End Sub

    Protected Sub RadComboBox1_SelectedIndexChanged(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxSelectedIndexChangedEventArgs) Handles RadComboBox1.SelectedIndexChanged
        _clasificacion = clasificacionBL.GetCLASIFICACIONByIdCatalogo(Me.RadComboBox1.SelectedValue)


        If _clasificacion.IdPlanActivoFijo > 0 Then
            _PlanContable1 = PlanContableBL.GetPLANCONTABLEByIdPlan(_clasificacion.IdPlanActivoFijo)
            Me.RadComboBox6.DataSourceID = "odsPlanContable1"
            Me.odsPlanContable1.SelectParameters("Texto").DefaultValue = _PlanContable1.Nombre
            Me.RadComboBox6.DataBind()
            Me.RadComboBox6.SelectedValue = _clasificacion.IdPlanActivoFijo
        End If


        If _clasificacion.IdPlanDepreciacion > 0 Then
            _PlanContable1 = PlanContableBL.GetPLANCONTABLEByIdPlan(_clasificacion.IdPlanDepreciacion)
            Me.RadComboBox7.DataSourceID = "odsPlanContable2"
            Me.odsPlanContable2.SelectParameters("Texto").DefaultValue = _PlanContable1.Nombre
            Me.RadComboBox7.DataBind()
            Me.RadComboBox7.SelectedValue = _clasificacion.IdPlanDepreciacion
        End If


        If _clasificacion.IdPlanDepreciacion1 > 0 Then
            _PlanContable1 = PlanContableBL.GetPLANCONTABLEByIdPlan(_clasificacion.IdPlanDepreciacion1)
            Me.RadComboBox8.DataSourceID = "odsPlanContable3"
            Me.odsPlanContable3.SelectParameters("Texto").DefaultValue = _PlanContable1.Nombre
            Me.RadComboBox8.DataBind()
            Me.RadComboBox8.SelectedValue = _clasificacion.IdPlanDepreciacion1
        End If

    End Sub

    Protected Sub RadNumericTextBox3_TextChanged(ByVal sender As Object, ByVal e As EventArgs) Handles RadNumericTextBox3.TextChanged
        Dim valor As Decimal
        valor = ParametrosBL.GetValorUIT(Me.txtDate.Text).valor
        If Me.RadNumericTextBox3.Text >= valor / 4 Then
            Me.RadioButtonList1.SelectedValue = "A"
        Else
            Me.RadioButtonList1.SelectedValue = "C"
        End If
    End Sub

    Protected Sub txtDate_TextChanged(ByVal sender As Object, ByVal e As EventArgs) Handles txtDate.TextChanged
        Dim valor As Decimal
        Try
            valor = ParametrosBL.GetValorUIT(Me.txtDate.Text).valor
            If Me.RadNumericTextBox3.Text >= valor / 4 Then
                Me.RadioButtonList1.SelectedValue = "A"
            Else
                Me.RadioButtonList1.SelectedValue = "C"
            End If
        Catch ex As Exception
            Dim cadena_java As String = _
                                     "<script type='text/javascript'> " & _
                                     " Alerta('¡Debe ingresar el valor de la UIT!'); " & _
                                     Chr(60) & "/script>"
            ScriptManager.RegisterStartupScript(Page, GetType(Page), "AlertaX", cadena_java.ToString, False)
        End Try

        
    End Sub
End Class