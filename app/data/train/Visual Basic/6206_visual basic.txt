Imports SistFoncreagro.BussinesLogic
Imports SistFoncreagro.BussinessEntities
Imports System.Xml

Public Class FrmEditarArea
    Inherits System.Web.UI.Page
    Dim areaBL As New AreaBL
    Dim proyectoBL As New ProyectoBL
    Dim proyectoAreaBL As New ProyectoAreaBL
    Dim area As Area
    Dim areaSuperior As New Area
    Dim IdSuperArea As Int32
    Dim IdArea As Int32
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        IdSuperArea = CInt(Me.Request.QueryString("Dependencia"))
        IdArea = CInt(Me.Request.QueryString("IdArea"))
        ' IdArea = 0
        ' IdSuperArea = 1


        If Not Page.IsPostBack Then
            If IdArea > 0 Then
                Dim argArea As New ArgumentosArea(0, 0, 10000, 0, 0)
                area = areaBL.GetAREAByIdArea(IdArea, argArea)
                Me.XmlDataSource1.XPath = "TipoAreas/" & area.TipoArea
                Me.ImageButton5.Visible = True
            Else
                Me.ImageButton5.Visible = False
                RadTabStrip1.Tabs.Item(1).Enabled = False
                If IdSuperArea > 0 Then
                    Dim argSuperArea As New ArgumentosArea(0, 0, 0, 0, 0)
                    areaSuperior = areaBL.GetAREAByIdArea(IdSuperArea, argSuperArea)
                    Me.lblDependencia.Text = Me.areaSuperior.Nombre
                    If Not IsDBNull(areaSuperior.Nivel) Then
                        Me.txtNivel.Text = CStr(areaSuperior.Nivel + 1)
                    End If
                    If Not IsDBNull(areaSuperior.TipoArea) Then
                        Dim tipoArea As String
                        tipoArea = areaBL.GetTipoAreaDeSubArea(IdSuperArea)

                        If String.IsNullOrEmpty(tipoArea) Then
                            'lee el xml
                            Me.XmlDataSource1.XPath = "TipoAreas/" & areaSuperior.TipoArea & "/tipoArea"
                        Else
                            Me.XmlDataSource1.XPath = "TipoAreas/" & tipoArea
                        End If
                        'Select Case (areaSuperior.TipoArea)
                        '    Case "general"
                        '        Me.lblTipoArea.Text = "Sub Area"
                        '    Case "subarea"
                        '        Me.lblTipoArea.Text = "Sub Area"
                        'End Select
                    End If
                Else
                    Me.lblDependencia.Text = "No tiene dependencia"
                    ' tenemos que leer el XML para sacar los tipos que puede tener el nivel superior al
                    Dim primeraArea As Area
                    Dim argPrimeraArea As New ArgumentosArea(0, 0, 0, 0, 0)
                    primeraArea = areaBL.GetAreaSuperior(argPrimeraArea)
                    Dim xDoc As New XmlDocument()
                    xDoc.Load("~/RRHH/XML/XMLTipos.xml")

                    Dim ListaTiposArea As XmlNodeList
                    ListaTiposArea = xDoc.GetElementsByTagName("TipoAreas")
                    Dim indiceNodo As Integer = 0

                    If ListaTiposArea.Count > 0 Then
                        If Not primeraArea Is Nothing Then

                            For Each nodo As XmlNode In ListaTiposArea
                                If nodo.Value = primeraArea.TipoArea Then
                                    If indiceNodo > 0 Then
                                        indiceNodo = indiceNodo - 1
                                        Me.XmlDataSource1.XPath = "TipoAreas/" & ListaTiposArea.ItemOf(indiceNodo).Value + "/tipoArea"
                                    Else
                                        Me.XmlDataSource1.XPath = "TipoAreas/" & primeraArea.TipoArea
                                    End If
                                    Return
                                Else
                                    Me.XmlDataSource1.XPath = "TipoAreas/" & primeraArea.TipoArea
                                End If
                                indiceNodo = indiceNodo + 1
                            Next
                        Else
                            Me.XmlDataSource1.XPath = "TipoAreas/" & ListaTiposArea.ItemOf(0).Value
                        End If
                    End If

                End If

                Me.txtIdArea.Text = 0
                Me.txtSuperArea.Text = CStr(IdSuperArea)

                If Not areaSuperior Is Nothing Then
                    If areaSuperior.TipoArea <> "direccion" Then
                        If areaSuperior.ControlaProyectos Then
                            Me.cbProyectos.Checked = True
                        Else
                            Me.cbProyectos.Checked = False
                            Me.cbProyectos.Enabled = False
                        End If

                        If Not areaSuperior.Administracion Then
                            Me.cbAdministra.Checked = True
                            'Me.cbProyectos.Enabled = False
                        Else
                            Me.cbAdministra.Checked = False
                            Me.cbAdministra.Enabled = False
                        End If
                    Else
                        Me.cbAdministra.Enabled = True
                        Me.cbProyectos.Enabled = True
                    End If
                End If
            End If

            If Not area Is Nothing Then
                RadTabStrip1.Tabs.Item(1).Enabled = True
                Me.txtIdArea.Text = area.IdArea

                If area.Nivel.HasValue Then
                    Me.txtNivel.Text = area.Nivel
                End If

                If area.Dependencia.HasValue Then
                    Me.txtSuperArea.Text = area.Dependencia
                End If

                If Not area.AreaSuperior Is Nothing Then
                    Me.lblDependencia.Text = area.AreaSuperior.Nombre
                End If

                If Not String.IsNullOrEmpty(area.Nombre) Then
                    Me.txtNombreArea.Text = area.Nombre
                End If

                If Not IsDBNull(area.Administracion) Then
                    Me.cbAdministra.Checked = area.Administracion
                End If

                If area.TipoArea = "direccion" Then
                    Me.cbAdministra.Enabled = False
                    Me.cbProyectos.Enabled = False
                    Me.ImageButton5.Visible = False
                Else
                    Me.ImageButton5.Visible = True
                End If

                If Not IsDBNull(area.ControlaProyectos) Then
                    Me.cbProyectos.Checked = area.ControlaProyectos

                    If cbProyectos.Checked = True Then
                        Me.Panel1.Visible = True
                    Else
                        Me.Panel1.Visible = False
                    End If
                    'If area.ControlaProyectos = True Then
                    '    Me.RCBProyecto.Enabled = True
                    '    Me.GridView1.Enabled = True
                    'Else
                    '    Me.RCBProyecto.Enabled = False
                    '    Me.GridView1.Enabled = False
                    'End If
                End If

                'Obtenemos los proyectos del área superior en caso de que exista
                'If IdSuperArea > 0 Then
                '    Me.RCBProyecto.DataSource = proyectoBL.GetPROYECTOByIdArea(IdSuperArea)
                'Else
                '    Me.RCBProyecto.DataSource = proyectoBL.GetAllFromProyecto
                'End If
                'Me.RCBProyecto.DataTextField = "Nombre"
                'Me.RCBProyecto.DataValueField = "IdProyecto"
                'Me.RCBProyecto.DataBind()
                Posiciones.ContentUrl = "~/RRHH/Formulario/FrmPosicion.aspx?IdArea=" & txtIdArea.Text
                Posiciones.DataBind()
            Else
                Me.Panel1.Visible = False
            End If


        End If


        If Not Page.IsPostBack Then
            If Me.txtNivel.Text = 1 Then
                Me.ODSAreaPresupuestal.TypeName = "SistFoncreagro.BussinesLogic.ProyectoBL"
                Me.ODSAreaPresupuestal.SelectMethod = "GetAllFromProyecto"

                Me.RCBProyecto.DataSourceID = "ODSAreaPresupuestal"
                Me.RCBProyecto.DataBind()

            ElseIf Me.txtNivel.Text > 1 Then
                Me.ODSAreaPresupuestal.TypeName = "SistFoncreagro.BussinesLogic.ProyectoAreaBL"
                Me.ODSAreaPresupuestal.SelectMethod = "GetProyectoAreaByIdAreaByProyectoVigentes"
                'Me.ODSAreaPresupuestal.SelectMethod = "GetCargoByIdCargo"
                Dim parametro1 As New System.Web.UI.WebControls.Parameter
                parametro1.Name = "IdArea"
                parametro1.Type = TypeCode.Int32
                parametro1.DefaultValue = CInt(Me.txtSuperArea.Text)
                Me.ODSAreaPresupuestal.SelectParameters.Add(parametro1)

                Me.RCBProyecto.DataSourceID = "ODSAreaPresupuestal"
                Me.RCBProyecto.DataBind()
                'Me.ODSAreaPresupuestal.SelectParameters("IdArea").DefaultValue = 1
                'Me.ODSAreaPresupuestal.DataBind()
            End If
        End If
    End Sub

    Protected Sub RCBProyecto_ItemDataBound(ByVal sender As Object, ByVal e As Telerik.Web.UI.RadComboBoxItemEventArgs) Handles RCBProyecto.ItemDataBound
        If txtNivel.Text = 1 Then
            e.Item.Text = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.Proyecto)).AreaProyecto.ToString()
            e.Item.Value = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.Proyecto)).IdProyecto.ToString()
        ElseIf txtNivel.Text > 0 Then
            e.Item.Text = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.ProyectoArea)).proyectoEntity.AreaProyecto
            e.Item.Value = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.ProyectoArea)).IdProyecto.ToString()
            'e.Item.Text = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.Cargo)).DescCargo.ToString()
            'e.Item.Value = (DirectCast(e.Item.DataItem, SistFoncreagro.BussinessEntities.Cargo)).IdCargo.ToString()
        End If

    End Sub
    Protected Sub BtnAgregarProyecto_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles BtnAgregarProyecto.Click
        Dim proyectoArea As New ProyectoArea

        If Not String.IsNullOrEmpty(Me.txtIdArea.Text) Then
            If Me.txtIdArea.Text > 0 Then
                proyectoArea.IdArea = Me.txtIdArea.Text
            End If
        End If

        If RCBProyecto.SelectedIndex > -1 Then
            proyectoArea.IdProyecto = RCBProyecto.SelectedValue
        End If

        proyectoArea.IdProyectoArea = 0

        proyectoAreaBL.SaveProyectoArea(proyectoArea)
        Me.gvProyectoArea.DataBind()
        'UpdatePanel1.DataBind
        'If Not String.IsNullOrEmpty(Me.txtIdArea.Text) And Me.RCBProyecto.SelectedValue <> -1 Then
        '    If area Is Nothing Then
        '        area = New Area
        '        area.IdArea = CInt(Me.txtIdArea.Text)
        '    End If
        '    areaBL.AgregarProyectoAArea(Me.RCBProyecto.SelectedValue, area)
        '    Me.gvProyectoArea.DataBind()
        'End If
    End Sub

    Protected Sub cbProyectos_CheckedChanged(ByVal sender As Object, ByVal e As EventArgs) Handles cbProyectos.CheckedChanged
        If Me.cbProyectos.Checked = True Then
            Me.RCBProyecto.Enabled = True
            Me.gvProyectoArea.Enabled = True
        Else
            Me.RCBProyecto.Enabled = False
            Me.gvProyectoArea.Enabled = False
        End If
    End Sub


    Protected Sub btnGrabar_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles btnGrabar.Click

        Dim area As New Area
        Dim idArea As Int32
        If String.IsNullOrEmpty(Me.txtIdArea.Text) Then
            area.IdArea = 0
        Else
            area.IdArea = CInt(Me.txtIdArea.Text)
        End If
        area.Nombre = Me.txtNombreArea.Text

        If String.IsNullOrEmpty(txtSuperArea.Text) Then
            area.Dependencia = 0
        Else
            area.Dependencia = CInt(Me.txtSuperArea.Text)
        End If
        area.Administracion = Me.cbAdministra.Checked
        area.ControlaProyectos = Me.cbProyectos.Checked
        area.TipoArea = Me.DDLTipoArea.SelectedValue
        If Not String.IsNullOrEmpty(Me.txtNivel.Text) Then
            area.Nivel = CInt(Me.txtNivel.Text)
        End If

        idArea = Me.areaBL.SaveArea(area)
        Me.txtIdArea.Text = idArea
        Me.txtIdArea.DataBind()

        Response.Redirect("~\RRHH\Formulario\FrmEditarArea.aspx?Dependencia=" & area.Dependencia & "&IdArea=" & idArea)
        'Response.Redirect("~\RRHH\Formulario\FrmArea.aspx")
    End Sub

    Protected Sub GridView1_SelectedIndexChanged(ByVal sender As Object, ByVal e As EventArgs) Handles gvProyectoArea.SelectedIndexChanged

    End Sub

    Protected Sub ImageButton7_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs)

    End Sub

    Protected Sub ImageButton4_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles ImageButton4.Click
        Response.Redirect("~\RRHH\Formulario\FrmArea.aspx")
    End Sub

    Protected Sub btnEliminarPA_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs)
        Dim btn As ImageButton = CType(sender, ImageButton)
        Me.proyectoAreaBL.DeleteProyectoArea(btn.CommandArgument)
        gvProyectoArea.DataBind()
    End Sub

    Protected Sub ImageButton5_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles ImageButton5.Click
        Response.Redirect("~\RRHH\Formulario\FrmEditarArea.aspx?Dependencia=" & IdSuperArea & "&IdArea=" & 0)
    End Sub
End Class