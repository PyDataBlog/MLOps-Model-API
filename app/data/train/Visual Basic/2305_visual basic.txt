Imports SistFoncreagro.BussinesLogic
Imports SistFoncreagro.BussinessEntities
Imports System.Data
Imports Telerik.Web.UI
Imports System.Web.Services
Imports System.Data.SqlClient
Imports System.Collections.Generic
Imports System.Collections
Imports System.Configuration
Imports System
Public Class VentanaAutorizacion
    Inherits System.Web.UI.Page
    Dim Nombre As String
    Dim IdPersonal As String
    Dim IdPermisoUsuario As String
    Dim PermisoUsuario As PermisoUsuario
    Dim _PermisoUsuario As New PermisoUsuario
    Dim PermisoUsuarioBL As New PermisoUsuarioBL
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Nombre = Request.QueryString("Nombre")
        IdPersonal = Request.QueryString("IdPersonal")
        IdPermisoUsuario = Request.QueryString("IdPermisoUsuario")
        Me.Label1.Text = Nombre

        If Page.IsPostBack = False Then


            'para recuperar datos 
            PermisoUsuario = PermisoUsuarioBL.GetPERMISOUSUARIOByIdPersonal(IdPersonal)
            If PermisoUsuario.FechaPermiso.HasValue Then
                Me.RadDatePicker1.SelectedDate = PermisoUsuario.FechaPermiso
            Else
                Me.RadDatePicker1.SelectedDate = Date.Now
            End If

            If PermisoUsuario.Estado = True Then
                Me.DropDownList1.SelectedValue = 1
            Else
                Me.DropDownList1.SelectedValue = 0
            End If
            Me.TextBox12.Text = PermisoUsuario.HoraInicio
            Me.TextBox1.Text = PermisoUsuario.HoraFin
        End If

    End Sub

    Protected Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        _PermisoUsuario.Estado = Me.DropDownList1.SelectedValue
        _PermisoUsuario.FechaPermiso = Me.RadDatePicker1.SelectedDate
        _PermisoUsuario.HoraFin = Me.TextBox1.Text
        _PermisoUsuario.HoraInicio = Me.TextBox12.Text
        _PermisoUsuario.IdPersonal = IdPersonal
        _PermisoUsuario.IdPersonalAutoriza = Session("IdUser")
        _PermisoUsuario.IdPermisoUsuario = IdPermisoUsuario

        PermisoUsuarioBL.SavePERMISOUSUARIO(_PermisoUsuario)
    End Sub
End Class