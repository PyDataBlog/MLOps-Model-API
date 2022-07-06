Imports System
Imports System.Data
Imports System.Collections
Imports System.Web.UI
Imports Telerik.Web.UI
Imports SistFoncreagro.BussinessEntities
Imports SistFoncreagro.BussinesLogic

Public Class FrmProyectoInformes
    Inherits System.Web.UI.Page

    Function Ruta(ByVal NomArch As String) As String
        Return "~\Archivos\Monitoreo\InformesProyecto\" + NomArch
    End Function

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        If Not IsPostBack Then
            Dim miId As String
            miId = Request.QueryString("IdProyecto")
            HIdProyecto.Value = miId
        End If
    End Sub

    Protected Sub RGInforme_InsertCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RGInforme.InsertCommand
        RGInforme.DataBind()
    End Sub

    Protected Sub RGInforme_ItemCommand(ByVal sender As Object, ByVal e As Telerik.Web.UI.GridCommandEventArgs) Handles RGInforme.ItemCommand
        If e.CommandName = "Ver" Then
            Dim _Archivo As String
            Dim editeditem As GridEditableItem = CType(e.Item, GridEditableItem)
            _Archivo = RGInforme.Items(editeditem.ItemIndex)("Archivo").Text
            
            System.Diagnostics.Process.Start("~\Archivos\Monitoreo\InformesProyecto\" + _Archivo)

            ''Dim assembly As System.Reflection.Assembly = _
            ''      System.Reflection.Assembly.GetExecutingAssembly()
            ''Dim root As String = assembly.GetName().Name
            ''Dim stream As System.IO.Stream = assembly.GetManifestResourceStream(root + "." + _Archivo)
            ''Dim buffer(Convert.ToInt32(stream.Length) - 1) As Byte
            ''stream.Read(buffer, 0, buffer.Length)
            ''stream.Close()
            ''Dim f As New IO.FileStream(_Archivo, IO.FileMode.Create, IO.FileAccess.Write)
            ''f.Write(buffer, 0, buffer.Length)
            ''f.Close()

            ''Process.Start(_Archivo)

            'Dim myFile As ProcessStartInfo
            'If (System.IO.File.Exists("\\servfoncreagro\c$\Inetpub\wwwroot\Archivos\Monitoreo\InformesProyecto\" + _Archivo)) Then
            Try
                'myFile = New ProcessStartInfo(Server.MapPath("~\Archivos\Monitoreo\InformesProyecto\" + _Archivo))
                'myFile = New ProcessStartInfo("\\servfoncreagro\c$\Inetpub\wwwroot\Archivos\Monitoreo\InformesProyecto\" + _Archivo)
                ''myFile.UseShellExecute = False
                ''myFile.RedirectStandardOutput = True
                'Process.Start(myFile)

            Catch ex As Exception
                RWM1.RadAlert(ex.Message.ToString, 300, 100, "Error", "")
            End Try

        ElseIf e.CommandName = "Borrar" Then
            Dim _Archivo As String
            Dim editeditem As GridEditableItem = CType(e.Item, GridEditableItem)
            _Archivo = Trim(RGInforme.Items(editeditem.ItemIndex)("Archivo").Text)

            Dim mIdInfProy As Int32
            mIdInfProy = Integer.Parse(editeditem.OwnerTableView.DataKeyValues(editeditem.ItemIndex)("IdInfProy"))

            Dim _Informe As New InformeProyecto
            Dim _InformeBL As New InformeProyectoBL

            Try
                If _Archivo <> "" And _Archivo <> "&nbsp;" Then
                    'Dim archivo As String = ("\\servfoncreagro\c$\Inetpub\wwwroot\Archivos\Monitoreo\InformesProyecto\" + _Archivo)
                    Dim archivo As String = (Server.MapPath("~\Archivos\Monitoreo\InformesProyecto\" + _Archivo))
                    My.Computer.FileSystem.DeleteFile(archivo)
                    Dim _ArchivoBL As New AdjMonitBL
                    _Informe = _InformeBL.GetAllFromInformeProyectoById(mIdInfProy)
                    _ArchivoBL.DeleteAdjMonit(_Informe.IdAdjMonit)
                End If
                _InformeBL.DeleteInformeProyecto(mIdInfProy)
            Catch ex As Exception
                MsgBox(ex.Message.ToString, MsgBoxStyle.Critical)
            End Try
            RGInforme.DataBind()
        End If
    End Sub

    Public Function ValidarFecha(ByVal Valor As Date) As String
        Dim MisFunciones As New Funciones
        Dim miFecha As String = MisFunciones.CampoFecha(Valor)
        Return miFecha
    End Function

    Protected Sub Menu1_MenuItemClick(ByVal sender As Object, ByVal e As System.Web.UI.WebControls.MenuEventArgs) Handles Menu1.MenuItemClick
        If HIdProyecto.Value = "" Then RWMensajes.RadAlert("No hay un proyecto seleccionado", 230, 100, "Error", "") : Exit Sub
        Select Case e.Item.Value
            Case "Datos"
                Response.Redirect("~\Monitoreo\Formularios\FrmDatosProyecto.aspx?IdProyecto=" + HIdProyecto.Value)

            Case "Convenio"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoConvenio.aspx?IdProyecto=" + HIdProyecto.Value)

            Case "Componentes"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoComponentes.aspx?IdProyecto=" + HIdProyecto.Value)

            Case "Actividades"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoActividad.aspx?IdProyecto=" + HIdProyecto.Value)

            Case "CCostos"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoCCosto.aspx?IdProyecto=" + HIdProyecto.Value)

            Case "Ambito"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoAmbito.aspx?IdProyecto=" + HIdProyecto.Value)

            Case "Informes"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoInformes.aspx?IdProyecto=" + HIdProyecto.Value)

            Case "ForFin"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoPresupuesto.aspx?IdProyecto=" + HIdProyecto.Value)

            Case "ForAnual"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoPresupuestoAnual.aspx?IdProyecto=" + HIdProyecto.Value)

            Case "AvanFis"
                Response.Redirect("~\Monitoreo\Formularios\FrmProyectoAvanceFisico.aspx?IdProyecto=" + HIdProyecto.Value)
        End Select
    End Sub
End Class