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
Imports System.Web.UI
Imports System.IO
Public Class ControlIntegrante
    Inherits System.Web.UI.UserControl
    Private _dataItem As Object = Nothing
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Me.RadComboBox1.Filter = 1
    End Sub
    Public Property DataItem() As Object
        Get
            Return Me._dataItem
        End Get
        Set(ByVal value As Object)
            Me._dataItem = value
        End Set
    End Property
    Protected Sub Page_DataBinding(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.DataBinding
        If TypeOf DataItem Is GridInsertionObject Then
            'Me.txtDate1.Text = Now
        Else
            Me.RadComboBox1.SelectedValue = DataBinder.Eval(DataItem, "IdPersonal")
            If Not DataBinder.Eval(DataItem, "IdVehiculo").Equals(DBNull.Value) Then
                Me.DropDownList1.SelectedValue = DataBinder.Eval(DataItem, "IdVehiculo")
            End If

            If DataBinder.Eval(DataItem, "Conduce") = "S" Then
                Me.CheckBox2.Checked = True
            Else
                Me.CheckBox2.Checked = False
            End If

            Me.TextBox5.Text = DataBinder.Eval(DataItem, "Observacion")

        End If
    End Sub

    'Function SaveFile(ByVal file As HttpPostedFile, ByVal ruta As String, ByVal nombre As String, ByVal objetoLoad As FileUpload) As String

    '    Dim savePath As String = Server.MapPath(ruta)
    '    Dim fileName As String = FileUploadControl.FileName
    '    Dim pathToCheck As String = savePath + fileName
    '    Dim tempfileName As String

    '    tempfileName = ""

    '    nombre += (objetoLoad.FileName)
    '    savePath += "\" + nombre
    '    'Guarda archivo
    '    Try
    '        objetoLoad.SaveAs(savePath)
    '        Return nombre
    '    Catch ex As Exception
    '        Return ""
    '    End Try

    'End Function
    'Protected Sub ibSubirArchivo_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles ibSubirArchivo.Click
    '    Dim fileExt As String
    '    Dim archivo As String

    '    fileExt = System.IO.Path.GetExtension(FileUploadControl.FileName)

    '    If (fileExt = ".doc" Or fileExt = ".pdf" Or fileExt = ".docx" Or fileExt = ".gif") Then
    '        archivo = SaveFile(FileUploadControl.PostedFile, "~\Monitoreo\Adjuntos\", "Adjunto", FileUploadControl)
    '        Me.TextBox3.Text = archivo
    '        Me.ImageButton13.Attributes.Add("onClick", "CargarVentana('" & Me.TextBox3.Text.ToString & "'); return false;")
    '    Else
    '        Me.lblMsn.Visible = True
    '        Me.lblMsn.ForeColor = Drawing.Color.Red
    '        Me.lblMsn.Text = "Solo se permiten archivos de formato .doc, .docx, .pdf o .Gif"
    '    End If
    'End Sub

    
End Class