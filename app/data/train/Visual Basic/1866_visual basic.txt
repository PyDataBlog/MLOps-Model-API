Option Strict Off
Option Explicit On
Friend Class frmDiagPtArtn
	Inherits System.Windows.Forms.Form
	'**************************************************
	'* frmDiagPtArtn version info:
	'*  See basGlobals (Globals.bas).
	'**************************************************
	
	
	Private Const TBarButtons As String = "Exit;"
	
	Public Sub IPAHelpPrint(ByRef bToPrinter As Boolean, ByRef bDummyArgument As Boolean)
		
		On Error Resume Next
		
		'UPGRADE_NOTE: Capture was upgraded to Capture_Renamed. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="A9E4979A-37FA-4718-9994-97DD76ED70A7"'
		Dim Capture_Renamed As clsCapture
		If (bToPrinter) Then
			Capture_Renamed = New clsCapture
			Capture_Renamed.PrintChart((Picture1.Image), "Chart:" & vbTab & vbTab & "Points of Articulation")
			'UPGRADE_NOTE: Object Capture_Renamed may not be destroyed until it is garbage collected. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6E35BFF6-CD74-4B09-9689-3E1A43DF8969"'
			Capture_Renamed = Nothing
		Else
			My.Computer.Clipboard.Clear()
			My.Computer.Clipboard.SetImage(Picture1.Image)
		End If
		
	End Sub
	
	'UPGRADE_WARNING: Form event frmDiagPtArtn.Activate has a new behavior. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6BA9B8D2-2A32-4B6E-8D36-44949974A5B4"'
	Private Sub frmDiagPtArtn_Activated(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Activated
		
		On Error Resume Next
		
		With mdiHelpCharts
			.ShowTBarButtons(TBarButtons)
			CType(.Controls("panStatus"), Object).Visible = True
			CType(.Controls("mnuTest"), Object).Enabled = False
			CType(.Controls("mnuExportBitmap"), Object).Visible = True
			CType(.Controls("mnuBkgrdColor"), Object)(1).Enabled = False
			CType(.Controls("mnuPrint"), Object)(0).Visible = True
			CType(.Controls("mnuPrint"), Object)(1).Visible = True
		End With
		
		gStatLine.Text = ""
		'UPGRADE_ISSUE: Unable to determine which constant to upgrade vbNormal to. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="B3B44E51-B5F1-4FD7-AA29-CAD31B71F487"'
		If WindowState = vbNormal Then
			Top = VB6.TwipsToPixelsY(-VB6.PixelsToTwipsY(Height))
			Show()
			WindowState = System.Windows.Forms.FormWindowState.Maximized
		End If
		
		
	End Sub
	
	'UPGRADE_WARNING: Form event frmDiagPtArtn.Deactivate has a new behavior. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6BA9B8D2-2A32-4B6E-8D36-44949974A5B4"'
	Private Sub frmDiagPtArtn_Deactivate(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Deactivate
		
		On Error Resume Next
		gStatLine.Text = ""
		CType(mdiHelpCharts.Controls("mnuExportBitmap"), Object).Visible = False
		CType(mdiHelpCharts.Controls("mnuBkgrdColor"), Object)(1).Enabled = True
		CType(mdiHelpCharts.Controls("mnuPrint"), Object)(0).Visible = False
		CType(mdiHelpCharts.Controls("mnuPrint"), Object)(1).Visible = False
		
	End Sub
	
	Private Sub frmDiagPtArtn_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load
		
		mdiHelpCharts.panStatus.Visible = True
		Top = VB6.TwipsToPixelsY(-VB6.PixelsToTwipsY(Height))
		Show()
		WindowState = System.Windows.Forms.FormWindowState.Maximized
		
	End Sub
	
	Private Sub frmDiagPtArtn_MouseMove(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.MouseEventArgs) Handles MyBase.MouseMove
		Dim Button As Short = eventArgs.Button \ &H100000
		Dim Shift As Short = System.Windows.Forms.Control.ModifierKeys \ &H10000
		Dim X As Single = VB6.PixelsToTwipsX(eventArgs.X)
		Dim y As Single = VB6.PixelsToTwipsY(eventArgs.Y)
		
		On Error Resume Next
		gStatLine.Text = ""
		
	End Sub
	
	Private Sub frmDiagPtArtn_FormClosing(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
		Dim Cancel As Boolean = eventArgs.Cancel
		Dim UnloadMode As System.Windows.Forms.CloseReason = eventArgs.CloseReason
		
		On Error Resume Next
		'UPGRADE_WARNING: Form event frmDiagPtArtn.Deactivate has a new behavior. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6BA9B8D2-2A32-4B6E-8D36-44949974A5B4"'
		frmDiagPtArtn_Deactivate(Me, New System.EventArgs())
		'UPGRADE_NOTE: Object frmDiagPtArtn may not be destroyed until it is garbage collected. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6E35BFF6-CD74-4B09-9689-3E1A43DF8969"'
        Me.Dispose()
		
		eventArgs.Cancel = Cancel
	End Sub
End Class