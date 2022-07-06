Option Strict Off
Option Explicit On
Friend Class Form3
	Inherits System.Windows.Forms.Form
	
	Private Sub Form3_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load
        Try
            Dim FileNum As Short
            Dim DataArray() As Byte

            DataArray = My.Resources.STARTUP_103

            FileNum = FreeFile()
            FileOpen(FileNum, "C:\eggman.wav", OpenMode.Binary)
            'UPGRADE_WARNING: Put was upgraded to FilePut and has a new behavior. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="9B7D5ADD-D8FE-4819-A36C-6DEDAF088CC7"'
            FilePut(FileNum, DataArray, 1)
            FileClose(FileNum)

            load_Renamed.Value = 0
            Dim a As Double
            Dim b As Double
        Catch ex As Exception

        End Try

    End Sub
	
	Private Sub Timer1_Tick(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Timer1.Tick
		Dim b As Object
		Dim a As Object
		'UPGRADE_WARNING: Couldn't resolve default property of object a. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
		a = Label1.Text
		'UPGRADE_WARNING: Couldn't resolve default property of object a. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
		'UPGRADE_WARNING: Couldn't resolve default property of object b. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
		b = a + 1
		'UPGRADE_WARNING: Couldn't resolve default property of object b. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
		Label1.Text = b
		load_Renamed.Value = CSng(Label1.Text)
		
		If CDbl(Label1.Text) = 20 Then
			Timer1.Interval = 500
		End If
		If CDbl(Label1.Text) = 26 Then
			Timer1.Interval = 60
		End If
		If CDbl(Label1.Text) = 50 Then
			Timer1.Interval = 2000
		End If
		If CDbl(Label1.Text) = 60 Then
			Timer1.Interval = 100
		End If
		If CDbl(Label1.Text) = 100 Then
			Timer1.Enabled = False
			Timer2.Interval = 3500
			Timer2.Enabled = True
			load_Renamed.Visible = False
			Label1.Visible = True
		End If
	End Sub
	
	Private Sub Timer2_Tick(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Timer2.Tick
		If Label1.Text = "creating errors" Then
			Timer2.Enabled = False
			Form4.Visible = True
			Me.Visible = False
			Exit Sub
		End If
		If Label1.Text = "Plotting Evil scemes" Then
			Label1.Text = "creating errors"
			Exit Sub
		End If
		If Label1.Text = "setting up desktop" Then
			Label1.Text = "Plotting Evil scemes"
			Exit Sub
		End If
		If Label1.Text = "Eating cookies" Then
			Label1.Text = "setting up desktop"
			Exit Sub
		End If
		If Label1.Text = "Loading remote car starter" Then
			Label1.Text = "Eating cookies"
			Exit Sub
		End If
		If Label1.Text = "Imitating windows 3.1/95/98/2000/ME/XP/Vista/7" Then
			Label1.Text = "Loading remote car starter"
			Exit Sub
		End If
		
		If Label1.Text = "Loading start menu" Then
			Label1.Text = "Imitating windows 3.1/95/98/2000/ME/XP/Vista/7"
		Else
			Label1.Text = "Loading start menu"
		End If
	End Sub
End Class