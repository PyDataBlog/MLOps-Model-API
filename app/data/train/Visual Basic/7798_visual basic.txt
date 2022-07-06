Public Class FrmUserAddResult

    'set up local variables
    Dim user As Login.User
    Dim openUser As Boolean

    'store the user to add the result to and whether to open the userform when closing
    Public Sub New(user As Login.User, openUser As Boolean)
        InitializeComponent()
        Me.user = user
        Me.openUser = openUser
    End Sub

    'store the user to add the result to
    Public Sub New(user As Login.User)
        InitializeComponent()
        Me.user = user
    End Sub

    'add all the activites to the drop down box when the form loads.
    Private Sub FrmUserAddResult_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        For x As Integer = 0 To ModResults.activites.length - 1
            CmBSport.Items.Add(ModResults.activites(x))
        Next
        CmBSport.SelectedIndex = 0

    End Sub

    'check the result and store it if it is valid
    Private Sub BtnAdd_Click(sender As Object, e As EventArgs) Handles BtnAdd.Click
        'check the time input is physically possible
        If Val(TxtTime.Text) <= 0 Then
            MsgBox("Please enter a realistic time.")
            Return
        End If

        'check the selected activity is valid and save the result to file
        If CmBSport.SelectedIndex <> -1 And TxtTime.Text <> "" Then
            ModResults.saveResult(user.id, DateTime.Now, TxtTime.Text, CmBSport.Items(CmBSport.SelectedIndex))
            MsgBox("Result Saved")
            Close()
        Else
            MsgBox("Please fill in all fields") 'alert the user if not all fields are filled.
        End If
    End Sub

    'make the activity drop down box ignore key presses
    Private Sub CmBSport_KeyPress(sender As Object, e As KeyPressEventArgs) Handles CmBSport.KeyPress
        e.Handled = True
    End Sub

    'make the time only accept decimal characters and control characters (backspace, delete)
    Private Sub TxtTime_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TxtTime.KeyPress
        If "01234567890".Contains(e.KeyChar) Then
            e.Handled = False
        ElseIf e.KeyChar = "." Then
            e.Handled = TxtTime.Text.Contains(".")
        ElseIf e.KeyChar = vbBack Then
            e.Handled = False
        Else
            e.Handled = True
        End If
    End Sub

    'open the previous form when this one is closed
    Private Sub FrmUserAddResult_FormClosed(sender As Object, e As FormClosedEventArgs) Handles MyBase.FormClosed
        If openUser Then 'open the user form if opened by the user form
            Dim Newform As New FrmUser(Me.user)
            Newform.Show()
        End If
    End Sub

End Class