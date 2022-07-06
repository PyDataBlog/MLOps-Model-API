Imports System.Data.SqlClient

Public Class LoginForm

    Dim nextForm As Form
    Public Sub New(ByRef formType As Form)
        nextForm = formType
        MsgBox(formType.GetType().ToString())
        InitializeComponent()
    End Sub


    Private Sub LoginButton_Click(sender As Object, e As EventArgs) Handles LoginButton.Click
        Dim conn = New SqlConnection("Data Source=(LocalDB)\v11.0;AttachDbFilename=C:\Users\dhaval\Documents\bankingdb.mdf;Integrated Security=True;Connect Timeout=30")

        Dim query = String.Format("SELECT * FROM users WHERE username='{0}' and password='{1}'", UsernameTextBox.Text, PasswordTextBox.Text)
        Dim cmd = New SqlCommand(query, conn)

        conn.Open()
        Dim result = cmd.ExecuteScalar() ' Makes sure only one columnis returned
        If Not result Is Nothing Then
            nextForm.Show()
        End If
    End Sub

    Private Sub LoginForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub
End Class