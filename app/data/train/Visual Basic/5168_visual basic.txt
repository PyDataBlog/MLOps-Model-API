Imports System.Data.SqlClient

Public Class RegistrationPage
    Inherits System.Web.UI.Page

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

    End Sub

    Public Sub Validate_Email_Existance(ByVal source As Object, ByVal args As ServerValidateEventArgs) Handles email_custom_validation.ServerValidate
        Dim conn As New SqlConnection(ConfigurationManager.ConnectionStrings("myConnectionString").ConnectionString)
        Dim sql_command As New SqlCommand()
        sql_command.Connection = conn
        conn.Open()
        Response.Write("email value: " + args.Value.ToString())
        sql_command.CommandText = "select count(*) from students where email ='" + args.Value + "'"
        sql_command.CommandType = CommandType.Text

        Dim email_count As Integer = sql_command.ExecuteScalar()
        Response.Write("email count: " + email_count.ToString())
        If email_count = 1 Then
            args.IsValid = False
            Response.Write("email used")

        Else
            args.IsValid = True
            Response.Write("email not used")
        End If


    End Sub

    Private Function Register_User(first_name As String, last_name As String, password As String, email As String, gender As String, date_of_birth As String, specialisation As String) As Integer
        If (Not Page.IsValid()) Then
            Return 0
        End If

        Dim conn As New SqlConnection(ConfigurationManager.ConnectionStrings("myConnectionString").ConnectionString)
        Dim sql_command As New SqlCommand()

        sql_command.Connection = conn

        sql_command.CommandText = "insert into students values('" + first_name + "','" + last_name + "','" + password + "','" + email + "','" + gender + "','" + date_of_birth + "','" + specialisation + "')"
        sql_command.CommandType = CommandType.Text
        email_custom_validation.Validate()
        conn.Open()
        Dim r As Integer = sql_command.ExecuteNonQuery()
        conn.Close()
        Response.Write("student added")

        Return 0
    End Function

    Protected Sub register_button_Click(sender As Object, e As EventArgs) Handles register_button.Click

        Dim dob As String = dob_calendar.SelectedDate.ToString("yyyy-MM-dd")

        Register_User(first_name_box.Text, last_name_box.Text, password_box.Text, email_box.Text, gender_list.SelectedValue, dob, specialisation_list.Text)
    End Sub
End Class