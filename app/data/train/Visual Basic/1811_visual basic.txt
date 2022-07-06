Imports System.Data.SqlServerCe

Public Class Database_Template

    Private Sub Form2_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Try
            Dim strDataSource As String
            strDataSource = "" & _
                "Data Source = C:\Users\" & _
                "Sam McClain\" & _
                "Documents\" & _
                "Visual Studio 2012\" & _
"Projects\ITT_Admin\ITT_Admin\Database1.sdf"

            Dim conn As New SqlCeConnection
            conn.ConnectionString = strDataSource

            Dim selectCmd As SqlCeCommand = conn.CreateCommand
            selectCmd.CommandText = "SELECT * FROM login"

            Dim adp As New SqlCeDataAdapter()

            Dim ds As New DataSet

            adp.SelectCommand = selectCmd

            ' Note: Fill will leave the connection in its original state;
            ' In this case, the connection was closed so it will be left closed
            adp.Fill(ds)

            For Each dr As DataRow In ds.Tables(0).Rows
                MsgBox(dr.Item("user_id").ToString)
            Next

            MsgBox(("The SqlCeDataAdapter succesfully filled " & _
                               ds.Tables.Item(0).Rows.Count & " rows in the DataSet!"))


            Dim insertCmd As SqlCeCommand = conn.CreateCommand

            ' Create the InsertCommand.
            insertCmd = New SqlCeCommand( _
                "insert into login(user_id,password,name,chngpass_flag,isAdmin) " & _
                "values ('15987041','password','Sam','Y','Y')", conn)


            adp.InsertCommand = insertCmd
            conn.Open()
            adp.InsertCommand.ExecuteNonQuery()


            Dim updateCmd As SqlCeCommand = conn.CreateCommand()
            updateCmd.CommandText = "UPDATE login SET name = 'Sam Haney'" + _
                " WHERE user_id = 15987041"


            adp.UpdateCommand = updateCmd
            adp.UpdateCommand.ExecuteNonQuery()




            adp.SelectCommand = selectCmd
            ds.Clear()

            adp.Fill(ds)

            For Each dr As DataRow In ds.Tables(0).Rows
                MsgBox(dr.Item("user_id").ToString)
            Next
        Catch ds As Exception
            MsgBox(ds.Message)
        Finally
            MsgBox(vbNewLine & vbNewLine & vbNewLine & _
                              "Press any key to continue...")
        End Try
    End Sub
End Class
