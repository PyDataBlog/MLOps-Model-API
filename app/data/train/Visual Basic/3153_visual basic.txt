Imports MySql.Data.MySqlClient
Module usuarios
    'variable global
    Dim conexion As New MySqlConnection
    Dim oconexion As New MySqlConnection
    Dim comandos As New MySqlCommand

    Function empleado(ByVal usuario As String, ByVal password As String) As Boolean
        'funcion para usuarios'

        'funcion para usuarios'
        Dim oconexion As New MySqlConnection
        Dim odataadapter As New MySqlDataAdapter
        Dim odataset As New DataSet
        Dim sSql As String
        Dim sw As Boolean = False
        Try
            oconexion.ConnectionString = "server=172.20.10.2;database=eura_database;user id=root;password=toor"
            sSql = "SELECT * FROM empleado WHERE username = '" & usuario & "' and password = '" & password & "' and status = 'Habilitado'"
            odataadapter = New MySqlDataAdapter(sSql, oconexion)
            oconexion.Open()
            odataset.Clear()
            odataadapter.Fill(odataset, "empleado")
            If (odataset.Tables("empleado").Rows.Count() <> 0) Then

                sw = True
            Else

                sw = False
            End If
        Catch ex As Exception

        End Try
        Return sw

    End Function

    Function administrador(ByVal usuario As String, ByVal password As String) As Boolean
        'funcion para usuarios'

        'funcion para usuarios'
        Dim oconexion As New MySqlConnection
        Dim odataadapter As New MySqlDataAdapter
        Dim odataset As New DataSet
        Dim sSql As String
        Dim sw As Boolean = False
        Try
            oconexion.ConnectionString = "server=172.20.10.2;database=eura_database;user id=root;password=toor"
            sSql = "SELECT * FROM empleado WHERE username = '" & usuario & "' and password = '" & password & "'"
            odataadapter = New MySqlDataAdapter(sSql, oconexion)
            oconexion.Open()
            odataset.Clear()
            odataadapter.Fill(odataset, "empleado")
            If (odataset.Tables("empleado").Rows.Count() <> 0) Then

                sw = True
            Else

                sw = False
            End If
        Catch ex As Exception

        End Try
        Return sw

    End Function




    Public Sub conectar()

        Dim servidor As String = "172.16.42.154"
        Dim baseDatos As String = "eura_database"
        Dim usuario As String = "root"
        Dim password As String = "toor"


        Try

            conexion.ConnectionString = "Server=" & servidor _
                & "; Database=" & baseDatos _
                & "; Uid=" & usuario _
                & "; Pwd=" & password

            conexion.Open()

        Catch ex As Exception
            MsgBox(ex.Message)
        End Try

    End Sub

    Public Sub desconectar()

        Try

            'Cerrar la conexion si esta abierta
            If conexion.State = ConnectionState.Open Then

                conexion.Close() 'Cerrar la conexion
                conexion.Dispose() 'Liberar recursos

            End If

        Catch ex As Exception
            MsgBox(ex.Message)
        End Try

    End Sub



End Module
