
Imports System.Net.NetworkInformation

Public Class cls_Host

    Public Function GetLocalIPs(Optional ByVal GenerateIP As Boolean = False) As String()
        Dim StrngAr() As String
        Try
            Dim IP As Net.IPAddress
            Dim IPList As Array = Net.Dns.GetHostEntry(SystemInformation.ComputerName.ToString).AddressList
            If IPList.Length > 0 Then
                ReDim StrngAr(IPList.Length - 1)
                For i As Integer = 0 To IPList.Length - 1
                    IP = IPList.GetValue(i)
                    StrngAr(i) = IP.ToString
                Next
                Return StrngAr
            End If
        Catch ex As Exception
        End Try

        ReDim StrngAr(0)
        If GenerateIP = True Then
            StrngAr(0) = "192.168.1.1"
        Else
            StrngAr(0) = "0.0.0.0"
        End If

        Return StrngAr
    End Function

    ''' <summary>
    ''' Gibt einen String mit der ersten gefundenen gültigen IP zurück.
    ''' </summary>
    ''' <param name="GenerateIP">wenn keine IP gefunden wird, eine genereiren?</param>
    Public Function GetLocalIP(Optional ByVal GenerateIP As Boolean = False) As String
        Try
            Dim IP As Net.IPAddress
            Dim IPList As Array = Net.Dns.GetHostEntry(SystemInformation.ComputerName.ToString).AddressList
            If IPList.Length > 0 Then
                For i As Integer = 0 To IPList.Length - 1
                    IP = IPList.GetValue(i)
                    If IP.ToString <> "0.0.0.0" Then Return IP.ToString
                Next
            End If
        Catch ex As Exception
        End Try

        If GenerateIP = True Then
            Return "192.168.1.1"
        Else
            Return "0.0.0.0"
        End If
    End Function


    Public Sub GetLocalInterfaces(ByVal ListBox1 As ListBox)
        ' NetzwerkInterface-Objekt
        Dim oInterface As NetworkInterface

        ' alle verfügbaten Netzwerk-Interfaces durchlaufen
        For Each oInterface In NetworkInterface.GetAllNetworkInterfaces()
            With oInterface
                ' nur wenn es sich um kein LoopBack/Tunnel-Interface handelt...
                If .NetworkInterfaceType <> NetworkInterfaceType.Loopback AndAlso _
                  .NetworkInterfaceType <> NetworkInterfaceType.Tunnel Then

                    ' Name der Netzwerkverbindung, Status und MAC-Adresse
                    ' in die ListBox schreiben
                    ListBox1.Items.Add(.Name.ToString & _
                      " Status: " & .OperationalStatus.ToString & " " & _
                      " [" & .GetPhysicalAddress.ToString & "]")
                End If
            End With
        Next
    End Sub


    Public Function GetLocalOpenPorts() As DataTable

        ' Ein neues Process-Objekt erzeugen 
        Dim Anwendung As System.Diagnostics.Process = New System.Diagnostics.Process()
        ' Die StartInfo-Struktur des Process- 
        ' Objekts mit Informationen befüllen 
        With Anwendung.StartInfo
            ' Befehlszeileninterpreter (COMMAND.COM/CMD.EXE): 
            .FileName = Environ("COMSPEC")
            ' Kein Konsolenfenster erzeugen: 
            .CreateNoWindow = True
            ' StandardInput, -Output und -Error umleiten: 
            .RedirectStandardInput = True
            .RedirectStandardOutput = True
            .RedirectStandardError = True
            ' UseShellExecute MUSS für eine Umleitung 
            ' von StdIn, StdOut und StdErr FALSE sein: 
            .UseShellExecute = False
        End With
        ' Den Befehlszeileninterpreter starten: 
        Anwendung.Start()

        ' StreamReader- und Writer-Objekte für den 
        ' Zugriff auf StdIn, StdOut und StdErr erzeugen 
        ' Über ein StreamWriter-Objekt in StandardInput schreiben 
        Dim StdIn As System.IO.StreamWriter = Anwendung.StandardInput
        StdIn.AutoFlush = True ' Puffer automatisch flushen 
        ' Über ein StreamReader-Objekt aus StandardOutput lesen 
        Dim StdOut As System.IO.StreamReader = Anwendung.StandardOutput
        ' Über ein StreamReader-Objekt aus StandardError lesen 
        Dim StdErr As System.IO.StreamReader = Anwendung.StandardError

        Dim sTemp As String = String.Empty ' Hilfsvariable 

        ' Einen DIR-Befehl über StdIn absetzen:
        StdIn.WriteLine("netstat -ano")

        ' Einen EXIT-Befehl über StdIn absetzen:
        StdIn.WriteLine("EXIT")

        sTemp = System.Text.Encoding.GetEncoding(850).GetString(System.Text.Encoding.Default.GetBytes(StdOut.ReadToEnd()))
        Dim pPortAusgabe() As String = sTemp.Split(vbCrLf)

        Dim dtPort As New DataTable
        With dtPort
            .Clear()
            .Columns.Add("Protokoll")
            .Columns.Add("Lokal Adress")
            .Columns.Add("Remote Adress")
            .Columns.Add("Status")
            .Columns.Add("PID")
        End With

        Dim PortLine() As String
        Dim PortColumns(5) As String
        Dim Counter As Integer

        For i As Integer = 8 To pPortAusgabe.Length - 4

            PortLine = pPortAusgabe(i).Split(" ")

            Counter = 0
            For j As Integer = 0 To PortLine.Length - 1
                If PortLine(j).ToString().Length > 2 Then
                    PortColumns(Counter) = PortLine(j).ToString()
                    Counter += 1
                End If
            Next

            dtPort.Rows.Add(PortColumns(0).Trim(), PortColumns(1).Trim(), PortColumns(2).Trim(), PortColumns(3).Trim(), PortColumns(4).Trim())
        Next


        ' Umwandlung der ASCII-Zeichen in Ansi-Codepage (Nr. 850): 
        ' Als Rückgabewert speichern
        GetLocalOpenPorts = dtPort

        ' Streams schliessen
        StdIn.Close()
        StdOut.Close()
        StdErr.Close()

        If Not Anwendung.HasExited Then
            Anwendung.Kill()
        End If
        ' Ressourcen des Process-Objekts freigeben 
        Anwendung.Close()

    End Function


    Public Function GetLocalRouteTable() As String()

        ' Ein neues Process-Objekt erzeugen 
        Dim Anwendung As System.Diagnostics.Process = New System.Diagnostics.Process()
        ' Die StartInfo-Struktur des Process- 
        ' Objekts mit Informationen befüllen 
        With Anwendung.StartInfo
            ' Befehlszeileninterpreter (COMMAND.COM/CMD.EXE): 
            .FileName = Environ("COMSPEC")
            ' Kein Konsolenfenster erzeugen: 
            .CreateNoWindow = True
            ' StandardInput, -Output und -Error umleiten: 
            .RedirectStandardInput = True
            .RedirectStandardOutput = True
            .RedirectStandardError = True
            ' UseShellExecute MUSS für eine Umleitung 
            ' von StdIn, StdOut und StdErr FALSE sein: 
            .UseShellExecute = False
        End With
        ' Den Befehlszeileninterpreter starten: 
        Anwendung.Start()

        ' StreamReader- und Writer-Objekte für den 
        ' Zugriff auf StdIn, StdOut und StdErr erzeugen 
        ' Über ein StreamWriter-Objekt in StandardInput schreiben 
        Dim StdIn As System.IO.StreamWriter = Anwendung.StandardInput
        StdIn.AutoFlush = True ' Puffer automatisch flushen 
        ' Über ein StreamReader-Objekt aus StandardOutput lesen 
        Dim StdOut As System.IO.StreamReader = Anwendung.StandardOutput
        ' Über ein StreamReader-Objekt aus StandardError lesen 
        Dim StdErr As System.IO.StreamReader = Anwendung.StandardError

        Dim sTemp() As String ' Hilfsvariable 

        ' Einen DIR-Befehl über StdIn absetzen:
        StdIn.WriteLine("route print")

        ' Einen EXIT-Befehl über StdIn absetzen:
        StdIn.WriteLine("EXIT")

        StdOut.ReadLine()
        StdOut.ReadLine()
        StdOut.ReadLine()
        StdOut.ReadLine()

        sTemp = System.Text.Encoding.GetEncoding(850).GetString(System.Text.Encoding.Default.GetBytes(StdOut.ReadToEnd())).Split(vbCrLf)

        ' Umwandlung der ASCII-Zeichen in Ansi-Codepage (Nr. 850): 
        ' Als Rückgabewert speichern
        GetLocalRouteTable = sTemp

        ' Streams schliessen
        StdIn.Close()
        StdOut.Close()
        StdErr.Close()

        If Not Anwendung.HasExited Then
            Anwendung.Kill()
        End If
        ' Ressourcen des Process-Objekts freigeben 
        Anwendung.Close()

    End Function

End Class
