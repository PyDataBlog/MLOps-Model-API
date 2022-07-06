Public Class Form1


    Dim col1 As Boolean = True
    Dim col2 As Boolean = True
    Dim col3 As Boolean = True
    Dim col4 As Boolean = True
    Dim col5 As Boolean = True
    Dim col6 As Boolean = True
    Dim col7 As Boolean = True
    Dim col8 As Boolean = True
    Dim port As String

    Private Sub ComboBox1_SelectedIndexChanged_1(sender As System.Object, e As System.EventArgs) Handles ComboBox1.SelectedIndexChanged
        If ComboBox1.SelectedItem = "COM1" Then
            port = "COM1"

        End If
        If ComboBox1.SelectedItem = "COM2" Then
            port = "COM2"

        End If
        If ComboBox1.SelectedItem = "COM3" Then
            port = "COM3"

        End If
        If ComboBox1.SelectedItem = "COM4" Then
            port = "COM4"

        End If
        If ComboBox1.SelectedItem = "COM5" Then
            port = "COM5"

        End If
        If ComboBox1.SelectedItem = "COM6" Then
            port = "COM6"

        End If
        If ComboBox1.SelectedItem = "COM7" Then
            port = "COM7"

        End If
        If ComboBox1.SelectedItem = "COM8" Then
            port = "COM8"

        End If
        If ComboBox1.SelectedItem = "COM9" Then
            port = "COM9"

        End If
        If ComboBox1.SelectedItem = "COM10" Then
            port = "COM10"

        End If
        If ComboBox1.SelectedItem = "COM11" Then
            port = "COM11"

        End If
        If ComboBox1.SelectedItem = "COM12" Then
            port = "COM12"

        End If
        If ComboBox1.SelectedItem = "COM13" Then
            port = "COM13"

        End If
        If ComboBox1.SelectedItem = "COM14" Then
            port = "COM14"

        End If
        If ComboBox1.SelectedItem = "COM15" Then
            port = "COM15"

        End If
        If ComboBox1.SelectedItem = "COM16" Then
            port = "COM16"

        End If
        If ComboBox1.SelectedItem = "COM17" Then
            port = "COM17"

        End If
        If ComboBox1.SelectedItem = "COM18" Then
            port = "COM18"

        End If
        If ComboBox1.SelectedItem = "COM19" Then
            port = "COM19"

        End If
    End Sub
    


    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Using COM7 As IO.Ports.SerialPort = My.Computer.Ports.OpenSerialPort(port)
            COM7.Write("0")
        End Using
    End Sub

    Private Sub TabPage1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TabPage1.Click

    End Sub

    Private Sub TabPage2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TabPage2.Click

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        If col8 = False Then
            col8 = True
            My.Computer.Audio.Play(My.Resources.Speech_Sleep, AudioPlayMode.Background)
            Label4.Text = "OFF!"
            OvalShape8.BackColor = Color.Red
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("g")
            End Using


        Else
            col8 = False
            My.Computer.Audio.Play(My.Resources.Speech_On, AudioPlayMode.Background)
            Label4.Text = "ON!"
            OvalShape8.BackColor = Color.Green
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("h")
            End Using


        End If
    End Sub

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        If col6 = False Then
            col6 = True
            My.Computer.Audio.Play(My.Resources.Speech_Sleep, AudioPlayMode.Background)
            Label2.Text = "OFF!"
            OvalShape6.BackColor = Color.Red
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("c")
            End Using


        Else
            col6 = False
            My.Computer.Audio.Play(My.Resources.Speech_On, AudioPlayMode.Background)
            Label2.Text = "ON!"
            OvalShape6.BackColor = Color.Green
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("d")
            End Using


        End If
    End Sub

    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        If col7 = False Then
            col7 = True
            My.Computer.Audio.Play(My.Resources.Speech_Sleep, AudioPlayMode.Background)
            Label3.Text = "OFF!"
            OvalShape7.BackColor = Color.Red
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("e")
            End Using


        Else
            col7 = False
            My.Computer.Audio.Play(My.Resources.Speech_On, AudioPlayMode.Background)
            Label3.Text = "ON!"
            OvalShape7.BackColor = Color.Green
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("f")
            End Using


        End If
    End Sub

    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        If col5 = False Then
            col5 = True
            My.Computer.Audio.Play(My.Resources.Speech_Sleep, AudioPlayMode.Background)
            Label1.Text = "OFF!"
            OvalShape5.BackColor = Color.Red
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("a")
            End Using



        Else
            col5 = False
            My.Computer.Audio.Play(My.Resources.Speech_On, AudioPlayMode.Background)
            Label1.Text = "ON!"
            OvalShape5.BackColor = Color.Green

            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("b")
            End Using


        End If
    End Sub

    Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label1.Click

    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        

        If col1 = False Then
            col1 = True
            My.Computer.Audio.Play(My.Resources.Speech_Sleep, AudioPlayMode.Background)
            Label8.Text = "OFF!"
            OvalShape1.BackColor = Color.Red
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("1")
            End Using


        Else
            col1 = False
            My.Computer.Audio.Play(My.Resources.Speech_On, AudioPlayMode.Background)
            Label8.Text = "ON!"
            OvalShape1.BackColor = Color.Green
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("2")
            End Using


        End If
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        If col2 = False Then
            col2 = True
            My.Computer.Audio.Play(My.Resources.Speech_Sleep, AudioPlayMode.Background)
            Label7.Text = "OFF!"
            OvalShape2.BackColor = Color.Red
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("3")
            End Using


        Else
            col2 = False
            My.Computer.Audio.Play(My.Resources.Speech_On, AudioPlayMode.Background)
            Label7.Text = "ON!"
            OvalShape2.BackColor = Color.Green
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("4")
            End Using


        End If
    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        If col3 = False Then
            col3 = True
            My.Computer.Audio.Play(My.Resources.Speech_Sleep, AudioPlayMode.Background)
            Label6.Text = "OFF!"
            OvalShape3.BackColor = Color.Red
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("5")
            End Using


        Else
            col3 = False
            My.Computer.Audio.Play(My.Resources.Speech_On, AudioPlayMode.Background)
            Label6.Text = "ON!"
            OvalShape3.BackColor = Color.Green
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("6")
            End Using


        End If
    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        If col4 = False Then
            col4 = True
            My.Computer.Audio.Play(My.Resources.Speech_Sleep, AudioPlayMode.Background)
            Label5.Text = "OFF!"
            OvalShape4.BackColor = Color.Red
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("7")
            End Using


        Else
            col4 = False
            My.Computer.Audio.Play(My.Resources.Speech_On, AudioPlayMode.Background)
            Label5.Text = "ON!"
            OvalShape4.BackColor = Color.Green
            Using COM7 As IO.Ports.SerialPort =
               My.Computer.Ports.OpenSerialPort(port)
                COM7.Write("8")
            End Using


        End If
    End Sub

    Private Sub OvalShape1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OvalShape1.Click, OvalShape8.Click, OvalShape7.Click, OvalShape6.Click, OvalShape5.Click, OvalShape4.Click, OvalShape3.Click, OvalShape2.Click

    End Sub

    Private Sub Label7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label7.Click

    End Sub

    Private Sub Label9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label9.Click

    End Sub

    Private Sub ListBox1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub


    Private Sub Button9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

   
End Class
