Imports System
Imports System.IO.Ports
Imports _2Display.GlobalVars
Public Class timer
    Dim startingNum
    Dim count
    Private Sub timer_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        clearCounter()
    End Sub
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles startBtn.Click
        tTimer.Enabled = True
        startingNum = startNumTB.Text
        If upChk.CheckState = CheckState.Checked Then
            count = 0
        Else
            count = startingNum
        End If

    End Sub

    Private Sub tTimer_Tick(sender As Object, e As EventArgs) Handles tTimer.Tick
        'Incresse counter 
        If upChk.CheckState = CheckState.Checked Then
            count += 1
            If count = startingNum Then
                tTimer.Stop()
            End If
        ElseIf downChk.CheckState = CheckState.Checked Then
            If count = 1 Then
                tTimer.Stop()
            End If
            count -= 1
        Else
            MsgBox("NO CHECK BOX CHECKED!")
            Me.Close()
        End If
        'Write to display
        displaySerial.Write("N" + spAddress + "V" + currentCounter + count.ToString + "$")
    End Sub

    Private Sub stopBtn_Click(sender As Object, e As EventArgs) Handles stopBtn.Click
        tTimer.Stop()
    End Sub

    Private Sub resetBtn_Click(sender As Object, e As EventArgs) Handles resetBtn.Click
        tTimer.Stop()
        clearCounter()
        startNumTB.Text = ""
    End Sub

    Private Sub startNumTB_TextChanged(sender As Object, e As EventArgs) Handles startNumTB.TextChanged
        If startNumTB.Text <> "" Then
            displaySerial.Write("N" + spAddress + "V" + currentCounter + startNumTB.Text + "$")
        Else
            clearCounter()
        End If
    End Sub

    Private Sub clearCounter()
        'Clear Counter
        displaySerial.Write("N" + spAddress + "V" + currentCounter + "0" + "$")
    End Sub
End Class