Public Class Form1
    Dim intGuess, intRight, intScore As Integer

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load


        intScore = 100

        'Get Random
        intRight = Int(Rnd() * (100 - 1 + 1)) + 1


    End Sub

    Private Sub btnRightOrWrong_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRightOrWrong.Click


        'Get Input
        intGuess = CInt(txtGuessedNum.Text)

        If intGuess < intRight Then

            intScore -= 10
            MessageBox.Show("Guess Higher" & ControlChars.NewLine & "Your score: " & CStr(intScore), "Guess Check", MessageBoxButtons.OK)

        ElseIf intGuess > intRight Then

            intScore -= 10
            MessageBox.Show("Guess Lower" & ControlChars.NewLine & "Your score: " & CStr(intScore), "Guess Check", MessageBoxButtons.OK)

        Else

            MessageBox.Show("You Won!" & ControlChars.NewLine & "Your score: " & CStr(intScore), "Guess Check", MessageBoxButtons.OK)
            My.Computer.Audio.Play("chimes.wav")
            Me.Close() 'Close form

        End If


    End Sub
End Class

'Kaleb Haslam
'Guessing Game