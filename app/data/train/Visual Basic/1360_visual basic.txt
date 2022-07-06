Public Class frmAddition
    Private Sub frmMenu_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        txtWelcome.Text = "Hello " + General.Name + " please answer the addition questions"
        If Type = 1 Then
            General.Answer = General.Number1 + General.Number2
            txtNum1.Text = General.Number1
            txtNum2.Text = General.Number2
            txtNum3.Text = Nothing
            txtNum1.Enabled = False
            txtNum2.Enabled = False
            txtNum3.Enabled = True
            txtNum3.Focus()
        ElseIf Type = 2 Then
            General.Answer = General.Number1 + General.Number2
            txtNum1.Text = General.Number1
            txtNum2.Text = Nothing
            txtNum3.Text = General.Answer
            txtNum1.Enabled = False
            txtNum2.Enabled = True
            txtNum3.Enabled = False
            txtNum2.Focus()
        ElseIf Type = 3 Then
            General.Answer = General.Number1 + General.Number2
            txtNum1.Text = Nothing
            txtNum2.Text = General.Number2
            txtNum3.Text = General.Answer
            txtNum1.Enabled = True
            txtNum2.Enabled = False
            txtNum3.Enabled = False
            txtNum1.Focus()
        End If
        txtCount.Text = General.Answered + 1 & " of " & General.MaxQuestion + 1
        'General.Answered = General.Answered + 1
    End Sub

    Private Sub btnMenu_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnMenu.Click
        General.ClearResults()
        General.MenuAction(Me, frmMenu)
    End Sub

    Private Sub btnCheck_Click2(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCheck.Click
        CheckAns()
    End Sub

    Private Sub btnCheck_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtNum1.KeyDown, txtNum2.KeyDown, txtNum3.KeyDown
        If e.KeyCode = Keys.Return Then
            CheckAns()
        End If
    End Sub

    Private Sub CheckAns()
        If (IsEmptyNullOrWhitespaceOnly(txtNum1.Text) Or IsEmptyNullOrWhitespaceOnly(txtNum2.Text) Or IsEmptyNullOrWhitespaceOnly(txtNum3.Text)) = False Then
            Try
                If Type = 1 Then
                    If Convert.ToInt32(txtNum3.Text) = General.Answer Then
                        imgDetails.Visible = True
                        txtDetails.Visible = True
                        txtDetails.Text = "Well Done"
                        imgDetails.Image = My.Resources.accept
                        btnCheck.Visible = False
                        'General.Answered = ++1
                        General.Correct = General.Correct + 1
                        btnNext.Focus()
                    Else
                        imgDetails.Visible = True
                        txtDetails.Visible = True
                        txtDetails.Text = "Try Again"
                        imgDetails.Image = My.Resources.delete
                        btnCheck.Visible = False
                        'General.Answered = ++1
                        txtNum3.Focus()
                    End If
                ElseIf Type = 2 Then
                    If Convert.ToInt32(txtNum2.Text) = General.Answer - General.Number1 Then
                        imgDetails.Visible = True
                        txtDetails.Visible = True
                        txtDetails.Text = "Well Done"
                        imgDetails.Image = My.Resources.accept
                        btnCheck.Visible = False
                        'General.Answered = ++1
                        General.Correct = General.Correct + 1
                        btnNext.Focus()
                    Else
                        imgDetails.Visible = True
                        txtDetails.Visible = True
                        txtDetails.Text = "Try Again"
                        imgDetails.Image = My.Resources.delete
                        btnCheck.Visible = False
                        'General.Answered = ++1
                        txtNum2.Focus()
                    End If
                ElseIf Type = 3 Then
                    If Convert.ToInt32(txtNum1.Text) = General.Answer - General.Number2 Then
                        imgDetails.Visible = True
                        txtDetails.Visible = True
                        txtDetails.Text = "Well Done"
                        imgDetails.Image = My.Resources.accept
                        btnCheck.Visible = False
                        'General.Answered = ++1
                        General.Correct = General.Correct + 1
                        btnNext.Focus()
                    Else
                        imgDetails.Visible = True
                        txtDetails.Visible = True
                        txtDetails.Text = "Try Again"
                        imgDetails.Image = My.Resources.delete
                        btnCheck.Visible = False
                        'General.Answered = ++1
                        txtNum1.Focus()
                    End If
                End If
            Catch FormatException As Exception
                MessageBox.Show("Please enter only numbers")
            End Try
        Else
            MessageBox.Show("Please enter you answer")
        End If

    End Sub

    Private Sub btnNext_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnNext.Click
        General.Generate()
        If General.Answered <> General.MaxQuestion Then
            If Type = 1 Then
                General.Answer = General.Number1 + General.Number2
                txtNum1.Text = General.Number1
                txtNum2.Text = General.Number2
                txtNum3.Text = Nothing
                txtNum1.Enabled = False
                txtNum2.Enabled = False
                txtNum3.Enabled = True
                txtNum3.Focus()
                General.Answered = General.Answered + 1
            ElseIf Type = 2 Then
                General.Answer = General.Number1 + General.Number2
                txtNum1.Text = General.Number1
                txtNum2.Text = Nothing
                txtNum3.Text = General.Answer
                txtNum1.Enabled = False
                txtNum2.Enabled = True
                txtNum3.Enabled = False
                txtNum2.Focus()
                General.Answered = General.Answered + 1
            ElseIf Type = 3 Then
                General.Answer = General.Number1 + General.Number2
                txtNum1.Text = Nothing
                txtNum2.Text = General.Number2
                txtNum3.Text = General.Answer
                txtNum1.Enabled = True
                txtNum2.Enabled = False
                txtNum3.Enabled = False
                txtNum1.Focus()
                General.Answered = General.Answered + 1
            End If
            btnCheck.Visible = True
            imgDetails.Visible = False
            txtDetails.Visible = False
            txtCount.Text = General.Answered + 1 & " of " & General.MaxQuestion + 1
        Else
            General.Topic = "Addition"
            General.MenuAction(Me, frmAward)
        End If
    End Sub

    Private Sub txtNum_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtNum1.TextChanged, txtNum2.TextChanged, txtNum3.TextChanged
        btnCheck.Visible = True
        imgDetails.Visible = False
        txtDetails.Visible = False
    End Sub
End Class
