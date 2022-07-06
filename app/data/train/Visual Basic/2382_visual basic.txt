Public Class frmStandard
    Private Function Answers()
        Dim Ans As Double
        If txtOperator.Text = "+" Then
            Ans = CDbl(txtPreAns.Text) + CDbl(lblAnswer.Text)
            txtAns.Text = Ans
            lblAnswer.Text = txtAns.Text
        Else
            If txtOperator.Text = "*" Then
                Ans = CDbl(txtPreAns.Text) * CDbl(lblAnswer.Text)
                txtAns.Text = Ans
                lblAnswer.Text = txtAns.Text
            Else
                If txtOperator.Text = "-" Then
                    Ans = CDbl(txtPreAns.Text) - CDbl(lblAnswer.Text)
                    txtAns.Text = Ans
                    lblAnswer.Text = txtAns.Text
                Else
                    If txtOperator.Text = "/" Then
                        Ans = CDbl(txtPreAns.Text) / CDbl(lblAnswer.Text)
                        txtAns.Text = Ans
                        lblAnswer.Text = txtAns.Text
                    End If
                End If
            End If
        End If
    End Function

    Private Sub cboMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cboMode.SelectedIndexChanged

    End Sub

    Private Sub cmdSeven_Click(sender As Object, e As EventArgs) Handles cmdSeven.Click
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "s" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "y" Then
            lblAnswer.Text = "7"
            lblScreen.Text = "7"
        Else
            If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" And Len(lblAnswer.Text) < 2 Then
                lblAnswer.Text = lblAnswer.Text & "7"
                lblScreen.Text = lblScreen.Text & "7"
            Else
                If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "-" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                    lblAnswer.Text = "7"
                    lblScreen.Text = lblScreen.Text & "7"
                Else
                    lblAnswer.Text = lblAnswer.Text & "7"
                    lblScreen.Text = lblScreen.Text & "7"
                End If
            End If
        End If


    End Sub

    Private Sub cmdEight_Click(sender As Object, e As EventArgs) Handles cmdEight.Click
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "s" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "y" Then
            lblAnswer.Text = "8"
            lblScreen.Text = "8"
        Else
            If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" And Len(lblAnswer.Text) < 2 Then
                lblAnswer.Text = lblAnswer.Text & "8"
                lblScreen.Text = lblScreen.Text & "8"
            Else
                If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "-" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                    lblAnswer.Text = "8"
                    lblScreen.Text = lblScreen.Text & "8"
                Else
                    lblAnswer.Text = lblAnswer.Text & "8"
                    lblScreen.Text = lblScreen.Text & "8"
                End If
            End If
        End If
    End Sub

    Private Sub cmdNine_Click(sender As Object, e As EventArgs) Handles cmdNine.Click
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "s" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "y" Then
            lblAnswer.Text = "9"
            lblScreen.Text = "9"
        Else
            If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" And Len(lblAnswer.Text) < 2 Then
                lblAnswer.Text = lblAnswer.Text & "9"
                lblScreen.Text = lblScreen.Text & "9"
            Else
                If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "-" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                    lblAnswer.Text = "9"
                    lblScreen.Text = lblScreen.Text & "9"
                Else
                    lblAnswer.Text = lblAnswer.Text & "9"
                    lblScreen.Text = lblScreen.Text & "9"
                End If
            End If
        End If
    End Sub

    Private Sub cmdFour_Click(sender As Object, e As EventArgs) Handles cmdFour.Click
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "s" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "y" Then
            lblAnswer.Text = "4"
            lblScreen.Text = "4"
        Else
            If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" And Len(lblAnswer.Text) < 2 Then
                lblAnswer.Text = lblAnswer.Text & "4"
                lblScreen.Text = lblScreen.Text & "4"
            Else
                If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "-" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                    lblAnswer.Text = "4"
                    lblScreen.Text = lblScreen.Text & "4"
                Else
                    lblAnswer.Text = lblAnswer.Text & "4"
                    lblScreen.Text = lblScreen.Text & "4"
                End If
            End If
        End If
    End Sub

    Private Sub cmdFive_Click(sender As Object, e As EventArgs) Handles cmdFive.Click
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "s" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "y" Then
            lblAnswer.Text = "5"
            lblScreen.Text = "5"
        Else
            If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" And Len(lblAnswer.Text) < 2 Then
                lblAnswer.Text = lblAnswer.Text & "5"
                lblScreen.Text = lblScreen.Text & "5"
            Else
                If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "-" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                    lblAnswer.Text = "5"
                    lblScreen.Text = lblScreen.Text & "5"
                Else
                    lblAnswer.Text = lblAnswer.Text & "5"
                    lblScreen.Text = lblScreen.Text & "5"
                End If
            End If
        End If
    End Sub

    Private Sub cmdSix_Click(sender As Object, e As EventArgs) Handles cmdSix.Click
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "s" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "y" Then
            lblAnswer.Text = "6"
            lblScreen.Text = "6"
        Else
            If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" And Len(lblAnswer.Text) < 2 Then
                lblAnswer.Text = lblAnswer.Text & "6"
                lblScreen.Text = lblScreen.Text & "6"
            Else
                If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "-" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                    lblAnswer.Text = "6"
                    lblScreen.Text = lblScreen.Text & "6"
                Else
                    lblAnswer.Text = lblAnswer.Text & "6"
                    lblScreen.Text = lblScreen.Text & "6"
                End If
            End If
        End If
    End Sub

    Private Sub cmdOne_Click(sender As Object, e As EventArgs) Handles cmdOne.Click
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "s" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "y" Then
            lblAnswer.Text = "1"
            lblScreen.Text = "1"
        Else
            If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" And Len(lblAnswer.Text) < 2 Then
                lblAnswer.Text = lblAnswer.Text & "1"
                lblScreen.Text = lblScreen.Text & "1"
            Else
                If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "-" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                    lblAnswer.Text = "1"
                    lblScreen.Text = lblScreen.Text & "1"
                Else
                    lblAnswer.Text = lblAnswer.Text & "1"
                    lblScreen.Text = lblScreen.Text & "1"
                End If
            End If
        End If
    End Sub

    Private Sub cmdTwo_Click(sender As Object, e As EventArgs) Handles cmdTwo.Click
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "s" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "y" Then
            lblAnswer.Text = "2"
            lblScreen.Text = "2"
        Else
            If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" And Len(lblAnswer.Text) < 2 Then
                lblAnswer.Text = lblAnswer.Text & "2"
                lblScreen.Text = lblScreen.Text & "2"
            Else
                If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "-" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                    lblAnswer.Text = "2"
                    lblScreen.Text = lblScreen.Text & "2"
                Else
                    lblAnswer.Text = lblAnswer.Text & "2"
                    lblScreen.Text = lblScreen.Text & "2"
                End If
            End If
        End If
    End Sub

    Private Sub cmdThree_Click(sender As Object, e As EventArgs) Handles cmdThree.Click
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "s" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "y" Then
            lblAnswer.Text = "3"
            lblScreen.Text = "3"
        Else
            If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" And Len(lblAnswer.Text) < 2 Then
                lblAnswer.Text = lblAnswer.Text & "3"
                lblScreen.Text = lblScreen.Text & "3"
            Else
                If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "-" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                    lblAnswer.Text = "3"
                    lblScreen.Text = lblScreen.Text & "3"
                Else
                    lblAnswer.Text = lblAnswer.Text & "3"
                    lblScreen.Text = lblScreen.Text & "3"
                End If
            End If
        End If
    End Sub

    Private Sub cmdZero_Click(sender As Object, e As EventArgs) Handles cmdZero.Click
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "s" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "y" Then
            lblAnswer.Text = "0"
            lblScreen.Text = "0"
        Else
            If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" And Len(lblAnswer.Text) < 2 Then
                lblAnswer.Text = lblAnswer.Text & "0"
                lblScreen.Text = lblScreen.Text & "0"
            Else
                If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "-" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                    lblAnswer.Text = "0"
                    lblScreen.Text = lblScreen.Text & "0"
                Else
                    lblAnswer.Text = lblAnswer.Text & "0"
                    lblScreen.Text = lblScreen.Text & "0"
                End If
            End If
        End If
    End Sub

    Private Sub cmdDecimal_Click(sender As Object, e As EventArgs) Handles cmdDecimal.Click
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "s" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "y" Then
            lblAnswer.Text = "."
            lblScreen.Text = "."
        Else
            If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" And Len(lblAnswer.Text) < 2 Then
                lblAnswer.Text = lblAnswer.Text & "."
                lblScreen.Text = lblScreen.Text & "."
            Else
                If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "-" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                    lblAnswer.Text = "."
                    lblScreen.Text = lblScreen.Text & "."
                Else
                    lblAnswer.Text = lblAnswer.Text & "."
                    lblScreen.Text = lblScreen.Text & "."
                End If
            End If
        End If
    End Sub

    Private Sub cmdDivide_Click(sender As Object, e As EventArgs) Handles cmdDivide.Click
        Dim PreAns As Double
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Then
            lblScreen.Text = "Ans"
            PreAns = CDbl(lblAnswer.Text)
            txtPreAns.Text = PreAns
        Else
            If InStr(1, lblScreen.Text, "+") > 0 Or InStr(1, lblScreen.Text, "-") Or InStr(1, lblScreen.Text, "*") Or InStr(1, lblScreen.Text, "/") Then
                Call Answers()
                PreAns = CDbl(lblAnswer.Text)
                txtPreAns.Text = PreAns
            Else
                PreAns = CDbl(lblAnswer.Text)
                txtPreAns.Text = PreAns
            End If
        End If
        lblScreen.Text = lblScreen.Text & "/"
        txtOperator.Text = "/"

    End Sub

    Private Sub cmdMultiply_Click(sender As Object, e As EventArgs) Handles cmdMultiply.Click
        Dim PreAns As Double
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Then
            lblScreen.Text = "Ans"
            PreAns = CDbl(lblAnswer.Text)
            txtPreAns.Text = PreAns
        Else
            If InStr(1, lblScreen.Text, "+") > 0 Or InStr(1, lblScreen.Text, "-") Or InStr(1, lblScreen.Text, "*") Or InStr(1, lblScreen.Text, "/") Then
                Call Answers()
                PreAns = CDbl(lblAnswer.Text)
                txtPreAns.Text = PreAns
            Else
                PreAns = CDbl(lblAnswer.Text)
                txtPreAns.Text = PreAns
            End If
        End If
        lblScreen.Text = lblScreen.Text & "*"
        txtOperator.Text = "*"

    End Sub

    Private Sub CmdMinus_Click(sender As Object, e As EventArgs) Handles CmdMinus.Click
        Dim PreAns As Double
        If lblScreen.Text = "" And lblAnswer.Text = "" Then
            lblAnswer.Text = "-"
        Else
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                lblAnswer.Text = "-"
            Else
                If Microsoft.VisualBasic.Left(lblScreen.Text, 1) = "-" And Len(lblScreen.Text) < 2 Then
                    lblAnswer.Text = "-"
                Else
                    If Microsoft.VisualBasic.Left(lblScreen.Text, 2) = "-" And Len(lblScreen.Text) < 2 Then
                        lblAnswer.Text = ""
                    Else
                        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Then
                            lblScreen.Text = "Ans"
                            PreAns = CDbl(lblAnswer.Text)
                            txtPreAns.Text = PreAns
                        Else
                            If InStr(1, lblScreen.Text, "+") > 0 Or InStr(1, lblScreen.Text, "-") > 0 Or InStr(1, lblScreen.Text, "*") > 0 Or InStr(1, lblScreen.Text, "/") > 0 Then
                                Call Answers()
                                PreAns = CDbl(lblAnswer.Text)
                                txtPreAns.Text = PreAns
                            Else
                                PreAns = CDbl(lblAnswer.Text)
                                txtPreAns.Text = PreAns
                            End If
                        End If
                    End If
                End If
            End If
        End If
        lblScreen.Text = lblScreen.Text & "-"
        If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" Then
            lblAnswer.Text = lblAnswer.Text
        Else
            txtOperator.Text = "-"
        End If

    End Sub

    Private Sub cmdAdd_Click(sender As Object, e As EventArgs) Handles cmdAdd.Click
        Dim PreAns As Double
        If Microsoft.VisualBasic.Right(lblScreen.text, 1) = "=" Then
            lblScreen.text = "Ans"
            PreAns = CDbl(lblanswer.text)
            txtPreAns.Text = PreAns
        Else
            If InStr(1, lblScreen.text, "+") > 0 Or InStr(1, lblScreen.text, "-") Or InStr(1, lblScreen.text, "*") Or InStr(1, lblScreen.text, "/") Then
                Call Answers()
                PreAns = CDbl(lblanswer.text)
                txtPreAns.Text = PreAns
            Else
                PreAns = CDbl(lblanswer.text)
                txtPreAns.Text = PreAns
            End If
        End If
        lblScreen.text = lblScreen.text & "+"
        txtOperator.Text = "+"

    End Sub

    Private Sub cmdAnswer_Click(sender As Object, e As EventArgs) Handles cmdAnswer.Click
        If lblAnswer.Text = "" Then
            MsgBox("Please type in a number first")
            lblScreen.Text = "Ans"
            lblAnswer.Text = txtAns.Text
        Else
            txtAns.Text = CDbl(lblAnswer.Text)
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or lblScreen.Text = "Ans" Then
                txtAns.Text = CDbl(lblAnswer.Text)
                lblAnswer.Text = txtAns.Text
            Else
                If InStr(1, lblScreen.Text, "+") > 0 Or InStr(1, lblScreen.Text, "-") Or InStr(1, lblScreen.Text, "*") Or InStr(1, lblScreen.Text, "/") Then
                    Dim Ans As Double
                    If txtOperator.Text = "+" Then
                        Ans = CDbl(txtPreAns.Text) + CDbl(lblAnswer.Text)
                        txtAns.Text = Ans
                        lblAnswer.Text = txtAns.Text
                    Else
                        If txtOperator.Text = "-" Then
                            Ans = CDbl(txtPreAns.Text) - CDbl(lblAnswer.Text)
                            txtAns.Text = Ans
                            lblAnswer.Text = txtAns.Text
                        Else
                            If txtOperator.Text = "*" Then
                                Ans = CDbl(txtPreAns.Text) * CDbl(lblAnswer.Text)
                                txtAns.Text = Ans
                                lblAnswer.Text = txtAns.Text
                            Else
                                If txtOperator.Text = "/" Then
                                    Ans = CDbl(txtPreAns.Text) / CDbl(lblAnswer.Text)
                                    txtAns.Text = Ans
                                    lblAnswer.Text = txtAns.Text
                                Else
                                    txtAns.Text = CDbl(lblAnswer.Text)
                                    lblAnswer.Text = txtAns.Text
                                End If
                            End If
                        End If
                    End If
                End If
            End If
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Then
                lblScreen.Text = lblScreen.Text & ""
            Else
                Dim OneKey As String
                OneKey = lblScreen.Text
                lblScreen.Text = OneKey & "="
            End If
        End If
    End Sub

    Private Sub cmdAns_Click(sender As Object, e As EventArgs) Handles cmdAns.Click
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Then
            lblAnswer.Text = CDbl(txtAns.Text)
            lblScreen.Text = "Ans"
        Else
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "-" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                lblAnswer.Text = CDbl(txtAns.Text)
                lblScreen.Text = lblScreen.Text & "Ans"
            Else
                lblScreen.Text = "Ans"
                lblAnswer.Text = CDbl(txtAns.Text)
            End If
        End If
    End Sub

    Private Sub cmdMemoryPlus_Click(sender As Object, e As EventArgs) Handles cmdMemoryPlus.Click
        If lblAnswer.Text = "" Then
            MsgBox("Please type in a number first")
            lblScreen.Text = "Ans"
            lblAnswer.Text = txtAns.Text
        Else
            Dim PreAns As Double
            Dim Memory As Double
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or lblScreen.Text = "Ans" Then
                PreAns = CDbl(lblAnswer.Text)
                Memory = CDbl(txtMemory.Text)
                txtMemory.Text = PreAns + Memory
            Else
                If InStr(1, lblScreen.Text, "+") > 0 Or InStr(1, lblScreen.Text, "-") Or InStr(1, lblScreen.Text, "*") Or InStr(1, lblScreen.Text, "/") Then
                    Call Answers()
                    PreAns = CDbl(lblAnswer.Text)
                    Memory = CDbl(txtMemory.Text)
                    txtMemory.Text = PreAns + Memory
                Else
                    PreAns = CDbl(lblAnswer.Text)
                    Memory = CDbl(txtMemory.Text)
                    txtMemory.Text = PreAns + Memory
                End If
            End If
            If txtMemory.Text = "0" Then
                lblMemory.Visible = False
            Else
                lblMemory.Visible = True
            End If
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Then
                lblScreen.Text = lblScreen.Text & ""
            Else
                lblScreen.Text = lblScreen.Text & "="
            End If
        End If
    End Sub

    Private Sub cmdMemoryRecall_Click(sender As Object, e As EventArgs) Handles cmdMemoryRecall.Click
        Dim OneKey As String
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Then
            lblAnswer.Text = CDbl(txtMemory.Text)
            lblScreen.Text = "Memory"
        Else
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "-" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                lblAnswer.Text = CDbl(txtMemory.Text)
                lblScreen.Text = lblScreen.Text & "Memory"
            Else
                OneKey = lblScreen.Text
                lblScreen.Text = OneKey & ""
                lblAnswer.Text = lblAnswer.Text & ""
            End If
        End If
        txtAns.Text = lblAnswer.Text

    End Sub

    Private Sub cmdSqrt_Click(sender As Object, e As EventArgs) Handles cmdSqrt.Click
        If lblAnswer.Text = "" Then
            MsgBox("Please type in a number first")
            lblScreen.Text = "Ans"
            lblAnswer.Text = txtAns.Text
        Else
            Dim Multiply As Double
            Dim PreAns As Double
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or lblScreen.Text = "Ans" Then
                lblScreen.Text = "Ans"
                If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" Then
                    Console.WriteLine("Cannot find the square root of negative numbers")
                    MsgBox("Cannot find the square root of negative numbers")
                Else
                    PreAns = CDbl(lblAnswer.Text)
                    txtPreAns.Text = PreAns
                    lblScreen.Text = "sqrt(" & lblAnswer.Text & ")="
                    Multiply = Math.Sqrt(CDbl(lblAnswer.Text))
                    txtAns.Text = Multiply
                    lblAnswer.Text = txtAns.Text
                End If
            Else
                Call Answers()
                If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" Then
                    lblScreen.Text = "Ans"
                    MsgBox("Cannot find the square root of negative numbers")
                Else
                    PreAns = CDbl(lblAnswer.Text)
                    txtPreAns.Text = PreAns
                    lblScreen.Text = "sqrt(" & lblAnswer.Text & ")="
                    Multiply = Math.Sqrt(CDbl(lblAnswer.Text))
                    txtAns.Text = Multiply
                    lblAnswer.Text = txtAns.Text
                End If
            End If
        End If
    End Sub

    Private Sub cmdPi_Click(sender As Object, e As EventArgs) Handles cmdPi.Click
        If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Then
            lblAnswer.Text = CDbl(4 * Math.Atan(1))
            lblScreen.Text = "π"
        Else
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "+" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "-" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "/" Or Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "*" Then
                lblAnswer.Text = CDbl(4 * Math.Atan(1))
                lblScreen.Text = lblScreen.Text & "π"
            Else
                lblScreen.Text = "π"
                lblAnswer.Text = CDbl(4 * Math.Atan(1))
            End If
        End If
    End Sub

    Private Sub cmdLogBase10_Click(sender As Object, e As EventArgs) Handles cmdLogBase10.Click
        If lblAnswer.Text = "" Then
            MsgBox("Please type in a number first")
            lblScreen.Text = "Ans"
            lblAnswer.Text = txtAns.Text
        Else
            Dim Multiply As Double
            Dim PreAns As Double
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or lblScreen.Text = "Ans" Then
                lblScreen.Text = "Ans"
                If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" Then
                    MsgBox("Cannot find the logarithm of a negative number")
                Else
                    PreAns = CDbl(lblAnswer.Text)
                    txtPreAns.Text = PreAns
                    lblScreen.Text = "log(" & lblAnswer.Text & ")="
                    Multiply = Math.Round(Math.Log(CDbl(lblAnswer.Text)) / Math.Log(10), 15)
                    txtAns.Text = Multiply
                    lblAnswer.Text = txtAns.Text
                End If
            Else
                Call Answers()
                If Microsoft.VisualBasic.Left(lblAnswer.Text, 1) = "-" Then
                    lblScreen.Text = "Ans"
                    MsgBox("Cannot find the logarithm of a negative number")
                Else
                    PreAns = CDbl(lblAnswer.Text)
                    txtPreAns.Text = PreAns
                    lblScreen.Text = "log(" & lblAnswer.Text & ")="
                    Multiply = Math.Round(Math.Log(CDbl(lblAnswer.Text)) / Math.Log(10), 15)
                    txtAns.Text = Multiply
                    lblAnswer.Text = txtAns.Text
                End If
            End If
        End If
    End Sub

    Private Sub cmdReciprocal_Click(sender As Object, e As EventArgs) Handles cmdReciprocal.Click
        If lblAnswer.Text = "" Then
            MsgBox("Please type in a number first")
            lblScreen.Text = "Ans"
            lblAnswer.Text = txtAns.Text
        Else
            Dim Multiply As Double
            Dim PreAns As Double
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or lblScreen.Text = "Ans" Then
                lblScreen.Text = "Ans"
                PreAns = CDbl(lblAnswer.Text)
                txtPreAns.Text = PreAns
                lblScreen.Text = "reciprocal(" & lblAnswer.Text & ")="
                Multiply = Math.Round(1 / (CDbl(lblAnswer.Text)), 15)
                txtAns.Text = Multiply
                lblAnswer.Text = txtAns.Text
            Else
                Call Answers()
                PreAns = CDbl(lblAnswer.Text)
                txtPreAns.Text = PreAns
                lblScreen.Text = "reciprocal(" & lblAnswer.Text & ")="
                Multiply = Math.Round(1 / (CDbl(lblAnswer.Text)), 15)
                txtAns.Text = Multiply
                lblAnswer.Text = txtAns.Text
            End If
        End If
    End Sub

    Private Sub cmdMemoryMinus_Click(sender As Object, e As EventArgs) Handles cmdMemoryMinus.Click
        If lblAnswer.Text = "" Then
            MsgBox("Please type in a number first")
            lblScreen.Text = "Ans"
            lblAnswer.Text = txtAns.Text
        Else
            Dim PreAns As Double
            Dim Memory As Double
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or lblScreen.Text = "Ans" Then
                PreAns = CDbl(lblAnswer.Text)
                Memory = CDbl(txtMemory.Text)
                txtMemory.Text = Memory - PreAns
            Else
                If InStr(1, lblScreen.Text, "+") > 0 Or InStr(1, lblScreen.Text, "-") Or InStr(1, lblScreen.Text, "*") Or InStr(1, lblScreen.Text, "/") Then
                    Call Answers()
                    PreAns = CDbl(lblAnswer.Text)
                    Memory = CDbl(txtMemory.Text)
                    txtMemory.Text = Memory - PreAns
                Else
                    PreAns = CDbl(lblAnswer.Text)
                    Memory = CDbl(txtMemory.Text)
                    txtMemory.Text = Memory - PreAns
                End If
            End If
            If txtMemory.Text = "0" Then
                lblMemory.Visible = False
            Else
                lblMemory.Visible = True
            End If
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Then
                lblScreen.Text = lblScreen.Text & ""
            Else
                lblScreen.Text = lblScreen.Text & "="
            End If
        End If
    End Sub

    Private Sub cmdSine_Click(sender As Object, e As EventArgs) Handles cmdSine.Click
        If lblAnswer.Text = "" Then
            MsgBox("Please type in a number first")
            lblScreen.Text = "Ans"
            lblAnswer.Text = txtAns.Text
        Else
            Dim Multiply As Double
            Dim PreAns As Double
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or lblScreen.Text = "Ans" Then
                lblScreen.Text = "Ans"
                PreAns = CDbl(lblAnswer.Text)
                txtPreAns.Text = PreAns
                lblScreen.Text = "sin(" & lblAnswer.Text & ")="
                Multiply = Math.Round(CDbl(Math.Sin(lblAnswer.Text * 4 * Math.Atan(1) / 180)), 15)
                txtAns.Text = Multiply
                lblAnswer.Text = txtAns.Text
            Else
                Call Answers()
                PreAns = CDbl(lblAnswer.Text)
                txtPreAns.Text = PreAns
                lblScreen.Text = "sin(" & lblAnswer.Text & ")="
                Multiply = Math.Round(CDbl(Math.Sin(lblAnswer.Text * 4 * Math.Atan(1) / 180)), 15)
                txtAns.Text = Multiply
                lblAnswer.Text = txtAns.Text
            End If
        End If
    End Sub

    Private Sub cmdCosine_Click(sender As Object, e As EventArgs) Handles cmdCosine.Click
        If lblAnswer.Text = "" Then
            MsgBox("Please type in a number first")
            lblScreen.Text = "Ans"
            lblAnswer.Text = txtAns.Text
        Else
            Dim Multiply As Double
            Dim PreAns As Double
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or lblScreen.Text = "Ans" Then
                lblScreen.Text = "Ans"
                PreAns = CDbl(lblAnswer.Text)
                txtPreAns.Text = PreAns
                lblScreen.Text = "cos(" & lblAnswer.Text & ")="
                Multiply = Math.Round(CDbl(Math.Cos(lblAnswer.Text * 4 * Math.Atan(1) / 180)), 15)
                txtAns.Text = Multiply
                lblAnswer.Text = txtAns.Text
            Else
                Call Answers()
                PreAns = CDbl(lblAnswer.Text)
                txtPreAns.Text = PreAns
                lblScreen.Text = "cos(" & lblAnswer.Text & ")="
                Multiply = Math.Round(CDbl(Math.Cos(lblAnswer.Text * 4 * Math.Atan(1) / 180)), 15)
                txtAns.Text = Multiply
                lblAnswer.Text = txtAns.Text
            End If
        End If
    End Sub

    Private Sub cmdTan_Click(sender As Object, e As EventArgs) Handles cmdTan.Click
        If lblAnswer.Text = "" Then
            MsgBox("Please type in a number first")
            lblScreen.Text = "Ans"
            lblAnswer.Text = txtAns.Text
        Else
            Dim Multiply As Double
            Dim PreAns As Double
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or lblScreen.Text = "Ans" Then
                lblScreen.Text = "Ans"
                PreAns = CDbl(lblAnswer.Text)
                txtPreAns.Text = PreAns
                lblScreen.Text = "tan(" & lblAnswer.Text & ")="
                Multiply = Math.Round(CDbl(Math.Tan(lblAnswer.Text * 4 * Math.Atan(1) / 180)), 15)
                txtAns.Text = Multiply
                lblAnswer.Text = txtAns.Text
            Else
                Call Answers()
                PreAns = CDbl(lblAnswer.Text)
                txtPreAns.Text = PreAns
                lblScreen.Text = "tan(" & lblAnswer.Text & ")="
                Multiply = Math.Round(CDbl(Math.Tan(lblAnswer.Text * 4 * Math.Atan(1) / 180)), 15)
                txtAns.Text = Multiply
                lblAnswer.Text = txtAns.Text
            End If
        End If
    End Sub

    Private Sub cmdClear_Click(sender As Object, e As EventArgs) Handles cmdClear.Click
        If lblAnswer.Text = "" Then
            lblAnswer.Text = lblAnswer.Text & ""
        Else
            lblAnswer.Text = Microsoft.VisualBasic.Left(lblAnswer.Text, Len(lblAnswer.Text) - 1)
        End If
        If lblScreen.Text = "" Then
            lblScreen.Text = lblScreen.Text & ""
        Else
            lblScreen.Text = Microsoft.VisualBasic.Left(lblScreen.Text, Len(lblScreen.Text) - 1)
        End If
    End Sub

    Private Sub cmdClearAll_Click(sender As Object, e As EventArgs) Handles cmdClearAll.Click
        lblScreen.Text = ""
        lblAnswer.Text = ""
        txtPreAns.Text = "0"
        txtOperator.Text = ""
    End Sub

    Private Sub cmdSquare_Click(sender As Object, e As EventArgs) Handles cmdSquare.Click
        If lblAnswer.Text = "" Then
            MsgBox("Please type in a number first")
            lblScreen.Text = "Ans"
            lblAnswer.Text = txtAns.Text
        Else
            Dim Multiply As Double
            Dim PreAns As Double
            If Microsoft.VisualBasic.Right(lblScreen.Text, 1) = "=" Or lblScreen.Text = "Ans" Then
                lblScreen.Text = "Ans"
                PreAns = CDbl(lblAnswer.Text)
                txtPreAns.Text = PreAns
                lblScreen.Text = lblAnswer.Text & "^2="
                Multiply = lblAnswer.Text ^ 2
                txtAns.Text = Multiply
                lblAnswer.Text = txtAns.Text
            Else
                Call Answers()
                PreAns = CDbl(lblAnswer.Text)
                txtPreAns.Text = PreAns
                lblScreen.Text = lblAnswer.Text & "^2="
                Multiply = lblAnswer.Text ^ 2
                txtAns.Text = Multiply
                lblAnswer.Text = txtAns.Text
            End If
        End If
    End Sub

    Private Sub frmStandard_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        cboMode.SelectedIndex = 0
    End Sub
End Class
