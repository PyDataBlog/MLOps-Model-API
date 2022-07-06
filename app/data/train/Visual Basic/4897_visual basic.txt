Public Class frmQE

    Private Sub TextBox2_TextChanged(sender As Object, e As EventArgs) Handles txtB.TextChanged

    End Sub

    Private Sub cmdClearAll_Click(sender As Object, e As EventArgs) Handles cmdClearAll.Click
        lblX1.Text = ""
        lblX2.Text = ""
        txtA.Text = ""
        txtB.Text = ""
        txtC.Text = ""
    End Sub

    Private Sub cmdAnswer_Click(sender As Object, e As EventArgs) Handles cmdAnswer.Click
        If (Val(txtA.Text) = 0 And (txtA.Text <> "0" Or txtA.Text = "0")) Or (Val(txtB.Text) = 0 And txtB.Text <> "0") Or (Val(txtC.Text) = 0 And txtC.Text <> "0") Then
            MsgBox("Please type in a real number in all the textboxes. Note that a cannot be zero")
        Else
            Dim A, B, C, D, M, F, G, H, X1, X2, UpOne, UpTwo, I, T, V As Double
            A = CDbl(txtA.Text)
            B = CDbl(txtB.Text)
            C = CDbl(txtC.Text)
            D = B ^ 2
            M = 4 * A * C
            F = D - M
            txtNegative.Text = CStr(F)
            If Microsoft.VisualBasic.Left(txtNegative.Text, 1) = "-" Then
                T = Microsoft.VisualBasic.Right(txtNegative.Text, (Len(txtNegative.Text) - 1))
                V = CDbl(T)
                G = Math.Sqrt(V)
                H = 2 * A
                I = -B
                UpOne = (I / H)
                UpTwo = (G / H)
                X1 = Math.Round(UpOne, 2)
                X2 = Math.Round(UpTwo, 4)
                If Microsoft.VisualBasic.Left(txtA.Text, 1) = "-" Then
                    lblX1.Text = "x1=" & X1 & "-" & (-1 * X2) & "i"
                Else
                    lblX1.Text = "x1=" & X1 & "+" & X2 & "i"
                End If

                If Microsoft.VisualBasic.Left(txtA.Text, 1) = "-" Then
                    lblX2.Text = "x2=" & X1 & "+" & (-1 * X2) & "i"
                Else
                    lblX2.Text = "x2=" & X1 & "-" & X2 & "i"
                End If
            Else
                G = Math.Sqrt(F)
                H = 2 * A
                I = -B
                UpOne = I + G
                UpTwo = I - G
                X1 = Math.Round((UpOne / H), 6)
                X2 = Math.Round((UpTwo / H), 6)
                lblX1.Text = "x1=" & X1
                lblX2.Text = "x2=" & X2
            End If
        End If
    End Sub

    Private Sub frmQE_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        cboMode.SelectedIndex = 1
    End Sub


    Private Sub cboMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cboMode.SelectedIndexChanged
        If cboMode.SelectedIndex = 1 Then
            Me.Show()
            frmStandard.Hide()
            frmSE.Hide()
            Me.cboMode.SelectedIndex = 1
            Me.lblX1.Text = ""
            Me.lblX2.Text = ""
            Me.txtA.Text = ""
            Me.txtB.Text = ""
            Me.txtC.Text = ""
        Else
            If cboMode.SelectedIndex = 0 Then
                Me.Hide()
                frmStandard.Show()
                frmSE.Hide()
                frmStandard.cboMode.SelectedIndex = 0
                frmStandard.lblScreen.Text = ""
                frmStandard.lblAnswer.Text = ""
            Else
                If cboMode.SelectedIndex = 2 Then
                    Me.Hide()
                    frmStandard.Hide()
                    frmSE.Show()
                    frmSE.cboMode.SelectedIndex = 2
                    frmSE.txtA1.Text = ""
                    frmSE.txtA2.Text = ""
                    frmSE.txtB1.Text = ""
                    frmSE.txtB2.Text = ""
                    frmSE.txtC1.Text = ""
                    frmSE.txtC2.Text = ""
                    frmSE.lblX.Text = ""
                    frmSE.lblY.Text = ""
                End If
            End If
        End If
    End Sub
End Class