Public Class frmMoney
    Dim generator As New Random
    Dim varNumber As Integer = generator.Next(0, 8)
    Dim varFirstCoin As Decimal
    Dim varSecondCoin As Decimal
    Dim varTotal As Decimal
    Dim varAnswer As Decimal
    Dim varCorrect As Boolean = False
    Private Sub frmMenu_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        txtWelcome.Text = "Hello " + General.Name + " please answer the money questions"
        txtCount.Text = General.Answered + 1 & " of " & General.MaxQuestion + 1
        GetMoney()
    End Sub

    Private Sub btnMenu_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnMenu.Click
        General.ClearResults()
        General.MenuAction(Me, frmMenu)
    End Sub

    Private Sub CheckAns(ByVal sender As Object)
        varAnswer = String.Format("{0:C}", sender.Text)
        If (varTotal = varAnswer) Then
            imgDetails.Visible = True
            txtDetails.Visible = True
            txtDetails.Text = "Well Done"
            imgDetails.Image = My.Resources.accept
            If varCorrect = False Then
                General.Correct = General.Correct + 1
            End If
            varCorrect = True
            btnNext.Focus()
        Else
            imgDetails.Visible = True
            txtDetails.Visible = True
            txtDetails.Text = "Try Again"
            imgDetails.Image = My.Resources.delete
        End If
    End Sub

    Private Sub btnNext_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnNext.Click
        If General.Answered <> General.MaxQuestion Then
            varCorrect = False
            imgDetails.Visible = False
            txtDetails.Visible = False
            GetMoney()
            General.Answered = General.Answered + 1
            txtCount.Text = General.Answered + 1 & " of " & General.MaxQuestion + 1
        Else
            General.Topic = "Money"
            General.MenuAction(Me, frmAward)
        End If
    End Sub

    Private Sub GetMoney()
        Dim temp As Integer = generator.Next(0, 2)
        If temp = 0 Then
            varFirstCoin = NewMoney(pbMoney)
            varSecondCoin = NewMoney(pbMoeny2)
            varTotal = varFirstCoin + varSecondCoin
            money1.Text = String.Format("{0:C}", varTotal)
            money2.Text = String.Format("{0:C}", NewMoney())
            Do While money1.Text = money2.Text
                money2.Text = String.Format("{0:C}", NewMoney())
            Loop

        ElseIf temp = 1 Then
            varFirstCoin = NewMoney(pbMoney)
            varSecondCoin = NewMoney(pbMoeny2)
            varTotal = varFirstCoin + varSecondCoin
            money1.Text = String.Format("{0:C}", NewMoney)
            money2.Text = String.Format("{0:C}", varTotal)
            Do While money1.Text = money2.Text
                money1.Text = String.Format("{0:C}", NewMoney)
            Loop
        End If
    End Sub

    Public Sub NewMoney(ByVal first As Decimal, ByVal coinlbl As ComponentFactory.Krypton.Toolkit.KryptonWrapLabel, ByVal coin As PictureBox)
        varNumber = generator.Next(0, 8)
        Select Case varNumber
            Case 0
                first = 0.01
            Case 1
                first = 0.02
            Case 2
                first = 0.05
            Case 3
                first = 0.1
            Case 4
                first = 0.2
            Case 5
                first = 0.5
            Case 6
                first = 1.0
            Case 7
                first = 2.0
        End Select
        coinlbl.Text = String.Format("{0:C}", first)
        coin.Image = imgMoney.Images(varNumber)
    End Sub
    Public Function NewMoney(ByVal coin As PictureBox)
        Dim first As Decimal
        varNumber = generator.Next(0, 8)
        Select Case varNumber
            Case 0
                first = 0.01
            Case 1
                first = 0.02
            Case 2
                first = 0.05
            Case 3
                first = 0.1
            Case 4
                first = 0.2
            Case 5
                first = 0.5
            Case 6
                first = 1.0
            Case 7
                first = 2.0
        End Select
        coin.Image = imgMoney.Images(varNumber)
        Return first
    End Function
    Public Function NewMoney()
        Dim first As Decimal
        varNumber = generator.Next(0, 8)
        Select Case varNumber
            Case 0
                first = 0.01
            Case 1
                first = 0.02
            Case 2
                first = 0.05
            Case 3
                first = 0.1
            Case 4
                first = 0.2
            Case 5
                first = 0.5
            Case 6
                first = 1.0
            Case 7
                first = 2.0
        End Select
        Return first
    End Function

    Private Sub money_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles money1.Click, money2.Click
        CheckAns(sender)
    End Sub
End Class
