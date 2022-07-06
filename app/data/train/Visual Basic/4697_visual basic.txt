Public Class frmShop
    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        Me.Hide()
        My.Forms.MainMenu.Show() 'go to main menu

    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        Me.Hide()
        My.Forms.frmLevel.Show() 'go to level select
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        End 'quit :(
    End Sub

    Private Sub frmShop_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        lblPlayerScore.Text = Score
        lblPlayerName.Text = PlayerName 'load the player name
    End Sub

    Private Sub btnUpSpeed_Click(sender As Object, e As EventArgs) Handles btnUpSpeed.Click
        If Score = 20 Then 'makes sure you have funds
            Score -= 20 'takes away the funds
            UpSpeed = True 'sets boolean
            btnUpSpeed.Enabled = False 'disables button
            btnUpSpeed.Text = "Upgrade Speed - Purchased" 'changes text to show its been purchased
        Else MsgBox("ERROR: Insufficient funds.") 'error if you don't have enough money
        End If
    End Sub

    Private Sub btnWinPts_Click(sender As Object, e As EventArgs) Handles btnWinPts.Click
        If Score = 30 Then 'makes sure you have funds
            Score -= 30 'takes away the funds
            UpWinPts = True 'sets boolean
            btnWinPts.Enabled = False 'disables button
            btnWinPts.Text = "Upgrade Win Points - Purchased" 'changes text to show its been purchased
        Else MsgBox("ERROR: Insufficient funds.") 'error if you don't have enough money
        End If

    End Sub

    Private Sub btnDblPts_Click(sender As Object, e As EventArgs) Handles btnDblPts.Click
        If Score = 50 Then 'makes sure you have funds
            Score -= 50 'takes away the funds
            UpDblPts = True 'sets boolean
            btnDblPts.Enabled = False 'disables button
            btnDblPts.Text = "Upgrade Double Points - Purchased" 'changes text to show its been purchased
        Else MsgBox("ERROR: Insufficient funds.") 'error if you don't have enough money
        End If
    End Sub
End Class