Public Class _Default
    Inherits System.Web.UI.Page

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

    End Sub

    Protected Sub SubmitButton_Click(ByVal sender As Object, ByVal e As EventArgs) Handles SubmitButton.Click
        Dim TicketsRodeo As Integer = 15
        Dim TicketsAward As Integer = 25
        Dim UserOrder As Integer = CInt(UserTickets.Text)
        Dim UserOrderTotal As Integer

        If AwardsCheckBox.Checked = True Then
            UserOrderTotal = (TicketsAward * UserOrder)
        Else
            UserOrderTotal = (TicketsRodeo * UserOrder)
        End If

        OrderTotalLabel.Text = "Order Total: " & FormatCurrency(UserOrderTotal.ToString)

        OrderConfirmLink.Visible = True
    End Sub
End Class