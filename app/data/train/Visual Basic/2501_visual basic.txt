Public Class Boggle_options
    ' Game time
    Const TimeUnlimited As String = "No timer (unlimited)"
    Const TimeThirtySecond As String = "30 seconds"
    Const TimeOneMinute As String = "1 minute"
    Const TimeOneHalfMinute As String = "1.5 minutes"
    Const TimeTwoMinute As String = "2 minutes"
    Const TimeThreeMinute As String = "3 minutes (default)"
    Const TimeFiveMinute As String = "5 minutes"

    Private Sub Options_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Reset (so we don't get duplicated) and add time options
        ctrlTime.Items.Clear()
        ctrlTime.Items.AddRange({TimeUnlimited, TimeThirtySecond, TimeOneMinute, TimeOneHalfMinute, TimeTwoMinute, TimeThreeMinute, TimeFiveMinute})
        ctrlTime.Text = TimeThreeMinute ' Default option
    End Sub

    ' Convert selected time option into seconds
    Private Function ResolveTime()
        Dim selector As String = ctrlTime.SelectedItem.ToString()
        Select Case selector
            Case TimeUnlimited
                Return -1
            Case TimeThirtySecond
                Return 30
            Case Else
                Return Int(selector.Split()(0)) * 60
        End Select
    End Function
    Private Sub OKButton_Click(sender As System.Object, e As System.EventArgs) Handles btnOK.Click
        Boggle.GameTime = ResolveTime()
        Boggle.SetGameState(3)
        Boggle.ButtonEvent() ' Immitate game reset
        Me.Close()
    End Sub

    Private Sub btnCancel_Click(sender As Object, e As EventArgs) Handles btnCancel.Click
        Me.Close()
    End Sub
End Class
