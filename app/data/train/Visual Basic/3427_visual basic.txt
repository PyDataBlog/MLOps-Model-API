Public Class Main

    Private Sub pb_gown_MouseHover(sender As Object, e As EventArgs) Handles pb_gown.MouseHover
        pb_gown.BackgroundImage = CType(My.Resources.ResourceManager.GetObject("gown_hover"), Image)
    End Sub

    Private Sub pb_gown_MouseLeave(sender As Object, e As EventArgs) Handles pb_gown.MouseLeave
        pb_gown.BackgroundImage = CType(My.Resources.ResourceManager.GetObject("gown"), Image)
    End Sub

    Private Sub pb_interview_MouseHover(sender As Object, e As EventArgs) Handles pb_interview.MouseHover
        pb_interview.BackgroundImage = CType(My.Resources.ResourceManager.GetObject("interview_hover"), Image)
    End Sub

    Private Sub pb_interview_MouseLeave(sender As Object, e As EventArgs) Handles pb_interview.MouseLeave
        pb_interview.BackgroundImage = CType(My.Resources.ResourceManager.GetObject("interview"), Image)
    End Sub

    Private Sub pb_swimwear_MouseHover(sender As Object, e As EventArgs) Handles pb_swimwear.MouseHover
        pb_swimwear.BackgroundImage = CType(My.Resources.ResourceManager.GetObject("swimwear_hover"), Image)
    End Sub

    Private Sub pb_swimwear_MouseLeave(sender As Object, e As EventArgs) Handles pb_swimwear.MouseLeave
        pb_swimwear.BackgroundImage = CType(My.Resources.ResourceManager.GetObject("swimwear"), Image)
    End Sub

    Private Sub pb_talent_MouseHover(sender As Object, e As EventArgs) Handles pb_talent.MouseHover
        pb_talent.BackgroundImage = CType(My.Resources.ResourceManager.GetObject("talent_hover"), Image)
    End Sub

    Private Sub pb_talent_MouseLeave(sender As Object, e As EventArgs) Handles pb_talent.MouseLeave
        pb_talent.BackgroundImage = CType(My.Resources.ResourceManager.GetObject("talent"), Image)
    End Sub

    Private Sub pb_gown_Click(sender As Object, e As EventArgs) Handles pb_gown.Click
        If isEnabled("Gown") = True Then
            lbl_event_id.Text = Functions.getId("SELECT id FROM t_event WHERE name = 'gown'")
            CandidateList1.lbl_event.Text = "Gown"
            CandidateList1.Show()
            Me.Hide()
        Else
            ScoreList.populateDGV("SELECT number, image, candidate, score FROM v_gown_overall ORDER BY score DESC")
            ScoreList.Text = "Gown"
            ScoreList.Show()
        End If

    End Sub

    Private Sub pb_interview_Click(sender As Object, e As EventArgs) Handles pb_interview.Click
        If isEnabled("Interview") = True Then
            lbl_event_id.Text = Functions.getId("SELECT id FROM t_event WHERE name = 'interview'")
            CandidateList1.lbl_event.Text = "Interview"
            CandidateList1.Show()
            Me.Hide()
        Else
            ScoreList.populateDGV("SELECT number, image, candidate, score FROM v_interview_overall ORDER BY score DESC")
            ScoreList.Text = "Interview"
            ScoreList.Show()
        End If
    End Sub

    Private Sub pb_swimwear_Click(sender As Object, e As EventArgs) Handles pb_swimwear.Click
        If isEnabled("Swimwear") = True Then
            lbl_event_id.Text = Functions.getId("SELECT id FROM t_event WHERE name = 'swimwear'")
            CandidateList1.lbl_event.Text = "Swimwear"
            CandidateList1.Show()
            Me.Hide()
        Else
            ScoreList.populateDGV("SELECT number, image, candidate, score FROM v_swimwear_overall ORDER BY score DESC")
            ScoreList.Text = "Swimwear"
            ScoreList.Show()
        End If
    End Sub

    Private Sub pb_talent_Click(sender As Object, e As EventArgs) Handles pb_talent.Click
        If isEnabled("Talent") = True Then
            lbl_event_id.Text = Functions.getId("SELECT id FROM t_event WHERE name = 'talent'")
            CandidateList1.lbl_event.Text = "Talent"
            CandidateList1.Show()
            Me.Hide()
        Else
            ScoreList.populateDGV("SELECT number, image, candidate, score FROM v_talent_overall ORDER BY score DESC")
            ScoreList.Text = "Talent"
            ScoreList.Show()
        End If
    End Sub

    Private Sub Main_FormClosing(sender As Object, e As FormClosingEventArgs) Handles MyBase.FormClosing
        Dim sql As String = "UPDATE t_judge SET status = 0 WHERE id = '" & lbl_judge_id.Text & "'"

        Try
            Connect.constring.Open()
            Functions.com.Connection = Connect.constring
            Functions.com.CommandText = sql
            Functions.com.ExecuteNonQuery()
        Catch ex As Exception
            MsgBox(ex.Message)
        Finally
            Connect.constring.Close()
            Login.Show()
        End Try
    End Sub
End Class