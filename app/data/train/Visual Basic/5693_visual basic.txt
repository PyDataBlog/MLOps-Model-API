Imports System.Windows.Forms

Public Class NewVocabDialog

    Private Sub OK_Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OK_Button.Click
        Me.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.Close()
    End Sub

    Private Sub Cancel_Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Cancel_Button.Click
        Me.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.Close()
    End Sub

    Private Sub TextBox1_TextChanged() Handles VocabNameTextBox.TextChanged
        If VocabNameTextBox.Text = "" Then OK_Button.Enabled = False Else OK_Button.Enabled = True
    End Sub

End Class
