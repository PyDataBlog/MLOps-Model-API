Imports System.Windows.Forms

Public Class ModifyEntryDialog
    Dim index As Integer

    Private Sub OK_Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OK_Button.Click
        Me.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.Close()
    End Sub

    Private Sub Cancel_Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Cancel_Button.Click
        Me.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.Close()
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BrowsetButton.Click
        OpenFileDialog.FileName = EntryTextBox.Text
        If OpenFileDialog.ShowDialog() = vbOK Then EntryTextBox.Text = OpenFileDialog.FileName
    End Sub

    Public Sub SetIndex(ByVal index As Integer)
        Me.index = index
    End Sub

    Public Function GetIndex() As Integer
        Return Me.index
    End Function
End Class
