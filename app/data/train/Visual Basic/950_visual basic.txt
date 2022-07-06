Imports System.Xml
Public Class Welcome

    Private _FunctionDoc As New XmlDocument
    Private _nodeSubset
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        
        _FunctionDoc.Load("FunctionData.xml")
        Dim selection = ListBox1.SelectedItem.ToString
        _nodeSubset = _FunctionDoc.SelectNodes("/help/Category[@name='" + selection + "']/Command/*")
        Dim SectionForm = New Section
        SectionForm.TitleLabel.Text = SectionForm.TitleLabel.Text + " " + selection
        SectionForm.PrePopulateTreeViewControl(_nodeSubset)
        SectionForm._selection = selection
        SectionForm.Show()
        Me.Hide()

    End Sub


End Class
