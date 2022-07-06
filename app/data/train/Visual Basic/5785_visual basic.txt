Public Class Form1
    Inherits MetroFramework.Forms.MetroForm

    Dim hash As Hashtable
    Dim tasks As List(Of String) = New List(Of String)
    Private Sub MetroTile3_Click(sender As Object, e As EventArgs) Handles MetroTile3.Click
        Try
            ListBox1.Items.Remove(ListBox1.SelectedItem)
        Catch ex As Exception
            MessageBox.Show("No item selected.")
        End Try
    End Sub

    Private Sub MetroTile2_Click(sender As Object, e As EventArgs) Handles MetroTile2.Click
        ListBox1.Items.Clear()
    End Sub

    Private Sub MetroTile1_Click(sender As Object, e As EventArgs) Handles MetroTile1.Click
        If String.IsNullOrWhiteSpace(MetroTextBox1.Text) Then
            MessageBox.Show("Empty task titles are not allowed.", "Productivity")
            Exit Sub
        End If
        If Not tasks.Contains(MetroTextBox1.Text) Then
            ListBox1.Items.Add(MetroTextBox1.Text)
            tasks.Add(MetroTextBox1.Text)
        End If
    End Sub

    Private Sub MetroTile4_Click(sender As Object, e As EventArgs) Handles MetroTile4.Click
        Animator1.Hide(MetroTile2)
        Animator1.Hide(MetroTile3)
        hash = New Hashtable
        Animator1.Show(MetroPanel1)
        Animator1.Hide(MetroLabel1)
        Animator1.Hide(MetroTextBox1)
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        MetroPanel1.Visible = False
        MetroPanel2.Visible = False
        MetroTile6.Enabled = False
        MetroPanel3.Visible = False
        MetroTile7.Enabled = False
    End Sub

    Private Sub MetroTile5_Click(sender As Object, e As EventArgs) Handles MetroTile5.Click
        ListView1.View = View.Details
        If ListView1.Columns.Count = 0 Then
            ListView1.Columns.Add("Rank")
            ListView1.Columns.Add("Task")
        End If
        If hash.ContainsValue(MetroTextBox2.Text) Then
            MessageBox.Show("Duplicates not allowed.")
            Exit Sub
        End If
        If tasks.Contains(MetroTextBox2.Text) Then
            ListView1.Items.Add(New ListViewItem(New String() {(ListView1.Items.Count + 1).ToString(), MetroTextBox2.Text}))
            hash.Add(MetroTextBox2.Text, ListView1.Items.Count + 1)
        Else
            MessageBox.Show("The task you've given doesn't exist in the list of tasks.")
            Exit Sub
        End If
        If ListView1.Items.Count = ListBox1.Items.Count Then
            MetroTextBox2.Enabled = False
            MetroTile5.Enabled = False
            MetroTile6.Enabled = True
        End If
    End Sub

    Private Sub MetroTile6_Click(sender As Object, e As EventArgs) Handles MetroTile6.Click
        Animator1.Hide(MetroPanel1)
        Animator1.Show(MetroPanel2)
    End Sub

    Private Sub MetroTile8_Click(sender As Object, e As EventArgs) Handles MetroTile8.Click
        ListView2.View = View.Details
        If ListView2.Columns.Count = 0 Then
            ListView2.Columns.Add("Rank")
            ListView2.Columns.Add("Task")
        End If
        If ListView2.Items.ContainsKey(MetroTextBox3.Text) Then
            MessageBox.Show("Duplicates not allowed.")
            Exit Sub
        End If
        If tasks.Contains(MetroTextBox3.Text) Then
            ListView2.Items.Add(New ListViewItem(New String() {(ListView2.Items.Count + 1).ToString(), MetroTextBox3.Text}))
            'hash.Add(MetroTextBox3.Text, ListView2.Items.Count + 1)
            'Dim rank As Integer = hash(MetroTextBox3.Text)
            'rank += ListView2.Items.Count
            'hash(MetroTextBox3.Text) = rank
        Else
            MessageBox.Show("The task you've given doesn't exist in the list of tasks.")
            Exit Sub
        End If
        If ListView2.Items.Count = ListBox1.Items.Count Then
            MetroTextBox3.Enabled = False
            MetroTile8.Enabled = False
            MetroTile7.Enabled = True
        End If
    End Sub

    Public Shared Function SplitList(Of T)(items As List(Of T), groupCount As Integer) As List(Of List(Of T))
        Dim allGroups As New List(Of List(Of T))()

        'split the list into equal groups
        Dim startIndex As Integer = 0
        Dim groupLength As Integer = CInt(Math.Round(CDbl(items.Count) / CDbl(groupCount), 0))
        While startIndex < items.Count
            Dim group As New List(Of T)()
            group.AddRange(items.GetRange(startIndex, groupLength))
            startIndex += groupLength

            'adjust group-length for last group
            If startIndex + groupLength > items.Count Then
                groupLength = items.Count - startIndex
            End If

            allGroups.Add(group)
        End While

        'merge last two groups, if more than required groups are formed
        If allGroups.Count > groupCount AndAlso allGroups.Count > 2 Then
            allGroups(allGroups.Count - 2).AddRange(allGroups.Last())
            allGroups.RemoveAt(allGroups.Count - 1)
        End If

        Return (allGroups)
    End Function
    Private Sub MetroTile7_Click(sender As Object, e As EventArgs) Handles MetroTile7.Click
        hash = New Hashtable
        For Each s As String In ListBox1.Items
            Dim lv As ListViewItem = ListView1.FindItemWithText(s)
            Dim val As Integer = CInt(lv.SubItems(0).Text)
            Dim lv1 As ListViewItem = ListView2.FindItemWithText(s)
            val += CInt(lv1.SubItems(0).Text)
            hash.Add(s, val)
        Next
        Dim lst As List(Of DictionaryEntry) = hash.Cast(Of DictionaryEntry)().OrderBy(Function(entry) entry.Value).ToList()
        Dim r As New List(Of String)
        For i As Integer = 0 To lst.Count - 1
            r.Add(lst(i).Key.ToString())
        Next

        Animator1.Hide(MetroPanel2)
        Animator1.Hide(ListBox1)

        Dim final As List(Of List(Of String)) = SplitList(r, 4)
        For Each s As String In final(0)
            ListBox3.Items.Add(s)
        Next
        For Each s As String In final(1)
            ListBox4.Items.Add(s)
        Next
        For Each s As String In final(2)
            ListBox2.Items.Add(s)
        Next
        For Each s As String In final(3)
            ListBox5.Items.Add(s)
        Next
        Animator1.Show(MetroPanel3)
    End Sub
End Class
