Partial Class RomsDataSet
    Partial Public Class SSRomsDataTable
        Private Sub SSRomsDataTable_ColumnChanging(sender As Object, e As DataColumnChangeEventArgs) Handles Me.ColumnChanging
            If (e.Column.ColumnName = Me.local_rommd5Column.ColumnName) Then
                'Add user code here
            End If

        End Sub

    End Class
End Class
