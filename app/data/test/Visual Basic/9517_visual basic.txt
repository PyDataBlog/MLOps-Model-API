

Partial Public Class Database1DataSet1
    Partial Class loginDataTable

        Private Sub loginDataTable_loginRowChanging(sender As Object, e As loginRowChangeEvent) Handles Me.loginRowChanging

        End Sub

        Private Sub loginDataTable_ColumnChanging(sender As Object, e As DataColumnChangeEventArgs) Handles Me.ColumnChanging
            If (e.Column.ColumnName = Me.isAdminColumn.ColumnName) Then
                'Add user code here
            End If

        End Sub

    End Class

End Class
