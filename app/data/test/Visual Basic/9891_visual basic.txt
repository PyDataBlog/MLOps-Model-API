Imports Microsoft.VisualBasic.FileIO

Public Class CsvImport
    Public Shared Function NewDataTable(fileName As String, delimiters As String, Optional firstRowContainsFieldNames As Boolean = True) As DataTable
        Dim result As New DataTable()

        Using tfp As New TextFieldParser(fileName)
            tfp.SetDelimiters(delimiters)

            ' Get Some Column Names
            If Not tfp.EndOfData Then
                Dim fields As String() = tfp.ReadFields()

                For i As Integer = 0 To fields.Count() - 1
                    If firstRowContainsFieldNames Then
                        result.Columns.Add(fields(i))
                    Else
                        result.Columns.Add("Col" + i)
                    End If
                Next

                ' If first line is data then add it
                If Not firstRowContainsFieldNames Then
                    result.Rows.Add(fields)
                End If
            End If

            ' Get Remaining Rows
            While Not tfp.EndOfData
                result.Rows.Add(tfp.ReadFields())
            End While
        End Using

        Return result
    End Function
End Class
