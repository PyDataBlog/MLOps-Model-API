Namespace QueryObjects.CustomerQuery
    ''' <summary>
    ''' Used to compare the dates of two InvoiceDetail objects
    ''' </summary>
    ''' <remarks>Return 0 if the dates are equal, else returns 1</remarks>
    <Serializable()> Public Class DateComparer
        Implements IComparer(Of InvoiceDetail)

        Friend Function Compare(ByVal x As InvoiceDetail, ByVal y As InvoiceDetail) As Integer Implements IComparer(Of InvoiceDetail).Compare

            If x.InvoiceDate = Date.MinValue Then
                If y.InvoiceDate = Date.MinValue Then
                    ' If x is Nothing and y is Nothing, they're
                    ' equal. 
                    Return 0
                Else
                    ' If x is Nothing and y is not Nothing, y
                    ' is greater. 
                    Return -1
                End If
            Else
                ' If x is not Nothing...
                '
                If y.InvoiceDate = Date.MinValue Then
                    ' ...and y is Nothing, x is greater.
                    Return 1
                Else
                    ' ...and y is not Nothing, compare the Dates
                    Return x.InvoiceDate.CompareTo(y.InvoiceDate)
                End If
            End If
        End Function
    End Class
End Namespace
