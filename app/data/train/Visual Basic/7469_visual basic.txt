Public Class 分析_詞性
    Public Function 分析_詞性(字 As Char) As String
        If "我你妳您他她它伱誰".Contains(字) Then Return "代詞"
        If "的是".Contains(字) Then Return "助詞"
        If "今昨尋明後".Contains(字) Then Return "時間"
        Return ""
    End Function
End Class
