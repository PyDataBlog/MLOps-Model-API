
    Function ToBase64(Str As String, Optional Base64 As String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/") As String
        ToBase64 = String.Concat(System.Text.Encoding.Default.GetBytes(Str).Select(Function(b) Convert.ToString(b, 2).PadLeft(8, "0")))
        Dim Len = ToBase64.Length - CInt(Math.Floor(ToBase64.Length / 6)) * 6
        If Len > 0 Then
            Return String.Concat(Enumerable.Range(0, CInt(Math.Floor(ToBase64.Length / 6))).Select(Function(b) Base64(Convert.ToInt32(ToBase64.Substring(b * 6, 6), 2)))) & Base64(Convert.ToInt32(ToBase64.Substring((CInt(Math.Floor(ToBase64.Length / 6)) * 6), Len).ToString.PadRight(6, "0"), 2)) & If(Len = 2, "==", "=")
        Else
            Return String.Concat(Enumerable.Range(0, ToBase64.Length / 6).Select(Function(b) Base64(Convert.ToInt32(ToBase64.Substring(b * 6, 6), 2))))
        End If
    End Function

    Function FromBase64(Str As String, Optional Base64 As String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/") As String
        FromBase64 = String.Concat(Str.Replace("=", "").Select(Function(b) Convert.ToString(Base64.IndexOf(b), 2).PadLeft(6, "0")))
        Return System.Text.Encoding.Default.GetString(Enumerable.Range(0, CInt(Math.Floor(FromBase64.Length / 8))).Select(Function(b) Convert.ToByte(FromBase64.Substring(b * 8, 8), 2)).ToArray)
    End Function
    
