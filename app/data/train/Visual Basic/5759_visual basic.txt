Imports xdevs23.Localization

Public Class LabelUpdater

    Private Shared LM As LanguageManager

    Public Shared Sub SetLangManager(ByRef LangManager As LanguageManager)
        LM = LangManager
    End Sub

    Public Shared Sub UpdateLabel(Lbl As Label, Text As String)
        Lbl.Text = Text
    End Sub

    Public Shared Sub UpdateLabel(Lbl As Label, Format As String, Text As String)
        UpdateLabel(Lbl, String.Format(Format, Text))
    End Sub

    Public Shared Sub UpdateLabel(Lbl As Label, Format As String, ParamArray Texts As String())
        UpdateLabel(Lbl, String.Format(Format, Texts))
    End Sub

    Public Shared Sub UpdateLabel(Lbl As Label, ParamArray Texts As String())
        UpdateLabel(Lbl, String.Format(LM.GetString(Lbl.Tag.ToString()), 
                                       IIf(Texts(0).StartsWith("==") And Texts(0).EndsWith("=="),
                                           LM.GetString(Texts(0).Replace("==", "")), Texts)))
    End Sub

End Class
