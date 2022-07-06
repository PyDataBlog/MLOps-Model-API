Imports Newtonsoft.Json
Imports Newtonsoft.Json.Linq

Public Class FormGlobalDamage
    Private ButtonTarget As Button

    Private Sub Reset()
        ButtonDirectEntity.Tag = "{}"
        ButtonSourceEntity.Tag = "{}"
        ButtonType.Tag = "{}"
        RadioButtonBlockedNull.Checked = True
        RadioButtonBlockedFalse.Checked = False
        RadioButtonBlockedTrue.Checked = False
        NumericUpDownDealtMin.Value = 0
        NumericUpDownDealtMax.Value = 0
        NumericUpDownTakenMin.Value = 0
        NumericUpDownTakenMax.Value = 0
    End Sub

    Public Sub Reading(ByRef ButtonTarget As Button)
        On Error Resume Next
        ' 显示本窗体
        Visible = False
        Show()
        ' 读取传送过来的 Json 文本
        Reset()
        Me.ButtonTarget = ButtonTarget
        Dim ObjJson As JObject = CType(JsonConvert.DeserializeObject(ButtonTarget.Tag), JObject)
        If ObjJson.ToString <> "{}" Then
            If ObjJson.Item("blocked") IsNot Nothing Then
                Select Case ObjJson.Item("blocked").ToString.ToLower
                    Case "true"
                        RadioButtonBlockedTrue.Checked = True
                    Case "false"
                        RadioButtonBlockedFalse.Checked = True
                End Select
            End If
            If ObjJson.Item("dealt") IsNot Nothing Then
                If ObjJson.Item("dealt").Item("max") IsNot Nothing Then
                    NumericUpDownDealtMax.Value = ObjJson.Item("dealt").Item("max").ToString
                End If
                If ObjJson.Item("dealt").Item("min") IsNot Nothing Then
                    NumericUpDownDealtMin.Value = ObjJson.Item("dealt").Item("min").ToString
                End If
                If ObjJson.Item("dealt").Item("max") Is Nothing And ObjJson.Item("dealt").Item("min") Is Nothing Then
                    If ObjJson.Item("dealt").ToString <> "{}" Then
                        NumericUpDownDealtMax.Value = ObjJson.Item("dealt").ToString
                        NumericUpDownDealtMin.Value = ObjJson.Item("dealt").ToString
                    End If
                End If
            End If
            If ObjJson.Item("taken") IsNot Nothing Then
                If ObjJson.Item("taken").Item("max") IsNot Nothing Then
                    NumericUpDownTakenMax.Value = ObjJson.Item("dealt").Item("max").ToString
                End If
                If ObjJson.Item("taken").Item("min") IsNot Nothing Then
                    NumericUpDownTakenMin.Value = ObjJson.Item("dealt").Item("min").ToString
                End If
                If ObjJson.Item("taken").Item("max") Is Nothing And ObjJson.Item("taken").Item("min") Is Nothing Then
                    If ObjJson.Item("taken").ToString <> "{}" Then
                        NumericUpDownTakenMax.Value = ObjJson.Item("taken").ToString
                        NumericUpDownTakenMin.Value = ObjJson.Item("taken").ToString
                    End If
                End If
            End If
            If ObjJson.Item("direct_entity") IsNot Nothing Then
                ButtonDirectEntity.Tag = ObjJson.Item("direct_entity").ToString
            End If
            If ObjJson.Item("source_entity") IsNot Nothing Then
                ButtonSourceEntity.Tag = ObjJson.Item("source_entity").ToString
            End If
            If ObjJson.Item("type") IsNot Nothing Then
                ButtonType.Tag = ObjJson.Item("type").ToString
            End If
        End If
    End Sub

    Private Sub Wirting(sender As Object, e As EventArgs) Handles ButtonEnter.Click
        Dim StrResult As String
        StrResult = "{"
        If RadioButtonBlockedFalse.Checked Then
            StrResult &= Chr(34) & "blocked" & Chr(34) & ":false,"
        ElseIf RadioButtonBlockedTrue.Checked Then
            StrResult &= Chr(34) & "blocked" & Chr(34) & ":true,"
        End If
        If NumericUpDownDealtMin.Value <> 0 Or NumericUpDownDealtMax.Value <> 0 Then
            StrResult &= Chr(34) & "dealt" & Chr(34) & ":" & "{"
            If NumericUpDownDealtMin.Value <> 0 Then
                StrResult &= Chr(34) & "min" & Chr(34) & ":" & NumericUpDownDealtMin.Value & ","
            End If
            If NumericUpDownDealtMax.Value <> 0 Then
                StrResult &= Chr(34) & "max" & Chr(34) & ":" & NumericUpDownDealtMax.Value
            End If
            StrResult &= "},"
        End If
        If NumericUpDownTakenMin.Value <> 0 Or NumericUpDownTakenMax.Value <> 0 Then
            StrResult &= Chr(34) & "taken" & Chr(34) & ":" & "{"
            If NumericUpDownTakenMin.Value <> 0 Then
                StrResult &= Chr(34) & "min" & Chr(34) & ":" & NumericUpDownTakenMin.Value & ","
            End If
            If NumericUpDownTakenMax.Value <> 0 Then
                StrResult &= Chr(34) & "max" & Chr(34) & ":" & NumericUpDownTakenMax.Value
            End If
            StrResult &= "},"
        End If
        If ButtonDirectEntity.Tag <> "{}" Then
            StrResult &= Chr(34) & "direct_entity" & Chr(34) & ":" & ButtonDirectEntity.Tag & ","
        End If
        If ButtonSourceEntity.Tag <> "{}" Then
            StrResult &= Chr(34) & "source_entity" & Chr(34) & ":" & ButtonSourceEntity.Tag & ","
        End If
        If ButtonType.Tag <> "{}" Then
            StrResult &= Chr(34) & "type" & Chr(34) & ":" & ButtonType.Tag
        End If
        StrResult &= "}"
        StrResult = StrResult.Replace(",}", "}")
        StrResult = StrResult.Replace(",]", "]")
        ButtonTarget.Tag = StrResult
        Hide()
        ButtonTarget.Parent.Hide()
        ButtonTarget.Parent.Show()
    End Sub

    Private Sub ButtonDirectEntity_Click(sender As Object, e As EventArgs) Handles ButtonDirectEntity.Click
        FormGlobalEntity.Reading(ButtonDirectEntity, {"N/A"})
    End Sub

    Private Sub ButtonSourceEntity_Click(sender As Object, e As EventArgs) Handles ButtonSourceEntity.Click
        FormGlobalEntity.Reading(ButtonSourceEntity, {"N/A"})
    End Sub

    Private Sub ButtonType_Click(sender As Object, e As EventArgs) Handles ButtonType.Click
        FormGlobalKillingBlow.Reading(ButtonType)
    End Sub
End Class