Imports Newtonsoft.Json
Imports Newtonsoft.Json.Linq

Public Class FormGlobalItem
    Private ButtonTarget As Button
    ' 状态效果相关
    Private IntEnchantments As Int16
    Private OldSelectedIndex As Int32 = -1
    Private StrEachEnchantmentJson(127) As String

    ' 常规
    Private Sub FormGlobalItem_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim i As Int16
        ' 读取 ComboBox 们
        ComboBoxEnchantment.Items.Add("")
        For i = 0 To UBound(ZhEnchantmentsIds)
            ComboBoxEnchantment.Items.Add(ZhEnchantmentsIds(i))
        Next
        ComboBoxPotion.Items.Add("")
        For i = 0 To UBound(ZhPotions)
            ComboBoxPotion.Items.Add(ZhPotions(i))
        Next
        ComboBoxItem.Items.Add("")
        For i = 0 To UBound(ZhItems)
            ComboBoxItem.Items.Add(ZhItems(i))
        Next
        ComboBoxPotion.Enabled = True
    End Sub
    Public Sub Reading(ByRef ButtonTarget As Button, StrItem As String())
        On Error Resume Next
        ' 显示本窗体
        Visible = False
        Show()
        ' 读取缺省信息
        ComboBoxItem.Items.Clear()
        If StrItem(0) <> "N/A" Then
            ComboBoxItem.Items.Add("")
            For i As Int16 = 0 To UBound(StrItem)
                ComboBoxItem.Items.Add(EnToZh(StrItem(i), ZhItems, EnItems))
            Next
            ComboBoxPotion.Enabled = False
        Else
            ComboBoxItem.Items.Add("")
            For i = 0 To UBound(ZhBlocks)
                ComboBoxItem.Items.Add(ZhBlocks(i))
            Next
            For i = 0 To UBound(ZhItems)
                ComboBoxItem.Items.Add(ZhItems(i))
            Next
            ComboBoxPotion.Enabled = True
        End If
        ' 读取传送过来的 Json 文本
        Reset()
        Me.ButtonTarget = ButtonTarget
        Dim ObjJson As JObject = CType(JsonConvert.DeserializeObject(ButtonTarget.Tag), JObject)
        If ObjJson.ToString <> "{}" Then
            ' 读取魔咒
            If ObjJson.Item("enchantments") IsNot Nothing Then
                For i = 0 To ObjJson.Item("enchantments").Count - 1
                    ListBoxEnchantments.Items.Add("魔咒" & IntEnchantments)
                    StrEachEnchantmentJson(i) = ObjJson.Item("enchantments").Item(i).ToString()
                Next
                ListBoxEnchantments.SelectedIndex = ListBoxEnchantments.Items.Count - 1
            End If
            ' 读取常规
            If ObjJson.Item("count") IsNot Nothing Then
                If ObjJson.Item("count").Item("max") IsNot Nothing Then
                    NumericUpDownCountMax.Value = ObjJson.Item("count").Item("max").ToString
                End If
                If ObjJson.Item("count").Item("min") IsNot Nothing Then
                    NumericUpDownCountMin.Value = ObjJson.Item("count").Item("min").ToString
                End If
                If ObjJson.Item("count").Item("max") Is Nothing And ObjJson.Item("count").Item("min") Is Nothing Then
                    If ObjJson.Item("count").ToString <> "{}" Then
                        NumericUpDownCountMax.Value = ObjJson.Item("count").ToString
                        NumericUpDownCountMin.Value = ObjJson.Item("count").ToString
                    End If
                End If
            End If
            If ObjJson.Item("data") IsNot Nothing Then
                If ObjJson.Item("data").Item("max") IsNot Nothing Then
                    NumericUpDownDataMax.Value = ObjJson.Item("data").Item("max").ToString
                End If
                If ObjJson.Item("data").Item("min") IsNot Nothing Then
                    NumericUpDownDataMin.Value = ObjJson.Item("data").Item("min").ToString
                End If
                If ObjJson.Item("data").Item("max") Is Nothing And ObjJson.Item("data").Item("min") Is Nothing Then
                    If ObjJson.Item("data").ToString <> "{}" Then
                        NumericUpDownDataMax.Value = ObjJson.Item("data").ToString
                        NumericUpDownDataMin.Value = ObjJson.Item("data").ToString
                    End If
                End If
            End If
            If ObjJson.Item("durability") IsNot Nothing Then
                If ObjJson.Item("durability").Item("max") IsNot Nothing Then
                    NumericUpDownDurabilityMax.Value = ObjJson.Item("durability").Item("max").ToString
                End If
                If ObjJson.Item("durability").Item("min") IsNot Nothing Then
                    NumericUpDownDurabilityMin.Value = ObjJson.Item("durability").Item("min").ToString
                End If
                If ObjJson.Item("durability").Item("max") Is Nothing And ObjJson.Item("durability").Item("min") Is Nothing Then
                    If ObjJson.Item("durability").ToString <> "{}" Then
                        NumericUpDownDurabilityMax.Value = ObjJson.Item("durability").ToString
                        NumericUpDownDurabilityMin.Value = ObjJson.Item("durability").ToString
                    End If
                End If
            End If
            If ObjJson.Item("item") IsNot Nothing Then
                ComboBoxItem.Tag = EnToZh(ObjJson.Item("item").ToString, ZhItems, EnItems)
                If ComboBoxItem.Tag = ObjJson.Item("item").ToString Then
                    ComboBoxItem.Tag = EnToZh(ObjJson.Item("item").ToString, ZhBlocks, EnBlocks)
                End If
                ComboBoxItem.Text = ComboBoxItem.Tag
            End If
            If ObjJson.Item("potion") IsNot Nothing Then
                ComboBoxPotion.Text = EnToZh(ObjJson.Item("potion").ToString, ZhPotions, EnPotions)
            End If
            If ObjJson.Item("nbt") IsNot Nothing Then
                TextBoxNbt.Text = ObjJson.Item("nbt").ToString.Replace("\" & Chr(34), Chr(34))
            End If
        End If
    End Sub
    Private Sub Writing(sender As Object, e As EventArgs) Handles ButtonEnter.Click
        Dim i As Int16
        Dim StrResult As String
        StrResult = "{"
        ' 写入魔咒
        SaveCurrentEnchantment(OldSelectedIndex)
        If ListBoxEnchantments.Items.Count >= 1 Then
            StrResult &= Chr(34) & "enchantments" & Chr(34) & ":" & "["
            For i = 0 To ListBoxEnchantments.Items.Count - 1
                StrResult &= StrEachEnchantmentJson(i) & ","
            Next
            StrResult &= "],"
        End If
        ' 写入常规
        If NumericUpDownCountMax.Value <> 0 Or NumericUpDownCountMin.Value <> 0 Then
            StrResult &= Chr(34) & "count" & Chr(34) & ":" & "{"
            If NumericUpDownCountMax.Value <> 0 Then
                StrResult &= Chr(34) & "max" & Chr(34) & ":" & NumericUpDownCountMax.Value & ","
            End If
            If NumericUpDownCountMin.Value <> 0 Then
                StrResult &= Chr(34) & "min" & Chr(34) & ":" & NumericUpDownCountMin.Value
            End If
            StrResult &= "},"
        End If
        If NumericUpDownDataMax.Value <> 0 Or NumericUpDownDataMin.Value <> 0 Then
            StrResult &= Chr(34) & "data" & Chr(34) & ":" & "{"
            If NumericUpDownDataMax.Value <> 0 Then
                StrResult &= Chr(34) & "max" & Chr(34) & ":" & NumericUpDownDataMax.Value & ","
            End If
            If NumericUpDownDataMin.Value <> 0 Then
                StrResult &= Chr(34) & "min" & Chr(34) & ":" & NumericUpDownDataMin.Value
            End If
            StrResult &= "},"
        End If
        If NumericUpDownDurabilityMax.Value <> 0 Or NumericUpDownDurabilityMin.Value <> 0 Then
            StrResult &= Chr(34) & "durability" & Chr(34) & ":" & "{"
            If NumericUpDownDurabilityMax.Value <> 0 Then
                StrResult &= Chr(34) & "max" & Chr(34) & ":" & NumericUpDownDurabilityMax.Value & ","
            End If
            If NumericUpDownDurabilityMin.Value <> 0 Then
                StrResult &= Chr(34) & "min" & Chr(34) & ":" & NumericUpDownDurabilityMin.Value
            End If
            StrResult &= "},"
        End If
        If ComboBoxItem.Text <> "" Then
            If ZhToEn(ComboBoxItem.Text, ZhItems, EnItems) <> ComboBoxItem.Text Then
                StrResult &= Chr(34) & "item" & Chr(34) & ":" & Chr(34) & ZhToEn(ComboBoxItem.Text, ZhItems, EnItems) & Chr(34) & ","
            Else
                StrResult &= Chr(34) & "item" & Chr(34) & ":" & Chr(34) & ZhToEn(ComboBoxItem.Text, ZhBlocks, EnBlocks) & Chr(34) & ","
            End If
        End If
            If ComboBoxPotion.Text <> "" Then
            StrResult &= Chr(34) & "potion" & Chr(34) & ":" & Chr(34) & ZhToEn(ComboBoxPotion.Text, ZhPotions, EnPotions) & Chr(34) & ","
        End If
        If TextBoxNbt.Text <> "" Then
            StrResult &= Chr(34) & "nbt" & Chr(34) & ":" & Chr(34) & TextBoxNbt.Text.Replace(Chr(34), "\" & Chr(34)) & Chr(34)
        End If
        StrResult &= "}"
        StrResult = StrResult.Replace(",}", "}")
        StrResult = StrResult.Replace(",]", "]")
        ' 将处理后的 Json 文本返回条件窗体
        ButtonTarget.Tag = StrResult
        Hide()
        ButtonTarget.Parent.Hide()
        ButtonTarget.Parent.Show()
    End Sub
    Private Sub Reset()
        IntEnchantments = 0
        OldSelectedIndex = -1
        ReDim StrEachEnchantmentJson(127)
        ComboBoxEnchantment.SelectedIndex = 0
        ComboBoxItem.SelectedIndex = 0
        ComboBoxPotion.SelectedIndex = 0
        ListBoxEnchantments.Items.Clear()
        NumericUpDownCountMax.Value = 0
        NumericUpDownCountMin.Value = 0
        NumericUpDownLevelsMax.Value = 0
        NumericUpDownLevelsMin.Value = 0
        NumericUpDownDataMax.Value = 0
        NumericUpDownDataMin.Value = 0
        NumericUpDownDurabilityMax.Value = 0
        NumericUpDownDurabilityMin.Value = 0
        TextBoxNbt.Text = ""
    End Sub

    ' 魔咒相关
    Private Sub ButtonAdd_Click(sender As Object, e As EventArgs) Handles ButtonAdd.Click
        ComboBoxEnchantment.Enabled = True
        NumericUpDownLevelsMax.Enabled = True
        NumericUpDownLevelsMin.Enabled = True
        ListBoxEnchantments.SelectedIndex = ListBoxEnchantments.Items.Add("魔咒" & IntEnchantments)
        ComboBoxEnchantment.Text = ""
        NumericUpDownLevelsMax.Value = 0
        NumericUpDownLevelsMin.Value = 0
        IntEnchantments += 1
    End Sub
    Private Sub ButtonDel_Click(sender As Object, e As EventArgs) Handles ButtonDel.Click
        StrEachEnchantmentJson(ListBoxEnchantments.SelectedIndex) = ""
        ListBoxEnchantments.Items.Remove(ListBoxEnchantments.SelectedItem)
    End Sub
    Private Sub ListBoxEnchantments_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ListBoxEnchantments.SelectedIndexChanged
        ' 保存旧的魔咒
        SaveCurrentEnchantment(OldSelectedIndex)
        If ListBoxEnchantments.SelectedIndex >= 0 Then
            OldSelectedIndex = Microsoft.VisualBasic.Right(ListBoxEnchantments.SelectedItem.ToString, ListBoxEnchantments.SelectedItem.ToString.Length - 2)
            ButtonDel.Enabled = True
        End If
        ' 读取新选中的编辑
        Dim ObjJson As JObject
        If ListBoxEnchantments.SelectedIndex >= 0 Then
            If StrEachEnchantmentJson(Microsoft.VisualBasic.Right(ListBoxEnchantments.SelectedItem.ToString, ListBoxEnchantments.SelectedItem.ToString.Length - 2)) <> "" Then
                ObjJson = CType(JsonConvert.DeserializeObject(StrEachEnchantmentJson(Microsoft.VisualBasic.Right(ListBoxEnchantments.SelectedItem.ToString, ListBoxEnchantments.SelectedItem.ToString.Length - 2))), JObject)
                If ObjJson.ToString <> "{}" Then
                    ComboBoxEnchantment.Text = EnToZh(ObjJson.Item("enchantment"), ZhEnchantmentsIds, EnEnchantmentsIds)
                    If ObjJson.Item("levels") IsNot Nothing Then
                        If ObjJson.Item("levels").Item("max") IsNot Nothing Then
                            NumericUpDownLevelsMax.Value = ObjJson.Item("levels").Item("max").ToString
                        Else
                            NumericUpDownLevelsMax.Value = 0
                        End If
                        If ObjJson.Item("levels").Item("min") IsNot Nothing Then
                            NumericUpDownLevelsMin.Value = ObjJson.Item("levels").Item("min").ToString
                        Else
                            NumericUpDownLevelsMin.Value = 0
                        End If
                        If ObjJson.Item("levels").Item("max") Is Nothing And ObjJson.Item("levels").Item("min") Is Nothing Then
                            If ObjJson.Item("levels").ToString <> "{}" Then
                                NumericUpDownLevelsMax.Value = ObjJson.Item("levels").ToString
                                NumericUpDownLevelsMin.Value = ObjJson.Item("levels").ToString
                            End If
                        End If
                    Else
                        NumericUpDownLevelsMax.Value = 0
                        NumericUpDownLevelsMin.Value = 0
                    End If
                Else
                    NumericUpDownLevelsMax.Value = 0
                    NumericUpDownLevelsMin.Value = 0
                End If
            End If
        End If
    End Sub
    Private Sub SaveCurrentEnchantment(OldSelectedIndex As Int32)
        ' 保存当前的魔咒
        If OldSelectedIndex >= 0 Then
            Dim StrResult As String = "{"
            If NumericUpDownLevelsMin.Value <> 0 Or NumericUpDownLevelsMax.Value <> 0 Then
                StrResult &= Chr(34) & "levels" & Chr(34) & ":{"
                If NumericUpDownLevelsMin.Value <> 0 Then
                    StrResult &= Chr(34) & "min" & Chr(34) & ":" & NumericUpDownLevelsMin.Value & ","
                End If
                If NumericUpDownLevelsMax.Value <> 0 Then
                    StrResult &= Chr(34) & "max" & Chr(34) & ":" & NumericUpDownLevelsMax.Value
                End If
                StrResult &= "},"
            End If
            If ComboBoxEnchantment.Text <> "" Then
                StrResult &= Chr(34) & "enchantment" & Chr(34) & ":" & Chr(34) & ZhToEn(ComboBoxEnchantment.Text, ZhEnchantmentsIds, EnEnchantmentsIds) & Chr(34)
            End If
            StrResult &= "}"
            StrResult = StrResult.Replace(",}", "}")
            StrResult = StrResult.Replace(",]", "]")
            StrEachEnchantmentJson(OldSelectedIndex) = StrResult
        End If
    End Sub

    Private Sub ComboBoxItem_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBoxItem.SelectedIndexChanged
        If ZhToEn(ComboBoxItem.Text, ZhItems, EnItems) = "minecraft:potion" Or
           ZhToEn(ComboBoxItem.Text, ZhItems, EnItems) = "minecraft:splash_potion" Or
           ZhToEn(ComboBoxItem.Text, ZhItems, EnItems) = "minecraft:lingering_potion" Then
            ComboBoxPotion.Enabled = True
        Else
            ComboBoxPotion.Enabled = False
        End If
    End Sub
End Class