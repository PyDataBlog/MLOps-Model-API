[System.Reflection.Assembly]::LoadWithPartialName("System.Windows.Forms") | Out-Null
$str = [string] ([System.Windows.Forms.Clipboard]::GetText())
$wd=New-Object -ComObject Word.Application
$doc=$wd.Documents.Open("C:\Users\royk\Documents\Card.docx")
$wd.Visible=$true
$fPath = $doc.AttachedTemplate.FullName
$cPath = $doc.AttachedTemplate.Path
$enc = New-Object -ComObject System.Security.Cryptography.MD5CryptoServiceProvider
$i=$str.Split(",")
$doc.Fields(1).Select
$wd.Selection.Text = $i.Item(0)

$doc.Fields(1).Select
$wd.Selection.Text = $i.Item(1)


'$b = con $str & ","  Get-Date()
b = enc.ComputeHash_2((b))
For pos = 1 To UBound(b)
        hsh = hsh & LCase(Right(Hex(AscB(MidB(b, pos, 1))), 2))
Next

$doc.Sections(1).Footers(wdHeaderFooterPrimary).Range.InsertBefore hsh & vbTab & vbTab
$doc.PrintOut
DoEvents

End If
Me.Close False

'If MsgBox("Would you like to add another patient?", vbYesNo) = vbYes Then
'Word.Documents.Add fPath
'
'End If

Word.Application.Quit
$wd.Visible=$true
