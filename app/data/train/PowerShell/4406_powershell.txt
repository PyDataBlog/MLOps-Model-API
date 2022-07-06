##############################################################################
## Author: William T. Webb
## Filename: autoFileMove.PS1
## Purpose: Recursively check for file extension types from sourceDir and move them to targetDir
###############################################################################


#$sourceDir = "C:\sourceDir\"
#$targetDir = "C:\targetDir\"
#$fileTypesToTransfer = @("*.doc", "*.xls", "*.ppt", "*.pdf", "*.docx", "*.xlsx", "*.pptx")

#gci -Path $sourceDir -Include $fileTypesToTransfer -Recurse | mi -Destination $targetDir

#Get-ChildItem -Path $sourceDir -Filter *.doc -Recurse | Move-Item -Destination $targetDir
#Get-ChildItem -Path $sourceDir -Filter *.xls -Recurse | Move-Item -Destination $targetDir
#Get-ChildItem -Path $sourceDir -Filter *.ppt -Recurse | Move-Item -Destination $targetDir

#Get-ChildItem -Path $sourceDir -Filter *.pdf -Recurse | Move-Item -Destination $targetDir

#Get-ChildItem -Path $sourceDir -Filter *.docx -Recurse | Move-Item -Destination $targetDir
#Get-ChildItem -Path $sourceDir -Filter *.xlsx -Recurse | Move-Item -Destination $targetDir
#Get-ChildItem -Path $sourceDir -Filter *.pptx -Recurse | Move-Item -Destination $targetDir

<#
$sourceDir = "d:\test\"
$targetDir = "d:\organised\"
$fileTypesToTransfer = @("*.doc", "*.xls", "*.ppt", "*.pdf", "*.docx", "*.xlsx", "*.pptx")

Try
{
gci -Path $sourceDir -Include $fileTypesToTransfer -Recurse | mi -Destination $targetDir -ErrorAction Stop
}
Catch 
{
$ErrorMessage = $_.Exception.Message
break
}
Finally
{
$time=Get-Date
"$time | Exception: $ErrorMessage" | Out-File d:\organised\history.txt -Append
}

Exit
#>

$sourceDir = "C:\sourceDirectory\"
$targetDir = "C:\targetDirectory\"
$logDir    = "C:\error.log"

$fileTypesToTransfer = @("*.doc", "*.xls", "*.ppt", "*.pdf", "*.docx", "*.xlsx", "*.pptx")

$timeStamp = (Get-Date).ToString("HH:mm:ss");
$files = gci -Path $sourceDir -Include $fileTypesToTransfer -Recurse

foreach ($f in $files) {
    mi -Path $f -Destination $targetDir -ErrorAction SilentlyContinue -ErrorVariable move_errors 
    "@ " + $timeStamp + " : " + $move_errors | Out-File $logDir -Append
    }

Exit


