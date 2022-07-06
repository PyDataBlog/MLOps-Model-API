$Filelocation = "C:\CLUS2016Sessions\CLUS2016.csv" #Mac/Linux example: "~/CLUS2016Sessions/raw_download_urls.txt"
$SaveLocation = "C:\CLUS2016Sessions\" #Mac/Linux example: "~/CLUS2016Sessions/"
$List = Import-CSV $Filelocation -Header "ID", "Link"
If (Get-Module BitsTransfer -ErrorAction SilentlyContinue) {
    $Bits=$true
    Write-Host "Using BITS transfer for faster more reliable downloads"
}

Foreach ($Session in $List) {
    if (Test-Path "$($Savelocation)$($Session.ID).mp4") {
        Write-Host "$($Session.ID) already downloaded... Skipping"
    } Else {
        Write-Host "Downloading Session $($Session.ID)"
        If ($Bits) {
            Start-BitsTransfer -Source $Session.Link -Destination "$($Savelocation)$($Session.ID).mp4"
        } Else {
            Invoke-WebRequest -Uri $Session.Link -OutFile "$($Savelocation)$($Session.ID).mp4"
        }
    }
}
