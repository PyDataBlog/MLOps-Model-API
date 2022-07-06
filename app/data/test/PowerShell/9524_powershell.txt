$content = Get-Content .\install-webpi-essentials.txt
$content | ForEach-Object {
    cinst $_ -y -source webpi
}
