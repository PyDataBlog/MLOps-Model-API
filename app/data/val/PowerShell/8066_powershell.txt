$conf = '{ "AuthType": "ptc", "GoogleUsername": null, "GooglePassword": null, "PtcUsername": "hfreanzr", "PtcPassword": "cnffjbeq" }'
$root = $PSScriptRoot
$accounts = Get-Content -Encoding ascii "$root\accounts.txt"
$count = $accounts.Length
$i = 1
foreach ($bot in $accounts) {
    $user = $bot.Split(",") |Select-Object -First 1
    $pass = $bot.Split(",") |Select-Object -Last 1
    Write-Host [$i of $count] Spinning up bot $user
    Out-File -InputObject ($conf.Replace("cnffjbeq",$pass).Replace("hfreanzr",$user)) -FilePath "$root\Config\auth.json" -Encoding ascii
    Start-Process -WindowStyle Hidden -FilePath "$root\NecroBot.exe"
    Start-Sleep -Seconds 1
    $i++
}
