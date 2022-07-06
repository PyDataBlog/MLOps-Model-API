# Use the terminal to run this code. 
# Or, using the Code Runner extention, highlight the code then click Run Code

Write-Output "Push Pop Directory exercise"
New-Item -Name "temp" -ItemType Directory | Out-Null
Push-Location -Path "temp"
Write-Output "Current path: $(get-location)"
Pop-Location
Remove-Item "temp"