try {
    $installPath = Join-Path $PSHome  "Modules\PSWindowsUpdate"
    Remove-Item -Recurse -Force $installPath
    Write-ChocolateySuccess 'PSWindowsUpdate'
} catch {
  Write-ChocolateyFailure 'PSWindowsUpdate' $($_.Exception.Message)
  throw
}