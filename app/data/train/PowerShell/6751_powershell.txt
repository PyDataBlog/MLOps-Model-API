#REQUIRES -Version 4.0

Write-Host -ForegroundColor Green "Starting . . ."

$path64 = "C:\Windows\System32\MicTray64.exe"
$path32 = "C:\Windows\System32\MicTray.exe"
$logPath = "C:\Users\Public\MicTray.log"

if (Test-Path -Path $path64) {
    Try {
        Get-Process MicTray64 | Select-Object -Property Id | Stop-Process
    } Catch {
        $ErrorMessage = $_.Exception.Message
        Write-Host $ErrorMessage
    }

    Try {
        Remove-Item -Path $path64 -Force
    } Catch {
        $ErrorMessage = $_.Exception.Message
        Write-Host $ErrorMessage
    }
}

if (Test-Path -Path $path32) {
    Try {
        Get-Process MicTray32 | Select-Object -Property Id | Stop-Process
    } Catch {
        $ErrorMessage = $_.Exception.Message
        Write-Host $ErrorMessage
    }

    Try {
        Remove-Item -Path $path32 -Force
    } Catch {
        $ErrorMessage = $_.Exception.Message
        Write-Host $ErrorMessage
    }
}

if (Test-Path -Path $logPath) {
    Try {
        Remove-Item -Path $logPath -Force
    } Catch {
        $ErrorMessage = $_.Exception.Message
        Write-Host $ErrorMessage
    }
}

Write-Host -ForegroundColor Green "Done!"
