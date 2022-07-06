set path=%path%;C:\Windows\Microsoft.NET\Framework\v4.0.30319
@powershell -NoProfile -ExecutionPolicy Bypass -File "%systemdrive%\vagrant\scripts\005_InstallWindowsFeatures.ps1"
