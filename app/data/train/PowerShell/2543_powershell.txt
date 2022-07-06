function Get-FileVersionName {
    param (
        [string] $Version
    )
    return "Trunk-2.0-Commit_$Version"
}