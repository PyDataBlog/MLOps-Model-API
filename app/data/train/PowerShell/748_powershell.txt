function Guard-ArgumentNotNullOrEmpty {
    param(
        [string]$ArgumentName,
        $ArgumentValue
    )

    Guard-ArgumentNotNull $ArgumentName $ArgumentValue

    if($ArgumentValue -is [System.Collections.IEnumerable]) {
        if (!$ArgumentValue.GetEnumerator().MoveNext()) {
            throw (New-Object System.ArgumentException("Argument was empty.", $ArgumentName))
        }
    }

    if($ArgumentValue -eq $null) {
        throw (New-Object System.ArgumentException("Value cannot be null.", $ArgumentName))
    } elseif($ArgumentValue -is [string] -and $ArgumentValue -eq [string]::Empty) {
        throw (New-Object System.ArgumentException("Argument was empty.", $ArgumentName))
    }
}