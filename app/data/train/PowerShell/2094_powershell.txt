<#
    .SYNOPSIS
    Downloads the VMworld 2017 US & Europe Breakout Session videos.

    .DESCRIPTION
    The VMworld 2017 Breakout Session video lists are downloaded from
    William Lam's GitHub page (https://github.com/lamw/vmworld2017-session-urls).
    The retrieved markdown files are parsed to identify the Session
    name, associated YouTube URL, and category. The script then relies
    on the YouTube-DL Windows executable (https://rg3.github.io/youtube-dl/download.html)
    to download the videos.

    .EXAMPLE
    get-Vmworld2017Videos
    Downloads the VMworld 2017 US & Europe Breakout Session videos.

    .NOTES
    The 'youtube-dl.exe' executable must be downloaded and placed
    into the same directory as this script. Remember to set
    $VerbosePreference = 'Continue'
    if you want to see verbose script output.

    .LINK
    https://teebeedee.org
    The first link is opened by Get-Help -Online Read-WebPage

    .INPUTS
    No inputs.

    .OUTPUTS
    No outputs.
#>

$videoLists = @{
    'US' = 'https://github.com/lamw/vmworld2017-session-urls/raw/master/vmworld-us-playback-urls.md'
    'Europe' = 'https://github.com/lamw/vmworld2017-session-urls/raw/master/vmworld-eu-playback-urls.md'
}

Function Read-WebPage {
    param (
        [Parameter(Mandatory)]
        [String]
        $Uri
    )

    # Invoke-WebRequest and Invoke-RestMethod can't work properly with UTF-8 Response so we need to do things this way.
    [Net.HttpWebRequest]$WebRequest = [Net.WebRequest]::Create($Uri)
    [Net.HttpWebResponse]$WebResponse = $WebRequest.GetResponse()
    $Reader = New-Object -TypeName IO.StreamReader -ArgumentList ($WebResponse.GetResponseStream())
    $Response = $Reader.ReadToEnd()
    $Reader.Close()
    return $Response
}

Function Remove-InvalidFileNameChars {
    param(
        [Parameter(Mandatory)]
        [string]
        $FileName
    )

    $invalidChars = [IO.Path]::GetInvalidFileNameChars() -Join ''
    $re = ('[{0}%]' -f [RegEx]::Escape($invalidChars))
    return ($FileName -Replace $re, '-')
}

Function Get-VmworldVideos {
    param
    (
        [Parameter(Mandatory)]
        [string]
        $Name,

        [Parameter(Mandatory)]
        [string]
        $Url
    )

    # Read Markdown file with list of videos, parsing for Session Name, Link and Category
        
    $videos = @()
    $currentCategory = ''
    $regexCategory = '(?<=## ).+(?= \(\d+\))'
    $regexSession = '\[[A-Z]+[0-9]+[A-Z]+ - .*\]\(.*\)'
    $regexSessionName = '(?<=\[)[A-Z]+[0-9]+[A-Z]+ - .*(?=\]\(.*\))'
    $regexSessionLink = '(?<=\[[A-Z]+[0-9]+[A-Z]+ - .*\]\().*(?=\))'
    Switch -RegEx ((Read-WebPage -Uri $Url).Split([Environment]::NewLine)) {
        $regexCategory {
            $currentCategory = $switch.Current | Select-String -Pattern $regexCategory | ForEach-Object{$_.matches} | ForEach-Object{$_.value}
            Write-Verbose -Message ('New {0} Category: {1}' -f $Name, $currentCategory)
        }
        $regexSession {
            # Parse if $currentCategory isn't empty
            If ($currentCategory -Ne '') {
                $currentSessionName = ($switch.Current | Select-String -Pattern $regexSessionName | ForEach-Object{$_.matches} | ForEach-Object{$_.value}).Trim() -Replace '\?\?\?',"'"
                $currentSessionLink = ($switch.Current | Select-String -Pattern $regexSessionLink | ForEach-Object{$_.matches} | ForEach-Object{$_.value}).Trim()
                Write-Verbose -Message ('Found {0} Session: {1}' -f $Name, $currentSessionName)
                $currentVideo = New-Object -TypeName PSObject
                $currentVideo | Add-Member -Type NoteProperty -Name 'Name' -Value $currentSessionName
                $currentVideo | Add-Member -Type NoteProperty -Name 'Link' -Value $currentSessionLink
                $currentVideo | Add-Member -Type NoteProperty -Name 'Category' -Value $currentCategory
                $videos += $currentVideo
            }
        }
        default {}
    }

    # Download each Session Video - Requires YouTube-DL executable (https://rg3.github.io/youtube-dl/)
    ForEach ($video in $videos) {
        $downloadCommand = Join-Path -Path $PSScriptRoot -ChildPath 'youtube-dl.exe'
        $downloadArgs = @(
            '--format', '22',
            '--output', ('"{0}/{1}/{2}.%(ext)s"' -f $Name, (Remove-InvalidFileNameChars -FileName $video.Category), (Remove-InvalidFileNameChars -FileName $video.Name)),
            '--no-check-certificate',
            ('{0}' -f $video.Link)
        )
        Write-Verbose -Message ('Downloading "{0}" from {1}' -f $video.Name, $video.Link)
        & $downloadCommand $downloadArgs
    }
}

$videoLists.GetEnumerator() | ForEach-Object {
    Get-VmworldVideos -Name $_.key -Url $_.value
}
