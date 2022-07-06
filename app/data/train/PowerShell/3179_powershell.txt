function Clean{
    rm flot -Recurse -Force -ErrorAction SilentlyContinue
    rm flot*.zip -ErrorAction SilentlyContinue
    rm pkg -Recurse -Force -ErrorAction SilentlyContinue
}

function Update{
    nuget update -self

    $source = "http://www.flotcharts.org/downloads/flot-0.8.3.zip"
    $destination = (Join-Path (pwd) "flot.zip")
    $wc = New-Object System.Net.WebClient
    $wc.DownloadFile($source, $destination)
}

function Package{
    mkdir flot
    & "C:\Program Files\7-Zip\7z.exe" x "flot.zip"

    mkdir pkg\Content\Scripts\flot
    cp flot\*.js pkg\Content\Scripts\flot -Exclude jquery.js,jquery.min.js
    cp flot.nuspec pkg
    nuget pack pkg\flot.nuspec
}

Clean
Update
Package
