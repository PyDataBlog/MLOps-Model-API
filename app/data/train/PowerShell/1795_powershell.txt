cls
<#
==== Created by benjamin-fahl.de
==== Author Benjamin Fahl
==== @bf87
#>

# get script dir to find config file (when running as background task)
$scriptDir = (split-path $SCRIPT:MyInvocation.MyCommand.Path -parent)
$iniFile = "$scriptDir\config.ini"

if (!([System.IO.FileInfo]"$iniFile").Exists) {
    Write-Host "[ERROR] Config file not found, should be $iniFile"
    pause
    exit 0
}

# Parse-IniFile by http://stackoverflow.com/users/406859/wafflesouffle
function Parse-IniFile ($file)
{
  $ini = @{}
  switch -regex -file $file
  {
    #Section.
    "^\[(.+)\]$"
    {
      $section = $matches[1].Trim()
      $ini[$section] = @{}
      continue
    }
    #Int.
    "^\s*([^#].+?)\s*=\s*(\d+)\s*$"
    {
      $name,$value = $matches[1..2]
      $ini[$section][$name] = [int]$value
      continue
    }
    #Decimal.
    "^\s*([^#].+?)\s*=\s*(\d+\.\d+)\s*$"
    {
      $name,$value = $matches[1..2]
      $ini[$section][$name] = [decimal]$value
      continue
    }
    #Everything else.
    "^\s*([^#].+?)\s*=\s*(.*)"
    {
      $name,$value = $matches[1..2]
      $ini[$section][$name] = $value.Trim()
    }
  }
  $ini
  
}
# get config from file
$config = (Parse-IniFile $iniFile)

if ($config.SYSTEM.baseDir -eq "notSet") {
    Write-Host "[ERROR] BaseDir no set, check your $iniFile"
    pause
    exit 0
}
#  #  #  #  #  # Functions

# # # Helper Functions
function hostmsg {
    param
    (
        [string]$texte,
        [string]$isLog = 0
    )
    $msgdate = get-Date -Format "yyyy-MM-dd HH:mm:ss"

    if ($isLog -gt 0) {

        Write-Host -ForegroundColor $config.SYSTEM.textColorLogger -BackgroundColor $config.SYSTEM.textColorBackground "$msgdate $texte"

    } else {

        Write-Host -ForegroundColor $config.SYSTEM.textColor -BackgroundColor $config.SYSTEM.textColorBackground "$msgdate $texte"

    }
  
}
$pathDone = ($config.SYSTEM.baseDir + "\" + $config.SYSTEM.folderDone)
$logfileDate = get-Date -Format "yyyy-MM-dd"
$logfilePath = ($config.SYSTEM.baseDir + "\" + $config.LOGGER.folder)
$logfileName = ($config.SYSTEM.baseDir + "\" + $config.LOGGER.folder+"\$logfileDate - "+ $config.LOGGER.filename+".log")
function logger {
    param
    (
        [string]$strMessage
    )
    if ($config.LOGGER.status -eq "on") {

        $date = get-Date -Format "yyyy-MM-dd HH:mm:ss"
        

        # check/create logfile path
        if (!([System.IO.DirectoryInfo]$logfilePath).Exists) {

            %(New-Item -ItemType Directory -Force -Path "$logfilePath" | hostmsg "[SETUP] Log Directory Created" 1)
        }
        # remove old log file
        if (!([System.IO.FileInfo]"$logfileName").Exists -and $config.LOGGER.clean -eq "on") {

            Remove-Item -Force -Path "$logfilePath"
            %(New-Item -ItemType File -Path "$pathLogFile" | hostmsg "[SETUP] ReCreating LogFile" 1)
        }
        
        # check/create logfile
        if (!([System.IO.FileInfo]"$logfileName").Exists) {

            %(New-Item -ItemType File -Path "$logfileName" | hostmsg "[SETUP] LogFile created" 1)
        }

        switch ($config.LOGGER.mode) 
        { 
            "quite" { 
                        Add-Content -Force -path $logfileName -value ($date + "`t:`t"+ $strMessage) 
                    } 
            "normal" {
                        Add-Content -Force -path $logfileName -value ($date + "`t:`t"+ $strMessage)
                        hostmsg "[LOGGED] $strMessage" 1
                    } 
            "show" { 
                        hostmsg "[LOGGED] $strMessage" 1 
                    } 
            
            default { 
                        Add-Content -Force -path $logfileName -value ($date + "`t:`t"+ "[ERROR] unknow logger mode: "+ $config.LOGGER.mode)
                        endit -text ("[ERROR] unknow logger mode: "+ $config.LOGGER.mode)
                    }
        }

    }
    
}

function endit {
    param
    (
        [string]$text
    )
    if ($text) {

        hostmsg "[SYSTEM] Process ended with Message: $text"

    } else {

        hostmsg "[SYSTEM] Process ended"

    }

    if ($config.SYSTEM.headless -eq "off") {
        pause
    }

    exit 0
        
}

function sickbeardPostProcess {

    if ($config.SYSTEM.sickBeard -eq "on") {

        $url = ("http://" + $config.SICKBEARD.ip + ":" + $config.SICKBEARD.port+"/api/"+$config.SICKBEARD.apikey+"/?cmd=postprocess")
        $qry = invoke-WebRequest -Uri $url -Method Get;
        $json = ConvertFrom-Json $qry.Content

        hostmsg -texte ("[SickBeard] "+$json.message)
        logger ("[SickBeard] "+$json.message)

    } else {

        logger ("[SickBeard] skipped by config")

    }
}

logger ("-- [MRClean] -- Startup -- ")

# # # Setup Functions
function createDirectorys {
    # Setup Directorys
    logger ("[SETUP] Directorys Starting")
    $path = @(
        ($config.SYSTEM.baseDir),
        ($config.SYSTEM.baseDir + "\" + $config.SYSTEM.folderDone),
        ($config.SYSTEM.baseDir + "\" + $config.SYSTEM.folderDownload),
        ($config.SYSTEM.baseDir + "\" + $config.LOGGER.folder)
    )

    # Creating Directorys
    $path | foreach {
        if (!([System.IO.DirectoryInfo]"$_").Exists) {

            %(New-Item -ItemType Directory -Force -Path "$_" | logger ("[SETUP] "+$_.ToString().Replace($config.SYSTEM.baseDir+"\","")+" Directory created"))

        } else { 

            logger ("[SETUP] "+$_.ToString().Replace($config.SYSTEM.baseDir+"\","")+" Directory ok" )

        }
    }

    logger ("[SETUP] Directorys Done")
}

function createTestData {

    if ($config.SYSTEM.testRun -eq "on") {
        
        logger "[TESTRUN] Started"
        $pathTest = ($config.SYSTEM.baseDir + "\" + $config.SYSTEM.folderTest)

                # check/create logfile path
        if (!([System.IO.DirectoryInfo]$pathTest).Exists) {

            %(New-Item -ItemType Directory -Force -Path "$pathTest" | logger "[TESTRUN] Test Directory Created")

        } else {
            logger "[TESTRUN] Test Directory ok"
        }

        $testDir1 = "$pathTest\testDir"
        $testDir2 = "$testDir1\testDirLvl2"
        $testDir3 = "$testDir2\testDirLvl3"
        $testDir4 = "$testDir1\Sample"
        $testDir5 = "$pathTest\downloading"
        $testDir6 = "$pathTest\downloadingRAR"

        [ARRAY]$testFilesDirs = @("$testDir1","$testDir2","$testDir3", "$testDir4", "$testDir5", "$testDir6")

        $testFile1 = "$pathTest\blabla-1080p-720p-badshort-lvl1.mkv"
        $testFile2 = "$testDir2\banana-1080p-720p-dl-badbad-lvl2.mkv"
        $testFile3 = "$testDir3\test.1080p.720p.badshort.badbad-lvl3.mkv"
        $testFile4 = "$testDir4\blabla.1080p.720p.dl.euhd.tvp.tvs-lvl3-sample.mkv"
        $testFile5 = "$testDir5\blabla.1080p.720p.dl.euhd.tvp.tvs-lvl3.rar"
        $testFile6 = "$testDir6\blabla.1080p.720p.dl.euhd.tvp.tvs-lvl3.chunk0"
        
        [ARRAY]$testFiles = @("$testFile1","$testFile2","$testFile3","$testFile4","$testFile5","$testFile6")

        $testFilesDirs | foreach {
            if (!([System.IO.DirectoryInfo]"$_").Exists) {
                %(New-Item -ItemType Directory -Force -Path "$_" | logger ("[TESTSETUP] Test Directory: "+$_.ToString().Replace("$pathTest\","")+" created")) 
            }
        }
        $testFiles | foreach { 
            if (!([System.IO.FileInfo]"$_").Exists) {
                %(New-Item -ItemType File -Path "$_" | logger ("[TESTSETUP] Test File: "+$_.ToString().Replace("$pathTest\","")+" created"))
            }
        }
        if ($testRun -eq $true) {
            logger "[TESTSETUP] TestRun is Active, moving Data"
                $testdata = @(gci $pathTest -Recurse )
                $testdata | foreach { 
                    try { 
                        move-Item -Path $_.fullname -Destination $pathDL -Confirm:$false -force -ea SilentlyContinue
                    } 
                    catch {
                        $error = $_.GetType().FullName
                        logger "[ERROR] $error"
                    }
                
                }
                logger "[TESTSETUP] TestData moved"
        }
        logger "[TESTSETUP] Done"
    } else {
        logger ("[TestRun] skipped by config")
    }
}

logger "[SETUP] started"

createDirectorys

logger "[SETUP] done"

createTestData

Function MoveFiles() {
    logger "[MoveFiles] Start moving files"
    hostmsg ("===== MoveFiles - Files started")

    $moveFilesTypes = @("*.iso","*.img","*.mkv","*.avi","*.mp4","*.mpeg","*.mov","*.mvts")
    try {
        $counterMoved = 0
        $pathDL = ($config.SYSTEM.baseDir + "\" + $config.SYSTEM.folderDownload)
        $moveFilesTypes | foreach {
            Get-ChildItem -path $pathDL -Filter "$_" -Exclude "*sample*" -ea SilentlyContinue -Recurse -File | foreach {

                $counterMoved++
                Move-Item $_.FullName $pathDone -Confirm:$false -Force
                hostmsg ("===== MoveFile - $_")
                logger "[CleanFiles] Moved - $_"
            }    
        }
    } catch {
        $error = $_.GetType().FullName
        logger "[ERROR] Move: $error"
    }
    logger "[MoveFiles] moving done - moved $counterMoved"
    hostmsg ("===== MoveFiles - Files done")
}

Function CleanDirectorys {
    logger "[CleanDirectorys] Start moving files"
    hostmsg ("===== CleanDirectorys - started")

    $deleteFilesTypes = @("*.nfo", "*.mkv", "*.avi", "*.mp4", "*.mpeg", "*.mov", "*.mvts")

    try {
        $counterDeleted = 0
        $pathDL = ($config.SYSTEM.baseDir + "\" + $config.SYSTEM.folderDownload)

        $deleteFilesTypes | foreach {
            Get-ChildItem -path $pathDL -Filter "$_" -ea SilentlyContinue -Recurse -File | foreach {

                $counterDeleted++
                Remove-Item $_.FullName -Confirm:$false -Force
                logger "[CleanDirectorys] deleted - $_"
            } 
        }
    } catch {
        $error = $_.GetType().FullName
        logger "[ERROR] CleanDirectorys: $error"
    }

    logger "[CleanDirectorys] done - deleted $counterDeleted"
    hostmsg ("===== CleanDirectorys - done - deleted $counterDeleted")
}

Function CleanupEmptyDirectorys(){

    logger "[CleanupEmptyDirectorys] Deleting started"
    hostmsg ("===== CleanupEmptyDirectorys - started")

    try {
        $unsafeRemove = @()
        $pathDL = ($config.SYSTEM.baseDir + "\" + $config.SYSTEM.folderDownload)

        Get-ChildItem -path $pathDL | foreach {

            $check = @(Get-ChildItem $_.FullName -Include *.chunk0,*.rar,*.mkv,*.iso,*.bin,*.img -File -Recurse)

            if ($check.Count -eq 0) {

                Remove-Item $_.FullName -Confirm:$false -force -ea Stop -Recurse
                logger ("[CleanDiretorys] "+ $_.Name +" removed")

            } else {

                logger ("[CleanDiretorys] "+ $_.Name +" Not safe to removed")
                $unsafeRemove = $unsafeRemove + $_.FullName

            }
        }
        if ($unsafeRemove.Count -ne 0) {

            $unsafeRemove | format-table BaseName,Name -AutoSize

        }
    } catch {

        $error = $_.GetType().FullName
        logger "[ERROR] $error"

    }
    logger "[CleanupEmptyDirectorys] Deleting Folders - Done"
    hostmsg ("===== CleanupEmptyDirectorys - Done")
}

Function CleanFileNames() {
    $count = 0
    $pathDone = ($config.SYSTEM.baseDir + "\" + $config.SYSTEM.folderDone)

    logger ("[CleanFileNames] Started")
    logger ("[CleanFileNames] renaming files by config.RENAME")
    hostmsg ("===== CleanFileNames - Renaming - started")

    # setup at config => rename
    [ARRAY]$long = @($config.RENAME.Values)
    [ARRAY]$short = @($config.RENAME.Keys)
    
    Get-ChildItem -path "$pathDone\*.mkv" -File  | foreach {
        logger ("[CleanFileNames] Checking "+ $_.Name)
        for ($i=0; $i -lt $config.RENAME.Count; $i++) {
            $word = $short[$i]
            $wordL = $long[$i]
            $filename = [STRING]$_.BaseName
            
            

            if ($filename -match [regex]::Escape("$word")) {

                Rename-Item $_.FullName -NewName $_.Name.Replace("$word","$wordL")
                logger ("[CleanFileNames] Renamed $word to $wordL")
                break
            } 
        
        }

    }

    logger ("[CleanFileNames] Renaming done")
    
    hostmsg ("===== CleanFileNames - Renaming - done")
    hostmsg ("===== CleanFileNames - Removing - started")
    logger ("[CleanFileNames] Removing nameParts by config.REMOVE")

    # config => remove
    $remove = @($config.REMOVE.Keys)
    
    for ($i=0; $i -lt $config.REMOVE.Count; $i++) {
        $word = $remove[$i]
        $regex = "$Word"
        Get-ChildItem -path $pathDone\* -Include *.mkv -filter "$word" -Recurse -File |
        foreach {
            
            If ($_.BaseName -match ("( |-|)"+[regex]::Escape("$regex")+"(|-| )")) {
                logger ("[CleanFileNames] Removing nameParts found "+$_.Name)
                Rename-Item $_.FullName -NewName $_.Name.Replace($Matches[0],$null)
                logger ("[CleanFileNames] Removing nameParts to "+$_.Name.Replace($Matches[0],$null))
            }
        }
    }
    logger ("[CleanFileNames] Removing nameParts done")
    hostmsg ("===== CleanFileNames - Removing - done")
}

Function CleanIdentifier() {
    logger "[CleanIdentifier] Started"
    hostmsg ("===== CleanIdentifier - started")

    $regex = "(-| )([0-9])([0-9][0-9])"
    Get-ChildItem -path $pathDone\* -Include *.mkv -Recurse -File | 
        foreach {
            If ($_.BaseName -match $regex) {

                $match = $matches[0]
                $newname = $matches[0] -replace "([0-9])([0-9][0-9])", "S0`${1}e`${2}"
                Rename-Item $_.FullName -NewName $_.Name.Replace($match,$newname)
                logger ("[CleanIdentifier] changed $match to valid episode identifier $newname")
            }
        }

    logger "[CleanIdentifier] Done"
    hostmsg ("===== CleanIdentifier - done")
}

 
$logCount = (Get-Content -Force ($config.SYSTEM.baseDir + "\" + $config.LOGGER.folder+"\$logfileDate - "+ $config.LOGGER.filename+".log")).Count

$sncount = $config.RENAME.Count
$removeCount = $config.REMOVE.Count

[ARRAY]$downloading = @(Get-ChildItem $pathDL -Filter "*.chunk0" -recurse -File)
[ARRAY]$downloading += @(Get-ChildItem $pathDL -Filter "*.rar" -recurse -File)

[ARRAY]$downloadingDone = @(Get-ChildItem $pathDone -Filter "*.mkv" -recurse -File)
# # # runner
Function runprocess {
    logger "[PROCESS] Start"

    logger "[PROCESS] Start"
    logger "[PROCESS] Stats Infos:"
    logger "[PROCESS] Stats - Series in DB: $snCount"
    logger "[PROCESS] Stats - Crap item Count: $crapCount"
    logger "[PROCESS] Stats - Events logged since last clean-up: $logCount"

    hostmsg "======================================="
    hostmsg "============     MRClean      ========="
    hostmsg "======================================="
    hostmsg "====  Move  ===  Rename  ===  Clean  =="
    hostmsg "======================================="
    hostmsg "============      Stats       ========="
    hostmsg "======================================="
    hostmsg "==== Checking $snCount Series"
    hostmsg "==== Removing $crapCount Crap-NameParts"
    hostmsg "==== events logged $logCount"
    hostmsg "======================================="
    MoveFiles

    hostmsg "======================================="
    CleanDirectorys

    hostmsg "======================================="
    CleanupEmptyDirectorys

    hostmsg "======================================="
    CleanFileNames

    hostmsg "======================================="
    CleanIdentifier


    if ($downloadingDone.count -gt 0) {
        hostmsg "======================================="
        sickbeardPostProcess
    } 
    hostmsg "======================================="
    logger "[PROCESS] Done"
}


# start runprocess function
runprocess

hostmsg "======================================="
hostmsg ("===== Downloads running: "+$downloading.count)
hostmsg ("===== Downloads done: "+$downloadingDone.count)
hostmsg "======================================="
if ($downloadingDone.count -gt 0) {
    hostmsg "Content of $pathDone"
    $downloadingDone | foreach {
        hostmsg ("===== "+$_)
    }
}
hostmsg "======================================="
endit "[PROCESS] Done"