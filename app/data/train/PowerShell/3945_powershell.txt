#
# FCOP V1.0 <http://github.com/dlid/fcop>
#
#
$path = Split-Path $script:MyInvocation.MyCommand.Path
[Reflection.Assembly]::LoadFile($path + "\bin\edtFTPnet.dll")

function Install-Fcop {
    param (
        [Parameter(Mandatory=$true)]
        [string]$Config,
        [Parameter(Mandatory=$false)]
        [int]$PreviewCount = 10
    )

   
    if ((Get-Host).Version.Major -lt 3) {
        throw "This module requires PowerShell version 4.0 or higher"
    }
    

    $global:taskdepth = 0
    $global:fcop = @{
        Commands = @();
        PreviewCount = $PreviewCount;
        Start = Get-Date
    }


    Write-Host "*"
    Write-Host "* FTP Changes OK Please"
    Write-Host "* Copyight (C) 2016 David Lidström"
    Write-Host "*"
    Write-Host "*"

    $c = Initialize-FCopConfig -Config $Config
    
    New-FCopFilecache -Cfg $c
    
     Resolve-FCopChanges -Cfg $c

    # Connect to the FTP and see if we need to create folders and stuff
    Resolve-FcopFTPChanges -Cfg $c

     $commands = $c.fcop._runtime.Commands.Command
    [void]$c.Save($c.fcop._runtime.ResolvedRuntimeFilePath)



    if ($commands) {

        Clear-CommandConflicts -Cfg $c

        if ($commands.GetType().Name -eq "XmlElement") {
            $commands = @($commands)
        }


    $d = Get-Date
    $ts = New-TimeSpan $global:fcop.Start $d

    Write-FCopInfo ("Time elapsed: " + $ts)

        Write-Host ""
        Write-Host ""
        Write-Host ($commands.Length.ToString() + " FTP Commands need to be executed")
        $uploads = $commands | where {$_.Type -eq "UPLOAD"}
        if ($uploads -and $uploads.Length) {
            if (-not $uploads.Length) {$uploads = @($uploads)}

            Write-Host ($uploads.Length.ToString() + " file(s) to upload. (") -NoNewline
            Write-Host (Format-Bytes $c.fcop._runtime.Commands.BytesToUpload) -ForegroundColor Yellow -NoNewline
            Write-Host ")"

        } 
        $dirs = $commands | where {$_.Type -eq "MKD"}
        if ($dirs) {
            if (-not $dirs.Length) {$dirs = @($del)}
            Write-Host ($dirs.Length.ToString() + " folders to be created")
        }
        $del = $commands | where {$_.Type -eq "DELETE"}
        if ($del ) {
            if (-not $del.Length) {$del = @($del)}
            Write-Host ($del.Length.ToString() + " file(s) to be deleted")
        }

        Write-FCopCommandSummary -Cfg $c

        Write-Host ""
        Write-Host "  Choose an action   " -ForegroundColor Yellow -BackgroundColor Black
        Write-Host ""

        Write-Host "Action: [" -NoNewline
        Write-Host "D" -ForegroundColor Yellow -NoNewline
        Write-Host "]eploy / [S]ave updated filecache / [V]iew all commands"
    
        $key = [Console]::ReadKey($true)
    
        #Write-Host ("You choose " + $key.key) -ForegroundColor Cyan

    } else {

        
    $d = Get-Date
    $ts = New-TimeSpan $global:fcop.Start $d

    Write-FCopInfo ("Time elapsed: " + $ts)
        
        Write-Host ""
        Write-Host "*** No changes detected" -BackgroundColor Black -ForegroundColor Green
        Write-Host ""
        $key = "_"
    }

    
    $t = Start-FCopTask ("Executing Post-commands")

    foreach($c.PostCommands as $cmd) {
        Write-Host $cmd.LocalName
    }

    Complete-FCopTask $t
   

    # Here, the FTP connection has been closed. We'll need to open it again later
#    foreach(

if ($key.key -eq "D") {
    $a = 0

    Write-Host ""
    Write-Host ""
    Write-Host "Deploy" -ForegroundColor Yellow -NoNewline
    Write-Host " selected."
    Write-Host ""
    
     #[void]$c.Save("C:\git\CogFramework\Deploy\runtime.xml");

    $ftp = Connect-FCopFtp -Cfg $c
    
    $n = 0
    $taskName = ("Executing " + $c.fcop._runtime.Commands.Command.Length + " commands")
    $t = Start-FCopTask "Executing FTP commands"
    $success = $true
    foreach($Command in $c.fcop._runtime.Commands.Command ) {
        if ($n -gt 0) {
            $a = ([math]::round( $n / $c.fcop._runtime.Commands.Command.Length, 2 ) * 100)
        }
        if ($command.Type -eq "MKD") {
            if (-not $ftp.DirectoryExists($Command.Target)) {
                Write-Progress -Activity $taskName -PercentComplete $a -CurrentOperation "$a% complete" ` -Status ("Creating remote directory " + $Command.Target)
                try {
                    $ftp.CreateDirectory($Command.Target)
                } catch {
                    $success = $false
                    throw
                    break
                }
            }
        } elseif ($command.Type -eq "UPLOAD") {
            Write-Progress -Activity $taskName -PercentComplete $a -CurrentOperation "$a% complete" ` -Status ("Uploading file " + (Split-Path $Command.ResolvedTargetFullPath -Leaf))
            try {
                $ftp.UploadFile($command.Source,  $command.ResolvedTargetFullPath)
            } catch {
                $success = $false
                throw
                break
            }
        } elseif ($command.Type -eq "DELETE") {
            Write-Progress -Activity $taskName -PercentComplete $a -CurrentOperation "$a% complete" ` -Status ("Deleting file " + (Split-Path $Command.Target -Leaf))
            try {
                if (-not $ftp.DeleteFile($Command.Target)) {
                    Write-FCopInfo ("Could not delete " + $Command.Target)
                }
            } catch {
                $success = $false
                throw
                break
            }
        }
        $n++
    }

    Write-FCopInfo "Closing FTP Connection"

    $ftp.Close()




    Complete-FCopTask $t


    if ($success -eq $true) {
        $t = Start-FCopTask ("Saving updated filecache " + $c.fcop._runtime.ResolvedFilecachePath)
        $filecache = [xml]"<?xml version='1.0' encoding='utf-8'?>"
        [void]$filecache.AppendChild( $filecache.ImportNode($c.fcop._runtime.filecache, $true) ) 
        $filecache.Save($c.fcop._runtime.ResolvedFilecachePath)
        Complete-FCopTask $t
    } else {
        Write-FCopInfo "Updated filecache not saved due to deployment errors"
    }

} else {
    if ($key -and $key.key) {
        Write-Host
        Write-Host ("  Unknown command '" + $key.key + "'. Exiting.") -BackgroundColor Black -ForegroundColor Yellow
        Write-Host
    }
}


   
    
    

}

#
# Will make sure no files are deleted AND uploaded
# In that case - the DELETE's will be removed
#
function Clear-CommandConflicts {
    param(
    [Parameter(Mandatory=$true)]
    [System.Xml.XmlDocument]$Cfg)

    $f = Start-FCopTask "Cleaning up command conflicts"

    $commands = $Cfg.fcop._runtime.Commands.ChildNodes | where {$_.LocalName -eq "Command"}
    foreach($cmd in $commands) {
        if ($cmd.Type -eq "UPLOAD") {
            $cmd.SetAttribute("ResolvedTargetFullPath", (Get-FCopCommandTargetFilename -Command $cmd))
        } elseif ($cmd.Type -eq "DELETE") {
            $cmd.SetAttribute("ResolvedTargetFullPath", $cmd.Target)
        }
    }

    $deleted = 0

    foreach($cmd in $commands) {
        if ($cmd.Type -eq "UPLOAD") {
            $alsoDelete = $commands | where { $_.Type -eq "DELETE" -and $_.ResolvedTargetFullPath -eq $cmd.ResolvedTargetFullPath }
            if ($alsoDelete) {
                foreach($delChild in $alsoDelete) {
                    $delChild.ParentNode.RemoveChild($delChild)
                     $deleted++
                }                
            }
        }        
    }

    if ($deleted -gt 0) {
        Write-FCopInfo ($deleted.ToString() + " commands removed")
    }

    Complete-FCopTask $f



   

}

function Get-FCopCommandTargetFilename {
    param(
    [Parameter(Mandatory=$true)]
    [System.Xml.XmlElement]$Command)

    $filename = split-path $command.Source -Leaf
    if ($command.TargetFilename) { $filename = $command.TargetFilename }
    $target = Join-Path $Command.Target $filename
    $target = $target.Replace("\", "/")

    return $target
}

#
# Setup the runtine configuration XML that is used in the session
# 
function Write-FCopCommandSummary{
    param(
    [Parameter(Mandatory=$true)]
    [System.Xml.XmlDocument]$Cfg)

    if ($Cfg.fcop._runtime.Commands.HasChildNodes -and $global:fcop.PreviewCount -gt 0) {

        $n = $Cfg.fcop._runtime.Commands.ChildNodes.Count
        if ($n -gt $global:fcop.PreviewCount) { $n = $global:fcop.PreviewCount }
         
        if ($Cfg.fcop._runtime.Commands.ChildNodes.Count -gt $global:fcop.PreviewCount) {
            Write-Host ("Preview of first "+$n+" command(s) ====== ") -ForegroundColor Cyan
        } else {
            Write-Host ("Preview of all command(s) ====== ") -ForegroundColor Cyan
        }
        Write-Host
        $cmdToShow = $Cfg.fcop._runtime.Commands.Command | select -First $global:fcop.PreviewCount
        foreach( $cmd in $cmdToShow) {
            $CommandName = ""
            $CmdColor = "Black"
            switch($cmd.Type) {
                "UPLOAD" {$CommandName = "PUT"; $CmdColor = "Green"}
                "DELETE" {$CommandName = "DELETE"; $CmdColor = "Red"}
                "MKD" {$CommandName = "MMDIR"; $CmdColor = "Green"}
            }

            Write-Host (" " + $CommandName + " " ) -ForegroundColor $CmdColor -NoNewline

            switch($cmd.Type) {
                "UPLOAD" {
                   Write-Host $cmd.Source"" -NoNewline
                   Write-Host $cmd.ResolvedTargetFullPath -ForegroundColor Green
                }
                "DELETE" {
                    Write-Host $cmd.Target -ForegroundColor Gray
                }
                "MKD" {
                    Write-Host $cmd.Target -ForegroundColor Gray
                }
            }
        }

        if ($Cfg.fcop._runtime.Commands.Command.Length -gt $global:fcop.PreviewCount) {
            Write-Host (" and " + ($Cfg.fcop._runtime.Commands.Command.Length-$global:fcop.PreviewCount).ToString() + " more...") -ForegroundColor Gray
        }

    }



}
#
# Setup the runtine configuration XML that is used in the session
# 
function Initialize-FCopConfig {
    param(
    [Parameter(Mandatory=$true)]
    [string]$Config)

    $resolvedPath = Resolve-path $Config
    $configfolder = (Split-Path $resolvedPath -Parent)
    
    $cc = Get-Content $resolvedPath
    if (-not $cc) {
        throw "Config content was empty"
    }

    [xml]$xmlCfg = $cc

    $sourceFolder = ($xmlCfg.fcop.Source | where { $_.Type -eq "File" }).InnerText
    $target = ($xmlCfg.fcop.Target | where { $_.Type -eq "FTP" })
    $targetFolder = $target.Path
    $theRoot = Resolve-Path (Join-Path $configfolder $xmlCfg.fcop.Source.InnerText)

    # Write-Host $theRoot -ForegroundColor Cyan

    if (-not $targetFolder.EndsWith("/")) {
        $targetFolder += "/"
    }


   # Write-Host -ForegroundColor Cyan $sourceFolder
    if ($sourceFolder) {
        $sourceFolder = Resolve-Path $sourceFolder
    }



   # Write-host $theRoot -ForegroundColor red
   # Read-Host
 
    $runtimeElement = $xmlCfg.CreateElement("_runtime")
    [void]$xmlCfg.DocumentElement.AppendChild($runtimeElement)

    $elm1 = $xmlCfg.CreateElement("ResolvedSourceFolder")
    [void]$elm1.AppendChild( $xmlCfg.CreateTextNode($theRoot))
    [void]$runtimeElement.AppendChild( $elm1 )

    $elm2 = $xmlCfg.CreateElement("ResolvedTargetFolder")
    [void]$elm2.AppendChild( $xmlCfg.CreateTextNode($targetFolder))
    [void]$runtimeElement.AppendChild( $elm2 )

    $elm = $xmlCfg.CreateElement("ResolvedConfigurationFilePath")
    [void]$elm.AppendChild( $xmlCfg.CreateTextNode($resolvedPath))
    [void]$runtimeElement.AppendChild( $elm )


    $filename = [io.path]::GetFileNameWithoutExtension($resolvedPath)
    $filecache_path = (Join-Path $configfolder ($filename  + ".fcopcache"))
    $runtime_path = (Join-Path $configfolder ($filename  + ".runtime.xml"))

    $elm = $xmlCfg.CreateElement("ResolvedFilecachePath")
    [void]$elm.AppendChild( $xmlCfg.CreateTextNode($filecache_path))
    [void]$runtimeElement.AppendChild( $elm )

    $elm = $xmlCfg.CreateElement("ResolvedRuntimeFilePath")
    [void]$elm.AppendChild( $xmlCfg.CreateTextNode($runtime_path))
    [void]$runtimeElement.AppendChild( $elm )

    $target = ($fcogcfg.fcop.Source | where { $_.Type -eq "FTP" })
    $targetFolder = $target.Path

    Set-Variable -Name "FcopCfg" -Scope Global -Value $xmlCfg


    $d = get-content $filecache_path -ErrorAction SilentlyContinue
    if ($d) {
        $cached = [xml]$d
        $cachedNode = $xmlCfg.CreateElement("cached")
        [void]$cachedNode.AppendChild($xmlCfg.ImportNode( $cached.DocumentElement, $true ))
        [void]$xmlCfg.fcop._runtime.AppendChild($cachedNode)
    }

    #Write-Host "Oh, got cached content" -ForegroundColor Yellow -BackgroundColor Black
    #Write-Host $d
   
    #   Read-Host "wait for it"

    return [System.Xml.XmlDocument]$xmlCfg
}

#
# 
#
function Resolve-FCopFTPChanges {
    param(
    [Parameter(Mandatory=$true)]
    [System.Xml.XmlDocument]$Cfg
    )

    $fnTask = Start-FCopTask "Checking changes for FTP server"
    $ftp = Connect-FCopFtp -Cfg $Cfg

    # Get all target directories
    $cmds = $Cfg.fcop._runtime.Commands.Command | where {$_.Type -eq "UPLOAD"}

    $uniqueTargetFolders = @($Cfg.fcop.Target.Path)

    #$f = Start-FCopTask "Enumerating all folders"
    foreach($cmd in $cmds) {
        if (-not $uniqueTargetFolders.Contains($cmd.Target)) {
            $d = $cmd.Target.Split("/")
            for($i =0; $i -lt $d.Length; $i++) {
                $sofar = $d[0..$i]
                $path = $sofar -join("/")
                if (-not $uniqueTargetFolders.Contains($path)) {
                    $uniqueTargetFolders += $path
                }
            }
        }
    }
   # Complete-FCopTask $f

    $uniqueTargetFolders = $uniqueTargetFolders | Sort-Object

#    Write-FCopInfo ($uniqueTargetFolders.Count.ToString() + " unique folders should be checked on FTP server")
    $mdirs = @()
   
    $f = Start-FCopTask ("Checking " + $uniqueTargetFolders.Count.ToString() + " folders on FTP server")
    do {
        foreach($folder in $uniqueTargetFolders ) {
            if (-not $ftp.DirectoryExists($folder)) {
                $others = $uniqueTargetFolders | where { $_.StartsWith($folder) }
                if ($others.GetType().Name -eq "String") { $others = @($others) }
                Write-FCopInfo ("MKD '" + $folder + "' (" + ($others.Length -1).ToString() + " subfolders)") 
                $mdirs+=$others
                $x = $uniqueTargetFolders | where { -not $_.StartsWith($folder) }
                $uniqueTargetFolders = $x
                break
            } else {
                $x = $uniqueTargetFolders | where { $_ -ne $folder }
                $uniqueTargetFolders = $x 
                break
            }
        }

    } while ($uniqueTargetFolders.Length -gt 0)
    
    if  ($mdirs.Count -gt 0) {

        $f2 = Start-FCopTask ("Distributing " + $mdirs.Count + " MKDIR commands before PUT commands")
        $mdirs = $mdirs | Sort-Object
        $allFoldersNode = $Cfg.CreateElement("AllFolders")
        [void]$Cfg.fcop._runtime.AppendChild($allFoldersNode)

        foreach($folderToCreate in $mdirs){

           $n = $Cfg.CreateElement("Makedir")
           [void]$n.AppendChild( $Cfg.CreateTextNode($folderToCreate) )
           [void]$allFoldersNode.AppendChild($n)

            # Find the first UPLOAD command for this folder
            $x = $Cfg.fcop._runtime.Commands.Command | where { $_.Type -eq "UPLOAD" -and ($_.Target.StartsWith($folderToCreate))}
            if ($x -ne $null -and $x.GetType().Name -eq "Object[]") {
                $x = $x[0]
            } elseif ( $x.GetType().Name -ne "XmlElement") {
                $x = $null
            }

            if ($x) {
                 $e = $Cfg.CreateElement("Command")
                 [void]$e.SetAttribute("Type", "MKD")
                 [void]$e.SetAttribute("Target", $folderToCreate)
                 [void]$x.ParentNode.InsertBefore($e, $x)
            }
        }

        Complete-FCopTask $f2
    } else {
        Write-FCopInfo "All remote folders exist"
    }
    Complete-FCopTask $f

    if ($ftp) {
        Write-FCopInfo "Closing FTP Connection"
        $ftp.Close()
    }

    Complete-FCopTask  $fnTask

}

function Get-FCopFTPPassword {
    param(
    [Parameter(Mandatory=$true)]
    [System.Xml.XmlDocument]$Cfg,
    [switch]$Force
    )
    $password = $null
    $ConfigHash = ($Cfg.fcop._runtime.ResolvedConfigurationFilePath).GetHashCode().ToString() + ";" + $Cfg.fcop.Target.Username.GetHashCode().ToString()

    
    if (-not $Force) {
    
        $regitem = Get-ItemProperty -Path "HKCU:\Software\dlid.se\fcop" -ErrorAction SilentlyContinue
        if ($regitem) {
            $SecurePassword = $regitem.$ConfigHash
            if ($SecurePassword) {
               # Write-Host "Found secure password "$SecurePassword
                $password = $SecurePassword | ConvertTo-SecureString
               # Write-Host $password.GetType()
            }
        }
    }

    if (-not $password ) {
        #    $SecurePassword = (Get-ItemProperty -Path "HKCU:\Software\dlid.se\fcop").$ConfigHash | ConvertTo-SecureString
        #Write-Output "" -ForegroundColor Yellow
        #Write-Output "FTP Password not found for config file" -ForegroundColor Yellow
        #Write-Output "====================" -ForegroundColor Yellow
        #Write-Output
        #Write-Output "Password will be stored encrypted using your Windows Account in the System Registry" -ForegroundColor Gray
        #Write-Output
        $password = Read-Host ("Enter FTP Password (" + $Cfg.fcop.Target.Username + "@" + $Cfg.fcop.Target.Host +")") -AsSecureString 
        if ($password) {
            $secureStringPasswordText = $password | ConvertFrom-SecureString
            New-Item -Path "HKCU:\Software\dlid.se" -ErrorAction SilentlyContinue
            New-Item -Path "HKCU:\Software\dlid.se\fcop" -ErrorAction SilentlyContinue
            New-ItemProperty -Path "HKCU:\Software\dlid.se\fcop" -Name $ConfigHash -Value $secureStringPasswordText -Force
         #   Write-Output $password
         #   Write-Output $secureStringPasswordText | ConvertTo-SecureString
            

        } else {
            throw "No password provided. Exiting."
        }
    } else {
       # Write-FCopInfo ("Vi har password?" + $password)
    }

    $user = "DUMMY\user"
   # Write-Host ("usertype " + $user.GetType()) -ForegroundColor Red
    #Write-Host ("pwdtype " + $password.getType()) -ForegroundColor Red
    try {
        $Credentials = New-Object System.Management.Automation.PSCredential -ArgumentList $user, $password
    } catch {
        Write-Host "* " -ForegroundColor red -BackgroundColor Black
        Write-Host "* Error reading password" -ForegroundColor red -BackgroundColor Black
        Write-Host "* " -ForegroundColor red -BackgroundColor Black
        throw
    }
    return $Credentials.GetNetworkCredential().Password

}

function Connect-FCopFtp {
    param(
    [Parameter(Mandatory=$true)]
    [System.Xml.XmlDocument]$Cfg
    )

    # Password is preferably not saved in the config file
    # Attempt to get it

    [EnterpriseDT.Net.Ftp.FTPConnection]$ftp = New-Object EnterpriseDT.Net.Ftp.FTPConnection

    $ftpConnected = $true
    $password = Get-FCopFTPPassword -Cfg $Cfg
    do {
        $ftpConnected = $true
        $ftp.ServerAddress = $Cfg.fcop.Target.Host
        $ftp.UserName = $Cfg.fcop.Target.Username

        if ($password.GetType().Name -eq "Object[]" -and $password.Length -gt 0) {
            $password = $password[$password.Length-1]
        }

        if ($password.GetType().Name -ne "String") {
            throw ("Password is of wrong type. Expected String, found " + $password.GetType().Name)
        }

        $ftp.Password = $password
      #  Write-FCopInfo ("Username " + $ftp.UserName)
      #  Write-FCopInfo ("Pass " + $password.GetType())
       # Write-FCopInfo ("Host" + $ftp.ServerAddress)
       # Write-FCopInfo ("Port" + $ftp.ServerPort)

        try {
            $t = Start-FCopTask "Opening FTP Connection"
            $ftp.Connect()
            Complete-FCopTask $t
            return $ftp
        } catch {
            try { $ftp.Close() } catch {}  
            $Message = $_.Exception.Message
            $ftpConnected = $false
            Write-FCopInfo $Message
            Stop-FCopTask $t

            if ($Message.ToLower().Contains("(code=530)")) {
                # Authentication failed
                $password = Get-FCopFTPPassword -Cfg $Cfg -Force
            } else {
                Write-Host "*" -ForegroundColor Red -BackgroundColor Black
                Write-Host "* Connection to FTP server failed" -ForegroundColor Red -BackgroundColor Black
                Write-Host "*" -ForegroundColor Red -BackgroundColor Black
                throw
            }
        }

    } while (-not $ftpConnected)
    return $null
}


function New-FCopUploadCommandElement {
     param(
    [Parameter(Mandatory=$true)]
    [System.Xml.XmlDocument]$Cfg,
    [Parameter(Mandatory=$true)]
    [object]$file,
    [Parameter(Mandatory=$true)]
    [object]$folder,
    [Parameter(Mandatory=$true)]
    [string]$reason,
    [Parameter(Mandatory=$true)]
    [ValidateSet('UPLOAD','DELETE')]
    [string]$type
    )
    
    $i = $file.SourcePath.LastIndexOf("\")
    
    if ($i -gt -1) {
        $SourceFolderPathOnly = $file.SourcePath.Substring(0, $i)
    } else {
        $SourceFolderPathOnly = ""
    }

    $folderTargetPath = $folder["fcop://TargetPath"]
    $folderSourcePath = $folder["fcop://SourcePath"]

    if ($folderSourcePath -eq ".") {
        $folderSourcePath = ""
    }

    if ($folderTargetPath -eq ".") {
        $folderTargetPath = $null
    }

    $cmdNode = $Cfg.CreateElement("Command")
    $cmdNode.SetAttribute("Type", $type)

   #$Cfg.fcop._runtime

    $cmdNode.SetAttribute("Source", (Join-Path (Join-Path $Cfg.fcop._runtime.ResolvedSourceFolder $folderSourcePath) $file.SourcePath)   )
                 
    $finalTargetFolder = $Cfg.fcop._runtime.ResolvedTargetFolder 
    if ($SourceFolderPathOnly) {
        if ($folderTargetPath) {
            $tp = (Join-Path $folderTargetPath $SourceFolderPathOnly)
        } else {
            $tp = $SourceFolderPathOnly
        }



     $finalTargetFolder = Join-Path $finalTargetFolder $tp
    } else {
       # Write-Host $finalTargetFolder -ForegroundColor green
       # write-host $folderTargetPath -ForegroundColor red
     $finalTargetFolder = Join-Path $finalTargetFolder $folderTargetPath
    }

    

    if ($type -eq "DELETE") {
        $filename = Split-Path $cmdNode.GetAttribute("Source") -Leaf
        $fullTarget = Join-Path $finalTargetFolder $filename
        $finalTargetFolder = $fullTarget
        $cmdNode.RemoveAttribute("Source")
    }

    $finalTargetFolder = $finalTargetFolder.Replace("\", "/")
    $finalTargetFolder = $finalTargetFolder -creplace "/{2,}", "/"

    $cmdNode.SetAttribute("Target", $finalTargetFolder)
    if ($file.TargetFilename) {
        $cmdNode.SetAttribute("TargetFilename", $file.TargetFilename)
    }
    $cmdNode.SetAttribute("Reason", $reason)
    return $cmdNode
}

function Convert-FCopFilecacheXmlToHashtable {
    param(
    [Parameter(Mandatory=$true)]
    [System.Xml.XmlNode]$filecache
    )

 #   $fnTask = Start-FCopTask "Converting XML to objects"
    $folderHashtable = @{}
    foreach($folder in $filecache.ChildNodes) {
        $filesHashtable = @{
            "fcop://TargetPath" = $folder.TargetPath;
            "fcop://sourcePath" = $folder.SourcePath;
        }
        foreach($file in $folder.ChildNodes) {

            $fileItem = @{
                Bytes = $file.Bytes;
                Hash = $file.Hash;
                SourcePath = $file.SourcePath;
                TargetFilename = $file.TargetFilename
            }    
            $filesHashtable.Add($file.SourcePath, $fileItem)
        }
        $folderHashtable.Add($folder.SourcePath, $filesHashtable)
    }
#    Complete-FCopTask $fnTask
    return $folderHashtable

}

function Resolve-FCopChanges {
    param(
    [Parameter(Mandatory=$true)]
    [System.Xml.XmlDocument]$Cfg
    )

    $fnTask = Start-FCopTask "Resolving changes"

    $cached = $Cfg.fcop._runtime.cached.filecache

    if (-not $cached) {
        Write-FCopInfo "Ok, no previous cache. Create empty element then"
        $cached = $Cfg.CreateElement("cached")
        [void]$Cfg.fcop._runtime.appendChild($cached)
    } 
    $commands = $Cfg.CreateElement("Commands")

    $addCommands = @()
    $bytesToUpload = 0
    
    $folders = Convert-FCopFilecacheXmlToHashtable -filecache $Cfg.fcop._runtime.filecache
    if ($cached) {
        $cached = Convert-FCopFilecacheXmlToHashtable -filecache $cached
    }

    foreach($folderPath in $folders.Keys) {
        $folder = $folders[$folderPath]
        if ($cached.ContainsKey($folderPath)) {
            $f2 = Start-FCopTask ("Checking changes in '" +$folderPath + "'")
            $cachedFolder = $cached[$folderPath]

            foreach($filePath in $folders[$folderPath].Keys) {
                $file = $folders[$folderPath][$filePath]
                if ($file.GetType().Name -eq "String") { continue }
                if ($cachedFolder.ContainsKey($filePath)) {
                    $cachedFile = $cachedFolder[$filePath]
                    if ($cachedFile.Hash -ne $file.Hash) {
                        $bytesToUpload += $file.Bytes
                        $cmdNode = New-FCopUploadCommandElement -Cfg $Cfg -file $file -folder $folder -reason "Changed" -type UPLOAD
                        [void]$commands.appendChild($cmdNode)
                    }
                } else {
                    $bytesToUpload += $file.Bytes
                    $cmdNode = New-FCopUploadCommandElement -Cfg $Cfg -file $file -folder $folder -reason "New" -type UPLOAD
                    [void]$commands.appendChild($cmdNode)
                }
            }
            
            foreach($filePath in $cachedFolder.keys) {
               $file = $cachedFolder[$filePath]
               if ($file.GetType().Name -eq "String") { continue }
               $existingfile = $folder.ContainsKey($filePath)
               if (-not -$existingfile) {
                    $cmdNode = New-FCopUploadCommandElement -Cfg $Cfg -file $file -folder $folder -reason "Deleted" -type "DELETE"
                    [void]$commands.appendChild($cmdNode)
               }
            }
            Complete-FCopTask $f2 
        } else {
            foreach($filePath in $folders[$folderPath].Keys) {
                    $file = $folders[$folderPath][$filePath]
                    if ($file.GetType().Name -eq "String") { continue }
                    $bytesToUpload += $file.Bytes
                    $cmdNode = New-FCopUploadCommandElement -Cfg $Cfg -file $file -folder $folder -reason "New" -type UPLOAD
                    [void]$commands.appendChild($cmdNode)
            }
        }

    }

    $commands.SetAttribute("BytesToUpload", $bytesToUpload)

    [void]$Cfg.fcop._runtime.appendChild($commands)
    Complete-FCopTask $fnTask
}

#
# Create a new filecache and put it in runtime config file
#
function New-FCopFilecache {
    param(
    [Parameter(Mandatory=$true)]
    [System.Xml.XmlDocument]$Cfg
    )
    

    $fnTask = Start-FCopTask "Creating up-to-date filecache"

    $filecacheRoot = $Cfg.CreateElement("filecache")
    $now = get-date 
    $now = $now.ToUniversalTime();
    $filecacheRoot.SetAttribute("created", $now.ToString("yyyy-MM-ddTHH:mm:ssZ")) 

    [void]$Cfg.fcop._runtime.AppendChild($filecacheRoot)

    $filecount = 0
    $totalbytes = 0

    foreach($cmd in $Cfg.fcop.Commands.ChildNodes) {
        if ($cmd.LocalName -eq "Copy") {
            $sourceFolder = $Cfg.fcop._runtime.ResolvedSourceFolder
            $localpath = (join-path $sourceFolder $cmd.SourcePath)

            if ($cmd.SourcePath -eq ".") {
                $localpath = $sourceFolder
                if ($sourceFolder.Substring(0,1) -eq "\") {
                    $localpath = $sourceFolder.Substring(1)
                }
                if (-not $localpath.EndsWith("\")) {
                    $localpath += "\"
                }
            }

            $files = Get-ChildItem  -Path $localpath -Recurse -ErrorAction SilentlyContinue -File
            [System.XML.XMLElement]$folderNode=$filecacheRoot.appendChild($Cfg.CreateElement("folder"))
            
            if (-not $cmd.TargetPath -or $cmd.TargetPath -eq ".") {
                $targetPath = $cmd.SourcePath;
            } else {
                $targetPath = $cmd.TargetPath
            }
            $folderNode.SetAttribute("SourcePath", $cmd.SourcePath);
            $folderNode.SetAttribute("TargetPath", $targetPath);
            $folderNode.SetAttribute("ResolvedSourcePath", $localpath)
            $n = 0
            $ignoredFiles = 0
            Write-FCopInfo ("Processing " + $localpath)

            foreach(  $fileo in $files) {
                $isIgnored = $false
                [System.IO.FileInfo]$file = $fileo
                #$folderToMatch = $file.DirectoryName.Substring($localpath.Length)
                #write-host  $localpath -ForegroundColor green
                $sourcePath = $file.FullName.Substring($localpath.Length)
             #   Write-Host $file.FullName -ForegroundColor cyan
             #   write-host $localpath -ForegroundColor blue
              #  Write-Host $cmd.ChildNodes.Length
                #Write-Host $sourcePath
                $rename = $false
                if ($cmd.HasChildNodes) {
                    foreach($ignored in $cmd.ChildNodes) {
                        if ($ignored.LocalName -eq "Ignore") {

                            if ($sourcePath -match $ignored.InnerText) {
                                $ignoredFiles ++
                                $isIgnored = $true
                            }
                        } elseif ($ignored.LocalName -eq "Rename") {
                            if ($sourcePath -match $ignored.SourcePath) {
                                $rename = $ignored.NewName
                                break
                            }
                        }
                    }
                }

                if($isIgnored) { continue }
                $filecount++

                $fileHash = Get-TcopFileHash -File $file.FullName
                $fileNode = [System.XML.XMLElement]$Cfg.CreateElement("file")
                $fileNode.SetAttribute("SourcePath", $sourcePath )
                $fileNode.SetAttribute("Bytes", $file.Length)
                $fileNode.SetAttribute("Hash", $fileHash)

                if ($rename) {
                    $fileNode.SetAttribute("TargetFilename", $rename)
                }

                $totalbytes += $file.Length
                #$fileNode.SetAttribute("Folder", $file.)
                [void]$folderNode.appendChild($fileNode)

               # [void]$folderNode.appendChild($oXMLSystem)
            }

            $folderNode.SetAttribute("IgnoredFiles", $ignoredFiles)
        }
    }

    Write-FCopInfo ("= " + $filecount + " file(s). " + (Format-Bytes $totalbytes))
    Complete-FCopTask $fnTask


}

function Get-TcopFileHash {
     param(
    [Parameter(Mandatory=$true)]
    [string]$File
    )
    $algorithm = [System.Security.Cryptography.HashAlgorithm]::Create("SHA256")
    $stream = New-Object System.IO.FileStream($File, [System.IO.FileMode]::Open, [System.IO.FileAccess]::Read)
    $md5StringBuilder = New-Object System.Text.StringBuilder
    $algorithm.ComputeHash($stream) | % { [void] $md5StringBuilder.Append($_.ToString("x2")) }
    $hash = $md5StringBuilder.ToString()
    $stream.Dispose()
    return $hash.ToUpper()
}


Function Format-Bytes() {
[cmdletbinding()]
Param ([long]$Type)
If ($Type -ge 1TB) {[string]::Format("{0:0.00} TB", $Type / 1TB)}
ElseIf ($Type -ge 1GB) {[string]::Format("{0:0.00} GB", $Type / 1GB)}
ElseIf ($Type -ge 1MB) {[string]::Format("{0:0.00} MB", $Type / 1MB)}
ElseIf ($Type -ge 1KB) {[string]::Format("{0:0.00} KB", $Type / 1KB)}
ElseIf ($Type -gt 0) {[string]::Format("{0:0.00} Bytes", $Type)}
Else {""}
}

function Start-FCopTask {
     param(
    [Parameter(Mandatory=$true)]
    [string]$Title
    )
    $callstack = Get-PSCallStack

    if (-not $global:taskdepth) { $global:taskdepth = 0}
    $indent = " " * $global:taskdepth
    $global:taskdepth ++
    $actionText = Get-FcopActionPaddedString "PROCESSING"
    Write-Host ($actionText + $indent) -ForegroundColor Yellow -NoNewline
    Write-Host $Title -ForegroundColor White -NoNewline
    Write-Host (" " + $callstack[1].FunctionName + " " + $callstack[1].Location) -ForegroundColor DarkGray
    
    return @{
        Title = $Title;
        StartTime = Get-Date;
        Function = $callstack[1].FunctionName
    }


}

# Fail a task
function Stop-FCopTask {
     param(
    [Parameter(Mandatory=$true)]
    [object]$Token
    )
    $global:taskdepth --
    $now = Get-Date
    $duration = New-TimeSpan -Start $Token.StartTime -End $now
    $indent = " " * $global:taskdepth

    $actionText = Get-FcopActionPaddedString "FAIL"

    Write-Host ($actionText + $indent) -ForegroundColor Red -NoNewline
    Write-Host $Token.Title -ForegroundColor White -NoNewline
    Write-Host " (" -NoNewline
    Write-Host $duration.ToString() -ForegroundColor DarkGray -NoNewline
    Write-Host ")"


}

function Get-FcopActionPaddedString {
    param(
    [Parameter(Mandatory=$true)]
    [string]$Text
    )
    return $Text.PadRight(14, " ")
}

function Write-FCopInfo {
     param(
    [Parameter(Mandatory=$true)]
    [string]$Text
    )

    $indent = " " * $global:taskdepth
    $actionText = Get-FcopActionPaddedString "INFORMATION"

    Write-Host ($actionText + $indent) -ForegroundColor Gray -NoNewline
    Write-Host $Text -ForegroundColor White


}


function Complete-FCopTask {
     param(
    [Parameter(Mandatory=$true)]
    [object]$Token
    )
    $global:taskdepth --
    $now = Get-Date
    $duration = New-TimeSpan -Start $Token.StartTime -End $now
    $indent = " " * $global:taskdepth
    $actionText = Get-FcopActionPaddedString "COMPLETED"

    Write-Host ($actionText + $indent) -ForegroundColor Green -NoNewline
    Write-Host $Token.Title -ForegroundColor White -NoNewline
    Write-Host " (" -NoNewline
    Write-Host $duration.ToString() -ForegroundColor DarkGray -NoNewline
    Write-Host ")"

}

function Get-ScriptDirectory
{
  $Invocation = (Get-Variable MyInvocation -Scope 1).Value
  Split-Path $Invocation.MyCommand.Path
}


Export-ModuleMember "Install-Fcop"





   # $filecachexml = [io.path]::GetFileNameWithoutExtension($resolvedPath)
   # $filecachexml += ".filecache.xml"

   # $filecachezip = [io.path]::GetFileNameWithoutExtension($resolvedPath)
   # $filecachezip += ".filecache.zip"

   # $p = Split-Path $resolvedPath -Parent
   # $filecachexml = Join-Path $p  $filecachexml 
   # $filecachezip = Join-Path $p  $filecachezip 



   # Write-Host "Saving filecache " -NoNewline -ForegroundColor Yellow
   # Write-Host "..." -NoNewline
   # $xmlDocument.Save($filecachexml)
  #  Write-Host "OK" -ForegroundColor Green
  #  Write-Host "Compressing " -NoNewline -ForegroundColor Yellow
  #  Write-Host "..." -NoNewline
   # Compress-Archive -LiteralPath $filecachexml -CompressionLevel Optimal -DestinationPath $filecachezip -Force
  #  Write-Host "OK" -ForegroundColor Green



#    $SecurePasswordText = 'UWvDNdnrWMMVDv6y' | ConvertTo-SecureString -AsPlainText -Force | ConvertFrom-SecureString
#   New-Item -Name $pathHash -Path HKCU:\Software\Fcop -Value $SecurePasswordText

#    $ftpConnection = New-Object EnterpriseDT.Net.Ftp.FTPConnection

#   $ftpConnection.ServerAddress = "ftpcluster.loopia.se"
#   $ftpConnection.UserName = "davidlidstrom.com"
#   $ftpConnection.Password = "UWvDNdnrWMMVDv6y"
#   Write-Host $ftpConnection.serveraddress -ForegroundColor Red
#  $ftpConnection.Connect();