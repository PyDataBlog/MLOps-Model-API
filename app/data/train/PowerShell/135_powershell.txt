$log = git log --merges --oneline --decorate --reverse 
$split = $log -split "`n"
$branchpattern = "\/(\w+)\/(\w+\-\d+)"
$tagpattern="\btag\b\:\s(\d+\.\d+\.\d+)"
$release = ""
$changelog = New-Object 'System.Collections.ArrayList'
$features = New-Object 'System.Collections.Generic.HashSet[string]'
$hotfixes = New-Object 'System.Collections.Generic.HashSet[string]'
$bugfixes = New-Object 'System.Collections.Generic.HashSet[string]'
$tags = ""
Out-File changelog.md

for($i=0; $i -le $split.Length-1; $i++)
{
  if (-not ($split[$i] -match $branchpattern))
  {
    continue;
  }
  
  switch ($matches[1])
  {
    'feature' {$features.Add($matches[2])}
    'bugfix' {$bugfixes.Add($matches[2])}
    'hotfix' {$hotfixes.Add($matches[2])}
  }

  if (-not ($split[$i] -match $tagpattern))
  {
    continue;
  }

  $tag = $matches[1] 
  $release = "# Release $tag" 
  
  if ($features.Count -gt 0) {
    
    $release += "`n Features:"
    foreach($feature in $features){
    $release += "`n     * $feature"}
  }
  if ($bugfixes.Count -gt 0) {
    $release += "`n Bugfixes:"
    foreach($bugfix in $bugfixes){
    $release += "`n     * $bugfix"}
     
  }
  if ($hotfixes.Count -gt 0) {
    $release += "`n Hotfixes:"
    foreach($hotfix in $hotfixes){
    $release += "`n     * $hotfix"}
    
   }
     
  $changelog.Add($release)
  echo $changelog >>changelog.md
  
  
  $features.Clear()
  $bugfixes.Clear()
  $hotfixes.Clear()
}





#1. split $log by lines
#2. go from bottom
#     for each line, 
#       find "/feature/", "/bugfix/", "/hotfix/"  (use list variable) 
#       extract <project name>-<number>  
#     if line has tag 
#       generate changelog for tag release
#             <tag>
#             Features: {list}  
#             Bug fixes: {list}
#             Hotfixes: {list}  
#             (if no features or not bugfixes etc, do not display the section)   