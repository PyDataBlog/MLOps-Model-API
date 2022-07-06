git clone https://github.com/beattyml1/Ex-NugetStuff-ProjectTemplates.git ProjectTemplates
git clone https://github.com/beattyml1/Ex-NugetStuff-TeamSettings.git TeamSettings
git clone https://github.com/beattyml1/Ex-NugetStuff-DevSettings.git DevSettings
git clone https://github.com/beattyml1/Ex-NugetStuff-BuildUtils.git src/BuildUtils
git clone https://github.com/beattyml1/Ex-NugetStuff-ThirdPartyBins.git bin/ext
gti clone https://github.com/beattyml1/Ex-NugetStuff-Common.git src/common

build-build-utils

set team =  %0

if %team% == "billing" (
  billing-setup.bat
  )
  
if %team% == "woundexpert" (
  woundexpert-setup.bat
  )
  
if %team% == "redoc" (
  redoc-setup.bat
  )
  
if %team% == "agility" (
  agility-setup.bat
  )
  
if %team% == "sheduler" (
  sheduler-setup.bat
  )
  
