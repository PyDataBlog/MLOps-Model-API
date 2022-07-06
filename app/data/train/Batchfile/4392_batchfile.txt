SET str1="%SSATROM%"
SET SATREG=4
if not x%str1:(J)=%==x%str1% goto :1
if not x%str1:(JAP)=%==x%str1% goto :1
if not x%str1:(JAPAN)=%==x%str1% goto :1
if not x%str1:(E)=%==x%str1% goto :c
if not x%str1:(EU)=%==x%str1% goto :c
if not x%str1:(EUR)=%==x%str1% goto :c
if not x%str1:(EUROPE)=%==x%str1% goto :c
if not x%str1:(U)=%==x%str1% goto :US
if not x%str1:(US)=%==x%str1% goto :US
if not x%str1:(USA)=%==x%str1% goto :US


:US	 
set SATREG=4
goto :inject

:1
set SATREG=1
goto :inject

:c
set SATREG=c
goto :inject

:inject
pushd "[ROMPTH]"
"%GBC%\inifile.exe" "%GBC%\net\%EXECT%\%SSAT%\%SSF%.ini" [Other] ROMFolder="[ROMPTH]"
"%GBC%\fart.exe" "%GBC%\net\%EXECT%\%SSAT%\%SSF%.ini" [REG] "%SATREG%"
popd