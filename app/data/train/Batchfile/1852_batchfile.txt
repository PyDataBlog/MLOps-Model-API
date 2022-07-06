@echo off
setlocal enabledelayedexpansion
del result.txt

set urls=www.good.com www.bad.com www.test.com www.example.com

for %%a in (%urls%) do (
    echo %%a >> result.txt
    curl -k -I -m 3 -o NUL %%a -w %%{http_code} >> result.txt
    echo. >> result.txt
) 









