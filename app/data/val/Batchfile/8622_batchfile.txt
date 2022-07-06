rem
rem chrome-logging project is a series of scripts that help collect logs of
rem Google Chrome browser
rem
rem The MIT License (MIT)
rem
rem Copyright (c) 2013 Wiktor Lawski <wiktor.lawski@gmail.com>
rem
rem Permission is hereby granted, free of charge, to any person obtaining a copy
rem of this software and associated documentation files (the "Software"), to deal
rem in the Software without restriction, including without limitation the rights
rem to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
rem copies of the Software, and to permit persons to whom the Software is
rem furnished to do so, subject to the following conditions:
rem
rem The above copyright notice and this permission notice shall be included in all
rem copies or substantial portions of the Software.
rem
rem THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
rem IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
rem FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
rem AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
rem LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
rem OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
rem SOFTWARE.
rem

rem
rem Batch script ensuring collection of Google Chrome logs when issue is
rem reproducible only between application restarts. There will be two log files
rem available to trace the source of issue.
rem
rem Script prepared for Windows 7.
rem

set LOG_DIR=C:\Users\%USERNAME%\AppData\Local\Google\Chrome\User Data
set OLD_LOG=chrome_debug.log.old
set NEW_LOG=chrome_debug.log

if exist "%LOG_DIR%\%OLD_LOG%" (
	rem nothing
) else (
	type nul > "%LOG_DIR%\%OLD_LOG%"
)

xcopy "%LOG_DIR%\%NEW_LOG%" "%LOG_DIR%\%OLD_LOG%" /J /Y

"C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" --enable-logging --v=1
