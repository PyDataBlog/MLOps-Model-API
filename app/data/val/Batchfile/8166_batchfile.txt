@echo off
@setlocal
@rem ----------------------------------------------------------------------------
@rem
@set TITLE=Cleanup temporary files, downloads and batch invocation of cleanmgr
@rem
@rem Sample for batch run
@rem    cleanup.bat -b 
@rem Sample for interactive run
@rem    cleanup.bat
@rem Optional setup possible by invoking 
@rem    cleanmgr /SAGESET:90
@rem ----------------------------------------------------------------------------
@rem
@title %TITLE%
@rem
@color fc
@echo %TITLE%
@set PARAMETER=%*
@rem
@if NOT "%PARAMETER%" == "-b" (
  @echo Hit CTRL-C to cancel cleanup . . .
  pause
)
@erase /q /s "C:\Users\%USERNAME%\Downloads\*.*" 
@erase /q /s "C:\Users\%USERNAME%\AppData\Local\Temp\*.*"
@rem
@cleanmgr /SAGERUN:90
@rem
@if NOT "%PARAMETER%" == "-b" (
  @pause
)
@endlocal
@goto :eof
