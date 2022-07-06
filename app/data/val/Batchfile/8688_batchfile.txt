@echo off

rem /************************************************************/
rem /*                                                          */
rem /* Inhalt:    CCO MyConfigLocalTools batch        	        */
rem /*            						*/
rem /*                                                          */
rem /* Autor(en): Kurt Hoffmann (kh)                            */
rem /* Firma:     Fachhochschule Amberg-Weiden                  */
rem /* Stand:     05. Jun 2003                                  */
rem /*                                                          */
rem /* Historie:  05. Jun 2003  kh  erstellt			*/
rem /*                                                          */
rem /* Copyright 2002 Kurt Hoffmann, FH Amberg-Weiden           */
rem /*                                                          */
rem /************************************************************/

rem echo Starting %0 batch...
setlocal

copy /y %CCO_DRV_LETTER%:\_tools\_exe\*.lnk %CCO_DESKTOP_TARGETDIR%

rem FH_VSNetToolAddIn for Visual-Studio.NET registrieren
regedit /s W:\_tools\_exe\FH_VSNetToolsAddIn.reg

rem echo ... fertig
endlocal
rem echo Finished %0 batch