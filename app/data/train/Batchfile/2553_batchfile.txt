@echo off
start "Gradle" /B /WAIT gradlew.bat setupDevWorkspace
start "Gradle" /B /WAIT gradlew.bat eclipse
start "Gradle" /B /WAIT gradlew.bat setupDecompWorkspace --refresh-dependencies
echo All done, press any key to exit...
PAUSE >nul
exit
