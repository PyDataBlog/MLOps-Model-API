::Executes Tests using the Testify Framework

@ECHO OFF
set DIRNAME=%~dp0%
set TESTIFY_HOME=%DIRNAME%..
set JAVA=%JAVA_HOME%\bin\java
SET TEST_DIR=
SET RESULT_DIR=
SET CONFIG_FILE=
SET LOG_LEVEL=

:Loop
IF [%1]==[] GOTO :Continue
IF "%1"=="-t" GOTO :setTestDir
IF "%1"=="--testDir" GOTO :setTestDir
IF "%1"=="-r" GOTO :setResultDir
IF "%1"=="--resultDir" GOTO :setResultDir
IF "%1"=="-c" GOTO :setConfigFile
IF "%1"=="--config" GOTO :setConfigFile
IF "%1"=="-l" GOTO :setLogLevel
IF "%1"=="--log" GOTO :setLogLevel
IF "%1"=="-h" GOTO :usage
IF "%1"=="--help" GOTO :usage

:setTestDir
SHIFT
SET TEST_DIR=%1
SHIFT
GOTO :LOOP

:setResultDir
SHIFT
SET RESULT_DIR=%1
SHIFT
GOTO :LOOP

:setConfigFile
SHIFT
SET CONFIG_FILE=%1
SHIFT
GOTO :LOOP

:setLogLevel
SHIFT
SET LOG_LEVEL=%1
SHIFT
GOTO :LOOP

:Continue

IF "%TEST_DIR%"=="" (
        ECHO.
        ECHO Test Directory not specified, assuming current directory
        SET TEST_DIR=%CD%
)

IF "%RESULT_DIR%"=="" (
        ECHO.
        ECHO Results Directory not specified, results will be created CurrentDirectory/testify
        SET RESULT_DIR=%CD%/testify
)

IF "%CONFIG_FILE%"=="" (
        ECHO.
        ECHO Config File not specified, system will look for .properties files in the test directory
        SET CONFIG_FILE=%TEST_DIR%
)

IF "%LOG_LEVEL%"=="" (
        ECHO.
        ECHO Log level not specified, system will set log level to INFO
        SET LOG_LEVEL="INFO"
)

ECHO.
%JAVA% -jar %TESTIFY_HOME%/lib/testify-core-jar-with-dependencies.jar %TEST_DIR% %RESULT_DIR% %CONFIG_FILE% %LOG_LEVEL%
GOTO :END

:usage
ECHO.
ECHO Usage: Executes Tests with the Testify Framework [[-t] [-r] [-c] [-l] [-h]]
ECHO -t OR --testDir     Location of the test bundle (Defaults to current directory)
ECHO -r OR --resultDir   Location of result output (Defaults to CurrentDirectory/testify)
ECHO -c OR --config      Location of configuration file or configuration directory (Defaults to test directory)
ECHO -l OR --log         Testify Logging level (Defaults to INFO)
ECHO -h OR --help	 Displays this message
ECHO.

:END
