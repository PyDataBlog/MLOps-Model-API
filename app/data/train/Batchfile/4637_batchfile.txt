@echo off
del examples-dp.war
echo %JETTY_HOME%
echo %JDK_HOME%
"%JDK_HOME%/bin/jar" cvf examples-dp.war -C examples .
copy examples-dp.war %JETTY_HOME%\webapps
pause