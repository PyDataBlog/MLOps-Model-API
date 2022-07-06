@echo off
set JETTY_HOME=D:\javaapps2014\jetty-distribution-8.1.1.v20120215
set JDK_HOME=c:\Program Files\Java\jdk1.7.0_51
echo %JETTY_HOME%
echo %JDK_HOME%
"%JDK_HOME%/bin/javac" -d webapps\beer-v3\WEB-INF\classes -cp %JETTY_HOME%/lib/servlet-api-3.0.jar src/com/example/model/BeerExpert.java 
"%JDK_HOME%/bin/javac" -d webapps\beer-v3\WEB-INF\classes -cp %JETTY_HOME%/lib/servlet-api-3.0.jar;webapps\beer-v3\WEB-INF\classes src/com/example/web/BeerSelect.java 
"%JDK_HOME%/bin/jar" cvf beer-v3.war -C webapps/beer-v3/ .
copy beer-v3.war %JETTY_HOME%\webapps
pause