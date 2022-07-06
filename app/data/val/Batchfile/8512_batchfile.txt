
@echo off
set jdk=C:\Program Files (x86)\Java\jdk1.7.0_45\bin
set java=C:\Program Files (x86)\Java\jre7\bin
SET CLASSPATH=.;D:\bin\Javalib\antlr-3.5.1-complete-no-st3.jar;%CLASSPATH%
@echo on

"%jdk%\javac" %*
