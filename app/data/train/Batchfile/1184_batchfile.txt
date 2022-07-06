@echo off
cd %~dp0
echo 127.0.0.1:5432:dbSGC:postgres:postgres> %userprofile%\AppData\Roaming\postgresql\pgpass.conf
psql.exe --username="postgres" --host=127.0.0.1 --port=5432 postgres < createDB.sql
pg_restore.exe --username="postgres" --host=127.0.0.1 --port=5432 -d dbSGC < %~dp0\dbSGC.backup
pause