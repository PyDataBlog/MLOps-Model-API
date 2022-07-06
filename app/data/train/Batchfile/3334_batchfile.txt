svn status | findstr /R "^!" > missing.list
for /F "tokens=2 delims= " %%A in (missing.list) do (svn delete %%A)