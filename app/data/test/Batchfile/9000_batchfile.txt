@echo off








:: Clique com o botão direito e salvar como nettest.bat



























ECHO.*********************************************************************
echo.* Executando testes     ...                                         *
echo.*                                                                   *
echo.* Os testes sao continuos. Caso queira terminar, feche esta janela. *
echo.* E gerado um arquivo de nome:                                      *
echo.*                                    -------         resultado.log  *
echo.* Envie este arquivo para analise.                                  *
ECHO.*********************************************************************
call :sub >>resultado.log
exit /b

:sub
echo @#@ Inicio ciclo de teste
echo @#@ date
date /t
echo @#@ time
time /t
ECHO.**********************************************************
echo.IPV4 Autenticação
powershell -command (Invoke-WebRequest -Uri https://api.ipify.org).Content
ECHO.**********************************************************
echo @#@ ipconfig
ipconfig
ECHO.**********************************************************
echo @#@ route print
route print
ECHO.**********************************************************
echo #@# ping 8.8.8.8
ping 8.8.8.8
ECHO.**********************************************************
echo @#@ tracert -d 8.8.8.8
tracert -d 8.8.8.8
ECHO.**********************************************************
echo @#@ tracert -4 google.com
tracert -4 google.com
ECHO.**********************************************************
echo @#@ tracert -4 terra.com.br
tracert -4 terra.com.br
ECHO.**********************************************************
ECHO @#@ tracert -4 uol.com.br
tracert -4 uol.com.br
ECHO.**********************************************************
echo #@# ping 2001:4860:4860::8888
ping 2001:4860:4860::8888
ECHO.**********************************************************
echo @#@ tracert -d -6 2001:4860:4860::8888
tracert -d -6 2001:4860:4860::8888
ECHO.**********************************************************
echo @#@ tracert -6 google.com
tracert -6 google.com
ECHO.**********************************************************
echo @#@ tracert -6 terra.com.br
tracert -6 terra.com.br
ECHO.**********************************************************
ECHO @#@ tracert -6 uol.com.br
tracert -6 uol.com.br
GOTO SUB
