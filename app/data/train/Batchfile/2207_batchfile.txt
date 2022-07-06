cls
@ECHO OFF
color 4
echo ###################################
echo #                                 #
echo #        Elija un programa        #
echo #                                 #
echo #                                 #
echo ###################################
echo ----------------------------------
echo ----------------------------------
echo ----------------------------------
echo ----------------------------------
echo AA- Camtasia studio (path y todo)
echo BB- pronto disponible
echo CC- pronto disponible
echo DD- pronto disponible
echo EE- pronto disponible
echo FF- pronto disponible
echo GG- pronto disponible
echo Introduce la letra del programa
:HOLA
echo (OBLIGATORIO: PONEROLO EN MAYUSCULAS. ejem. AA)
set/p "cho=>"
if %cho%==AA goto AA
if %cho%==BB goto BB
if %cho%==CC goto CC
if %cho%==DD goto DD
if %cho%==EE goto EE
if %cho%==FF goto FF
if %cho%==GG goto GG
if %cho%==HH goto HH
echo caracteres invalidos
goto HOLA
:AA
echo Dale a cualquier boton para continuar:
pause
start camtac.bat
exit