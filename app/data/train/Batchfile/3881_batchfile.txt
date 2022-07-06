::
:: Script original creat de omnipotentul si omniprezentul HaZZe.
:: Versiune actuala: 0.0.2 (Beta)
::
@echo off
setlocal enabledelayedexpansion
color 0f
title V de la Vetta (BETA v0.0.2)

:entergame
cls
echo.
echo Salutare aventurierule, cu ce gand p-aici?
echo.
echo [1] Vreau sa ma conectez
echo [2] Vreau sa ma inregistrez
echo.
set /p login=
if %login% GEQ 3 goto troll
if %login% EQU 1 goto login
if %login% EQU 2 goto createuser

:createuser
cls
echo Deci esti nou prin aceste tinuturi, o sa-ti pun cateva intrebari:
echo.
echo Cum te-o botezat ma-ta?
set /p username1= 
set v1f=0

:checkforspaces
set x=!v1f!
set Letter%v1f%=!username1:~%x%,1!
if "!Letter%v1f%!" EQU " " (
echo.
echo Numele tau nu trebuie sa contina space.
pause>nul
goto entergame
)
if NOT "!Letter%v1f%!" EQU "" (
set /a v1f=%v1f%+1
goto checkforspaces
)
echo.
echo Care vrei sa fie parola ta?
set /p password1= 
goto DATA_VALUES

:login
cls
echo Scrie datele de logare mai jos:
echo.
set /p name1=Utilizator: 
if not exist "%name1%.bat" (
echo Trebuie sa-ti faci un cont mai intai.
pause>nul
goto entergame
)
set /p pass1=Parola: 
call %name1%.bat
if not %password1% EQU %pass1% (
echo Ai gresit parola.
pause>nul
goto entergame
)
goto MENU


:DATA_VALUES
set level=1
set exp=0
set money=0
set class=None
set weapon=Mana_Dreapta
set armour=None
set chp=100
set maxhp=100
set hpp=0
set cmp=50
set maxmp=50
set mpp=0
set defense=0
set damage=10
goto SAVE_GAME_FILES

:MENU
cls
echo.
echo ------------------------------------
echo        Ce vrei sa faci acum?
echo ------------------------------------
echo [1] Merg la curve
echo [2] Laba, fac laba
echo [3] Aleg o clasa
echo [4] Merg la cersit 
echo [5] Cumpara arma
echo [6] Cumpara armura
echo.
echo [98] Statistici
echo [99] Log out
echo.
echo ------------------------------------
echo Esti conectat ca: %username1%
echo.
set /p a1=
if %a1% EQU 1 goto whore
if %a1% EQU 2 goto wank
if %a1% EQU 3 goto class_chose
if %a1% EQU 4 goto make_money
if %a1% EQU 5 goto buy_weapon
if %a1% EQU 6 goto buy_armour
if %a1% EQU 98 goto statistics
if %a1% EQU 99 goto entergame
goto MENU

:troll
cls
echo.
echo Muie Frank.
pause>nul
goto entergame

:whore
cls
echo.
echo Tocmai ai luat sida, felicitari.
pause>nul
goto MENU

:wank
cls
echo.
echo %username1% esti un labagist profesionist.
pause>nul
goto MENU

:class_chose
cls
echo.
if %class% EQU Samanca (
echo Ti-ai ales deja clasa martalogule.
pause>nul
goto MENU
)
echo Salut %username1%, deci vrei sa-ti alegi clasa, nu?
echo Este o alegere fundamentala, te rog sa alegi intelept.
echo Ei bine, ai aici o varietate de clase din care poti alege:
echo [1] Samanca
echo [2] Samanca
echo [3] Samanca
echo [4] Samanca
echo [5] Samanca
set /p a2=
if %a2% EQU 1 set class=Samanca
if %a2% EQU 2 set class=Samanca
if %a2% EQU 3 set class=Samanca
if %a2% EQU 4 set class=Samanca
if %a2% EQU 5 set class=Samanca
goto class_chose_done

:class_chose_done
cls
echo.
echo Felicitari, ti-ai ales clasa: %class%
echo Inca n-am facut sistem de skill-uri, deci ti-ai ales-o degeaba.
pause>nul
goto SAVE_GAME_FILES

:make_money
cls
echo.
set /a money=%money%+10
set /a a3=%random% %% 3+1
if %a3% EQU 1 (
echo Un democrat te-a ranit si ai primit despagubire 10 lovele.
)
if %a3% EQU 2 (
echo Ai cautat intr-un tomberon si ai gasit 10 lovele si 39 de sobolani aproape morti.
)
if %a3% EQU 3 (
echo Un vagabond ti-a donat sperma si ai vandut-o pe 10 lovele.
echo P.S: Vagabondul avea sifilis si 3 tipuri de cancer pulmonar la ventriculul din artera dreapta a urechii.
)
pause>nul
goto SAVE_GAME_FILES

:buy_weapon
cls
echo.
echo ------------------------------------
echo           Cumpara o arma
echo ------------------------------------
echo Nu mai e sabii.
echo.
echo Dubgheste un buton pentru a reveni la meniu.
pasue>null
goto MENU

:buy_armour
cls
echo.
echo ------------------------------------
echo          Cumpara o armura
echo ------------------------------------
echo [1] Armura de piele
echo [2] Armura de fier
echo [3] Armura de paie
echo.
set /p a4=
if %a4% EQU 1 (
set armour=Armura_De_Piele
set defense=5 )
if %a4% EQU 2 (
set armour=Armura_De_Fier
set defense=10 )
if %a4% EQU 3 (
set armour=Armura_De_Paie
set defense=1 )
goto buyarmour

:buyarmour
cls
echo.
if %money% LSS 20 goto nomoney
set /a money=%money%-20
echo Ai cumparat armura %armour% cu 20 lovele.
pause>nul
goto SAVE_GAME_FILES

:nomoney
echo.
echo.
echo Nu ai suficienti bani.
echo.
pause>nul
goto MENU

:statistics
cls
echo.
echo ---------------------------------------				---------------------------------------
echo                Statistici							Armament
echo ---------------------------------------				---------------------------------------
echo Nume: %username1%		Clasa: %class%				Arma: %weapon%	Armura: %armour%
echo Viata: %chp%/%maxhp%		Mana: %cmp%/%maxmp%				Energizante: %hpp%		Bere: %mpp%
echo Nivel: %level%		Experienta: %exp%
echo Lovele: %money%
pause>nul
goto MENU

:SAVE_GAME_FILES
(
echo set username1=%username1%
echo set password1=%password1%
:DATA_VALUES
echo set level=%level%
echo set exp=%exp%
echo set money=%money%
echo set class=%class%
echo set weapon=%weapon%
echo set armour=%armour%
echo set chp=%chp%
echo set maxhp=%maxhp%
echo set hpp=%hpp%
echo set cmp=%cmp%
echo set maxmp=%maxmp%
echo set mpp=%mpp%
echo set defense=%defense%
echo set damage=%damage%
)>%username1%.bat
goto MENU
::
:: Copyright HaZZe - 2k17
::
