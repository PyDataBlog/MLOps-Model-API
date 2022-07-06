if "%HYPTYPE%"=="BA" do pushd "%HSMETA%\%CPS%\Images\%ARTNUM%" && set CPSNAME=Folder.png
if "%HYPTYPE%"=="VD" do pushd "%HSMETA%\%CPS%\Video" && set CPSNAME=.trailer\%%~a
if "%HYPTYPE%"=="BN" do pushd "%HSMETA%\%CPS%\Images\Wheel" && set CPSNAME=Banner.png
for %%a in ("Forgotten Worlds (World)") do set NWDIR=%%~a
for %%a in ("forgottn.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Forgotten Worlds (USA, B-Board 88621B-2)") do set NWDIR=%%~a
for %%a in ("forgottnu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Forgotten Worlds (USA, B-Board 88618B-?)") do set NWDIR=%%~a
for %%a in ("forgottnua.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Lost Worlds (Japan)") do set NWDIR=%%~a
for %%a in ("lostwrld.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Lost Worlds (Japan Old Ver.)") do set NWDIR=%%~a
for %%a in ("lostwrldo.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Ghouls'n Ghosts (World)") do set NWDIR=%%~a
for %%a in ("ghouls.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Ghouls'n Ghosts (USA)") do set NWDIR=%%~a
for %%a in ("ghoulsu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Daimakaimura (Japan)") do set NWDIR=%%~a
for %%a in ("daimakai.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Daimakaimura (Japan Resale Ver.)") do set NWDIR=%%~a
for %%a in ("daimakair.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Strider (USA, B-Board 89624B-2)") do set NWDIR=%%~a
for %%a in ("strider.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Strider (USA, B-Board 89624B-3)") do set NWDIR=%%~a
for %%a in ("striderua.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Strider Hiryu (Japan)") do set NWDIR=%%~a
for %%a in ("striderj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Strider Hiryu (Japan Resale Ver.)") do set NWDIR=%%~a
for %%a in ("striderjr.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Dynasty Wars (USA, B-Board 89624B-?)") do set NWDIR=%%~a
for %%a in ("dynwar.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Dynasty Wars (USA, B-Board 88622B-3)") do set NWDIR=%%~a
for %%a in ("dynwara.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Tenchi wo Kurau (Japan)") do set NWDIR=%%~a
for %%a in ("dynwarj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Willow (USA)") do set NWDIR=%%~a
for %%a in ("willow.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Willow (Japan, Japanese)") do set NWDIR=%%~a
for %%a in ("willowj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Willow (Japan, English)") do set NWDIR=%%~a
for %%a in ("willowje.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("U.N. Squadron (USA)") do set NWDIR=%%~a
for %%a in ("unsquad.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Area 88 (Japan)") do set NWDIR=%%~a
for %%a in ("area88.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Area 88 (Japan Resale Ver.)") do set NWDIR=%%~a
for %%a in ("area88r.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Final Fight (World)") do set NWDIR=%%~a
for %%a in ("ffight.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Final Fight (USA)") do set NWDIR=%%~a
for %%a in ("ffightu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Final Fight (USA 900112)") do set NWDIR=%%~a
for %%a in ("ffightua.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Final Fight (USA 900613)") do set NWDIR=%%~a
for %%a in ("ffightub.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Final Fight (Japan)") do set NWDIR=%%~a
for %%a in ("ffightj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Final Fight (Japan 900112)") do set NWDIR=%%~a
for %%a in ("ffightj1.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Final Fight (Japan 900305)") do set NWDIR=%%~a
for %%a in ("ffightj2.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Smart / Final Fight (Japan, hack)") do set NWDIR=%%~a
for %%a in ("ffightjh.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("1941 - Counter Attack (World)") do set NWDIR=%%~a
for %%a in ("1941.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("1941 - Counter Attack (Japan)") do set NWDIR=%%~a
for %%a in ("1941j.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Mercs (World 900302)") do set NWDIR=%%~a
for %%a in ("mercs.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Mercs (USA 900302)") do set NWDIR=%%~a
for %%a in ("mercsu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Mercs (USA 900608)") do set NWDIR=%%~a
for %%a in ("mercsua.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Senjou no Ookami II (Japan 900302)") do set NWDIR=%%~a
for %%a in ("mercsj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Mega Twins (World 900619)") do set NWDIR=%%~a
for %%a in ("mtwins.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Chiki Chiki Boys (Japan 900619)") do set NWDIR=%%~a
for %%a in ("chikij.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Magic Sword - Heroic Fantasy (World 900725)") do set NWDIR=%%~a
for %%a in ("msword.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Magic Sword - Heroic Fantasy (World 900623)") do set NWDIR=%%~a
for %%a in ("mswordr1.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Magic Sword - Heroic Fantasy (USA 900725)") do set NWDIR=%%~a
for %%a in ("mswordu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Magic Sword - Heroic Fantasy (Japan 900623)") do set NWDIR=%%~a
for %%a in ("mswordj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Carrier Air Wing (World 901012)") do set NWDIR=%%~a
for %%a in ("cawing.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Carrier Air Wing (World 901009)") do set NWDIR=%%~a
for %%a in ("cawingr1.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Carrier Air Wing (USA 901012)") do set NWDIR=%%~a
for %%a in ("cawingu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("U.S. Navy (Japan 901012)") do set NWDIR=%%~a
for %%a in ("cawingj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Nemo (World 901130)") do set NWDIR=%%~a
for %%a in ("nemo.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Nemo (Japan 901120)") do set NWDIR=%%~a
for %%a in ("nemoj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (World 910522)") do set NWDIR=%%~a
for %%a in ("sf2.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (World 910214)") do set NWDIR=%%~a
for %%a in ("sf2eb.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (TAB Austria, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2ebbl.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (USA 910206)") do set NWDIR=%%~a
for %%a in ("sf2ua.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (USA 910214)") do set NWDIR=%%~a
for %%a in ("sf2ub.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (USA 910318)") do set NWDIR=%%~a
for %%a in ("sf2ud.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (USA 910228)") do set NWDIR=%%~a
for %%a in ("sf2ue.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (USA 910411)") do set NWDIR=%%~a
for %%a in ("sf2uf.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (USA 910522)") do set NWDIR=%%~a
for %%a in ("sf2ui.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (USA 911101)") do set NWDIR=%%~a
for %%a in ("sf2uk.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (Japan 911210)") do set NWDIR=%%~a
for %%a in ("sf2j.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (Japan 910214)") do set NWDIR=%%~a
for %%a in ("sf2ja.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (Japan 910306)") do set NWDIR=%%~a
for %%a in ("sf2jc.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (Quicken Pt-I, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2qp1.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II - The World Warrior (Thunder Edition, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2thndr.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Three Wonders (World 910520)") do set NWDIR=%%~a
for %%a in ("3wonders.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Three Wonders (USA 910520)") do set NWDIR=%%~a
for %%a in ("3wondersu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Wonder 3 (Japan 910520)") do set NWDIR=%%~a
for %%a in ("wonder3.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Three Wonders (hack)") do set NWDIR=%%~a
for %%a in ("3wondersh.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("The King of Dragons (World 910711)") do set NWDIR=%%~a
for %%a in ("kod.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("The King of Dragons (USA 910910)") do set NWDIR=%%~a
for %%a in ("kodu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("The King of Dragons (Japan 910805, B-Board 90629B-3)") do set NWDIR=%%~a
for %%a in ("kodj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("The King of Dragons (Japan 910805, B-Board 89625B-1)") do set NWDIR=%%~a
for %%a in ("kodja.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Captain Commando (World 911202)") do set NWDIR=%%~a
for %%a in ("captcomm.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Captain Commando (World 911014)") do set NWDIR=%%~a
for %%a in ("captcommr1.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Captain Commando (USA 910928)") do set NWDIR=%%~a
for %%a in ("captcommu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Captain Commando (Japan 911202)") do set NWDIR=%%~a
for %%a in ("captcommj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Captain Commando (Japan 910928)") do set NWDIR=%%~a
for %%a in ("captcommjr1.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Captain Commando (bootleg)") do set NWDIR=%%~a
for %%a in ("captcommb.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Knights of the Round (World 911127)") do set NWDIR=%%~a
for %%a in ("knights.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Knights of the Round (USA 911127)") do set NWDIR=%%~a
for %%a in ("knightsu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Knights of the Round (Japan 911127, B-Board 91634B-2)") do set NWDIR=%%~a
for %%a in ("knightsj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Knights of the Round (Japan 911127, B-Board 89625B-1)") do set NWDIR=%%~a
for %%a in ("knightsja.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Knights of the Round (bootleg)") do set NWDIR=%%~a
for %%a in ("knightsb.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (World 920513)") do set NWDIR=%%~a
for %%a in ("sf2ce.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (World 920313)") do set NWDIR=%%~a
for %%a in ("sf2ceea.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (USA 920313)") do set NWDIR=%%~a
for %%a in ("sf2ceua.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (USA 920513)") do set NWDIR=%%~a
for %%a in ("sf2ceub.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (USA 920803)") do set NWDIR=%%~a
for %%a in ("sf2ceuc.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (Japan 920513)") do set NWDIR=%%~a
for %%a in ("sf2cej.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (Rainbow set 1, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2rb.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (Rainbow set 2, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2rb2.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (Rainbow set 3, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2rb3.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (Red Wave, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2red.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (V004, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2v004.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (Accelerator!, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2acc.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (Accelerator Pt.II, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2accp2.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (Double K.O. Turbo II, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2dkot2.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (M1, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2m1.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (M2, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2m2.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (M3, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2m3.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (M4, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2m4.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (M5, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2m5.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (M6, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2m6.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (M7, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2m7.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (YYC, bootleg)") do set NWDIR=%%~a
for %%a in ("sf2yyc.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (Xiang Long, Chinese bootleg)") do set NWDIR=%%~a
for %%a in ("sf2koryu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Magic Delta Turbo (bootleg)") do set NWDIR=%%~a
for %%a in ("sf2mdt.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Varth - Operation Thunderstorm (World 920714)") do set NWDIR=%%~a
for %%a in ("varth.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Varth - Operation Thunderstorm (World 920612)") do set NWDIR=%%~a
for %%a in ("varthr1.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Varth - Operation Thunderstorm (USA 920612)") do set NWDIR=%%~a
for %%a in ("varthu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Varth - Operation Thunderstorm (Japan 920714)") do set NWDIR=%%~a
for %%a in ("varthj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Adventure Quiz Capcom World 2 (Japan 920611)") do set NWDIR=%%~a
for %%a in ("cworld2j.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Warriors of Fate (World 921002)") do set NWDIR=%%~a
for %%a in ("wof.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Sangokushi II (Asia 921005)") do set NWDIR=%%~a
for %%a in ("wofa.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Warriors of Fate (USA 921031)") do set NWDIR=%%~a
for %%a in ("wofu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Tenchi wo Kurau II - Sekiheki no Tatakai (Japan 921031)") do set NWDIR=%%~a
for %%a in ("wofj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Huo Feng Huang (Chinese bootleg of Sangokushi II)") do set NWDIR=%%~a
for %%a in ("wofhfh.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Hyper Fighting (World 921209)") do set NWDIR=%%~a
for %%a in ("sf2hf.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Hyper Fighting (USA 921209)") do set NWDIR=%%~a
for %%a in ("sf2hfu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' Turbo - Hyper Fighting (Japan 921209)") do set NWDIR=%%~a
for %%a in ("sf2hfj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Cadillacs and Dinosaurs (World 930201)") do set NWDIR=%%~a
for %%a in ("dino.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Cadillacs and Dinosaurs (USA 930201)") do set NWDIR=%%~a
for %%a in ("dinou.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Cadillacs - Kyouryuu Shin Seiki (Japan 930201)") do set NWDIR=%%~a
for %%a in ("dinoj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Cadillacs and Dinosaurs (bootleg with PIC16c57, set 1)") do set NWDIR=%%~a
for %%a in ("dinopic.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Cadillacs and Dinosaurs (bootleg with PIC16c57, set 2)") do set NWDIR=%%~a
for %%a in ("dinopic2.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Dinosaur Hunter (Chinese bootleg of Cadillacs and Dinosaurs)") do set NWDIR=%%~a
for %%a in ("dinohunt.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("The Punisher (World 930422)") do set NWDIR=%%~a
for %%a in ("punisher.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("The Punisher (USA 930422)") do set NWDIR=%%~a
for %%a in ("punisheru.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("The Punisher (Japan 930422)") do set NWDIR=%%~a
for %%a in ("punisherj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("The Punisher (bootleg with PIC16c57, set 1)") do set NWDIR=%%~a
for %%a in ("punipic.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("The Punisher (bootleg with PIC16c57, set 2)") do set NWDIR=%%~a
for %%a in ("punipic2.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("The Punisher (bootleg with PIC16c57, set 3)") do set NWDIR=%%~a
for %%a in ("punipic3.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Biaofeng Zhanjing (Chinese bootleg of The Punisher)") do set NWDIR=%%~a
for %%a in ("punisherbz.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Saturday Night Slam Masters (World 930713)") do set NWDIR=%%~a
for %%a in ("slammast.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Saturday Night Slam Masters (USA 930713)") do set NWDIR=%%~a
for %%a in ("slammastu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Muscle Bomber - The Body Explosion (Japan 930713)") do set NWDIR=%%~a
for %%a in ("mbomberj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Muscle Bomber Duo - Ultimate Team Battle (World 931206)") do set NWDIR=%%~a
for %%a in ("mbombrd.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Muscle Bomber Duo - Heat Up Warriors (Japan 931206)") do set NWDIR=%%~a
for %%a in ("mbombrdj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Pnickies (Japan 940608)") do set NWDIR=%%~a
for %%a in ("pnickj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Quiz &amp; Dragons - Capcom Quiz Game (USA 920701)") do set NWDIR=%%~a
for %%a in ("qad.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Quiz &amp; Dragons - Capcom Quiz Game (Japan 940921)") do set NWDIR=%%~a
for %%a in ("qadj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Quiz Tonosama no Yabou 2 - Zenkoku-ban (Japan 950123)") do set NWDIR=%%~a
for %%a in ("qtono2j.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Pang! 3 (Euro 950601)") do set NWDIR=%%~a
for %%a in ("pang3.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Pang! 3 (Euro 950511, not encrypted)") do set NWDIR=%%~a
for %%a in ("pang3n.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Pang! 3 - Kaitou Tachi no Karei na Gogo (Japan 950511)") do set NWDIR=%%~a
for %%a in ("pang3j.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Mega Man - The Power Battle (CPS1, Asia 951006)") do set NWDIR=%%~a
for %%a in ("megaman.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Rockman - The Power Battle (CPS1, Japan 950922)") do set NWDIR=%%~a
for %%a in ("rockmanj.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Cadillacs and Dinosaurs (Hack)") do set NWDIR=%%~a
for %%a in ("dinoh.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Knights of the Round (Hack)") do set NWDIR=%%~a
for %%a in ("knightsh.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter Zero (CPS Changer)") do set NWDIR=%%~a
for %%a in ("sfzch.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter Alpha (Publicity CPS Changer 950727)") do set NWDIR=%%~a
for %%a in ("sfach.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Warriors of Fate (Hack)") do set NWDIR=%%~a
for %%a in ("wofh.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (non-MAME, Turyu)") do set NWDIR=%%~a
for %%a in ("sf2turyu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (Turyu Alternate)") do set NWDIR=%%~a
for %%a in ("sf2turyu1.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"
for %%a in ("Street Fighter II' - Champion Edition (Tulong)") do set NWDIR=%%~a
for %%a in ("sf2toryu.png") do if exist "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%" copy /y "%%~a" "%GBG%\%GAM%\%CONS%\%CPS%\%NWDIR%\%CPSNAME%"