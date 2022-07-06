echo off

set RHAP_JARS_DIR=C:/ProgramData/IBM/Rational/Rhapsody/8.1.1/Share\LangJava\lib
set FRAMEWORK_NONE_JARS=C:/ProgramData/IBM/Rational/Rhapsody/8.1.1/Share\LangJava\lib\oxf.jar;C:/ProgramData/IBM/Rational/Rhapsody/8.1.1/Share\LangJava\lib\anim.jar;C:/ProgramData/IBM/Rational/Rhapsody/8.1.1/Share\LangJava\lib\animcom.jar;C:/ProgramData/IBM/Rational/Rhapsody/8.1.1/Share\LangJava\lib\oxfInstMock.jar;
set FRAMEWORK_ANIM_JARS=C:/ProgramData/IBM/Rational/Rhapsody/8.1.1/Share\LangJava\lib\oxf.jar;C:/ProgramData/IBM/Rational/Rhapsody/8.1.1/Share\LangJava\lib\anim.jar;C:/ProgramData/IBM/Rational/Rhapsody/8.1.1/Share\LangJava\lib\animcom.jar;C:/ProgramData/IBM/Rational/Rhapsody/8.1.1/Share\LangJava\lib\oxfInst.jar;
set SOURCEPATH=%SOURCEPATH%
set CLASSPATH=%CLASSPATH%;.
set PATH=%RHAP_JARS_DIR%;%PATH%;
set INSTRUMENTATION=Animation   

set BUILDSET=Debug

if %INSTRUMENTATION%==Animation goto anim

:noanim
set CLASSPATH=%CLASSPATH%;%FRAMEWORK_NONE_JARS%
goto setEnv_end

:anim
set CLASSPATH=%CLASSPATH%;%FRAMEWORK_ANIM_JARS%

:setEnv_end

if "%1" == "" goto compile
if "%1" == "build" goto compile
if "%1" == "clean" goto clean
if "%1" == "rebuild" goto clean
if "%1" == "run" goto run

:clean
echo cleaning class files
if exist Default\EventFinAppelTemoin.class del Default\EventFinAppelTemoin.class
if exist Default\EventVictimeSurvie.class del Default\EventVictimeSurvie.class
if exist Default\EventVictimeSecouru.class del Default\EventVictimeSecouru.class
if exist Default\EventEnCoursAssaut.class del Default\EventEnCoursAssaut.class
if exist Default\EventEnvoye.class del Default\EventEnvoye.class
if exist Default\EventSeFaitExploser.class del Default\EventSeFaitExploser.class
if exist Default\Victime.class del Default\Victime.class
if exist Default\EventPriseOtage.class del Default\EventPriseOtage.class
if exist Default\EventFinCooperation.class del Default\EventFinCooperation.class
if exist Default\EventDemandeInformation.class del Default\EventDemandeInformation.class
if exist Default\EventDebutAppelCentreAppel.class del Default\EventDebutAppelCentreAppel.class
if exist Default\EventGIGNMort.class del Default\EventGIGNMort.class
if exist Default\EventAppelPoliceEnCours.class del Default\EventAppelPoliceEnCours.class
if exist Default\EventReussi.class del Default\EventReussi.class
if exist Default\EventAppelleCentreAppel.class del Default\EventAppelleCentreAppel.class
if exist Default\police.class del Default\police.class
if exist Default\Terroriste.class del Default\Terroriste.class
if exist Default\EventEtateAlerte.class del Default\EventEtateAlerte.class
if exist Default\EventFinCopperation.class del Default\EventFinCopperation.class
if exist Default\GIGN.class del Default\GIGN.class
if exist Default\EventFinAppelPolice.class del Default\EventFinAppelPolice.class
if exist Default\EventDebutAppelPolice.class del Default\EventDebutAppelPolice.class
if exist Default\EventEtatAlerte.class del Default\EventEtatAlerte.class
if exist Default\EventCooperation.class del Default\EventCooperation.class
if exist Default\EventEnAttenteInstruction.class del Default\EventEnAttenteInstruction.class
if exist Default\EventGIGNVivant.class del Default\EventGIGNVivant.class
if exist Default\EventEnAttente.class del Default\EventEnAttente.class
if exist Default\EventLibre.class del Default\EventLibre.class
if exist Default\EventLanceAssaut.class del Default\EventLanceAssaut.class
if exist Default\EventAppelleCentreCommandement.class del Default\EventAppelleCentreCommandement.class
if exist Default\EventTirSurVictime.class del Default\EventTirSurVictime.class
if exist Default\Default_pkgClass.class del Default\Default_pkgClass.class
if exist Default\EventAssautReussi.class del Default\EventAssautReussi.class
if exist Default\EventRecoitAppel.class del Default\EventRecoitAppel.class
if exist Default\EventFinMission.class del Default\EventFinMission.class
if exist Default\EventNonSecurisee.class del Default\EventNonSecurisee.class
if exist Default\EventMort.class del Default\EventMort.class
if exist Default\EventTerroristeNeutralise.class del Default\EventTerroristeNeutralise.class
if exist Default\Temoin.class del Default\Temoin.class
if exist Default\EventDebutAppelCentreCommandement.class del Default\EventDebutAppelCentreCommandement.class
if exist Default\EventAnalyseSituation.class del Default\EventAnalyseSituation.class
if exist Default\EventFinAppel.class del Default\EventFinAppel.class
if exist Default\EventSoigne.class del Default\EventSoigne.class
if exist Default\EventVictimeMort.class del Default\EventVictimeMort.class
if exist Default\EventAssautEchec.class del Default\EventAssautEchec.class
if exist Default\otage.class del Default\otage.class
if exist Default\EventFinAppelCentreAppel.class del Default\EventFinAppelCentreAppel.class
if exist Default\EventEchec.class del Default\EventEchec.class
if exist Default\centreCommandement.class del Default\centreCommandement.class
if exist MainDefaultComponent.class del MainDefaultComponent.class
if exist Default\EventFinAppelCentreCommandement.class del Default\EventFinAppelCentreCommandement.class
if exist Default\EventAppelTemoin.class del Default\EventAppelTemoin.class
if exist Default\EventSecurisee.class del Default\EventSecurisee.class
if exist Default\CentreAppel.class del Default\CentreAppel.class
if exist Default\EventDebutAppelTemoin.class del Default\EventDebutAppelTemoin.class
if exist Default\Secours.class del Default\Secours.class
if exist Default\Place.class del Default\Place.class

if "%1" == "clean" goto end

:compile   
if %BUILDSET%==Debug goto compile_debug
echo compiling JAVA source files
javac  @files.lst
goto end

:compile_debug
echo compiling JAVA source files
javac -g  @files.lst
goto end

:run

java %2

:end


