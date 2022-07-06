
setlocal enabledelayedexpansion

SET iter=200000



set BGE[01]="Enhance all Heal 1"
set BGE[02]="Enhance all Heal 2"
set BGE[03]="Enhance all Heal 3"



SET find="Optimized"
SET file="Brawl_of_Deities.txt"

echo %time%>%file% 

for /F "tokens=2 delims==" %%a in ('set BGE[') do (

	echo %%a>>%file%
	tuo "nkorboy" "GTD" gw ordered -e %%a -o=invent.txt -t 6 climb %iter% | find %find% >> %file% 
)
echo %time%>>%file%