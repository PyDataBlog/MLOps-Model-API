#!/bin/bash
# lag_ruter.sh
# Christer Jonassen 06.11.13

# Lager automatisk unike ruteverdier. f.eks krsman, sogven
# lager A-B og B-A av steder lagt til i filen steder.txt
# Genererer disse inn i switch statements for php

echo -e "<?php"
echo -e ""
echo -e "// Case statements laget automatisk med lag_ruter.sh"
echo -e "switch (\"\$rutekode\") {"
echo -e ""
while read -r fra
do
	
	while read -r til
	do
		if [ "$fra" != "$til" ]; then
    		echo -e "	case '$fra$til':	//Hvis rute er identifisert som $fra$til"
    		echo -e ""
    		echo -e "		switch (\"\$parameter\") {"
    		echo -e "			case 'hentinfo':	//Sett variabler for reisen"
    		echo -e "				//SETT INN VARIABLER FOR $fra-$til HER"
    		echo -e "				break;"
    		echo -e ""
    		echo -e "			case 'viskart':		//Vis HTML kart"
    		echo -e "				?>"
    		echo -e "				SETT INN KART FOR $fra-$til PÅ DENNE LINJEN"
    		echo -e "				<?php"
    		echo -e "				break;"
    		echo -e "		}"
    		echo -e "	break;"
    		echo -e ""
		fi
	done < steder.txt
done < steder.txt
echo -e "	case 'feil':"
echo -e "	break;"
echo -e ""
echo -e "}"
echo -e "?>"
