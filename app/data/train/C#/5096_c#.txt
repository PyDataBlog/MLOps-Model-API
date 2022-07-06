/**
* This is the BaseDragoniarRace Class
* Just look what you edit here!
*
* @author Flavio Kleiber
* @copyright (c) 2015 PiratesAndChees Studios 
*/
using UnityEngine;
using System.Collections;

public class BaseDragoniarRace : BaseRace {

	public BaseDragoniarRace() {
		RaceName = "Dragoniar";
		RaceDesc = "Dragoniar are like dragons";

		//Set now the race bonus
		HasStaminaBonus 	= true;
		HasStrenghtBonus 	= true;
		HasAgillityBonus	= true;
	}
}
