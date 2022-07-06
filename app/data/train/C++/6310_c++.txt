/*!
* \file ComportementRoiMachine.cpp
* \brief Fichier contenant l'implémentation de la classe ComportementRoiMachine
* \author François Hallereau
* \author Sébastien Vallée
* \date 12.2014
*/

//--------------------------------------------------
/*!
* \brief Constructeur de la classe
*/
ComportementRoiMachine::ComportementRoiMachine():ComportementMachine(){}

//--------------------------------------------------
/*!
* \brief Destructeur de la classe
*/
ComportementRoiMachine::~ComportementRoiMachine(){}

//--------------------------------------------------
/*!
* \brief méthode de jeu pour le roi machine
* \param partie la partie
* \param joueur le joueur
*/
void ComportementRoiMachine::jouer(Partie * partie, Joueur * joueur){
	Roi::capacite(partie,joueur);
	choisirGainTour(joueur);
	choisirConstruction(joueur);
}
