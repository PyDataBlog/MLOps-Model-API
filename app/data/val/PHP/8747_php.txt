<?php 


/**
*	Classe de Connect à la base de donnée
* 
*	Vous noterez quelques redondances sur  les classes je m'en excuse pour le moment
*
*/
class Connect
{

	protected $bdd;
	
	function __construct()
	{
		$this->bdd = new PDO('mysql:host=localhost;dbname=intraesp_test','root','');
//		getBdd();
		
	}

	public function getBdd()
	{
		return $this->bdd;
	}
}


 ?>