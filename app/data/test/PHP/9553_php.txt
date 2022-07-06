<?php
namespace App\LouvreBundle\Services;

use Symfony\Component\HttpFoundation\RedirectResponse;
use Symfony\Component\Routing\RouterInterface;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpKernel\Exception\NotFoundHttpException;
use Symfony\Component\Security\Core\Exception\AccessDeniedException;
use App\LouvreBundle\Controller\DefaultController;
use Symfony\component\HttpFoundation\Session\Session;

class ValidTime
{
	/**
	* Vérifie si le texte est un spam ou non
	*
	* @param string $text
	* @return bool
	*/
	public function ticketsTypeValid($redirection)
	{
		$now = new \DateTime();
		$dDay = $now->format('y-m-d');
		$dHour = $now->format('H');
		$session = new Session();
		$vDay = $session->get('visitDate')->format('y-m-d');
		
		// Pas de billet journée après 14h
		if($session->get('ticketsType') == 1){

  			if ($dDay == $vDay && $dHour > 13){

				$session->clear();
				$session->getFlashBag()->add('errors', 'Erreur impossible de Commander un billet journée après 14h');
				return $redirection;
			}
		}
  }
}