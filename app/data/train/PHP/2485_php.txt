<?php 

namespace Outil\ServiceBundle\Param;
use Symfony\Component\HttpKernel\Exception\Exception;
class Param{
	// private $mailer;
 //  private $locale;
 //  private $minLength;

  // public function __construct(\Swift_Mailer $mailer, $locale, $minLength)
  // {
  //   $this->mailer    = $mailer;
  //   $this->locale    = $locale;
  //   $this->minLength = (int) $minLength;
  // }
	/**
   * Vérifie si le texte est un spam ou non
   *
   * param string $text
   * return bool
   */
  public function except($text)
  {
    throw new \Exception('Votre message a été détecté comme spam !');
  }


}