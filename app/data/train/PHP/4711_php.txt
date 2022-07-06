<?php

namespace Web\GeneralBundle\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\Controller;

class GeneralController extends Controller
{
	//Muestra Index
    public function indexAction()
    {
    	$name='mensaje index';
        return $this->render('GeneralBundle:General:index.html.twig', array('name' => $name));
    }

    //Muestra sección.
    public function seccionAction()
    {
    	$name='mensaje seccion';
        return $this->render('GeneralBundle:General:index.html.twig', array('name' => $name));
    }
    
}
