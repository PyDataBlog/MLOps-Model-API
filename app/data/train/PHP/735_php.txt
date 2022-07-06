<?php

namespace RandomRango\Bundle\MainBundle\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\Controller;

class DefaultController extends Controller
{
    public function indexAction()
    {
        return $this->render('RandomRangoMainBundle:Default:index.html.twig');
    }
}
