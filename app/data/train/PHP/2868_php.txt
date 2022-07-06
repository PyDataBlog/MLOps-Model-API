<?php

namespace Cinemino\SiteBundle\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\Controller;

class DefaultController extends Controller
{
    public function indexAction($name)
    {
        return $this->render('CineminoSiteBundle:Default:index.html.twig', array('name' => $name));
    }
}
