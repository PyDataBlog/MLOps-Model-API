<?php

namespace AppBundle\Controller;

use Sensio\Bundle\FrameworkExtraBundle\Configuration\Route;
use Symfony\Bundle\FrameworkBundle\Controller\Controller;

class MyTemperatureController extends Controller
{

    /**
     * @Route("/temperatures.html", name="temperatures")
     */
    public function indexAction()
    {
        $em = $this->getDoctrine()->getManager();

        $entities = $em->getRepository('AppBundle:Temperature')->findAll();

        return $this->render(
            'default/temperatures.html.twig',
            array(
                'entities' => $entities,
            )
        );
    }

}
