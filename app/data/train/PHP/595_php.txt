<?php

namespace Superjobs\HomeBundle\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\Controller;
use Symfony\Component\HttpFoundation\Request;
use Superjobs\HomeBundle\Entity\CVtheque;
use Superjobs\HomeBundle\Form\CVthequeType;
use Superjobs\HomeBundle\Entity\Category;
use Superjobs\HomeBundle\Form\CategoryType;

class MainController extends Controller {

    public function indexAction(Request $request) {

        $em = $this->getDoctrine()->getManager();
        $jobs = $em->getRepository("SuperjobsHomeBundle:Jobs")->findBy(
                array(), array('id' => 'DESC')
        );

        $productRepository = $em->getRepository('SuperjobsHomeBundle:Category');
        $oCategory = $productRepository->findAll();
        
        foreach ($oCategory as $key => $oCategoryValue) {
            $category[$key] = $oCategoryValue->getName();
        }
        
        $paginator = $this->get('knp_paginator');
        $pagination = $paginator->paginate(
                $jobs, $request->query->get('page', 1)/* page number */, 10/* limit per page */
        );

        return $this->render('SuperjobsHomeBundle:Main:index.html.twig', array(
                    'pagination' => $pagination,
                    'category' => $category
        ));
    }

    public function feedbackAction() {
        return $this->render('SuperjobsHomeBundle:Main:feedback.html.twig');
    }

    public function contactAction() {
        return $this->render('SuperjobsHomeBundle:Main:contact.html.twig');
    }

    public function faqAction() {
        return $this->render('SuperjobsHomeBundle:Main:faq.html.twig');
    }

    public function pressAction() {
        return $this->render('SuperjobsHomeBundle:Main:press.html.twig');
    }

    public function cguAction() {
        return $this->render('SuperjobsHomeBundle:Main:cgu.html.twig');
    }

    public function aboutAction() {
        return $this->render('SuperjobsHomeBundle:Main:about.html.twig');
    }

    function detailsAction($id, Request $request) {

        $id = $request->query->get('tag');
        $job = $this->getDoctrine()->getRepository("SuperjobsHomeBundle:Jobs")->findOneBy(array('id' => $id));

        $CVtheque = new CVtheque();
        $form = $this->createForm(new CVthequeType(), $CVtheque);


        return $this->render('SuperjobsHomeBundle:Main:details.html.twig', array(
                    'job' => $job,
                    'form' => $form->createView(),
        ));
    }

    function searchEngineAction($pattern, Request $request) {
        if ($pattern == "all")
            $pattern = "";
        $Jobs = $this->get("superjobs_search_engine")->searchExpress($pattern);

        return $this->render('SuperjobsHomeBundle:SearchEngine:searchResults.html.twig', array(
                    'Jobs' => $Jobs
        ));
    }

}
