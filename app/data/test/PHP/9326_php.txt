<?php

namespace ItechSup\Bundle\QuestionnaireBundle\Menu;

use Knp\Menu\FactoryInterface;
use Doctrine\ORM\Entitymanager;

class Builder {
    private $factory;
    private $em;
    
    public function __construct(FactoryInterface $factory, Entitymanager $em){
        $this->factory = $factory;
        $this->em = $em;
    }
    
    public function createMainMenu() {
        $menu = $this->factory->createItem('root');
        
        //if ($this->get('security.context')->isGranted('ROLE_ADMIN')) {
            $menu->addChild( 'Gestion des appreciations', array('route' => 'questionnaire_appreciation_index') );
            $menu->addChild( 'Gestion des commentaires', array('route' => 'questionnaire_commentaire_index') );
            $menu->addChild( 'Getion des formulaires', array('route' => 'questionnaire_formulaire_index') );
            $menu->addChild( 'Gestion des questions', array('route' => 'questionnaire_question_index') );
            $menu->addChild( 'Gestion des sections', array('route' => 'questionnaire_section_index') );
            $menu->addChild( 'Retour à la page principale', array('route' => 'itech_sup_questionnaire_homepage') );
        //}
        
        
        
        return $menu;
        
    }
    
}