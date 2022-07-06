<?php
namespace Fdr\UserBundle\Form\Type;

//use Symfony\Component\Form\FormBuilder;
//use FOS\UserBundle\Form\Type\RegistrationFormType as BaseType;
use Symfony\Component\Form\AbstractType;
use Symfony\Component\Form\FormBuilderInterface;
use Doctrine\ORM\EntityRepository;
class RegistrationFormType extends AbstractType
//extends BaseType getParent
{
    //public function buildForm(FormBuilder $builder, array $options)
    public function buildForm(FormBuilderInterface $builder, array $options)
    {
        //parent::buildForm($builder, $options);
        $builder->add('nom');
        $builder->add('prenom');
        $builder->add('matricule',null,array('required'=>false));
        $builder->add('tel');
        $builder->add('adresse');
        $builder->add('plainPassword', 'repeated', array('label'=>false,
                'type' => 'password',
                'first_options' => array('label' => 'Taper mdp. securisé  ','attr'=>array('class'=>'')),
                'second_options' => array('label' =>'Confirmation de mdp ','attr'=>array('class'=>'')),
                'invalid_message' => 'Les deux mots de passe ne sont pas identiques',
            ));
        $builder->add('email');
        $builder->add('cin');
        $builder->add('depot',null,array('placeholder'=>'Choisir un depot','attr'=>array('title'=>'Choisir un depot')));
        $builder->add("locked",null,array('label'=>'Compte verrouillé','required'=>false));
        $builder->add("expired",null,array('label'=>'Compte expiré ','required'=>false));
        $builder->add("expiresAt",'datetime', array('date_widget' => "single_text", 'time_widget' => "single_text",'required'=>false));
        $builder->add('roles', 'entity', array('required'=>true,
                                               'placeholder'=>'Choisir un rôle',
                                               'mapped'=>false,
                                               'attr'=>array('title'=>'Choisir un rôle'),
                                               'class' => 'FdrAdminBundle:Role',
                                               'query_builder' => function(EntityRepository $er) {
                                                                         return $er->createQueryBuilder('c');
                                                                                     })); 
       $builder->add('submit','submit',array('label'=>'Enregistrer','attr'=>array('class'=>'btn btn-default')));                                                                                     
    }

    public function getName()
    {
        return 'fdr_user_registration';
    }
    public function getParent()
    {
        return 'fos_user_registration';
    }
}