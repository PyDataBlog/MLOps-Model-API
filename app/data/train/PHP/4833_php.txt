<?php

namespace Kubus\BackendBundle\Form;

use Symfony\Component\Form\AbstractType;
use Symfony\Component\Form\FormBuilderInterface;
use Symfony\Component\OptionsResolver\OptionsResolverInterface;

class LessonType extends AbstractType
{
        /**
     * @param FormBuilderInterface $builder
     * @param array $options
     */
    public function buildForm(FormBuilderInterface $builder, array $options)
    {
        $builder
            ->add('courseId')
            ->add('stateId')
            ->add('beginAt')
            ->add('endAt')
            ->add('participantsMinNumber')
            ->add('participantsMaxNumber')
            ->add('childCare')
            ->add('publishAt')
            ->add('description')
            ->add('charge')
            ->add('externUrl')
            ->add('graduationYears')
            ->add('createdAt')
            ->add('updatedAt')
        ;
    }

    /**
     * @param OptionsResolverInterface $resolver
     */
    public function setDefaultOptions(OptionsResolverInterface $resolver)
    {
        $resolver->setDefaults(array(
            'data_class' => 'Kubus\BackendBundle\Entity\Lesson'
        ));
    }

    /**
     * @return string
     */
    public function getName()
    {
        return 'kubus_backendbundle_lesson';
    }
}
