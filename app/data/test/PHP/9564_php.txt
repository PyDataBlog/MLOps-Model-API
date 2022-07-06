<?php
/**
 * Created by JetBrains PhpStorm.
 * User: Petit
 * Date: 20/02/13
 * Time: 14:56
 * To change this template use File | Settings | File Templates.
 */

namespace Petrae\FichierPlanteBundle\Entity;
use Symfony\Component\Validator\Constraints as Assert;

class Categorie
{
    /**
     * @Assert\NotBlank()
     */
    public $name;
}