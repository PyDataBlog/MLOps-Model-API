<?php

namespace Lam\MdlBundle\Entity;

use Doctrine\ORM\Mapping as ORM;

/**
 * Lam\MdlBundle\Entity\FormationInformatique
 * @ORM\Table()
 * @ORM\Entity(repositoryClass="Lam\MdlBundle\Entity\FormationInformatiqueRepository")
 */
class Formationinformatique
{
    /**
     * @var integer $id
     *
     * @ORM\Column(name="id", type="integer", nullable=false)
     * @ORM\Id
     * @ORM\GeneratedValue(strategy="IDENTITY")
     */
    private $id;

    /**
     * @var string $titre
     *
     * @ORM\Column(name="Titre", type="string", length=100, nullable=false)
     */
    private $titre;

    /**
     * @var text $objectif
     *
     * @ORM\Column(name="Objectif", type="text", nullable=false)
     */
    private $objectif;

    /**
     * @var string $public
     *
     * @ORM\Column(name="Public", type="string", length=150, nullable=false)
     */
    private $public;

    /**
     * @var text $prerequis
     *
     * @ORM\Column(name="Prerequis", type="text", nullable=false)
     */
    private $prerequis;

    /**
     * @var string $logo
     *
     * @ORM\Column(name="Logo", type="string", length=50, nullable=false)
     */
    private $logo;

    /**
     * @var integer $nbplace
     *
     * @ORM\Column(name="nbPlace", type="integer", nullable=false)
     */
    private $nbplace;

   


    /**
     * Get id
     *
     * @return integer 
     */
    public function getId()
    {
        return $this->id;
    }

    /**
     * Set titre
     *
     * @param string $titre
     */
    public function setTitre($titre)
    {
        $this->titre = $titre;
    }

    /**
     * Get titre
     *
     * @return string 
     */
    public function getTitre()
    {
        return $this->titre;
    }

    /**
     * Set objectif
     *
     * @param text $objectif
     */
    public function setObjectif($objectif)
    {
        $this->objectif = $objectif;
    }

    /**
     * Get objectif
     *
     * @return text 
     */
    public function getObjectif()
    {
        return $this->objectif;
    }

    /**
     * Set public
     *
     * @param string $public
     */
    public function setPublic($public)
    {
        $this->public = $public;
    }

    /**
     * Get public
     *
     * @return string 
     */
    public function getPublic()
    {
        return $this->public;
    }

    /**
     * Set prerequis
     *
     * @param text $prerequis
     */
    public function setPrerequis($prerequis)
    {
        $this->prerequis = $prerequis;
    }

    /**
     * Get prerequis
     *
     * @return text 
     */
    public function getPrerequis()
    {
        return $this->prerequis;
    }

    /**
     * Set logo
     *
     * @param string $logo
     */
    public function setLogo($logo)
    {
        $this->logo = $logo;
    }

    /**
     * Get logo
     *
     * @return string 
     */
    public function getLogo()
    {
        return $this->logo;
    }

    /**
     * Set nbplace
     *
     * @param integer $nbplace
     */
    public function setNbplace($nbplace)
    {
        $this->nbplace = $nbplace;
    }

    /**
     * Get nbplace
     *
     * @return integer 
     */
    public function getNbplace()
    {
        return $this->nbplace;
    }


}