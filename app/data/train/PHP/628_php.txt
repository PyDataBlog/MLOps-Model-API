<?php

namespace repositorio\estudianteBundle\Entity;

use Doctrine\ORM\Mapping as ORM;

/**
 * Sede
 *
 * @ORM\Table(name="sede")
 * @ORM\Entity
 */
class Sede
{
    /**
     * @var string
     *
     * @ORM\Column(name="codigo_sede", type="string", length=5, nullable=false)
     * @ORM\Id
     * @ORM\GeneratedValue(strategy="IDENTITY")
     */
    private $codigoSede;

    /**
     * @var string
     *
     * @ORM\Column(name="nombre_sede", type="string", length=50, nullable=false)
     */
    private $nombreSede;



    /**
     * Get codigoSede
     *
     * @return string 
     */
    public function getCodigoSede()
    {
        return $this->codigoSede;
    }
    public function setCodigoSede($codigosede)
    {
        $this->codigosede = $codigosede;
    
        return $this;
    }

    /**
     * Set nombreSede
     *
     * @param string $nombreSede
     * @return Sede
     */
    public function setNombreSede($nombreSede)
    {
        $this->nombreSede = $nombreSede;
    
        return $this;
    }

    /**
     * Get nombreSede
     *
     * @return string 
     */
    public function getNombreSede()
    {
        return $this->nombreSede;
    }
}