<?php

namespace MPM\EntityBundle\Entity;

use Doctrine\ORM\Mapping as ORM;

/**
 * Mensaje
 *
 * @ORM\Table(name="mensajes")
 * @ORM\Entity(repositoryClass="MPM\EntityBundle\Entity\MensajeRepository")
 */
class Mensaje
{
    /**
     * @var integer
     *
     * @ORM\Column(name="id", type="integer")
     * @ORM\Id
     * @ORM\GeneratedValue(strategy="AUTO")
     */
    private $id;

    /**
     * @var string
     *
     * @ORM\Column(name="cuerpo", type="text")
     */
    private $cuerpo;

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="fecha", type="datetime")
     */
    private $fecha;

    /**
     * @var string
     *
     * @ORM\Column(name="ip", type="string", length=255)
     */
    private $ip;

    /**
     * @ORM\ManyToOne(targetEntity="Conversacion", inversedBy="mensajes")
     * @ORM\JoinColumn(name="conversacion_id", referencedColumnName="id")
     */
    private $conversacion;

    /**
     * @ORM\ManyToOne(targetEntity="Persona")
     * @ORM\JoinColumn(name="creador_id", referencedColumnName="id")
     */
    private $creador;

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
     * Set cuerpo
     *
     * @param string $cuerpo
     * @return Mensaje
     */
    public function setCuerpo($cuerpo)
    {
        $this->cuerpo = $cuerpo;

        return $this;
    }

    /**
     * Get cuerpo
     *
     * @return string 
     */
    public function getCuerpo()
    {
        return $this->cuerpo;
    }

    /**
     * Set fecha
     *
     * @param \DateTime $fecha
     * @return Mensaje
     */
    public function setFecha($fecha)
    {
        $this->fecha = $fecha;

        return $this;
    }

    /**
     * Get fecha
     *
     * @return \DateTime 
     */
    public function getFecha()
    {
        return $this->fecha;
    }

    /**
     * Set ip
     *
     * @param string $ip
     * @return Mensaje
     */
    public function setIp($ip)
    {
        $this->ip = $ip;

        return $this;
    }

    /**
     * Get ip
     *
     * @return string 
     */
    public function getIp()
    {
        return $this->ip;
    }

    /**
     * Set conversacion
     *
     * @param \MPM\EntityBundle\Entity\Conversacion $conversacion
     * @return Mensaje
     */
    public function setConversacion(\MPM\EntityBundle\Entity\Conversacion $conversacion = null)
    {
        $this->conversacion = $conversacion;

        return $this;
    }

    /**
     * Get conversacion
     *
     * @return \MPM\EntityBundle\Entity\Conversacion 
     */
    public function getConversacion()
    {
        return $this->conversacion;
    }

    /**
     * Set creador
     *
     * @param \MPM\EntityBundle\Entity\Persona $creador
     * @return Mensaje
     */
    public function setCreador(\MPM\EntityBundle\Entity\Persona $creador = null)
    {
        $this->creador = $creador;

        return $this;
    }

    /**
     * Get creador
     *
     * @return \MPM\EntityBundle\Entity\Persona 
     */
    public function getCreador()
    {
        return $this->creador;
    }
}
