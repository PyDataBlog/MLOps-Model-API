<?php

namespace Mastercel\ChartBundle\Entity;

use Doctrine\ORM\Mapping as ORM;

/**
 * Solicitudespagos
 *
 * @ORM\Table(name="SolicitudesPagos", indexes={@ORM\Index(name="Indice_1", columns={"DteSolicitud", "TmeSolicitud"}), @ORM\Index(name="Indice_2", columns={"DteActualizacion"}), @ORM\Index(name="Indice_3", columns={"NumEstadoComunicacion"})})
 * @ORM\Entity
 */
class Solicitudespagos
{
    /**
     * @var integer
     *
     * @ORM\Column(name="NumSolicitud_id", type="integer", nullable=false)
     * @ORM\Id
     * @ORM\GeneratedValue(strategy="NONE")
     */
    private $numsolicitudId = '0';

    /**
     * @var integer
     *
     * @ORM\Column(name="NumAlmacenSolicitud_id", type="integer", nullable=false)
     * @ORM\Id
     * @ORM\GeneratedValue(strategy="NONE")
     */
    private $numalmacensolicitudId = '0';

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="DteSolicitud", type="datetime", nullable=true)
     */
    private $dtesolicitud;

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="TmeSolicitud", type="datetime", nullable=true)
     */
    private $tmesolicitud;

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="DteVencimiento", type="datetime", nullable=true)
     */
    private $dtevencimiento;

    /**
     * @var integer
     *
     * @ORM\Column(name="NumEmpresa_id", type="integer", nullable=true)
     */
    private $numempresaId = '0';

    /**
     * @var integer
     *
     * @ORM\Column(name="NumMoneda_id", type="integer", nullable=true)
     */
    private $nummonedaId = '0';

    /**
     * @var integer
     *
     * @ORM\Column(name="NumEjecutivo_id", type="integer", nullable=true)
     */
    private $numejecutivoId = '0';

    /**
     * @var integer
     *
     * @ORM\Column(name="NumProveedor_id", type="integer", nullable=true)
     */
    private $numproveedorId = '0';

    /**
     * @var integer
     *
     * @ORM\Column(name="NumCategoria_id", type="integer", nullable=true)
     */
    private $numcategoriaId = '0';

    /**
     * @var integer
     *
     * @ORM\Column(name="NumAutorizador_id", type="integer", nullable=true)
     */
    private $numautorizadorId = '0';

    /**
     * @var integer
     *
     * @ORM\Column(name="NumCreadoPor_id", type="integer", nullable=true)
     */
    private $numcreadoporId = '0';

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="DteCreacion", type="datetime", nullable=true)
     */
    private $dtecreacion;

    /**
     * @var integer
     *
     * @ORM\Column(name="NumActualizadoPor_id", type="integer", nullable=true)
     */
    private $numactualizadoporId = '0';

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="DteActualizacion", type="datetime", nullable=true)
     */
    private $dteactualizacion;

    /**
     * @var integer
     *
     * @ORM\Column(name="NumTipoEstado", type="integer", nullable=true)
     */
    private $numtipoestado = '0';

    /**
     * @var integer
     *
     * @ORM\Column(name="NumEstadoComunicacion", type="integer", nullable=true)
     */
    private $numestadocomunicacion = '0';



    /**
     * Set numsolicitudId
     *
     * @param integer $numsolicitudId
     * @return Solicitudespagos
     */
    public function setNumsolicitudId($numsolicitudId)
    {
        $this->numsolicitudId = $numsolicitudId;

        return $this;
    }

    /**
     * Get numsolicitudId
     *
     * @return integer 
     */
    public function getNumsolicitudId()
    {
        return $this->numsolicitudId;
    }

    /**
     * Set numalmacensolicitudId
     *
     * @param integer $numalmacensolicitudId
     * @return Solicitudespagos
     */
    public function setNumalmacensolicitudId($numalmacensolicitudId)
    {
        $this->numalmacensolicitudId = $numalmacensolicitudId;

        return $this;
    }

    /**
     * Get numalmacensolicitudId
     *
     * @return integer 
     */
    public function getNumalmacensolicitudId()
    {
        return $this->numalmacensolicitudId;
    }

    /**
     * Set dtesolicitud
     *
     * @param \DateTime $dtesolicitud
     * @return Solicitudespagos
     */
    public function setDtesolicitud($dtesolicitud)
    {
        $this->dtesolicitud = $dtesolicitud;

        return $this;
    }

    /**
     * Get dtesolicitud
     *
     * @return \DateTime 
     */
    public function getDtesolicitud()
    {
        return $this->dtesolicitud;
    }

    /**
     * Set tmesolicitud
     *
     * @param \DateTime $tmesolicitud
     * @return Solicitudespagos
     */
    public function setTmesolicitud($tmesolicitud)
    {
        $this->tmesolicitud = $tmesolicitud;

        return $this;
    }

    /**
     * Get tmesolicitud
     *
     * @return \DateTime 
     */
    public function getTmesolicitud()
    {
        return $this->tmesolicitud;
    }

    /**
     * Set dtevencimiento
     *
     * @param \DateTime $dtevencimiento
     * @return Solicitudespagos
     */
    public function setDtevencimiento($dtevencimiento)
    {
        $this->dtevencimiento = $dtevencimiento;

        return $this;
    }

    /**
     * Get dtevencimiento
     *
     * @return \DateTime 
     */
    public function getDtevencimiento()
    {
        return $this->dtevencimiento;
    }

    /**
     * Set numempresaId
     *
     * @param integer $numempresaId
     * @return Solicitudespagos
     */
    public function setNumempresaId($numempresaId)
    {
        $this->numempresaId = $numempresaId;

        return $this;
    }

    /**
     * Get numempresaId
     *
     * @return integer 
     */
    public function getNumempresaId()
    {
        return $this->numempresaId;
    }

    /**
     * Set nummonedaId
     *
     * @param integer $nummonedaId
     * @return Solicitudespagos
     */
    public function setNummonedaId($nummonedaId)
    {
        $this->nummonedaId = $nummonedaId;

        return $this;
    }

    /**
     * Get nummonedaId
     *
     * @return integer 
     */
    public function getNummonedaId()
    {
        return $this->nummonedaId;
    }

    /**
     * Set numejecutivoId
     *
     * @param integer $numejecutivoId
     * @return Solicitudespagos
     */
    public function setNumejecutivoId($numejecutivoId)
    {
        $this->numejecutivoId = $numejecutivoId;

        return $this;
    }

    /**
     * Get numejecutivoId
     *
     * @return integer 
     */
    public function getNumejecutivoId()
    {
        return $this->numejecutivoId;
    }

    /**
     * Set numproveedorId
     *
     * @param integer $numproveedorId
     * @return Solicitudespagos
     */
    public function setNumproveedorId($numproveedorId)
    {
        $this->numproveedorId = $numproveedorId;

        return $this;
    }

    /**
     * Get numproveedorId
     *
     * @return integer 
     */
    public function getNumproveedorId()
    {
        return $this->numproveedorId;
    }

    /**
     * Set numcategoriaId
     *
     * @param integer $numcategoriaId
     * @return Solicitudespagos
     */
    public function setNumcategoriaId($numcategoriaId)
    {
        $this->numcategoriaId = $numcategoriaId;

        return $this;
    }

    /**
     * Get numcategoriaId
     *
     * @return integer 
     */
    public function getNumcategoriaId()
    {
        return $this->numcategoriaId;
    }

    /**
     * Set numautorizadorId
     *
     * @param integer $numautorizadorId
     * @return Solicitudespagos
     */
    public function setNumautorizadorId($numautorizadorId)
    {
        $this->numautorizadorId = $numautorizadorId;

        return $this;
    }

    /**
     * Get numautorizadorId
     *
     * @return integer 
     */
    public function getNumautorizadorId()
    {
        return $this->numautorizadorId;
    }

    /**
     * Set numcreadoporId
     *
     * @param integer $numcreadoporId
     * @return Solicitudespagos
     */
    public function setNumcreadoporId($numcreadoporId)
    {
        $this->numcreadoporId = $numcreadoporId;

        return $this;
    }

    /**
     * Get numcreadoporId
     *
     * @return integer 
     */
    public function getNumcreadoporId()
    {
        return $this->numcreadoporId;
    }

    /**
     * Set dtecreacion
     *
     * @param \DateTime $dtecreacion
     * @return Solicitudespagos
     */
    public function setDtecreacion($dtecreacion)
    {
        $this->dtecreacion = $dtecreacion;

        return $this;
    }

    /**
     * Get dtecreacion
     *
     * @return \DateTime 
     */
    public function getDtecreacion()
    {
        return $this->dtecreacion;
    }

    /**
     * Set numactualizadoporId
     *
     * @param integer $numactualizadoporId
     * @return Solicitudespagos
     */
    public function setNumactualizadoporId($numactualizadoporId)
    {
        $this->numactualizadoporId = $numactualizadoporId;

        return $this;
    }

    /**
     * Get numactualizadoporId
     *
     * @return integer 
     */
    public function getNumactualizadoporId()
    {
        return $this->numactualizadoporId;
    }

    /**
     * Set dteactualizacion
     *
     * @param \DateTime $dteactualizacion
     * @return Solicitudespagos
     */
    public function setDteactualizacion($dteactualizacion)
    {
        $this->dteactualizacion = $dteactualizacion;

        return $this;
    }

    /**
     * Get dteactualizacion
     *
     * @return \DateTime 
     */
    public function getDteactualizacion()
    {
        return $this->dteactualizacion;
    }

    /**
     * Set numtipoestado
     *
     * @param integer $numtipoestado
     * @return Solicitudespagos
     */
    public function setNumtipoestado($numtipoestado)
    {
        $this->numtipoestado = $numtipoestado;

        return $this;
    }

    /**
     * Get numtipoestado
     *
     * @return integer 
     */
    public function getNumtipoestado()
    {
        return $this->numtipoestado;
    }

    /**
     * Set numestadocomunicacion
     *
     * @param integer $numestadocomunicacion
     * @return Solicitudespagos
     */
    public function setNumestadocomunicacion($numestadocomunicacion)
    {
        $this->numestadocomunicacion = $numestadocomunicacion;

        return $this;
    }

    /**
     * Get numestadocomunicacion
     *
     * @return integer 
     */
    public function getNumestadocomunicacion()
    {
        return $this->numestadocomunicacion;
    }
}
