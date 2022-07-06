<?php

namespace MPM\EntityBundle\Entity;

use Doctrine\ORM\Mapping as ORM;

/**
 * Persona
 *
 * @ORM\Table(name="personas")
 * @ORM\Entity(repositoryClass="MPM\EntityBundle\Entity\PersonaRepository")
 * @ORM\InheritanceType("JOINED")
 * @ORM\DiscriminatorColumn(name="discr", type="string")
 * @ORM\DiscriminatorMap({"persona" = "Persona", "alumno" = "Alumno", "profesor" = "Profesor", "secretario" = "Secretario", "tutor" = "Tutor"})
 */
class Persona
{
    /**
     * @var integer
     *
     * @ORM\Column(name="id", type="integer")
     * @ORM\Id
     * @ORM\GeneratedValue(strategy="AUTO")
     */
    protected $id;

    /**
     * @var string
     *
     * @ORM\Column(name="nombre", type="string", length=255)
     */
    protected $nombre;

    /**
     * @var string
     *
     * @ORM\Column(name="apellido", type="string", length=255)
     */
    protected $apellido;

    /**
     * @var string
     *
     * @ORM\Column(name="tipoDocumento", type="string", length=255)
     */
    protected $tipoDocumento;

    /**
     * @var string
     *
     * @ORM\Column(name="numeroDocumento", type="string", length=255)
     */
    protected $numeroDocumento;

    /**
     * @var boolean
     *
     * @ORM\Column(name="sexo", type="boolean")
     */
    protected $sexo;

    /**
     * @ORM\ManyToMany(targetEntity="Telefono", inversedBy="personas")
     * @ORM\JoinTable(name="personas_telefonos")
     **/
    protected $telefonos;

    /**
     * @ORM\ManyToMany(targetEntity="Domicilio", inversedBy="personas")
     * @ORM\JoinTable(name="personas_domicilios")
     **/
    protected $domicilios;

    /**
     * @var string
     *
     * @ORM\Column(name="email", type="string", length=255)
     */
    protected $email;

    /**
     * @var string
     *
     * @ORM\Column(name="password", type="string", length=255)
     */
    protected $password;

    /**
     * @var string
     *
     * @ORM\Column(name="salt", type="string", length=255)
     */
    protected $salt;

    /**
     * @ORM\ManyToMany(targetEntity="Role", mappedBy="personas")
     **/
    protected $roles;

    /**
     * @ORM\ManyToMany(targetEntity="Permiso", mappedBy="personas")
     **/
    protected $permisos;

    /**
     * @ORM\ManyToMany(targetEntity="Grupo", mappedBy="personas")
     **/
    protected $grupos;

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="lastLogin", type="datetime")
     */
    protected $lastLogin;

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="sysLoad", type="datetime")
     */
    protected $sysLoad;

    /**
     * @var string
     *
     * @ORM\Column(name="confirmationToken", type="string", length=255)
     */
    protected $confirmationToken;

    /**
     * @var boolean
     *
     * @ORM\Column(name="enable", type="boolean")
     */
    protected $enable;

    /**
     * @var boolean
     *
     * @ORM\Column(name="locked", type="boolean")
     */
    protected $locked;

    /**
     * @var boolean
     *
     * @ORM\Column(name="expired", type="boolean")
     */
    protected $expired;

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="expiredAt", type="datetime")
     */
    protected $expiredAt;

    /**
     * @var string
     *
     * @ORM\Column(name="credentials", type="string", length=255)
     */
    protected $credentials;

    /**
     * @var boolean
     *
     * @ORM\Column(name="credentialsExpired", type="boolean")
     */
    protected $credentialsExpired;

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="credentialsExpiredAt", type="datetime")
     */
    protected $credentialsExpiredAt;

    /**
     * @var string
     *
     * @ORM\Column(name="image", type="blob")
     */
    protected $image;

    /**
     * @var string
     *
     * @ORM\Column(name="imageMime", type="string", length=255)
     */
    protected $imageMime;

    /**
     * @ORM\ManyToMany(targetEntity="Conversacion", mappedBy="personas")
     **/
    protected $conversaciones;

    /**
     * @ORM\ManyToMany(targetEntity="Evento", mappedBy="creador")
     **/
    protected $eventos;

    /**
     * @ORM\ManyToMany(targetEntity="Tarea", mappedBy="shareList")
     **/
    protected $tareasCompartidas;
    /**
     * Constructor
     */
    public function __construct()
    {
        $this->telefonos = new \Doctrine\Common\Collections\ArrayCollection();
        $this->domicilios = new \Doctrine\Common\Collections\ArrayCollection();
        $this->roles = new \Doctrine\Common\Collections\ArrayCollection();
        $this->permisos = new \Doctrine\Common\Collections\ArrayCollection();
        $this->grupos = new \Doctrine\Common\Collections\ArrayCollection();
        $this->conversaciones = new \Doctrine\Common\Collections\ArrayCollection();
        $this->eventos = new \Doctrine\Common\Collections\ArrayCollection();
        $this->tareasCompartidas = new \Doctrine\Common\Collections\ArrayCollection();
    }

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
     * Set nombre
     *
     * @param string $nombre
     * @return Persona
     */
    public function setNombre($nombre)
    {
        $this->nombre = $nombre;

        return $this;
    }

    /**
     * Get nombre
     *
     * @return string 
     */
    public function getNombre()
    {
        return $this->nombre;
    }

    /**
     * Set apellido
     *
     * @param string $apellido
     * @return Persona
     */
    public function setApellido($apellido)
    {
        $this->apellido = $apellido;

        return $this;
    }

    /**
     * Get apellido
     *
     * @return string 
     */
    public function getApellido()
    {
        return $this->apellido;
    }

    /**
     * Set tipoDocumento
     *
     * @param string $tipoDocumento
     * @return Persona
     */
    public function setTipoDocumento($tipoDocumento)
    {
        $this->tipoDocumento = $tipoDocumento;

        return $this;
    }

    /**
     * Get tipoDocumento
     *
     * @return string 
     */
    public function getTipoDocumento()
    {
        return $this->tipoDocumento;
    }

    /**
     * Set numeroDocumento
     *
     * @param string $numeroDocumento
     * @return Persona
     */
    public function setNumeroDocumento($numeroDocumento)
    {
        $this->numeroDocumento = $numeroDocumento;

        return $this;
    }

    /**
     * Get numeroDocumento
     *
     * @return string 
     */
    public function getNumeroDocumento()
    {
        return $this->numeroDocumento;
    }

    /**
     * Set sexo
     *
     * @param boolean $sexo
     * @return Persona
     */
    public function setSexo($sexo)
    {
        $this->sexo = $sexo;

        return $this;
    }

    /**
     * Get sexo
     *
     * @return boolean 
     */
    public function getSexo()
    {
        return $this->sexo;
    }

    /**
     * Set email
     *
     * @param string $email
     * @return Persona
     */
    public function setEmail($email)
    {
        $this->email = $email;

        return $this;
    }

    /**
     * Get email
     *
     * @return string 
     */
    public function getEmail()
    {
        return $this->email;
    }

    /**
     * Set password
     *
     * @param string $password
     * @return Persona
     */
    public function setPassword($password)
    {
        $this->password = $password;

        return $this;
    }

    /**
     * Get password
     *
     * @return string 
     */
    public function getPassword()
    {
        return $this->password;
    }

    /**
     * Set salt
     *
     * @param string $salt
     * @return Persona
     */
    public function setSalt($salt)
    {
        $this->salt = $salt;

        return $this;
    }

    /**
     * Get salt
     *
     * @return string 
     */
    public function getSalt()
    {
        return $this->salt;
    }

    /**
     * Set lastLogin
     *
     * @param \DateTime $lastLogin
     * @return Persona
     */
    public function setLastLogin($lastLogin)
    {
        $this->lastLogin = $lastLogin;

        return $this;
    }

    /**
     * Get lastLogin
     *
     * @return \DateTime 
     */
    public function getLastLogin()
    {
        return $this->lastLogin;
    }

    /**
     * Set sysLoad
     *
     * @param \DateTime $sysLoad
     * @return Persona
     */
    public function setSysLoad($sysLoad)
    {
        $this->sysLoad = $sysLoad;

        return $this;
    }

    /**
     * Get sysLoad
     *
     * @return \DateTime 
     */
    public function getSysLoad()
    {
        return $this->sysLoad;
    }

    /**
     * Set confirmationToken
     *
     * @param string $confirmationToken
     * @return Persona
     */
    public function setConfirmationToken($confirmationToken)
    {
        $this->confirmationToken = $confirmationToken;

        return $this;
    }

    /**
     * Get confirmationToken
     *
     * @return string 
     */
    public function getConfirmationToken()
    {
        return $this->confirmationToken;
    }

    /**
     * Set enable
     *
     * @param boolean $enable
     * @return Persona
     */
    public function setEnable($enable)
    {
        $this->enable = $enable;

        return $this;
    }

    /**
     * Get enable
     *
     * @return boolean 
     */
    public function getEnable()
    {
        return $this->enable;
    }

    /**
     * Set locked
     *
     * @param boolean $locked
     * @return Persona
     */
    public function setLocked($locked)
    {
        $this->locked = $locked;

        return $this;
    }

    /**
     * Get locked
     *
     * @return boolean 
     */
    public function getLocked()
    {
        return $this->locked;
    }

    /**
     * Set expired
     *
     * @param boolean $expired
     * @return Persona
     */
    public function setExpired($expired)
    {
        $this->expired = $expired;

        return $this;
    }

    /**
     * Get expired
     *
     * @return boolean 
     */
    public function getExpired()
    {
        return $this->expired;
    }

    /**
     * Set expiredAt
     *
     * @param \DateTime $expiredAt
     * @return Persona
     */
    public function setExpiredAt($expiredAt)
    {
        $this->expiredAt = $expiredAt;

        return $this;
    }

    /**
     * Get expiredAt
     *
     * @return \DateTime 
     */
    public function getExpiredAt()
    {
        return $this->expiredAt;
    }

    /**
     * Set credentials
     *
     * @param string $credentials
     * @return Persona
     */
    public function setCredentials($credentials)
    {
        $this->credentials = $credentials;

        return $this;
    }

    /**
     * Get credentials
     *
     * @return string 
     */
    public function getCredentials()
    {
        return $this->credentials;
    }

    /**
     * Set credentialsExpired
     *
     * @param boolean $credentialsExpired
     * @return Persona
     */
    public function setCredentialsExpired($credentialsExpired)
    {
        $this->credentialsExpired = $credentialsExpired;

        return $this;
    }

    /**
     * Get credentialsExpired
     *
     * @return boolean 
     */
    public function getCredentialsExpired()
    {
        return $this->credentialsExpired;
    }

    /**
     * Set credentialsExpiredAt
     *
     * @param \DateTime $credentialsExpiredAt
     * @return Persona
     */
    public function setCredentialsExpiredAt($credentialsExpiredAt)
    {
        $this->credentialsExpiredAt = $credentialsExpiredAt;

        return $this;
    }

    /**
     * Get credentialsExpiredAt
     *
     * @return \DateTime 
     */
    public function getCredentialsExpiredAt()
    {
        return $this->credentialsExpiredAt;
    }

    /**
     * Set image
     *
     * @param string $image
     * @return Persona
     */
    public function setImage($image)
    {
        $this->image = $image;

        return $this;
    }

    /**
     * Get image
     *
     * @return string 
     */
    public function getImage()
    {
        return $this->image;
    }

    /**
     * Set imageMime
     *
     * @param string $imageMime
     * @return Persona
     */
    public function setImageMime($imageMime)
    {
        $this->imageMime = $imageMime;

        return $this;
    }

    /**
     * Get imageMime
     *
     * @return string 
     */
    public function getImageMime()
    {
        return $this->imageMime;
    }

    /**
     * Add telefonos
     *
     * @param \MPM\EntityBundle\Entity\Telefono $telefonos
     * @return Persona
     */
    public function addTelefono(\MPM\EntityBundle\Entity\Telefono $telefonos)
    {
        $this->telefonos[] = $telefonos;

        return $this;
    }

    /**
     * Remove telefonos
     *
     * @param \MPM\EntityBundle\Entity\Telefono $telefonos
     */
    public function removeTelefono(\MPM\EntityBundle\Entity\Telefono $telefonos)
    {
        $this->telefonos->removeElement($telefonos);
    }

    /**
     * Get telefonos
     *
     * @return \Doctrine\Common\Collections\Collection 
     */
    public function getTelefonos()
    {
        return $this->telefonos;
    }

    /**
     * Add domicilios
     *
     * @param \MPM\EntityBundle\Entity\Domicilio $domicilios
     * @return Persona
     */
    public function addDomicilio(\MPM\EntityBundle\Entity\Domicilio $domicilios)
    {
        $this->domicilios[] = $domicilios;

        return $this;
    }

    /**
     * Remove domicilios
     *
     * @param \MPM\EntityBundle\Entity\Domicilio $domicilios
     */
    public function removeDomicilio(\MPM\EntityBundle\Entity\Domicilio $domicilios)
    {
        $this->domicilios->removeElement($domicilios);
    }

    /**
     * Get domicilios
     *
     * @return \Doctrine\Common\Collections\Collection 
     */
    public function getDomicilios()
    {
        return $this->domicilios;
    }

    /**
     * Add roles
     *
     * @param \MPM\EntityBundle\Entity\Role $roles
     * @return Persona
     */
    public function addRole(\MPM\EntityBundle\Entity\Role $roles)
    {
        $this->roles[] = $roles;

        return $this;
    }

    /**
     * Remove roles
     *
     * @param \MPM\EntityBundle\Entity\Role $roles
     */
    public function removeRole(\MPM\EntityBundle\Entity\Role $roles)
    {
        $this->roles->removeElement($roles);
    }

    /**
     * Get roles
     *
     * @return \Doctrine\Common\Collections\Collection 
     */
    public function getRoles()
    {
        return $this->roles;
    }

    /**
     * Add permisos
     *
     * @param \MPM\EntityBundle\Entity\Permiso $permisos
     * @return Persona
     */
    public function addPermiso(\MPM\EntityBundle\Entity\Permiso $permisos)
    {
        $this->permisos[] = $permisos;

        return $this;
    }

    /**
     * Remove permisos
     *
     * @param \MPM\EntityBundle\Entity\Permiso $permisos
     */
    public function removePermiso(\MPM\EntityBundle\Entity\Permiso $permisos)
    {
        $this->permisos->removeElement($permisos);
    }

    /**
     * Get permisos
     *
     * @return \Doctrine\Common\Collections\Collection 
     */
    public function getPermisos()
    {
        return $this->permisos;
    }

    /**
     * Add grupos
     *
     * @param \MPM\EntityBundle\Entity\Grupo $grupos
     * @return Persona
     */
    public function addGrupo(\MPM\EntityBundle\Entity\Grupo $grupos)
    {
        $this->grupos[] = $grupos;

        return $this;
    }

    /**
     * Remove grupos
     *
     * @param \MPM\EntityBundle\Entity\Grupo $grupos
     */
    public function removeGrupo(\MPM\EntityBundle\Entity\Grupo $grupos)
    {
        $this->grupos->removeElement($grupos);
    }

    /**
     * Get grupos
     *
     * @return \Doctrine\Common\Collections\Collection 
     */
    public function getGrupos()
    {
        return $this->grupos;
    }

    /**
     * Add conversaciones
     *
     * @param \MPM\EntityBundle\Entity\Conversacion $conversaciones
     * @return Persona
     */
    public function addConversacione(\MPM\EntityBundle\Entity\Conversacion $conversaciones)
    {
        $this->conversaciones[] = $conversaciones;

        return $this;
    }

    /**
     * Remove conversaciones
     *
     * @param \MPM\EntityBundle\Entity\Conversacion $conversaciones
     */
    public function removeConversacione(\MPM\EntityBundle\Entity\Conversacion $conversaciones)
    {
        $this->conversaciones->removeElement($conversaciones);
    }

    /**
     * Get conversaciones
     *
     * @return \Doctrine\Common\Collections\Collection 
     */
    public function getConversaciones()
    {
        return $this->conversaciones;
    }

    /**
     * Add eventos
     *
     * @param \MPM\EntityBundle\Entity\Evento $eventos
     * @return Persona
     */
    public function addEvento(\MPM\EntityBundle\Entity\Evento $eventos)
    {
        $this->eventos[] = $eventos;

        return $this;
    }

    /**
     * Remove eventos
     *
     * @param \MPM\EntityBundle\Entity\Evento $eventos
     */
    public function removeEvento(\MPM\EntityBundle\Entity\Evento $eventos)
    {
        $this->eventos->removeElement($eventos);
    }

    /**
     * Get eventos
     *
     * @return \Doctrine\Common\Collections\Collection 
     */
    public function getEventos()
    {
        return $this->eventos;
    }

    /**
     * Add tareasCompartidas
     *
     * @param \MPM\EntityBundle\Entity\Tarea $tareasCompartidas
     * @return Persona
     */
    public function addTareasCompartida(\MPM\EntityBundle\Entity\Tarea $tareasCompartidas)
    {
        $this->tareasCompartidas[] = $tareasCompartidas;

        return $this;
    }

    /**
     * Remove tareasCompartidas
     *
     * @param \MPM\EntityBundle\Entity\Tarea $tareasCompartidas
     */
    public function removeTareasCompartida(\MPM\EntityBundle\Entity\Tarea $tareasCompartidas)
    {
        $this->tareasCompartidas->removeElement($tareasCompartidas);
    }

    /**
     * Get tareasCompartidas
     *
     * @return \Doctrine\Common\Collections\Collection 
     */
    public function getTareasCompartidas()
    {
        return $this->tareasCompartidas;
    }
}
