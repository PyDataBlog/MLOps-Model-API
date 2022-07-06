<?php

namespace Nutricion\EmpresasBundle\Entity;

use Doctrine\ORM\Mapping as ORM;

/**
 * PermisosRole
 *
 * @ORM\Table(name="permisos_role", uniqueConstraints={@ORM\UniqueConstraint(name="role", columns={"role", "permiso"})})
 * @ORM\Entity
 */
class PermisosRole
{
    /**
     * @var integer
     *
     * @ORM\Column(name="id", type="integer", nullable=false)
     * @ORM\Id
     * @ORM\GeneratedValue(strategy="IDENTITY")
     */
    private $id;

    /**
     * @var integer
     *
     * @ORM\Column(name="role", type="integer", nullable=false)
     */
    private $role;

    /**
     * @var integer
     *
     * @ORM\Column(name="permiso", type="integer", nullable=false)
     */
    private $permiso;

    /**
     * @var boolean
     *
     * @ORM\Column(name="valor", type="boolean", nullable=false)
     */
    private $valor;



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
     * Set role
     *
     * @param integer $role
     * @return PermisosRole
     */
    public function setRole($role)
    {
        $this->role = $role;

        return $this;
    }

    /**
     * Get role
     *
     * @return integer 
     */
    public function getRole()
    {
        return $this->role;
    }

    /**
     * Set permiso
     *
     * @param integer $permiso
     * @return PermisosRole
     */
    public function setPermiso($permiso)
    {
        $this->permiso = $permiso;

        return $this;
    }

    /**
     * Get permiso
     *
     * @return integer 
     */
    public function getPermiso()
    {
        return $this->permiso;
    }

    /**
     * Set valor
     *
     * @param boolean $valor
     * @return PermisosRole
     */
    public function setValor($valor)
    {
        $this->valor = $valor;

        return $this;
    }

    /**
     * Get valor
     *
     * @return boolean 
     */
    public function getValor()
    {
        return $this->valor;
    }
}
