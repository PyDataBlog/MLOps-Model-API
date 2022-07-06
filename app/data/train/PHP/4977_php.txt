<?php

namespace Chipin\DBBundle\Entity;

use Doctrine\ORM\Mapping as ORM;

/**
 * Frontuser
 *
 * @ORM\Table(name="frontUser")
 * @ORM\Entity
 * @ORM\HasLifecycleCallbacks()
 */
class Frontuser
{
    /**
     * @var string
     *
     * @ORM\Column(name="id", type="string", length=255, nullable=false)
     * @ORM\Id
     * @ORM\GeneratedValue(strategy="NICE16")
     */
    private $id;

    /**
     * @var string
     *
     * @ORM\Column(name="email", type="string", length=255, nullable=false)
     */
    private $email;

    /**
     * @var string
     *
     * @ORM\Column(name="forename", type="string", length=255, nullable=true)
     */
    private $forename;

    /**
     * @var string
     *
     * @ORM\Column(name="surname", type="string", length=255, nullable=true)
     */
    private $surname;

    /**
     * @var string
     *
     * @ORM\Column(name="nickname", type="string", length=255, nullable=true)
     */
    private $nickname;

    /**
     * @var string
     *
     * @ORM\Column(name="password", type="string", length=32, nullable=true)
     */
    private $password;

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="dateCreated", type="datetime", nullable=false)
     */
    private $datecreated;

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="dateEdited", type="datetime", nullable=false)
     */
    private $dateedited;

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="dateRegistered", type="datetime", nullable=true)
     */
    private $dateregistered;

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="dateActivate", type="datetime", nullable=true)
     */
    private $dateactivate;

    /**
     * @var \DateTime
     *
     * @ORM\Column(name="dateDeleted", type="datetime", nullable=true)
     */
    private $datedeleted;

    /**
     * @var \Usersstatus
     *
     * @ORM\ManyToOne(targetEntity="Usersstatus")
     * @ORM\JoinColumns({
     *   @ORM\JoinColumn(name="usersStatusID", referencedColumnName="id")
     * })
     */
    private $usersstatusid;





    /**
     * @ORM\PrePersist
     */

    public function setInitData() {

        $currentDate = new \DateTime();
        $this->setDatecreated($currentDate);
        $this->setDateedited($currentDate);
            
        // @ORM\HasLifecycleCallbacks()
    }

    /**
     * Get id
     *
     * @return string 
     */
    public function getId()
    {
        return $this->id;
    }

    /**
     * Set email
     *
     * @param string $email
     * @return Frontuser
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
     * Set forename
     *
     * @param string $forename
     * @return Frontuser
     */
    public function setForename($forename)
    {
        $this->forename = $forename;
    
        return $this;
    }

    /**
     * Get forename
     *
     * @return string 
     */
    public function getForename()
    {
        return $this->forename;
    }

    /**
     * Set surname
     *
     * @param string $surname
     * @return Frontuser
     */
    public function setSurname($surname)
    {
        $this->surname = $surname;
    
        return $this;
    }

    /**
     * Get surname
     *
     * @return string 
     */
    public function getSurname()
    {
        return $this->surname;
    }

    /**
     * Set nickname
     *
     * @param string $nickname
     * @return Frontuser
     */
    public function setNickname($nickname)
    {
        $this->nickname = $nickname;
    
        return $this;
    }

    /**
     * Get nickname
     *
     * @return string 
     */
    public function getNickname()
    {
        return $this->nickname;
    }

    /**
     * Set password
     *
     * @param string $password
     * @return Frontuser
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
     * Set datecreated
     *
     * @param \DateTime $datecreated
     * @return Frontuser
     */
    public function setDatecreated($datecreated)
    {
        $this->datecreated = $datecreated;
    
        return $this;
    }

    /**
     * Get datecreated
     *
     * @return \DateTime 
     */
    public function getDatecreated()
    {
        return $this->datecreated;
    }

    /**
     * Set dateedited
     *
     * @param \DateTime $dateedited
     * @return Frontuser
     */
    public function setDateedited($dateedited)
    {
        $this->dateedited = $dateedited;
    
        return $this;
    }

    /**
     * Get dateedited
     *
     * @return \DateTime 
     */
    public function getDateedited()
    {
        return $this->dateedited;
    }

    /**
     * Set dateregistered
     *
     * @param \DateTime $dateregistered
     * @return Frontuser
     */
    public function setDateregistered($dateregistered)
    {
        $this->dateregistered = $dateregistered;
    
        return $this;
    }

    /**
     * Get dateregistered
     *
     * @return \DateTime 
     */
    public function getDateregistered()
    {
        return $this->dateregistered;
    }

    /**
     * Set dateactivate
     *
     * @param \DateTime $dateactivate
     * @return Frontuser
     */
    public function setDateactivate($dateactivate)
    {
        $this->dateactivate = $dateactivate;
    
        return $this;
    }

    /**
     * Get dateactivate
     *
     * @return \DateTime 
     */
    public function getDateactivate()
    {
        return $this->dateactivate;
    }

    /**
     * Set datedeleted
     *
     * @param \DateTime $datedeleted
     * @return Frontuser
     */
    public function setDatedeleted($datedeleted)
    {
        $this->datedeleted = $datedeleted;
    
        return $this;
    }

    /**
     * Get datedeleted
     *
     * @return \DateTime 
     */
    public function getDatedeleted()
    {
        return $this->datedeleted;
    }

    /**
     * Set usersstatusid
     *
     * @param \Chipin\DBBundle\Entity\Usersstatus $usersstatusid
     * @return Frontuser
     */
    public function setUsersstatusid(\Chipin\DBBundle\Entity\Usersstatus $usersstatusid = null)
    {
        $this->usersstatusid = $usersstatusid;
    
        return $this;
    }

    /**
     * Get usersstatusid
     *
     * @return \Chipin\DBBundle\Entity\Usersstatus 
     */
    public function getUsersstatusid()
    {
        return $this->usersstatusid;
    }
}