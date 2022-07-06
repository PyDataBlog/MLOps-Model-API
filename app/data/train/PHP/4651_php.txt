<?php


namespace Models;

/**
 * Class Candidate
 * @package Models
 *
 * @Source("candidates")
 *
 */
class Candidate extends \Phalcon\Mvc\Model
{
    /**
     * @Id
     * @Identity
     * @GeneratedValue
     * @Primary
     * @Column(type="integer")
     * @var integer
     */
    public $id;

    /**
     * @Column(column="pib", type="string")
     * @var string
     */
    public $pib;

    /**
     * @Column(column="birthDate", type="timestamp")
     * @var string
     */
    public $birthDate;

    /**
     * @Column(name="maritalStatus", type="enum")
     * @var string
     */
    public $maritalStatus;

    /**
     * @Column(name="cityId", type="integer")
     * @var string
     */
    public $cityId;

    /**
     * @Column(name="educationId", type="integer")
     * @var string
     */
    public $educationId;

    /**
     * @Column(name="phoneMobile", type="string", length=200)
     * @var string
     */
    public $phoneMobile;

    /**
     * @Column(name="phoneHome", type="string")
     * @var string
     */
    public $phoneHome;

    /**
     * @Column(name="email", type="string")
     * @var string
     */
    public $email;

    /**
     * @Column(name="drivingExp", type="string")
     * @var string
     */
    public $drivingExp;

    /**
     * @Column(type="$recommendations", nullable=false, name="group_id", size="11")
     */
    public $recommendations;

    /**
     * @Column(column="changed", type="timestamp")
     * @var string
     */
    public $changed;

    /**
     * @Column(column="created", type="timestamp")
     * @var string
     */
    public $created;


    public function getSource()
    {
        return "candidates";
    }
}
