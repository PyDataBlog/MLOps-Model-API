<?php


namespace FrankSons\UnitTesting\Basic\Contacts;

use FrankSons\UnitTesting\Basic\Validator\EmailValidator;
use FrankSons\UnitTesting\Basic\Validator\ZipCodeValidator;

class Contacts
{
    /**
     * @var EmailValidator
     */
    private $emailValidator;

    /**
     * @var ZipCodeValidator
     */
    private $zipCodeValidator;

    /**
     * @var array
     */
    private $users = [];

    /**
     * Contacts constructor.
     * @param EmailValidator $emailValidator
     * @param ZipCodeValidator $zipCodeValidator
     */
    public function __construct(EmailValidator $emailValidator, ZipCodeValidator $zipCodeValidator)
    {
        $this->emailValidator = $emailValidator;
        $this->zipCodeValidator = $zipCodeValidator;
    }

    /**
     * add email for user id
     * @param int $userId
     * @param string $email
     * @return bool
     */
    public function addEmail($userId, $email)
    {
        if ($this->emailValidator->isValid($email)) {
            $this->users[$userId]['email'] = $email;
            return true;
        }
        return false;
    }

    /**
     * add zip code for user id
     * @param int $userId
     * @param int $zipCode
     * @return bool
     */
    public function addZipCode($userId, $zipCode)
    {
        if ($this->zipCodeValidator->isValid($zipCode)) {
            $this->users[$userId]['zipcode'] = $zipCode;
            return true;
        }
        return false;
    }

    /**
     * fetch email for given user id
     * @param int $userId
     * @return string|null
     */
    public function getUserEmail($userId)
    {
        return empty($this->users[$userId]['email']) ? null : $this->users[$userId]['email'];
    }

    /**
     * fetch zip code for given user id
     * @param int $userId
     * @return string|null
     */
    public function getUserZipCode($userId)
    {
        return empty($this->users[$userId]['zipCode']) ? null : $this->users[$userId]['zipCode'];
    }

    /**
     * get the complete user data for given user id
     * @param int $userId
     * @return array|null
     */
    public function getUserData($userId)
    {
        return empty($this->users[$userId]) ? null : $this->users[$userId];
    }

    /**
     * get all users
     * @param int $userId
     * @return array
     */
    public function getAllUsers($userId)
    {
        return $this->users;
    }
}