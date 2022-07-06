<?php
namespace Jac\ContactBundle\Entity;


class Contact
{
    protected $address;
    protected $comments;
    protected $companyName;
    protected $emailAddress;
    protected $faxNumber;
    protected $fullName;
    protected $phoneNumber;
    protected $position;
    protected $whereDidYouLearnAboutUs;


    /*  Class getters  */
    public function getAddress()
    {
        return $this->address;
    }


    public function getComments()
    {
        return $this->comments;
    }


    public function getCompanyName()
    {
        return $this->companyName;
    }


    public function getEmailAddress()
    {
        return $this->emailAddress;
    }


    public function getFaxNumber()
    {
        return $this->faxNumber;
    }


    public function getFullName()
    {
        return $this->fullName;
    }


    public function getPhoneNumber()
    {
        return $this->phoneNumber;
    }


    public function getPosition()
    {
        return $this->position;
    }


    public function getWhereDidYouLearnAboutUs()
    {
        return $this->whereDidYouLearnAboutUs;
    }


    /* Class setters */
    public function setAddress($address)
    {
        $this->address = $address;
    }


    public function setComments($comments)
    {
        $this->comments = $comments;
    }


    public function setCompanyName($company)
    {
        $this->companyName = $company;
    }


    public function setEmailAddress($address)
    {
        $this->emailAddress = $address;
    }


    public function setFaxNumber($number)
    {
        $this->faxNumber = $number;
    }


    public function setFullName($name)
    {
        $this->fullName = $name;
    }


    public function setPhoneNumber($number)
    {
        $this->phoneNumber = $number;
    }


    public function setPosition($position)
    {
        $this->position = $position;
    }


    public function setWhereDidYouLearnAboutUs($where)
    {
        $this->whereDidYouLearnAboutUs = $where;
    }
}