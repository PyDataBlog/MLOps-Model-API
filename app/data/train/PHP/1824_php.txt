<?php

namespace edu\wisc\services\caos;

class GetCourseByCatalogNumberRequest
{

    /**
     * @var termCodeType $termCode
     */
    protected $termCode = null;

    /**
     * @var string $subjectCode
     */
    protected $subjectCode = null;

    /**
     * @var string $catalogNumber
     */
    protected $catalogNumber = null;

    /**
     * @param termCodeType $termCode
     * @param string $subjectCode
     * @param string $catalogNumber
     */
    public function __construct($termCode, $subjectCode, $catalogNumber)
    {
      $this->termCode = $termCode;
      $this->subjectCode = $subjectCode;
      $this->catalogNumber = $catalogNumber;
    }

    /**
     * @return termCodeType
     */
    public function getTermCode()
    {
      return $this->termCode;
    }

    /**
     * @param termCodeType $termCode
     * @return \edu\wisc\services\caos\GetCourseByCatalogNumberRequest
     */
    public function setTermCode($termCode)
    {
      $this->termCode = $termCode;
      return $this;
    }

    /**
     * @return string
     */
    public function getSubjectCode()
    {
      return $this->subjectCode;
    }

    /**
     * @param string $subjectCode
     * @return \edu\wisc\services\caos\GetCourseByCatalogNumberRequest
     */
    public function setSubjectCode($subjectCode)
    {
      $this->subjectCode = $subjectCode;
      return $this;
    }

    /**
     * @return string
     */
    public function getCatalogNumber()
    {
      return $this->catalogNumber;
    }

    /**
     * @param string $catalogNumber
     * @return \edu\wisc\services\caos\GetCourseByCatalogNumberRequest
     */
    public function setCatalogNumber($catalogNumber)
    {
      $this->catalogNumber = $catalogNumber;
      return $this;
    }

}
