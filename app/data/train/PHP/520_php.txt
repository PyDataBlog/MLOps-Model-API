<?php

namespace Torchbox\Thankq\Api;

class doContactInsert
{

    /**
     * @var esitWSdoContactInsertArgument $doContactInsertArgument
     */
    protected $doContactInsertArgument = null;

    /**
     * @param esitWSdoContactInsertArgument $doContactInsertArgument
     */
    public function __construct($doContactInsertArgument)
    {
      $this->doContactInsertArgument = $doContactInsertArgument;
    }

    /**
     * @return esitWSdoContactInsertArgument
     */
    public function getDoContactInsertArgument()
    {
      return $this->doContactInsertArgument;
    }

    /**
     * @param esitWSdoContactInsertArgument $doContactInsertArgument
     * @return \Torchbox\Thankq\Api\doContactInsert
     */
    public function setDoContactInsertArgument($doContactInsertArgument)
    {
      $this->doContactInsertArgument = $doContactInsertArgument;
      return $this;
    }

}
