<?php
namespace XeroPHP\Reports;

class AgedReceivablesByContact
{
    /**
     * @var string[]
     */
    private $headers;

    /**
     * @var mixed[]
     */
    private $rows;

    /**
     * @param string[] $headers
     * @param mixed[] $rows
     */
    public function __construct(array $headers, array $rows = [])
    {
        $this->headers = $headers;
        $this->rows = $rows;
    }

    /**
     * @return string[]
     */
    public function getHeaders()
    {
        return $this->headers;
    }

    /**
     * @return mixed[]
     */
    public function getRows()
    {
        return $this->rows;
    }
}
