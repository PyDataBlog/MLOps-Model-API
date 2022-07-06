<?php

/**
 * Class FunctionStrncasecmpTest
 * @link http://php.net/manual/en/function.strncasecmp.php
 */
class FunctionStrncasecmpTest extends PHPUnit_Framework_TestCase
{
    public function testFunctionStrncasecmp()
    {
        $this->assertEquals(0, strncasecmp("Hello World", "hello world", 10));
    }
}
