<?php

/**
 * Copyright (c) 2016-present Ganbaro Digital Ltd
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 *
 *   * Neither the names of the copyright holders nor the names of his
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * @category  Libraries
 * @package   HttpStatus/StatusProviders
 * @author    Stuart Herbert <stuherbert@ganbarodigital.com>
 * @copyright 2016-present Ganbaro Digital Ltd www.ganbarodigital.com
 * @license   http://www.opensource.org/licenses/bsd-license.php  BSD License
 * @link      http://ganbarodigital.github.io/php-http-status
 */

namespace GanbaroDigitalTest\HttpStatus\StatusProviders\RequestError;

use GanbaroDigital\HttpStatus\Interfaces\HttpStatusProvider;
use GanbaroDigital\HttpStatus\StatusProviders\RequestError\UnsupportedMediaTypeStatusProvider;
use GanbaroDigital\HttpStatus\StatusValues\RequestError\UnsupportedMediaTypeStatus;
use PHPUnit_Framework_TestCase;

/**
 * @coversDefaultClass GanbaroDigital\HttpStatus\StatusProviders\RequestError\UnsupportedMediaTypeStatusProvider
 */
class UnsupportedMediaTypeStatusProviderTest extends PHPUnit_Framework_TestCase
{
    /**
     * @coversNothing
     */
    public function testCanInstantiateClassThatUsesThisTrait()
    {
        // ----------------------------------------------------------------
        // setup your test

        // ----------------------------------------------------------------
        // perform the change

        $unit = new UnsupportedMediaTypeStatusProviderTestHelper;

        // ----------------------------------------------------------------
        // test the results

        // make sure the class could instantiate
        $this->assertInstanceOf(UnsupportedMediaTypeStatusProviderTestHelper::class, $unit);

        // make sure our test helper does use the trait we're trying to test
        $traits = class_uses($unit);
        $this->assertArrayHasKey(UnsupportedMediaTypeStatusProvider::class, $traits);
    }

    /**
     * @covers ::getHttpStatus
     */
    public function testReturnsUnsupportedMediaTypeStatus()
    {
        // ----------------------------------------------------------------
        // setup your test

        $expectedType = UnsupportedMediaTypeStatus::class;
        $unit = new UnsupportedMediaTypeStatusProviderTestHelper;

        // ----------------------------------------------------------------
        // perform the change

        $actualType = $unit->getHttpStatus();

        // ----------------------------------------------------------------
        // test the results

        $this->assertInstanceOf($expectedType, $actualType);
    }
}

class UnsupportedMediaTypeStatusProviderTestHelper
{
    use UnsupportedMediaTypeStatusProvider;
}
