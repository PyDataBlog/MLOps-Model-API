<?php
namespace Tollwerk\TwPrototype\Tests\Unit\Controller;

    /***************************************************************
     *  Copyright notice
     *
     *  (c) 2016 Joschi Kuphal <joschi@tollwerk.de>, tollwerk GmbH
     *
     *  All rights reserved
     *
     *  This script is part of the TYPO3 project. The TYPO3 project is
     *  free software; you can redistribute it and/or modify
     *  it under the terms of the GNU General Public License as published by
     *  the Free Software Foundation; either version 2 of the License, or
     *  (at your option) any later version.
     *
     *  The GNU General Public License can be found at
     *  http://www.gnu.org/copyleft/gpl.html.
     *
     *  This script is distributed in the hope that it will be useful,
     *  but WITHOUT ANY WARRANTY; without even the implied warranty of
     *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     *  GNU General Public License for more details.
     *
     *  This copyright notice MUST APPEAR in all copies of the script!
     ***************************************************************/

/**
 * Test case for class Tollwerk\TwPrototype\Controller\PersonController.
 *
 * @author Joschi Kuphal <joschi@tollwerk.de>
 */
class PersonControllerTest extends \TYPO3\CMS\Core\Tests\UnitTestCase
{
    /**
     * Person controller instance
     *
     * @var \Tollwerk\TwPrototype\Controller\PersonController
     */
    protected $subject = null;

    /**
     * Set up
     */
    public function setUp()
    {
        $this->subject = $this->getMockBuilder('Tollwerk\\TwPrototype\\Controller\\PersonController')
            ->disableOriginalConstructor()
            ->disableArgumentCloning()
            ->disallowMockingUnknownTypes()
            ->setMethods(array('redirect', 'forward', 'addFlashMessage'))->disableOriginalClone()
            ->getMock();
    }

    /**
     * Tear down
     */
    public function tearDown()
    {
        unset($this->subject);
    }

    /**
     * Test whether the list action fetches all persons from the repository and assigns them to the view
     *
     * @test
     */
    public function listActionFetchesAllPersonsFromRepositoryAndAssignsThemToView()
    {
        $allPersons = $this->createMock('Tollwerk\\TwPrototype\\Controller\\PersonController');

        $personRepository = $this->getMockBuilder('Tollwerk\\TwPrototype\\Domain\\Repository\\PersonRepository')
            ->disableOriginalConstructor()
            ->disableArgumentCloning()
            ->disallowMockingUnknownTypes()
            ->setMethods(array('findAll'))->disableOriginalClone()
            ->getMock();
        $personRepository->expects($this->once())->method('findAll')->will($this->returnValue($allPersons));
        $this->inject($this->subject, 'personRepository', $personRepository);

        $view = $this->createMock('TYPO3\\CMS\\Extbase\\Mvc\\View\\ViewInterface');
        $view->expects($this->once())->method('assign')->with('persons', $allPersons);
        $this->inject($this->subject, 'view', $view);

        $this->subject->listAction();
    }

    /**
     * Test whether the show action assigns the given person to the view
     *
     * @test
     */
    public function showActionAssignsTheGivenPersonToView()
    {
        $person = new \Tollwerk\TwPrototype\Domain\Model\Person();

        $view = $this->createMock('TYPO3\\CMS\\Extbase\\Mvc\\View\\ViewInterface');
        $this->inject($this->subject, 'view', $view);
        $view->expects($this->once())->method('assign')->with('person', $person);

        $this->subject->showAction($person);
    }
}
