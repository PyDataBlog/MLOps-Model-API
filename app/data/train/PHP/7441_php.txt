<?php

namespace APManagerBundle\Tests\Controller;

use Symfony\Bundle\FrameworkBundle\Test\WebTestCase;

class AdminControllerTest extends WebTestCase
{
    public function testAdminmainpage()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/admin');
    }

    public function testShowallusers()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/showAllUsers');
    }

    public function testCreateuser()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/createUser');
    }

    public function testDeleteuser()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/deleteUser');
    }

    public function testEdituser()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/editUser');
    }

    public function testAddcredentials()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/addCredentials');
    }

    public function testShowallproducts()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/showAllProducts');
    }

    public function testAddproduct()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/addProduct');
    }

    public function testDeleteproduct()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/deleteProduct');
    }

    public function testEditproduct()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/editProduct');
    }

    public function testShowallcycles()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/showAllCycles');
    }

    public function testAddcycle()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/addCycle');
    }

    public function testDeletecycle()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/deleteCycle');
    }

    public function testEditcycle()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/editCycle');
    }

    public function testShowbudgets()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/showBudgets');
    }

    public function testAddbudget()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/addBudget');
    }

    public function testDeletebudget()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/deleteBudget');
    }

    public function testEditbudget()
    {
        $client = static::createClient();

        $crawler = $client->request('GET', '/editBudget');
    }

}
