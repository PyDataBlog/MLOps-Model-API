<?php
/**
 * Shows how currency type hints
 */

use Currency\AbstractCurrency as Currency;
use Currency\Eur;
use Currency\Usd;

require '../vendor/autoload.php';

echo PHP_EOL;

class Example
{
    public function workWithCurrency(Currency $c)
    {
        echo $c, PHP_EOL;
        var_dump($c);
    }

    public function workOnlyWithUsd(Usd $usd)
    {
        echo $usd, PHP_EOL;
        var_dump($usd);
    }
}

$eur = new Eur('2');
$usd = new Usd('300');

$example = new Example();
$example->workWithCurrency($eur);
$example->workOnlyWithUsd($usd);

// This would rise fatal error
//$example->workOnlyWithUsd($eur);