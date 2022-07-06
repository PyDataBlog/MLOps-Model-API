#!/usr/bin/env php
<?php
require_once __DIR__.'/../vendor/autoload.php'; 

use Nightengale\Command;
use Knp\Provider\ConsoleServiceProvider;

$app = require 'Bootstrap.php';

$app->register(new ConsoleServiceProvider(),
                array('console.name'              => 'Console',
                      'console.version'           => '1.0.0',
                      'console.project_directory' => __DIR__.'/..'));

$application = $app['console'];
$application->add(new Command\WorkCommand());

$application->run();
