<?php
require_once 'vendor/autoload.php';
require_once 'autoload.php';
use Silex\Application;
use Symfony\Component\HttpFoundation\Request;
use Command\CommandContext;


$app = new Application();
$app->register(new Silex\Provider\ServiceControllerServiceProvider());

// criando view, esse objeto é injetado na criacao dos commands específicos
$app['view'] = $app->share(function() use ($app){
    return new Site\View\View;
});
// fim criacao

//registro objeto que define contexto para comandos
$app['commandContext'] = $app->share(function () use ($app){
  return new CommandContext();
});
//fim registro


