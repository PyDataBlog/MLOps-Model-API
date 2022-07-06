<?php

namespace Spark\Project\Configuration;

use Auryn\Injector;
use League\Plates\Engine;
use Spark\Configuration\ConfigurationInterface;
use Spark\Formatter\PlatesFormatter;
use Spark\Responder\FormattedResponder;

class PlatesConfiguration implements ConfigurationInterface
{
    public function apply(Injector $injector)
    {
        $injector->prepare(FormattedResponder::class, [$this, 'prepareResponder']);
        $injector->prepare(Engine::class, [$this, 'prepareEngine']);
    }

    public function prepareResponder(FormattedResponder $responder)
    {
        return $responder->withFormatters([
            PlatesFormatter::class => 1.0
        ]);
    }

    public function prepareEngine(Engine $engine)
    {
		// set file extension
		// $engine->setFileExtension('php');

        // Add folders
        $engine->addFolder('templates', APPPATH.'templates');
        $engine->addFolder('partials', APPPATH.'templates/partials');
        $engine->addFolder('staticpages', APPPATH.'templates/staticpages');
    }
}
