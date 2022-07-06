<?php
/**
 *
 */
namespace StrokerTemplateInspector;

use StrokerTemplateInspector\View\Renderer\PhpRenderer;
use Zend\ModuleManager\Feature\AutoloaderProviderInterface;
use Zend\ModuleManager\Feature\ConfigProviderInterface;
use Zend\ModuleManager\Feature\ServiceProviderInterface;
use Zend\View\Strategy\PhpRendererStrategy;

class Module implements ConfigProviderInterface, ServiceProviderInterface, AutoloaderProviderInterface
{
    /**
     * Returns configuration to merge with application configuration
     *
     * @return array|\Traversable
     */
    public function getConfig()
    {
        return include __DIR__ . '/config/module.config.php';
    }

    /**
     * Expected to return \Zend\ServiceManager\Config object or array to
     * seed such an object.
     *
     * @return array|\Zend\ServiceManager\Config
     */
    public function getServiceConfig()
    {
        return array(
            'invokables' => array(
                'StrokerTemplateInspector\Collector\TemplateCollector' => 'StrokerTemplateInspector\Collector\TemplateCollector'
            ),
            'factories' => array(
                'StrokerTemplateStrategy' => function($sm) {
                    $renderer = new PhpRenderer;
                    $renderer->setHelperPluginManager($sm->get('ViewHelperManager'));
                    $renderer->setResolver($sm->get('ViewResolver'));
                    $renderer->setTemplateCollector($sm->get('StrokerTemplateInspector\Collector\TemplateCollector'));
                    return new PhpRendererStrategy($renderer);
                }
            )
        );
    }

    /**
     * Return an array for passing to Zend\Loader\AutoloaderFactory.
     *
     * @return array
     */
    public function getAutoloaderConfig()
    {
        return array(
            'Zend\Loader\StandardAutoloader' => array(
                'namespaces' => array(
                    __NAMESPACE__ => __DIR__ . '/src/' . __NAMESPACE__,
                ),
            ),
        );
    }
}
