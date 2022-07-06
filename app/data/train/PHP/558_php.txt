<?php

use Symfony\Component\Routing\RequestContext;
use Symfony\Component\Routing\Exception\RouteNotFoundException;


/**
 * appdevUrlGenerator
 *
 * This class has been auto-generated
 * by the Symfony Routing Component.
 */
class appdevUrlGenerator extends Symfony\Component\Routing\Generator\UrlGenerator
{
    static private $declaredRouteNames = array(
       '_wdt' => true,
       '_profiler_search' => true,
       '_profiler_purge' => true,
       '_profiler_import' => true,
       '_profiler_export' => true,
       '_profiler_search_results' => true,
       '_profiler' => true,
       '_configurator_home' => true,
       '_configurator_step' => true,
       '_configurator_final' => true,
       'Creditunions' => true,
       'Creditunions_show' => true,
       'Creditunions_new' => true,
       'Creditunions_create' => true,
       'Creditunions_edit' => true,
       'Creditunions_update' => true,
       'Creditunions_delete' => true,
       'Customers' => true,
       'Customers_show' => true,
       'Customers_new' => true,
       'Customers_create' => true,
       'Customers_edit' => true,
       'Customers_update' => true,
       'Customers_delete' => true,
       'creditunion_frontend_default_index' => true,
       'creditunion_frontend_default_contact' => true,
       'Depositsandloans' => true,
       'Depositsandloans_show' => true,
       'Depositsandloans_new' => true,
       'Depositsandloans_create' => true,
       'Depositsandloans_edit' => true,
       'Depositsandloans_update' => true,
       'Depositsandloans_delete' => true,
       'Price' => true,
       'Price_show' => true,
       'Price_new' => true,
       'Price_create' => true,
       'Price_edit' => true,
       'Price_update' => true,
       'Price_delete' => true,
       'creditunion_frontend_welcome_index' => true,
       'worker' => true,
       'worker_show' => true,
       'worker_new' => true,
       'worker_create' => true,
       'worker_edit' => true,
       'worker_update' => true,
       'worker_delete' => true,
    );

    /**
     * Constructor.
     */
    public function __construct(RequestContext $context)
    {
        $this->context = $context;
    }

    public function generate($name, $parameters = array(), $absolute = false)
    {
        if (!isset(self::$declaredRouteNames[$name])) {
            throw new RouteNotFoundException(sprintf('Route "%s" does not exist.', $name));
        }

        $escapedName = str_replace('.', '__', $name);

        list($variables, $defaults, $requirements, $tokens) = $this->{'get'.$escapedName.'RouteInfo'}();

        return $this->doGenerate($variables, $defaults, $requirements, $tokens, $parameters, $name, $absolute);
    }

    private function get_wdtRouteInfo()
    {
        return array(array (  0 => 'token',), array (  '_controller' => 'Symfony\\Bundle\\WebProfilerBundle\\Controller\\ProfilerController::toolbarAction',), array (), array (  0 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'token',  ),  1 =>   array (    0 => 'text',    1 => '/_wdt',  ),));
    }

    private function get_profiler_searchRouteInfo()
    {
        return array(array (), array (  '_controller' => 'Symfony\\Bundle\\WebProfilerBundle\\Controller\\ProfilerController::searchAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/_profiler/search',  ),));
    }

    private function get_profiler_purgeRouteInfo()
    {
        return array(array (), array (  '_controller' => 'Symfony\\Bundle\\WebProfilerBundle\\Controller\\ProfilerController::purgeAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/_profiler/purge',  ),));
    }

    private function get_profiler_importRouteInfo()
    {
        return array(array (), array (  '_controller' => 'Symfony\\Bundle\\WebProfilerBundle\\Controller\\ProfilerController::importAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/_profiler/import',  ),));
    }

    private function get_profiler_exportRouteInfo()
    {
        return array(array (  0 => 'token',), array (  '_controller' => 'Symfony\\Bundle\\WebProfilerBundle\\Controller\\ProfilerController::exportAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '.txt',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/\\.]+?',    3 => 'token',  ),  2 =>   array (    0 => 'text',    1 => '/_profiler/export',  ),));
    }

    private function get_profiler_search_resultsRouteInfo()
    {
        return array(array (  0 => 'token',), array (  '_controller' => 'Symfony\\Bundle\\WebProfilerBundle\\Controller\\ProfilerController::searchResultsAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/search/results',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'token',  ),  2 =>   array (    0 => 'text',    1 => '/_profiler',  ),));
    }

    private function get_profilerRouteInfo()
    {
        return array(array (  0 => 'token',), array (  '_controller' => 'Symfony\\Bundle\\WebProfilerBundle\\Controller\\ProfilerController::panelAction',), array (), array (  0 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'token',  ),  1 =>   array (    0 => 'text',    1 => '/_profiler',  ),));
    }

    private function get_configurator_homeRouteInfo()
    {
        return array(array (), array (  '_controller' => 'Sensio\\Bundle\\DistributionBundle\\Controller\\ConfiguratorController::checkAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/_configurator/',  ),));
    }

    private function get_configurator_stepRouteInfo()
    {
        return array(array (  0 => 'index',), array (  '_controller' => 'Sensio\\Bundle\\DistributionBundle\\Controller\\ConfiguratorController::stepAction',), array (), array (  0 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'index',  ),  1 =>   array (    0 => 'text',    1 => '/_configurator/step',  ),));
    }

    private function get_configurator_finalRouteInfo()
    {
        return array(array (), array (  '_controller' => 'Sensio\\Bundle\\DistributionBundle\\Controller\\ConfiguratorController::finalAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/_configurator/final',  ),));
    }

    private function getCreditunionsRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\CreditunionsController::indexAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/Creditunions/',  ),));
    }

    private function getCreditunions_showRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\CreditunionsController::showAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/show',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Creditunions',  ),));
    }

    private function getCreditunions_newRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\CreditunionsController::newAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/Creditunions/new',  ),));
    }

    private function getCreditunions_createRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\CreditunionsController::createAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/Creditunions/create',  ),));
    }

    private function getCreditunions_editRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\CreditunionsController::editAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/edit',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Creditunions',  ),));
    }

    private function getCreditunions_updateRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\CreditunionsController::updateAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/update',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Creditunions',  ),));
    }

    private function getCreditunions_deleteRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\CreditunionsController::deleteAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/delete',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Creditunions',  ),));
    }

    private function getCustomersRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\CustomersController::indexAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/Customers/',  ),));
    }

    private function getCustomers_showRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\CustomersController::showAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/show',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Customers',  ),));
    }

    private function getCustomers_newRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\CustomersController::newAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/Customers/new',  ),));
    }

    private function getCustomers_createRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\CustomersController::createAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/Customers/create',  ),));
    }

    private function getCustomers_editRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\CustomersController::editAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/edit',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Customers',  ),));
    }

    private function getCustomers_updateRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\CustomersController::updateAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/update',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Customers',  ),));
    }

    private function getCustomers_deleteRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\CustomersController::deleteAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/delete',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Customers',  ),));
    }

    private function getcreditunion_frontend_default_indexRouteInfo()
    {
        return array(array (  0 => 'name',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\DefaultController::indexAction',), array (), array (  0 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'name',  ),  1 =>   array (    0 => 'text',    1 => '/hello',  ),));
    }

    private function getcreditunion_frontend_default_contactRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\DefaultController::contact',), array (), array (  0 =>   array (    0 => 'text',    1 => '/contact',  ),));
    }

    private function getDepositsandloansRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\DepositsandloansController::indexAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/Depositsandloans/',  ),));
    }

    private function getDepositsandloans_showRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\DepositsandloansController::showAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/show',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Depositsandloans',  ),));
    }

    private function getDepositsandloans_newRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\DepositsandloansController::newAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/Depositsandloans/new',  ),));
    }

    private function getDepositsandloans_createRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\DepositsandloansController::createAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/Depositsandloans/create',  ),));
    }

    private function getDepositsandloans_editRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\DepositsandloansController::editAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/edit',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Depositsandloans',  ),));
    }

    private function getDepositsandloans_updateRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\DepositsandloansController::updateAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/update',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Depositsandloans',  ),));
    }

    private function getDepositsandloans_deleteRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\DepositsandloansController::deleteAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/delete',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Depositsandloans',  ),));
    }

    private function getPriceRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\PricelistController::indexAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/Price/',  ),));
    }

    private function getPrice_showRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\PricelistController::showAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/show',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Price',  ),));
    }

    private function getPrice_newRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\PricelistController::newAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/Price/new',  ),));
    }

    private function getPrice_createRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\PricelistController::createAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/Price/create',  ),));
    }

    private function getPrice_editRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\PricelistController::editAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/edit',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Price',  ),));
    }

    private function getPrice_updateRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\PricelistController::updateAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/update',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Price',  ),));
    }

    private function getPrice_deleteRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\PricelistController::deleteAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/delete',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Price',  ),));
    }

    private function getcreditunion_frontend_welcome_indexRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\WelcomeController::indexAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/',  ),));
    }

    private function getworkerRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\WorkerController::indexAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/Worker/',  ),));
    }

    private function getworker_showRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\WorkerController::showAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/show',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Worker',  ),));
    }

    private function getworker_newRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\WorkerController::newAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/Worker/new',  ),));
    }

    private function getworker_createRouteInfo()
    {
        return array(array (), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\WorkerController::createAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/Worker/create',  ),));
    }

    private function getworker_editRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\WorkerController::editAction',), array (), array (  0 =>   array (    0 => 'text',    1 => '/edit',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Worker',  ),));
    }

    private function getworker_updateRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\WorkerController::updateAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/update',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Worker',  ),));
    }

    private function getworker_deleteRouteInfo()
    {
        return array(array (  0 => 'id',), array (  '_controller' => 'CreditUnion\\FrontendBundle\\Controller\\WorkerController::deleteAction',), array (  '_method' => 'post',), array (  0 =>   array (    0 => 'text',    1 => '/delete',  ),  1 =>   array (    0 => 'variable',    1 => '/',    2 => '[^/]+?',    3 => 'id',  ),  2 =>   array (    0 => 'text',    1 => '/Worker',  ),));
    }
}
