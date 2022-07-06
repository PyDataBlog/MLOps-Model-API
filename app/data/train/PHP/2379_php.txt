<?php

use Symfony\Component\Routing\Exception\MethodNotAllowedException;
use Symfony\Component\Routing\Exception\ResourceNotFoundException;
use Symfony\Component\Routing\RequestContext;

/**
 * appDevUrlMatcher
 *
 * This class has been auto-generated
 * by the Symfony Routing Component.
 */
class appDevUrlMatcher extends Symfony\Bundle\FrameworkBundle\Routing\RedirectableUrlMatcher
{
    /**
     * Constructor.
     */
    public function __construct(RequestContext $context)
    {
        $this->context = $context;
    }

    public function match($pathinfo)
    {
        $allow = array();
        $pathinfo = rawurldecode($pathinfo);

        if (0 === strpos($pathinfo, '/_')) {
            // _wdt
            if (0 === strpos($pathinfo, '/_wdt') && preg_match('#^/_wdt/(?P<token>[^/]++)$#s', $pathinfo, $matches)) {
                return $this->mergeDefaults(array_replace($matches, array('_route' => '_wdt')), array (  '_controller' => 'web_profiler.controller.profiler:toolbarAction',));
            }

            if (0 === strpos($pathinfo, '/_profiler')) {
                // _profiler_home
                if (rtrim($pathinfo, '/') === '/_profiler') {
                    if (substr($pathinfo, -1) !== '/') {
                        return $this->redirect($pathinfo.'/', '_profiler_home');
                    }

                    return array (  '_controller' => 'web_profiler.controller.profiler:homeAction',  '_route' => '_profiler_home',);
                }

                if (0 === strpos($pathinfo, '/_profiler/search')) {
                    // _profiler_search
                    if ($pathinfo === '/_profiler/search') {
                        return array (  '_controller' => 'web_profiler.controller.profiler:searchAction',  '_route' => '_profiler_search',);
                    }

                    // _profiler_search_bar
                    if ($pathinfo === '/_profiler/search_bar') {
                        return array (  '_controller' => 'web_profiler.controller.profiler:searchBarAction',  '_route' => '_profiler_search_bar',);
                    }

                }

                // _profiler_purge
                if ($pathinfo === '/_profiler/purge') {
                    return array (  '_controller' => 'web_profiler.controller.profiler:purgeAction',  '_route' => '_profiler_purge',);
                }

                if (0 === strpos($pathinfo, '/_profiler/i')) {
                    // _profiler_info
                    if (0 === strpos($pathinfo, '/_profiler/info') && preg_match('#^/_profiler/info/(?P<about>[^/]++)$#s', $pathinfo, $matches)) {
                        return $this->mergeDefaults(array_replace($matches, array('_route' => '_profiler_info')), array (  '_controller' => 'web_profiler.controller.profiler:infoAction',));
                    }

                    // _profiler_import
                    if ($pathinfo === '/_profiler/import') {
                        return array (  '_controller' => 'web_profiler.controller.profiler:importAction',  '_route' => '_profiler_import',);
                    }

                }

                // _profiler_export
                if (0 === strpos($pathinfo, '/_profiler/export') && preg_match('#^/_profiler/export/(?P<token>[^/\\.]++)\\.txt$#s', $pathinfo, $matches)) {
                    return $this->mergeDefaults(array_replace($matches, array('_route' => '_profiler_export')), array (  '_controller' => 'web_profiler.controller.profiler:exportAction',));
                }

                // _profiler_phpinfo
                if ($pathinfo === '/_profiler/phpinfo') {
                    return array (  '_controller' => 'web_profiler.controller.profiler:phpinfoAction',  '_route' => '_profiler_phpinfo',);
                }

                // _profiler_search_results
                if (preg_match('#^/_profiler/(?P<token>[^/]++)/search/results$#s', $pathinfo, $matches)) {
                    return $this->mergeDefaults(array_replace($matches, array('_route' => '_profiler_search_results')), array (  '_controller' => 'web_profiler.controller.profiler:searchResultsAction',));
                }

                // _profiler
                if (preg_match('#^/_profiler/(?P<token>[^/]++)$#s', $pathinfo, $matches)) {
                    return $this->mergeDefaults(array_replace($matches, array('_route' => '_profiler')), array (  '_controller' => 'web_profiler.controller.profiler:panelAction',));
                }

                // _profiler_router
                if (preg_match('#^/_profiler/(?P<token>[^/]++)/router$#s', $pathinfo, $matches)) {
                    return $this->mergeDefaults(array_replace($matches, array('_route' => '_profiler_router')), array (  '_controller' => 'web_profiler.controller.router:panelAction',));
                }

                // _profiler_exception
                if (preg_match('#^/_profiler/(?P<token>[^/]++)/exception$#s', $pathinfo, $matches)) {
                    return $this->mergeDefaults(array_replace($matches, array('_route' => '_profiler_exception')), array (  '_controller' => 'web_profiler.controller.exception:showAction',));
                }

                // _profiler_exception_css
                if (preg_match('#^/_profiler/(?P<token>[^/]++)/exception\\.css$#s', $pathinfo, $matches)) {
                    return $this->mergeDefaults(array_replace($matches, array('_route' => '_profiler_exception_css')), array (  '_controller' => 'web_profiler.controller.exception:cssAction',));
                }

            }

            if (0 === strpos($pathinfo, '/_configurator')) {
                // _configurator_home
                if (rtrim($pathinfo, '/') === '/_configurator') {
                    if (substr($pathinfo, -1) !== '/') {
                        return $this->redirect($pathinfo.'/', '_configurator_home');
                    }

                    return array (  '_controller' => 'Sensio\\Bundle\\DistributionBundle\\Controller\\ConfiguratorController::checkAction',  '_route' => '_configurator_home',);
                }

                // _configurator_step
                if (0 === strpos($pathinfo, '/_configurator/step') && preg_match('#^/_configurator/step/(?P<index>[^/]++)$#s', $pathinfo, $matches)) {
                    return $this->mergeDefaults(array_replace($matches, array('_route' => '_configurator_step')), array (  '_controller' => 'Sensio\\Bundle\\DistributionBundle\\Controller\\ConfiguratorController::stepAction',));
                }

                // _configurator_final
                if ($pathinfo === '/_configurator/final') {
                    return array (  '_controller' => 'Sensio\\Bundle\\DistributionBundle\\Controller\\ConfiguratorController::finalAction',  '_route' => '_configurator_final',);
                }

            }

        }

        if (0 === strpos($pathinfo, '/collection')) {
            // lien_collection
            if (rtrim($pathinfo, '/') === '/collection') {
                if (substr($pathinfo, -1) !== '/') {
                    return $this->redirect($pathinfo.'/', 'lien_collection');
                }

                return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\CollectionController::indexAction',  '_route' => 'lien_collection',);
            }

            // lien_collection_show
            if (preg_match('#^/collection/(?P<id>[^/]++)/(?P<slug>[^/]++)(?:/(?P<page>[^/]++))?$#s', $pathinfo, $matches)) {
                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_collection_show')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\CollectionController::showAction',  'page' => 1,));
            }

            // lien_collection_new
            if ($pathinfo === '/collection/new') {
                return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\CollectionController::newAction',  '_route' => 'lien_collection_new',);
            }

            // lien_collection_create
            if ($pathinfo === '/collection/create') {
                if ($this->context->getMethod() != 'POST') {
                    $allow[] = 'POST';
                    goto not_lien_collection_create;
                }

                return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\CollectionController::createAction',  '_route' => 'lien_collection_create',);
            }
            not_lien_collection_create:

            // lien_collection_edit
            if (0 === strpos($pathinfo, '/collection/edit') && preg_match('#^/collection/edit/(?P<id>[^/]++)/?$#s', $pathinfo, $matches)) {
                if (substr($pathinfo, -1) !== '/') {
                    return $this->redirect($pathinfo.'/', 'lien_collection_edit');
                }

                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_collection_edit')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\CollectionController::editAction',));
            }

            // lien_collection_update
            if (preg_match('#^/collection/(?P<id>[^/]++)/update$#s', $pathinfo, $matches)) {
                if (!in_array($this->context->getMethod(), array('POST', 'PUT'))) {
                    $allow = array_merge($allow, array('POST', 'PUT'));
                    goto not_lien_collection_update;
                }

                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_collection_update')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\CollectionController::updateAction',));
            }
            not_lien_collection_update:

            // lien_collection_delete
            if (preg_match('#^/collection/(?P<id>[^/]++)/delete$#s', $pathinfo, $matches)) {
                if (!in_array($this->context->getMethod(), array('POST', 'DELETE'))) {
                    $allow = array_merge($allow, array('POST', 'DELETE'));
                    goto not_lien_collection_delete;
                }

                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_collection_delete')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\CollectionController::deleteAction',));
            }
            not_lien_collection_delete:

        }

        if (0 === strpos($pathinfo, '/avis')) {
            // lien_avis
            if (rtrim($pathinfo, '/') === '/avis') {
                if (substr($pathinfo, -1) !== '/') {
                    return $this->redirect($pathinfo.'/', 'lien_avis');
                }

                return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\AvisUserLivreController::indexAction',  '_route' => 'lien_avis',);
            }

            // lien_avis_show
            if (preg_match('#^/avis/(?P<id>[^/]++)/show$#s', $pathinfo, $matches)) {
                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_avis_show')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\AvisUserLivreController::showAction',));
            }

            // lien_avis_new
            if ($pathinfo === '/avis/new') {
                return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\AvisUserLivreController::newAction',  '_route' => 'lien_avis_new',);
            }

            // lien_avis_create
            if ($pathinfo === '/avis/create') {
                if ($this->context->getMethod() != 'POST') {
                    $allow[] = 'POST';
                    goto not_lien_avis_create;
                }

                return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\AvisUserLivreController::createAction',  '_route' => 'lien_avis_create',);
            }
            not_lien_avis_create:

            // lien_avis_edit
            if (preg_match('#^/avis/(?P<id>[^/]++)/edit$#s', $pathinfo, $matches)) {
                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_avis_edit')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\AvisUserLivreController::editAction',));
            }

            // lien_avis_update
            if (preg_match('#^/avis/(?P<id>[^/]++)/update$#s', $pathinfo, $matches)) {
                if (!in_array($this->context->getMethod(), array('POST', 'PUT'))) {
                    $allow = array_merge($allow, array('POST', 'PUT'));
                    goto not_lien_avis_update;
                }

                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_avis_update')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\AvisUserLivreController::updateAction',));
            }
            not_lien_avis_update:

            // lien_avis_delete
            if (preg_match('#^/avis/(?P<id>[^/]++)/delete$#s', $pathinfo, $matches)) {
                if (!in_array($this->context->getMethod(), array('POST', 'DELETE'))) {
                    $allow = array_merge($allow, array('POST', 'DELETE'));
                    goto not_lien_avis_delete;
                }

                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_avis_delete')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\AvisUserLivreController::deleteAction',));
            }
            not_lien_avis_delete:

        }

        if (0 === strpos($pathinfo, '/livre')) {
            // lien_livre
            if (rtrim($pathinfo, '/') === '/livre') {
                if (substr($pathinfo, -1) !== '/') {
                    return $this->redirect($pathinfo.'/', 'lien_livre');
                }

                return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\LivreController::indexAction',  '_route' => 'lien_livre',);
            }

            // lien_livre_read
            if (0 === strpos($pathinfo, '/livre/read') && preg_match('#^/livre/read/(?P<collection>[^/]++)/(?P<auteur>[^/]++)/(?P<id>\\d+)/(?P<titre>[^/]++)$#s', $pathinfo, $matches)) {
                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_livre_read')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\LivreController::readAction',));
            }

            // lien_livre_show
            if (preg_match('#^/livre/(?P<collection>[^/]++)/(?P<auteur>[^/]++)/(?P<id>\\d+)/(?P<titre>[^/]++)$#s', $pathinfo, $matches)) {
                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_livre_show')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\LivreController::showAction',));
            }

            // lien_livre_all
            if (0 === strpos($pathinfo, '/livre/liste') && preg_match('#^/livre/liste(?:/(?P<page>[^/]++))?$#s', $pathinfo, $matches)) {
                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_livre_all')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\LivreController::allAction',  'page' => 1,));
            }

            // lien_livre_show_all
            if (0 === strpos($pathinfo, '/livre/catalogue') && preg_match('#^/livre/catalogue(?:/(?P<page>[^/]++))?$#s', $pathinfo, $matches)) {
                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_livre_show_all')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\LivreController::showAllAction',  'page' => 1,));
            }

            // lien_livre_find
            if ($pathinfo === '/livre/recherche') {
                if ($this->context->getMethod() != 'POST') {
                    $allow[] = 'POST';
                    goto not_lien_livre_find;
                }

                return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\LivreController::findAction',  '_route' => 'lien_livre_find',);
            }
            not_lien_livre_find:

            // lien_livre_new
            if ($pathinfo === '/livre/new') {
                return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\LivreController::newAction',  '_route' => 'lien_livre_new',);
            }

            // lien_livre_create
            if ($pathinfo === '/livre/create') {
                if ($this->context->getMethod() != 'POST') {
                    $allow[] = 'POST';
                    goto not_lien_livre_create;
                }

                return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\LivreController::createAction',  '_route' => 'lien_livre_create',);
            }
            not_lien_livre_create:

            // lien_livre_edit
            if (preg_match('#^/livre/(?P<id>[^/]++)/edit$#s', $pathinfo, $matches)) {
                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_livre_edit')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\LivreController::editAction',));
            }

            // lien_livre_update
            if (preg_match('#^/livre/(?P<id>[^/]++)/update$#s', $pathinfo, $matches)) {
                if (!in_array($this->context->getMethod(), array('POST', 'PUT'))) {
                    $allow = array_merge($allow, array('POST', 'PUT'));
                    goto not_lien_livre_update;
                }

                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_livre_update')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\LivreController::updateAction',));
            }
            not_lien_livre_update:

            // lien_livre_delete
            if (preg_match('#^/livre/(?P<id>[^/]++)/delete$#s', $pathinfo, $matches)) {
                if (!in_array($this->context->getMethod(), array('POST', 'DELETE'))) {
                    $allow = array_merge($allow, array('POST', 'DELETE'));
                    goto not_lien_livre_delete;
                }

                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_livre_delete')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\LivreController::deleteAction',));
            }
            not_lien_livre_delete:

        }

        if (0 === strpos($pathinfo, '/user')) {
            // lien_user
            if (rtrim($pathinfo, '/') === '/user') {
                if (substr($pathinfo, -1) !== '/') {
                    return $this->redirect($pathinfo.'/', 'lien_user');
                }

                return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\UserController::indexAction',  '_route' => 'lien_user',);
            }

            // lien_user_show
            if (preg_match('#^/user/(?P<id>[^/]++)/show$#s', $pathinfo, $matches)) {
                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_user_show')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\UserController::showAction',));
            }

            // lien_user_new
            if ($pathinfo === '/user/new') {
                return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\UserController::newAction',  '_route' => 'lien_user_new',);
            }

            // lien_user_create
            if ($pathinfo === '/user/create') {
                if ($this->context->getMethod() != 'POST') {
                    $allow[] = 'POST';
                    goto not_lien_user_create;
                }

                return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\UserController::createAction',  '_route' => 'lien_user_create',);
            }
            not_lien_user_create:

            // lien_user_modif
            if ($pathinfo === '/user/modif') {
                if ($this->context->getMethod() != 'POST') {
                    $allow[] = 'POST';
                    goto not_lien_user_modif;
                }

                return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\UserController::modifAction',  '_route' => 'lien_user_modif',);
            }
            not_lien_user_modif:

            // lien_user_edit
            if (0 === strpos($pathinfo, '/user/edit') && preg_match('#^/user/edit/(?P<id>[^/]++)/?$#s', $pathinfo, $matches)) {
                if (substr($pathinfo, -1) !== '/') {
                    return $this->redirect($pathinfo.'/', 'lien_user_edit');
                }

                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_user_edit')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\UserController::editAction',));
            }

            // lien_user_edit_admin
            if (preg_match('#^/user/(?P<id>[^/]++)/edit$#s', $pathinfo, $matches)) {
                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_user_edit_admin')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\UserController::editAdminAction',));
            }

            // lien_user_update
            if (preg_match('#^/user/(?P<id>[^/]++)/update$#s', $pathinfo, $matches)) {
                if (!in_array($this->context->getMethod(), array('POST', 'PUT'))) {
                    $allow = array_merge($allow, array('POST', 'PUT'));
                    goto not_lien_user_update;
                }

                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_user_update')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\UserController::updateAction',));
            }
            not_lien_user_update:

            // lien_user_update_admin
            if (preg_match('#^/user/(?P<id>[^/]++)/update$#s', $pathinfo, $matches)) {
                if (!in_array($this->context->getMethod(), array('POST', 'PUT'))) {
                    $allow = array_merge($allow, array('POST', 'PUT'));
                    goto not_lien_user_update_admin;
                }

                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_user_update_admin')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\UserController::updateAdminAction',));
            }
            not_lien_user_update_admin:

            // lien_user_delete
            if (preg_match('#^/user/(?P<id>[^/]++)/delete$#s', $pathinfo, $matches)) {
                if (!in_array($this->context->getMethod(), array('POST', 'DELETE'))) {
                    $allow = array_merge($allow, array('POST', 'DELETE'));
                    goto not_lien_user_delete;
                }

                return $this->mergeDefaults(array_replace($matches, array('_route' => 'lien_user_delete')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\UserController::deleteAction',));
            }
            not_lien_user_delete:

            if (0 === strpos($pathinfo, '/user/log')) {
                // lien_user_login
                if ($pathinfo === '/user/login') {
                    if ($this->context->getMethod() != 'POST') {
                        $allow[] = 'POST';
                        goto not_lien_user_login;
                    }

                    return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\UserController::loginAction',  '_route' => 'lien_user_login',);
                }
                not_lien_user_login:

                // lien_user_logout
                if ($pathinfo === '/user/logout') {
                    return array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\UserController::logoutAction',  '_route' => 'lien_user_logout',);
                }

            }

        }

        // lsi_biblio_homepage
        if (0 === strpos($pathinfo, '/hello') && preg_match('#^/hello/(?P<name>[^/]++)$#s', $pathinfo, $matches)) {
            return $this->mergeDefaults(array_replace($matches, array('_route' => 'lsi_biblio_homepage')), array (  '_controller' => 'Lsi\\BiblioBundle\\Controller\\DefaultController::indexAction',));
        }

        // _welcome
        if (rtrim($pathinfo, '/') === '') {
            if (substr($pathinfo, -1) !== '/') {
                return $this->redirect($pathinfo.'/', '_welcome');
            }

            return array (  '_controller' => 'Acme\\DemoBundle\\Controller\\WelcomeController::indexAction',  '_route' => '_welcome',);
        }

        if (0 === strpos($pathinfo, '/demo')) {
            if (0 === strpos($pathinfo, '/demo/secured')) {
                if (0 === strpos($pathinfo, '/demo/secured/log')) {
                    if (0 === strpos($pathinfo, '/demo/secured/login')) {
                        // _demo_login
                        if ($pathinfo === '/demo/secured/login') {
                            return array (  '_controller' => 'Acme\\DemoBundle\\Controller\\SecuredController::loginAction',  '_route' => '_demo_login',);
                        }

                        // _security_check
                        if ($pathinfo === '/demo/secured/login_check') {
                            return array (  '_controller' => 'Acme\\DemoBundle\\Controller\\SecuredController::securityCheckAction',  '_route' => '_security_check',);
                        }

                    }

                    // _demo_logout
                    if ($pathinfo === '/demo/secured/logout') {
                        return array (  '_controller' => 'Acme\\DemoBundle\\Controller\\SecuredController::logoutAction',  '_route' => '_demo_logout',);
                    }

                }

                if (0 === strpos($pathinfo, '/demo/secured/hello')) {
                    // acme_demo_secured_hello
                    if ($pathinfo === '/demo/secured/hello') {
                        return array (  'name' => 'World',  '_controller' => 'Acme\\DemoBundle\\Controller\\SecuredController::helloAction',  '_route' => 'acme_demo_secured_hello',);
                    }

                    // _demo_secured_hello
                    if (preg_match('#^/demo/secured/hello/(?P<name>[^/]++)$#s', $pathinfo, $matches)) {
                        return $this->mergeDefaults(array_replace($matches, array('_route' => '_demo_secured_hello')), array (  '_controller' => 'Acme\\DemoBundle\\Controller\\SecuredController::helloAction',));
                    }

                    // _demo_secured_hello_admin
                    if (0 === strpos($pathinfo, '/demo/secured/hello/admin') && preg_match('#^/demo/secured/hello/admin/(?P<name>[^/]++)$#s', $pathinfo, $matches)) {
                        return $this->mergeDefaults(array_replace($matches, array('_route' => '_demo_secured_hello_admin')), array (  '_controller' => 'Acme\\DemoBundle\\Controller\\SecuredController::helloadminAction',));
                    }

                }

            }

            // _demo
            if (rtrim($pathinfo, '/') === '/demo') {
                if (substr($pathinfo, -1) !== '/') {
                    return $this->redirect($pathinfo.'/', '_demo');
                }

                return array (  '_controller' => 'Acme\\DemoBundle\\Controller\\DemoController::indexAction',  '_route' => '_demo',);
            }

            // _demo_hello
            if (0 === strpos($pathinfo, '/demo/hello') && preg_match('#^/demo/hello/(?P<name>[^/]++)$#s', $pathinfo, $matches)) {
                return $this->mergeDefaults(array_replace($matches, array('_route' => '_demo_hello')), array (  '_controller' => 'Acme\\DemoBundle\\Controller\\DemoController::helloAction',));
            }

            // _demo_contact
            if ($pathinfo === '/demo/contact') {
                return array (  '_controller' => 'Acme\\DemoBundle\\Controller\\DemoController::contactAction',  '_route' => '_demo_contact',);
            }

        }

        throw 0 < count($allow) ? new MethodNotAllowedException(array_unique($allow)) : new ResourceNotFoundException();
    }
}
