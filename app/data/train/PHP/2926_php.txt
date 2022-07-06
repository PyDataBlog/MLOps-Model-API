<?php

/* AdminBundle:Admin:gestionCategories.html.twig */
class __TwigTemplate_d67b492aebafcd5f8ab65d887049fd9f778babe4069922cd9ffcb861388774ee extends Twig_Template
{
    public function __construct(Twig_Environment $env)
    {
        parent::__construct($env);

        // line 1
        $this->parent = $this->loadTemplate("layout/layoutAdmin.html.twig", "AdminBundle:Admin:gestionCategories.html.twig", 1);
        $this->blocks = array(
            'title' => array($this, 'block_title'),
            'bread' => array($this, 'block_bread'),
            'body' => array($this, 'block_body'),
            'angular' => array($this, 'block_angular'),
        );
    }

    protected function doGetParent(array $context)
    {
        return "layout/layoutAdmin.html.twig";
    }

    protected function doDisplay(array $context, array $blocks = array())
    {
        $__internal_cde43e323352527c7dce020759e59a34f735f773650cd3c8debd02ad4afe50e2 = $this->env->getExtension("Symfony\\Bundle\\WebProfilerBundle\\Twig\\WebProfilerExtension");
        $__internal_cde43e323352527c7dce020759e59a34f735f773650cd3c8debd02ad4afe50e2->enter($__internal_cde43e323352527c7dce020759e59a34f735f773650cd3c8debd02ad4afe50e2_prof = new Twig_Profiler_Profile($this->getTemplateName(), "template", "AdminBundle:Admin:gestionCategories.html.twig"));

        $__internal_a3e67b5942a51cae08f3a97d8fefbbf6838cbff08c3f39a90a6f06a2e8cdc9a3 = $this->env->getExtension("Symfony\\Bridge\\Twig\\Extension\\ProfilerExtension");
        $__internal_a3e67b5942a51cae08f3a97d8fefbbf6838cbff08c3f39a90a6f06a2e8cdc9a3->enter($__internal_a3e67b5942a51cae08f3a97d8fefbbf6838cbff08c3f39a90a6f06a2e8cdc9a3_prof = new Twig_Profiler_Profile($this->getTemplateName(), "template", "AdminBundle:Admin:gestionCategories.html.twig"));

        $this->parent->display($context, array_merge($this->blocks, $blocks));
        
        $__internal_cde43e323352527c7dce020759e59a34f735f773650cd3c8debd02ad4afe50e2->leave($__internal_cde43e323352527c7dce020759e59a34f735f773650cd3c8debd02ad4afe50e2_prof);

        
        $__internal_a3e67b5942a51cae08f3a97d8fefbbf6838cbff08c3f39a90a6f06a2e8cdc9a3->leave($__internal_a3e67b5942a51cae08f3a97d8fefbbf6838cbff08c3f39a90a6f06a2e8cdc9a3_prof);

    }

    // line 3
    public function block_title($context, array $blocks = array())
    {
        $__internal_079df296d951057f1621359ed00bca076690b15930c830e2f97c9a6b202ac183 = $this->env->getExtension("Symfony\\Bundle\\WebProfilerBundle\\Twig\\WebProfilerExtension");
        $__internal_079df296d951057f1621359ed00bca076690b15930c830e2f97c9a6b202ac183->enter($__internal_079df296d951057f1621359ed00bca076690b15930c830e2f97c9a6b202ac183_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "title"));

        $__internal_e124196bf64e610832b44c3175fc7e3d48077e78d94b0725a604f1e8cc3f7e42 = $this->env->getExtension("Symfony\\Bridge\\Twig\\Extension\\ProfilerExtension");
        $__internal_e124196bf64e610832b44c3175fc7e3d48077e78d94b0725a604f1e8cc3f7e42->enter($__internal_e124196bf64e610832b44c3175fc7e3d48077e78d94b0725a604f1e8cc3f7e42_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "title"));

        echo "Dashboad";
        
        $__internal_e124196bf64e610832b44c3175fc7e3d48077e78d94b0725a604f1e8cc3f7e42->leave($__internal_e124196bf64e610832b44c3175fc7e3d48077e78d94b0725a604f1e8cc3f7e42_prof);

        
        $__internal_079df296d951057f1621359ed00bca076690b15930c830e2f97c9a6b202ac183->leave($__internal_079df296d951057f1621359ed00bca076690b15930c830e2f97c9a6b202ac183_prof);

    }

    // line 6
    public function block_bread($context, array $blocks = array())
    {
        $__internal_382c7fc54e6323ab81fede2cbfb23ade7c4a21db62f2fa56d6e71324d8935d01 = $this->env->getExtension("Symfony\\Bundle\\WebProfilerBundle\\Twig\\WebProfilerExtension");
        $__internal_382c7fc54e6323ab81fede2cbfb23ade7c4a21db62f2fa56d6e71324d8935d01->enter($__internal_382c7fc54e6323ab81fede2cbfb23ade7c4a21db62f2fa56d6e71324d8935d01_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "bread"));

        $__internal_2df00f4c839063dfac10d57588d51910a9f6b22ee636ee297dd6156979a6c4a7 = $this->env->getExtension("Symfony\\Bridge\\Twig\\Extension\\ProfilerExtension");
        $__internal_2df00f4c839063dfac10d57588d51910a9f6b22ee636ee297dd6156979a6c4a7->enter($__internal_2df00f4c839063dfac10d57588d51910a9f6b22ee636ee297dd6156979a6c4a7_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "bread"));

        // line 7
        echo "

    <div class=\"row wrapper border-bottom white-bg page-heading\">
        <div class=\"col-sm-4\">
            <h2>Gestion catégories</h2>
            <ol class=\"breadcrumb\">
                <li>
                    <a href=\"";
        // line 14
        echo $this->env->getExtension('Symfony\Bridge\Twig\Extension\RoutingExtension')->getPath("admin_homepage");
        echo "\">Admin</a>
                </li>
                <li class=\"active\">
                    <strong>Gestion Catégories</strong>
                </li>
            </ol>
        </div>

    </div>

";
        
        $__internal_2df00f4c839063dfac10d57588d51910a9f6b22ee636ee297dd6156979a6c4a7->leave($__internal_2df00f4c839063dfac10d57588d51910a9f6b22ee636ee297dd6156979a6c4a7_prof);

        
        $__internal_382c7fc54e6323ab81fede2cbfb23ade7c4a21db62f2fa56d6e71324d8935d01->leave($__internal_382c7fc54e6323ab81fede2cbfb23ade7c4a21db62f2fa56d6e71324d8935d01_prof);

    }

    // line 29
    public function block_body($context, array $blocks = array())
    {
        $__internal_8650bd4d84fa446e9a145bb3e2b14f5378b57446d85338804d96bd00fde7920a = $this->env->getExtension("Symfony\\Bundle\\WebProfilerBundle\\Twig\\WebProfilerExtension");
        $__internal_8650bd4d84fa446e9a145bb3e2b14f5378b57446d85338804d96bd00fde7920a->enter($__internal_8650bd4d84fa446e9a145bb3e2b14f5378b57446d85338804d96bd00fde7920a_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "body"));

        $__internal_edd6a65c7f003fd9676fa1538de0ee47701f4e3fbc5cc7a6ec81ce9897b7ff3f = $this->env->getExtension("Symfony\\Bridge\\Twig\\Extension\\ProfilerExtension");
        $__internal_edd6a65c7f003fd9676fa1538de0ee47701f4e3fbc5cc7a6ec81ce9897b7ff3f->enter($__internal_edd6a65c7f003fd9676fa1538de0ee47701f4e3fbc5cc7a6ec81ce9897b7ff3f_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "body"));

        // line 30
        echo "



    <div class=\"wrapper wrapper-content\" ng-controller=\"gestionCatCtrl\">
        <div class=\" text-center animated fadeInRightBig\">



            <div class=\"row\" ng-init=\"txtSearch=''\">

                <div class=\"form-group\">

                    <div class=\"col-sm-1\"></div>
                    <div class=\"col-sm-10\"><input placeholder=\"Chercher des catégories\" ng-model=\"txtSearch\" ng-change=\"getCategories(txtSearch)\" type=\"text\" class=\"form-control\"></div>

                </div>

            </div>


            <div class=\"clearP\"></div>


            <div class=\"row\">
                <div class=\"col-lg-3\" ng-repeat=\"categorie in categories\" >
                    <div class=\"contact-box center-version\" style=\"height: 280px!important;\">

                        <a href=\"#\">

                            <img alt=\"image\" class=\"img-circle\" src=\"";
        // line 60
        echo twig_escape_filter($this->env, $this->env->getExtension('Symfony\Bridge\Twig\Extension\AssetExtension')->getAssetUrl("img/a2.jpg"), "html", null, true);
        echo "\">


                            <h3 class=\"m-b-xs\"><strong ng-bind=\"categorie.nom\"></strong></h3>

                            <address class=\"m-t-md\">
                            <span ng-bind=\"categorie.desc | limitTo:50\"></span>
                            <span ng-if=\"categorie.desc.length > 50\">...</span>
                            </address>


                        </a>
                        <div class=\"contact-box-footer\">
                            <div class=\"m-t-xs btn-group\">
                                <a class=\"btn btn-xs btn-white\"><i class=\"fa fa-pencil\"></i> Modifier </a>
                                <a class=\"btn btn-xs btn-white\" ng-click=\"openModalDel(categorie.id,categorie.nom)\" ><i class=\"fa fa-close\"></i> Supprimer</a>

                            </div>
                        </div>

                    </div>
                </div>

             </div>





        </div>





        <div class=\"modal inmodal fade\" id=\"modalDelete\" tabindex=\"-1\" role=\"dialog\"  aria-hidden=\"true\">
            <div class=\"modal-dialog modal-sm\">
                <div class=\"modal-content\">
                    <div class=\"modal-header\">
                        <button type=\"button\" class=\"close\" data-dismiss=\"modal\"><span aria-hidden=\"true\">&times;</span><span class=\"sr-only\">Close</span></button>
                        <h4 class=\"modal-title\">Suppression</h4>
                    </div>
                    <div class=\"modal-body\">
                        <h3>
                            Vous etes sûr de <strong>supprimer</strong> la catégorie <span ng-bind=\"currentNameToDel\"></span>


                        </h3>



                    </div>
                    <div class=\"modal-footer\">
                        <button type=\"button\" class=\"btn btn-danger\" ng-click=\"deleteCategorie(currentIdToDel)\">Supprimer</button>
                        <button type=\"button\" class=\"btn btn-white\" data-dismiss=\"modal\">Annuler</button>
                    </div>
                </div>
            </div>
        </div>




    </div>



";
        
        $__internal_edd6a65c7f003fd9676fa1538de0ee47701f4e3fbc5cc7a6ec81ce9897b7ff3f->leave($__internal_edd6a65c7f003fd9676fa1538de0ee47701f4e3fbc5cc7a6ec81ce9897b7ff3f_prof);

        
        $__internal_8650bd4d84fa446e9a145bb3e2b14f5378b57446d85338804d96bd00fde7920a->leave($__internal_8650bd4d84fa446e9a145bb3e2b14f5378b57446d85338804d96bd00fde7920a_prof);

    }

    // line 131
    public function block_angular($context, array $blocks = array())
    {
        $__internal_e098835ee57ea6fc8e88750a5bc6ea99258053fa5791e59d0ddaf331696e506b = $this->env->getExtension("Symfony\\Bundle\\WebProfilerBundle\\Twig\\WebProfilerExtension");
        $__internal_e098835ee57ea6fc8e88750a5bc6ea99258053fa5791e59d0ddaf331696e506b->enter($__internal_e098835ee57ea6fc8e88750a5bc6ea99258053fa5791e59d0ddaf331696e506b_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "angular"));

        $__internal_0449b6bb9057f83abfb5af1ba1df0fd19fc961e1364f086aac02c6e0fba65027 = $this->env->getExtension("Symfony\\Bridge\\Twig\\Extension\\ProfilerExtension");
        $__internal_0449b6bb9057f83abfb5af1ba1df0fd19fc961e1364f086aac02c6e0fba65027->enter($__internal_0449b6bb9057f83abfb5af1ba1df0fd19fc961e1364f086aac02c6e0fba65027_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "angular"));

        // line 132
        echo "    <script>


        appAdmin.controller(\"gestionCatCtrl\",function(\$scope,\$http){
            \$scope.categories=[];
            \$scope.txtSearch=\"\";

            \$scope.currentNameToDel = \"\";
            \$scope.currentIdToDel = 0;

            \$scope.getCategories = function(txtChange){





                if(txtChange == undefined){
                    \$scope.txtRecherche = \"\";
                }else{
                    \$scope.txtRecherche = txtChange;
                }


                \$scope.objToSend = {
                    txt : \$scope.txtRecherche
                }



                \$http.post(\"http://172.16.128.8/formation/web/app_dev.php/Admin/Categories/getCategoriesJson\",\$scope.objToSend)
                        .then(function (response) {

                            \$scope.categories = response.data;

                        });


            }

            \$scope.getCategories(\$scope.txtSearch);


            setInterval(function(){
                \$scope.getCategories(\$scope.txtSearch);

            },3000);



            \$scope.openModalDel = function(id,nom){

                \$scope.currentNameToDel = nom;
                \$scope.currentIdToDel = id;
                \$(\"#modalDelete\").modal('show');
            }

            \$scope.deleteCategorie = function(id){


                \$scope.urlDel = \"http://172.16.128.8/formation/web/app_dev.php/Admin/Categories/deleteCategorieJson/\" + id ;

                \$http.post(\$scope.urlDel)
                        .then(function (reponse) {

                            if(reponse.data.success){
                                toastr.success(reponse.data.message,\"Suppression\");
                            }else{
                                toastr.error(reponse.data.message,\"Suppression\");
                            }

                            \$scope.getCategories(\$scope.txtSearch);
                            \$(\"#modalDelete\").modal('hide');

                        });




            }










        });


    </script>


";
        
        $__internal_0449b6bb9057f83abfb5af1ba1df0fd19fc961e1364f086aac02c6e0fba65027->leave($__internal_0449b6bb9057f83abfb5af1ba1df0fd19fc961e1364f086aac02c6e0fba65027_prof);

        
        $__internal_e098835ee57ea6fc8e88750a5bc6ea99258053fa5791e59d0ddaf331696e506b->leave($__internal_e098835ee57ea6fc8e88750a5bc6ea99258053fa5791e59d0ddaf331696e506b_prof);

    }

    public function getTemplateName()
    {
        return "AdminBundle:Admin:gestionCategories.html.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  228 => 132,  219 => 131,  141 => 60,  109 => 30,  100 => 29,  79 => 14,  70 => 7,  61 => 6,  43 => 3,  11 => 1,);
    }

    /** @deprecated since 1.27 (to be removed in 2.0). Use getSourceContext() instead */
    public function getSource()
    {
        @trigger_error('The '.__METHOD__.' method is deprecated since version 1.27 and will be removed in 2.0. Use getSourceContext() instead.', E_USER_DEPRECATED);

        return $this->getSourceContext()->getCode();
    }

    public function getSourceContext()
    {
        return new Twig_Source("{% extends 'layout/layoutAdmin.html.twig' %}

    {% block title %}Dashboad{% endblock %}


{% block bread %}


    <div class=\"row wrapper border-bottom white-bg page-heading\">
        <div class=\"col-sm-4\">
            <h2>Gestion catégories</h2>
            <ol class=\"breadcrumb\">
                <li>
                    <a href=\"{{ path('admin_homepage') }}\">Admin</a>
                </li>
                <li class=\"active\">
                    <strong>Gestion Catégories</strong>
                </li>
            </ol>
        </div>

    </div>

{% endblock %}




{% block body %}




    <div class=\"wrapper wrapper-content\" ng-controller=\"gestionCatCtrl\">
        <div class=\" text-center animated fadeInRightBig\">



            <div class=\"row\" ng-init=\"txtSearch=''\">

                <div class=\"form-group\">

                    <div class=\"col-sm-1\"></div>
                    <div class=\"col-sm-10\"><input placeholder=\"Chercher des catégories\" ng-model=\"txtSearch\" ng-change=\"getCategories(txtSearch)\" type=\"text\" class=\"form-control\"></div>

                </div>

            </div>


            <div class=\"clearP\"></div>


            <div class=\"row\">
                <div class=\"col-lg-3\" ng-repeat=\"categorie in categories\" >
                    <div class=\"contact-box center-version\" style=\"height: 280px!important;\">

                        <a href=\"#\">

                            <img alt=\"image\" class=\"img-circle\" src=\"{{ asset('img/a2.jpg') }}\">


                            <h3 class=\"m-b-xs\"><strong ng-bind=\"categorie.nom\"></strong></h3>

                            <address class=\"m-t-md\">
                            <span ng-bind=\"categorie.desc | limitTo:50\"></span>
                            <span ng-if=\"categorie.desc.length > 50\">...</span>
                            </address>


                        </a>
                        <div class=\"contact-box-footer\">
                            <div class=\"m-t-xs btn-group\">
                                <a class=\"btn btn-xs btn-white\"><i class=\"fa fa-pencil\"></i> Modifier </a>
                                <a class=\"btn btn-xs btn-white\" ng-click=\"openModalDel(categorie.id,categorie.nom)\" ><i class=\"fa fa-close\"></i> Supprimer</a>

                            </div>
                        </div>

                    </div>
                </div>

             </div>





        </div>





        <div class=\"modal inmodal fade\" id=\"modalDelete\" tabindex=\"-1\" role=\"dialog\"  aria-hidden=\"true\">
            <div class=\"modal-dialog modal-sm\">
                <div class=\"modal-content\">
                    <div class=\"modal-header\">
                        <button type=\"button\" class=\"close\" data-dismiss=\"modal\"><span aria-hidden=\"true\">&times;</span><span class=\"sr-only\">Close</span></button>
                        <h4 class=\"modal-title\">Suppression</h4>
                    </div>
                    <div class=\"modal-body\">
                        <h3>
                            Vous etes sûr de <strong>supprimer</strong> la catégorie <span ng-bind=\"currentNameToDel\"></span>


                        </h3>



                    </div>
                    <div class=\"modal-footer\">
                        <button type=\"button\" class=\"btn btn-danger\" ng-click=\"deleteCategorie(currentIdToDel)\">Supprimer</button>
                        <button type=\"button\" class=\"btn btn-white\" data-dismiss=\"modal\">Annuler</button>
                    </div>
                </div>
            </div>
        </div>




    </div>



{% endblock %}



{% block angular %}
    <script>


        appAdmin.controller(\"gestionCatCtrl\",function(\$scope,\$http){
            \$scope.categories=[];
            \$scope.txtSearch=\"\";

            \$scope.currentNameToDel = \"\";
            \$scope.currentIdToDel = 0;

            \$scope.getCategories = function(txtChange){





                if(txtChange == undefined){
                    \$scope.txtRecherche = \"\";
                }else{
                    \$scope.txtRecherche = txtChange;
                }


                \$scope.objToSend = {
                    txt : \$scope.txtRecherche
                }



                \$http.post(\"http://172.16.128.8/formation/web/app_dev.php/Admin/Categories/getCategoriesJson\",\$scope.objToSend)
                        .then(function (response) {

                            \$scope.categories = response.data;

                        });


            }

            \$scope.getCategories(\$scope.txtSearch);


            setInterval(function(){
                \$scope.getCategories(\$scope.txtSearch);

            },3000);



            \$scope.openModalDel = function(id,nom){

                \$scope.currentNameToDel = nom;
                \$scope.currentIdToDel = id;
                \$(\"#modalDelete\").modal('show');
            }

            \$scope.deleteCategorie = function(id){


                \$scope.urlDel = \"http://172.16.128.8/formation/web/app_dev.php/Admin/Categories/deleteCategorieJson/\" + id ;

                \$http.post(\$scope.urlDel)
                        .then(function (reponse) {

                            if(reponse.data.success){
                                toastr.success(reponse.data.message,\"Suppression\");
                            }else{
                                toastr.error(reponse.data.message,\"Suppression\");
                            }

                            \$scope.getCategories(\$scope.txtSearch);
                            \$(\"#modalDelete\").modal('hide');

                        });




            }










        });


    </script>


{% endblock %}


", "AdminBundle:Admin:gestionCategories.html.twig", "/var/www/html/formation/src/Admin/AdminBundle/Resources/views/Admin/gestionCategories.html.twig");
    }
}
