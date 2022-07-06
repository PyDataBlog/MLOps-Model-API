<?php

/* :informacion:index.html.twig */
class __TwigTemplate_3fe207d07cb2a7021f8181185385f866207e1e1b6e8e118c7b08937099592d8c extends Twig_Template
{
    public function __construct(Twig_Environment $env)
    {
        parent::__construct($env);

        // line 1
        $this->parent = $this->loadTemplate("base.html.twig", ":informacion:index.html.twig", 1);
        $this->blocks = array(
            'stylesheets_extra' => array($this, 'block_stylesheets_extra'),
            'body' => array($this, 'block_body'),
            'javascripts_extra' => array($this, 'block_javascripts_extra'),
        );
    }

    protected function doGetParent(array $context)
    {
        return "base.html.twig";
    }

    protected function doDisplay(array $context, array $blocks = array())
    {
        $__internal_680719deb7b4b8273131714fbcec70196a09115941cbaa49b48bf1542be78804 = $this->env->getExtension("native_profiler");
        $__internal_680719deb7b4b8273131714fbcec70196a09115941cbaa49b48bf1542be78804->enter($__internal_680719deb7b4b8273131714fbcec70196a09115941cbaa49b48bf1542be78804_prof = new Twig_Profiler_Profile($this->getTemplateName(), "template", ":informacion:index.html.twig"));

        $this->parent->display($context, array_merge($this->blocks, $blocks));
        
        $__internal_680719deb7b4b8273131714fbcec70196a09115941cbaa49b48bf1542be78804->leave($__internal_680719deb7b4b8273131714fbcec70196a09115941cbaa49b48bf1542be78804_prof);

    }

    // line 2
    public function block_stylesheets_extra($context, array $blocks = array())
    {
        $__internal_61d99f116531dc40075507813235c209b44752ea80cdd7cb2cd9d8861a67adae = $this->env->getExtension("native_profiler");
        $__internal_61d99f116531dc40075507813235c209b44752ea80cdd7cb2cd9d8861a67adae->enter($__internal_61d99f116531dc40075507813235c209b44752ea80cdd7cb2cd9d8861a67adae_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "stylesheets_extra"));

        // line 3
        echo "      
";
        
        $__internal_61d99f116531dc40075507813235c209b44752ea80cdd7cb2cd9d8861a67adae->leave($__internal_61d99f116531dc40075507813235c209b44752ea80cdd7cb2cd9d8861a67adae_prof);

    }

    // line 7
    public function block_body($context, array $blocks = array())
    {
        $__internal_05f88ce797520f6d572ef3db3c35f16034597aa098181ffb6b04c774ae1bc814 = $this->env->getExtension("native_profiler");
        $__internal_05f88ce797520f6d572ef3db3c35f16034597aa098181ffb6b04c774ae1bc814->enter($__internal_05f88ce797520f6d572ef3db3c35f16034597aa098181ffb6b04c774ae1bc814_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "body"));

        // line 8
        echo "    <h1>Información</h1>
 <!-- BEGIN CONTAINER -->
        <div class=\"page-container\">
            <!-- BEGIN CONTENT -->
                    <div class=\"row\">
                       <div class=\"col-xs-12 col-sm-12        col-md-12 col-lg-12 \">
                            <a class=\"btn btn-success\" href=\"";
        // line 14
        echo $this->env->getExtension('routing')->getPath("informacion_new");
        echo "\" ><span>Agregar</span></a>
                            <br><br>                            
                            <!-- BEGIN EXAMPLE TABLE PORTLET-->
                            <div class=\"portlet light bordered\">
                                <div class=\"portlet-title\">
                                    <div class=\"caption font-green\">
                                        <i class=\"icon-settings font-green\"></i>
                                        <span class=\"caption-subject bold uppercase\"></span>
                                    </div>
                                    <div class=\"tools\"> </div>
                                </div>
                                <div class=\"portlet-body\">
                                    <table class=\"table table-striped table-bordered table-hover dt-responsive\" width=\"100%\" id=\"sample_2\">
                                        <thead>
                                            <tr>
                                                <th class=\"all\" width=\"10%\">Detalle</th>
                                                <th class=\"all\" width=\"10%\">Editar</th>
                                                <th class=\"all\">Descripcion</th>
                                                <th class=\"all\">Fecha</th>

                                            </tr>
                                        </thead>
                                        <tbody>
                                        ";
        // line 37
        $context['_parent'] = $context;
        $context['_seq'] = twig_ensure_traversable((isset($context["informacions"]) ? $context["informacions"] : $this->getContext($context, "informacions")));
        foreach ($context['_seq'] as $context["_key"] => $context["Informacion"]) {
            // line 38
            echo "                                            <tr>
                                                <td>
                                                    <center>
                                                        <a href=\"";
            // line 41
            echo twig_escape_filter($this->env, $this->env->getExtension('routing')->getPath("informacion_show", array("id" => $this->getAttribute($context["Informacion"], "id", array()))), "html", null, true);
            echo "\">
                                                            <i class=\"glyphicon glyphicon-search\"></i>
                                                        </a>      
                                                    </center>
                                                </td>
                                                    <td>
                                                    <center>
                                                        <a href=\"";
            // line 48
            echo twig_escape_filter($this->env, $this->env->getExtension('routing')->getPath("informacion_edit", array("id" => $this->getAttribute($context["Informacion"], "id", array()))), "html", null, true);
            echo "\">
                                                            <i class=\"glyphicon glyphicon-pencil\"></i>
                                                        </a>      
                                                    </center>
                                                </td>
                                            
                                                <td>";
            // line 54
            echo $this->getAttribute($context["Informacion"], "descripcion", array());
            echo "</td>

                                                <td>";
            // line 56
            echo twig_escape_filter($this->env, twig_date_format_filter($this->env, $this->getAttribute($context["Informacion"], "fechahora", array()), "d/m/Y"), "html", null, true);
            echo "</td>

                                            </tr>
                                        ";
        }
        $_parent = $context['_parent'];
        unset($context['_seq'], $context['_iterated'], $context['_key'], $context['Informacion'], $context['_parent'], $context['loop']);
        $context = array_intersect_key($context, $_parent) + $_parent;
        // line 60
        echo "                                        </tbody>                                        
                                    </table>
                                </div>
                            </div>
                            <!-- END EXAMPLE TABLE PORTLET-->
                        </div>
                        
                    </div>
                                    
                                        
            <!-- END CONTENT BODY -->
            
        </div>                                      
        <!-- END CONTAINER -->
    
";
        
        $__internal_05f88ce797520f6d572ef3db3c35f16034597aa098181ffb6b04c774ae1bc814->leave($__internal_05f88ce797520f6d572ef3db3c35f16034597aa098181ffb6b04c774ae1bc814_prof);

    }

    // line 77
    public function block_javascripts_extra($context, array $blocks = array())
    {
        $__internal_3fcefd8ca3b85ea42d90154347a109ffe2c9ace7fa7a1773265a41a63c892a2a = $this->env->getExtension("native_profiler");
        $__internal_3fcefd8ca3b85ea42d90154347a109ffe2c9ace7fa7a1773265a41a63c892a2a->enter($__internal_3fcefd8ca3b85ea42d90154347a109ffe2c9ace7fa7a1773265a41a63c892a2a_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "javascripts_extra"));

        // line 78
        echo "

 <script>    
 ";
        
        $__internal_3fcefd8ca3b85ea42d90154347a109ffe2c9ace7fa7a1773265a41a63c892a2a->leave($__internal_3fcefd8ca3b85ea42d90154347a109ffe2c9ace7fa7a1773265a41a63c892a2a_prof);

    }

    public function getTemplateName()
    {
        return ":informacion:index.html.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  161 => 78,  155 => 77,  133 => 60,  123 => 56,  118 => 54,  109 => 48,  99 => 41,  94 => 38,  90 => 37,  64 => 14,  56 => 8,  50 => 7,  42 => 3,  36 => 2,  11 => 1,);
    }
}
/* {% extends 'base.html.twig' %}*/
/* {% block stylesheets_extra %}*/
/*       */
/* {% endblock %}*/
/* */
/* */
/* {% block body %}*/
/*     <h1>Información</h1>*/
/*  <!-- BEGIN CONTAINER -->*/
/*         <div class="page-container">*/
/*             <!-- BEGIN CONTENT -->*/
/*                     <div class="row">*/
/*                        <div class="col-xs-12 col-sm-12        col-md-12 col-lg-12 ">*/
/*                             <a class="btn btn-success" href="{{ path('informacion_new') }}" ><span>Agregar</span></a>*/
/*                             <br><br>                            */
/*                             <!-- BEGIN EXAMPLE TABLE PORTLET-->*/
/*                             <div class="portlet light bordered">*/
/*                                 <div class="portlet-title">*/
/*                                     <div class="caption font-green">*/
/*                                         <i class="icon-settings font-green"></i>*/
/*                                         <span class="caption-subject bold uppercase"></span>*/
/*                                     </div>*/
/*                                     <div class="tools"> </div>*/
/*                                 </div>*/
/*                                 <div class="portlet-body">*/
/*                                     <table class="table table-striped table-bordered table-hover dt-responsive" width="100%" id="sample_2">*/
/*                                         <thead>*/
/*                                             <tr>*/
/*                                                 <th class="all" width="10%">Detalle</th>*/
/*                                                 <th class="all" width="10%">Editar</th>*/
/*                                                 <th class="all">Descripcion</th>*/
/*                                                 <th class="all">Fecha</th>*/
/* */
/*                                             </tr>*/
/*                                         </thead>*/
/*                                         <tbody>*/
/*                                         {% for Informacion in informacions%}*/
/*                                             <tr>*/
/*                                                 <td>*/
/*                                                     <center>*/
/*                                                         <a href="{{ path('informacion_show', { 'id': Informacion.id }) }}">*/
/*                                                             <i class="glyphicon glyphicon-search"></i>*/
/*                                                         </a>      */
/*                                                     </center>*/
/*                                                 </td>*/
/*                                                     <td>*/
/*                                                     <center>*/
/*                                                         <a href="{{ path('informacion_edit', { 'id': Informacion.id }) }}">*/
/*                                                             <i class="glyphicon glyphicon-pencil"></i>*/
/*                                                         </a>      */
/*                                                     </center>*/
/*                                                 </td>*/
/*                                             */
/*                                                 <td>{{ Informacion.descripcion | raw }}</td>*/
/* */
/*                                                 <td>{{ Informacion.fechahora | date("d/m/Y")}}</td>*/
/* */
/*                                             </tr>*/
/*                                         {% endfor %}*/
/*                                         </tbody>                                        */
/*                                     </table>*/
/*                                 </div>*/
/*                             </div>*/
/*                             <!-- END EXAMPLE TABLE PORTLET-->*/
/*                         </div>*/
/*                         */
/*                     </div>*/
/*                                     */
/*                                         */
/*             <!-- END CONTENT BODY -->*/
/*             */
/*         </div>                                      */
/*         <!-- END CONTAINER -->*/
/*     */
/* {% endblock %}*/
/* */
/* {% block javascripts_extra %}*/
/* */
/* */
/*  <script>    */
/*  {% endblock %}*/
