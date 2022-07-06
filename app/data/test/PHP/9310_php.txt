<?php

/* sil12VitrineBundle:Promotion:new.html.twig */
class __TwigTemplate_6b35c8288681521c57bf3eaa34c3cd1f8147156caf9e081e884352d40f9d44eb extends Twig_Template
{
    public function __construct(Twig_Environment $env)
    {
        parent::__construct($env);

        $this->parent = $this->env->loadTemplate("sil12VitrineBundle:BackOffice:layout.html.twig");

        $this->blocks = array(
            'content' => array($this, 'block_content'),
        );
    }

    protected function doGetParent(array $context)
    {
        return "sil12VitrineBundle:BackOffice:layout.html.twig";
    }

    protected function doDisplay(array $context, array $blocks = array())
    {
        $this->parent->display($context, array_merge($this->blocks, $blocks));
    }

    // line 4
    public function block_content($context, array $blocks = array())
    {
        // line 5
        echo "<div class=\"col-md-6 col-md-offset-3\">
\t<div class=\"panel panel-default\">
\t    <div class=\"panel-heading\">
\t        <h3 class=\"panel-title\">Ajout de promotion</h3>
\t    </div>
\t    <div class=\"panel-body\">
\t\t    ";
        // line 11
        echo         $this->env->getExtension('form')->renderer->renderBlock((isset($context["form"]) ? $context["form"] : $this->getContext($context, "form")), 'form');
        echo "

\t\t    <ul class=\"record_actions\">
\t\t\t    <li>
\t\t\t        <a href=\"";
        // line 15
        echo $this->env->getExtension('routing')->getPath("promotion");
        echo "\">
\t\t\t            Retour à la liste
\t\t\t        </a>
\t\t\t    </li>
\t\t\t</ul>
\t    </div>
\t</div> 
</div>
";
    }

    public function getTemplateName()
    {
        return "sil12VitrineBundle:Promotion:new.html.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  46 => 15,  39 => 11,  31 => 5,  28 => 4,);
    }
}
