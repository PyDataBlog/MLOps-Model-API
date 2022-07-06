<?php

/* PgGsbFraisBundle::accueilCompta.html.twig */
class __TwigTemplate_6194dcf338f5cd969f229bda597abec8b69c1f8371639ebcadc7c86b484b4c01 extends Twig_Template
{
    public function __construct(Twig_Environment $env)
    {
        parent::__construct($env);

        $this->parent = $this->env->loadTemplate("PgGsbFraisBundle::layout.html.twig");

        $this->blocks = array(
            'body' => array($this, 'block_body'),
            'section' => array($this, 'block_section'),
        );
    }

    protected function doGetParent(array $context)
    {
        return "PgGsbFraisBundle::layout.html.twig";
    }

    protected function doDisplay(array $context, array $blocks = array())
    {
        $this->parent->display($context, array_merge($this->blocks, $blocks));
    }

    // line 2
    public function block_body($context, array $blocks = array())
    {
        // line 3
        echo "<nav>
    <div class=\"row\">
        <div class=\"col-lg-10\">
            <nav class=\"navbar navbar-default\" role=\"navigation\">
                <div class=\"container-fluid\">
                    <div class=\"navbar-header\">
                        <a class=\"navbar-brand\" href=\"";
        // line 9
        echo $this->env->getExtension('routing')->getPath("pg_gsb_frais_home");
        echo "\">Accueil</a>
                    </div>
                    <!-- Collect the nav links, forms, and other content for toggling -->
                    <div class=\"collapse navbar-collapse\" id=\"bs-example-navbar-collapse-1\">
                        <ul class=\"nav navbar-nav\">
                            <li>
                                <a href=\"";
        // line 15
        echo $this->env->getExtension('routing')->getPath("pg_gsb_frais_suivrefrais");
        echo "\" title=\"Suivre fiche de frais \">Suivre fiche de frais</a>
                            </li>

                            <li>
                                <a href= \"";
        // line 19
        echo $this->env->getExtension('routing')->getPath("pg_gsb_frais_validerfrais");
        echo "\" title=\"Valider fiches de frais\">Valider fiches de frais</a>
                            </li>    
                        </ul>
                    </div>
            </nav>
        </div>
        <div class=\"col-lg-2\">
            <nav class=\"navbar navbar-default\" role=\"deconnexion\">
                <div class=\"container-fluid\">

        ";
        // line 29
        if ($this->getAttribute($this->getAttribute((isset($context["app"]) ? $context["app"] : null), "session", array(), "any", false, true), "get", array(0 => "nom"), "method", true, true)) {
            // line 30
            echo "                    Bonjour ";
            echo twig_escape_filter($this->env, $this->getAttribute($this->getAttribute((isset($context["app"]) ? $context["app"] : $this->getContext($context, "app")), "session"), "get", array(0 => "nom"), "method"), "html", null, true);
            echo " <br />
        ";
        }
        // line 32
        echo "                    <a href=\"";
        echo $this->env->getExtension('routing')->getPath("pg_gsb_frais_deconnexion");
        echo "\" title=\"Se déconnecter\">Déconnexion</a>
                </div>
            </nav>
        </div>
    </div>
</nav> 
<section>   
    ";
        // line 39
        $this->displayBlock('section', $context, $blocks);
        // line 41
        echo "</section>
";
    }

    // line 39
    public function block_section($context, array $blocks = array())
    {
        // line 40
        echo "    ";
    }

    public function getTemplateName()
    {
        return "PgGsbFraisBundle::accueilCompta.html.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  98 => 40,  95 => 39,  90 => 41,  88 => 39,  77 => 32,  71 => 30,  69 => 29,  56 => 19,  49 => 15,  40 => 9,  32 => 3,  29 => 2,  31 => 3,  28 => 2,);
    }
}
