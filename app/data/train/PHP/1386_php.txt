<?php

/* TwigBundle:Exception:exception.rdf.twig */
class __TwigTemplate_70e8b249c1428c255435d8a44ef7c09891fdf8c23551b886c441d0a716433b6a extends Twig_Template
{
    public function __construct(Twig_Environment $env)
    {
        parent::__construct($env);

        $this->parent = false;

        $this->blocks = array(
        );
    }

    protected function doDisplay(array $context, array $blocks = array())
    {
        $__internal_526d7a805785be4fb721c754b2cee69154d58ade11c606da9590ba90d330241a = $this->env->getExtension("Symfony\\Bundle\\WebProfilerBundle\\Twig\\WebProfilerExtension");
        $__internal_526d7a805785be4fb721c754b2cee69154d58ade11c606da9590ba90d330241a->enter($__internal_526d7a805785be4fb721c754b2cee69154d58ade11c606da9590ba90d330241a_prof = new Twig_Profiler_Profile($this->getTemplateName(), "template", "TwigBundle:Exception:exception.rdf.twig"));

        $__internal_eec35cd5a10ba99b284e27c6831565132f96b0a120b58f10f2e9b54513d0d1f7 = $this->env->getExtension("Symfony\\Bridge\\Twig\\Extension\\ProfilerExtension");
        $__internal_eec35cd5a10ba99b284e27c6831565132f96b0a120b58f10f2e9b54513d0d1f7->enter($__internal_eec35cd5a10ba99b284e27c6831565132f96b0a120b58f10f2e9b54513d0d1f7_prof = new Twig_Profiler_Profile($this->getTemplateName(), "template", "TwigBundle:Exception:exception.rdf.twig"));

        // line 1
        $this->loadTemplate("@Twig/Exception/exception.xml.twig", "TwigBundle:Exception:exception.rdf.twig", 1)->display(array_merge($context, array("exception" => (isset($context["exception"]) ? $context["exception"] : $this->getContext($context, "exception")))));
        
        $__internal_526d7a805785be4fb721c754b2cee69154d58ade11c606da9590ba90d330241a->leave($__internal_526d7a805785be4fb721c754b2cee69154d58ade11c606da9590ba90d330241a_prof);

        
        $__internal_eec35cd5a10ba99b284e27c6831565132f96b0a120b58f10f2e9b54513d0d1f7->leave($__internal_eec35cd5a10ba99b284e27c6831565132f96b0a120b58f10f2e9b54513d0d1f7_prof);

    }

    public function getTemplateName()
    {
        return "TwigBundle:Exception:exception.rdf.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  25 => 1,);
    }

    /** @deprecated since 1.27 (to be removed in 2.0). Use getSourceContext() instead */
    public function getSource()
    {
        @trigger_error('The '.__METHOD__.' method is deprecated since version 1.27 and will be removed in 2.0. Use getSourceContext() instead.', E_USER_DEPRECATED);

        return $this->getSourceContext()->getCode();
    }

    public function getSourceContext()
    {
        return new Twig_Source("{% include '@Twig/Exception/exception.xml.twig' with { 'exception': exception } %}
", "TwigBundle:Exception:exception.rdf.twig", "C:\\wamp64\\www\\Symfony\\vendor\\symfony\\symfony\\src\\Symfony\\Bundle\\TwigBundle/Resources/views/Exception/exception.rdf.twig");
    }
}
