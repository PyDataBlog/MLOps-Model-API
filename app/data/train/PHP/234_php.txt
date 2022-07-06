<?php

/* SRVDVServerBundle:ChangePassword:changePassword.html.twig */
class __TwigTemplate_e5da0f0553fed635f2774271a2ef3982b389e99e651b1a8527dd0c0b6ca90bd7 extends Twig_Template
{
    public function __construct(Twig_Environment $env)
    {
        parent::__construct($env);

        $this->parent = false;

        $this->blocks = array(
            'fos_user_content' => array($this, 'block_fos_user_content'),
        );
    }

    protected function doDisplay(array $context, array $blocks = array())
    {
        $__internal_dba29332523289305b524aa02b65cdc5bbca3fb6efbdc513d345b9889461d749 = $this->env->getExtension("Symfony\\Bridge\\Twig\\Extension\\ProfilerExtension");
        $__internal_dba29332523289305b524aa02b65cdc5bbca3fb6efbdc513d345b9889461d749->enter($__internal_dba29332523289305b524aa02b65cdc5bbca3fb6efbdc513d345b9889461d749_prof = new Twig_Profiler_Profile($this->getTemplateName(), "template", "SRVDVServerBundle:ChangePassword:changePassword.html.twig"));

        // line 1
        echo "

";
        // line 3
        $this->displayBlock('fos_user_content', $context, $blocks);
        
        $__internal_dba29332523289305b524aa02b65cdc5bbca3fb6efbdc513d345b9889461d749->leave($__internal_dba29332523289305b524aa02b65cdc5bbca3fb6efbdc513d345b9889461d749_prof);

    }

    public function block_fos_user_content($context, array $blocks = array())
    {
        $__internal_88a9de92c19248f7d08ce7d00e126ae41ccc9a58d5b4ae6e4fbe4c1d49a1a1db = $this->env->getExtension("Symfony\\Bridge\\Twig\\Extension\\ProfilerExtension");
        $__internal_88a9de92c19248f7d08ce7d00e126ae41ccc9a58d5b4ae6e4fbe4c1d49a1a1db->enter($__internal_88a9de92c19248f7d08ce7d00e126ae41ccc9a58d5b4ae6e4fbe4c1d49a1a1db_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "fos_user_content"));

        // line 4
        echo "    ";
        $this->loadTemplate("SRVDVServerBundle:ChangePassword:changePassword_content.html.twig", "SRVDVServerBundle:ChangePassword:changePassword.html.twig", 4)->display($context);
        
        $__internal_88a9de92c19248f7d08ce7d00e126ae41ccc9a58d5b4ae6e4fbe4c1d49a1a1db->leave($__internal_88a9de92c19248f7d08ce7d00e126ae41ccc9a58d5b4ae6e4fbe4c1d49a1a1db_prof);

    }

    public function getTemplateName()
    {
        return "SRVDVServerBundle:ChangePassword:changePassword.html.twig";
    }

    public function getDebugInfo()
    {
        return array (  39 => 4,  27 => 3,  23 => 1,);
    }

    /** @deprecated since 1.27 (to be removed in 2.0). Use getSourceContext() instead */
    public function getSource()
    {
        @trigger_error('The '.__METHOD__.' method is deprecated since version 1.27 and will be removed in 2.0. Use getSourceContext() instead.', E_USER_DEPRECATED);

        return $this->getSourceContext()->getCode();
    }

    public function getSourceContext()
    {
        return new Twig_Source("

{% block fos_user_content %}
    {% include \"SRVDVServerBundle:ChangePassword:changePassword_content.html.twig\" %}
{% endblock fos_user_content %}
", "SRVDVServerBundle:ChangePassword:changePassword.html.twig", "C:\\wamp64\\www\\serveurDeVoeux\\src\\SRVDV\\ServerBundle/Resources/views/ChangePassword/changePassword.html.twig");
    }
}
