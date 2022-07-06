<?php

/* @Twig/layout.html.twig */
class __TwigTemplate_14fd6edafabb0aabd141100bb5e628f42686e5ae0ca7d4ffd2d57185f3c83998 extends Twig_Template
{
    public function __construct(Twig_Environment $env)
    {
        parent::__construct($env);

        $this->parent = false;

        $this->blocks = array(
            'title' => array($this, 'block_title'),
            'head' => array($this, 'block_head'),
            'body' => array($this, 'block_body'),
        );
    }

    protected function doDisplay(array $context, array $blocks = array())
    {
        $__internal_bd021ee689e7f108e975c831b30940c30f24f507cd5c6c09fdac9f452821178d = $this->env->getExtension("Symfony\\Bundle\\WebProfilerBundle\\Twig\\WebProfilerExtension");
        $__internal_bd021ee689e7f108e975c831b30940c30f24f507cd5c6c09fdac9f452821178d->enter($__internal_bd021ee689e7f108e975c831b30940c30f24f507cd5c6c09fdac9f452821178d_prof = new Twig_Profiler_Profile($this->getTemplateName(), "template", "@Twig/layout.html.twig"));

        $__internal_836ecac9e6bfe9643e5ccbb971b50b6c2c11ce53f0cb07cf3e9a8b1a747ea468 = $this->env->getExtension("Symfony\\Bridge\\Twig\\Extension\\ProfilerExtension");
        $__internal_836ecac9e6bfe9643e5ccbb971b50b6c2c11ce53f0cb07cf3e9a8b1a747ea468->enter($__internal_836ecac9e6bfe9643e5ccbb971b50b6c2c11ce53f0cb07cf3e9a8b1a747ea468_prof = new Twig_Profiler_Profile($this->getTemplateName(), "template", "@Twig/layout.html.twig"));

        // line 1
        echo "<!DOCTYPE html>
<html>
    <head>
        <meta charset=\"";
        // line 4
        echo twig_escape_filter($this->env, $this->env->getCharset(), "html", null, true);
        echo "\" />
        <meta name=\"robots\" content=\"noindex,nofollow\" />
        <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\" />
        <title>";
        // line 7
        $this->displayBlock('title', $context, $blocks);
        echo "</title>
        <link rel=\"icon\" type=\"image/png\" href=\"";
        // line 8
        echo twig_include($this->env, $context, "@Twig/images/favicon.png.base64");
        echo "\">
        <style>";
        // line 9
        echo twig_include($this->env, $context, "@Twig/exception.css.twig");
        echo "</style>
        ";
        // line 10
        $this->displayBlock('head', $context, $blocks);
        // line 11
        echo "    </head>
    <body>
        <header>
            <div class=\"container\">
                <h1 class=\"logo\">";
        // line 15
        echo twig_include($this->env, $context, "@Twig/images/symfony-logo.svg");
        echo " Symfony Exception</h1>

                <div class=\"help-link\">
                    <a href=\"https://symfony.com/doc\">
                        <span class=\"icon\">";
        // line 19
        echo twig_include($this->env, $context, "@Twig/images/icon-book.svg");
        echo "</span>
                        <span class=\"hidden-xs-down\">Symfony</span> Docs
                    </a>
                </div>

                <div class=\"help-link\">
                    <a href=\"https://symfony.com/support\">
                        <span class=\"icon\">";
        // line 26
        echo twig_include($this->env, $context, "@Twig/images/icon-support.svg");
        echo "</span>
                        <span class=\"hidden-xs-down\">Symfony</span> Support
                    </a>
                </div>
            </div>
        </header>

        ";
        // line 33
        $this->displayBlock('body', $context, $blocks);
        // line 34
        echo "        ";
        echo twig_include($this->env, $context, "@Twig/base_js.html.twig");
        echo "
    </body>
</html>
";
        
        $__internal_bd021ee689e7f108e975c831b30940c30f24f507cd5c6c09fdac9f452821178d->leave($__internal_bd021ee689e7f108e975c831b30940c30f24f507cd5c6c09fdac9f452821178d_prof);

        
        $__internal_836ecac9e6bfe9643e5ccbb971b50b6c2c11ce53f0cb07cf3e9a8b1a747ea468->leave($__internal_836ecac9e6bfe9643e5ccbb971b50b6c2c11ce53f0cb07cf3e9a8b1a747ea468_prof);

    }

    // line 7
    public function block_title($context, array $blocks = array())
    {
        $__internal_1d56bc214c2d73e840233c1cc7a9f54d0ef7009a3649819141e2033ab26e2dae = $this->env->getExtension("Symfony\\Bundle\\WebProfilerBundle\\Twig\\WebProfilerExtension");
        $__internal_1d56bc214c2d73e840233c1cc7a9f54d0ef7009a3649819141e2033ab26e2dae->enter($__internal_1d56bc214c2d73e840233c1cc7a9f54d0ef7009a3649819141e2033ab26e2dae_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "title"));

        $__internal_f0483ee5b975d457eb1c9520efc9d5dfb2de2d30f49eada1b42042dfcfda52cd = $this->env->getExtension("Symfony\\Bridge\\Twig\\Extension\\ProfilerExtension");
        $__internal_f0483ee5b975d457eb1c9520efc9d5dfb2de2d30f49eada1b42042dfcfda52cd->enter($__internal_f0483ee5b975d457eb1c9520efc9d5dfb2de2d30f49eada1b42042dfcfda52cd_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "title"));

        
        $__internal_f0483ee5b975d457eb1c9520efc9d5dfb2de2d30f49eada1b42042dfcfda52cd->leave($__internal_f0483ee5b975d457eb1c9520efc9d5dfb2de2d30f49eada1b42042dfcfda52cd_prof);

        
        $__internal_1d56bc214c2d73e840233c1cc7a9f54d0ef7009a3649819141e2033ab26e2dae->leave($__internal_1d56bc214c2d73e840233c1cc7a9f54d0ef7009a3649819141e2033ab26e2dae_prof);

    }

    // line 10
    public function block_head($context, array $blocks = array())
    {
        $__internal_d0936ed7dee8802c04398ea89e43570e86d0c86502620722189f4c839d0c3c7c = $this->env->getExtension("Symfony\\Bundle\\WebProfilerBundle\\Twig\\WebProfilerExtension");
        $__internal_d0936ed7dee8802c04398ea89e43570e86d0c86502620722189f4c839d0c3c7c->enter($__internal_d0936ed7dee8802c04398ea89e43570e86d0c86502620722189f4c839d0c3c7c_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "head"));

        $__internal_a3844d16b28cca120098323b462eb07c41cb3be6a53afa92028597323ad17b8f = $this->env->getExtension("Symfony\\Bridge\\Twig\\Extension\\ProfilerExtension");
        $__internal_a3844d16b28cca120098323b462eb07c41cb3be6a53afa92028597323ad17b8f->enter($__internal_a3844d16b28cca120098323b462eb07c41cb3be6a53afa92028597323ad17b8f_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "head"));

        
        $__internal_a3844d16b28cca120098323b462eb07c41cb3be6a53afa92028597323ad17b8f->leave($__internal_a3844d16b28cca120098323b462eb07c41cb3be6a53afa92028597323ad17b8f_prof);

        
        $__internal_d0936ed7dee8802c04398ea89e43570e86d0c86502620722189f4c839d0c3c7c->leave($__internal_d0936ed7dee8802c04398ea89e43570e86d0c86502620722189f4c839d0c3c7c_prof);

    }

    // line 33
    public function block_body($context, array $blocks = array())
    {
        $__internal_1a560645990e8d2f4ef326fe57bf2b7a54040f72f295139c201a9f318733e6e6 = $this->env->getExtension("Symfony\\Bundle\\WebProfilerBundle\\Twig\\WebProfilerExtension");
        $__internal_1a560645990e8d2f4ef326fe57bf2b7a54040f72f295139c201a9f318733e6e6->enter($__internal_1a560645990e8d2f4ef326fe57bf2b7a54040f72f295139c201a9f318733e6e6_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "body"));

        $__internal_97d06a59813b0677cc197a22dd8fe89ef1aff8351c779aa8fa01b5f48d2d1b5b = $this->env->getExtension("Symfony\\Bridge\\Twig\\Extension\\ProfilerExtension");
        $__internal_97d06a59813b0677cc197a22dd8fe89ef1aff8351c779aa8fa01b5f48d2d1b5b->enter($__internal_97d06a59813b0677cc197a22dd8fe89ef1aff8351c779aa8fa01b5f48d2d1b5b_prof = new Twig_Profiler_Profile($this->getTemplateName(), "block", "body"));

        
        $__internal_97d06a59813b0677cc197a22dd8fe89ef1aff8351c779aa8fa01b5f48d2d1b5b->leave($__internal_97d06a59813b0677cc197a22dd8fe89ef1aff8351c779aa8fa01b5f48d2d1b5b_prof);

        
        $__internal_1a560645990e8d2f4ef326fe57bf2b7a54040f72f295139c201a9f318733e6e6->leave($__internal_1a560645990e8d2f4ef326fe57bf2b7a54040f72f295139c201a9f318733e6e6_prof);

    }

    public function getTemplateName()
    {
        return "@Twig/layout.html.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  137 => 33,  120 => 10,  103 => 7,  88 => 34,  86 => 33,  76 => 26,  66 => 19,  59 => 15,  53 => 11,  51 => 10,  47 => 9,  43 => 8,  39 => 7,  33 => 4,  28 => 1,);
    }

    /** @deprecated since 1.27 (to be removed in 2.0). Use getSourceContext() instead */
    public function getSource()
    {
        @trigger_error('The '.__METHOD__.' method is deprecated since version 1.27 and will be removed in 2.0. Use getSourceContext() instead.', E_USER_DEPRECATED);

        return $this->getSourceContext()->getCode();
    }

    public function getSourceContext()
    {
        return new Twig_Source("<!DOCTYPE html>
<html>
    <head>
        <meta charset=\"{{ _charset }}\" />
        <meta name=\"robots\" content=\"noindex,nofollow\" />
        <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\" />
        <title>{% block title %}{% endblock %}</title>
        <link rel=\"icon\" type=\"image/png\" href=\"{{ include('@Twig/images/favicon.png.base64') }}\">
        <style>{{ include('@Twig/exception.css.twig') }}</style>
        {% block head %}{% endblock %}
    </head>
    <body>
        <header>
            <div class=\"container\">
                <h1 class=\"logo\">{{ include('@Twig/images/symfony-logo.svg') }} Symfony Exception</h1>

                <div class=\"help-link\">
                    <a href=\"https://symfony.com/doc\">
                        <span class=\"icon\">{{ include('@Twig/images/icon-book.svg') }}</span>
                        <span class=\"hidden-xs-down\">Symfony</span> Docs
                    </a>
                </div>

                <div class=\"help-link\">
                    <a href=\"https://symfony.com/support\">
                        <span class=\"icon\">{{ include('@Twig/images/icon-support.svg') }}</span>
                        <span class=\"hidden-xs-down\">Symfony</span> Support
                    </a>
                </div>
            </div>
        </header>

        {% block body %}{% endblock %}
        {{ include('@Twig/base_js.html.twig') }}
    </body>
</html>
", "@Twig/layout.html.twig", "/Users/Rachid/SymfonyProjects/agi_protection/vendor/symfony/symfony/src/Symfony/Bundle/TwigBundle/Resources/views/layout.html.twig");
    }
}
