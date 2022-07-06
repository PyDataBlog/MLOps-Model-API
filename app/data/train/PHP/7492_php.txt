<?php

/* BundleProjectBundle:Comment:form.html.twig */
class __TwigTemplate_6774676db612523dd2bd82295b8275eb extends Twig_Template
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
        // line 2
        echo "
<form action=\"";
        // line 3
        echo twig_escape_filter($this->env, $this->env->getExtension('routing')->getPath("BundleProjectBundle_comment_create", array("place_id" => $this->getAttribute($this->getAttribute((isset($context["comment"]) ? $context["comment"] : $this->getContext($context, "comment")), "place"), "id"))), "html", null, true);
        echo "\" method=\"post\" ";
        echo $this->env->getExtension('form')->renderer->searchAndRenderBlock((isset($context["form"]) ? $context["form"] : $this->getContext($context, "form")), 'enctype');
        echo " class=\"global_forms\">
    ";
        // line 4
        echo $this->env->getExtension('form')->renderer->searchAndRenderBlock((isset($context["form"]) ? $context["form"] : $this->getContext($context, "form")), 'widget');
        echo "
    <p>
        <input type=\"submit\" value=\"Submit\">
    </p>
</form>";
    }

    public function getTemplateName()
    {
        return "BundleProjectBundle:Comment:form.html.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  28 => 4,  74 => 11,  45 => 5,  40 => 4,  22 => 3,  19 => 2,  398 => 152,  394 => 151,  388 => 149,  379 => 143,  367 => 133,  365 => 132,  362 => 131,  353 => 123,  340 => 115,  329 => 106,  326 => 105,  321 => 103,  317 => 102,  313 => 101,  309 => 100,  304 => 99,  299 => 97,  295 => 96,  291 => 95,  287 => 94,  282 => 93,  277 => 91,  273 => 90,  269 => 89,  265 => 88,  260 => 87,  255 => 85,  251 => 84,  247 => 83,  243 => 82,  238 => 81,  232 => 78,  228 => 77,  224 => 76,  220 => 75,  215 => 74,  213 => 73,  209 => 72,  205 => 71,  200 => 70,  197 => 69,  184 => 60,  180 => 58,  174 => 56,  172 => 55,  166 => 54,  162 => 53,  158 => 52,  154 => 51,  148 => 48,  142 => 47,  134 => 45,  128 => 42,  125 => 41,  119 => 38,  116 => 37,  114 => 36,  110 => 35,  101 => 29,  97 => 28,  90 => 24,  86 => 23,  79 => 19,  75 => 18,  71 => 17,  67 => 9,  60 => 15,  56 => 13,  51 => 6,  47 => 11,  36 => 4,  30 => 3,);
    }
}
