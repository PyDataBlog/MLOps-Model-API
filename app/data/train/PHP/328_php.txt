<?php

/* TwigBundle:Exception:exception.json.twig */
class __TwigTemplate_8d9b12e119daada0de361618d51aaa01 extends Twig_Template
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
        // line 1
        if (isset($context["exception"])) { $_exception_ = $context["exception"]; } else { $_exception_ = null; }
        echo twig_jsonencode_filter($this->getAttribute($_exception_, "toarray"));
        echo "
";
    }

    public function getTemplateName()
    {
        return "TwigBundle:Exception:exception.json.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  47 => 8,  30 => 4,  27 => 3,  22 => 4,  117 => 22,  112 => 21,  109 => 20,  104 => 19,  96 => 18,  84 => 14,  80 => 12,  68 => 9,  44 => 7,  26 => 4,  23 => 3,  20 => 2,  17 => 1,  92 => 39,  86 => 6,  79 => 40,  57 => 22,  46 => 7,  37 => 8,  33 => 7,  29 => 6,  24 => 4,  19 => 1,  144 => 54,  138 => 50,  130 => 46,  124 => 24,  121 => 41,  115 => 40,  111 => 38,  108 => 37,  99 => 32,  94 => 29,  91 => 17,  88 => 16,  85 => 26,  77 => 39,  74 => 20,  71 => 19,  65 => 16,  62 => 15,  58 => 8,  54 => 11,  51 => 10,  42 => 9,  38 => 8,  35 => 5,  31 => 4,  28 => 3,);
    }
}
