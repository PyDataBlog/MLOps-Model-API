<?php

/* ApplicationMyVedhikaBundle:Registration:registration.html.twig */
class __TwigTemplate_601020e7a4f0477e30cb8e7a8e7b10410de6cb57ff4ea8abba5a232093ef6e60 extends Twig_Template
{
    public function __construct(Twig_Environment $env)
    {
        parent::__construct($env);

        $this->parent = $this->env->loadTemplate("base.html.twig");

        $this->blocks = array(
            'body' => array($this, 'block_body'),
        );
    }

    protected function doGetParent(array $context)
    {
        return "base.html.twig";
    }

    protected function doDisplay(array $context, array $blocks = array())
    {
        $this->parent->display($context, array_merge($this->blocks, $blocks));
    }

    // line 3
    public function block_body($context, array $blocks = array())
    {
        // line 4
        echo "
          <div class=\"form-box\" id=\"login-box\">
            <div class=\"header\" style=\"background-color:gray\">Register New Membership</div>
            <form action=\"../../index.html\" method=\"post\">
                <div class=\"body bg-gray\" style=\"background-color:gray\">
                    <div class=\"form-group\">
                        <input type=\"text\" name=\"name\" class=\"form-control\" placeholder=\"Full name\"/>
                    </div>
                    <div class=\"form-group\">
                        <input type=\"text\" name=\"userid\" class=\"form-control\" placeholder=\"User ID\"/>
                    </div>
                    <div class=\"form-group\">
                        <input type=\"password\" name=\"password\" class=\"form-control\" placeholder=\"Password\"/>
                    </div>
                    <div class=\"form-group\">
                        <input type=\"password\" name=\"password2\" class=\"form-control\" placeholder=\"Retype password\"/>
                    </div>
                </div>
                <div class=\"footer\" style=\"background-color:gray\">                    

                    <button type=\"submit\" class=\"btn btn-block\" style=\"background-color:lightgray\">Sign me up</button>

                    <a href=\"login.html\" class=\"text-center\" style=\"color:white\">I already have a membership</a>
                </div>
            </form>

           ";
        // line 38
        echo "        </div>

";
    }

    public function getTemplateName()
    {
        return "ApplicationMyVedhikaBundle:Registration:registration.html.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  59 => 38,  31 => 4,  28 => 3,);
    }
}
