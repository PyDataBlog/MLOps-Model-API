<?php

/* CCDNForumForumBundle:Common:Layout/Sidebar/user_forum.html.twig */
class __TwigTemplate_bb44f10f11ff92ffd7da83e9c97335883959fc7f73189862ce492aba2682a1a6 extends Twig_Template
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
        ob_start();
        // line 3
        echo "<ul class=\"nav nav-pills nav-stacked\">";
        // line 5
        $context["route"] = $this->getAttribute($this->getAttribute($this->getAttribute((isset($context["app"]) ? $context["app"] : $this->getContext($context, "app")), "request"), "attributes"), "get", array(0 => "_route"), "method");
        // line 7
        if ((!array_key_exists("topic", $context))) {
            // line 8
            if (array_key_exists("post", $context)) {
                // line 9
                $context["topic"] = $this->getAttribute((isset($context["post"]) ? $context["post"] : $this->getContext($context, "post")), "getTopic");
            }
        }
        // line 13
        if ((!array_key_exists("board", $context))) {
            // line 14
            if (array_key_exists("topic", $context)) {
                // line 15
                $context["board"] = $this->getAttribute((isset($context["topic"]) ? $context["topic"] : $this->getContext($context, "topic")), "getBoard");
            } else {
                // line 17
                if (array_key_exists("post", $context)) {
                    // line 18
                    if ($this->getAttribute((isset($context["post"]) ? $context["post"] : null), "getTopic", array(), "any", true, true)) {
                        // line 19
                        $context["board"] = $this->getAttribute($this->getAttribute((isset($context["post"]) ? $context["post"] : $this->getContext($context, "post")), "getTopic"), "getBoard");
                    }
                }
            }
        }
        // line 25
        if ((!array_key_exists("category", $context))) {
            // line 26
            if (array_key_exists("board", $context)) {
                // line 27
                $context["category"] = $this->getAttribute((isset($context["board"]) ? $context["board"] : $this->getContext($context, "board")), "getCategory");
            } else {
                // line 29
                if (array_key_exists("topic", $context)) {
                    // line 30
                    $context["category"] = $this->getAttribute((isset($context["topic"]) ? $context["topic"] : $this->getContext($context, "topic")), "getBoard");
                }
            }
        }
        // line 37
        if ((array_key_exists("forum", $context) && (!null))) {
            // line 38
            if ((((isset($context["route"]) ? $context["route"] : $this->getContext($context, "route")) == "ccdn_homepage") || ((isset($context["route"]) ? $context["route"] : $this->getContext($context, "route")) == "ccdn_forum_user_category_index"))) {
                // line 40
                echo "<li class=\"active\">
\t\t\t\t\t<a href=\"";
                // line 41
                echo twig_escape_filter($this->env, $this->env->getExtension('routing')->getPath("ccdn_forum_user_category_index", array("forumName" => (isset($context["forumName"]) ? $context["forumName"] : $this->getContext($context, "forumName")))), "html", null, true);
                echo "\">
\t\t\t\t\t\t<i class=\"glyphicon glyphicon-home\"></i>";
            } else {
                // line 44
                echo "<li>
\t\t\t\t\t<a href=\"";
                // line 45
                echo twig_escape_filter($this->env, $this->env->getExtension('routing')->getPath("ccdn_forum_user_category_index", array("forumName" => (isset($context["forumName"]) ? $context["forumName"] : $this->getContext($context, "forumName")))), "html", null, true);
                echo "\">
\t\t\t\t\t\t<i class=\"glyphicon glyphicon-home\"></i>";
            }
            // line 48
            echo twig_escape_filter($this->env, $this->env->getExtension('translator')->trans("link.category.index", array(), "CCDNForumForumBundle"), "html", null, true);
            // line 49
            echo "</a>
\t\t\t\t</li>";
            // line 52
            if ((twig_slice($this->env, (isset($context["route"]) ? $context["route"] : $this->getContext($context, "route")), 0, 28) != "ccdn_forum_user_subscription")) {
                // line 53
                $this->env->loadTemplate("CCDNForumForumBundle:Common:Layout/Sidebar/Partial/user_boards.html.twig")->display($context);
            }
        }
        // line 57
        echo "</ul>";
        echo trim(preg_replace('/>\s+</', '><', ob_get_clean()));
    }

    public function getTemplateName()
    {
        return "CCDNForumForumBundle:Common:Layout/Sidebar/user_forum.html.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  95 => 57,  89 => 52,  86 => 49,  84 => 48,  79 => 45,  76 => 44,  71 => 41,  68 => 40,  66 => 38,  64 => 37,  59 => 30,  57 => 29,  52 => 26,  50 => 25,  40 => 17,  37 => 15,  33 => 13,  29 => 9,  25 => 7,  23 => 5,  21 => 3,  19 => 1,  93 => 26,  91 => 53,  87 => 22,  74 => 20,  56 => 19,  54 => 27,  51 => 16,  46 => 12,  44 => 19,  42 => 18,  39 => 9,  35 => 14,  32 => 5,  27 => 8,);
    }
}
