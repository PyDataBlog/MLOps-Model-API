<?php

/* AppBundle:Events:event_list.html.twig */
class __TwigTemplate_dbc7825bfa4a9158f7a4c20dd82f3fa099e324b04d5dd10b3813ab8ea5109eb9 extends Twig_Template
{
    public function __construct(Twig_Environment $env)
    {
        parent::__construct($env);

        // line 1
        $this->parent = $this->loadTemplate("AppBundle:Layouts:main_layout.html.twig", "AppBundle:Events:event_list.html.twig", 1);
        $this->blocks = array(
            'stylesheets' => array($this, 'block_stylesheets'),
            'content' => array($this, 'block_content'),
        );
    }

    protected function doGetParent(array $context)
    {
        return "AppBundle:Layouts:main_layout.html.twig";
    }

    protected function doDisplay(array $context, array $blocks = array())
    {
        $this->parent->display($context, array_merge($this->blocks, $blocks));
    }

    // line 2
    public function block_stylesheets($context, array $blocks = array())
    {
        // line 3
        echo "    ";
        $this->displayParentBlock("stylesheets", $context, $blocks);
        echo "
    
    <style>
        #events-table {
            text-align: center;
        }
        #events-table table {
            display: inline-block;
        }
        
        table, th, tr, td {
            border: 1px solid black;
        }
        
        th {
            padding: 0px 2px 0px;
        }
        td {
            padding: 0px 2px 0px;
        }
        
        #events-table a {
            color: blue;
        }
        
        .table-min-width {
            min-width: 15px;
        }
        
        .in-event {
            color: red !important;
        }
        
    </style>
    
";
    }

    // line 39
    public function block_content($context, array $blocks = array())
    {
        // line 40
        echo "    
    <div id=\"events-table\">
        <table>
            <tr>
                <th class=\"table-min-width\"></th>
                <th>Event</th>
                <th>Sport</th>
                <th>Teams</th>
            </tr>
            ";
        // line 49
        if ((twig_length_filter($this->env, (isset($context["events"]) ? $context["events"] : null)) > 0)) {
            // line 50
            echo "                ";
            $context['_parent'] = $context;
            $context['_seq'] = twig_ensure_traversable(range(0, (twig_length_filter($this->env, (isset($context["events"]) ? $context["events"] : null)) - 1)));
            foreach ($context['_seq'] as $context["_key"] => $context["i"]) {
                // line 51
                echo "                    ";
                $context["event"] = $this->getAttribute((isset($context["events"]) ? $context["events"] : null), $context["i"], array(), "array");
                // line 52
                echo "                    ";
                $context["eventId"] = $this->getAttribute((isset($context["event"]) ? $context["event"] : null), "id", array());
                // line 53
                echo "                    <tr>
                        <td class=\"table-min-width\">";
                // line 54
                echo twig_escape_filter($this->env, ($context["i"] + 1), "html", null, true);
                echo "</td>
                        ";
                // line 55
                if (twig_in_filter((isset($context["eventId"]) ? $context["eventId"] : null), (isset($context["teamEvents"]) ? $context["teamEvents"] : null))) {
                    echo " 
                            <td><a class=\"in-event\" href=\"";
                    // line 56
                    echo twig_escape_filter($this->env, $this->env->getExtension('routing')->getPath("event_view", array("eventId" => (isset($context["eventId"]) ? $context["eventId"] : null))), "html", null, true);
                    echo "\">";
                    echo twig_escape_filter($this->env, $this->getAttribute((isset($context["event"]) ? $context["event"] : null), "name", array()), "html", null, true);
                    echo "</a></td>
                        ";
                } else {
                    // line 58
                    echo "                             <td><a href=\"";
                    echo twig_escape_filter($this->env, $this->env->getExtension('routing')->getPath("event_view", array("eventId" => (isset($context["eventId"]) ? $context["eventId"] : null))), "html", null, true);
                    echo "\">";
                    echo twig_escape_filter($this->env, $this->getAttribute((isset($context["event"]) ? $context["event"] : null), "name", array()), "html", null, true);
                    echo "</a></td>
                        ";
                }
                // line 60
                echo "                         <td>";
                echo twig_escape_filter($this->env, $this->getAttribute($this->getAttribute((isset($context["event"]) ? $context["event"] : null), "sport", array()), "name", array()), "html", null, true);
                echo "</td>
                        <td>";
                // line 61
                echo twig_escape_filter($this->env, twig_length_filter($this->env, $this->getAttribute((isset($context["event"]) ? $context["event"] : null), "teams", array())), "html", null, true);
                echo "</td>

                    ";
                // line 63
                if ($this->env->getExtension('security')->isGranted("ROLE_ADMIN")) {
                    // line 64
                    echo "                        <td><a href=\"";
                    echo twig_escape_filter($this->env, $this->env->getExtension('routing')->getPath("event_delete", array("eventId" => (isset($context["eventId"]) ? $context["eventId"] : null))), "html", null, true);
                    echo "\">Delete</a></td>
                    ";
                } elseif ($this->getAttribute($this->getAttribute(                // line 65
(isset($context["app"]) ? $context["app"] : null), "user", array(), "any", false, true), "team", array(), "any", true, true)) {
                    // line 66
                    echo "                        ";
                    if (twig_in_filter((isset($context["eventId"]) ? $context["eventId"] : null), (isset($context["teamEvents"]) ? $context["teamEvents"] : null))) {
                        echo " 
                            <td><a href=\"";
                        // line 67
                        echo twig_escape_filter($this->env, $this->env->getExtension('routing')->getPath("event_leave", array("eventId" => (isset($context["eventId"]) ? $context["eventId"] : null))), "html", null, true);
                        echo "\">Leave</a></td>
                        ";
                    } else {
                        // line 69
                        echo "                            <td><a href=\"";
                        echo twig_escape_filter($this->env, $this->env->getExtension('routing')->getPath("event_join", array("eventId" => (isset($context["eventId"]) ? $context["eventId"] : null))), "html", null, true);
                        echo "\">Join</a></td>
                        ";
                    }
                    // line 71
                    echo "                    ";
                }
                // line 72
                echo "                    </tr>
                ";
            }
            $_parent = $context['_parent'];
            unset($context['_seq'], $context['_iterated'], $context['_key'], $context['i'], $context['_parent'], $context['loop']);
            $context = array_intersect_key($context, $_parent) + $_parent;
            // line 74
            echo "            ";
        }
        // line 75
        echo "        </table>
        ";
        // line 76
        if ($this->env->getExtension('security')->isGranted("ROLE_ADMIN")) {
            // line 77
            echo "            <div>
                <a href=\"";
            // line 78
            echo $this->env->getExtension('routing')->getPath("event_create");
            echo "\">Create new event</a>
            </div>
        ";
        }
        // line 81
        echo "    </div>
";
    }

    public function getTemplateName()
    {
        return "AppBundle:Events:event_list.html.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  188 => 81,  182 => 78,  179 => 77,  177 => 76,  174 => 75,  171 => 74,  164 => 72,  161 => 71,  155 => 69,  150 => 67,  145 => 66,  143 => 65,  138 => 64,  136 => 63,  131 => 61,  126 => 60,  118 => 58,  111 => 56,  107 => 55,  103 => 54,  100 => 53,  97 => 52,  94 => 51,  89 => 50,  87 => 49,  76 => 40,  73 => 39,  32 => 3,  29 => 2,  11 => 1,);
    }
}
/* {% extends 'AppBundle:Layouts:main_layout.html.twig' %}*/
/* {% block stylesheets %}*/
/*     {{ parent() }}*/
/*     */
/*     <style>*/
/*         #events-table {*/
/*             text-align: center;*/
/*         }*/
/*         #events-table table {*/
/*             display: inline-block;*/
/*         }*/
/*         */
/*         table, th, tr, td {*/
/*             border: 1px solid black;*/
/*         }*/
/*         */
/*         th {*/
/*             padding: 0px 2px 0px;*/
/*         }*/
/*         td {*/
/*             padding: 0px 2px 0px;*/
/*         }*/
/*         */
/*         #events-table a {*/
/*             color: blue;*/
/*         }*/
/*         */
/*         .table-min-width {*/
/*             min-width: 15px;*/
/*         }*/
/*         */
/*         .in-event {*/
/*             color: red !important;*/
/*         }*/
/*         */
/*     </style>*/
/*     */
/* {% endblock %}*/
/* {% block content %}*/
/*     */
/*     <div id="events-table">*/
/*         <table>*/
/*             <tr>*/
/*                 <th class="table-min-width"></th>*/
/*                 <th>Event</th>*/
/*                 <th>Sport</th>*/
/*                 <th>Teams</th>*/
/*             </tr>*/
/*             {% if events|length > 0 %}*/
/*                 {% for i in 0..events|length - 1 %}*/
/*                     {% set event = events[i] %}*/
/*                     {% set eventId = event.id %}*/
/*                     <tr>*/
/*                         <td class="table-min-width">{{ i+1 }}</td>*/
/*                         {% if eventId in teamEvents %} */
/*                             <td><a class="in-event" href="{{ path('event_view', {'eventId': eventId}) }}">{{ event.name }}</a></td>*/
/*                         {% else %}*/
/*                              <td><a href="{{ path('event_view', {'eventId': eventId}) }}">{{ event.name }}</a></td>*/
/*                         {% endif %}*/
/*                          <td>{{ event.sport.name }}</td>*/
/*                         <td>{{ event.teams|length }}</td>*/
/* */
/*                     {% if is_granted('ROLE_ADMIN') %}*/
/*                         <td><a href="{{ path('event_delete', {'eventId': eventId}) }}">Delete</a></td>*/
/*                     {% elseif app.user.team is defined %}*/
/*                         {% if eventId in teamEvents %} */
/*                             <td><a href="{{ path('event_leave', {'eventId': eventId}) }}">Leave</a></td>*/
/*                         {% else %}*/
/*                             <td><a href="{{ path('event_join', {'eventId' : eventId}) }}">Join</a></td>*/
/*                         {% endif %}*/
/*                     {% endif %}*/
/*                     </tr>*/
/*                 {% endfor %}*/
/*             {% endif %}*/
/*         </table>*/
/*         {% if is_granted('ROLE_ADMIN') %}*/
/*             <div>*/
/*                 <a href="{{ path('event_create') }}">Create new event</a>*/
/*             </div>*/
/*         {% endif %}*/
/*     </div>*/
/* {% endblock %}*/
