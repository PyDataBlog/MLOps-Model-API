<?php

/* AdminBundle::layoutSUP.html.twig */
class __TwigTemplate_66433e67719e263cb349ed4744109ccf2266f78b6012a7d47e226769c3932f4d extends Twig_Template
{
    public function __construct(Twig_Environment $env)
    {
        parent::__construct($env);

        $this->parent = false;

        $this->blocks = array(
            'user_content' => array($this, 'block_user_content'),
        );
    }

    protected function doDisplay(array $context, array $blocks = array())
    {
        // line 1
        echo "<!DOCTYPE html>
<html>
    <head>
        <meta charset=\"UTF-8\">
        <title>AfrikIsol | Dashboard</title>
        <meta content='width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no' name='viewport'>
        <!-- Bootstrap 3.3.4 -->
        <link href=\"";
        // line 8
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("bootstrap/css/bootstrap.min.css"), "html", null, true);
        echo "\" rel=\"stylesheet\" type=\"text/css\" />    
        <!-- FontAwesome 4.3.0 -->
        <link href=\"https://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css\" rel=\"stylesheet\" type=\"text/css\" />
        <!-- Ionicons 2.0.0 -->
        <link href=\"https://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css\" rel=\"stylesheet\" type=\"text/css\" />    
        <!-- Theme style -->
        <link href=\"";
        // line 14
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("dist/css/AdminLTE.min.css"), "html", null, true);
        echo "\" rel=\"stylesheet\" type=\"text/css\" />
       <!-- DATA TABLES -->
        <link href=\"";
        // line 16
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/datatables/dataTables.bootstrap.css"), "html", null, true);
        echo "\" rel=\"stylesheet\" type=\"text/css\" />
        
        <link href=\"";
        // line 18
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("dist/css/skins/_all-skins.min.css"), "html", null, true);
        echo "\" rel=\"stylesheet\" type=\"text/css\" />
        <!-- iCheck -->
        <link href=\"";
        // line 20
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/iCheck/flat/blue.css"), "html", null, true);
        echo "\" rel=\"stylesheet\" type=\"text/css\" />
        <!-- Morris chart -->
        <link href=\"";
        // line 22
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/morris/morris.css"), "html", null, true);
        echo "\" rel=\"stylesheet\" type=\"text/css\" />
        <!-- jvectormap -->
        <link href=\"";
        // line 24
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/jvectormap/jquery-jvectormap-1.2.2.css"), "html", null, true);
        echo "\" rel=\"stylesheet\" type=\"text/css\" />
        <!-- Date Picker -->
        <link href=\"";
        // line 26
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/datepicker/datepicker3.css"), "html", null, true);
        echo "\" rel=\"stylesheet\" type=\"text/css\" />
        <!-- Daterange picker -->
        <link href=\"";
        // line 28
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/daterangepicker/daterangepicker-bs3.css"), "html", null, true);
        echo "\" rel=\"stylesheet\" type=\"text/css\" />
        <!-- bootstrap wysihtml5 - text editor -->
        <link href=\"";
        // line 30
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/bootstrap-wysihtml5/bootstrap3-wysihtml5.min.css"), "html", null, true);
        echo "\" rel=\"stylesheet\" type=\"text/css\" />
         <script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js\" type=\"text/javascript\"></script>
          <script type=\"text/javascript\">
      \$(function () {
        \$(\"#example1\").dataTable();
        \$('#example2').dataTable({
          \"bPaginate\": true,
          \"bLengthChange\": false,
          \"bFilter\": false,
          \"bSort\": true,
          \"bInfo\": true,
          \"bAutoWidth\": false
        });
      });
    </script>

    </head>
    <body class=\"skin-blue sidebar-mini\">
        <div class=\"wrapper\">

            <header class=\"main-header\">
                <!-- Logo -->
                <a href=\"\" class=\"logo\" height=\"40\" width=\"40\">
                    <!-- mini logo for sidebar mini 50x50 pixels -->
                    <span class=\"logo-mini\"><b>A</b>AI</span>
                    <!-- logo for regular state and mobile devices -->
                    <span class=\"logo-lg\"><b>Admin</b>AfrikIsol<br></span>
                    
                </a>
                
                <!-- Header Navbar: style can be found in header.less -->
                <nav class=\"navbar navbar-static-top\" role=\"navigation\">
                    <!-- Sidebar toggle button-->
                    <a href=\"#\" class=\"sidebar-toggle\" data-toggle=\"offcanvas\" role=\"button\">
                        <span class=\"sr-only\">Toggle navigation</span>
                    </a>
                    <div class=\"navbar-custom-menu\">
                        <ul class=\"nav navbar-nav\">
                            <li class=\"dropdown user user-menu\">
                                ";
        // line 69
        $context["imag"] = $this->env->getExtension('img_extension')->afficheImg($this->getAttribute((isset($context["app"]) ? $context["app"] : $this->getContext($context, "app")), "user", array()));
        // line 70
        echo "                                <a href=\"";
        echo $this->env->getExtension('routing')->getPath("fos_user_security_logout");
        echo "\" class=\"dropdown-toggle\" >
                                    <img src=\"data:image/png;base64,";
        // line 71
        echo twig_escape_filter($this->env, (isset($context["imag"]) ? $context["imag"] : $this->getContext($context, "imag")), "html", null, true);
        echo "\" class=\"user-image\" alt=\"User Image\" />
                                    <span class=\"hidden-xs\">Déconnexion</span>
                                </a>

                            </li>
                            <!-- Control Sidebar Toggle Button 
                            <li>
                              <a href=\"#\" data-toggle=\"control-sidebar\"><i class=\"fa fa-gears\"></i></a>
                            </li>
                            -->
                        </ul>
                    </div>
                </nav>
            </header>
            <!-- Left side column. contains the logo and sidebar -->
            <aside class=\"main-sidebar\">
                <!-- sidebar: style can be found in sidebar.less -->
                <section class=\"sidebar\">
                    <!-- Sidebar user panel -->
                    <div class=\"user-panel\">
                        <div class=\"pull-left image\">
                            
                            <img src=\"data:image/png;base64,";
        // line 93
        echo twig_escape_filter($this->env, (isset($context["imag"]) ? $context["imag"] : $this->getContext($context, "imag")), "html", null, true);
        echo "\" class=\"img-circle\" alt=\"User Image\" />                                  
                      
                        </div>
                        <div class=\"pull-left info\">
                            <p>";
        // line 97
        echo twig_escape_filter($this->env, $this->getAttribute($this->getAttribute((isset($context["app"]) ? $context["app"] : $this->getContext($context, "app")), "user", array()), "username", array()), "html", null, true);
        echo " </p>
                            <a href=\"#\"><i class=\"fa fa-circle text-success\"></i> Online</a>
                        </div>
                    </div>
                    <!-- search form -->
                    <form action=\"#\" method=\"get\" class=\"sidebar-form\">
                        <div class=\"input-group\">
                            <input type=\"text\" name=\"q\" class=\"form-control\" placeholder=\"Search...\"/>
                            <span class=\"input-group-btn\">
                                <button type='submit' name='search' id='search-btn' class=\"btn btn-flat\"><i class=\"fa fa-search\"></i></button>
                            </span>
                        </div>
                    </form>
                    <!-- /.search form -->
                    <!-- sidebar menu: : style can be found in sidebar.less -->
                    <ul class=\"sidebar-menu\">
                        <li class=\"header\">Menu de navigation</li>
                        <li class=\"active treeview\">
                            <a href=\"#\">
                                <i class=\"fa fa-desktop\"></i> <span>Gestion des comptes</span> <i class=\"fa fa-angle-left pull-right\"></i>
                            </a>
                            <ul class=\"treeview-menu\">
                                <li class=\"active\">
                                    <a href=\"";
        // line 120
        echo $this->env->getExtension('routing')->getPath("fos_user_profile_show");
        echo "\"><i class=\"fa fa-user-secret\"></i> Mon compte<i class=\"fa fa-angle-left pull-right\"></i></a>
                                    <ul class=\"treeview-menu\">
                                        <li><a href=\"";
        // line 122
        echo $this->env->getExtension('routing')->getPath("admin_updateprofile");
        echo "\"><i class=\"fa fa-wrench\"></i> Modifier infos</a></li>
                                        <li><a href=\"";
        // line 123
        echo $this->env->getExtension('routing')->getPath("fos_user_change_password");
        echo "\"><i class=\"fa fa-lock\"></i> Changer mot de passe</a></li>
                                    </ul>
                                </li>
                                <li>
                                    <a href=\"index2.html\"><i class=\"fa fa-users\"></i> Tous les utilisateurs<i class=\"fa fa-angle-left pull-right\"></i></a>
                                    <ul class=\"treeview-menu\">
                                        <li><a href=\"";
        // line 129
        echo $this->env->getExtension('routing')->getPath("fos_user_registration_register");
        echo "\"><i class=\"fa fa-user-plus\"></i> Créer</a></li>
                                        <li><a href=\"";
        // line 130
        echo $this->env->getExtension('routing')->getPath("admin_listerUser");
        echo "\"><i class=\"fa fa-list-alt\"></i> Lister</a></li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                         <li class=\"active treeview\">
                            <a href=\"#\">
                                <i class=\"fa fa-slideshare\"></i> <span>Gestion des Clients</span> <i class=\"fa fa-angle-left pull-right\"></i>
                            </a>
                            <ul class=\"treeview-menu\">
                                <li class=\"active\">
                                    <a href=\"";
        // line 141
        echo $this->env->getExtension('routing')->getPath("tech_addClient");
        echo "\"><i class=\"fa fa-plus\"></i> Ajouter Client</a>
                                  
                                </li>
                                <li>
                                    <a href=\"";
        // line 145
        echo $this->env->getExtension('routing')->getPath("tech_listClient");
        echo "\"><i class=\"fa fa-th-list\"></i> Lister Clients</a>
                                   
                                </li>
                               
                            </ul>
                        </li>
                        <li class=\"active treeview\">
                            <a href=\"#\">
                                <i class=\"fa fa-clipboard\"></i> <span>Gestion des Projets</span> <i class=\"fa fa-angle-left pull-right\"></i>
                            </a>
                            <ul class=\"treeview-menu\">
                                <li class=\"active\">
                                    <a href=\"";
        // line 157
        echo $this->env->getExtension('routing')->getPath("tech_addProjet");
        echo "\"><i class=\"fa fa-plus-square-o\"></i> Ajouter Projet</a>
                                  
                                </li>
                                <li>
                                    <a href=\"";
        // line 161
        echo $this->env->getExtension('routing')->getPath("tech_listProjet");
        echo "\"><i class=\"fa fa-list-ul\"></i> Lister projets</a>
                                   
                                </li>
                                <li>
                                  <a href=\"";
        // line 165
        echo $this->env->getExtension('routing')->getPath("tech_listGantt");
        echo "\"><i class=\"fa fa-bar-chart\"></i>Planification des travaux</a>
                                </li> 
                                <li>
                                   <a href=\"";
        // line 168
        echo $this->env->getExtension('routing')->getPath("tech_listTole");
        echo "\"><i class=\"fa fa-edit\"></i> Ajouter/Lister tôles</a>  
                                </li>
                                <li>
                                   <a href=\"";
        // line 171
        echo $this->env->getExtension('routing')->getPath("tech_listPlan");
        echo "\"><i class=\"fa fa-area-chart\"></i>Planification </a> 
                                </li>
                                <li>
                                  <a href=\"";
        // line 174
        echo $this->env->getExtension('routing')->getPath("tech_listAvancement");
        echo "\"><i class=\"fa fa-area-chart\"></i>Avancement</a>   
                                </li>
                                <li>
                                   <a href=\"";
        // line 177
        echo $this->env->getExtension('routing')->getPath("tech_listMAD");
        echo "\"><i class=\"fa fa-area-chart\"></i>Mise à disposition</a>     
                                </li>
                             
                            </ul>
                        </li>

                        <li class=\"active treeview\">
                            <a href=\"#\">
                                <i class=\"fa fa-database\"></i> <span>Gestion de Stock</span> <i class=\"fa fa-angle-left pull-right\"></i>
                            </a>
                            <ul class=\"treeview-menu\">
                                <li class=\"active\">
                                    <a href=\"";
        // line 189
        echo $this->env->getExtension('routing')->getPath("log_addStock");
        echo "\"><i class=\"fa fa-calculator\"></i> Ajouter au stock</a>
                                  
                                </li>
                                <li>
                                    <a href=\"";
        // line 193
        echo $this->env->getExtension('routing')->getPath("log_listStock");
        echo "\"><i class=\"fa fa-list\"></i> Lister Stock</a>
                                   
                                </li>
                                <li>
                                    <a href=\"";
        // line 197
        echo $this->env->getExtension('routing')->getPath("log_listDmd");
        echo "\"><i class=\"fa fa-list\"></i> Demandes <span class=\"label label-primary pull-right\">";
        echo twig_escape_filter($this->env, $this->env->getExtension('dmd_extension')->calcul(0), "html", null, true);
        echo "</span></a>
                                   
                                </li>
                            </ul>
                        </li>
                    </ul>
                </section>
                <!-- /.sidebar -->
            </aside>

            <!-- Content Wrapper. Contains page content -->
            <div class=\"content-wrapper\">
                <!-- Content Header (Page header) -->
                <section class=\"content-header\">
                    <h1>
                        Tableau de bord
                        <small>Panneau de contrôle</small>
                    </h1>
                    <ol class=\"breadcrumb\">
                        <li><a href=\"#\"><i class=\"fa fa-dashboard\"></i> Accueil</a></li>
                        <li class=\"active\">Tableau de bord</li>
                    </ol>
                </section>

                <!-- Main content -->
                <section class=\"content\">
                    <br>
                    <br>
                    <br>
                    <br>
                    ";
        // line 227
        $this->displayBlock('user_content', $context, $blocks);
        // line 242
        echo "                         
                         <br>
                         <br>
                         ";
        // line 245
        if ($this->getAttribute($this->getAttribute((isset($context["app"]) ? $context["app"] : $this->getContext($context, "app")), "request", array()), "hasPreviousSession", array())) {
            // line 246
            echo "            ";
            $context['_parent'] = (array) $context;
            $context['_seq'] = twig_ensure_traversable($this->getAttribute($this->getAttribute($this->getAttribute((isset($context["app"]) ? $context["app"] : $this->getContext($context, "app")), "session", array()), "flashbag", array()), "all", array(), "method"));
            foreach ($context['_seq'] as $context["type"] => $context["messages"]) {
                // line 247
                echo "                ";
                $context['_parent'] = (array) $context;
                $context['_seq'] = twig_ensure_traversable($context["messages"]);
                foreach ($context['_seq'] as $context["_key"] => $context["message"]) {
                    // line 248
                    echo "                    <div class=\"flash-";
                    echo twig_escape_filter($this->env, $context["type"], "html", null, true);
                    echo "\">
                        <h3> ";
                    // line 249
                    echo twig_escape_filter($this->env, $context["message"], "html", null, true);
                    echo " </h3>
                    </div>
                ";
                }
                $_parent = $context['_parent'];
                unset($context['_seq'], $context['_iterated'], $context['_key'], $context['message'], $context['_parent'], $context['loop']);
                $context = array_intersect_key($context, $_parent) + $_parent;
                // line 252
                echo "            ";
            }
            $_parent = $context['_parent'];
            unset($context['_seq'], $context['_iterated'], $context['type'], $context['messages'], $context['_parent'], $context['loop']);
            $context = array_intersect_key($context, $_parent) + $_parent;
            // line 253
            echo "        ";
        }
        // line 254
        echo "                </section><!-- /.content -->
            </div><!-- /.content-wrapper -->
            <footer class=\"main-footer\">

                <strong>Copyright &copy; 2014-2015 <a href=\"\">Ali Brahem</a>.</strong> All rights reserved.
            </footer>

            <!-- Control Sidebar -->      

            <!-- Add the sidebar's background. This div must be placed
                 immediately after the control sidebar -->
            <div class='control-sidebar-bg'></div>
        </div><!-- ./wrapper -->

        <!-- jQuery 2.1.4 -->
        <script src=\"";
        // line 269
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/jQuery/jQuery-2.1.4.min.js"), "html", null, true);
        echo "\"></script>
        <!-- jQuery UI 1.11.2 -->
        <script src=\"http://code.jquery.com/ui/1.11.2/jquery-ui.min.js\" type=\"text/javascript\"></script>
        <!-- Resolve conflict in jQuery UI tooltip with Bootstrap tooltip -->
        <script>
            \$.widget.bridge('uibutton', \$.ui.button);
        </script>
        <!-- Bootstrap 3.3.2 JS -->
        <script src=\"";
        // line 277
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("bootstrap/js/bootstrap.min.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>    
        <!-- Morris.js charts -->
        <script src=\"http://cdnjs.cloudflare.com/ajax/libs/raphael/2.1.0/raphael-min.js\"></script>
        <script src=\"";
        // line 280
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/morris/morris.min.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
        <!-- Sparkline -->
        <script src=\"";
        // line 282
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/sparkline/jquery.sparkline.min.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
        <!-- jvectormap -->
        <script src=\"";
        // line 284
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/jvectormap/jquery-jvectormap-1.2.2.min.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
        <script src=\"";
        // line 285
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/jvectormap/jquery-jvectormap-world-mill-en.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
        <!-- jQuery Knob Chart -->
        <script src=\"";
        // line 287
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/knob/jquery.knob.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
        <!-- daterangepicker -->
        <script src=\"https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.10.2/moment.min.js\" type=\"text/javascript\"></script>
        <script src=\"";
        // line 290
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/daterangepicker/daterangepicker.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
        <!-- datepicker -->
        <script src=\"";
        // line 292
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/datepicker/bootstrap-datepicker.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
        <!-- Bootstrap WYSIHTML5 -->
        <script src=\"";
        // line 294
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/bootstrap-wysihtml5/bootstrap3-wysihtml5.all.min.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
        <!-- Slimscroll -->
        <script src=\"";
        // line 296
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/slimScroll/jquery.slimscroll.min.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
        <!-- FastClick -->
        <script src=\"";
        // line 298
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/fastclick/fastclick.min.js"), "html", null, true);
        echo "\"></script>
        <!-- AdminLTE App -->
        <script src=\"";
        // line 300
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("dist/js/app.min.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>    
        <script src=\"";
        // line 301
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("bootstrap/js/calculTole.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
        <script src=\"";
        // line 302
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("bootstrap/js/avancement.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
        <script src=\"";
        // line 303
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("bootstrap/js/stock.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
        <!-- AdminLTE dashboard demo (This is only for demo purposes) -->
        <script src=\"";
        // line 305
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("dist/js/pages/dashboard.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>   
        <!-- DATA TABES SCRIPT -->
    <script src=\"";
        // line 307
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/datatables/jquery.dataTables.min.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
    <script src=\"";
        // line 308
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("plugins/datatables/dataTables.bootstrap.min.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
     <link href=\"";
        // line 309
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("bootstrap/css/loading.css"), "html", null, true);
        echo "\" rel=\"stylesheet\" type=\"text/css\" />
        
        <!-- AdminLTE for demo purposes -->
        <script src=\"";
        // line 312
        echo twig_escape_filter($this->env, $this->env->getExtension('assets')->getAssetUrl("dist/js/demo.js"), "html", null, true);
        echo "\" type=\"text/javascript\"></script>
    </body>
</html>";
    }

    // line 227
    public function block_user_content($context, array $blocks = array())
    {
        // line 228
        echo "           
                    <!-- Small boxes (Stat box) -->
                    <div class=\"row\">
                        <div class=\"col-lg-3 col-xs-6\">
                            <!-- small box -->

                        </div><!-- ./col -->

                        <!-- Main row -->
                        <div class=\"row\">
                            
                        </div><!-- /.row (main row) -->
                        
                         ";
    }

    public function getTemplateName()
    {
        return "AdminBundle::layoutSUP.html.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  517 => 228,  514 => 227,  507 => 312,  501 => 309,  497 => 308,  493 => 307,  488 => 305,  483 => 303,  479 => 302,  475 => 301,  471 => 300,  466 => 298,  461 => 296,  456 => 294,  451 => 292,  446 => 290,  440 => 287,  435 => 285,  431 => 284,  426 => 282,  421 => 280,  415 => 277,  404 => 269,  387 => 254,  384 => 253,  378 => 252,  369 => 249,  364 => 248,  359 => 247,  354 => 246,  352 => 245,  347 => 242,  345 => 227,  310 => 197,  303 => 193,  296 => 189,  281 => 177,  275 => 174,  269 => 171,  263 => 168,  257 => 165,  250 => 161,  243 => 157,  228 => 145,  221 => 141,  207 => 130,  203 => 129,  194 => 123,  190 => 122,  185 => 120,  159 => 97,  152 => 93,  127 => 71,  122 => 70,  120 => 69,  78 => 30,  73 => 28,  68 => 26,  63 => 24,  58 => 22,  53 => 20,  48 => 18,  43 => 16,  38 => 14,  29 => 8,  20 => 1,);
    }
}
