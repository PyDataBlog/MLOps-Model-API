<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Rajdeep's Programs Repository</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta name="description" content="">
    <meta name="author" content="">

    <!-- Le styles -->
    <link href="assets/css/bootstrap.css" rel="stylesheet">
    <link href="assets/css/bootstrap-responsive.css" rel="stylesheet">
    <link href="assets/css/bootstrap-fileupload.css" rel="stylesheet">
    <link href="assets/css/bootstrap-fileupload.min.css" rel="stylesheet">
    <link href="assets/css/docs.css" rel="stylesheet">
    <link href="assets/js/google-code-prettify/prettify.css" rel="stylesheet">

    <style type="text/css">
   .footer{
    background-color: #d8d8d8;
   }
   </style>
    

   <script src="assets/js/jquery-1.9.0.js"></script>

    <!-- Le HTML5 shim, for IE6-8 support of HTML5 elements -->
    <!--[if lt IE 9]>
      <script src="assets/js/html5shiv.js"></script>
    <![endif]-->

  </head>

 <body >
<a href="https://github.com/rajdeep26/MyProgramRepository"><img style="position: absolute; top: 15; left: 0; border: 0;" src="./assets/img/forkme_left_red.png" alt="Fork me on GitHub"></a>
    <!-- Navbar
    ================================================== -->
      <div id="nav" class="navbar navbar-inverse navbar-fixed-top">
        <div class="navbar-inner">
          <div class="container">
            <button type="button" class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
              <span class="icon-bar"></span>
              <span class="icon-bar"></span>
              <span class="icon-bar"></span>
              <span class="icon-bar"></span>
            </button>
            <a class="brand" href="./index.html">My Program Repository</a>
            <div class="nav-collapse collapse">
              <ul class="nav">
                <li class="active">
                  <a href="index.html">Home</a>
                </li>
                <li><a href="ShowPrograms.php" >Programs</a></li>
                <li><a href="AddProgram.php" >Add Programs</a></li>
                <li><a href="Feedback.html" >Feedback</a></li>
              </ul>
            </div>
          </div>
        </div>
      </div>
      <br><br>
    <div class="container">
      <div class="row-fluid">
        <a href="AddProgram.php" class="btn btn-primary">Add Program</a>
        <div class="span8 offset2">
          <legend>Programs</legend>
          <p align="center"><span class="label label-info">Just a Start. More Programs will be added soon.</span></p>
          <div class="span11">
            <div class="accordion" id="accordion2">
              <?php
              include 'mysql.php';
              $res = mysql_query("SELECT id,program_title,program_language,program FROM programs");

              $i=1;
              while($row = mysql_fetch_assoc($res)) 
              {
                // <=================  will replace each occurance of '<' with '&lt;' =========>
                $programdata = $row['program']; 
                $search = "<";
                $replace = "&lt;";
               
                $programdata = str_replace($search, $replace, $programdata);

                 // <======================================================================>

                echo '<div class="accordion-group">';
                echo '<div class="accordion-heading">';
                  echo '<a class="accordion-toggle" data-toggle="collapse" data-parent="#accordion2" href="#collapse'.$i.'">';
                  echo ' '.$row['program_title'].'<p class="pull-right">Language: '.$row['program_language'].' </p>';
                  echo '</a>';
                  echo '</div>';
                  echo '<div id="collapse'.$i.'" class="accordion-body collapse '; if($i==1) echo 'in'; echo '">';
                  echo '<div class="accordion-inner">';
                    echo '<pre class="prettyprint linenums">'.$programdata.'</pre>';
                    echo '<p>';
                      echo '<a href="EditProgram.php?id='.$row['id'].'" class="btn btn-primary">Edit</a>';
                      echo '<a href="#myModal'.$i.'" role="button" class="btn btn-danger pull-right" data-toggle="modal">Delete</a>';
                      // echo '<a href="DeleteProgram.php?id='.$row['id'].'" class="btn btn-danger pull-right">Delete</a>';
                      
                      echo '<div id="myModal'.$i.'" class="modal hide fade" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">';
                        echo '<div class="modal-header">';
                          echo '<button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>';
                          echo '<h3 id="myModalLabel">Delete</h3>';
                        echo '</div>';
                        echo '<div class="modal-body">';
                          echo '<h4 align="center">Are you sure you want to delete this program??</h4>';
                          echo '<div class="span10 offset2">';
                          echo '<div class="span4 offset2">';
                          echo '<a href="DeleteProgram.php?id='.$row['id'].'" class="btn">Yes</a>';
                          echo '</div>';
                          echo '<div class="span6">';
                          echo '<button class="btn" data-dismiss="modal" aria-hidden="true">No</button>';
                          echo '</div>';
                          echo '</div>';
                        echo '</div>';

                      echo '</div>';
                    echo '</p>';
                  echo '</div>';
                echo '</div>';
                echo '</div>';
                $i++;
              }
              mysql_close($dbhandle);
              ?>
            </div>
          </div>
        </div>
      </div>
    </div>

    <!-- Footer
    ================================================== -->
    <footer class="footer">
      <div class="container">
        <p></p>
        <p>&copy; Copyrights by Rajdeep Mandrekar</p>
      </div>
    </footer>



    <!-- Le javascript
    ================================================== -->
    <!-- Placed at the end of the document so the pages load faster -->
    <script type="text/javascript" src="http://platform.twitter.com/widgets.js"></script>
    <script src="assets/js/jquery.js"></script>
    <script src="assets/js/bootstrap-transition.js"></script>
    <script src="assets/js/bootstrap-alert.js"></script>
    <script src="assets/js/bootstrap-modal.js"></script>
    <script src="assets/js/bootstrap-dropdown.js"></script>
    <script src="assets/js/bootstrap-scrollspy.js"></script>
    <script src="assets/js/bootstrap-tab.js"></script>
    <script src="assets/js/bootstrap-tooltip.js"></script>
    <script src="assets/js/bootstrap-popover.js"></script>
    <script src="assets/js/bootstrap-button.js"></script>
    <script src="assets/js/bootstrap-collapse.js"></script>
    <script src="assets/js/bootstrap-carousel.js"></script>
    <script src="assets/js/bootstrap-typeahead.js"></script>
    <script src="assets/js/bootstrap-affix.js"></script>
    <script src="assets/js/bootstrap-fileupload.js"></script>

    <script src="assets/js/holder/holder.js"></script>
    <script src="assets/js/google-code-prettify/prettify.js"></script>

    <script src="assets/js/application.js"></script>



  </body>
</html>
