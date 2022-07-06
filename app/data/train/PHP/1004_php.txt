<?php
session_start();
  $exercise = $_SESSION['exercise'];
  if($_POST){
    unset($exercise[$_POST['id']]);
    $_SESSION['exercise'] = $exercise;
    header('Location: ./');
  }
  
  $workout = $exercise[$_REQUEST['id']];
?>
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
    <title>Exercise Log: Delete</title>

    <!-- Bootstrap -->
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css">
    <link rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/toastr.js/latest/css/toastr.min.css" type="text/css" />
    <link rel="stylesheet" href="//code.jquery.com/ui/1.11.4/themes/smoothness/jquery-ui.css">
    <link rel="stylesheet" type="text/css" href="../style/stylesheet.css" />
  </head>
  <body>
    <div class="container">

        <div class="page-header">
          <h1>Exercise <small>Viewing <?=$workout['Name']?> workout</small></h1>
        </div>
        
        <ul>
            <li>Workout Name: <?=$workout['Name']?></li>
            <li>Workout Amount: <?=$workout['Amount']?></li>
            <li>Workout Time: <?=$workout['Time']?></li>
            <li>Workout Calories Burned: <?=$workout['Calories']?></li>
        </ul>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"></script>
    <script type="text/javascript" src="//cdnjs.cloudflare.com/ajax/libs/toastr.js/latest/js/toastr.min.js"></script>
    <script src="//code.jquery.com/ui/1.11.4/jquery-ui.js"></script>
    <script type="text/javascript">
      (function($){
        $(function(){
          
        });
      })(jQuery);
    </script>
  </body>
</html>