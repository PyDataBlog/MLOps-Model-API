<?php
 
 function db_init($host,$user,$password,$dbname){
   $conn = mysqli_connect($host, $user, $password);
   mysqli_select_db($conn, $dbname);
   return $conn;
 }
 

?>