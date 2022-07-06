
<?php
    require 'connect_database.php';
    echo " you are in ajex checking ";
    
    if(isset($_POST["firstname"]) && $_POST["firstname"] != ""){
        
        $username =$_POST['firstname']; 
        echo " you are in ajex checking 2 ";
        if (strlen($username) < 4 ) {
                echo 'Too short(4-15)';
                exit();
        }
        else if(strlen($username) > 15){
            echo 'Too long(4-15)';
                exit();
        }
        
        else if (is_numeric($username[0])) {
                echo 'First character must be a letter';
                exit();
        }
        else {
                echo '<strong>' . $username . '</strong> is OK';
                exit();
        }
       
    }
    
    if(isset($_POST["email"]) && $_POST["email"] != ""){
        
        $username =$_POST['email']; 
        $sql_uname_check = mysql_query("SELECT email FROM usr_data where email='$username'"); 
        $uname_check = mysql_num_rows($sql_uname_check);
        
       
         if (is_numeric($username[0])) {
                echo 'First character must be a letter';
                exit();
        }
        else if (!preg_match("/.[@]/", $username)){
           echo 'Not a valid email';
           exit();
        }
        else if (mysql_num_rows($sql_uname_check) < 1) {
                echo '<strong>' . $username . '</strong> is OK';
                exit();
        }
        else {
                echo '<strong>' . $username . '</strong> Already Exist';
                exit();
        }
    }
    
   
    
     if(isset($_POST["password"]) && $_POST["password"] != ""){
        
        $username =$_POST['password']; 
         if (strlen($username) < 6 ) {
                echo 'Too short (minimum 6 character)';
                exit();
        
         }
        else if (!preg_match("/[0-9]/", $username)){
            echo 'Must contain a number';
            exit();
        }
        else if (!preg_match("/.[!,@,#,$,%,^,&,*,?,_,~,-,£,(,)]/", $username)){
           echo 'Must contain a special character';
           exit();
        }
        else {
           echo 'ok';
           exit();
        }
    }
    
    
     if(isset($_POST["confirmpassword"]) && $_POST["confirmpassword"] != ""){
       $confirmpassword=$_POST['confirmpassword'];
            echo "Matched";
     }
?>





