<?php
require_once('/wamp/www/banp/nobelba_data/common/DbMgr.php');

//SignUp
//This is test

class MASTERREGISTER
{
//**************** UserMaster Register **************
function registerusermaster($userexist)
{
	//Check User Input
	if (!empty($_POST["inputemail"]) && !empty($_POST["inputpass"])) {
   	$newemail = $_POST['inputemail'];
	$newpassword = $_POST['inputpass'];
	$phoneNo = $_POST['inputph'];
	
	//Check if the user exist or not  
	$sqluserexist = 'SELECT email FROM user_detail WHERE email = ?';
	
	try {
	$dbh = DbMgr::getDB();
	$stmt = $dbh->prepare($sqluserexist);
	$ret = $stmt->execute(array($newemail));
		if (!$ret) {
          print("Authentication Error");
          exit();
        }
        $result = $stmt->fetch(PDO::FETCH_ASSOC);
        $data =   array($result ['email']);
    } catch (Exception $e) {
		print('Error:'.$e->getMessage());
		die();
    }
	echo "This is user exit" .$userexist."" ;
	echo "This is user Email in DB" .$result['email']. "" ;
	echo "This is user Email input" .$_POST["inputemail"]. "";
	
	if (($_POST["inputemail"]  == $result['email']))  {
	$userexist = 1;
	return $userexist;
	}  else {
	 echo "I am inside Registration";
	  //If User Does Not Exist then proceed for registration 
	
	//Register User Data
	$sql = "INSERT INTO user_detail(email, password, phone_no) VALUES (
	'$newemail','$newpassword','$phoneNo')";
	
	try {
	$dbh = DbMgr::getDB();
	$stmt = $dbh->prepare($sql);
	$ret = $stmt->execute();
    if (!$ret) {
          print("Errro in Registraion");
          exit();
		} 
	} catch (Exception $e) {
	  print('Error:'.$e->getMessage());
      die();
    }
	
	//Send Email for Authentication 
	require "phpmailer/class.phpmailer.php"; 
	$Auth_URL = "http://nobelprize.aimjapan.org/authenticate.php/sjeiw7w829290102929";
	// Instantiate Class
	try {     
    $mail = new PHPMailer();  
    $mail->IsSMTP();                  // Sets up a SMTP connection  
    $mail->SMTPAuth = true;           // Connection with the SMTP does require authorization    
    $mail->SMTPSecure = "ssl";        // Connect using a TLS connection  
    $mail->Host = "smtp.lolipop.jp";  //SMTP server address
    $mail->Port = 465;  			  //SMTP port
    $mail->Encoding = '7bit';
    
    // Authentication  
    $mail->Username   = "yogesh@shintensystems.com"; // Your full Address
    $mail->Password   = "yogesh1123"; // Your Email password
	$mail->From       = "admin@nobelprizetobrambedkar.com";
	$mail->Subject  = "Welcome to Nobel Prize To Dr. Ambedkar Nomination";
	$body             = ' Jai Bhim!! <br /><br />
	Thank you for registering to Nominate Dr. B.R.Ambedkar for Nobel Prize <br /><br />
	User ID = '.$_POST['inputemail'].'<br />
    Password = '.$_POST['inputpass'].'<br /><br />
	Please click below URL to complte your registration <br />
	'.$Auth_URL.'<br /><br /><br />
	Regards <br />
	Team AIM Japan <br />
	http://nobelprize.aimjapan.org/ <br />
	';
    $to = $_POST["inputemail"];
	$mail->AddAddress($to);
	$mail->MsgHTML($body);
	$mail->Send(); 
    } catch (phpmailerException $e) {
	echo $e->errorMessage();
    }
	$userexist = 2;
	return $userexist;
  }
}

 }
}

?>
