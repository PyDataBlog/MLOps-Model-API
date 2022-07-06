<?php

$absolute_path = __FILE__;
$path_to_file = explode( 'wp-content', $absolute_path );
$path_to_wp = $path_to_file[0];
require_once( $path_to_wp.'/wp-load.php' );


$to = isset($_POST['to'])?trim($_POST['to']):'';
$name = isset($_POST['name'])?trim($_POST['name']):'';
$email = isset($_POST['email'])?trim($_POST['email']):'';
$content = isset($_POST['content'])?trim($_POST['content']):'';

$error = true;
if($to == '' || $email == '' || $content == '' || $name == '') $error = false;


$sitename = get_bloginfo('name');

if($error){
	$subject = 'A message from '.$name;
	$headers = 'From: '.$email . "\r\n" . 'Reply-To: '.$email . "\r\n";
	mail($to, $subject, $content, $headers);
}