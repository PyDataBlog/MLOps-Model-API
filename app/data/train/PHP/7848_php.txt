<?php
/**
* created by steptian@tencent.com 2014.01.29
*/

 date_default_timezone_set('Asia/Shanghai');
//out type by default json as 0 else html
require_once('../etc/const.php');

$mime = 0;// json
$header = array();
if(!empty($_REQUEST['MIME'])){
	$mime = intval($_REQUEST['MIME']);	
}
if($mime == 1){
	$headers = array(
		"Content-type: text/html; charset=utf-8",
	);
}else{
	$headers = array(
		"Content-type: application/json; charset=utf-8",
	);
}
//route the file
$biz=$mod=$act=null;
if(!empty($_REQUEST['biz'])){
	$biz = $_REQUEST['biz'];
}

if(!empty($_REQUEST['mod'])){
	$mod= $_REQUEST['mod'];
}

if(!empty($_REQUEST['act'])){
	$act = $_REQUEST['act'];
}

if(!in_array($biz,$_BIZLIST) || empty($biz)){
	exit('wrong biz');	
}
$mod_file = './mod/'.$biz.'/'.$mod.'.php';
if(!in_array($mod,$_MODLIST) ||!file_exists($mod_file) ){
	exit('wrong mod or mod_file not exists');	
}

$mod_file = './mod/'.$biz.'/'.$mod.'.php';
require_once($mod_file);

$function_name = $mod.'_'.$act;
if(!function_exists($function_name)){
	exit('The function '.$function_name.' is not exists.');
}

//output
foreach($headers as $header){
	header($header);
}
if($mime == 0){
	echo json_encode($function_name());
	return;
}else{
	return $function_name();
}
print_r('biz = '.$biz.' mod = '.$mod.' act = '.$act);

