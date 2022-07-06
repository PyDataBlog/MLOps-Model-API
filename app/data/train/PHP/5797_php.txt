<?php 
session_start();
$usuario=$_POST['usuario'];
$recordar=$_POST['recordarme'];

$retorno;

if( is_numeric($usuario) && ($usuario>999999) && ($usuario<100000000) )
{			
	if($recordar=="true")
	{
		setcookie("registro",$usuario,  time()+36000 , '/');
		
	}else
	{
		setcookie("registro",$usuario,  time()-36000 , '/');
		
	}
	$_SESSION['registrado']=$usuario;
	$retorno=" ingreso";

	
}else
{
	$retorno= "No-esta";
}

echo $retorno;
?>