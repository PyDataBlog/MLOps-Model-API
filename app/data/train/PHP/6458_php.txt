<?php
//inicio la sesión
error_reporting(0);
session_start();
//comprueba que el usuario esta autentificado
if ($_SESSION["autentificado"] != "1") {
    //si no está logueado lo envío a la página de autentificación
    header("Location: index.php");
	exit();}
?>