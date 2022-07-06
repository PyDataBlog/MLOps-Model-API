<?php

require_once 'Libs/Modelo.php';

/**
* Modelo Empresa, contiene todo lo referente a el modelado de empresa
* Implemente los metodos, como consultas y constructor
*
* @author Juan Jose Perdomo Forero
* @author Andres feliPe Perdomo Forero
*
* @package modelos
*/

class Empresa extends Modelo{

function __construct(){
 	parent::__construct();

 }

/**
* Se encarga de llamar el metodo insert que hereda de modelo 
* para registrar una empresa en la base de datos
* @access public
*/

public function insertEmpresa($nombretabla, $params){
	$this->insert($nombretabla,$params);
}

/**
* Se encarga de ejecutar una consulta a  la base de datos
* para autentificar a la empresa
* @access public
*/

public function authenticate($contraseña,$correo){
	return $this->query("Select * from empresa where correo='$correo' and contrasena='$contraseña'");
	}
} 

?>