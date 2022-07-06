 <?php
    session_start();
    include('acceso_db.php');
	
	
	$sql = mysql_query("UPDATE usuarios SET nombre = '$_POST[nombre]',apellidos = '$_POST[apellidos]',dni = '$_POST[dni]',usuario_nombre = '$_POST[usuario_nombre]',apellidos = '$_POST[apellidos]',usuario_clave = '$_POST[usuario_clave]',usuario_email = '$_POST[usuario_email]' WHERE usuario_nombre = '".$_SESSION['usuario_nombre']."'" );
	
mysql_query($sql);
if($sql) {
    echo "<script type='text/javascript'>  window.location='panel_admin.php';</script>";
    } else {
      echo "Error, <a href='javascript:history.back();'>Reintentar</a>";
}
mysql_close();
?>
