<?
if($_GET['action'] == "login") {
$conn = mysql_connect("localhost","user","password"); // your MySQL connection data
$db = mysql_select_db("DATABASENAME"); //put your database name in here 
$name = $_POST['user'];
$q_user = mysql_query("SELECT * FROM USERS WHERE login='$name'");
?>
