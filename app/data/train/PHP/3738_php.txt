<?php
#Test data
$a_tablename = "Herberg Liste";

$location = "Mændenes hjem";
$road = "Mandevej";
$number = "1";
$floor = "1st";
$door = "tv";
$city = "Copenhagen";
$country = "Denmark";
$phone = "11223344";
$website = "www.awebsite.dk";
?>

<?php
#Test data
$hlocation = "Navn";
$hroad = "Vej";
$hnumber = "Nummer";
$hfloor = "Etage";
$hdoor = "Dør";
$hcity = "By";
$hcountry = "Land";
$hphone = "Telefon";
$hwebsite = "Hjemmeside";

$hm_array = array(
  array($hlocation, $hroad, $hnumber, $hfloor, $hdoor, $hcity, $hcountry,$hphone, $hwebsite)
);
?>

<?php
$m_array = array(
  array($location, $road, $number, $floor, $door, $city, $country, $phone, $website),
  array($location, $road, $number, $floor, $door, $city, $country, $phone, $website)
);
?>
