<div align="center" style="margin: 20px;">
<?php
$letters=array('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'); 
foreach($letters AS $letter){ 
    echo "&nbsp<a class='letter' href='".strtolower($letter)."' >".$letter."</a>"; 
}
    echo "&nbsp<a class='letter' href='' >[TODOS]</a>"; 
?>
</div>