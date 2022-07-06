<?php foreach ($heroes as $key => $value) : ?>
<?php


$remove[] = "'";

$linkPrimaryName = str_replace($remove,"", $value['PrimaryName']);

// $linkPrimaryName = preg_replace("/'/", "", $value['PrimaryName']);

$replace1[] = ".";
$replace2[] = " ";
$replace3[] = "-";

$linkPrimaryName = str_replace($replace1,"", $linkPrimaryName);
$linkPrimaryName = str_replace($replace2,"-", $linkPrimaryName);

if ($value['PrimaryName'] == 'Li Li') {
  $linkPrimaryName = str_replace($replace3,"", $linkPrimaryName);
}

switch ($value['PrimaryName']):
  case 'Cho': { $linkPrimaryName = "chogall";
              break; }
  case 'Gall': { $linkPrimaryName = "chogall";
              break;}
endswitch;

// dsm($linkPrimaryName);



  $lowerHero = strtolower($linkPrimaryName);
  ?>
  <?php  print "<a href='http://us.battle.net/heroes/en/heroes/{$lowerHero}/'>" . $value['PrimaryName'] ."<br>" .
         $value['Group'] . "</a><br/>"; ?>
         
<?php endforeach; ?>
