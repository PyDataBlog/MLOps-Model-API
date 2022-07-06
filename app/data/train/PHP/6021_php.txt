<?php
function start_getpage(){
    $default_page='./pages/accueil/default.php';
    $page_404='./pages/404/default.php';
    if(empty($_GET['page'])
        ||is_numeric(strpos($_GET['page'],"/"))
        ||is_numeric(strpos($_GET['page'],"\\"))){
    $_GET['page']="accueil";
    }
    if(empty($_GET['index'])
        ||is_numeric(strpos($_GET['index'],"/"))
        ||is_numeric(strpos($_GET['index'],"\\"))){
        $_GET['index']="default";
    }
    if(file_exists("./pages/".$_GET['page']."/".$_GET['index'].".php")){
        $page = "./pages/".$_GET['page']."/".$_GET['index'].".php";
    }elseif(file_exists("./pages/".$_GET['page'].".php")){
        $page = "./pages/".$_GET['page'].".php";
    }
     else{
       $page = $page_404;
     }

     return $page;   
}
$page = start_getpage();
?>