<?php
/*
Plugin Name: Allowed posttags
Plugin URI: 
Description: 特権管理者じゃなくてもdlとかのclassを消されたり、WordPress Importerで投稿をインポートした時記事内にiframeとかobjectとかあっても消されたりしないようにする。（<a href="http://ja.forums.wordpress.org/topic/6393?replies=4" target="_blank">元ネタはこちら</a>）
Author: Kei Nomura
Version: 1.0
*/
function set_allowedposttags($content){
  global $allowedposttags;
  $allowedposttags['dl'] = array(
    'class' => array(),
    'id' => array()
  );
  $allowedposttags['dt'] = array(
    'class' => array(),
    'id' => array()
  );
  $allowedposttags['dd'] = array(
    'class' => array(),
    'id' => array()
  );
  $allowedposttags['iframe'] = array(
    'class' => array(),
    'src' => array(),
    'width' => array(),
    'height' => array() ,
    'frameborder' => array(),
    'scrolling' => array(),
    'marginheight' => array(),
    'marginwidth' => array()
  );
  $allowedposttags['object'] = array(
    'class' => array(),
    'data' => array(),
    'width' => array(),
    'height' => array(),
    'type' => array()
  );
  $allowedposttags['param'] = array(
    'class' => array(),
    'name' => array(),
    'value' => array()
  );
  return $content;
}
add_filter('content_save_pre', 'set_allowedposttags', 8);
