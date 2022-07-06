<?php
use google\appengine\api\users\User;
use google\appengine\api\users\UserService;

$user = UserService::getCurrentUser();
if (isset($user)) {
 /* echo sprintf('<li>Welcome, %s! (<a href="%s">sign out</a>)',
               $user->getNickname(),
               UserService::createLogoutUrl('/')); */
			   $url = UserService::createLogoutUrl('index.php');
 // แสดงภาพผใู้ช ้โดยการเรยี กฟังกช์ นั userpic จากข ้อที่ 1
?>	
<?php	
   if(UserService::isCurrentUserAdmin()){
     include("menu_admin.php"); 
   }
} else {
  echo sprintf('<li><a href="%s">Sign in or register</a>',
               UserService::createLoginUrl('/'));
}
?>