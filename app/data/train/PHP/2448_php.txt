<?php 
	$path = '/home3/johangau/orcapilot';
	set_include_path(get_include_path() . PATH_SEPARATOR . $path);	
require_once 'framework_0_2/_include.inc';

$session = new Session();
$pa = new PilotArray();


$pa->loadOperationPilots($_GET['operation']);

include 'framework_0_2/_header.php';

?>
<h3>Make a payment</h3>
<p>select from the list</p>
<p><form action="singleoperation.php"><?php $pa->getHTMLdropdown(); ?> <input type="text" class="dark" name="amount"><br>
	<input class="button" type="image" width="88" height="19" border="0" src="images/paypilot.png" />
	<input type="hidden" name="operation" value="<?php if(isset($_GET['operation']))echo $_GET['operation'];?>" />	
	<input type="hidden" name="makepayment" value="yes" />
	<input type="hidden" name="session" value="<?php echo $session->getSessionHash();?>">
	<input type="hidden" name="verification" value="<?php echo Payment::getNewVerificationCode();?>">
</form></p>
<p>Or add a new pilot/user</p>
