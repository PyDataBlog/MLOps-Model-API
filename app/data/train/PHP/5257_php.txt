<?php
/*
복수 인자의 사용
*/
function get_two_argument($arg1, $arg2){
	return $arg1 * $arg2;
}

print get_two_argument(1, 200);
print '<br/>';
print get_two_argument(100, 200);
?>