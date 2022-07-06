<?php

include 'common.php';

echo '문자전송 내역 및 전송상태 확인 테스트'.chr(10).'처리결과 : ' ;

try
{
	//접수번호
	$ReceiptNum = '014042115000000001';
	
	$result = $MessagingService->GetMessages('1231212312',$ReceiptNum,'userid');
	echo json_encode($result,JSON_PRETTY_PRINT | JSON_UNESCAPED_UNICODE);
}
catch(PopbillException $pe) {
	echo '['.$pe->getCode().'] '.$pe->getMessage();
}
echo chr(10);

?>
