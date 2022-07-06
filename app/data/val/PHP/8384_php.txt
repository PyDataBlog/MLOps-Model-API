<?php

namespace Web\CommonBundle\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\Controller;
use Symfony\Component\HttpFoundation\Response;


class DefaultController extends Controller
{
	
	public function exmailAction()
	{
		try{
			
			$mailService = $this->get('Web.CommonBundle.Mail.Tencent');
			$mailService->setMailTo('262864308@qq.com')
						->setSubject('测试邮件标题')
						->setBody('测试邮件内容');
			
			$mailService->send();
		}
		catch(\Exception $e){
			
			die($e->getMessage());
			
		}		
		
		return new Response();
	}
	
	
	
	
    
   
    
    
    
    
    
    
    
    
}
