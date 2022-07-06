<?php  if ( ! defined('BASEPATH')) exit('No direct script access allowed');

/**
*    微信端函数库
*/
class Wxfunction
{
	

#上传图片
function fileimg($arr,$url){
	if ($this->upload->do_upload($arr)) {
		# 上传成功，获取文件路径
		$fileinfo = $this->upload->data();
		$this->config->load('upload.php');
		$upload  = $this->config->item('upload_path');
		$data = $upload . $fileinfo['file_name'];
		return $data;
	}else{
		#上传失败
		$data['message'] = $this->upload->display_errors();
		$data['wait'] = 3;
		$data['url'] = $url;
		$this->load->view('message.html',$data);
		}
	



}



function getExtension($str)  
{  
         $i = strrpos($str,".");  
         if (!$i) { return ""; }  
         $l = strlen($str) - $i;  
         $ext = substr($str,$i+1,$l);  
         return $ext;  
} 

}

?>