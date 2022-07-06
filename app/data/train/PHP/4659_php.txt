<?php
defined('BASEPATH') OR exit('No direct script access allowed');

class Home extends CI_Controller {

	public function __construct()
    {
        parent::__construct();
        $this->curpage = "JOHNMARKABRIL";
        $this->load->model('About_model');
        $this->load->model('Projects_model');



    	date_default_timezone_set("Asia/Manila");
    	$this->date = date("F d, Y");
    	$this->time = date("g:i A");
    }

	public function index()
	{
		$details = array (
			'get_all_projects'		=>	$this->Projects_model->get_all_projects(),
			'get_all_about'			=>	$this->About_model->get_all_about()
		);

		$data['content']	=	$this->load->view('home', $details, TRUE);
		$data['curpage']	= 	$this->curpage;
		$this->load->view('template', $data);
	}

}
