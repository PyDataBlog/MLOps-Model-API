<?php
defined('BASEPATH') OR exit('No direct script access allowed');
class Man extends MX_Controller {
	public function __construct() {
		// parent::__construct();
		$this->load->module('init_sys/Init_sys');
		$this->load->model('man/Man_models');
	}

	public function man_page() 
	{
		$data['txt_search_name'] = $this->input->post('txt_search_name');
		$data['man_list'] = $this->get_man($data['txt_search_name']);
		$data['content']='man/man';
		
		$this->init_sys->content($data);
	}

	public function man_add(){
		$data['alert'] = $this->session->flashdata('alert');
		$data['content']='man/add-man';
		$this->init_sys->content($data);
	}
	public function man_edit(){
		$data['alert'] = $this->session->flashdata('alert');
		$data['content']='man/edit-man';
		$eq_id = $this->uri->segment(3);
		$data['man_details'] = $this->Man_models->get_man_details($eq_id);
		$this->init_sys->content($data);
	}
	public function add_man(){
		$data['man_list'] = $this->get_man();
        $this->load->model('Man_models');
        $timestam = date('Y-m-d H:i:s');
        $this->Man_models->add_man();
		$this->session->set_flashdata('alert', 1);
		redirect('man/man_page');
	}

	public function get_man($qstr ='')
	{
		$result = $this->Man_models->get_man($qstr);
		return $result;

	}
	public function delete_man(){
		$this->load->model('Man_models');
		$id = $this->uri->segment(3);
		$this->Man_models->delete_man($id);
		$this->session->set_flashdata('alert', 2);
		redirect('man/man_page');
	}
	public function update_man_details(){
		$this->load->model('Man_models');
		$timestam = date('Y-m-d H:i:s');
		$eq_id = $this->uri->segment(3);
		$this->Man_models->update_man_details($eq_id);
		$this->session->set_flashdata('alert', 1);
		redirect('man/man_page');
	}

}//end class
