<?php

class C_admin extends CI_Controller {

	public function __construct() {
		parent::__construct();
		if($this->session->userdata('level') != 'admin') {
			redirect('auth'); }
		
	}
	public function index() {
		$data['username'] = $this->session->userdata('username');
		$this->load->view('dashboard_admin', $data);
	}


	public function activity() {
		$data['username'] = $this->session->userdata('username');
		$this->load->view('activity_admin', $data);
	}	

	public function mahasiswa() {
		$data['username'] = $this->session->userdata('username');
		$this->load->view('mahasiswa_admin', $data);
	}	

	public function logout() {
		$this->session->unset_userdata('username');
		$this->session->unset_userdata('level');
		$this->session->sess_destroy();
		redirect('auth'); }
}
?>
