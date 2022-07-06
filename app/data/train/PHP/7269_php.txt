<?php
defined('BASEPATH') OR exit('No direct script access allowed');

/**
*Programmed By: Reza Nur Rochmat
*Developed By KOMSI UGM
*CodeIgniter Framework 3.1.2
*Copyright 2016. All Right Reserved
*/

class register extends CI_Controller{

	public function __construct(){
		parent::__construct();
		$this->load->helper('url');
		$this->load->helper('form');
		$this->load->library('session');
		$this->load->library('form_validation');
		$this->load->model('model_register_member');
	}


	public function member(){
		$this->load->view('view_dashboard');
	}

	public function tambah_member(){
		$this->form_validation->set_rules('nomor_telepon','Nomor Telepon','required');
		$this->form_validation->set_rules('nama_member','Nama Member','required');
		$this->form_validation->set_rules('password_member','Password Member','required');
		$this->form_validation->set_rules('jenis_kelamin_member','Jenis Kelamin Member');
		$this->form_validation->set_rules('tanggal_lahir_member','Tanggal Lahir Member','required');
		$this->form_validation->set_rules('jenis_member','Jenis Member','required');
		$this->form_validation->set_rules('alamat_member','Alamat Member','required');

		if($this->form_validation->run() == FALSE){
			$this->load->view('view_register');

		}
		else {
			$this->model_register_member->tambah_member();
			$sukses = "<div class='alert alert-success'>Pendaftaran Member Berhasil</div>";
			$this->session->set_flashdata("sukses",$sukses);
			redirect('register/member');
		}
		
	}
}




?>