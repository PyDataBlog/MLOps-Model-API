<?php
class Jobs extends CI_Controller {

        public function __construct()
        {
                parent::__construct();
                $this->load->model('job_get');
                $this->load->helper('url_helper');
                $this->load->library('session');
        }

        public function index()
        {
                $data['jobs'] = $this->job_get->get_jobs($this->session->userdata['logged_in']['user_id']);
                $data['title'] = 'Jobs';
                $data['Type']=$this->session->userdata['logged_in']['type'];
                $this->load->view('jobs/joblist', $data);

        }
        public function view($slug = NULL)
        {
                $data['jobs_item'] = $this->Jobs_model->get_jobs($slug);
        }
        public function insert_applied()
        {
                $this->load->model->insert_desc();
        }
        public function insert_jobposted()
        {
                $this->load->model->insert_job();
        }
}