<?php

defined('BASEPATH') OR exit('No direct script access allowed');

class cartController extends CI_Controller {

    /**
     * Index Page for this controller.
     *
     * Maps to the following URL
     * 		http://example.com/index.php/welcome
     * 	- or -
     * 		http://example.com/index.php/welcome/index
     * 	- or -
     * Since this controller is set as the default controller in
     * config/routes.php, it's displayed at http://example.com/
     *
     * So any other public methods not prefixed with an underscore will
     * map to /index.php/welcome/<method_name>
     * @see http://codeigniter.com/user_guide/general/urls.html
     */
    function __construct() {
        parent::__construct();
        $this->load->library('cart');
        //$this->load->Model('DBhelper');
        //$this->load->database();
    }

    public function index() {
        $this->load->view('frontend/cart');
    }

    public function add_cart_byProduct($id = '', $price, $productName = "") {
        $data = array(
            'id' => $id,
            'qty' => 1,
            'price' => $price,
            'name' => $productName,
            'options' => array('Size' => 'L', 'Color' => 'Red')
        );
        $this->cart->insert($data);
        redirect('indexController/index');
    }

    public function delete_cart($rowId = '') {
        $this->cart->remove($rowId);
        redirect('frontend/cartController', 'refresh');
    }

    public function add_cart($rowId = '', $qty) {
        $data = array(
            'rowid' => $rowId,
            'qty' => 1 + $qty
        );
        $this->cart->update($data);
        redirect('frontend/cartController', 'refresh');
    }

    public function reduce_cart($rowId = '', $qty) {
        $data = array(
            'rowid' => $rowId,
            'qty' => $qty - 1
        );
        $this->cart->update($data);
        redirect('frontend/cartController', 'refresh');
    }

    public function check_login() {
        if ($this->session->userdata('logincomplete') != 1) {
            echo 0;
        } else {
            echo 1;
        }
    }

}
