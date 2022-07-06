<?php
defined('BASEPATH') OR exit('No direct script access allowed');

class Product extends MX_Controller
{
    public function __construct()
    {
        parent::__construct();
        $this->load->library('grocery_CRUD');
    }

    public function render($output)
    {
 		$this->load->view('index.php',$output);
    }

    public function index()
    {
        $crud = new grocery_CRUD();
 
        $crud->set_table('product')
                ->display_as('id_product_category', 'Product Category')
                ->display_as('id_product_unit', 'Product Unit')
                ->set_relation('id_product_category', 'product_category', 'category')
                ->set_relation('id_product_unit', 'product_unit', 'unit')
                ->unset_fields('weight', 'lenght', 'width', 'height');
        $output = $crud->render();
 		
        $this->render($output);
    }

    public function category($action = null, $id_product_category = null)
    {
        $crud = new grocery_CRUD();
        $crud->set_table('product_category')
                ->set_relation('parent', 'product_category', '{id_product_category} - {category}');
    	
    	$output = $crud->render();
        $this->render($output);
    }

    public function parent($id_product_category)
    {
        $crud = new grocery_CRUD();
        $crud->set_table('product_category')
                ->where('parent', $id_product_category)
                ->unset_add()
                ->unset_edit()
                ->unset_delete()
                ->unset_read();
        $output = $crud->render();
        $this->render($output);
    }

    public function unit() 
    {
    	$crud = new grocery_CRUD();
    	$crud->set_table('product_unit');
    	$output = $crud->render();
        $this->render($output);
    }
} 