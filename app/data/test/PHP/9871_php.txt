<?php
defined('BASEPATH') OR exit('No direct script access allowed');

class Warriors extends CI_Controller
 {
    public  function  __construct() 
    { 
        parent:: __construct(); 
        $this->layout->setLayout('template');
    } 

	public function index()
	{	
        if($this->session->userdata('logged_in'))
        {
                //nombre de usuario con data['username']
                 //$session_data = $this->session->userdata('logged_in');
                 //$data['username'] = $session_data['usuario'];

    	   $datos=$this->Equipos_model->getEquipos();
    	   $this->layout->view('index',compact("datos"));
        }
        else
        {
            $this->load->helper(array('form'));
            $this->load->view('login/login_view');

        }
	}


    public function nuevo()
    {
        if($this->session->userdata('logged_in'))
        {

            if($this->input->post())
                {
                    if($this->form_validation->run("warriors/nuevo"))
                    {
                      
                        $datos=array
                        (
                            'dyndns' => $this->input->post("dyn",true),
                            'usuario' => $this->input->post("usua",true),
                            'contrasena' => base64_encode($this->input->post("pass",true))
                        );
                        $guardar=$this->Equipos_model->insertarEquipos($datos);
                        if($guardar)
                        {
                            $this->session->set_flashdata('ControllerMessage', 'Se ha agragado el ususario exitosamente');
                            redirect(base_url().'warriors/index', 301);
                        }
                        else
                        {
                            $this->session->set_flashdata('ControllerMessage', 'Se ha producido un error. Inténtelo de nuevo.');
                            redirect(base_url().'warriors/new',  301);
                        }
                    }
                }

            $this->layout->view('new');
        }
        else
        {
            $this->load->helper(array('form'));
            $this->load->view('login/login_view');

        }
    }

    public function show($id=null)
    {
        if($this->session->userdata('logged_in'))
        {
            $datos = $this->Equipos_model->showEquipo($id);
            $this->layout->view('show',compact("datos"));
        }
        else
        {
            $this->load->helper(array('form'));
            $this->load->view('login/login_view');
        }
    }

    public function edit($id=null)
    {
        if($this->session->userdata('logged_in'))
        {
            if($this->input->post())
            {
                if($this->form_validation->run("warriors/nuevo"))
                { 
                    $datos=array
                    (
                        'dyndns'=> $this->input->post("dyn",true),
                        'usuario'=> $this->input->post("usua",true),
                        'contrasena'=> sha1($this->input->post('pas',true))   
                        );
                    $guardar=$this->Equipos_model->editEquipos($datos,$id);
                    if($guardar)
                    {
                        $this->session->set_flashdata('ControllerMessage', 'Se ha editado el ususario exitosamente');
                        redirect(base_url().'warriors/index', 301);
                    }
                    else
                    {
                        $this->session->set_flashdata('ControllerMessage', 'Se ha producido un error. Inténtelo de nuevo.');
                        redirect(base_url().'warriors/edit/'.$id,  301);
                    }
                }
            }
                    $datos=$this->Equipos_model->getEditEquipos($id);
                    if(sizeof($datos) == 0)
                    {
                        show_404();
                    } 
                    $this->layout->view('edit',compact("datos"));
        }
        else
        {
            $this->load->helper(array('form'));
            $this->load->view('login/login_view');
        }
    }

    public function iframe($id=null)
    {
        if($this->session->userdata('logged_in'))
        {
            $datos = $this->Equipos_model->iframeEquipo($id);
            $pass =base64_decode($datos->contrasena);
            $this->layout->view('iframe',compact("datos","pass"));
        }
        else
        {
            $this->load->helper(array('form'));
            $this->load->view('login/login_view');
        }
    }


    public function delete($id=null)
    {
        if($this->session->userdata('logged_in'))
        {
            $guardar=$this->Equipos_model->eliminar($id);
            if($guardar)
            {
                $this->session->set_flashdata('ControllerMessage', 'Se ha eliminado el ususario exitosamente');
                redirect(base_url().'usuario/index', 301);
            }
            else
            {
                $this->session->set_flashdata('ControllerMessage', 'Se ha producido un error. Inténtelo de nuevo.');
                redirect(base_url().'usuario/index',  301);
            }
        }
        else
        {
            $this->load->helper(array('form'));
            $this->load->view('login/login_view');
        }
    }

    public function logout()
    {
       $this->session->unset_userdata('logged_in');
       session_destroy();
       redirect('Login', 'refresh');
    }        		
    
}

