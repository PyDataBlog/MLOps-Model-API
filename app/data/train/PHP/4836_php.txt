<?php
Class Contact_model extends MY_Model
{
    var $table = 'contact';
     function get_list_contact()
    {
        $query = $this->db->get($this->table);

        if ($query->result()) {
            return $query->result();
        } else {
            return FALSE;
        }
    }
    
}
