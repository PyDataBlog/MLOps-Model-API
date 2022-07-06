<?php

/**
* @package Author
* @author 
* @website 
* @email 
* @copyright 
* @license 
**/

// no direct access
defined('_JEXEC') or die('Restricted access');

class OperacionModelFacturaciones extends JModelList{

	public function __construct($config = array()){	

		if (empty($config['filter_fields'])) {

			$config['filter_fields'] = array(
				'nombre', 'a.nombre',
				'disabled', 'a.disabled',
			);
			
			if (JLanguageAssociations::isEnabled()){
				$config['filter_fields'][] = 'association';
			}

		}

		parent::__construct($config);

	}

	protected function populateState($ordering = null, $direction = null){

		$this->setState('filter.search', $this->getUserStateFromRequest($this->context . '.filter.search', 'filter_search'));
		
		// Load the parameters.
		$this->setState('params', JComponentHelper::getParams('com_administracion'));
		
		$this->setState('filter.id_cliente', $this->getUserStateFromRequest($this->context . '.filter.id_cliente', 'filter_id_cliente', '', 'int'));
		$this->setState('filter.id_vehiculos', $this->getUserStateFromRequest($this->context . '.filter.id_vehiculos', 'filter_id_vehiculos', '', 'int'));
		$this->setState('filter.id_conductor', $this->getUserStateFromRequest($this->context . '.filter.id_conductor', 'filter_id_conductor', '', 'int'));
		$this->setState('filter.id_material_recoger', $this->getUserStateFromRequest($this->context . '.filter.id_material_recoger', 'filter_id_material_recoger', '', 'int'));
		$this->setState('filter.tipo_orden', $this->getUserStateFromRequest($this->context . '.filter.tipo_orden', 'filter_tipo_orden', '', 'int'));
		$this->setState('filter.id_estadodepago', $this->getUserStateFromRequest($this->context . '.filter.id_estadodepago', 'filter_id_estadodepago', '', 'int'));
		$this->setState('filter.id_estadodeorden', $this->getUserStateFromRequest($this->context . '.filter.id_estadodeorden', 'filter_id_estadodeorden', '', 'int'));
	
		// List state information.
		parent::populateState($ordering, $direction);

	}

	protected function getStoreId($id = ''){
		
		// Compile the store id.
		$id	.= ':'.$this->getState('filter.search');
		$id	.= ':'.$this->getState('filter.id_cliente');
		$id	.= ':'.$this->getState('filter.id_vehiculos');
		$id	.= ':'.$this->getState('filter.id_conductor');
		$id	.= ':'.$this->getState('filter.id_material_recoger');
		$id	.= ':'.$this->getState('filter.tipo_orden');
		$id	.= ':'.$this->getState('filter.id_estadodepago');
		$id	.= ':'.$this->getState('filter.id_estadodeorden');
				
		return parent::getStoreId($id);

	}

	function getListQuery(){
	
		// Create a new query object.
		$db 		= $this->getDbo();
		$my			= JFactory::getUser();
	
		$query = $db->getQuery(true);

		$query->select(
			$this->getState('list.select', 'a.*, b.nombre as cliente, c.nombre as tipoorden, d.placa as placa, e.nombre AS estado_pedido, f.nombre AS estado_facturacion')
		);

		$query->from('#__operacion_ordenes_pedido AS a');
		$query->join('inner', '#__core_clientes AS b ON a.id_cliente = b.id');
		$query->join('inner', '#__core_tipo_operacion AS c ON a.tipo_orden = c.id');
		$query->join('inner', '#__core_vehiculos AS d ON a.id_vehiculo = d.id');		
		$query->join('inner', '#__operacion_ordenes_estado AS e ON a.id_estado = e.id');		
		$query->join('inner', '#__operacion_estado_factura AS f ON a.estado_factura = f.id');			
		
		$search = $this->getState('filter.search');
		
		if (!empty($search)){
			if (stripos($search, 'id:') === 0){
				$query->where('a.id = '.(int) substr($search, 3));
			}else {
				$search = $db->quote('%' . str_replace(' ', '%', $db->escape(trim($search), true) . '%'));
				$query->where('(b.nombre LIKE '.$search.')');
			}
		}
	
		// Filter on the level.
		if ($level = $this->getState('filter.id_cliente')){
			$query->where('a.id_cliente = ' . (int) $level);
		}
		
		// Filter on the vehiculo.
		if ($id_vehiculos = $this->getState('filter.id_vehiculos')){
			$query->where('a.id_vehiculo = ' . (int) $id_vehiculos);
		}
		
		if ($id_conductor = $this->getState('filter.id_conductor')){
			$query->where('a.id_conductor = ' . (int) $id_conductor);
		}
		
		if ($id_material_recoger = $this->getState('filter.id_material_recoger')){
			$query->where('a.id_material = ' . (int) $id_material_recoger);
		}
		
		if ($tipo_orden = $this->getState('filter.tipo_orden')){
			$query->where('a.tipo_orden = ' . (int) $tipo_orden);
		}
		
		if ($id_estadodepago = $this->getState('filter.id_estadodepago')){
			$query->where('a.estado_factura = ' . (int) $id_estadodepago);
		}
		
		if ($id_estadodeorden = $this->getState('filter.id_estadodeorden')){
			$query->where('a.id_estado = ' . (int) $id_estadodeorden);
		}
		
		if($my->id_cantera > 0){
			$query->where('(a.id_cantera = '. $my->id_cantera. ')');
		}		
		
		$query->where('a.principal= 1 AND a.id_estado in (2,5)');
		
		$listOrdering 	= $this->getState('list.ordering', 'a.id');
		$listDirn 		= $db->escape($this->getState('list.direction', 'ASC'));
		
		$query->order($db->escape($listOrdering) . ' ' . $listDirn);
		
		return $query;

	}//function	

}//class ?>