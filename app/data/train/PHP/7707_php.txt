<?php

// No direct access
defined('_JEXEC') or die;

class RentalModelBroker_Profile extends JModel
{
	/**
	 * Function to get broker info
	 * @return Object Broker Info
	 */
	public function getItem()
	{
		$db = JFactory::getDbo();
		$query = $db->getQuery(true);
		
		$id = JRequest::getInt('id');
		
		$query->select('*')->from('#__retal_agents')->where('id = ' . $id);
		
		$db->setQuery($query);
		$obj = $db->loadObject();
		
		return $obj;
	}
	
	/**
	 * Get list apartments of broker
	 * @return Objects - List apartments
	 */
	public function getItems()
	{
		$db = JFactory::getDbo();
		$query = $db->getQuery(true);
		
		$agentId = JRequest::getInt('id');
		
		$query->select('*')->from('#__rental_apartments')->where('agent_id = ' . $agentId);
		
		$db->setQuery($query);
		$rs = $db->loadObjectList();
		
		return $rs;
	}
}