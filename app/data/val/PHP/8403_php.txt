<?php
class AppCard extends Component
{
	public static $ComponentName='AppCard';
	public $RenderType = 'Smart';
	public $ChildTag = 'ul';
	public $ChildClasses=array('round','AppCard');

	public $ContainerClasses = array('HorizontalList');
}

?>