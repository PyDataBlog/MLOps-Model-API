<?php
 // created: 2017-09-21 17:27:56
$layout_defs["bh_org"]["subpanel_setup"]['bh_org_bh_place_of_work_1'] = array (
  'order' => 100,
  'module' => 'bh_place_of_work',
  'subpanel_name' => 'default',
  'sort_order' => 'asc',
  'sort_by' => 'id',
  'title_key' => 'LBL_BH_ORG_BH_PLACE_OF_WORK_1_FROM_BH_PLACE_OF_WORK_TITLE',
  'get_subpanel_data' => 'bh_org_bh_place_of_work_1',
  'top_buttons' => 
  array (
    0 => 
    array (
      'widget_class' => 'SubPanelTopButtonQuickCreate',
    ),
    1 => 
    array (
      'widget_class' => 'SubPanelTopSelectButton',
      'mode' => 'MultiSelect',
    ),
  ),
);
