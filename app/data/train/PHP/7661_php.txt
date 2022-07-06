<?php
defined('_JEXEC') or die;
class EventsComponentHelper {
    
    public static function getContents(){
        $db = JFactory::getDBO();
        $strSqlStatement = "SELECT * FROM #__content WHERE mask <> 1 AND catid=10";
        $db->setQuery(str_replace('#__', $db->getPrefix(), $strSqlStatement));
        $mixResponse = $db->loadObjectList();
        return $mixResponse;
    }
    
    public static function getContentsItem($intId){
        $db = JFactory::getDBO();
        $strSqlStatement = "SELECT * FROM #__event WHERE id_content=".$intId;
        $db->setQuery(str_replace('#__', $db->getPrefix(), $strSqlStatement));
        $mixResponse = $db->loadObjectList();
        return $mixResponse;
    }
    
    public static function saveEventItems(
            $strContentId,
            $strEventType,
            $strPlace,
            $strPublic,
            $strManager,
            $strContact,
            $strPrice,
            $strStartDate,
            $strFinishDate){
        if(!empty($strContentId) && !empty($strEventType) && !empty($strPublic) && !empty($strPlace) && !empty($strManager) && !empty($strContact) && !empty($strPrice) && !empty($strStartDate) && !empty($strFinishDate)){
            $db = JFactory::getDBO();
            $strSqlStatement = "INSERT INTO #__event VALUES(LAST_INSERT_ID(),".$strContentId.",'".$strEventType."','".$strPlace.
                    "','".$strPublic."','".$strManager."','".$strContact."','".$strPrice."','".$strStartDate."','".$strFinishDate."')";
            $db->setQuery(str_replace('#__', $db->getPrefix(), $strSqlStatement));
            if(!$db->query()){
                JError::raiseError(100, 'FALLÓ EL QUERY DE INSERCIÓN');
                return false;
            }else{
                return true;
            }
        } else {
            JError::raiseError(100, 'FALTARON PARAMETROS');
            return false;
        }
    }
    
    public static function editEventItems(
            $strContentId,
            $strEventType,
            $strPlace,
            $strPublic,
            $strManager,
            $strContact,
            $strPrice,
            $strStartDate,
            $strFinishDate){
        if(!empty($strContentId) && !empty($strEventType) && !empty($strPublic) && !empty($strPlace) && !empty($strManager) && !empty($strContact) && !empty($strPrice) && !empty($strStartDate) && !empty($strFinishDate)){
            $db = JFactory::getDBO();
            $strSqlStatement = "UPDATE #__event SET `type`='".$strEventType."', place='".$strPlace.
                    "', public='".$strPublic."', manager='".$strManager."', contact='".$strContact."', price='".$strPrice.
                    "', start_date='".$strStartDate."', finish_date='".$strFinishDate."' WHERE id_content=".$strContentId."";
            $db->setQuery(str_replace('#__', $db->getPrefix(), $strSqlStatement));
            if(!$db->query()){
                JError::raiseError(100, 'FALLÓ EL QUERY DE ACTUALIZACIÓN');
                return false;
            }else{
                return true;
            }
        } else {
            JError::raiseError(100, 'FALTARON PARAMETROS');
            return false;
        }
    }
    
    public static function getEvent($intEvent = null){
        if(!empty($intEvent)){
            $db = JFactory::getDBO();
            $strSqlStatement = "";
            $db->setQuery(str_replace('#__', $db->getPrefix(), $strSqlStatement));
            $mixResponse = $db->loadObjectList();
            if($mixResponse != null){
                return $mixResponse;
            } else {
                JError::raiseError(100, 'FALLÓ EL QUERY DE CONSULTA');
                return false;
            }
        }
    }
    public static function getEvents(){
        $db = JFactory::getDBO();
        $strSqlStatement = "";
        $db->setQuery(str_replace('#__', $db->getPrefix(), $strSqlStatement));
        $mixResponse = $db->loadObjectList();
        if($mixResponse != null){
            return $mixResponse;
        } else {
            JError::raiseError(100, 'FALLÓ EL QUERY DE CONSULTA');
            return false;
        }
    }
}

?>
