<?php

/**
 * Model for Pages within the CMS
 *
 * @author Chris
 */
class modelPage extends ormModelCsv implements hamleModel {
  function setup() {
    $this->openFile(__DIR__."/../../data/page.csv");
    $this->fields = array(
        new ormType_KeyInt("pageid"),
        new ormType_String("title"),
        new ormType_String("content"),
    );
  }
  function hamleGet($key) {
    try {
      return $this->$key;
    } catch (Exception $e) {
      throw new hamleEx_NoKey("Unable to find Key ($key)");
    }
  }
  function hamleExec($func, $args) {
    throw new hamleEx_NoFunc("Not Enabled");
  }
  function hamleRel($rel, $typeTags) {
    throw new hamleEx_NoFunc("Not Implemented");
  }
}
?>
