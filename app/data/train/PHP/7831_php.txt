<?php
/**
 * Plugin Name: Lynch Forms
 * Plugin URI: http://fightlynch.org
 * Description: Helper plugin for Gravity Forms
 * Author: Burton Kent
 * Author URI: http://nerdery.com/vj
 */

function lynch_submitted($entry, $form){
    $questions[] = $entry[1];
    $questions[] = $entry[2];
    $questions[] = $entry[3];
}

add_action("gform_after_submission", "lynch_submitted", 10, 2);
