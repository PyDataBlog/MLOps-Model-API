<?php

//add a button to the content editor, next to the media button
//this button will show a popup that contains inline content
add_action('media_buttons_context', 'add_my_custom_button');

//add some content to the bottom of the page 
//This will be shown in the inline modal
if(is_admin()){
add_action('admin_footer', 'add_inline_popup_content');
}
//action to add a custom button to the content editor
function add_my_custom_button($context) {
  
  //path to my icon
  $img = get_template_directory_uri() .'/images/shortcode_button.png';
  
 
  //our popup's title
  $title = __('Cosmone Shortcodes','cosmone');

  //append the icon
  $context .= "<a class='cosmone_shortcodes' title='{$title}'><img src='{$img}' /></a>";
  
  return $context;
}

function add_inline_popup_content() {
global $cosmone_shortcodes ;

?>


<div class="white-popup cosmone_shortcodes_container mfp-with-anim mfp-hide" id="cosmone_shortcodes_container" style="" >
 <form>
  <h4><?php _e("MageeWP Shortcodes Generator",'cosmone');?></h4>
  <ul class="cosmone_shortcodes_list">
  <?php if(is_array($cosmone_shortcodes )):foreach($cosmone_shortcodes as $key => $val){ 	
         if(in_array($key ,array("testimonial_item","pricing_row",'tab','accordion'))){continue;}
  ?>
  
  <li><a class='cosmone_shortcode_item <?php echo $key;?>' title='<?php echo ucwords(str_replace("_"," ",$key));?>' data-shortcode="<?php echo $key;?>" href="javascript:;"><?php echo ucwords(str_replace("_"," ",$key));?></a></li>
  <?php } ?>
  <?php endif;?>
	  </ul>
	  <div id="cosmone-shortcodes-settings">
	  
	 
  <div id="cosmone-generator-breadcrumbs">
  <a title="Click to return to the shortcodes list" class="cosmone-shortcodes-home" href="javascript:void(0);"><?php  _e("All shortcodes",'cosmone');?></a>  &rarr; <span class="current_shortcode"></span>
    <div class="clear"></div>
  </div>
	        <div id="cosmone-shortcodes-settings-inner"></div>
			<input name="cosmone-shortcode" type="hidden" id="cosmone-shortcode" value="" />
			<input name="cosmone-shortcode-textarea" type="hidden" id="cosmone-shortcode-textarea" value="" />
			<div class="cosmone-shortcode-actions cosmone-shortcode-clearfix">
			<!--<a class="button button-secondary button-large cosmone-shortcode-preview "  href="javascript:void(0);"><?php _e("Preview shortcode",'cosmone');?></a>-->
			<a class="button button-primary button-large cosmone-shortcode-insert "  href="javascript:void(0);"><?php _e("Insert shortcode",'cosmone');?></a>
			
	  </div>
	  <div class="clear"></div>
	  </div></form>
	  <div class="clear"></div>
</div>
<div id="cosmone-shortcode-preview" style="display:none;">

</div>
<?php } ?>