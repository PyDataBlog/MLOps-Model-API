
<?php

// Exit if accessed directly
if ( !defined('ABSPATH')) exit;

/**
 * Sidebar Right Half Template
 *
 *
 * @file           sidebar-right-half.php
 * @package        Responsive 
 * @author         Emil Uzelac 
 * @copyright      2003 - 2012 ThemeID
 * @license        license.txt
 * @version        Release: 1.0
 * @filesource     wp-content/themes/responsive/sidebar-right-half.php
 * @link           http://codex.wordpress.org/Theme_Development#Widgets_.28sidebar.php.29
 * @since          available since Release 1.0
 */
?>
</div>
        <div id="widgets" class="grid col-460 fit">
        <?php responsive_widgets(); // above widgets hook ?>
            
            <?php if (!dynamic_sidebar('right-sidebar')) : ?>
            <div class="widget-wrapper-right">
             </div><!-- end of .widget-wrapper -->
			<?php endif; //end of sidebar-right-half ?>

        <?php responsive_widgets_end(); // after widgets hook ?>
        </div><!-- end of #widgets -->