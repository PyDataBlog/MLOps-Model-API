<?php
/**
 * @package     Joomla.Site
 * @subpackage  Templates.protostar
 *
 * @copyright   Copyright (C) 2005 - 2016 Open Source Matters, Inc. All rights reserved.
 * @license     GNU General Public License version 2 or later; see LICENSE.txt
 * Line added to test how sync works
 */

defined('_JEXEC') or die;

$app             = JFactory::getApplication();
$doc             = JFactory::getDocument();
$user            = JFactory::getUser();
$this->language  = $doc->language;
$this->direction = $doc->direction;

// Output as HTML5
$doc->setHtml5(true);

// Getting params from template
$params = $app->getTemplate(true)->params;

// Detecting Active Variables
$option   = $app->input->getCmd('option', '');
$view     = $app->input->getCmd('view', '');
$layout   = $app->input->getCmd('layout', '');
$task     = $app->input->getCmd('task', '');
$itemid   = $app->input->getCmd('Itemid', '');
$sitename = $app->get('sitename');


// Add JavaScript Frameworks
JHtml::_('bootstrap.framework');

$doc->addScript($this->baseurl . '/templates/' . $this->template . '/js/jquery-1.11.1.min.js');

// Add Stylesheets
$doc->addStyleSheet($this->baseurl . '/templates/' . $this->template . '/css/style.css');
$doc->addStyleSheet($this->baseurl . '/templates/' . $this->template . '/css/font-awesome.css');
$doc->addStyleSheet($this->baseurl . '/templates/' . $this->template . '/css/responsive.css');

// Use of Google Font
// if ($this->params->get('googleFont'))
// {
// 	$doc->addStyleSheet('//fonts.googleapis.com/css?family=' . $this->params->get('googleFontName'));
// 	$doc->addStyleDeclaration("
// 	h1, h2, h3, h4, h5, h6, .site-title {
// 		font-family: '" . str_replace('+', ' ', $this->params->get('googleFontName')) . "', sans-serif;
// 	}");
// }



?>

<?php
 // Getting page class and body class
$menu = $app->getMenu();
$activeMenu = $menu->getActive();
$defaultMenu = $menu->getDefault();
if ($activeMenu == $defaultMenu) {
//Home page
 $body_class="home-page";
}
else
{
//Inner Page
 $body_class="inner-page";
}

$pageclass = '';
if (is_object($activeMenu))
{
	$pageclass = $activeMenu->params->get('pageclass_sfx');
}
?>

<!DOCTYPE html>
<html lang="<?php echo $this->language; ?>" dir="<?php echo $this->direction; ?>">
<head>
	<meta name="viewport" content="width=device-width, initial-scale=1.0" />
	<jdoc:include type="head" />
	<!--[if lt IE 9]><script src="<?php echo JUri::root(true); ?>/media/jui/js/html5.js"></script><![endif]-->
</head>
<body class="<?php echo $body_class.$pageclass; ?>" >

	<?php if($this->countModules('left-header or logo or middle-header or main-menu')) :?>
		<header>
			<div class="container">

				<?php if($this->countModules('left-header')) :  ?>
					<div class="left-header">
			        	<jdoc:include type="modules" name="left-header" style="xhtml"/>
			        </div>
				<?php endif; ?>

				<?php if($this->countModules('logo')) :  ?>
					<div class="logo">
			        	<a href="<?php echo JURI::base(); ?>">
			        		<jdoc:include type="modules" name="logo" style="xhtml"/>
		        		</a>
			        </div>
				<?php endif; ?>

				<?php if($this->countModules('middle-header')) :  ?>
					<div class="middle-header">
			        	<jdoc:include type="modules" name="middle-header" style="xhtml"/>
			        </div>
				<?php endif; ?>

				<?php if($this->countModules('main-menu')) : ?>
		    		<div class="menu-fix">
	                    <div class="navigation-toggle">
	                        <button class="navigation-toggle-btn">
	                            <span class="icon-bar"></span>
	                            <span class="icon-bar"></span>
	                            <span class="icon-bar"></span>   
	                            <a>Menu</a>                 
	                        </button>
	                    </div>
	                    <div class="navigation">
				        	<jdoc:include type="modules" name="main-menu" style="xhtml"/>
				        </div>
		            </div>			        
	    		<?php endif; ?>

			</div>
		</header>
	<?php endif; ?>

	<?php if($this->countModules('slide-show')) : ?>
        <div class="slider">
        	<jdoc:include type="modules" name="slide-show" style="xhtml"/>
        </div>
	<?php endif; ?>

	<?php
		if($this->countModules('left and right')) {
			$contentClass="both";
		} elseif($this->countModules('left')) {
			$contentClass="left";	
		} elseif($this->countModules('right or service')) {
			$contentClass="right";
		} else {
			$contentClass="full";
		}
	?>

	<div class="content-box">
		<div class="container">

			<?php if($this->countModules('top')) : ?>
				<div class="top">
					<jdoc:include type="modules" name="top" style="xhtml"/>
				</div>
			<?php endif; ?>			

			<?php if($this->countModules('mainbody-top')) : ?>
				<div class="main-body-top">
					<jdoc:include type="modules" name="mainbody-top" style="xhtml"/>
				</div>
			<?php endif; ?>

			<?php if($this->countModules('left')) : ?>
				<div class="left-side-main">
					<div class="left">
						<jdoc:include type="modules" name="left" style="xhtml"/>
					</div>
				</div>
			<?php endif; ?>			
			

			<div class="content-side <?php echo $contentClass; ?>">
				<?php if (count(JFactory::getApplication()->getMessageQueue())) : ?>
		            <jdoc:include type="message" />
		        <?php endif; ?>
		        <jdoc:include type="component" />
			</div>

			<?php if($this->countModules('right')) : ?>
				<div class="right-side-main">
					<div class="right-side">
						<jdoc:include type="modules" name="right" style="xhtml"/>
					</div>
				</div>
			<?php endif; ?>

			<?php if($this->countModules('mainbody-bottom')) : ?>
				<div class="main-body-bottom">
					<jdoc:include type="modules" name="mainbody-bottom" style="xhtml"/>
				</div>
			<?php endif; ?>

			<?php if($this->countModules('service')) : ?>
				<div class="right-side-main">
					<div class="right-side">
						<jdoc:include type="modules" name="service" style="xhtml"/>
					</div>
				</div>
			<?php endif; ?>

			<?php if($this->countModules('gallery')) : ?>
				<div class="col-8 fl gallery">
					<jdoc:include type="modules" name="gallery" style="xhtml"/>
				</div>
			<?php endif; ?>

			<?php if($this->countModules('bottom')) : ?>
				<div class="bottom">
					<jdoc:include type="modules" name="bottom" style="xhtml"/>
				</div>
			<?php endif; ?>

			
		</div>
	</div>

	<?php if($this->countModules('testimonials')) : ?>
        <div class="testimonials">
            <div class="container">
            	<jdoc:include type="modules" name="testimonials" style="xhtml"/>
            </div>        	
        </div>
	<?php endif; ?>

	<?php if($this->countModules('footer-left or footer-logo or social or copyright or footer-right')) : ?>
		<footer>
			<div class="container">				
			
				<?php if($this->countModules('footer-left')) : ?>
					<div class="footer-left">
						<jdoc:include type="modules" name="footer-left" style="xhtml"/>		
					</div>
				<?php endif; ?>

				<?php if($this->countModules('footer-logo')) : ?>
					<div class="footer-logo">
						<a href="<?php echo JURI::base(); ?>">
							<jdoc:include type="modules" name="footer-logo" style="xhtml"/>
						</a>	
					</div>
				<?php endif; ?>

				<?php if($this->countModules('social')) : ?>
					<div class="footer-social">
						<jdoc:include type="modules" name="social" style="xhtml"/>
					</div>
				<?php endif; ?>

				<?php if($this->countModules('copyright')) : ?>
					<div class="designed">
						<a href="#">
							<jdoc:include type="modules" name="copyright" style="xhtml"/>
						</a>					
					</div>
				<?php endif; ?>

				<?php if($this->countModules('footer-right')) : ?>
					<div class="footer-right">
						<jdoc:include type="modules" name="footer-right" style="xhtml"/>		
					</div>
				<?php endif; ?>

			</div>
		</footer>
	<?php endif; ?>

	<jdoc:include type="modules" name="debug" style="none" />

	<script>
            jQuery(document).ready(function(){    
                jQuery(".navigation-toggle-btn").click(function(){
                    jQuery(".navigation").fadeToggle();
                });
            });
        </script>
</body>
</html>
