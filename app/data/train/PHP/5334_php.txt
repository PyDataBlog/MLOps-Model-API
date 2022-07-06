<?php
/**
 * @package     Joomla.Site
 * @subpackage  Layout
 *
 * @copyright   Copyright (C) 2005 - 2015 Open Source Matters, Inc. All rights reserved.
 * @license     GNU General Public License version 2 or later; see LICENSE.txt
 */

defined('_JEXEC') or die;

extract($displayData);

/**
 * Layout variables
 *
 * @var  stdClass   $article  Article as it comes from joomla models/helpers
 * @var  array   $options     Array with options. Available options:
 *       						'resolution'    : Resolution of the thumbnail
 *       						'force_png'     : Convert always to PNG for transparent backgrounds
 *       						'force_rewrite' : Force thumbnail regeneration
 *       						'thumb_method'  : Method to use to create the thumbnails. See JImage
 */

$layoutOptions = empty($options) ? new JInput : new JInput($options);

//$images = json_decode($article->images);

// Intro image available
if (!empty($url))
{
	$imageUrl = $url;
	//$imageAlt = $images->image_intro_alt;
}
// Full text image available
/*elseif (!empty($images->image_fulltext))
{
	$imageUrl = $images->image_fulltext;
	$imageAlt = $images->image_fulltext_alt;
}*/
// Nothing to display
else
{
	return;
}

$resolution  = $layoutOptions->get('resolution', '350x250');

// A thumbnail has been required
if ($resolution)
{
	$thumbUrl = JLayoutHelper::render(
		'image.thumbnail',
		array(
			'imageUrl' => $imageUrl,
			'options'  => $options
		)
	);

	if ($thumbUrl)
	{
		$imageUrl = $thumbUrl;
	}
}
?>
<?php /* ?><img src="<?php echo $imageUrl; ?>" alt="<?php echo $imageAlt; ?>" /><?php */ ?>
<?php print_r($imageUrl); ?>
