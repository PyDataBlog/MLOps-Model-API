<h2><?php print $node->title; ?></h2>
<h3><?php print content_format('field_subtitle', $field_subtitle[0]); ?></h3>

<? print l('Back', 'node/'.$node->field_case_project[0]['nid'], array('html'=>TRUE, 'attributes'=>array('class'=>'back-link'))); ?>

<? print content_format('field_case_intro_title', $field_case_intro_title[0]); ?>
<? print content_format('field_case_intro_body', $field_case_intro_body[0]); ?>


<? print content_format('field_case_video', $field_case_video[0]); ?>

<?
$distribution_body = strip_tags($field_case_distribution_body[0]['value']);
if (!empty($distribution_body)) 
	print content_format('field_case_distribution_body', $field_case_distribution_body[0]);
?>
<? if (!empty($field_case_distribution_image[0]['filename'])) print content_format('field_case_distribution_image', $field_case_distribution_image[0], 'image_plain'); ?>


<?
$finance_body = strip_tags($field_case_finance_body[0]['value']);
if (!empty($finance_body)) 
	print content_format('field_case_finance_body', $field_case_finance_body[0]);
?>
<? if (!empty($field_case_finance_image[0]['filename'])) print content_format('field_case_finance_image', $field_case_finance_image[0], 'image_plain'); ?>


<?
$multiplatform_body = strip_tags($field_case_multiplatform_body[0]['value']);
if (!empty($multiplatform_body)) 
	print content_format('field_case_multiplatform_body', $field_case_multiplatform_body[0]);
?>
<? if (!empty($field_case_multiplatform_image[0]['filename'])) print content_format('field_case_multiplatform_image', $field_case_multiplatform_image[0], 'image_plain'); ?>


<?
$ad_body = strip_tags($field_case_ad_body[0]['value']);
if (!empty($ad_body)) 
	print content_format('field_case_ad_body', $field_case_ad_body[0]);
?>
<? if (!empty($field_case_ad_image[0]['filename'])) print content_format('field_case_ad_image', $field_case_ad_image[0], 'image_plain'); ?>


<?
$international_body = strip_tags($field_case_international_body[0]['value']);
if (!empty($international_body)) 
	print content_format('field_case_international_body', $field_case_international_body[0]);
?>
<? if (!empty($field_case_international_image[0]['filename'])) print content_format('field_case_international_image', $field_case_international_image[0], 'image_plain'); ?>


<?
$format_body = strip_tags($field_case_format_body[0]['value']);
if (!empty($format_body)) 
	print content_format('field_case_format_body', $field_case_format_body[0]);
?>
<? if (!empty($field_case_format_image[0]['filename'])) print content_format('field_case_format_image', $field_case_format_image[0], 'image_plain'); ?>



<? print content_format('field_case_whitepaper', $field_case_whitepaper[0]); ?>


<div class="sponsor-logos">
	<? print views_embed_view('sponsor_logos', 'block_1', $field_case_project[0]['nid']); ?>
</div>