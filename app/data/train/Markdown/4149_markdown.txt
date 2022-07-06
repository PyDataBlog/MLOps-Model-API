Nivo Slider for Wordpress
-------------------------

This plugin is taken from [Nivo Slider for Wordpress](http://wordpress.org/plugins/nivo-slider-for-wordpress/)
on the wordpress website.  The original plugin works very well, but the
navigation bullets don't show up even when *Show the navigation bullets* is
checked. I modified the plugin so that a row of bullets are shown at the
top-left corner of the slide when the option is checked. Tested with Wordpress
3.4.2.

Besides the fix, I also added a *Content* field for images. You can write
description for each image in HTML. The content for the current slide will be
shown in a separate content block, which can be placed anywhere in your theme.
Use the following code to insert a content block:

```php
<?php
if (function_exists('nivoslider4wp_show_content')) {
	nivoslider4wp_show_content();
}?>
```

Please read the original [readme](readme.txt) and [license](license.txt) for
more information about installation and redistribution rules.

Screenshot
----------

bullets turned on
![nivoslider4wp with bullets](screenshot-4.png)

separate content block placed beside the slider
![content block](screenshot-5.png)
