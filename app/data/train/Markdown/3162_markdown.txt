StringUtilities
==================

String conversion tools

# StringToAscii
Converts a string with special characters and accents into an regular ASCII string

Converting a string
-------------------
To convert a string in ASCII, see the following code:
```php
$string = Mrk\StringUtilities\StringToAscii::convert('Un été'); // returns "Un ete"
```

# Slugify
Converts a string into a URL compatible slug

Converting a string
-------------------
To convert a string in slug, see the following code:
```php
$string = Mrk\StringUtilities\Slugify::convert('Un été'); // returns "Un-ete"
$string = Mrk\StringUtilities\Slugify::convert('Un été', '_'); // returns "Un_ete"
$string = Mrk\StringUtilities\Slugify::convertLC('Un été'); // returns "un-ete"
```

# Installation
StringUtilities is a composer-compatible library. It's not on packagist, but to install you just need to add the repository definition and require to your composer.json file, then run `composer update`

    "repositories": [
		{
			"type": "git",
			"url": "https://github.com/mrk-te/stringutilities.git"
		}
	],
	"require": {
		"mrk/stringutilities": "*"
	}

