<?php
/**
 * This is a Athena pagecontroller.
 *
 */
// Include the essential config-file which also creates the $athena variable with its defaults.
include(__DIR__.'/config.php');


// Do it and store it all in variables in the Athena container.
$athena['title'] = "Hello World";

$athena['header'] = <<<EOD
<img class='sitelogo' src='img/athena.jpg' alt='Athena Logo'/>
<span class='sitetitle'>Athena webbtemplate</span>
<span class='siteslogan'>Återanvändbara moduler för webbutveckling med PHP</span>
EOD;

$athena['main'] = <<<EOD
<h1>Hej Världen, Athena is reborn!</h1>
<p>Detta är en exempelsida som visar hur Athena ser ut och fungerar.</p>
EOD;

$athena['footer'] = <<<EOD
<footer><span class='sitefooter'>Copyright (c) Anders Bolin (dslitefix@gmail.com) | <a href='https://github.com/mosbth/Athena-base'>Anax på GitHub</a> | <a href='http://validator.w3.org/unicorn/check?ucn_uri=referer&amp;ucn_task=conformance'>Unicorn</a></span></footer>
EOD;


// Finally, leave it all to the rendering phase of Athena.
include(ATHENA_THEME_PATH);