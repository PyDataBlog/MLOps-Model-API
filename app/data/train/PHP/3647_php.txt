
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>title</title>
  </head>
  <body>
    <?php
echo "{\"Relay0\":";
echo apc_fetch('Relay0');
echo "}";
?>
  </body>
</html>