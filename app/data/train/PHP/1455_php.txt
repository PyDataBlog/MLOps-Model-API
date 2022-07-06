<?php

$test = array('2004', '2005', '2006');
$test2 = array('1000', '1170', '660');

$end = array_pop($test);
$end2 = array_pop($test2);

?>

<html>
  <head>
    <script type="text/javascript"
          src="https://www.google.com/jsapi?autoload={
            'modules':[{
              'name':'visualization',
              'version':'1',
              'packages':['corechart']
            }]
          }"></script>

    <script type="text/javascript">
      google.setOnLoadCallback(drawChart);

      function drawChart() {
        var data = google.visualization.arrayToDataTable([
          ['Year', 'Sales'],
          <?php

          	foreach (array_combine($test, $test2) as $nb => $nb2)
				{
					echo '["'.$nb.'", '.$nb2."],";
					echo "\n";
				}
				echo '["'.$end.'", '.$end2."]";
          ?>
        ]);

        var options = {
          title: 'Company Performance',
          curveType: 'function',
          legend: { position: 'bottom' }
        };

        var chart = new google.visualization.LineChart(document.getElementById('curve_chart'));

        chart.draw(data, options);
      }
    </script>
  </head>
  <body>
    <div id="curve_chart" style="width: 900px; height: 500px"></div>
  </body>
</html>