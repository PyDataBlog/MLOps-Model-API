// NOTE: nbApp is defined in app.js
nbApp.directive("languageContainerDirective", function() {
    return {
        restrict : 'E',
        templateUrl : 'js/templates/language-container.html',
        scope: {
          color: "@",
          language: "@",
          reading: "@",
          writing: "@",
          listening: "@",
          speaking: "@",
          flag: "@",
        },
        link: function(scope, element, attrs) {
          scope.color = attrs.color;
          scope.language = attrs.language;
          scope.reading = attrs.reading;
          scope.writing = attrs.writing;
          scope.listening = attrs.listening;
          scope.speaking = attrs.speaking;
          scope.flag = attrs.flag;

          scope.$watch('language', function(nV, oV) {
            if(nV){
                RadarChart.defaultConfig.color = function() {};
                RadarChart.defaultConfig.radius = 3;
                RadarChart.defaultConfig.w = 250;
                RadarChart.defaultConfig.h = 250;

                /*
                 * 0 - No Practical Proficiency
                 * 1 - Elementary Proficiency
                 * 2 - Limited Working Proficiency
                 * 3 - Minimum Professional Proficiency
                 * 4 - Full Professional Proficiency
                 * 5 - Native or Bilingual Proficiency
                   Read: the ability to read and understand texts written in the language
                   Write: the ability to formulate written texts in the language
                   Listen: the ability to follow and understand speech in the language
                   Speak: the ability to produce speech in the language and be understood by its speakers.
                 */
                var data = [
                  {
                    className: attrs.language, // optional can be used for styling
                    axes: [
                      {axis: "Reading", value: attrs.reading},
                      {axis: "Writing", value: attrs.writing},
                      {axis: "Listening", value: attrs.listening},
                      {axis: "Speaking", value: attrs.speaking},
                    ]
                  },
                ];
                function mapData() {
                  return data.map(function(d) {
                    return {
                      className: d.className,
                      axes: d.axes.map(function(axis) {
                        return {axis: axis.axis, value: axis.value};
                      })
                    };
                  });
                }

                // chart.config.w;
                // chart.config.h;
                // chart.config.axisText = true;
                // chart.config.levels = 5;
                // chart.config.maxValue = 5;
                // chart.config.circles = true;
                // chart.config.actorLegend = 1;

                var chart = RadarChart.chart();
                var cfg = chart.config(); // retrieve default config
                    cfg = chart.config({axisText: true, levels: 5, maxValue: 5, circles: true}); // retrieve default config
                var svg = d3.select('.' + attrs.language).append('svg')
                  .attr('width', 250)
                  .attr('height', 270);
                svg.append('g').classed('single', 1).datum(mapData()).call(chart);

                console.log('Rendering new language Radar Viz! --> ' + attrs.language);
            }
          })

        }
    };
});
