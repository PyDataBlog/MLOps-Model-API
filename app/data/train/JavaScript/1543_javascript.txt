
var activeElements = [];
var sleepElements = [];
var promises = [];

$.ajax("https://osiproghackuc2015.osisoft.com/piwebapi/assetdatabases/D0EgxEhIf8KUieOFdFcX1IWQZ8qIGYDdE0m5aJCwNb4x_gSlVQSVRFUjAwMVxQSUZJVE5FU1M/elements", { 
		type : 'GET',
		headers: { "Authorization" : "Basic " + btoa("osiproghack\\hackuser051:bO2rA53P2")},
		success: function(results){
			for (var i = 0; i < results.Items.length; i++) {
				
				var item = results.Items[i];

				getSubElements(item);
			}
		}
	}).done(function(){
		$.when.apply($,promises).done(function(){
			spinner.stop(target);

			var blackout = document.getElementById('blackout');
			$('#blackout').css('opacity', '0');
			$('#blackout').css('width', '0%');
			$('#blackout').css('height', '0%');
		});
	});

var getSubElements = function(personElement){

	promises.push($.ajax("https://osiproghackuc2015.osisoft.com/piwebapi/elements/" + personElement.WebId + "/elements", { 
		type : 'GET', 
		headers: { "Authorization" : "Basic " + btoa("osiproghack\\hackuser051:bO2rA53P2")},
		success: function(results){
			for (var i = 0; i < results.Items.length; i++) {
							
			var innerItem = results.Items[i];

			if (innerItem.TemplateName == "Fitbit Activity Template") {
				getFitbitActivityAttributes({
					Person : personElement.Name,
					Child : "Fitbit Activity",
					ChildWebId : innerItem.WebId
				});
			} else if (innerItem.TemplateName == "Fitbit Sleep Template") {
				getFitbitSleepAttributes({
					Person : personElement.Name,
					Child : "Fitbit Sleep",
					ChildWebId : innerItem.WebId
				});
			}

		}}}));
}

var getFitbitActivityAttributes = function(object) {
	promises.push($.ajax("https://osiproghackuc2015.osisoft.com/piwebapi/elements/" + object.ChildWebId + "/attributes",{
		type : 'GET', 
		headers: { "Authorization" : "Basic " + btoa("osiproghack\\hackuser051:bO2rA53P2")},
		success: function(results){
			object.Attributes = [];
			activeElements.push(object);
			for (var i = 0; i < results.Items.length; i++) {
				var attribute = results.Items[i];

				object.Attributes.push({
					Attribute : attribute.Name,
					AttributeWebId : attribute.WebId
				});
			};
		}
	}));
}

var getFitbitSleepAttributes = function(object) {
	promises.push($.ajax("https://osiproghackuc2015.osisoft.com/piwebapi/elements/" + object.ChildWebId + "/attributes",{
		type : 'GET', 
		headers: { "Authorization" : "Basic " + btoa("osiproghack\\hackuser051:bO2rA53P2")},
		success: function(results){
			object.Attributes = [];
			sleepElements.push(object);
			for (var i = 0; i < results.Items.length; i++) {
				var attribute = results.Items[i];

				object.Attributes.push({
					Attribute : attribute.Name,
					AttributeWebId : attribute.WebId
				});
			};
		}
	}));
}







