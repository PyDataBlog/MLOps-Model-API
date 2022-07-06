/*
	general map generation class
	Since September 2013 | mark@prmanenttourist.ch
	
	This code is provided as-is under the GPL v2 licence, which is available via http://www.gnu.org/licenses/gpl-2.0.html,
	and may be freely used, adapted and built upon. No guarantee is provided or implied. Test your code!
	http://www.gnu.org/licenses/gpl.html

	Simplify generation of Google Maps on a web page

	The existing default usage via Google's own implementation is great, but this version adds an improved
	overlay function, the option to add MarkerLabel functionality by setting an option to true, and much
	easier addition of custom map styles.
	
	• Labels are displayed with the markers. Label content is displayed directly in the map.
	• Markercontent is displayed in the overlay when a marker is clicked.
	• The current implementation (July 2015) requires that markerclusterer_compiled.js and markerwithlabel_packed.js be 
		linked in the HTML head if you need to use this functionality.
	
	This file is quite heavy and heavily commented. Use class.googlemaps.min.js instead.

	See class.googlemaps.html for a demo of how to use this script with a couple of simple options.

*/

var mhmli_googlemaps_rootclass, MarkerWithLabel, marker, bounds, mhmli_googlemaps, google, customStyle;

(function($){
	
	mhmli_googlemaps = {
		map: null,
		mapContainer: null,
		infowindow: null,
		infowindowContent: null,
		locations: [],
		pins: [],
		map_clustered: null,
		
		////////////////////////////////////////////////////////////////////////////////
		// Options. Can be reset externally before drawing the map.
		text: {
			reset: 'Reset',
			close: 'Close'
		},
		asset_path:'',
		pin_default: function(){
			return null;
		},
		useStyledMarkers: false,
		options: {
			cluster: false,				// cluster markers together, when there are too many to be displayed in one go
			clusterstyles: null,		// array containing custom attributes for the MarkerClusterer object. e.g. styles.
			mapTypeID: null,			// e,g. google.maps.MapTypeId.HYBRID
			mapTypes: null,				// array of types google.maps.MapTypeId
			lat_default: 46.817918,		// default latitude
			lon_default: 8.227386,		// default longitude
			zoomlevel_start: 7,			// default zoom level
			zoomlevel_default: 7,		// default zoom level
			zoomlevel_near: 10,			// if map zooms when clicking on a marker, the zoom level to go to
			zoomlevel_minimum: 1,		// don't let the user zoom out to more than this zoom level
			focus_centre: true,			// when clicking on a marker, should the map centre itself on the clicked marker
			showLabels: false,			// should labels be added to the markers?
			fitToBounds: false,			// should the map automatically set itself to include all markers in the default view?
			hasInfoWindow: true,		// does an infowindow appear when clicking on a marker
			focus_zoom: true,			// when clicking on a marker, should the map zoom in to the clicked marker
			icon_offset: [0,0],			// default icon offset
			icon_size: [16,16],			// default icon size
			icon: null					// optional: use your own icon
		},
		controls: {
			// these options are here in order to comply with Google logic for separating controls and other options
			blockUI: false,				// completely block all UI options including zoom, scroll etc
			resetbutton:true,			// add a reset button to the map?
			visualRefresh:true,			// use the new Google Maps (Summer 2013) - https://developers.google.com/maps/documentation/javascript/basics#VisualRefresh
			disableDefaultUI: false,	// disable all default map features with one command
			panControl: false,			// enable/disable pan controls
			mapTypeControl: false,		// enable/disable map type controls
			scaleControl: true,			// enable/disable scaling (zoom) controls
			streetViewControl: false,	// enable/disable street view control
			overviewMapControl: false,	// enable/disable overview control (bottom right; small controller to see where current section of map is, in relation to e.g. Europe)
			zoomControl: true,			// enable/disable zoom control. if zoomControlOptions are set, they will override this option.
			zoomControlOptions: {		// customization of individual zoom control options - https://developers.google.com/maps/documentation/javascript/controls#ControlOptions
				style: null,
				position:null
			}
		},
		
		////////////////////////////////////////////////////////////////////////////////
	
		setMapStyle: function(mapStyle) {
			if (mapStyle && mapStyle!==null) {
				if(this.options.mapTypes && this.options.mapTypes.length){
					this.options.mapTypes.push(customStyle);
				}
				var customStyle = new google.maps.StyledMapType(
				mapStyle, {
					name: 'customStyle'
				});
				this.map.mapTypes.set('customStyle', customStyle);
				this.map.setMapTypeId('customStyle');
				
				
				
			}
		},//setMapStyle
		
		////////////////////////////////////////////////////////////////////////////////
	
		Marker: function(atts) {
			if(atts.baseClass.options.showLabels){
				marker = new MarkerWithLabel({
					position: atts.position,
					zIndex: atts.zIndex,
					map: atts.map,
					icon: atts.icon,
					labelContent: atts.labelContent,
					markerContent: atts.markerContent,
					labelAnchor: new google.maps.Point(12,8),
					title: atts.title,
					locationID: atts.locationID
				});
			}else{
				marker = new google.maps.Marker({
					position: atts.position,
					zIndex: atts.zIndex,
					map: atts.map,
					icon: atts.icon,
					labelContent: atts.labelContent,
					markerContent: atts.markerContent,
					title: atts.title,
					locationID: atts.locationID
				});
			}
			marker.baseClass = atts.baseClass;
			return marker;
		},//Marker
		
		////////////////////////////////////////////////////////////////////////////////
	
		makeMap: function(divID, layout) {

			if(this.controls.visualRefresh){
				google.maps.visualRefresh = true;
			}
			this.mapContainer = $('#'+divID);

			var mapOptions = {
				zoom: this.options.zoomlevel_default,
				center: this.centrePoint(),
				panControl: this.controls.panControl,
				mapTypeControl: this.controls.mapTypeControl,
				scaleControl: this.controls.scaleControl,
				streetViewControl: this.controls.streetViewControl,
				overviewMapControl: this.controls.overviewMapControl,
				zoomControl: this.controls.zoomControl,
				zoomControlOptions: this.controls.zoomControlOptions
			};
			
			if(this.options.mapTypes && this.options.mapTypes.length){
				mapOptions.mapTypeControlOptions = {
					mapTypeIds: this.options.mapTypes
				};
				mapOptions.mapTypeControl = true;
			}
  
			if(this.options.mapTypeID){
				mapOptions.mapTypeId = this.options.mapTypeID;
			}

			if(this.controls.disableDefaultUI){
				mapOptions.disableDefaultUI = true;
			}

			if(this.controls.blockUI){
				mapOptions.draggable = false;
				mapOptions.zoomable = false;
				mapOptions.scrollwheel = false;
				mapOptions.navigationControl = false;
				mapOptions.mapTypeControl = false;
				mapOptions.scaleControl = false;
				mapOptions.draggable = false;
			}

			this.map = new google.maps.Map(document.getElementById(divID), mapOptions);
			this.map.setZoom(this.options.zoomlevel_start);

			this.setMapStyle(layout);
			if(this.options.fitToBounds){
				bounds = new google.maps.LatLngBounds();
			}
			
			var marker, i, baseClass=this, markers = [];
			for (i in this.locations) {
				if(this.locations.hasOwnProperty(i)){
					var rp = this.locations[i].rankedPosition;
					marker = new this.Marker({
						//map: this.map,
						position: new google.maps.LatLng(this.locations[i].longitude, this.locations[i].latitude),
						title: this.locations[i].title,
						labelContent: this.locations[i].labelContent,
						markerContent: this.locations[i].markerContent,
						rankedPosition: rp + '',
						zIndex: 10000 - i,
						locationID: this.locations[i].locationID,
						icon: this.options.icon,
						baseClass: this
					});

					if(this.locations[i] && this.locations[i].icon){
						var icon_offset = this.locations[i].offset ? this.locations[i].offset : this.options.icon_offset;
						var icon_size = this.locations[i].icon_size ? this.locations[i].icon_size : this.options.icon_size;
						marker.setIcon({
							url: this.locations[i].icon,
							anchor: new google.maps.Point(icon_offset[0],icon_offset[1]),
							size: new google.maps.Size(icon_size[0], icon_size[1])
						});
					}
					if(this.options.fitToBounds){
						bounds.extend(marker.position);
					}

					google.maps.event.addListener(marker, 'click', function(event){
						baseClass.markerClickHandler(event,this,baseClass);
					});

					markers.push(marker);
   				}
			}

			google.maps.event.addListener(this.map, 'zoom_changed', function() {
				if (baseClass.map.getZoom() < baseClass.options.zoomlevel_minimum){
					baseClass.map.setZoom(baseClass.options.zoomlevel_minimum);
				}
			});

			if(this.options.cluster){
				var markerClustererExtras = this.options.clusterstyles ? {styles:this.options.clusterstyles} : {};
				this.map_clustered = new MarkerClusterer(this.map, markers, markerClustererExtras);
			}else{
				for (var i = 0; i < markers.length; i++) {
					markers[i].setMap(this.map);
				}
			}

			if(this.options.fitToBounds){
				this.map.fitBounds(bounds);
			}

			this.addResetButton();
			this.monitorEscapeButton();
		},//makeMap
		
		////////////////////////////////////////////////////////////////////////////////
		
		centrePoint: function(){
			return new google.maps.LatLng(this.options.lat_default, this.options.lon_default);
		},//centrePoint
		
		////////////////////////////////////////////////////////////////////////////////
		
		resetMap: function(){
			this.hideOverlay();
			this.map.setZoom(this.options.zoomlevel_default);
			this.map.setCenter(this.centrePoint());
		},//resetMap
	
		////////////////////////////////////////////////////////////////////////////////
	
		markerClickHandler: function(event,marker,baseClass){
			if(baseClass.options.focus_centre){
				baseClass.map.panTo(marker.getPosition());
			}
			if(baseClass.options.focus_zoom){
				var currentZoom = baseClass.map.getZoom();
				if(currentZoom < baseClass.options.zoomlevel_near){
					baseClass.map.setZoom(baseClass.options.zoomlevel_near);
				}
			}
			if(baseClass.options.hasInfoWindow){
				baseClass.showOverlay(baseClass.map, marker);
			}
		},//markerClickHandler
		
		////////////////////////////////////////////////////////////////////////////////
	
		showOverlay: function(map,marker){
			if(!this.infowindow){
				this.infowindow = $('<div class="overlay window"><div class="wrapper"><div class="inner"><a class="close button" href="#">' +this.text.close+ '</a><div class="content"></div></div></div></div>');
				this.infowindowContent = this.infowindow.find('div.content');
				var mhmli_googlemaps_rootclass = this;
				this.infowindow.find('a.close').click(function(e){
					e.preventDefault();
					mhmli_googlemaps_rootclass.hideOverlay();
				});
				this.mapContainer.append(this.infowindow);
			}
			this.fillOverlay(marker.markerContent);
			this.infowindow.show();
		},//showOverlay
		
		////////////////////////////////////////////////////////////////////////////////
	
		fillOverlay: function(content){
			if(this.infowindowContent){
				this.infowindowContent.html(content);
			}
		},//fillOverlay
		
		////////////////////////////////////////////////////////////////////////////////
	
		hideOverlay: function(){
			if(this.infowindow){
				this.infowindowContent.empty();
				this.infowindow.hide();
			}
		},//hideOverlay
		
		////////////////////////////////////////////////////////////////////////////////
	
		addResetButton: function(){
			if(this.controls.resetbutton){
				this.resetButton = $('<button class="reset">' +this.text.reset+ '</button>');
				baseClass=this;
				this.resetButton.click(function(event){
					baseClass.resetMap();
				});
				$(this.map.b).append(this.resetButton);
			}
		},//addResetButton
		
		////////////////////////////////////////////////////////////////////////////////
	
		monitorEscapeButton: function(){
			mhmli_googlemaps_rootclass = this;
			$(document).keydown(function(e) {
				if(e.keyCode === 27 && mhmli_googlemaps_rootclass.hideOverlay){
					mhmli_googlemaps_rootclass.hideOverlay();
				}
			});
		}//monitorEscapeButton
	
	};
})(jQuery);