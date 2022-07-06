/**
 *  App
 *
 *  Root application file.
 *  Load main controllers here.
 */
 define([
    "jquery",
    "modules/connected-devices/index"
], function(
    $,
    ConnectedDevices
) {
    "use strict";

    var retriveData = function(callback) {
            var connected_devices_request = $.getJSON("data/connected-devices.json", { cache: + new Date() });

            $.when(connected_devices_request)
             .then(function(connected_devices_data) {
                callback(null, connected_devices_data);
             }, function() {
                callback(new Error("An error has occurred requesting init data"));
            });
         },
         app = {},
         connected_devices;


    /*
     *
     *
     */
    app.init = function() {
        var $app_el = $(".js-app");

        if ($app_el.length < 1) {
            throw new Error("Placeholder element is not available");
        }

        retriveData(function(err, connected_devices_data) {
            if (err) {
                return window.console.error(err);
            }

            connected_devices = new ConnectedDevices({
                el: $app_el.get(0),
                data: connected_devices_data
            });

            connected_devices.render();

        });

        return true;
    };


    /*
     *
     *
     */
    app.destroy = function() {
        connected_devices.destroy();
    };


    /*
     *
     *
     */
    app.update = function() {
        retriveData(function(err, connected_devices_data) {
            if (err) {
                return window.console.error(err);
            }

            connected_devices.update({
                data: connected_devices_data
            });
        });
    };


    return app;
});
