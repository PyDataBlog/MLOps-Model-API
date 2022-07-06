/**
 * Created by BlueX on 6/11/16.
 */
angular
    .module('TK-WEB-PITCH')
    .factory('ConstantFactory', function(){

            //var API_URL = 'http://127.0.0.1:8888/TK-API/';
            var API_URL = 'http://api-test.tarangkhmer.com/';

            //  return $http.get('http://192.168.1.100:8888/TK-API/sportclubs.json', config);
        return {
            API_URL: API_URL

        }

    });