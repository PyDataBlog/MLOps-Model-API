(function() {

    'use strict';

    function HomeController($scope, $q, $timeout) {

        var self = this;

        self.time    = 50;
        self.message = "Mensagem para mostrar";

        var promiseTimeout;
        var deferredToast;

        self.mostrarToast = function() {
            esconderToast();

            deferredToast = $q.defer();
            deferredToast.promise.then(toastResolved, toastRejected);

            self.activeShow    = true;
            self.mensagemAtual = self.message;

            promiseTimeout     = $timeout(timeoutToast, self.time * 1000);
        };

        function timeoutToast() {
            deferredToast.reject();

            esconderToast();
        }

        function esconderToast() {
            if (promiseTimeout) {
                $timeout.cancel(promiseTimeout);

                promiseTimeout = undefined;
            }

            self.activeShow = false;
        }

        self.clickBotaoFechar = function() {
            if (deferredToast) { // verifica pra evitar problema com duplo clique rápido
                deferredToast.resolve();

                deferredToast = undefined;
            }

            esconderToast();
        };

        self.clickNoToast = function() {
            if (deferredToast) {
                timeoutToast(); // clicar no toast equivale ao timeout
            }
        };

        function toastResolved() {
            console.log('Resolved');

            self.ultimaResposta = 'Resolved';
        }

        function toastRejected() {
            console.log('Rejected');

            self.ultimaResposta = 'Rejected';
        }
    }

    angular
        .module('hotMilhasApp')
        .controller('HomeController', ['$scope', '$q', '$timeout', HomeController]);

})();