'use strict';
moduloCategoria.factory('categoriaService', ['serverService', function (serverService) {
        return {
            getFields: function () {
                return [
                    {name: "id", shortname: "ID", longname: "Identificador", visible: true, type: "id"},
                    {name: "nombre", shortname: "Nombre", longname: "Nombre", visible: true, type: "text", required: true,pattern: serverService.getRegExpr("alpha-numeric"), help: serverService.getRegExpl("alpha-numeric")},
                ];
            },
            getIcon: function () {
                return "fa-tags";
            },
            getObTitle: function () {
                return "categoria";
            },
            getTitle: function () {
                return "categoria";
            }
        };
    }]);


