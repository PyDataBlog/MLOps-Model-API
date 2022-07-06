function Controller() {
    function __alloyId24() {
        __alloyId24.opts || {};
        var models = __alloyId23.models;
        var len = models.length;
        var rows = [];
        for (var i = 0; len > i; i++) {
            var __alloyId9 = models[i];
            __alloyId9.__transform = {};
            var __alloyId10 = Ti.UI.createTableViewRow({
                layout: "vertical",
                font: {
                    fontSize: "16dp"
                },
                height: "auto",
                title: "undefined" != typeof __alloyId9.__transform["nome"] ? __alloyId9.__transform["nome"] : __alloyId9.get("nome"),
                model: "undefined" != typeof __alloyId9.__transform["alloy_id"] ? __alloyId9.__transform["alloy_id"] : __alloyId9.get("alloy_id"),
                editable: "true"
            });
            rows.push(__alloyId10);
            var __alloyId12 = Ti.UI.createView({
                layout: "vertical"
            });
            __alloyId10.add(__alloyId12);
            var __alloyId14 = Ti.UI.createLabel({
                width: Ti.UI.SIZE,
                height: Ti.UI.SIZE,
                right: "10dp",
                color: "blue",
                font: {
                    fontSize: "16dp"
                },
                text: "undefined" != typeof __alloyId9.__transform["nome"] ? __alloyId9.__transform["nome"] : __alloyId9.get("nome")
            });
            __alloyId12.add(__alloyId14);
            var __alloyId16 = Ti.UI.createView({
                height: Ti.UI.SIZE,
                width: Ti.UI.FILL
            });
            __alloyId12.add(__alloyId16);
            var __alloyId18 = Ti.UI.createScrollView({
                scrollType: "horizontal",
                layout: "horizontal",
                horizontalWrap: "false"
            });
            __alloyId16.add(__alloyId18);
            var __alloyId20 = Ti.UI.createImageView({
                top: "15dp",
                image: "undefined" != typeof __alloyId9.__transform["foto1"] ? __alloyId9.__transform["foto1"] : __alloyId9.get("foto1"),
                height: "180dp",
                width: "320dp"
            });
            __alloyId18.add(__alloyId20);
            var __alloyId22 = Ti.UI.createImageView({
                top: "15dp",
                image: "undefined" != typeof __alloyId9.__transform["foto2"] ? __alloyId9.__transform["foto2"] : __alloyId9.get("foto2"),
                height: "180dp",
                width: "320dp"
            });
            __alloyId18.add(__alloyId22);
        }
        $.__views.tableviewContatos.setData(rows);
    }
    function openAdd1() {
        var add1 = Alloy.createController("add1");
        add1.getView().open({
            modal: true
        });
    }
    function maisDetalhes(e) {
        var contato = Alloy.Collections.contato.get(e.rowData.model);
        var ctrl = Alloy.createController("detalhesContato", contato);
        $.homeTab.open(ctrl.getView());
    }
    require("alloy/controllers/BaseController").apply(this, Array.prototype.slice.call(arguments));
    this.__controllerPath = "home";
    arguments[0] ? arguments[0]["__parentSymbol"] : null;
    arguments[0] ? arguments[0]["$model"] : null;
    arguments[0] ? arguments[0]["__itemTemplate"] : null;
    var $ = this;
    var exports = {};
    var __defers = {};
    $.__views.homeWindow = Ti.UI.createWindow({
        backgroundColor: "white",
        layout: "vertical",
        id: "homeWindow",
        titleid: "home"
    });
    $.__views.contatosSearch = Ti.UI.createSearchBar({
        hinttextid: "procurarText",
        height: "50dp",
        id: "contatosSearch",
        showCancel: "false"
    });
    $.__views.homeWindow.add($.__views.contatosSearch);
    $.__views.Btadd = Ti.UI.createButton({
        top: "10dp",
        width: "200dp",
        height: "auto",
        borderRadius: "10dp",
        font: {
            fontSize: "17dp"
        },
        title: L("adicionar"),
        id: "Btadd"
    });
    $.__views.homeWindow.add($.__views.Btadd);
    openAdd1 ? $.__views.Btadd.addEventListener("click", openAdd1) : __defers["$.__views.Btadd!click!openAdd1"] = true;
    $.__views.tableviewContatos = Ti.UI.createTableView({
        id: "tableviewContatos"
    });
    $.__views.homeWindow.add($.__views.tableviewContatos);
    var __alloyId23 = Alloy.Collections["contato"] || contato;
    __alloyId23.on("fetch destroy change add remove reset", __alloyId24);
    maisDetalhes ? $.__views.tableviewContatos.addEventListener("click", maisDetalhes) : __defers["$.__views.tableviewContatos!click!maisDetalhes"] = true;
    $.__views.homeTab = Ti.UI.createTab({
        backgroundSelectedColor: "#C8C8C8 ",
        backgroundFocusedColor: "#999",
        icon: "/images/ic_home.png",
        window: $.__views.homeWindow,
        id: "homeTab",
        titleid: "home"
    });
    $.__views.homeTab && $.addTopLevelView($.__views.homeTab);
    exports.destroy = function() {
        __alloyId23.off("fetch destroy change add remove reset", __alloyId24);
    };
    _.extend($, $.__views);
    Alloy.Collections.contato.fetch();
    var contatos = Alloy.Collections.contato;
    contatos.fetch();
    $.tableviewContatos.search = $.contatosSearch;
    __defers["$.__views.Btadd!click!openAdd1"] && $.__views.Btadd.addEventListener("click", openAdd1);
    __defers["$.__views.tableviewContatos!click!maisDetalhes"] && $.__views.tableviewContatos.addEventListener("click", maisDetalhes);
    _.extend($, exports);
}

var Alloy = require("alloy"), Backbone = Alloy.Backbone, _ = Alloy._;

module.exports = Controller;