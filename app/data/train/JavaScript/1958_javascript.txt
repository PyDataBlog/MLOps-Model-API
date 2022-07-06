function __processArg(obj, key) {
    var arg = null;
    if (obj) {
        arg = obj[key] || null;
        delete obj[key];
    }
    return arg;
}

function Controller() {
    function goSlide(event) {
        var index = event.source.mod;
        var arrViews = $.scrollableView.getViews();
        switch (index) {
          case "0":
            $.lbl1.backgroundColor = "#453363";
            $.lbl1.color = "#AA9DB6";
            $.lbl2.backgroundColor = "#E6E7E9";
            $.lbl2.color = "#4CC4D2";
            $.lbl3.backgroundColor = "#E6E7E9";
            $.lbl3.color = "#4CC4D2";
            break;

          case "1":
            $.lbl1.backgroundColor = "#E6E7E9";
            $.lbl1.color = "#4CC4D2";
            $.lbl2.backgroundColor = "#453363";
            $.lbl2.color = "#AA9DB6";
            $.lbl3.backgroundColor = "#E6E7E9";
            $.lbl3.color = "#4CC4D2";
            break;

          case "2":
            $.lbl1.backgroundColor = "#E6E7E9";
            $.lbl1.color = "#4CC4D2";
            $.lbl2.backgroundColor = "#E6E7E9";
            $.lbl2.color = "#4CC4D2";
            $.lbl3.backgroundColor = "#453363";
            $.lbl3.color = "#AA9DB6";
        }
        $.scrollableView.scrollToView(arrViews[index]);
    }
    require("alloy/controllers/BaseController").apply(this, Array.prototype.slice.call(arguments));
    this.__controllerPath = "contact";
    if (arguments[0]) {
        var __parentSymbol = __processArg(arguments[0], "__parentSymbol");
        {
            __processArg(arguments[0], "$model");
        }
        {
            __processArg(arguments[0], "__itemTemplate");
        }
    }
    var $ = this;
    var exports = {};
    var __defers = {};
    $.__views.win = Ti.UI.createView({
        layout: "vertical",
        id: "win",
        backgroundColor: "white"
    });
    $.__views.win && $.addTopLevelView($.__views.win);
    $.__views.__alloyId87 = Alloy.createController("_header", {
        id: "__alloyId87",
        __parentSymbol: $.__views.win
    });
    $.__views.__alloyId87.setParent($.__views.win);
    $.__views.__alloyId88 = Ti.UI.createView({
        height: "20%",
        backgroundColor: "#836EAF",
        id: "__alloyId88"
    });
    $.__views.win.add($.__views.__alloyId88);
    $.__views.__alloyId89 = Ti.UI.createLabel({
        text: "Contact us",
        left: "10",
        top: "10",
        color: "white",
        id: "__alloyId89"
    });
    $.__views.__alloyId88.add($.__views.__alloyId89);
    $.__views.menu = Ti.UI.createView({
        id: "menu",
        layout: "horizontal",
        height: "50",
        width: "100%"
    });
    $.__views.win.add($.__views.menu);
    $.__views.lbl1 = Ti.UI.createLabel({
        text: "Our Offices",
        id: "lbl1",
        mod: "0",
        height: "100%",
        width: "33%",
        textAlign: "center",
        backgroundColor: "#453363",
        color: "#AA9DB6"
    });
    $.__views.menu.add($.__views.lbl1);
    goSlide ? $.__views.lbl1.addEventListener("touchend", goSlide) : __defers["$.__views.lbl1!touchend!goSlide"] = true;
    $.__views.__alloyId90 = Ti.UI.createView({
        backgroundColor: "#4CC4D2",
        height: "100%",
        width: "0.45%",
        id: "__alloyId90"
    });
    $.__views.menu.add($.__views.__alloyId90);
    $.__views.lbl2 = Ti.UI.createLabel({
        text: "Care Center",
        id: "lbl2",
        mod: "1",
        height: "100%",
        width: "33%",
        textAlign: "center",
        backgroundColor: "#E6E7E9",
        color: "#4CC4D2"
    });
    $.__views.menu.add($.__views.lbl2);
    goSlide ? $.__views.lbl2.addEventListener("touchend", goSlide) : __defers["$.__views.lbl2!touchend!goSlide"] = true;
    $.__views.__alloyId91 = Ti.UI.createView({
        backgroundColor: "#4CC4D2",
        height: "100%",
        width: "0.45%",
        id: "__alloyId91"
    });
    $.__views.menu.add($.__views.__alloyId91);
    $.__views.lbl3 = Ti.UI.createLabel({
        text: "XOX Dealers",
        id: "lbl3",
        mod: "2",
        height: "100%",
        width: "33%",
        textAlign: "center",
        backgroundColor: "#E6E7E9",
        color: "#4CC4D2"
    });
    $.__views.menu.add($.__views.lbl3);
    goSlide ? $.__views.lbl3.addEventListener("touchend", goSlide) : __defers["$.__views.lbl3!touchend!goSlide"] = true;
    var __alloyId92 = [];
    $.__views.__alloyId93 = Alloy.createController("contact1", {
        id: "__alloyId93",
        __parentSymbol: __parentSymbol
    });
    __alloyId92.push($.__views.__alloyId93.getViewEx({
        recurse: true
    }));
    $.__views.__alloyId94 = Alloy.createController("contact2", {
        id: "__alloyId94",
        __parentSymbol: __parentSymbol
    });
    __alloyId92.push($.__views.__alloyId94.getViewEx({
        recurse: true
    }));
    $.__views.__alloyId95 = Alloy.createController("contact3", {
        id: "__alloyId95",
        __parentSymbol: __parentSymbol
    });
    __alloyId92.push($.__views.__alloyId95.getViewEx({
        recurse: true
    }));
    $.__views.scrollableView = Ti.UI.createScrollableView({
        views: __alloyId92,
        id: "scrollableView",
        showPagingControl: "false",
        scrollingEnabled: "false"
    });
    $.__views.win.add($.__views.scrollableView);
    exports.destroy = function() {};
    _.extend($, $.__views);
    var storeModel = Alloy.createCollection("storeLocator");
    var details = storeModel.getStoreList();
    console.log(details);
    __defers["$.__views.lbl1!touchend!goSlide"] && $.__views.lbl1.addEventListener("touchend", goSlide);
    __defers["$.__views.lbl2!touchend!goSlide"] && $.__views.lbl2.addEventListener("touchend", goSlide);
    __defers["$.__views.lbl3!touchend!goSlide"] && $.__views.lbl3.addEventListener("touchend", goSlide);
    _.extend($, exports);
}

var Alloy = require("alloy"), Backbone = Alloy.Backbone, _ = Alloy._;

module.exports = Controller;