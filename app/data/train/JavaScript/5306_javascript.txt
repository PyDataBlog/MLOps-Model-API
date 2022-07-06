/**
 Create by Huy: codocmm@gmail.com ~ nqhuy2k6@gmail.com
 07/31/2015
 */
define(["durandal/app", "knockout", "bootstrap", "viewmodels/component-4"], function (app, ko, bootstrap, Component4) {
    return function () {
        var me = this;
        var dashboardViewModel = this;
        dashboardViewModel.compoment4 = ko.observable();
        dashboardViewModel.activate = function () {
            me.compoment4(new Component4(0))
        }
    }
});

