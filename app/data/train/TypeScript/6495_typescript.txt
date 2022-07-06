/// <reference path="../../_references.d.ts" />

import Backbone = require('backbone');
import HeaderView = require('./headerview');

class LayoutView extends Backbone.View<Backbone.Model> {
    headerView: HeaderView;

    constructor(options?: Backbone.ViewOptions<Backbone.Model>) {
        this.el = '#layout';
        this.__setupHeader();
        super(options);
    }

    private __setupHeader() {
        var headermodel = new Backbone.Model({
            Brand: 'TypeScript Backbone Bootstrap Boilerplate'
        });
        this.headerView = new HeaderView();
        this.headerView.model = headermodel;
    }

    public render() {
        this.headerView.render();
        return this;
    }
}

export = LayoutView;