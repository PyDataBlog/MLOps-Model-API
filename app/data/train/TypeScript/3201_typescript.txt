/// <reference path="./typings/app.d.ts" />
import {Component, View, ElementRef, bootstrap, FORM_DIRECTIVES} from 'angular2/angular2';

@Component({
    selector: 'pr-list'
})
@View({
    directives: [FORM_DIRECTIVES],
    template: `
        List of Pull Requests
    `
})
export class PrList {

}
