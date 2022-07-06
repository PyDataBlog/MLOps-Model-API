import {Component, View} from "angular2/core";
import {FORM_DIRECTIVES} from "angular2/common";
import {bootstrap} from "angular2/platform/browser";

import {Window1} from "./Window1";
import {Window2} from "./Window2";

@Component({
    selector: "fluid-window-app"
})
@View({
    templateUrl: "modules/ng-school/display/template.html",
    directives: [
        FORM_DIRECTIVES,
        Window1,Window2
    ],
})
export class FluidWindowExample {

    constructor() {

    }

}
bootstrap(FluidWindowExample);
