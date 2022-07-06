import { Component} from '@angular/core';
import {RenderingUIAbstractComponent} from "../../../main/components/rendering-iu/rendering-ui.abstract.component";

@Component({
    selector:'gota-home-content',
    moduleId :  module.id || __moduleName,
    templateUrl: './product-detail-content.component.html',
    styleUrls: ['./product-detail-content.component.css']
})
export class ProductDetailContentContentComponent extends RenderingUIAbstractComponent {
    renderUI() {

    }

}