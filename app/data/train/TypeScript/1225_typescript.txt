import {Component} from 'angular2/core';

@Component({
  selector: 'home',
  templateUrl: './components/home/home.html',
  styleUrls: ['./components/home/home.css']
})
export class HomeCmp {

  constructor() {
    //
    // a must for initiating materialize components
    //

    // see: http://materializecss.com/tabs.html
    $('ul.tabs').tabs();

    // see: http://materializecss.com/forms.html
    $('select').material_select();

  }
}

