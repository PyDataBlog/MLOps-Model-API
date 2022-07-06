import {bootstrap, Component, View, NgIf, bind, Inject, Observable, FormBuilder} from 'angular2/angular2';
import {People} from '../people/people';

@Component({
  selector: 'main'
})

@View({
  templateUrl: './components/main/main.html',
  directives: [People]
})

export class Main {
  constructor() { }
}