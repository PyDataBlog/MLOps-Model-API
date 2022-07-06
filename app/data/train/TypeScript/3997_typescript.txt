import { Component, Input } from '@angular/core';

@Component({
  selector: 'my-countdown',
  templateUrl: './countdown.component.html'
})
export class CountdownComponent {
  @Input() milliseconds: number;

  constructor() {
  }

  getHours() {
    return ('0' + Math.floor(this.milliseconds / 3600000)).slice(-2);
  }

  getMinutes() {
    return ('0' + Math.floor((this.milliseconds % 3600000) / 60000)).slice(-2);
  }

  getSeconds() {
    return ('0' + Math.floor((this.milliseconds % 60000) / 1000)).slice(-2);
  }
}
