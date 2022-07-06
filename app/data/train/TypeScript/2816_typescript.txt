import {Component, OnInit, Input} from '@angular/core';
import {IDream} from "../shared/models/dream.interface";

@Component({
  selector: 'app-dream-balance',
  templateUrl: 'dream-balance.component.html',
  styleUrls: ['dream-balance.component.css']
})
export class DreamBalanceComponent implements OnInit {

  @Input('wallet')
  wallet: number;

  @Input('dreams')
  dreams: IDream[];

  private isReady = false;

  constructor() { }

  ngOnInit() {
    if (this.wallet) {
      this.isReady = true;
    }
  }

  get currentBalance() {
    let dreamCovered = this.dreams.reduce((prev, dream) => prev + dream.currentlyCovered, 0);
    return this.wallet - dreamCovered;
  }

}
