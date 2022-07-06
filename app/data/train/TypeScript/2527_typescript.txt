import { Component, OnInit } from '@angular/core';
import { ListService } from '../../services/list.service';
import {Store} from "../../app.store";

@Component({
  selector: 'seker-questions-list',
  styleUrls: ['./questions-list.component.css'],
  template: `
    <ul class="todo-list">
     <seker-question *ngFor="let item of store.state.list" 
     [item]="item">
     </seker-question>
   </ul>

  `
})
export class QuestionsListComponent implements OnInit {
private store: Store;

  // needs ListService in order to init the list
  constructor(  _store: Store ,  private listService : ListService) {
  this.store = _store;
  }

  ngOnInit() {
  }

}
