import { Component, OnInit } from '@angular/core';
import { Router, ActivatedRoute, Params } from '@angular/router';
import { Thing } from './thing';

@Component({
  selector: 'app-thing',
  templateUrl: './thing.component.html',
  styleUrls: ['./thing.component.css']
})
export class ThingComponent implements OnInit {
  private thing1: Thing;
  private thing2: Thing;
  private thing3: Thing;
  private things: Thing[];
  public paramId;

  constructor(private _route: ActivatedRoute, private _router: Router) {
    this.thing1 = new Thing(1, 'Thing 1', 'A');
    this.thing2 = new Thing(2, 'Thing 2', 'B');
    this.thing3 = new Thing(3, 'Thing 3', 'A');
    this.things = [this.thing1, this.thing2, this.thing3];
  }

  ngOnInit() {
    this._route.params.forEach((params: Params) => {
      console.log(params);
      this.paramId = Number(params['id']);
    });
  }

  redirect(id: number) {
    this._router.navigate(['/thing', id]);
  }

}
