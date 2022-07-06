import {Component, OnInit,Input} from 'angular2/core';
import {Identifier} from 'gedcomx';
import {CollapsibleFieldsetComponent} from './collapsibleFieldset.component';

@Component({
    selector: 'identifier',
    template: `
    <collapsibleFieldset legendLabel="Identifier">
      <button (click)="createIdentifier()" *ngIf="!data">Set Identifier</button>
      <div *ngIf="data">
        <label>
          Value: <input [(ngModel)]="data.value" placeholder="Value"/>
        </label>
        <label *ngIf="data">Type:
          <select [(ngModel)]="data.type">
            <option *ngFor="#type of getAllIdentifierTypes()">{{type}}</option>
          </select>
        </label>
      </div>
    </collapsibleFieldset>
    `,
    inputs:['data'],
    directives:[CollapsibleFieldsetComponent]
})
export class IdentifierComponent implements OnInit {
    data: Identifier;
    constructor() { }
    ngOnInit() { }
    getAllIdentifierTypes() {
        return [];
    }
}
