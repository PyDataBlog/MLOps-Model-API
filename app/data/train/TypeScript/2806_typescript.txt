import { ChangeDetectionStrategy, Component } from '@angular/core';

// webpack html imports
let template = require('./tab1.component.html');

@Component({
  selector: 'tabs-demo',
  changeDetection: ChangeDetectionStrategy.OnPush,
  template: template
})
export class TabsDemoComponent {

  public tabs:Array<any> = [
    {title: 'Details', content: 'Dynamic content 1'},
    {title: 'Company Profile', content: 'Dynamic content 2'},
    {title: 'Tag', content: 'Dynamic content 3'},
    {title: 'Reviews (4)', content: ''}
  ];

  public alertMe():void {
    setTimeout(function ():void {
      alert('You\'ve selected the alert tab!');
    });
  };

  public setActiveTab(index:number):void {
    this.tabs[index].active = true;
  };

  public removeTabHandler(/*tab:any*/):void {
    console.log('Remove Tab handler');
  };
}
