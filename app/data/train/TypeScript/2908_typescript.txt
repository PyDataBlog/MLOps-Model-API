import { Component } from '@angular/core';

@Component({
    selector: 'my-app',
    styles: [`h1 {
	color: white;
	background: darkgray;
	padding: 20px;
}
`],
    template: `
	    <meta http-equiv="Cache-Control" content="no-cache, no-store, must-revalidate">
		<meta http-equiv="Pragma" content="no-cache">
		<meta http-equiv="Expires" content="0">
		<h1>{{name}}</h1>
		<a [routerLink]="['/']">Home</a> | <a [routerLink]="['/about/', { id: 2 }]">About</a>
		<router-outlet></router-outlet>
	`,

})
export class AppComponent {
    name: string = "Literature Lantern";

    constructor() {}
}