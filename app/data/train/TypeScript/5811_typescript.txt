import { Component } from "@angular/core";

@Component({
  selector: 'app-server',
  templateUrl: './server.component.html'
})
export class ServerComponent {
  serverId = 10;
  serverStatus:string = 'offline';

  getServerStatus() {
    return "Good to go";
  }

  constructor() {
    this.serverStatus = Math.random() > 0.5 ? 'online' : 'offline';
  }

  getColor() {
    return this.serverStatus === 'online' ? 'green' : 'red';
  }


}
