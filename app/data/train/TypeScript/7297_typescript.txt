import { Injectable,EventEmitter,Output } from '@angular/core';
import { Storage } from '@ionic/storage';


@Injectable()
export class SharedService {

    @Output() notifyUserLoggedIn: EventEmitter<any> = new EventEmitter();
    data: any;
    private userLoggedIn:Boolean=false;

    constructor() {}

    setUserName(data:any){
      localStorage.setItem('currentUser', JSON.stringify({name: data }));
      this.notifyUserLoggedIn.emit(localStorage.getItem('currentUser'));
    }
    getUserName(){
      if(localStorage.getItem('currentUser') != null){
        var userName= JSON.parse(localStorage.getItem('currentUser'));
        return userName;
    }
      return;
    }

    setUserState(val) {
        this.userLoggedIn = val;
    }

    getUserState() {
        return this.userLoggedIn ;
    }

}
