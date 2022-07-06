import {Component, View, CORE_DIRECTIVES} from 'angular2/angular2';
import {ROUTER_DIRECTIVES} from 'angular2/router';
import UserRepo from '../../services/repositories/user_repo';
import User from '../../services/models/user';
import {Alert} from '../../ng2-bootstrap/alert/alert';
@Component({
    selector: 'home'
})
@View({
    templateUrl: './components/home/home.html?v=<%= VERSION %>',
    directives: [CORE_DIRECTIVES, ROUTER_DIRECTIVES, Alert]
})
export class Home {
    private loading:boolean;
    //Bootstrap alert
    private alertOpened:boolean = true;
    userName:string ='';
    constructor(private repo:UserRepo) { console.log(UserRepo); }
    addUser(currentUser) {
        this.loading = true;
        this.repo.getUser(currentUser.value)
            .then(u => {
                this.loading = false;
                this.userName = u.name;
            });
        currentUser.value = '';
    }
    getAll():User[] {
        return this.repo.getAll();
    }
    remove(user:User) {
        this.repo.remove(user);
    }
    //Bootstrap Alert
    close(evt:MouseEvent) {
        evt.preventDefault();
        this.alertOpened = false;
    }

    log(msg:string) {
        console.log(msg);
    }

    toggle() {
        this.alertOpened = !this.alertOpened;
    }
}
