import {Component} from 'angular2/core';
import {DataService} from './services/data.service';
import {UserService} from './services/user.service';
import {ToolService} from './services/tools.service';
import {Router, ROUTER_DIRECTIVES} from 'angular2/router';

@Component({
    selector: 'my-login',
    templateUrl: './templates/login.tpl.html',
    styleUrls: ['../src/css/login.css'],
    directives: [ROUTER_DIRECTIVES]
})
export class LoginComponent {

    show_error: Object;

    constructor(
        private _dataService: DataService, 
        private _router: Router, 
        private _userService: UserService, 
        private _tools: ToolService) {}

    onSubmit(form) {
        this._dataService.postData(form.value, 'login')
        .subscribe(
            data => this.loginUser(data),
            error => this.handleError(error)
        );
    }

    loginUser(data) {
        this._tools.setCookie('user_session', data.response.user.session_id);
        this._tools.setCookie('username', data.response.user.username);
        
        this._userService.setUserData()
        .subscribe(
            success => {
                this._router.navigate(['MyClients', {group: 'all'}]);
            }, 
            error => console.log(error)
        );
    }

    handleError(error) {
        this.show_error = error;
        console.log(error);
    }
}
