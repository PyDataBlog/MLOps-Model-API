import {Component} from 'angular2/core';
import {UserService} from '../services/user.services';
@Component({
    selector: 'Sign-in',
    template: `
    <div id="Signinmodal" class="modal fade" role="dialog" style="margin-left:4cm;">
  <div class="modal-dialog">

    <!-- Modal content-->
    <div class="modal-content">
      <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal">&times;</button>
        <h4 class="modal-title">Sign In to out web site </h4>
      </div>
      <div class="modal-body">
      <img src="../../assets/svgimages/ripple.svg"><br>
        <i class="mdi mdi-account">Login</i>
        <input type="text" class="form-control" [(ngModel)]="email" /><br><br>
        <i class="mdi mdi-account md-70">Password</i>
        <input type="password" class="form-control" [(ngModel)]="Password" /><br><br>
        <button type="submit"
        class="mdc-button

               mdc-button--raised
               mdc-button--primary
               mdc-ripple-surface"
                  (click)="Connect()"
        data-mdc-auto-init="MDCRipple" id="but">
<div [innerHTML]="spinonbutt"></div>

</button>


      </div>
      <div [innerHTML]="htmlStr" style="margin-left: 5cm;"></div>
      <div class="modal-footer">
        <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
      </div>
    </div>

  </div>
    `,
    styleUrls: ["../../assets/css/signinstyle.css"],
    providers: [UserService]
})
export class SignInComponent {

    private spinonbutt='Sign In' :string;
    private email:string;
    private  Password:string;
    private  htmlStr = ''
:
    string;

    constructor(private _userservice:UserService) {
    }

    Connect() {

        this.spinonbutt = '<img src="../../assets/svgimages/ripple.svg" width="25px" height="25px">';
        this._userservice.Authentification(this.email, this.Password).subscribe(
            (data)=>
        if (data == 0) {
            this.spinonbutt = 'Sign In';
            this.htmlStr = '<p style="color:red;">Error !! Please check your details</p>';
        } else if (data == 2) {
            this.spinonbutt = 'Sign In';
            this.htmlStr = '<p style="color:red;">Error !! Your account is not activated yet !!' +
                'please check your email and click on the link below to activate ' +
                'your account </p>';
        } else {
            this.htmlStr = 'ahla wasahla';
        }
    )
        ;


    }
}
