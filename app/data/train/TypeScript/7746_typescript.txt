import {Component} from '@angular/core';
import {Platform} from 'ionic-angular';
import {SocialSharing} from 'ionic-native';

/*
 This component is intented to be as an easy and simple example of:
 http://ionicframework.com/docs/v2/native/social-sharing/
 */

@Component({
    templateUrl: 'build/pages/share/share.html'
})
export class SharePage {

    public shareMsg:string;

    constructor(private platform:Platform) {
        this.platform = platform;
        this.shareMsg = "Hey! I'm currently in a workshop at http://angularcamp.org starting an Ionic 2 app! @AngularCamp #AngularCamp";
    }

    share() {
        this.platform.ready().then(() => {
            SocialSharing.share(this.shareMsg, null, null, null);
        });
    };

    shareViaTwitter() {
        this.platform.ready().then(() => {
            if (SocialSharing.canShareVia("twitter")) {
                SocialSharing.shareViaTwitter(this.shareMsg, null, null);
            }
        });
    };

    shareViaFacebook() {
        this.platform.ready().then(() => {
            if (SocialSharing.canShareVia("facebook")) {
                SocialSharing.shareViaFacebookWithPasteMessageHint(this.shareMsg, null, null, 'Paste your facebook comment!');
            }
        });
    };
}
