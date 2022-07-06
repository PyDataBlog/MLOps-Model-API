import { Component } from '@angular/core';
import { NavController, AlertController } from 'ionic-angular';

@Component({
    templateUrl: 'build/pages/alert/alert.html',
})
export class AlertPage {
    user: string = 'nome';
    constructor(public navCtrl: NavController, public alertCtrl: AlertController) { }
    showAlert() {
        let alert = this.alertCtrl.create({
            title: 'Alert Test',
            message: "Digite seu nome",
            inputs: [
                {
                    name: 'nome',
                    placeholder: 'Seu nome'
                }
            ],
            buttons: [
                {
                    text: 'Cancelar'
                },
                {
                    text: 'Ok',
                    handler: (data) => {
                        this.user = data.nome;
                    }
                }
            ]
        });

        alert.present();
    }
}
