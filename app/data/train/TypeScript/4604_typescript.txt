import { Component } from '@angular/core';
import { NavController } from 'ionic-angular';
import { ServiceProvider} from "../../providers/service/service";
import { ControlUserApp } from '../../control/ControlUserApp';
import { UserApp } from '../../model/UserApp';

@Component({
  selector: 'page-home',
  templateUrl: 'home.html',
  providers: [
    ServiceProvider,
    ControlUserApp
  ]
})

export class HomePage {

  constructor(
    public navCtrl: NavController,
    public movProvides: ServiceProvider,
    public controlUser: ControlUserApp,
    public userApp : UserApp
  ) {}


  gravarTeste(){
    console.log('GRAVANDO DADOS');
      //this.userApp = new UserApp();
      //this.userApp.dsLogin  = "OLA MUNDO";
      //this.userApp.dsSenha  = "OLA MUNDO";
      //this.controlUser.save(this.userApp);
      console.log('DADOS GRAVADOR');
  }

  consultaTeste(){
    this.controlUser.findAll();
    console.log('CONSULTOU DADOS');
  }

  testeService(){
    console.log('EFETUANDO CONSULTA');
    this.movProvides.getTest().subscribe(
      data=>{
        const response = (data as any);
        console.log('RETORNO DO SERVIDOR : ' +response);
        console.log('RETORNO DO SERVIDOR : ' +response._body);
        const objeto_retorno = JSON.parse(response._body);
        console.log('RETORNO DO SERVIDOR : ' +objeto_retorno);
      },
      error=>{
        console.log('Erro executado : '+error);
      })
  }
}
