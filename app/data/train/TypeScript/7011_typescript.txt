import { Injectable, Component } from '@angular/core';
import { VendedorCliente } from "../model/VendedorCliente";
import { DaoVendedorCliente } from '../dao/DaoVendedorCliente';
import { ToastMensagem } from '../providers/toast/toast'


@Component({
  providers: [
    DaoVendedorCliente
  ]
})

@Injectable()
export class ControlVendedorCliente {

  constructor (
    public daoObj : DaoVendedorCliente,
    public toast: ToastMensagem ){

  }

  save(obj: VendedorCliente): any {
    this.daoObj.save(obj);
  }

  update(obj: VendedorCliente): any{
    this.daoObj.save(obj);
  }

  delete(obj: VendedorCliente): any{
    this.daoObj.delete(obj);
  }

  findAll(): any{
      this.daoObj.findAll();
  }

  findById(id: string){
      this.daoObj.findById(id);
  }


}
