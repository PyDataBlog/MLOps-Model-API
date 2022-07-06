import {Vereador} from "../Modelors/Vereador";
import {Propositura} from "../Modelors/Propositura";
import {Sessao} from "../Modelors/Sessao";
import {SessaoVereadores} from "../Modelors/SessaoVereadores";
/**
 * Created by Bruno Melo on 10/06/17.
 */

export class DataBase {

  private _vereadores: Vereador[] = [];
  private _proposituras: Propositura[] = [];
  private _sessao: Sessao[] = [];
  private _sessaoVereadores: SessaoVereadores[] = [];

  get vereadores(): Vereador[] {
    return this._vereadores;
  }

  set vereadores(value: Vereador[]) {
    this._vereadores = value;
  }

  get proposituras(): Propositura[] {
    return this._proposituras;
  }

  set proposituras(value: Propositura[]) {
    this._proposituras = value;
  }

  get sessao(): Sessao[] {
    return this._sessao;
  }

  set sessao(value: Sessao[]) {
    this._sessao = value;
  }

  get sessaoVereadores(): SessaoVereadores[] {
    return this._sessaoVereadores;
  }

  set sessaoVereadores(value: SessaoVereadores[]) {
    this._sessaoVereadores = value;
  }
}
