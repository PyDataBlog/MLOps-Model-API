/**
 * Created by andypax on 15/10/16.
 */
export class PasseadorModel {

    constructor(
        public nome?: string,
        public sobreNome?: string,
        public endereco?: string,
        public numero?: string,
        public complemento?: string,
        public cidade?: string,
        public cep?: string,
        public estado?: string,
        public telefone?: string,
        public celular?: string,
        public foto?: string,
        public email?: string,
        public preco?: number,
        public login?: string,
        public senha?: string,
        public confirmarSenha?: string
    ){}
}