using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;


namespace Savage_Hotel_System.Class
{
    class Funcoes
    {
        public Funcoes()
        {

        }

        public int verificatelefone(String entrada)
        {
            //Verifica Telefone
            //Se tudo estiver ok, retorna 0
            //Caso contrário retorna 1
            //Se o tamanho da entrada for diferente de 10 ou 11 retornar 2 (Casas incompletas ou maior que a necessária)
            int tam = entrada.Length;
            for (int i = 0; i < tam; i++)
            {
                if (entrada[i] > 47 && entrada[i] < 58)
                {
                    //cout<<entrada[i]<<endl;
                }
                else
                {
                    //cout<<entrada[i];
                    //cout<<" caracter invalido"<<endl;
                    return 1;
                }
            }
            if (tam < 10 || tam > 11)
            {
                return 2;
            }
            return 0;
        }

        /*public int verificanome(String entrada)
        {
            //Verifica Nome
            //Se tudo estiver ok, retorna 0
            //Caso contrário retorna 2 (Tamanho Mínino 1)
            //Caso contrário retorna 1 (caracteres Inválidos)
            int tam = entrada.Length;
            for (int i = 0; i < tam; i++)
            {
                if (((entrada[i] > 64 && entrada[i] < 91) || (entrada[i] > 96 && entrada[i] < 123)) || (entrada[i] == 32))
                {
                    //cout<<entrada[i];
                }
                else
                {
                    //cout<<entrada[i];
                    //cout<<" caracter invalido"<<endl;
                    return 1;
                }
            }
            if (tam <= 1)
            {
                return 2;
            }
            return 0;
        }*/

		public int verificanome(String entrada)
        {
            //Verifica Nome
            //Se tudo estiver ok, retorna 0
            //Caso contrário retorna 2 (Tamanho Mínino 1)
            //Caso contrário retorna 1 (caracteres Inválidos)
            int tam = entrada.Length;
            for (int i = 0; i < tam; i++)
            {
                if((int)entrada[i]>64 && (int)entrada[i] < 91){
				}else{
					if((int)entrada[i] > 96 && (int)entrada[i] < 123){
					}else{
						if((int)entrada[i] == 32){
						}else{
							if((int)entrada[i] > 127 && (int)entrada[i] < 145){
							}else{
								if((int)entrada[i] > 146 && (int)entrada[i] < 155){
								}else{
									if((int)entrada[i] > 159 && (int)entrada[i] < 166){
									}else{
										if((int)entrada[i] > 180 && (int)entrada[i] < 184){
										}else{
											if((int)entrada[i] > 197 && (int)entrada[i] < 200){
											}else{
												if((int)entrada[i] == 208){
												}else{
													if((int)entrada[i] > 209 && (int)entrada[i] < 217){
													}else{
														if((int)entrada[i] == 222){
														}else{
															if((int)entrada[i] == 224){
															}else{
																if((int)entrada[i] > 225 && (int)entrada[i] < 230){
																}else{
																	if((int)entrada[i] > 232 && (int)entrada[i] < 238){
																	}else{
																		//caracter inválido
																		return 1;
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
            }
            if (tam <= 1)
            {
                return 2;
            }
            return 0;
        }
		
        public int verificacpf(String entrada)
        {
            //Verifica CPF
            //Se tudo estiver ok, retorna 0
            //Caso contrário retorna 1
            //Se o tamanho da entrada for diferente de 11 retorna 2 (Casas incompletas ou maior que a necessária)
            int tam = entrada.Length;
            for (int i = 0; i < tam; i++)
            {
                if (entrada[i] > 47 && entrada[i] < 58)
                {
                    //cout<<entrada[i]<<endl;
                }
                else
                {
                    //cout<<entrada[i];
                    //cout<<" caracter invalido"<<endl;
                    return 1;
                }
            }
            if (tam != 11)
            {
                return 2;
            }
            return 0;
        }

        public int verificacnpj(String entrada)
        {
            //Verifica CPF
            //Se tudo estiver ok, retorna 0
            //Caso contrário retorna 1
            //Se o tamanho da entrada for diferente de 14 retorna 2 (Casas incompletas ou maior que a necessária)
            int tam = entrada.Length;
            for (int i = 0; i < tam; i++)
            {
                if (entrada[i] > 47 && entrada[i] < 58)
                {
                    //cout<<entrada[i]<<endl;
                }
                else
                {
                    //cout<<entrada[i];
                    //cout<<" caracter invalido"<<endl;
                    return 1;
                }
            }
            if (tam != 14)
            {
                return 2;
            }
            return 0;
        }

        public int verificasalario(String entrada)
        {
            //Verifica Salário
            //Se tudo estiver ok, retorna 0
            //Caso contrário retorna 1
            //Caso contrário retorna 2 (Tamanho Mínino 1)
            int tam = entrada.Length;
            if (tam < 1)
            {
                return 2;
            }
            for (int i = 0; i < tam; i++)
            {
                if (entrada[i] > 47 && entrada[i] < 58)
                {
                    //cout<<entrada[i]<<endl;
                }
                else
                {
                    //cout<<entrada[i];
                    //cout<<" caracter invalido"<<endl;
                    return 1;
                }
            }
            return 0;
        }

        public int verificanumeroquarto(String entrada)
        {
            //Verifica Numero do Quarto
            //Se tudo estiver ok, retorna 0
            //Caso contrário retorna 1
            //Se o tamanho da entrada for diferente de 4 retorna 2 (Casas incompletas ou maior que a necessária)
            int tam = entrada.Length;
            for (int i = 0; i < tam; i++)
            {
                if (entrada[i] > 47 && entrada[i] < 58)
                {
                    //cout<<entrada[i]<<endl;
                }
                else
                {
                    //cout<<entrada[i];
                    //cout<<" caracter invalido"<<endl;
                    return 1;
                }
            }
            if (tam != 4)
            {
                return 2;
            }
            return 0;
        }

        public int verificaQuantidade(String entrada)
        {
            //Verifica quantidade generico (valor inteiro maior igual a 0)
            //Se tudo estiver ok, retorna 0
            //Caracter invalido retorna 1            
            int tam = entrada.Length;
            for (int i = 0; i < tam; i++)
            {
                if (entrada[i] > 47 && entrada[i] < 58)
                {
                    //cout<<entrada[i]<<endl;
                }
                else
                {
                    //cout<<entrada[i];
                    //cout<<" caracter invalido"<<endl;
                    return 1;
                }
            }            
            return 0;
        }

        public int verificavalor(String entrada)
        {
            //Verifica Valor
            //Se tudo estiver ok, retorna 0
            //Caso contrário retorna 1
            //Caso tenha mais de 2 casas após a vírgula ou se possuir 2 virgulas, retorna 2
            int numeropontos = 0;
            int casas = 0;
            int tam = entrada.Length;
            for (int i = 0; i < tam; i++)
            {
                if (entrada[i] > 47 && entrada[i] < 58)
                {
                    //cout<<entrada[i]<<endl;
                    if (numeropontos > 0)
                    {
                        casas++;
                    }
                    if (casas > 2)
                    {
                        return 2;
                    }
                }
                else
                {
                    if (entrada[i] == 46)
                    {
                        if (numeropontos == 0)
                        {
                            numeropontos++;
                        }
                        else
                        {
                            return 2;
                        }
                    }
                    else
                    {
                        //cout<<entrada[i];
                        //cout<<" caracter invalido"<<endl;
                        return 1;
                    }
                }
            }
            if (numeropontos == 0)
            {
                return 2;
            }
            if (numeropontos == 1 && casas < 2)
            {
                return 2;
            }
            if (entrada.Length > 8)
            {
                return 3;
            }
            return 0;
        }


        //Verificar data de nascimentos
        public int VerificaDataNascimento(String DataSelecionada) {
            String DatadeHoje = DateTime.Now.ToString();            
            Console.WriteLine("DataHoje: " + DatadeHoje + " DataSelecionada: " + DataSelecionada);

            if (DatadeHoje == DataSelecionada)
            {
                return 2;
            }
            else {
                int diaHoje = ((((int)DatadeHoje[0]) - 48) * 10);
                diaHoje += (((int)DatadeHoje[1]) - 48);

                int mesHoje = ((((int)DatadeHoje[3]) - 48) * 10);
                mesHoje += (((int)DatadeHoje[4]) - 48);

                int anoHoje = ((((int)DatadeHoje[6]) - 48) * 1000);
                anoHoje += ((((int)DatadeHoje[7]) - 48) * 100);
                anoHoje += ((((int)DatadeHoje[8]) - 48) * 10);
                anoHoje += (((int)DatadeHoje[9]) - 48);

                int diaSelecionado = ((((int)DataSelecionada[0])-48) * 10);
                diaSelecionado += (((int)DataSelecionada[1])-48);
                int mesSelecionado = ((((int)DataSelecionada[3]) - 48) * 10);
                mesSelecionado += (((int)DataSelecionada[4]) - 48);
                int anoSelecionado = ((((int)DataSelecionada[6]) - 48) * 1000);
                anoSelecionado += ((((int)DataSelecionada[7]) - 48) * 100);
                anoSelecionado += ((((int)DataSelecionada[8]) - 48) * 10);
                anoSelecionado += (((int)DataSelecionada[9]) - 48);

                if (anoSelecionado == anoHoje)
                {
                    if (mesSelecionado == mesHoje)
                    {
                        if (diaHoje == diaSelecionado)
                        {
                            return 1;
                        }
                        else {
                            if (diaSelecionado > diaHoje) {
                                return 2;
                            }
                        }
                    }
                    else {
                        if (mesSelecionado > mesHoje) {
                            return 2;
                        }
                    }
                }
                else {
                    if (anoSelecionado > anoHoje) {
                        return 2;
                    }
                }
            }
            return 0;
        }

        //Como regra de negócio vamos definir que 
        //esta data é valida se for do mesmo mes que o dia atual entao retorna 0
        // se for do futuro ou mais antiga q um mes retorna 1
        public int VerificaDataPedido(DateTime dataSelecionada)
        {
            double days = (DateTime.Now - dataSelecionada).TotalDays;

            if (days > 30 || days < 0)
                return 1;
            else
                return 0;  
           

        }



        //Verificar datas entre reservas
        public int VerificaDataReserva(String DataEntrada,String DataSaida)
        {
            String DatadeHoje = DateTime.Now.ToString();
            if (DataEntrada == DataSaida)
            {
                return 1;
            }
            else
            {
                int diaHoje = ((((int)DatadeHoje[0]) - 48) * 10);
                diaHoje += (((int)DatadeHoje[1]) - 48);
                int mesHoje = ((((int)DatadeHoje[3]) - 48) * 10);
                mesHoje += (((int)DatadeHoje[4]) - 48);
                int anoHoje = ((((int)DatadeHoje[6]) - 48) * 1000);
                anoHoje += ((((int)DatadeHoje[7]) - 48) * 100);
                anoHoje += ((((int)DatadeHoje[8]) - 48) * 10);
                anoHoje += (((int)DatadeHoje[9]) - 48);

                int diaEntrada = ((((int)DataEntrada[0]) - 48) * 10);
                diaEntrada += (((int)DataEntrada[1]) - 48);
                int mesEntrada = ((((int)DataEntrada[3]) - 48) * 10);
                mesEntrada += (((int)DataEntrada[4]) - 48);
                int anoEntrada = ((((int)DataEntrada[6]) - 48) * 1000);
                anoEntrada += ((((int)DataEntrada[7]) - 48) * 100);
                anoEntrada += ((((int)DataEntrada[8]) - 48) * 10);
                anoEntrada += (((int)DataEntrada[9]) - 48);

                int diaSaida = ((((int)DataSaida[0]) - 48) * 10);
                diaSaida += (((int)DataSaida[1]) - 48);
                int mesSaida = ((((int)DataSaida[3]) - 48) * 10);
                mesSaida += (((int)DataSaida[4]) - 48);
                int anoSaida = ((((int)DataSaida[6]) - 48) * 1000);
                anoSaida += ((((int)DataSaida[7]) - 48) * 100);
                anoSaida += ((((int)DataSaida[8]) - 48) * 10);
                anoSaida += (((int)DataSaida[9]) - 48);

                if ((anoEntrada < anoHoje)||(anoSaida < anoHoje)) {
                    return 2;
                }
                if ((mesEntrada < mesHoje) || (mesSaida < mesHoje))
                {
                    return 2;
                }
                if ((diaEntrada < diaHoje) || (diaSaida < diaHoje))
                {
                    return 2;
                }

                if (anoSaida == anoEntrada)
                {
                    if (mesSaida == mesEntrada)
                    {
                        if (diaSaida == diaEntrada)
                        {
                            return 1;
                        }
                        else
                        {
                            if ((diaSaida < diaEntrada)||(diaEntrada > diaSaida))
                            {
                                return 2;
                            }
                        }
                    }
                    else
                    {
                        if ((mesSaida < mesEntrada)||(mesEntrada>mesSaida))
                        {
                            return 2;
                        }
                    }
                }
                else
                {
                    if ((anoSaida < anoEntrada)||(anoEntrada>anoSaida))
                    {
                        return 2;
                    }
                }
            }
            return 0;
        }

    }
}
