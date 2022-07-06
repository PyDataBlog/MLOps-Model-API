using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VaiFundos
{
    class Program
    {
        static void Main(string[] args)
        {
            GerenciadorCliente gerenciador = new GerenciadorCliente();
            FundosEmDolar fundo_dolar1 = new FundosEmDolar("Fundos USA", "FSA");
            FundosEmDolar fundo_dolar2 = new FundosEmDolar("Cambio USA", "CSA");
            FundosEmReal fundo_real1 = new FundosEmReal("Fundo Deposito Interbancario", "DI");
            FundosEmReal fundo_real2 = new FundosEmReal("Atmos Master", "FIA");
            int opcao = 0;
            Console.WriteLine("*==============Vai Fundos===================*");
            Console.WriteLine("*-------------------------------------------*");
            Console.WriteLine("*1------Cadastro Cliente--------------------*");
            Console.WriteLine("*2------Fazer Aplicacao---------------------*");
            Console.WriteLine("*3------Listar Clientes---------------------*");
            Console.WriteLine("*4------Relatorio Do Cliente----------------*");
            Console.WriteLine("*5------Relatorio Do Fundo------------------*");
            Console.WriteLine("*6------Transferir Aplicacao De Fundo-------*");
            Console.WriteLine("*7------Resgatar Aplicacao------------------*");
            Console.WriteLine("*8------Sair Do Sistema --------------------*");
            Console.WriteLine("*===========================================*");
            Console.WriteLine("Informe a opcao Desejada:");
            opcao = int.Parse(Console.ReadLine());
            while(opcao >= 1 && opcao < 8)
            {
                if (opcao == 1)
                {
                    Console.WriteLine("Informe o nome do Cliente Que deseja Cadastrar");
                    gerenciador.cadastrarCliente(Console.ReadLine());
                    Console.WriteLine("Toque uma tecla para voltar ao menu principal:");
                    Console.ReadKey();
                    Console.Clear();
                    
                }
                else if (opcao == 2)
                {
                    int codFundo = 0;

                    Console.WriteLine("Cod: 1-{0}", fundo_dolar1.getInfFundo());
                    Console.WriteLine("Cod: 2-{0}", fundo_dolar2.getInfFundo());
                    Console.WriteLine("Cod: 3-{0}", fundo_real1.getInfFundo());
                    Console.WriteLine("Cod: 4-{0}", fundo_real2.getInfFundo());
                    Console.WriteLine("Informe o Codigo Do Fundo que Deseja Aplicar");
                    codFundo = int.Parse(Console.ReadLine());
                    if (codFundo == 1)
                    {
                        double valorAplicacao = 0;
                        int codCliente = 0;
                        gerenciador.listarClientes();
                        Console.WriteLine("Informe o Codigo do Cliente que Deseja Aplicar:");
                        codCliente = int.Parse(Console.ReadLine());
                        Console.WriteLine("Informe o valor da Aplicacao");
                        valorAplicacao = double.Parse(Console.ReadLine());

                        fundo_dolar1.Aplicar(valorAplicacao, codCliente, gerenciador);
                        Console.WriteLine("Toque uma tecla para voltar ao menu principal:");
                        Console.ReadKey();
                        Console.Clear();
                       

                    }
                    else if (codFundo == 2)
                    {
                        double valorAplicacao = 0;
                        int codCliente = 0;
                        gerenciador.listarClientes();
                        Console.WriteLine("Informe o Codigo do Cliente que Deseja Aplicar:");
                        codCliente = int.Parse(Console.ReadLine());
                        Console.WriteLine("Informe o valor da Aplicacao");
                        valorAplicacao = double.Parse(Console.ReadLine());

                        fundo_dolar2.Aplicar(valorAplicacao, codCliente, gerenciador);
                        Console.WriteLine("Toque uma tecla para voltar ao menu principal:");
                        Console.ReadKey();
                        Console.Clear();
                                            }
                    else if (codFundo == 3)
                    {
                        double valorAplicacao = 0;
                        int codCliente = 0;
                        gerenciador.listarClientes();
                        Console.WriteLine("Informe o Codigo do Cliente que Deseja Aplicar:");
                        codCliente = int.Parse(Console.ReadLine());
                        Console.WriteLine("Informe o valor da Aplicacao");
                        valorAplicacao = double.Parse(Console.ReadLine());

                        fundo_real1.Aplicar(valorAplicacao, codCliente, gerenciador);
                        Console.WriteLine("Toque uma tecla para voltar ao menu principal:");
                        Console.ReadKey();
                        Console.Clear();
                        
                    }
                    else if (codFundo == 4)
                    {
                        double valorAplicacao = 0;
                        int codCliente = 0;
                        gerenciador.listarClientes();
                        Console.WriteLine("Informe o Codigo do Cliente que Deseja Aplicar:");
                        codCliente = int.Parse(Console.ReadLine());
                        Console.WriteLine("Informe o valor da Aplicacao");
                        valorAplicacao = double.Parse(Console.ReadLine());

                        fundo_real2.Aplicar(valorAplicacao, codCliente, gerenciador);
                        Console.WriteLine("Toque uma tecla para voltar ao menu principal:");
                        Console.ReadKey();
                        Console.Clear();
                        
                    }

                }
                else if (opcao == 3)
                {
                    Console.Clear();
                    Console.WriteLine("Clientes Cadastrados:");
                    gerenciador.listarClientes();
                    Console.WriteLine("Toque uma tecla para voltar ao menu principal:");
                    Console.ReadKey();


                }
                else if (opcao == 4)
                {
                    int codCliente = 0;
                    Console.WriteLine("Clientes Cadastrados");
                    gerenciador.listarClientes();
                    codCliente = int.Parse(Console.ReadLine());
                    gerenciador.relatorioCliente(gerenciador.buscaCliente(codCliente));
                    Console.WriteLine("Toque uma tecla para voltar ao menu principal:");
                    Console.ReadKey();
                }
                else if (opcao == 5)
                {
                    int codFundo = 0;

                    Console.WriteLine("1-{0}", fundo_dolar1.getInfFundo());
                    Console.WriteLine("2-{0}", fundo_dolar2.getInfFundo());
                    Console.WriteLine("3-{0}", fundo_real1.getInfFundo());
                    Console.WriteLine("4-{0}", fundo_real2.getInfFundo());
                    Console.WriteLine("Informe o Codigo Do Fundo que Deseja o Relatorio");
                    codFundo = int.Parse(Console.ReadLine());
                    if (codFundo == 1)
                    {

                        fundo_dolar1.relatorioFundo();
                        Console.WriteLine("Toque uma tecla para voltar ao menu principal:");
                        Console.ReadKey();
                        Console.Clear();


                    }
                    else if (codFundo == 2)
                    {
                        fundo_dolar2.relatorioFundo();
                        Console.WriteLine("Toque uma tecla para voltar ao menu principal:");
                        Console.ReadKey();
                        Console.Clear();
                    }
                    else if (codFundo == 3)
                    {
                        fundo_real1.relatorioFundo();
                        Console.WriteLine("Toque uma tecla para voltar ao menu principal:");
                        Console.ReadKey();
                        Console.Clear();

                    }
                    else if (codFundo == 4)
                    {
                        fundo_real2.relatorioFundo();
                        Console.WriteLine("Toque uma tecla para voltar ao menu principal:");
                        Console.ReadKey();
                        Console.Clear();

                    }

                }
                else if (opcao == 6)
                {
                    Console.WriteLine("Fundos De Investimentos Disponiveis");
                    Console.WriteLine("1-{0}", fundo_dolar1.getInfFundo());
                    Console.WriteLine("2-{0}", fundo_dolar2.getInfFundo());
                    Console.WriteLine("3-{0}", fundo_real1.getInfFundo());
                    Console.WriteLine("4-{0}", fundo_real2.getInfFundo());
                    int codFundoOrigem = 0;
                    int codDestino = 0;
                    int codCliente = 0;
                    double valor = 0;

                    Console.WriteLine("Informe o Codigo do fundo que deseja transferir");
                    codFundoOrigem = int.Parse(Console.ReadLine());

                    gerenciador.listarClientes();
                    Console.WriteLine("Informe o codigo do cliente que deseja realizar a transferencia");
                    codCliente = int.Parse(Console.ReadLine());
                    Console.WriteLine("Informe o valor da aplicacao que deseja transferir");
                    valor = double.Parse(Console.ReadLine());
                    if(codFundoOrigem == 1)
                    {
                        Console.WriteLine("Fundos De Investimentos Disponiveis Para Troca");
                        Console.WriteLine("2-{0}", fundo_dolar2.getInfFundo());
                        Console.WriteLine("Informe o Codigo do fundo que recebera a aplicacao");
                        codDestino = int.Parse(Console.ReadLine());
                        if (codDestino == 2)
                        {
                            fundo_dolar1.TrocarFundo(codCliente,valor,fundo_dolar2,'D');
                        }
                     
                    }
                    else if(codFundoOrigem == 2)
                    {
                        Console.WriteLine("Fundos De Investimentos Disponiveis Para Troca");
                        Console.WriteLine("1-{0}", fundo_dolar1.getInfFundo());
                        Console.WriteLine("Informe o Codigo do fundo que recebera a aplicacao");
                        codDestino = int.Parse(Console.ReadLine());
                        if (codDestino == 1)
                        {
     
                            fundo_dolar2.TrocarFundo(codCliente, valor, fundo_dolar1,'D');
                        }
                        
                    }
                    else if(codFundoOrigem == 3)
                    {
                        Console.WriteLine("Fundos De Investimentos Disponiveis Para Troca");
                        Console.WriteLine("4-{0}", fundo_real2.getInfFundo());
                        Console.WriteLine("Informe o Codigo do fundo que recebera a aplicacao");
                        codDestino = int.Parse(Console.ReadLine());
                        if (codDestino == 4)
                        {
                            fundo_real1.TrocarFundo(codCliente, valor, fundo_real2,'R');
                        }
                       
                    }
                    else if(codFundoOrigem == 4)
                    {
                        Console.WriteLine("Fundos De Investimentos Disponiveis Para Troca");
                        Console.WriteLine("3-{0}", fundo_real1.getInfFundo());
                        Console.WriteLine("Informe o Codigo do fundo que recebera a aplicacao");
                        codDestino = int.Parse(Console.ReadLine());
                        if (codDestino == 3)
                        {
                            fundo_real2.TrocarFundo(codCliente, valor, fundo_real1,'R');
                        }
                       
                    }
                    Console.WriteLine("Troca Efetuada");
                    Console.WriteLine("Toque uma tecla para voltar ao menu principal:");
                    Console.ReadKey();
                    Console.Clear();
                }
                else if(opcao == 7)
                {
                    Console.WriteLine("Fundos De Investimentos Disponiveis");
                    Console.WriteLine("1-{0}", fundo_dolar1.getInfFundo());
                    Console.WriteLine("2-{0}", fundo_dolar2.getInfFundo());
                    Console.WriteLine("3-{0}", fundo_real1.getInfFundo());
                    Console.WriteLine("4-{0}", fundo_real2.getInfFundo());
                    int codFundo = 0;
                    int codCliente = 0;
                    double valor = 0;

                    Console.WriteLine("Informe o Codigo do Fundo Que Deseja Sacar");
                    codFundo = int.Parse(Console.ReadLine());
                    gerenciador.listarClientes();
                    Console.WriteLine("Informe o codigo do cliente que deseja realizar o saque");
                    codCliente = int.Parse(Console.ReadLine());
                    Console.WriteLine("Informe o valor da aplicacao que deseja sacar");
                    valor = double.Parse(Console.ReadLine());
                    if(codFundo == 1)
                    {
                        fundo_dolar1.resgate(valor,codCliente,gerenciador);
                    }
                    else if(codFundo == 2)
                    {
                        fundo_dolar2.resgate(valor, codCliente, gerenciador);
                    }
                    else if(codFundo == 3)
                    {
                        fundo_real1.resgate(valor, codCliente, gerenciador);
                    }
                    else if(codFundo == 4)
                    {
                        fundo_real2.resgate(valor, codCliente, gerenciador);
                    }
                    Console.WriteLine("Toque uma tecla para voltar ao menu principal:");
                    Console.ReadKey();
                    Console.Clear();

                }
                Console.WriteLine("*==============Vai Fundos===================*");
                Console.WriteLine("*-------------------------------------------*");
                Console.WriteLine("*1------Cadastro Cliente--------------------*");
                Console.WriteLine("*2------Fazer Aplicacao---------------------*");
                Console.WriteLine("*3------Listar Clientes---------------------*");
                Console.WriteLine("*4------Relatorio Do Cliente----------------*");
                Console.WriteLine("*5------Relatorio Do Fundo------------------*");
                Console.WriteLine("*6------Transferir Aplicacao De Fundo-------*");
                Console.WriteLine("*7------Resgatar Aplicacao------------------*");
                Console.WriteLine("*8------Sair Do Sistema --------------------*");
                Console.WriteLine("*===========================================*");
                Console.WriteLine("Informe a opcao Desejada:");
                opcao = int.Parse(Console.ReadLine());


            }     

        }
    }
}
