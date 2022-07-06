using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using AppCantina.model;

namespace AppCantina.Impressao
{
    public class TextPrint : IPrint
    {

        private StreamWriter _sw;

        public void AbreCupom()
        {
            var arquivo = "textprint_" + DateTime.Now.Millisecond + ".txt";
            _sw = new StreamWriter(arquivo, false, Encoding.Default);
        }

        public void VendeItens(IList<ItensVenda> itens)
        {
            foreach (var iten in itens)
            {
                _sw.WriteLine("Item.....:" + iten.ItemId);
                _sw.WriteLine("Produto..:" + iten.Produto);
                _sw.WriteLine("Qtde.....:" + iten.Quantidade);
                _sw.WriteLine("R$ Unit..:" + iten.ValorUnitario);
                _sw.WriteLine("R$ Total.:" + iten.ValorTotal);
                _sw.WriteLine("--------------------------------------------");
                _sw.Flush();
                

            }
        }

        public void FechaCupom(string totalRecebido)
        {
            _sw.WriteLine("Valor Recebido:" + totalRecebido);
            _sw.WriteLine("--------------------------------------------");
            _sw.Flush();
            _sw.Close();
        }
    }
}
