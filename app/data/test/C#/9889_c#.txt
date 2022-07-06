using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using SistemaMundoAnimal.Source.Entidades;

namespace SistemaMundoAnimal.Source.Dados.Tipos {
    public class TipoRedeSocial : Tipo<string> {

        private static int Length = 150;
        private TipoContato Tipo;

        public TipoRedeSocial(string redesocial, TipoContato tipo) {
            this.Valor = redesocial;
            this.Tipo = tipo;
        }

        public static bool ValidarRedeSocial(string valor, TipoContato tipo) {
            // TODO: Validar rdee social pelo tipo
            return valor.Length < Length;
        }

        public override string ToString() {
            return this.Valor;
        }

    }
}
