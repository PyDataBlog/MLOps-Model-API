using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace ProyectoFinalMacoRios
{
    public class Garaje:Propiedades
    {

        string abierto;
        string bodega;

        public string Abierto
        {
            get
            {
                return abierto;
            }

            set
            {
                abierto = value;
            }
        }

        public string Bodega
        {
            get
            {
                return bodega;
            }

            set
            {
                bodega = value;
            }
        }
    }
}