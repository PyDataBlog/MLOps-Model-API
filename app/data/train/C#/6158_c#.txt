using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ConsoleApplication4
{
class CSV
{
string rutaArchivo;

public void leerArchivo()
{
Console.WriteLine("Dame la ruta del archivo a leer");
this.rutaArchivo = Console.ReadLine();

}

public Boolean VerificarRuta()
{
Boolean ArchivoExiste;
if (File.Exists(this.rutaArchivo))
ArchivoExiste = true;
else
ArchivoExiste = false;
return ArchivoExiste;
}

public void VerificaArchivo()
{
if (VerificarRuta())
if (Path.GetExtension(this.rutaArchivo) == ".csv")
Console.WriteLine("\nTu archivo si tiene la extensión CSV\n");
else
Console.WriteLine("\nTu archivo no tiene la extensión CSV.. pertenece a la extensión.. " + Path.GetExtension(this.rutaArchivo));
}



public List<string[]> ObtenerContenido()
{

List<string[]> csvArreglo = new List<string[]>();
try
{
string linea;
string[] fila;

StreamReader readFile = new StreamReader(this.rutaArchivo);
while ((linea = readFile.ReadLine()) != null)
{
fila = linea.Split(',');
csvArreglo.Add(fila);
}
}
catch (Exception e)
{

Console.WriteLine("\nEl archivo con extensión .CSV no existe\n", e);


}

return csvArreglo;

}

}
}
