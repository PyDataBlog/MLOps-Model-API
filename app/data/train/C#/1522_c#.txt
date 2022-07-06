using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Cinema
{
    class Program
    {
        static void Main(string[] args)
        {
            var project = Console.ReadLine().ToLower();
            var r = double.Parse(Console.ReadLine());
            var c = double.Parse(Console.ReadLine());
            var result = r * c;
            var price = -1.0;

            if (project == "premiere")
            {
                price = 12.00;
            }
            else if (project == "normal")
            {
                price = 7.50;
            }
            else if (project == "discount")
            {
                price = 5.00;
            }
            if (price >= 0)
            {
                Console.WriteLine("{0:f2}", price * result);
            }
        }
    }
}
