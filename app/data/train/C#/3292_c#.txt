using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sunglasses
{
    class Program
    {
        static void Main(string[] args)
        {
            int n = int.Parse(Console.ReadLine());
            Console.WriteLine("{0}{1}{2}", new string('*', 2*n), new string(' ', n), new string('*', 2 * n));
            for (int i = 1; i <= n - 2; i++)
            {
                Console.Write("*");
                Console.Write(new string('/', (2*n) - 2));
                Console.Write("*");
                int middle = (n - 1) / 2 ; 
                if (i == middle)
                {
                    Console.Write(new string('|', n));
                }
                else
                {
                    Console.Write(new string(' ', n));
                }
                Console.Write("*");
                Console.Write(new string('/', (2 * n) - 2));
                Console.Write("*");
                Console.WriteLine();
            }
            Console.WriteLine("{0}{1}{2}", new string('*', 2 * n), new string(' ', n), new string('*', 2 * n));
        }
    }
}
