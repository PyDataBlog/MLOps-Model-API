using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _10.CatalanNumbers
{
    class Program
    {
        static void Main(string[] args)
        {
            // 10. Write a program to calculate the Nth Catalan number by given N.


            Console.Write("Insert N = ");
            string input = Console.ReadLine();
            int num;
            int catalanNumber;

            int factN = 1;
            int factM = 1;
            int factK = 1;

            if (int.TryParse(input, out num))
            {
                if (num > -1)
                {
                    if (num == 0)
                    {
                        Console.WriteLine("Catalan number given by '0' is: 1");
                    }
                    else if (num >= 1)
                    {
                        int n = num;
                        while (n > 1)
                        {
                            factN = factN * n; // for (n!)
                            n = n - 1;
                        }

                        n = num + 1;
                        while (n > 1)
                        {
                            factM = factM * n; // for  (n + 1)
                            n = n - 1;
                        }

                        n = 2 * num;
                        while (n > 1)
                        {
                            factK = factK * n; // for (2 * n)
                            n = n - 1;
                        }

                        catalanNumber = factK / (factM * factN);
                        Console.WriteLine("Catalan number given by 'N' is: " + catalanNumber);
                    }
                }
                else
                {
                    Console.WriteLine("N < 0");
                }
            }
            else
            {
                Console.WriteLine("Incorect input!");
            }
        }
    }
}
