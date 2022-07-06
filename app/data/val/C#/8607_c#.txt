using System;
using System.Threading;
using System.Globalization;

namespace TheBigOfFive

//    Problem 6. The Biggest of Five Numbers
//    Write a program that finds the biggest of five numbers by using only five if statements.

{
    class TheBigOfFive
    {
        static void Main()
        {
            Thread.CurrentThread.CurrentCulture = CultureInfo.InvariantCulture;

            Console.Write("Enter first number: ");
            double a = double.Parse(Console.ReadLine());

            Console.Write("Enter second number: ");
            double b = double.Parse(Console.ReadLine());

            Console.Write("Enter third number: ");
            double c = double.Parse(Console.ReadLine());

            Console.Write("Enter fourth number: ");
            double d = double.Parse(Console.ReadLine());

            Console.Write("Enter fifth number: ");
            double e = double.Parse(Console.ReadLine());

            if (a > b && a > c && a > d && a > e)
            {
                Console.WriteLine("THe biggest is {0}", a);
            }
            else if (b > a && a > c && b > d && b > e)
            {
                Console.WriteLine("The biggest is {0}", b);
            }
            else if (c > a && c > b && c > d && c > e)
            {
                Console.WriteLine("The biggest is {0}", c);
            }
            else if (d > a && d > b && d > c && c > e)
            {
                Console.WriteLine("The biggest is {0}", d);
            }
            else if (e > a && e > b && e > c && e > d)
            {
                Console.WriteLine("The biggest is {0}", e);
            }
        }
    }
}
