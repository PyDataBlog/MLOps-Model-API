//Write a program that enters a number n and after that enters more n numbers and calculates and prints their sum.

using System;
using System.Globalization;
using System.Threading;

namespace SumOfNNumbers
{
    class SumOfNNumbers
    {
        static void Main()
        {
            Thread.CurrentThread.CurrentCulture = CultureInfo.InvariantCulture;
            Console.Write("Please enter integer number n: ");
            int n = Convert.ToInt32(Console.ReadLine());
            double sum = 0;
            for (int i = 1; i <= n; i++)
            {
                Console.Write("Please enter integer number: ");
                sum =double.Parse(Console.ReadLine()) + sum;
            }
            Console.WriteLine("Sum is: " + sum);
            
        }
    }
}
