//Problem 6. Calculate N! / K!

//Write a program that calculates n! / k! for given n and k (1 < k < n < 100).
//Use only one loop.
//Examples:

//n	    k	n! / k!
//5	    2	    60
//6	    5	    6
//8	    3	    6720

using System;

class SecondCalculate
{
    static void Main()
    {
        Console.Write("Enter n: ");
        int n = int.Parse(Console.ReadLine());

        Console.Write("Enter k: ");
        int k = int.Parse(Console.ReadLine());

        decimal sum = 1;
        if (1 < k && k < n && n < 100)
        {
            for (int i = 1 + k; i <= n; i++)
            {
                sum *= i;
            }
        }
        else
        {
            Console.WriteLine("Sorry, can not calculate such high shit");
        }
        Console.WriteLine(sum);
        
    }
}
