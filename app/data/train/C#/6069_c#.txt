using System;
/*Problem 1. Leap year

Write a program that reads a year from the console and checks whether it is a leap one.
Use System.DateTime.*/
namespace _01.Leap_year
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Please enter a year");
            int year = int.Parse(Console.ReadLine());
            bool isLeapYear = DateTime.IsLeapYear(year);
            if (isLeapYear)
            {
                Console.WriteLine("The year is leap");
            }
            else
            {
                Console.WriteLine("The year isn't leap");
            }
        }
    }
}
