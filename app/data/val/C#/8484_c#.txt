using System;

namespace FourDigitSecondWay
{
    class Program
    {
        static void Main()
        {
            int number = int.Parse(Console.ReadLine());

            int fourthNumber = number % 10;//a
            int thirdNumber = (number / 10) % 10;//b   
            int secondNumber = (number / 100) % 10;//c
            int firstNumber = (number / 1000) % 10;//d
            Console.WriteLine((fourthNumber + thirdNumber + secondNumber + firstNumber));
            Console.WriteLine("{0}{1}{2}{3}", fourthNumber, thirdNumber, secondNumber, firstNumber);
            Console.WriteLine("{0}{1}{2}{3}", fourthNumber, firstNumber, secondNumber, thirdNumber);
            Console.WriteLine("{0}{1}{2}{3}", firstNumber, thirdNumber, secondNumber, fourthNumber);
        }
    }
}
