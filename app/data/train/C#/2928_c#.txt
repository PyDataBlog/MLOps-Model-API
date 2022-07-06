using System;
using System.Linq;

namespace Task_5
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.OutputEncoding = System.Text.Encoding.UTF8;
            int count= 0;
            Console.WriteLine("Ohjelma kertoo kuinka monta vokaalia lauseessa on!");
            Console.Write("Anna lause:");
            string UserInput = Console.ReadLine().ToUpper();


               foreach (char letter in UserInput)
                {
                if (IsVowel(letter))
                    count++;
                    }
                Console.WriteLine($"Vokaaleita on: {count}");
            Console.ReadLine();
        }

        static bool IsVowel(char letter)
        {
            if (letter == 'A' || letter == 'E' || letter == 'I' || letter == 'O' || letter == 'U' || letter == 'Y' || letter == 'Ä' || letter == 'Ö')
                return true;
            else
                return false;
        }
    }
}
