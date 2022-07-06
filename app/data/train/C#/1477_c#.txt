using System;

namespace _14.MagicLetter
{
    class Program
    {
        static void Main(string[] args)
        {
            char letter1 = char.Parse(Console.ReadLine());
            char letter2 = char.Parse(Console.ReadLine());
            string letter3 = Console.ReadLine();

            for (char i = letter1; i <= letter2; i++)
            {
                for (char p = letter1; p <= letter2; p++)
                {
                    for (char k = letter1; k <= letter2; k++)
                    {
                        string result = $"{i}{p}{k}";
                        if (!result.Contains(letter3))
                        {
                            Console.Write(result + " ");
                        }
                    }
                }
            }
        }
    }
}
