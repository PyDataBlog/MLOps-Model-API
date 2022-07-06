using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace _01.Melrah_Shake
{
    class Program
    {
        static void Main(string[] args)
        {
            string inputString = Console.ReadLine();
            string pattern = Console.ReadLine();

            Regex regex = new Regex(Regex.Escape(pattern));
            MatchCollection matches = regex.Matches(inputString);

            while (true)
            {
                if (matches.Count >= 2 && pattern.Length > 0)
                {
                    int startIndex = inputString.IndexOf(pattern);
                    int lastIndex = inputString.LastIndexOf(pattern);

                    inputString = inputString.Remove(lastIndex, pattern.Length);
                    inputString = inputString.Remove(startIndex, pattern.Length);

                    pattern = pattern.Remove(pattern.Length / 2, 1);
                    Console.WriteLine("Shaked it.");
                }
                else
                {
                    Console.WriteLine("No shake.");
                    break;
                }
                regex = new Regex(Regex.Escape(pattern));
                matches = regex.Matches(inputString);
            }
            Console.WriteLine(inputString);
        }
    }
}
