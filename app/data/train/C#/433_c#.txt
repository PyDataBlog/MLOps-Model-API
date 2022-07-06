using System;
using System.Text.RegularExpressions;

namespace _07_Hideout
{
    class Hideout
    {
        static void Main()
        {
            string input = Console.ReadLine();
            while (true)
            {  
                string[] parameters = Console.ReadLine().Split();
                string key = Regex.Escape(parameters[0]);
                string pattern = @"(" + key + "{" + parameters[1] + ",})";  
                Regex r = new Regex(pattern);
                Match match = r.Match(input);
                if (match.Success)
                {
                    Console.WriteLine("Hideout found at index {0} and it is with size {1}!", match.Index, match.Groups[0].Length);
                    break;
                }
            }            
        }
    }
}
