using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace Regex_Test_MatchCollection
{
    class Program
    {
        static void Main(string[] args)
        {
            string pattern = "[A-Z][a-z]+ [A-Z][a-z]+";
            Regex regex = new Regex(pattern);

            string text = "Ivan Ivanov Martin Milenov Iordan Iovkov";

            MatchCollection names = regex.Matches(text);

            foreach(Match matches in names)
            {
                Console.WriteLine(matches.Groups[0]);
            }
        }
    }
}
