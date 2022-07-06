using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
namespace _3.CameraView
{
    class Program
    {
        static void Main()
        {
            int[] array = Console.ReadLine().Split().Select(int.Parse).ToArray();
            int skip = array[0];
            int take = array[1];
            string text = Console.ReadLine();
            string pattern = "\\|<(\\w{" + skip + "})(\\w{0," + take + "})";

            MatchCollection views = Regex.Matches(text, pattern);
            List<string> results = new List<string>();

            foreach (Match view in views)
            {
                results.Add(view.Groups[2].Value);
            }

            Console.WriteLine(String.Join(", ", results));
        }
    }
}
