using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
class Program
{
    static void Main()
    {
        StreamReader file = new StreamReader("text.txt");

        using (file)
        {
            MatchCollection words = Regex.Matches(file.ReadToEnd(), @"\w+");
            List<string> result = new List<string>();
            foreach (Match item in words)
            {
                if (!result.Contains(item.Value))
                {
                    result.Add(item.Value);
                }
            }
            result.Sort();
            Console.WriteLine(string.Join("\n", result));
        }
    }
}
