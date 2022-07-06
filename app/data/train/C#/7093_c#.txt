using System;
using System.Linq;

class SubstringInText
{
    static void Main()
    {
        string pattern = Console.ReadLine();
        string text = Console.ReadLine();

        int counter = 0;
        for (int i = 0; i < text.Length - pattern.Length + 1; i++)
        {
            if (text.Substring(i, pattern.Length).Equals(pattern, StringComparison.OrdinalIgnoreCase))
            { 
                counter++;
            }
        }
        Console.WriteLine(counter);

    }
}