using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RemoveOddPosition
{
    class RemoveOddPosition
    {
        static void Main(string[] args)
        {
            var words = Console.ReadLine().Split(' ').ToList();
            var output = new List<string>();

            for (int i = 0; i < words.Count; i++)
            {
                if (i%2 != 0)
                {
                    output.Add(words[i]);
                }
            }

            Console.WriteLine(string.Join("", output));
        }
    }
}
